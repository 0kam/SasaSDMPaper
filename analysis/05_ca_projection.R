# =============================================================================
# 05_ca_projection.R -- annual cellular-automaton projection, 2021-2030
# =============================================================================

t0 <- Sys.time()
source(file.path("analysis", "00_config.R"))
source(file.path(DIR_ANALYSIS, "R", "distance.R"))
source(file.path(DIR_ANALYSIS, "R", "model_utils.R"))

suppressPackageStartupMessages(library(mgcv))
set.seed(SEED_CA)

# Colonized cells persist permanently. Extinction is deliberately absent from
# this projection because loss observations are confounded with canopy-class
# changes (decisions.md section 7).

inference_model_path <- model_b_inference_path()
required_inputs <- c(
  PATH_PREDICTORS, PATH_FOLDS_RASTER, PATH_DIST12, PATH_SNOW_SCENARIOS,
  PATH_VEGE_2012, PATH_VEGE_2021,
  inference_model_path
)
missing_inputs <- required_inputs[!file.exists(required_inputs)]
if (length(missing_inputs)) {
  stop("missing WP4 inputs: ", paste(missing_inputs, collapse = ", "))
}

PROJECTION_YEARS <- 2022:2030
HINDCAST_YEARS <- 2013:2021
DISTANCE_LOOKUP_STEP_M <- 0.5
LOGDIST_LOOKUP_STEP <- 1e-4
OBSERVED_GROSS_COLONIZED_AREA_M2 <- 4097
CA_WORKER_RNG <- if (CA_PARALLEL_BACKEND == "PSOCK") {
  paste(
    "PSOCK clusterSetRNGStream",
    "with scheduling-independent per-replicate set.seed"
  )
} else {
  "sequential scheduling-independent per-replicate set.seed"
}

available_ca_workers <- function(n_tasks = CA_MAX_WORKERS) {
  cores <- suppressWarnings(parallel::detectCores(logical = FALSE))
  if (length(cores) != 1L || !is.finite(cores) || cores < 1L) {
    cores <- suppressWarnings(parallel::detectCores(logical = TRUE))
  }
  if (length(cores) != 1L || !is.finite(cores) || cores < 1L) {
    detected <- tryCatch(
      system("getconf _NPROCESSORS_ONLN", intern = TRUE),
      error = function(e) character(0)
    )
    cores <- if (length(detected)) {
      suppressWarnings(as.integer(detected[[1]]))
    } else {
      NA_integer_
    }
  }
  if (length(cores) != 1L || !is.finite(cores) || cores < 1L) cores <- 1L
  as.integer(min(CA_MAX_WORKERS, n_tasks, cores))
}

scenario_tag <- function(shift_d_yr) {
  if (isTRUE(all.equal(shift_d_yr, 0, tolerance = 1e-12))) return("s0")
  if (isTRUE(all.equal(shift_d_yr, -0.71, tolerance = 1e-12))) return("sm071")
  if (isTRUE(all.equal(shift_d_yr, -2.24, tolerance = 1e-12))) return("sm224")
  stop("unsupported snow shift: ", shift_d_yr)
}

auc_rank <- function(truth, probability) {
  ok <- !is.na(truth) & is.finite(probability)
  truth <- as.integer(truth[ok])
  probability <- probability[ok]
  n_pos <- as.double(sum(truth == 1L))
  n_neg <- as.double(sum(truth == 0L))
  if (n_pos == 0L || n_neg == 0L) return(NA_real_)
  ranks <- rank(probability, ties.method = "average")
  (sum(ranks[truth == 1L]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
}

make_newdata <- function(predictor_data, rows, snow_override = NULL,
                         distance = 0) {
  distance <- as.numeric(distance)
  if (length(distance) == 1L) {
    distance <- rep(distance, length(rows))
  } else if (length(distance) != length(rows)) {
    stop("distance must have length one or match rows")
  }
  out <- data.frame(
    dist12 = distance,
    log1p_dist12 = log1p(distance),
    predictor_data[rows, PREDICTOR_NAMES, drop = FALSE],
    check.names = FALSE
  )
  if (!is.null(snow_override)) out$snow_mean <- snow_override
  out
}

predict_term_sum <- function(model, predictor_data, term_labels,
                             snow_values = NULL, block_size = 100000L) {
  n <- nrow(predictor_data)
  out <- numeric(n)
  starts <- seq.int(1L, n, by = block_size)
  for (first in starts) {
    rows <- first:min(n, first + block_size - 1L)
    snow_block <- if (is.null(snow_values)) NULL else snow_values[rows]
    nd <- make_newdata(predictor_data, rows, snow_block, distance = 0)
    z <- stats::predict(
      model, newdata = nd, type = "terms", terms = term_labels,
      discrete = FALSE
    )
    if (is.null(dim(z))) z <- matrix(z, ncol = 1L)
    out[rows] <- rowSums(z)
  }
  out
}

precompute_environment <- function(model, predictor_data, scenarios, years) {
  labels <- vapply(model$smooth, `[[`, character(1), "label")
  distance_labels <- intersect(c("s(dist12)", "s(log1p_dist12)"), labels)
  if (length(distance_labels) != 1L) {
    stop("Model B does not contain exactly one supported distance term")
  }
  distance_label <- distance_labels[[1]]
  snow_label <- "s(snow_mean)"
  if (!all(c(distance_label, snow_label) %in% labels)) {
    stop("Model B does not contain the required additive distance and snow terms")
  }
  static_labels <- setdiff(labels, c(distance_label, snow_label))
  intercept <- unname(stats::coef(model)[["(Intercept)"]])
  if (!is.finite(intercept)) stop("Model B intercept is not finite")

  msg("Precomputing the six static environmental GAM terms")
  static_lp <- intercept + predict_term_sum(
    model, predictor_data, static_labels
  )

  cache <- new.env(parent = emptyenv())
  output <- vector("list", nrow(scenarios))
  names(output) <- scenarios$tag
  for (i in seq_len(nrow(scenarios))) {
    by_year <- vector("list", length(years))
    names(by_year) <- as.character(years)
    for (j in seq_along(years)) {
      delta <- scenarios$shift_d_yr[[i]] * (years[[j]] - ANCHOR_YEAR)
      if (abs(delta) < 1e-12) delta <- 0
      key <- sprintf("%.10f", delta)
      if (!exists(key, envir = cache, inherits = FALSE)) {
        shifted_snow <- pmax(0, predictor_data$snow_mean + delta)
        msg("Precomputing snow GAM term for delta DOY = ",
            sprintf("%.2f", delta))
        snow_effect <- predict_term_sum(
          model, predictor_data, snow_label, snow_values = shifted_snow
        )
        assign(key, static_lp + snow_effect, envir = cache)
      }
      by_year[[j]] <- get(key, envir = cache, inherits = FALSE)
    }
    output[[i]] <- by_year
  }
  output
}

make_distance_lookup <- function(model, predictor_data, max_distance_m) {
  distance_labels <- intersect(
    c("s(dist12)", "s(log1p_dist12)"),
    vapply(model$smooth, `[[`, character(1), "label")
  )
  if (length(distance_labels) != 1L) {
    stop("Model B does not contain exactly one supported distance term")
  }
  if (identical(distance_labels[[1]], "s(log1p_dist12)")) {
    lookup_scale <- "log1p"
    maximum_coordinate <- log1p(max_distance_m)
    lookup_coordinate <- seq(
      0, ceiling(maximum_coordinate / LOGDIST_LOOKUP_STEP) *
        LOGDIST_LOOKUP_STEP,
      by = LOGDIST_LOOKUP_STEP
    )
    lookup_distance <- expm1(lookup_coordinate)
  } else {
    lookup_scale <- "distance_m"
    lookup_distance <- seq(
      0, ceiling(max_distance_m / DISTANCE_LOOKUP_STEP_M) *
        DISTANCE_LOOKUP_STEP_M,
      by = DISTANCE_LOOKUP_STEP_M
    )
    lookup_coordinate <- lookup_distance
  }
  reference <- predictor_data[rep(1L, length(lookup_distance)), , drop = FALSE]
  nd <- data.frame(
    dist12 = lookup_distance,
    log1p_dist12 = log1p(lookup_distance),
    reference[, PREDICTOR_NAMES, drop = FALSE],
    check.names = FALSE
  )
  lookup_effect <- as.numeric(stats::predict(
    model, newdata = nd, type = "terms", terms = distance_labels[[1]],
    discrete = FALSE
  ))
  if (any(!is.finite(lookup_effect))) stop("distance lookup contains non-finite values")
  list(
    distance = lookup_distance, coordinate = lookup_coordinate,
    scale = lookup_scale, effect = lookup_effect
  )
}

interpolate_distance_effect <- function(distance_m, lookup) {
  if (length(distance_m) == 0L) return(numeric(0))
  if (any(!is.finite(distance_m))) stop("non-finite distance in an eligible cell")
  tolerance <- 1e-8
  if (min(distance_m) < -tolerance ||
      max(distance_m) > max(lookup$distance) + tolerance) {
    stop("simulation distance lies outside the validated lookup range")
  }
  interpolation_coordinate <- if (identical(lookup$scale, "log1p")) {
    log1p(distance_m)
  } else {
    distance_m
  }
  stats::approx(
    x = lookup$coordinate, y = lookup$effect,
    xout = interpolation_coordinate,
    method = "linear", rule = 2, ties = "ordered"
  )$y
}

validate_decomposition <- function(model, predictor_data, domain_cells,
                                   dist12_values, env_lp, lookup) {
  valid_position <- which(is.finite(dist12_values[domain_cells]))
  if (length(valid_position) < 10000L) {
    stop("fewer than 10,000 cells are available for decomposition validation")
  }
  set.seed(SEED_CA)
  position <- sample(valid_position, 10000L, replace = FALSE)
  cells <- domain_cells[position]
  nd <- make_newdata(
    predictor_data, position, snow_override = predictor_data$snow_mean[position],
    distance = dist12_values[cells]
  )
  direct <- as.numeric(stats::predict(
    model, newdata = nd, type = "response", discrete = FALSE
  ))
  reconstructed <- stats::plogis(
    env_lp[position] + interpolate_distance_effect(dist12_values[cells], lookup)
  )
  delta <- abs(reconstructed - direct)
  maximum <- max(delta)
  result <- data.frame(
    n_cells = length(delta),
    lookup_scale = lookup$scale,
    lookup_step = if (identical(lookup$scale, "log1p")) {
      LOGDIST_LOOKUP_STEP
    } else {
      DISTANCE_LOOKUP_STEP_M
    },
    max_abs_delta_p9 = maximum,
    mean_abs_delta_p9 = mean(delta),
    gate = 1e-6,
    passed = maximum < 1e-6
  )
  utils::write.csv(
    result, file.path(DIR_OUT, "ca_decomposition_validation.csv"),
    row.names = FALSE
  )
  msg("GAM decomposition maximum |delta p9| = ",
      format(maximum, scientific = TRUE, digits = 6))
  if (!isTRUE(result$passed[[1]])) {
    stop("GAM decomposition validation failed: max |delta p9| = ", maximum)
  }
  result
}

distance_values_from_state <- function(base_path, state_values, min_area_m2) {
  current <- terra::rast(base_path)
  terra::values(current) <- state_values
  as.numeric(terra::values(
    sasa_distance(current, min_area_m2 = min_area_m2), mat = FALSE
  ))
}

run_ca_replicate <- function(seed, base_path, base_values, eligible_cells,
                             eligible_domain_position, env_lp_by_year, years,
                             lookup, cell_area_m2, initial_distance_values,
                             min_area_m2 = 5,
                             annualization = c("exact", "first_order")) {
  annualization <- match.arg(annualization)
  set.seed(seed)
  state <- base_values
  occupied <- rep(FALSE, length(eligible_cells))
  annual_area <- numeric(length(years))
  distance_values <- initial_distance_values

  for (j in seq_along(years)) {
    candidate_position <- which(!occupied)
    if (!length(candidate_position)) break
    if (j > 1L) {
      distance_values <- distance_values_from_state(
        base_path, state, min_area_m2 = min_area_m2
      )
    }
    candidate_cells <- eligible_cells[candidate_position]
    candidate_domain_position <- eligible_domain_position[candidate_position]
    distance_effect <- interpolate_distance_effect(
      distance_values[candidate_cells], lookup
    )
    p9 <- stats::plogis(
      env_lp_by_year[[j]][candidate_domain_position] + distance_effect
    )
    p9 <- pmin(1, pmax(0, p9))
    p1 <- if (annualization == "exact") {
      1 - (1 - p9)^(1 / 9)
    } else {
      p9 / 9
    }
    colonized <- stats::rbinom(length(p1), size = 1L, prob = p1) == 1L
    if (any(colonized)) {
      new_position <- candidate_position[colonized]
      new_cells <- eligible_cells[new_position]
      occupied[new_position] <- TRUE
      state[new_cells] <- 1
      annual_area[[j]] <- sum(cell_area_m2[new_cells])
    }
  }
  list(
    new_position = which(occupied),
    annual_area_m2 = annual_area,
    cumulative_area_m2 = cumsum(annual_area)
  )
}

run_ca_replicate_worker <- function(i) {
  run_ca_replicate(
    seed = ca_worker_seeds[[i]],
    base_path = ca_worker_base_path,
    base_values = ca_worker_base_values,
    eligible_cells = ca_worker_eligible_cells,
    eligible_domain_position = ca_worker_eligible_domain_position,
    env_lp_by_year = ca_worker_env_lp_by_year,
    years = ca_worker_years,
    lookup = ca_worker_lookup,
    cell_area_m2 = ca_worker_cell_area_m2,
    initial_distance_values = ca_worker_initial_distance_values,
    min_area_m2 = ca_worker_min_area_m2,
    annualization = ca_worker_annualization
  )
}

run_replicates <- function(n_replicates, seed_base, base_path, base_values,
                           eligible_cells, eligible_domain_position,
                           env_lp_by_year, years, lookup, cell_area_m2,
                           initial_distance_values, min_area_m2 = 5,
                           annualization = "exact", label = "CA") {
  workers <- available_ca_workers(n_replicates)
  if (CA_PARALLEL_BACKEND == "sequential") workers <- 1L
  msg(label, ": running ", n_replicates, " replicates on ", workers,
      " ", CA_PARALLEL_BACKEND, " worker(s)")

  seeds <- as.integer(seed_base + seq_len(n_replicates) - 1L)
  if (CA_PARALLEL_BACKEND == "sequential") {
    result <- lapply(seq_len(n_replicates), function(i) {
      run_ca_replicate(
        seed = seeds[[i]], base_path = base_path, base_values = base_values,
        eligible_cells = eligible_cells,
        eligible_domain_position = eligible_domain_position,
        env_lp_by_year = env_lp_by_year, years = years, lookup = lookup,
        cell_area_m2 = cell_area_m2,
        initial_distance_values = initial_distance_values,
        min_area_m2 = min_area_m2, annualization = annualization
      )
    })
    return(result)
  }

  # terra/GDAL state must never cross a fork boundary. PSOCK starts clean R
  # sessions, loads terra in each worker, and creates every SpatRaster there.
  cluster <- parallel::makeCluster(workers, type = "PSOCK")
  on.exit(parallel::stopCluster(cluster), add = TRUE)
  parallel::clusterEvalQ(cluster, {
    suppressPackageStartupMessages(library(terra))
    NULL
  })
  distance_source <- file.path(DIR_ANALYSIS, "R", "distance.R")
  parallel::clusterCall(cluster, function(path) {
    source(path, local = .GlobalEnv)
    NULL
  }, distance_source)

  worker_functions <- c(
    "interpolate_distance_effect", "distance_values_from_state",
    "run_ca_replicate", "run_ca_replicate_worker"
  )
  parallel::clusterExport(
    cluster, worker_functions, envir = environment(run_replicates)
  )
  ca_worker_seeds <- seeds
  ca_worker_base_path <- base_path
  ca_worker_base_values <- base_values
  ca_worker_eligible_cells <- eligible_cells
  ca_worker_eligible_domain_position <- eligible_domain_position
  ca_worker_env_lp_by_year <- env_lp_by_year
  ca_worker_years <- years
  ca_worker_lookup <- lookup
  ca_worker_cell_area_m2 <- cell_area_m2
  ca_worker_initial_distance_values <- initial_distance_values
  ca_worker_min_area_m2 <- min_area_m2
  ca_worker_annualization <- annualization
  worker_data <- c(
    "ca_worker_seeds", "ca_worker_base_path", "ca_worker_base_values",
    "ca_worker_eligible_cells", "ca_worker_eligible_domain_position",
    "ca_worker_env_lp_by_year", "ca_worker_years", "ca_worker_lookup",
    "ca_worker_cell_area_m2", "ca_worker_initial_distance_values",
    "ca_worker_min_area_m2", "ca_worker_annualization"
  )
  parallel::clusterExport(cluster, worker_data, envir = environment())

  # Initialize independent L'Ecuyer streams as required for PSOCK. The fixed
  # per-replicate seeds inside run_ca_replicate additionally make each result
  # stable if task scheduling or the worker count changes.
  parallel::clusterSetRNGStream(cluster, iseed = seed_base)
  result <- parallel::parLapply(
    cluster, seq_len(n_replicates), run_ca_replicate_worker
  )
  failed <- vapply(result, inherits, logical(1), what = "try-error")
  if (any(failed)) {
    stop(label, " replicate failure(s): ",
         paste(as.character(result[failed]), collapse = " | "))
  }
  result
}

summarize_replicates <- function(result, eligible_cells, cell_area_m2, years) {
  counts <- integer(length(eligible_cells))
  for (z in result) counts[z$new_position] <- counts[z$new_position] + 1L
  probability <- counts / length(result)
  annual <- do.call(rbind, lapply(result, `[[`, "annual_area_m2"))
  cumulative <- t(apply(annual, 1L, cumsum))
  if (is.null(dim(cumulative))) cumulative <- matrix(cumulative, nrow = 1L)
  replicate_sd <- if (nrow(cumulative) > 1L) {
    apply(cumulative, 2L, stats::sd)
  } else {
    rep(0, length(years))
  }
  expected_from_probability <- sum(
    probability * cell_area_m2[eligible_cells]
  )
  expected_from_replicates <- mean(cumulative[, length(years)])
  if (!isTRUE(all.equal(expected_from_probability, expected_from_replicates,
                        tolerance = 1e-8))) {
    stop("probability-sum and replicate-mean expected areas disagree")
  }
  list(
    probability = probability,
    expected_area_m2 = expected_from_probability,
    trajectory = data.frame(
      year = years,
      expected_new_area_m2 = colMeans(annual),
      cumulative_expected_area_m2 = colMeans(cumulative),
      replicate_sd_m2 = replicate_sd
    ),
    example_new_cells = eligible_cells[result[[1]]$new_position]
  )
}

write_probability_raster <- function(probability, eligible_cells, base_values,
                                     template, filename) {
  output_values <- rep(NA_real_, length(base_values))
  output_values[!is.na(base_values) & base_values == 1] <- 0
  output_values[eligible_cells] <- probability
  out <- terra::rast(template)
  terra::values(out) <- output_values
  names(out) <- "p_new_colonized_by_2030"
  assert_on_ref_grid(out, basename(filename))
  terra::writeRaster(
    out, filename, overwrite = TRUE, datatype = "FLT4S",
    gdal = "COMPRESS=DEFLATE"
  )
  filename
}

write_example_raster <- function(new_cells, eligible_cells, base_values,
                                 template, filename) {
  # This example shows the full 2030 occupied distribution: both the initial
  # Sasa cells and the newly colonized cells have value 1.
  output_values <- rep(NA_integer_, length(base_values))
  output_values[eligible_cells] <- 0L
  output_values[!is.na(base_values) & base_values == 1] <- 1L
  output_values[new_cells] <- 1L
  out <- terra::rast(template)
  terra::values(out) <- output_values
  names(out) <- "sasa_2030_example"
  assert_on_ref_grid(out, basename(filename))
  terra::writeRaster(
    out, filename, overwrite = TRUE, datatype = "INT1U", NAflag = 255,
    gdal = "COMPRESS=DEFLATE"
  )
  filename
}

# ---- shared rasters and analysis domain ------------------------------------
msg("Loading WP4 inputs")
predictors <- terra::rast(PATH_PREDICTORS)
assert_on_ref_grid(predictors, "WP4 predictors")
if (!identical(names(predictors), PREDICTOR_NAMES)) {
  stop("predictor names/order do not match PREDICTOR_NAMES")
}
folds <- read_shared_folds()
vege12 <- terra::rast(PATH_VEGE_2012)
vege21 <- terra::rast(PATH_VEGE_2021)
dist12 <- terra::rast(PATH_DIST12)
assert_on_ref_grid(dist12, "WP4 dist12")

predictor_values_all <- terra::values(predictors)
domain_cells <- which(complete.cases(predictor_values_all))
domain_predictors <- as.data.frame(
  predictor_values_all[domain_cells, PREDICTOR_NAMES, drop = FALSE],
  check.names = FALSE
)
rm(predictor_values_all)
cell_to_domain <- integer(terra::ncell(predictors))
cell_to_domain[domain_cells] <- seq_along(domain_cells)

v12 <- as.numeric(terra::values(vege12, mat = FALSE))
v21 <- as.numeric(terra::values(vege21, mat = FALSE))
fold_values <- as.integer(terra::values(folds, mat = FALSE))
dist12_values <- as.numeric(terra::values(dist12, mat = FALSE))

projection_eligible <- domain_cells[
  !is.na(v21[domain_cells]) & v21[domain_cells] != 1
]
projection_eligible_domain <- cell_to_domain[projection_eligible]
hindcast_eligible <- domain_cells[
  is.finite(dist12_values[domain_cells]) &
    !is.na(v12[domain_cells]) & !is.na(v21[domain_cells]) &
    is.finite(fold_values[domain_cells]) & v12[domain_cells] != 1
]
hindcast_eligible_domain <- cell_to_domain[hindcast_eligible]

# cellSize is terra's per-cell counterpart to expanse. The explicit equality
# check below ties the weighted Sigma-p calculation to the required geodesic,
# expanse-based area convention.
cell_area_raster <- terra::cellSize(
  terra::rast(predictors[[1]]), unit = "m", transform = TRUE
)
cell_area_m2 <- as.numeric(terra::values(cell_area_raster, mat = FALSE))
row_zone <- terra::rast(predictors[[1]])
row_zone[] <- NA_integer_
cell_row <- rep(seq_len(terra::nrow(row_zone)), each = terra::ncol(row_zone))
row_zone[domain_cells] <- cell_row[domain_cells]
row_expanse <- terra::expanse(
  row_zone, unit = "m", transform = TRUE, byValue = TRUE
)
raw_row_area <- rowsum(
  cell_area_m2[domain_cells], cell_row[domain_cells], reorder = FALSE
)
row_scale <- rep(NA_real_, terra::nrow(row_zone))
row_scale[row_expanse$value] <- row_expanse$area /
  raw_row_area[as.character(row_expanse$value), 1]
cell_area_m2 <- cell_area_m2 * row_scale[cell_row]
area_by_expanse <- sum(row_expanse$area)
area_by_cells <- sum(cell_area_m2[domain_cells])
if (!is.finite(area_by_cells) ||
    abs(area_by_cells - area_by_expanse) / area_by_expanse > 1e-10) {
  stop("cell-area weights do not reproduce terra::expanse geodesic area")
}

scenario_table <- utils::read.csv(PATH_SNOW_SCENARIOS, stringsAsFactors = FALSE)
if (!all(c("scenario", "shift_d_yr") %in% names(scenario_table)) ||
    !isTRUE(all.equal(sort(scenario_table$shift_d_yr),
                      c(-2.24, -0.71, 0), tolerance = 1e-12))) {
  stop("snowmelt_scenarios.csv does not contain exactly 0/-0.71/-2.24 d/yr")
}
scenario_table$tag <- vapply(scenario_table$shift_d_yr, scenario_tag, character(1))
if (SASA_SMOKE) {
  scenario_table <- scenario_table[
    scenario_table$shift_d_yr %in% c(0, -0.71), , drop = FALSE
  ]
}

final_gam <- readRDS(inference_model_path)
max_distance_m <- max(dist12_values, na.rm = TRUE)
distance_lookup <- make_distance_lookup(
  final_gam, domain_predictors, max_distance_m
)
environment_main <- precompute_environment(
  final_gam, domain_predictors, scenario_table, PROJECTION_YEARS
)
zero_tag <- scenario_tag(0)
decomposition <- validate_decomposition(
  final_gam, domain_predictors, domain_cells, dist12_values,
  environment_main[[zero_tag]][[1]], distance_lookup
)

msg("Computing 2021 distance surface for the dynamic projection")
dist21_values <- as.numeric(terra::values(
  sasa_distance(vege21, min_area_m2 = 5), mat = FALSE
))

n_main <- if (SASA_SMOKE) N_CA_REPLICATES_SMOKE else N_CA_REPLICATES
runtime_deviation <- "none"
if (!SASA_SMOKE) {
  msg("Timing one full-grid pilot replicate for the four-hour runtime guard")
  pilot_time <- system.time(run_ca_replicate(
    seed = SEED_CA, base_path = PATH_VEGE_2021, base_values = v21,
    eligible_cells = projection_eligible,
    eligible_domain_position = projection_eligible_domain,
    env_lp_by_year = environment_main[[1]], years = PROJECTION_YEARS,
    lookup = distance_lookup, cell_area_m2 = cell_area_m2,
    initial_distance_values = dist21_values, min_area_m2 = 5,
    annualization = "exact"
  ))[["elapsed"]]
  projected_hours <- pilot_time * nrow(scenario_table) * n_main /
    available_ca_workers(n_main) / 3600
  msg("Projected main-set wall time = ", sprintf("%.2f h", projected_hours))
  if (projected_hours > 4) {
    n_main <- 100L
    runtime_deviation <- paste0(
      "main replicates reduced from 200 to 100; pilot projected ",
      sprintf("%.2f", projected_hours), " h"
    )
    cat("\n*** RUNTIME NOTICE: ", runtime_deviation, " ***\n\n", sep = "")
  }
}

# ---- main 2021-2030 scenario projections -----------------------------------
trajectory_rows <- list()
main_summary_rows <- list()
main_result_by_tag <- list()
created_files <- file.path(DIR_OUT, "ca_decomposition_validation.csv")

for (i in seq_len(nrow(scenario_table))) {
  tag <- scenario_table$tag[[i]]
  result <- run_replicates(
    n_replicates = n_main,
    seed_base = SEED_CA + i * 10000L,
    base_path = PATH_VEGE_2021, base_values = v21,
    eligible_cells = projection_eligible,
    eligible_domain_position = projection_eligible_domain,
    env_lp_by_year = environment_main[[tag]], years = PROJECTION_YEARS,
    lookup = distance_lookup, cell_area_m2 = cell_area_m2,
    initial_distance_values = dist21_values,
    min_area_m2 = 5, annualization = "exact",
    label = paste("Projection", tag)
  )
  summary <- summarize_replicates(
    result, projection_eligible, cell_area_m2, PROJECTION_YEARS
  )
  main_result_by_tag[[tag]] <- summary
  trajectory <- summary$trajectory
  trajectory$scenario <- tag
  trajectory$shift_d_yr <- scenario_table$shift_d_yr[[i]]
  trajectory$n_replicates <- n_main
  trajectory_rows[[i]] <- trajectory[, c(
    "scenario", "shift_d_yr", "year", "expected_new_area_m2",
    "cumulative_expected_area_m2", "replicate_sd_m2", "n_replicates"
  )]
  main_summary_rows[[i]] <- data.frame(
    scenario = tag,
    shift_d_yr = scenario_table$shift_d_yr[[i]],
    expected_new_area_2030_m2 = summary$expected_area_m2,
    n_replicates = n_main
  )

  probability_path <- file.path(
    DIR_OUT, paste0("ca_pcol_2030_", tag, ".tif")
  )
  example_path <- file.path(
    DIR_OUT, paste0("ca_example_2030_", tag, ".tif")
  )
  created_files <- c(
    created_files,
    write_probability_raster(
      summary$probability, projection_eligible, v21, predictors[[1]],
      probability_path
    ),
    write_example_raster(
      summary$example_new_cells, projection_eligible, v21, predictors[[1]],
      example_path
    )
  )
}

trajectory_all <- do.call(rbind, trajectory_rows)
trajectory_path <- file.path(DIR_OUT, "ca_trajectory.csv")
utils::write.csv(trajectory_all, trajectory_path, row.names = FALSE)
created_files <- c(created_files, trajectory_path)
main_summary <- do.call(rbind, main_summary_rows)

# ---- 2012-2021 hindcast: calibration check, not independent validation -----
msg("Starting 2012-2021 hindcast calibration check (not independent validation)")
n_hindcast <- if (SASA_SMOKE) {
  N_CA_REPLICATES_SMOKE
} else {
  N_CA_HINDCAST_REPLICATES
}
hindcast_result <- run_replicates(
  n_replicates = n_hindcast,
  seed_base = SEED_CA_HINDCAST,
  base_path = PATH_VEGE_2012, base_values = v12,
  eligible_cells = hindcast_eligible,
  eligible_domain_position = hindcast_eligible_domain,
  env_lp_by_year = environment_main[[zero_tag]], years = HINDCAST_YEARS,
  lookup = distance_lookup, cell_area_m2 = cell_area_m2,
  initial_distance_values = dist12_values,
  min_area_m2 = 5, annualization = "exact",
  label = "Hindcast calibration check"
)
hindcast <- summarize_replicates(
  hindcast_result, hindcast_eligible, cell_area_m2, HINDCAST_YEARS
)
observed_hindcast <- as.integer(v21[hindcast_eligible] == 1)
observed_area_geodesic <- sum(
  cell_area_m2[hindcast_eligible[observed_hindcast == 1L]]
)
hindcast_ratio <- hindcast$expected_area_m2 /
  OBSERVED_GROSS_COLONIZED_AREA_M2
hindcast_auc <- auc_rank(observed_hindcast, hindcast$probability)
hindcast_summary <- data.frame(
  validation_type = "calibration_check_not_independent",
  start_year = 2012L,
  end_year = 2021L,
  n_replicates = n_hindcast,
  observed_gross_colonized_area_m2 = OBSERVED_GROSS_COLONIZED_AREA_M2,
  observed_area_geodesic_m2 = observed_area_geodesic,
  expected_simulated_area_m2 = hindcast$expected_area_m2,
  expected_to_observed_ratio = hindcast_ratio,
  cell_auc = hindcast_auc,
  reporting_lower = 0.5,
  reporting_upper = 2.0
)
hindcast_summary_path <- file.path(DIR_OUT, "ca_hindcast_summary.csv")
utils::write.csv(hindcast_summary, hindcast_summary_path, row.names = FALSE)
created_files <- c(created_files, hindcast_summary_path)

band_breaks <- c(0, 5, 10, 20, 40, 80, 160, Inf)
band_labels <- c("[0,5)", "[5,10)", "[10,20)", "[20,40)",
                 "[40,80)", "[80,160)", ">=160")
band <- cut(
  dist12_values[hindcast_eligible], breaks = band_breaks,
  right = FALSE, include.lowest = TRUE, labels = band_labels
)
hindcast_bands <- do.call(rbind, lapply(seq_along(band_labels), function(i) {
  in_band <- band == band_labels[[i]]
  data.frame(
    validation_type = "calibration_check_not_independent",
    distance_band_m = band_labels[[i]],
    n_cells = sum(in_band),
    observed_colonized_cells = sum(observed_hindcast[in_band]),
    observed_colonization_rate = mean(observed_hindcast[in_band]),
    simulated_colonization_frequency = mean(hindcast$probability[in_band]),
    simulated_expected_area_m2 = sum(
      hindcast$probability[in_band] * cell_area_m2[hindcast_eligible[in_band]]
    )
  )
}))
hindcast_bands_path <- file.path(DIR_OUT, "ca_hindcast_band_comparison.csv")
utils::write.csv(hindcast_bands, hindcast_bands_path, row.names = FALSE)
created_files <- c(created_files, hindcast_bands_path)

# ---- genuinely independent spatial held-block validation -------------------
msg("Starting genuinely independent held-block spatial validation")
model_data_all <- data.frame(
  cell = hindcast_eligible,
  colonized = observed_hindcast,
  fold_id = fold_values[hindcast_eligible],
  dist12 = dist12_values[hindcast_eligible],
  domain_predictors[hindcast_eligible_domain, PREDICTOR_NAMES, drop = FALSE],
  check.names = FALSE
)
model_data_all$log1p_dist12 <- log1p(model_data_all$dist12)
sample_rows <- stratified_smoke_sample(
  model_data_all, model_data_all$colonized, model_data_all$fold_id,
  fraction = 0.10, seed = SEED_MODEL_B
)
model_data_fit <- model_data_all[sample_rows, , drop = FALSE]
if (identical(inference_model_path, PATH_MODEL_B_LOGDIST)) {
  gam_formula_ca <- colonized ~
    s(log1p_dist12, k = 5) + s(snow_mean, k = 10) +
    s(elevation, k = 10) + s(slope, k = 10) +
    s(TPI, k = 10) + s(twi, k = 10) +
    s(northness, k = 10) + s(eastness, k = 10)
} else {
  gam_formula_ca <- colonized ~
    s(dist12, k = 3) + s(snow_mean, k = 10) +
    s(elevation, k = 10) + s(slope, k = 10) +
    s(TPI, k = 10) + s(twi, k = 10) +
    s(northness, k = 10) + s(eastness, k = 10)
}

n_spatial <- if (SASA_SMOKE) {
  N_CA_REPLICATES_SMOKE
} else {
  N_CA_SPATIAL_REPLICATES
}
spatial_rows <- vector("list", CV_FOLDS_FULL)
zero_scenario <- data.frame(
  scenario = "none", shift_d_yr = 0, tag = zero_tag,
  stringsAsFactors = FALSE
)
for (k in seq_len(CV_FOLDS_FULL)) {
  msg("Spatial validation fold ", k, ": refitting Model B without the block")
  train <- model_data_fit$fold_id != k
  set.seed(SEED_CA_SPATIAL + k)
  fold_model <- mgcv::bam(
    gam_formula_ca,
    data = model_data_fit[train, c(
      "colonized", "dist12", "log1p_dist12", PREDICTOR_NAMES
    ), drop = FALSE],
    family = stats::binomial(), discrete = TRUE,
    nthreads = MODEL_THREADS, method = "fREML"
  )
  fold_environment <- precompute_environment(
    fold_model, domain_predictors, zero_scenario, HINDCAST_YEARS
  )[[zero_tag]]
  fold_lookup <- make_distance_lookup(
    fold_model, domain_predictors, max_distance_m
  )
  fold_result <- run_replicates(
    n_replicates = n_spatial,
    seed_base = SEED_CA_SPATIAL + k * 10000L,
    base_path = PATH_VEGE_2012, base_values = v12,
    eligible_cells = hindcast_eligible,
    eligible_domain_position = hindcast_eligible_domain,
    env_lp_by_year = fold_environment, years = HINDCAST_YEARS,
    lookup = fold_lookup, cell_area_m2 = cell_area_m2,
    initial_distance_values = dist12_values,
    min_area_m2 = 5, annualization = "exact",
    label = paste("Spatial validation fold", k)
  )
  fold_simulation <- summarize_replicates(
    fold_result, hindcast_eligible, cell_area_m2, HINDCAST_YEARS
  )
  score_position <- which(fold_values[hindcast_eligible] == k)
  truth <- observed_hindcast[score_position]
  probability <- fold_simulation$probability[score_position]
  observed_area <- sum(
    cell_area_m2[hindcast_eligible[score_position[truth == 1L]]]
  )
  expected_area <- sum(
    probability * cell_area_m2[hindcast_eligible[score_position]]
  )
  spatial_rows[[k]] <- data.frame(
    validation_type = "independent_spatial_held_block",
    fold = k,
    n_scored_cells = length(score_position),
    n_replicates = n_spatial,
    auc = auc_rank(truth, probability),
    observed_colonized_area_m2 = observed_area,
    expected_colonized_area_m2 = expected_area,
    expected_to_observed_ratio = expected_area / observed_area
  )
  rm(fold_model, fold_environment, fold_lookup, fold_result, fold_simulation)
  gc(verbose = FALSE)
}
spatial_validation <- do.call(rbind, spatial_rows)
spatial_path <- file.path(DIR_OUT, "ca_spatial_validation.csv")
utils::write.csv(spatial_validation, spatial_path, row.names = FALSE)
created_files <- c(created_files, spatial_path)

# ---- reduced sensitivity set -----------------------------------------------
msg("Starting reduced CA sensitivity simulations")
n_sensitivity <- if (SASA_SMOKE) {
  N_CA_REPLICATES_SMOKE
} else {
  N_CA_SENSITIVITY_REPLICATES
}
sensitivity_tag <- scenario_tag(-0.71)
baseline_area <- main_result_by_tag[[sensitivity_tag]]$expected_area_m2

msg("Computing the min_area_m2 = 0 initial distance surface")
dist21_min0_values <- as.numeric(terra::values(
  sasa_distance(vege21, min_area_m2 = 0), mat = FALSE
))
sensitivity_min0_result <- run_replicates(
  n_replicates = n_sensitivity,
  seed_base = SEED_CA_SENSITIVITY,
  base_path = PATH_VEGE_2021, base_values = v21,
  eligible_cells = projection_eligible,
  eligible_domain_position = projection_eligible_domain,
  env_lp_by_year = environment_main[[sensitivity_tag]],
  years = PROJECTION_YEARS, lookup = distance_lookup,
  cell_area_m2 = cell_area_m2,
  initial_distance_values = dist21_min0_values,
  min_area_m2 = 0, annualization = "exact",
  label = "Sensitivity min_area_m2=0"
)
sensitivity_min0 <- summarize_replicates(
  sensitivity_min0_result, projection_eligible, cell_area_m2, PROJECTION_YEARS
)

sensitivity_annual_result <- run_replicates(
  n_replicates = n_sensitivity,
  seed_base = SEED_CA_SENSITIVITY + 10000L,
  base_path = PATH_VEGE_2021, base_values = v21,
  eligible_cells = projection_eligible,
  eligible_domain_position = projection_eligible_domain,
  env_lp_by_year = environment_main[[sensitivity_tag]],
  years = PROJECTION_YEARS, lookup = distance_lookup,
  cell_area_m2 = cell_area_m2,
  initial_distance_values = dist21_values,
  min_area_m2 = 5, annualization = "first_order",
  label = "Sensitivity p1=p9/9"
)
sensitivity_annual <- summarize_replicates(
  sensitivity_annual_result, projection_eligible, cell_area_m2, PROJECTION_YEARS
)
sensitivity <- data.frame(
  scenario = sensitivity_tag,
  shift_d_yr = -0.71,
  sensitivity = c("distance_min_area_m2_0", "annualization_p9_div_9"),
  n_replicates = n_sensitivity,
  baseline_expected_area_2030_m2 = baseline_area,
  sensitivity_expected_area_2030_m2 = c(
    sensitivity_min0$expected_area_m2,
    sensitivity_annual$expected_area_m2
  )
)
sensitivity$difference_m2 <-
  sensitivity$sensitivity_expected_area_2030_m2 -
  sensitivity$baseline_expected_area_2030_m2
sensitivity$ratio_to_baseline <-
  sensitivity$sensitivity_expected_area_2030_m2 /
  sensitivity$baseline_expected_area_2030_m2
sensitivity_path <- file.path(DIR_OUT, "ca_sensitivity.csv")
utils::write.csv(sensitivity, sensitivity_path, row.names = FALSE)
created_files <- c(created_files, sensitivity_path)

# ---- run summary ------------------------------------------------------------
elapsed_seconds <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
run_deviations <- paste0(runtime_deviation, "; worker_rng=", CA_WORKER_RNG)
run_summary <- data.frame(
  smoke = SASA_SMOKE,
  runtime_seconds = elapsed_seconds,
  main_replicates = n_main,
  hindcast_replicates = n_hindcast,
  spatial_replicates_per_fold = n_spatial,
  sensitivity_replicates = n_sensitivity,
  hindcast_ratio = hindcast_ratio,
  decomposition_max_abs_delta_p9 = decomposition$max_abs_delta_p9,
  deviations = run_deviations
)
run_summary_path <- file.path(DIR_OUT, "ca_run_summary.csv")
utils::write.csv(run_summary, run_summary_path, row.names = FALSE)
created_files <- unique(c(created_files, run_summary_path))

cat("\n=== WP4 execution summary ===\n")
cat("Mode:", if (SASA_SMOKE) "SMOKE" else "FULL", "\n")
cat(sprintf("Runtime: %.1f s\n", elapsed_seconds))
cat(sprintf("Hindcast calibration-check ratio: %.6f\n", hindcast_ratio))
cat("Per-scenario expected newly colonized area by 2030:\n")
for (i in seq_len(nrow(main_summary))) {
  cat(sprintf("  %s (%.2f d/yr): %.3f m2\n",
              main_summary$scenario[[i]], main_summary$shift_d_yr[[i]],
              main_summary$expected_new_area_2030_m2[[i]]))
}
cat("Files created:\n")
for (path in created_files) cat("  ", path, "\n", sep = "")
cat("Deviations:", run_deviations, "\n")

finish_script("05_ca_projection.R", t0)
