# =============================================================================
# 05b_ca_loss_sensitivity.R -- CA with observed annual loss, 2021-2030
# =============================================================================

t0 <- Sys.time()
source(file.path("analysis", "00_config.R"))
source(file.path(DIR_ANALYSIS, "R", "distance.R"))
source(file.path(DIR_ANALYSIS, "R", "model_utils.R"))

suppressPackageStartupMessages(library(mgcv))
set.seed(SEED_CA_SENSITIVITY)

inference_model_path <- model_b_inference_path()
baseline_path <- file.path(DIR_OUT, "ca_trajectory.csv")
decomposition_path <- file.path(DIR_OUT, "ca_decomposition_validation.csv")
required_inputs <- c(
  PATH_PREDICTORS, PATH_SNOW_SCENARIOS, PATH_VEGE_2012, PATH_VEGE_2021,
  inference_model_path, baseline_path, decomposition_path
)
missing_inputs <- required_inputs[!file.exists(required_inputs)]
if (length(missing_inputs)) {
  stop("missing CA loss-sensitivity inputs: ",
       paste(missing_inputs, collapse = ", "))
}

PROJECTION_YEARS <- 2022:2030
DISTANCE_LOOKUP_STEP_M <- 0.5
LOGDIST_LOOKUP_STEP <- 1e-4

# Helpers below are copied verbatim from 05_ca_projection.R and must be kept
# in sync. Only run_ca_replicate is renamed and modified to add the loss process.

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

distance_values_from_state <- function(base_path, state_values, min_area_m2) {
  current <- terra::rast(base_path)
  terra::values(current) <- state_values
  as.numeric(terra::values(
    sasa_distance(current, min_area_m2 = min_area_m2), mat = FALSE
  ))
}

run_ca_replicate_loss <- function(seed, base_path, base_values, eligible_cells,
                             eligible_domain_position, env_lp_by_year, years,
                             lookup, cell_area_m2, initial_distance_values,
                             min_area_m2 = 5,
                             annualization = c("exact", "first_order"),
                             p_loss = attr(env_lp_by_year, "p_loss_annual")) {
  annualization <- match.arg(annualization)
  if (length(p_loss) != 1L || !is.finite(p_loss) ||
      p_loss < 0 || p_loss > 1) {
    stop("p_loss must be one finite probability")
  }
  set.seed(seed)
  state <- base_values
  initial_cells <- which(!is.na(base_values) & base_values == 1)
  # Initial Sasa cells are never eligible; occupied records first colonization
  # permanently, even after loss. Thus lost cells cannot be recolonized.
  # Cumulative gross establishment stays comparable with the no-loss baseline
  # and observed two-date gross expansion; recolonization bookkeeping is
  # deliberately avoided.
  occupied <- rep(FALSE, length(eligible_cells))
  annual_area <- numeric(length(years))

  for (j in seq_along(years)) {
    occupied_cells <- which(!is.na(state) & state == 1)
    lost_cells <- occupied_cells[
      stats::rbinom(length(occupied_cells), size = 1L, prob = p_loss) == 1L
    ]
    state[lost_cells] <- 0
    # Even the first year needs a fresh distance surface after loss.
    distance_values <- distance_values_from_state(
      base_path, state, min_area_m2 = min_area_m2
    )
    candidate_position <- which(!occupied)
    # Continue losses through 2030 even if all eligible cells have colonized.
    if (!length(candidate_position)) next
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
    cumulative_area_m2 = cumsum(annual_area),
    lost_initial_area_2030_m2 = sum(
      cell_area_m2[initial_cells[state[initial_cells] != 1]]
    )
  )
}

# Retain the original entry point so both copied PSOCK helpers stay verbatim.
# The loss probability travels in the existing environment payload's attribute.
run_ca_replicate <- run_ca_replicate_loss

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

# ---- shared rasters and analysis domain ------------------------------------
msg("Loading CA loss-sensitivity inputs")
predictors <- terra::rast(PATH_PREDICTORS)
assert_on_ref_grid(predictors, "CA loss-sensitivity predictors")
if (!identical(names(predictors), PREDICTOR_NAMES)) {
  stop("predictor names/order do not match PREDICTOR_NAMES")
}
vege12 <- terra::rast(PATH_VEGE_2012)
vege21 <- terra::rast(PATH_VEGE_2021)
assert_on_ref_grid(vege21, "CA loss-sensitivity 2021 vegetation")
v12 <- as.numeric(terra::values(vege12, mat = FALSE))
v21 <- as.numeric(terra::values(vege21, mat = FALSE))

# Estimate loss on all jointly observed vegetation cells, before restricting
# colonization to the complete-predictor domain used by 05.
observed_initial <- !is.na(v12) & !is.na(v21) & v12 == 1
if (!any(observed_initial)) stop("no jointly observed 2012 Sasa cells")
loss_fraction_9yr <- sum(v21[observed_initial] != 1) / sum(observed_initial)
p_loss_annual <- 1 - (1 - loss_fraction_9yr)^(1 / 9)
msg("Observed 9-year gross loss fraction = ",
    sprintf("%.8f", loss_fraction_9yr))
msg("Annual loss probability = ", sprintf("%.8f", p_loss_annual))
if (SASA_SMOKE) {
  stopifnot(loss_fraction_9yr >= 0.25, loss_fraction_9yr <= 0.33,
            p_loss_annual >= 0.03, p_loss_annual <= 0.045)
}

predictor_values_all <- terra::values(predictors)
domain_cells <- which(complete.cases(predictor_values_all))
domain_predictors <- as.data.frame(
  predictor_values_all[domain_cells, PREDICTOR_NAMES, drop = FALSE],
  check.names = FALSE
)
rm(predictor_values_all)
cell_to_domain <- integer(terra::ncell(predictors))
cell_to_domain[domain_cells] <- seq_along(domain_cells)
projection_eligible <- domain_cells[
  !is.na(v21[domain_cells]) & v21[domain_cells] != 1
]
projection_eligible_domain <- cell_to_domain[projection_eligible]

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

# Loss also applies to initial Sasa outside the predictor domain.
initial_cells <- which(!is.na(v21) & v21 == 1)
if (any(!is.finite(cell_area_m2[initial_cells]))) {
  stop("initial Sasa cells have non-finite area weights")
}

scenario_table <- utils::read.csv(PATH_SNOW_SCENARIOS, stringsAsFactors = FALSE)
if (!all(c("scenario", "shift_d_yr") %in% names(scenario_table)) ||
    !isTRUE(all.equal(sort(scenario_table$shift_d_yr),
                      c(-2.24, -0.71, 0), tolerance = 1e-12))) {
  stop("snowmelt_scenarios.csv does not contain exactly 0/-0.71/-2.24 d/yr")
}
scenario_table$tag <- vapply(scenario_table$shift_d_yr, scenario_tag, character(1))
scenario_table <- scenario_table[
  scenario_table$shift_d_yr %in% c(0, -0.71), , drop = FALSE
]

# Read the persisted baseline only; never rerun or overwrite 05's outputs.
baseline <- utils::read.csv(baseline_path, stringsAsFactors = FALSE)
if (!all(c("scenario", "shift_d_yr", "year", "cumulative_expected_area_m2") %in%
         names(baseline))) {
  stop("ca_trajectory.csv is missing required baseline columns")
}
baseline <- baseline[
  !is.na(baseline$year) & baseline$year == 2030 &
    baseline$scenario %in% scenario_table$tag, , drop = FALSE
]
if (nrow(baseline) != nrow(scenario_table) ||
    anyDuplicated(baseline$scenario) ||
    !all(scenario_table$tag %in% baseline$scenario)) {
  stop("ca_trajectory.csv must have one 2030 row per sensitivity scenario")
}
baseline <- baseline[match(scenario_table$tag, baseline$scenario), , drop = FALSE]
if (!isTRUE(all.equal(baseline$shift_d_yr, scenario_table$shift_d_yr,
                      tolerance = 1e-12)) ||
    any(!is.finite(baseline$cumulative_expected_area_m2)) ||
    any(baseline$cumulative_expected_area_m2 <= 0)) {
  stop("baseline scenario shifts or expected areas are invalid")
}

# Skip the decomposition gate already validated by 05, retaining its saved
# verdict without rewriting ca_decomposition_validation.csv.
decomposition <- utils::read.csv(decomposition_path)
if (nrow(decomposition) != 1L || !isTRUE(decomposition$passed[[1]])) {
  stop("05's saved GAM decomposition validation has not passed")
}
msg("Reusing 05's passed GAM decomposition gate")
final_gam <- readRDS(inference_model_path)
# Loss can move fronts beyond the historical maximum distance. The grid
# diagonal bounds every possible within-grid distance after loss.
max_distance_m <- sqrt(sum((terra::res(predictors) *
  c(terra::ncol(predictors), terra::nrow(predictors)))^2))
distance_lookup <- make_distance_lookup(
  final_gam, domain_predictors, max_distance_m
)
environment_main <- precompute_environment(
  final_gam, domain_predictors, scenario_table, PROJECTION_YEARS
)
for (tag in scenario_table$tag) {
  attr(environment_main[[tag]], "p_loss_annual") <- p_loss_annual
}

# ---- loss-sensitivity projections ------------------------------------------
n_sensitivity <- if (SASA_SMOKE) {
  N_CA_REPLICATES_SMOKE
} else {
  N_CA_SENSITIVITY_REPLICATES
}
seed_offsets <- c(s0 = 50000L, sm071 = 60000L)
trajectory_rows <- list()
summary_rows <- list()
for (i in seq_len(nrow(scenario_table))) {
  tag <- scenario_table$tag[[i]]
  result <- run_replicates(
    n_replicates = n_sensitivity,
    seed_base = SEED_CA_SENSITIVITY + seed_offsets[[tag]],
    base_path = PATH_VEGE_2021, base_values = v21,
    eligible_cells = projection_eligible,
    eligible_domain_position = projection_eligible_domain,
    env_lp_by_year = environment_main[[tag]], years = PROJECTION_YEARS,
    lookup = distance_lookup, cell_area_m2 = cell_area_m2,
    # Retained for the copied interface; loss always recomputes this surface.
    initial_distance_values = NULL,
    min_area_m2 = 5, annualization = "exact",
    label = paste("Loss sensitivity", tag)
  )
  summary <- summarize_replicates(
    result, projection_eligible, cell_area_m2, PROJECTION_YEARS
  )
  trajectory <- summary$trajectory
  trajectory$scenario <- tag
  trajectory$shift_d_yr <- scenario_table$shift_d_yr[[i]]
  trajectory$n_replicates <- n_sensitivity
  trajectory_rows[[i]] <- trajectory[, c(
    "scenario", "shift_d_yr", "year", "expected_new_area_m2",
    "cumulative_expected_area_m2", "replicate_sd_m2", "n_replicates"
  )]
  baseline_area <- baseline$cumulative_expected_area_m2[[i]]
  summary_rows[[i]] <- data.frame(
    scenario = tag,
    shift_d_yr = scenario_table$shift_d_yr[[i]],
    loss_fraction_9yr = loss_fraction_9yr,
    p_loss_annual = p_loss_annual,
    n_replicates = n_sensitivity,
    baseline_expected_area_2030_m2 = baseline_area,
    loss_expected_area_2030_m2 = summary$expected_area_m2,
    difference_m2 = summary$expected_area_m2 - baseline_area,
    ratio_to_baseline = summary$expected_area_m2 / baseline_area,
    expected_lost_initial_area_2030_m2 = mean(vapply(
      result, `[[`, numeric(1), "lost_initial_area_2030_m2"
    ))
  )
  rm(result)
  gc(verbose = FALSE)
}

sensitivity <- do.call(rbind, summary_rows)
trajectory_all <- do.call(rbind, trajectory_rows)
stopifnot(all(is.finite(sensitivity$ratio_to_baseline)),
          all(is.finite(sensitivity$expected_lost_initial_area_2030_m2)))
sensitivity_path <- file.path(DIR_OUT, "ca_loss_sensitivity.csv")
trajectory_path <- file.path(DIR_OUT, "ca_loss_trajectory.csv")
utils::write.csv(sensitivity, sensitivity_path, row.names = FALSE)
utils::write.csv(trajectory_all, trajectory_path, row.names = FALSE)

cat("\n=== CA loss-sensitivity execution summary ===\n")
cat("Mode:", if (SASA_SMOKE) "SMOKE" else "FULL", "\n")
cat(sprintf("Runtime: %.1f s\n",
            as.numeric(difftime(Sys.time(), t0, units = "secs"))))
cat(sprintf("Observed 9-year gross loss fraction: %.8f\n", loss_fraction_9yr))
cat(sprintf("Annual loss probability: %.8f\n", p_loss_annual))
cat("Replicates per scenario:", n_sensitivity, "\n")
cat("Per-scenario cumulative gross establishment by 2030:\n")
for (i in seq_len(nrow(sensitivity))) {
  cat(sprintf(
    "  %s (%.2f d/yr): baseline %.3f m2; loss %.3f m2; ratio %.6f\n",
    sensitivity$scenario[[i]], sensitivity$shift_d_yr[[i]],
    sensitivity$baseline_expected_area_2030_m2[[i]],
    sensitivity$loss_expected_area_2030_m2[[i]],
    sensitivity$ratio_to_baseline[[i]]
  ))
  cat(sprintf("    Difference: %.3f m2; initial 2021 Sasa lost: %.3f m2\n",
              sensitivity$difference_m2[[i]],
              sensitivity$expected_lost_initial_area_2030_m2[[i]]))
}
if (SASA_SMOKE) {
  cat("Smoke ratios are reported only; the saved baseline may differ in mode.\n")
}
cat("Files created:\n")
for (path in c(sensitivity_path, trajectory_path)) {
  cat("  ", path, "\n", sep = "")
}

finish_script("05b_ca_loss_sensitivity.R", t0)
