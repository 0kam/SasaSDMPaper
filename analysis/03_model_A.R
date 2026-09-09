# =============================================================================
# 03_model_A.R -- Model A: environmental potential
# =============================================================================

t0 <- Sys.time()
source(file.path("analysis", "00_config.R"))
source(file.path(DIR_ANALYSIS, "R", "model_utils.R"))

suppressPackageStartupMessages({
  library(tidysdm)
  library(tidymodels)
  library(stacks)
  library(DALEX)
  library(DALEXtra)
})
set.seed(SEED_MODEL_A)

if (!file.exists(PATH_PREDICTORS)) stop("run analysis/01_predictors.R first")
predictors <- terra::rast(PATH_PREDICTORS)
if (!identical(names(predictors), PREDICTOR_NAMES)) {
  stop("unexpected predictor names: ", paste(names(predictors), collapse = ", "))
}
assert_on_ref_grid(predictors, "Model A predictors")
folds <- read_shared_folds()
vege21 <- terra::rast(PATH_VEGE_2021)

msg("Extracting the Model A response and deterministic 5 m pseudo-absences")
pred_values <- terra::values(predictors)
vege_values <- terra::values(vege21, mat = FALSE)
fold_values <- terra::values(folds, mat = FALSE)
valid <- complete.cases(pred_values) & !is.na(vege_values) & !is.na(fold_values)
presence_cells <- which(valid & vege_values == 1)
absence_cells <- which(valid & vege_values != 1)

# Exactly one pseudo-absence is retained from each aligned 5 x 5 m group.
rc <- terra::rowColFromCell(vege21, absence_cells)
n_group_cols <- ceiling(terra::ncol(vege21) / 5)
group_id <- ((rc[, 1] - 1L) %/% 5L) * n_group_cols +
  ((rc[, 2] - 1L) %/% 5L)
set.seed(SEED_MODEL_A)
random_order <- sample(seq_along(absence_cells))
keep_order <- random_order[!duplicated(group_id[random_order])]
pseudoabsence_cells <- sort(absence_cells[keep_order])
model_cells <- sort(c(presence_cells, pseudoabsence_cells))

base_data <- data.frame(
  cell = model_cells,
  response = factor(ifelse(vege_values[model_cells] == 1,
                           "presence", "absence"),
                    levels = c("presence", "absence")),
  fold_id = as.integer(fold_values[model_cells]),
  pred_values[model_cells, PREDICTOR_NAMES, drop = FALSE],
  check.names = FALSE
)

fit_variant <- function(variant, elevation_cutoff = NULL) {
  msg("Fitting Model A variant: ", variant)
  dat <- base_data
  if (!is.null(elevation_cutoff)) {
    dat <- dat[dat$elevation < elevation_cutoff, , drop = FALSE]
  }
  sample_rows <- stratified_smoke_sample(
    dat, dat$response, dat$fold_id, fraction = 0.10,
    seed = SEED_MODEL_A + match(variant, c("primary", "cut2560"))
  )
  dat <- dat[sample_rows, , drop = FALSE]
  model_data <- dat[, c("response", PREDICTOR_NAMES), drop = FALSE]
  cv <- make_shared_rset(model_data, dat$fold_id)
  rec <- recipes::recipe(response ~ ., data = model_data)

  specs <- list(
    rf = tidysdm::sdm_spec_rf() |>
      parsnip::set_args(num.threads = MODEL_THREADS),
    gam = tidysdm::sdm_spec_gam(),
    maxent = tidysdm::sdm_spec_maxent(),
    boost_tree = tidysdm::sdm_spec_boost_tree() |>
      parsnip::set_engine("xgboost", nthread = MODEL_THREADS)
  )
  workflows <- workflowsets::workflow_set(
    preproc = list(default = rec), models = specs, cross = TRUE
  ) |>
    workflowsets::update_workflow_model(
      "default_gam", spec = tidysdm::sdm_spec_gam(),
      formula = tidysdm::gam_formula(rec)
    ) |>
    workflowsets::option_add(control = stacks::control_stack_grid())

  set.seed(SEED_MODEL_A)
  candidates <- workflowsets::workflow_map(
    workflows, "tune_grid", resamples = cv,
    metrics = yardstick::metric_set(tidysdm::tss_max), grid = TUNE_GRID_A,
    verbose = TRUE
  )
  saveRDS(candidates, file.path(DIR_MODELS,
                                paste0("model_A_", variant, "_candidates.rds")))

  set.seed(SEED_MODEL_A)
  model_stack <- stacks::stacks() |>
    stacks::add_candidates(candidates) |>
    stacks::blend_predictions(metric = yardstick::metric_set(tidysdm::tss_max)) |>
    stacks::fit_members()
  saveRDS(model_stack, file.path(DIR_MODELS,
                                 paste0("model_A_", variant, "_stack.rds")))

  oof_order <- stack_oof_row_order(model_stack, candidates)
  if (!identical(as.character(model_stack$data_stack$response),
                 as.character(model_data$response[oof_order]))) {
    stop("stack OOF rows do not align with the Model A data")
  }
  oof_probability <- stack_oof_probability(model_stack)
  metrics <- blocked_metrics(
    as.integer(model_data$response[oof_order] == "presence"),
    oof_probability, dat$fold_id[oof_order]
  )
  metrics$variant <- variant

  composition <- stack_composition(model_stack)
  composition$variant <- variant
  if (nrow(composition) < 1L) stop("stack selected no members")

  pred_stack <- predictors
  if (!is.null(elevation_cutoff)) {
    domain_mask <- terra::ifel(predictors[["elevation"]] < elevation_cutoff,
                               1, NA)
    pred_stack <- predictors * domain_mask
    names(pred_stack) <- names(predictors)
  }
  suitability_path <- file.path(
    DIR_OUT, paste0("suitability_A_", variant, ".tif")
  )
  suitability <- predict_stack_raster(model_stack, pred_stack, suitability_path)

  cutoffs <- seq(0.1, 0.7, by = 0.05)
  threshold_sensitivity <- data.frame(
    variant = variant,
    cutoff = cutoffs,
    suitable_area_m2 = geodesic_threshold_areas(suitability, cutoffs)
  )

  pooled <- metrics[metrics$scope == "pooled", , drop = FALSE]
  thresholds <- data.frame(
    variant = variant,
    threshold = pooled$threshold,
    pooled_heldout_tss = pooled$tss,
    n_heldout_predictions = nrow(model_data)
  )

  # DALEXtra receives the same rows whose candidate probabilities were produced
  # out of fold. The final fitted stack is profiled over that held-out-consistent
  # covariate distribution; no independent response is claimed for PDPs.
  pdp_n <- min(nrow(model_data), if (SASA_SMOKE) 500L else 5000L)
  set.seed(SEED_MODEL_A)
  pdp_rows <- sample(seq_len(nrow(model_data)), pdp_n)
  pdp_data <- model_data[pdp_rows, PREDICTOR_NAMES, drop = FALSE]
  pdp_y <- as.integer(model_data$response[pdp_rows] == "presence")
  predict_probability <- function(model, newdata) {
    as.numeric(predict(model, newdata, type = "prob")$.pred_presence)
  }
  explainer <- DALEXtra::explain_tidymodels(
    model_stack, data = pdp_data, y = pdp_y,
    predict_function = predict_probability,
    label = paste("Model A", variant), type = "classification",
    verbose = FALSE, precalculate = FALSE
  )
  profiles <- DALEX::model_profile(
    explainer, variables = PREDICTOR_NAMES, N = pdp_n,
    type = "partial", center = FALSE, grid_points = 31
  )$agr_profiles
  for (predictor in PREDICTOR_NAMES) {
    plot_data <- profiles[profiles$`_vname_` == predictor, , drop = FALSE]
    p <- ggplot2::ggplot(plot_data,
                         ggplot2::aes(x = .data$`_x_`, y = .data$`_yhat_`)) +
      ggplot2::geom_line(linewidth = 0.8, colour = "#1B7837") +
      ggplot2::labs(x = predictor, y = "Partial-dependence suitability",
                    title = paste("Model A", variant, predictor)) +
      ggplot2::theme_bw(base_size = 11)
    ggplot2::ggsave(
      file.path(DIR_OUT, paste0("response_A_", variant, "_", predictor, ".png")),
      p, width = 5.5, height = 4, dpi = 300
    )
  }

  list(metrics = metrics, composition = composition,
       thresholds = thresholds,
       threshold_sensitivity = threshold_sensitivity,
       n = nrow(model_data), n_presence = sum(model_data$response == "presence"))
}

results <- list(
  fit_variant("primary"),
  fit_variant("cut2560", elevation_cutoff = 2560)
)

metrics <- do.call(rbind, lapply(results, `[[`, "metrics"))
composition <- do.call(rbind, lapply(results, `[[`, "composition"))
thresholds <- do.call(rbind, lapply(results, `[[`, "thresholds"))
threshold_sensitivity <- do.call(
  rbind, lapply(results, `[[`, "threshold_sensitivity")
)
utils::write.csv(metrics, file.path(DIR_OUT, "model_A_heldout_metrics.csv"),
                 row.names = FALSE)
utils::write.csv(composition,
                 file.path(DIR_OUT, "model_A_ensemble_composition.csv"),
                 row.names = FALSE)
utils::write.csv(thresholds, file.path(DIR_OUT, "model_A_thresholds.csv"),
                 row.names = FALSE)
utils::write.csv(threshold_sensitivity,
                 file.path(DIR_OUT, "model_A_threshold_sensitivity.csv"),
                 row.names = FALSE)
utils::write.csv(data.frame(
  variant = c("primary", "cut2560"),
  n_model = vapply(results, `[[`, integer(1), "n"),
  n_presence = vapply(results, `[[`, integer(1), "n_presence"),
  smoke = SASA_SMOKE
), file.path(DIR_OUT, "model_A_data_summary.csv"), row.names = FALSE)

msg("Model A pooled held-out metrics:")
print(metrics[metrics$scope == "pooled", c("variant", "auc", "tss", "threshold")])
finish_script("03_model_A.R", t0)
