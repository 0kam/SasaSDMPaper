# =============================================================================
# 04_model_B.R -- Model B: nine-year colonization GAM
# =============================================================================

t0 <- Sys.time()
source(file.path("analysis", "00_config.R"))
source(file.path(DIR_ANALYSIS, "R", "distance.R"))
source(file.path(DIR_ANALYSIS, "R", "model_utils.R"))

suppressPackageStartupMessages({
  library(mgcv)
  library(tidysdm)
  library(tidymodels)
  library(stacks)
})
set.seed(SEED_MODEL_B)

if (!file.exists(PATH_DIST12)) {
  stop("run analysis/02_folds_and_distance.R first")
}
predictors <- terra::rast(PATH_PREDICTORS)
dist12 <- terra::rast(PATH_DIST12)
names(dist12) <- "dist12"
assert_on_ref_grid(predictors, "Model B environmental predictors")
assert_on_ref_grid(dist12, "Model B dist12")
folds <- read_shared_folds()
vege12 <- terra::rast(PATH_VEGE_2012)
vege21 <- terra::rast(PATH_VEGE_2021)

msg("Extracting all cells that were non-Sasa in 2012")
pred_values <- terra::values(predictors)
dist_values <- terra::values(dist12, mat = FALSE)
v12 <- terra::values(vege12, mat = FALSE)
v21 <- terra::values(vege21, mat = FALSE)
fold_values <- terra::values(folds, mat = FALSE)
valid <- complete.cases(pred_values) & is.finite(dist_values) &
  !is.na(v12) & !is.na(v21) & !is.na(fold_values)
eligible_cells <- which(valid & v12 != 1)
full_response <- as.integer(v21[eligible_cells] == 1)
full_n_positive <- sum(full_response)

all_data <- data.frame(
  cell = eligible_cells,
  colonized = full_response,
  fold_id = as.integer(fold_values[eligible_cells]),
  dist12 = dist_values[eligible_cells],
  pred_values[eligible_cells, PREDICTOR_NAMES, drop = FALSE],
  check.names = FALSE
)
all_data$log1p_dist12 <- log1p(all_data$dist12)
sample_rows <- stratified_smoke_sample(
  all_data, all_data$colonized, all_data$fold_id, fraction = 0.10,
  seed = SEED_MODEL_B
)
dat <- all_data[sample_rows, , drop = FALSE]
model_predictors <- c("dist12", PREDICTOR_NAMES)
model_data <- dat[, c("colonized", "dist12", "log1p_dist12",
                      PREDICTOR_NAMES), drop = FALSE]

# k = 3 for distance encodes the deliberately low-complexity dispersal kernel:
# colonization positives are sparse, and higher bases produced unsupported
# oscillations between distant stands. k = 10 is retained for environmental
# smooths. The model remains strictly additive, with no tensor interactions.
gam_formula <- colonized ~
  s(dist12, k = 3) + s(snow_mean, k = 10) +
  s(elevation, k = 10) + s(slope, k = 10) +
  s(TPI, k = 10) + s(twi, k = 10) +
  s(northness, k = 10) + s(eastness, k = 10)
gam_formula_k5 <- stats::update.formula(
  gam_formula, . ~ . - s(dist12, k = 3) + s(dist12, k = 5)
)
gam_formula_k10 <- stats::update.formula(
  gam_formula, . ~ . - s(dist12, k = 3) + s(dist12, k = 10)
)
gam_formula_logdist <- stats::update.formula(
  gam_formula, . ~ . - s(dist12, k = 3) + s(log1p_dist12, k = 5)
)

msg("Leave-one-shared-block-out evaluation of the primary GAM")
eval_folds <- evaluation_fold_ids(dat$fold_id)
gam_oof <- rep(NA_real_, nrow(model_data))
for (k in sort(unique(eval_folds))) {
  train <- eval_folds != k
  heldout <- eval_folds == k
  set.seed(SEED_MODEL_B + k)
  fold_model <- mgcv::bam(
    gam_formula, data = model_data[train, , drop = FALSE],
    family = stats::binomial(), discrete = TRUE,
    nthreads = MODEL_THREADS, method = "fREML"
  )
  gam_oof[heldout] <- as.numeric(stats::predict(
    fold_model, newdata = model_data[heldout, , drop = FALSE],
    type = "response"
  ))
}
if (any(!is.finite(gam_oof))) stop("GAM produced non-finite OOF probabilities")
gam_metrics <- blocked_metrics(model_data$colonized, gam_oof, dat$fold_id)

msg("Leave-one-shared-block-out evaluation of the log-distance GAM")
logdist_oof <- rep(NA_real_, nrow(model_data))
for (k in sort(unique(eval_folds))) {
  train <- eval_folds != k
  heldout <- eval_folds == k
  set.seed(SEED_MODEL_B + k)
  fold_model <- mgcv::bam(
    gam_formula_logdist, data = model_data[train, , drop = FALSE],
    family = stats::binomial(), discrete = TRUE,
    nthreads = MODEL_THREADS, method = "fREML"
  )
  logdist_oof[heldout] <- as.numeric(stats::predict(
    fold_model, newdata = model_data[heldout, , drop = FALSE],
    type = "response"
  ))
}
if (any(!is.finite(logdist_oof))) {
  stop("log-distance GAM produced non-finite OOF probabilities")
}
logdist_metrics <- blocked_metrics(
  model_data$colonized, logdist_oof, dat$fold_id
)

msg("Fitting XGBoost tidysdm robustness companion")
# Maxent was removed from the robustness companion because near-separation on
# dist12 makes the maxnet/glmnet regularization path fail even at 1.5e5 rows.
# Observed failures were convergence errors -180/-184/-188, 79% NA OOF rows at
# blending, and a fatal error in fit_members().
positive_rows <- which(all_data$colonized == 1L)
negative_rows <- which(all_data$colonized == 0L)
negative_target <- if (SASA_SMOKE) {
  sum(dat$colonized == 0L)
} else {
  min(N_COMPANION_NEG, length(negative_rows))
}
negative_by_fold <- split(negative_rows, all_data$fold_id[negative_rows])
negative_sizes <- lengths(negative_by_fold)
raw_quota <- as.double(negative_target) * as.double(negative_sizes) /
  sum(negative_sizes)
negative_quota <- floor(raw_quota)
n_extra <- negative_target - sum(negative_quota)
if (n_extra > 0L) {
  extra_folds <- order(raw_quota - negative_quota, decreasing = TRUE)[
    seq_len(n_extra)
  ]
  negative_quota[extra_folds] <- negative_quota[extra_folds] + 1L
}
set.seed(SEED_MODEL_B)
sampled_negative_rows <- unlist(Map(function(rows, n) {
  if (n == 0L) integer(0) else rows[sample.int(length(rows), n)]
}, negative_by_fold, negative_quota), use.names = FALSE)
companion_rows <- sort(c(positive_rows, sampled_negative_rows))
companion_data <- all_data[companion_rows, , drop = FALSE]
if (sum(companion_data$colonized == 1L) != full_n_positive ||
    sum(companion_data$colonized == 0L) != negative_target) {
  stop("Model B companion sampling did not retain the requested rows")
}
sampling_folds <- sort(unique(companion_data$fold_id))
companion_sampling <- do.call(rbind, lapply(sampling_folds, function(k) {
  in_fold <- companion_data$fold_id == k
  data.frame(
    fold_id = k,
    n_positive = sum(in_fold & companion_data$colonized == 1L),
    n_negative = sum(in_fold & companion_data$colonized == 0L),
    seed = SEED_MODEL_B
  )
}))
utils::write.csv(
  companion_sampling,
  file.path(DIR_OUT, "model_B_companion_sampling.csv"),
  row.names = FALSE
)

robust_data <- companion_data[, c("colonized", model_predictors), drop = FALSE]
robust_data$colonized <- factor(
  ifelse(robust_data$colonized == 1L, "presence", "absence"),
  levels = c("presence", "absence")
)
robust_cv <- make_shared_rset(robust_data, companion_data$fold_id)
robust_recipe <- recipes::recipe(colonized ~ ., data = robust_data)
robust_workflows <- workflowsets::workflow_set(
  preproc = list(default = robust_recipe),
  models = list(
    boost_tree = tidysdm::sdm_spec_boost_tree() |>
      parsnip::set_engine("xgboost", nthread = MODEL_THREADS)
  ),
  cross = TRUE
) |>
  workflowsets::option_add(control = stacks::control_stack_grid())

set.seed(SEED_MODEL_B)
robust_candidates <- workflowsets::workflow_map(
  robust_workflows, "tune_grid", resamples = robust_cv,
  metrics = yardstick::metric_set(yardstick::roc_auc), grid = TUNE_GRID_B,
  verbose = TRUE
)
saveRDS(robust_candidates,
        file.path(DIR_MODELS, "model_B_robustness_candidates.rds"))
set.seed(SEED_MODEL_B)
robust_stack <- stacks::stacks() |>
  stacks::add_candidates(robust_candidates) |>
  stacks::blend_predictions(metric = yardstick::metric_set(yardstick::roc_auc)) |>
  stacks::fit_members()
saveRDS(robust_stack,
        file.path(DIR_MODELS, "model_B_robustness_stack.rds"))

robust_order <- stack_oof_row_order(robust_stack, robust_candidates)
if (!identical(as.character(robust_stack$data_stack$colonized),
               as.character(robust_data$colonized[robust_order]))) {
  stop("stack OOF rows do not align with the Model B robustness data")
}
robust_oof <- rep(NA_real_, nrow(robust_data))
robust_oof[robust_order] <- stack_oof_probability(robust_stack)
robust_metrics <- blocked_metrics(
  companion_data$colonized, robust_oof, companion_data$fold_id
)

side_by_side <- gam_metrics
names(side_by_side)[names(side_by_side) %in% c("auc", "tss", "threshold")] <-
  c("gam_auc", "gam_tss", "gam_threshold")
side_by_side$tidysdm_auc <- robust_metrics$auc
side_by_side$tidysdm_tss <- robust_metrics$tss
side_by_side$tidysdm_threshold <- robust_metrics$threshold
side_by_side$companion_n <- robust_metrics$n
utils::write.csv(side_by_side,
                 file.path(DIR_OUT, "model_B_blocked_metrics.csv"),
                 row.names = FALSE)

msg("Fitting the final all-eligible-cell GAM for inference")
set.seed(SEED_MODEL_B)
final_gam <- mgcv::bam(
  gam_formula, data = model_data,
  family = stats::binomial(), discrete = TRUE,
  nthreads = MODEL_THREADS, method = "fREML"
)
saveRDS(final_gam, PATH_MODEL_B_PRIMARY)

msg("Fitting the final log-distance GAM variant")
set.seed(SEED_MODEL_B)
logdist_gam <- mgcv::bam(
  gam_formula_logdist, data = model_data,
  family = stats::binomial(), discrete = TRUE,
  nthreads = MODEL_THREADS, method = "fREML"
)
saveRDS(logdist_gam, PATH_MODEL_B_LOGDIST)

msg("Fitting k = 5 and k = 10 dispersal-kernel sensitivity GAMs")
set.seed(SEED_MODEL_B)
sensitivity_gam_k5 <- mgcv::bam(
  gam_formula_k5, data = model_data,
  family = stats::binomial(), discrete = TRUE,
  nthreads = MODEL_THREADS, method = "fREML"
)
saveRDS(sensitivity_gam_k5,
        file.path(DIR_MODELS, "model_B_gam_k5.rds"))
set.seed(SEED_MODEL_B)
sensitivity_gam_k10 <- mgcv::bam(
  gam_formula_k10, data = model_data,
  family = stats::binomial(), discrete = TRUE,
  nthreads = MODEL_THREADS, method = "fREML"
)
saveRDS(sensitivity_gam_k10,
        file.path(DIR_MODELS, "model_B_gam_k10.rds"))

msg("Exporting every GAM smooth")
grDevices::pdf(NULL)
smooth_data <- mgcv::plot.gam(final_gam, pages = 0, se = TRUE)
grDevices::dev.off()
for (i in seq_along(smooth_data)) {
  sm <- smooth_data[[i]]
  term <- final_gam$smooth[[i]]$label
  plot_data <- data.frame(x = as.numeric(sm$x), fit = as.numeric(sm$fit),
                          se = as.numeric(sm$se))
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$x, y = .data$fit)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data$fit - 2 * .data$se,
                   ymax = .data$fit + 2 * .data$se),
      fill = "#9ECAE1", alpha = 0.5
    ) +
    ggplot2::geom_line(linewidth = 0.8, colour = "#08519C") +
    ggplot2::geom_hline(yintercept = 0, linetype = 2, colour = "grey45") +
    ggplot2::labs(x = sm$xlab, y = "Partial effect (logit scale)", title = term) +
    ggplot2::theme_bw(base_size = 11)
  safe_term <- gsub("[^A-Za-z0-9]+", "_", term)
  ggplot2::ggsave(file.path(DIR_OUT, paste0("smooth_B_", safe_term, ".png")),
                  p, width = 5.5, height = 4, dpi = 300)
}

msg("Exporting the 0-400 m nine-year dispersal kernel")
medians <- vapply(model_data[, PREDICTOR_NAMES, drop = FALSE],
                  stats::median, numeric(1), na.rm = TRUE)
kernel <- data.frame(dist12 = seq(0, 400, by = 5))
kernel$log1p_dist12 <- log1p(kernel$dist12)
for (nm in PREDICTOR_NAMES) kernel[[nm]] <- medians[[nm]]
kernel$p9_k3 <- as.numeric(stats::predict(final_gam, newdata = kernel,
                                          type = "response"))
kernel$p9_k5 <- as.numeric(stats::predict(sensitivity_gam_k5, newdata = kernel,
                                          type = "response"))
kernel$p9_k10 <- as.numeric(stats::predict(sensitivity_gam_k10, newdata = kernel,
                                           type = "response"))
kernel$p9_logdist <- as.numeric(stats::predict(
  logdist_gam, newdata = kernel, type = "response"
))
utils::write.csv(kernel[, c("dist12", "p9_k3", "p9_k5", "p9_k10",
                            "p9_logdist",
                            PREDICTOR_NAMES)],
                 file.path(DIR_OUT, "model_B_distance_kernel.csv"),
                 row.names = FALSE)

msg("Comparing blocked metrics and observed-transition distance bands")
comparison_row <- function(
    section, model = NA_character_, scope = NA_character_, fold = NA_integer_,
    n = NA_integer_, auc = NA_real_, tss = NA_real_, threshold = NA_real_,
    distance_band_m = NA_character_, n_cells = NA_integer_,
    observed_colonized_cells = NA_integer_,
    observed_colonization_rate = NA_real_, predicted_mean_p9 = NA_real_,
    absolute_error = NA_real_, criterion = NA_character_,
    primary_value = NA_real_, logdist_value = NA_real_,
    comparison_value = NA_real_, required_value = NA_real_,
    passed = NA, verdict = NA_character_) {
  data.frame(
    section, model, scope, fold, n, auc, tss, threshold, distance_band_m,
    n_cells, observed_colonized_cells, observed_colonization_rate,
    predicted_mean_p9,
    absolute_error, criterion, primary_value, logdist_value,
    comparison_value, required_value, passed, verdict,
    stringsAsFactors = FALSE
  )
}

comparison_rows <- list()
add_metric_rows <- function(metrics, model_name) {
  metrics <- metrics[metrics$scope %in% c("fold", "pooled"), , drop = FALSE]
  lapply(seq_len(nrow(metrics)), function(i) comparison_row(
    section = "blocked_metrics", model = model_name,
    scope = metrics$scope[[i]], fold = metrics$fold[[i]], n = metrics$n[[i]],
    auc = metrics$auc[[i]], tss = metrics$tss[[i]],
    threshold = metrics$threshold[[i]]
  ))
}
comparison_rows <- c(
  comparison_rows,
  add_metric_rows(gam_metrics, "primary"),
  add_metric_rows(logdist_metrics, "logdist")
)

transition_newdata <- all_data[, c(
  "dist12", "log1p_dist12", PREDICTOR_NAMES
), drop = FALSE]
transition_prediction <- list(
  primary = as.numeric(stats::predict(
    final_gam, newdata = transition_newdata, type = "response"
  )),
  logdist = as.numeric(stats::predict(
    logdist_gam, newdata = transition_newdata, type = "response"
  ))
)
if (any(!is.finite(transition_prediction$primary)) ||
    any(!is.finite(transition_prediction$logdist))) {
  stop("non-finite fitted probability in the distance-band comparison")
}
band_breaks <- c(0, 5, 10, 20, 40, 80, 160, Inf)
band_labels <- c("[0,5)", "[5,10)", "[10,20)", "[20,40)",
                 "[40,80)", "[80,160)", ">=160")
transition_band <- cut(
  all_data$dist12, breaks = band_breaks, right = FALSE,
  include.lowest = TRUE, labels = band_labels
)
band_tables <- lapply(names(transition_prediction), function(model_name) {
  do.call(rbind, lapply(band_labels, function(label) {
    in_band <- transition_band == label
    observed <- mean(all_data$colonized[in_band])
    predicted <- mean(transition_prediction[[model_name]][in_band])
    data.frame(
      model = model_name,
      distance_band_m = label,
      n_cells = sum(in_band),
      observed_colonized_cells = sum(all_data$colonized[in_band]),
      observed_colonization_rate = observed,
      predicted_mean_p9 = predicted,
      absolute_error = abs(predicted - observed),
      stringsAsFactors = FALSE
    )
  }))
})
names(band_tables) <- names(transition_prediction)
for (band_table in band_tables) {
  comparison_rows <- c(comparison_rows, lapply(seq_len(nrow(band_table)), function(i) {
    comparison_row(
      section = "distance_band", model = band_table$model[[i]],
      n_cells = band_table$n_cells[[i]],
      distance_band_m = band_table$distance_band_m[[i]],
      observed_colonized_cells = band_table$observed_colonized_cells[[i]],
      observed_colonization_rate =
        band_table$observed_colonization_rate[[i]],
      predicted_mean_p9 = band_table$predicted_mean_p9[[i]],
      absolute_error = band_table$absolute_error[[i]]
    )
  }))
}

primary_auc <- gam_metrics$auc[gam_metrics$scope == "pooled"]
logdist_auc <- logdist_metrics$auc[logdist_metrics$scope == "pooled"]
auc_degradation <- primary_auc - logdist_auc
criterion_auc <- auc_degradation <= 0.005

front_primary_error <- band_tables$primary$absolute_error[
  band_tables$primary$distance_band_m == "[0,5)"
]
front_logdist_error <- band_tables$logdist$absolute_error[
  band_tables$logdist$distance_band_m == "[0,5)"
]
front_improvement <- if (front_primary_error > 0) {
  (front_primary_error - front_logdist_error) / front_primary_error
} else if (front_logdist_error == 0) {
  1
} else {
  -Inf
}
criterion_front <- front_improvement >= 0.30

mid_labels <- c("[5,10)", "[10,20)")
mid_primary_error <- sum(band_tables$primary$absolute_error[
  band_tables$primary$distance_band_m %in% mid_labels
])
mid_logdist_error <- sum(band_tables$logdist$absolute_error[
  band_tables$logdist$distance_band_m %in% mid_labels
])
mid_error_change <- mid_logdist_error - mid_primary_error
criterion_mid <- mid_logdist_error <= mid_primary_error
verdict <- if (criterion_auc && criterion_front && criterion_mid) {
  "ADOPT"
} else {
  "REJECT"
}

comparison_rows <- c(comparison_rows, list(
  comparison_row(
    section = "decision", criterion = "pooled_auc_degradation_le_0.005",
    primary_value = primary_auc, logdist_value = logdist_auc,
    comparison_value = auc_degradation, required_value = 0.005,
    passed = criterion_auc
  ),
  comparison_row(
    section = "decision", criterion = "front_abs_error_improvement_ge_0.30",
    primary_value = front_primary_error, logdist_value = front_logdist_error,
    comparison_value = front_improvement, required_value = 0.30,
    passed = criterion_front
  ),
  comparison_row(
    section = "decision", criterion = "mid_band_aggregate_not_worse",
    primary_value = mid_primary_error, logdist_value = mid_logdist_error,
    comparison_value = mid_error_change, required_value = 0,
    passed = criterion_mid
  ),
  comparison_row(
    section = "decision", criterion = "verdict", passed = all(
      criterion_auc, criterion_front, criterion_mid
    ), verdict = verdict
  )
))
logdist_comparison <- do.call(rbind, comparison_rows)
utils::write.csv(
  logdist_comparison, PATH_MODEL_B_LOGDIST_COMPARISON, row.names = FALSE,
  na = ""
)

msg("Predicting p9 for 2021 non-Sasa cells with distance recomputed from 2021")
dist21 <- sasa_distance(vege21, min_area_m2 = 5)
names(dist21) <- "dist12"
log1p_dist21 <- log1p(dist21)
names(log1p_dist21) <- "log1p_dist12"
inference_stack <- c(dist21, log1p_dist21, predictors)
names(inference_stack) <- c("dist12", "log1p_dist12", PREDICTOR_NAMES)
inference_gam <- if (identical(verdict, "ADOPT")) logdist_gam else final_gam
gam_predict <- function(model, data) {
  as.numeric(stats::predict(model, newdata = data, type = "response"))
}
p9 <- terra::predict(
  inference_stack, inference_gam, fun = gam_predict,
  wopt = list(datatype = "FLT4S", gdal = "COMPRESS=DEFLATE")
)
non_sasa21 <- terra::ifel(vege21 != 1, 1, NA)
p9 <- terra::mask(p9, non_sasa21)
p9 <- terra::clamp(p9, lower = 0, upper = 1, values = TRUE)
names(p9) <- "p9"
assert_on_ref_grid(p9, "p9 colonization")
terra::writeRaster(
  p9, file.path(DIR_OUT, "p9_colonization.tif"), overwrite = TRUE,
  datatype = "FLT4S", gdal = "COMPRESS=DEFLATE"
)

utils::write.csv(data.frame(
  n_eligible_full = length(eligible_cells),
  n_positive_full = full_n_positive,
  n_model = nrow(model_data),
  n_positive_model = sum(model_data$colonized),
  smoke = SASA_SMOKE
), file.path(DIR_OUT, "model_B_data_summary.csv"), row.names = FALSE)

msg("Model B blocked metrics:")
print(side_by_side)
msg("Kernel Spearman rho (0-200 m): ", sprintf("%.4f", stats::cor(
  kernel$dist12[kernel$dist12 <= 200],
  kernel[[if (identical(verdict, "ADOPT")) "p9_logdist" else "p9_k3"]][
    kernel$dist12 <= 200
  ],
  method = "spearman"
)))
cat("\n=== PATCH4 log-distance execution summary ===\n")
cat(sprintf(
  "Criterion (a): pooled blocked AUC primary %.6f, logdist %.6f; degradation %.6f <= 0.005: %s\n",
  primary_auc, logdist_auc, auc_degradation,
  if (criterion_auc) "PASS" else "FAIL"
))
cat(sprintf(
  "Criterion (b): [0,5) absolute error primary %.6f, logdist %.6f; improvement %.2f%% >= 30%%: %s\n",
  front_primary_error, front_logdist_error, 100 * front_improvement,
  if (criterion_front) "PASS" else "FAIL"
))
cat(sprintf(
  "Criterion (c): [5,20) aggregate absolute error primary %.6f, logdist %.6f; change %.6f <= 0: %s\n",
  mid_primary_error, mid_logdist_error, mid_error_change,
  if (criterion_mid) "PASS" else "FAIL"
))
cat("Verdict:", verdict, "\n")
cat("Files created:\n")
for (path in c(
  PATH_MODEL_B_LOGDIST,
  PATH_MODEL_B_LOGDIST_COMPARISON,
  file.path(DIR_OUT, "model_B_distance_kernel.csv"),
  file.path(DIR_OUT, "p9_colonization.tif")
)) cat("  ", path, "\n", sep = "")
cat(
  "Deviations:",
  if (SASA_SMOKE) "none; configured SMOKE sampling/CV used; full mode not run" else "none",
  "\n"
)
finish_script("04_model_B.R", t0)
