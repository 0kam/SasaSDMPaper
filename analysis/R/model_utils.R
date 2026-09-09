# Shared data, resampling, prediction and evaluation utilities for WP2/WP3.

read_shared_folds <- function() {
  if (!file.exists(PATH_FOLDS_RASTER)) {
    stop("shared fold raster is missing; run analysis/02_folds_and_distance.R")
  }
  folds <- terra::rast(PATH_FOLDS_RASTER)
  assert_on_ref_grid(folds, "shared folds")
  folds
}

# Smoke mode uses two assessment groups but derives them only from the persisted
# four-fold artefact. The four-fold IDs remain unchanged for WP4 reuse.
evaluation_fold_ids <- function(fold_id) {
  fold_id <- as.integer(fold_id)
  if (SASA_SMOKE) ((fold_id - 1L) %% CV_FOLDS_SMOKE) + 1L else fold_id
}

make_shared_rset <- function(data, fold_id) {
  fold_id <- evaluation_fold_ids(fold_id)
  ids <- sort(unique(fold_id[is.finite(fold_id)]))
  expected <- if (SASA_SMOKE) CV_FOLDS_SMOKE else CV_FOLDS_FULL
  if (!identical(ids, seq_len(expected))) {
    stop("fold IDs are not complete: got ", paste(ids, collapse = ","))
  }
  splits <- lapply(ids, function(k) {
    assessment <- which(fold_id == k)
    analysis <- which(fold_id != k & !is.na(fold_id))
    rsample::make_splits(list(analysis = analysis, assessment = assessment), data)
  })
  rsample::manual_rset(splits, paste0("Fold", ids))
}

stratified_smoke_sample <- function(data, response, fold_id, fraction = 0.10,
                                    seed = SEED_GLOBAL) {
  if (!SASA_SMOKE) return(seq_len(nrow(data)))
  set.seed(seed)
  strata <- interaction(response, evaluation_fold_ids(fold_id), drop = TRUE)
  selected <- unlist(lapply(split(seq_len(nrow(data)), strata), function(i) {
    sample(i, max(1L, round(length(i) * fraction)), replace = FALSE)
  }), use.names = FALSE)
  sort(selected)
}

max_tss <- function(truth, probability) {
  truth <- as.integer(truth)
  ok <- !is.na(truth) & is.finite(probability)
  truth <- truth[ok]
  probability <- probability[ok]
  n_pos <- sum(truth == 1L)
  n_neg <- sum(truth == 0L)
  if (n_pos == 0L || n_neg == 0L) {
    return(c(threshold = NA_real_, tss = NA_real_))
  }
  ord <- order(probability, decreasing = TRUE)
  probability <- probability[ord]
  truth <- truth[ord]
  last_at_score <- !duplicated(probability, fromLast = TRUE)
  sensitivity <- cumsum(truth == 1L)[last_at_score] / n_pos
  specificity <- 1 - cumsum(truth == 0L)[last_at_score] / n_neg
  score <- sensitivity + specificity - 1
  j <- which.max(score)
  c(threshold = probability[last_at_score][j], tss = score[j])
}

auc_value <- function(truth, probability) {
  truth <- factor(ifelse(as.integer(truth) == 1L, "presence", "absence"),
                  levels = c("presence", "absence"))
  as.numeric(yardstick::roc_auc_vec(truth, probability,
                                    event_level = "first"))
}

binary_metrics <- function(truth, probability) {
  mt <- max_tss(truth, probability)
  c(auc = auc_value(truth, probability),
    tss = unname(mt[["tss"]]),
    threshold = unname(mt[["threshold"]]))
}

blocked_metrics <- function(truth, probability, fold_id) {
  fold_id <- evaluation_fold_ids(fold_id)
  per_fold <- do.call(rbind, lapply(sort(unique(fold_id)), function(k) {
    z <- binary_metrics(truth[fold_id == k], probability[fold_id == k])
    data.frame(scope = "fold", fold = k, n = sum(fold_id == k),
               auc = z[["auc"]], tss = z[["tss"]],
               threshold = z[["threshold"]])
  }))
  pooled <- binary_metrics(truth, probability)
  mean_row <- data.frame(scope = "mean", fold = NA_integer_, n = length(truth),
                         auc = mean(per_fold$auc), tss = mean(per_fold$tss),
                         threshold = NA_real_)
  pooled_row <- data.frame(scope = "pooled", fold = NA_integer_, n = length(truth),
                           auc = pooled[["auc"]], tss = pooled[["tss"]],
                           threshold = pooled[["threshold"]])
  rbind(per_fold, mean_row, pooled_row)
}

stack_oof_probability <- function(model_stack) {
  # data_stack consists of candidate predictions made on each assessment fold.
  # Applying the fitted non-negative blend equation gives pooled OOF ensemble
  # probabilities without predicting final members on their own training data.
  pred <- get("stack_predict", asNamespace("stacks"))(
    model_stack$equations$prob, model_stack$data_stack
  )
  as.numeric(pred$.pred_presence)
}

stack_oof_row_order <- function(model_stack, candidates) {
  # stacks pivots the first candidate's summarized predictions and preserves
  # their first-occurrence .row order (which is not the original data order).
  first_result <- workflowsets::extract_workflow_set_result(
    candidates, candidates$wflow_id[[1]]
  )
  order <- unique(tune::collect_predictions(first_result, summarize = TRUE)$.row)
  if (length(order) != nrow(model_stack$data_stack) ||
      !identical(sort(order), seq_len(nrow(model_stack$train)))) {
    stop("could not recover one OOF prediction row per training observation")
  }
  order
}

stack_composition <- function(model_stack) {
  top <- get("top_coefs", asNamespace("stacks"))(
    model_stack, n = length(model_stack$member_fits)
  )
  data.frame(member = sub("^[.]pred_(presence|absence)_", "", top$member),
             algorithm = as.character(top$type),
             weight = as.numeric(top$weight), stringsAsFactors = FALSE)
}

predict_stack_raster <- function(model_stack, predictors, filename,
                                 overwrite = TRUE) {
  pred_fun <- function(model, data) {
    as.numeric(predict(model, data, type = "prob")$.pred_presence)
  }
  out <- terra::predict(predictors, model_stack, fun = pred_fun,
                        na.rm = TRUE,
                        filename = filename, overwrite = overwrite,
                        wopt = list(datatype = "FLT4S", gdal = "COMPRESS=DEFLATE"))
  names(out) <- "probability"
  assert_on_ref_grid(out, basename(filename))
  out
}

geodesic_threshold_areas <- function(probability_raster, cutoffs) {
  cutoffs <- sort(as.numeric(cutoffs))
  step <- unique(round(diff(cutoffs), 12))
  if (length(step) != 1L || step <= 0) stop("cutoffs must be equally spaced")
  # A zone's integer value is the number of thresholds it exceeds. One call to
  # expanse therefore gives exact geodesic bin areas for every requested cutoff.
  zone <- floor((probability_raster - cutoffs[[1]]) / step) + 1
  zone <- terra::clamp(zone, lower = 1, upper = length(cutoffs), values = TRUE)
  zone <- terra::mask(zone, probability_raster >= cutoffs[[1]], maskvalues = 0)
  area <- terra::expanse(zone, unit = "m", transform = TRUE, byValue = TRUE)
  bin_area <- stats::setNames(rep(0, length(cutoffs)), seq_along(cutoffs))
  if (nrow(area)) bin_area[as.character(area$value)] <- area$area
  vapply(seq_along(cutoffs), function(j) sum(bin_area[j:length(bin_area)]),
         numeric(1))
}
