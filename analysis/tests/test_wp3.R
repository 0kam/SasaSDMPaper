# Acceptance tests for WP3 / Model B.

t0 <- Sys.time()
source(file.path("analysis", "00_config.R"))
checker <- new_checker("WP3 acceptance")

summary_path <- file.path(DIR_OUT, "model_B_data_summary.csv")
checker$check("Model B data summary exists", file.exists(summary_path), summary_path)
data_summary <- NULL
if (file.exists(summary_path)) {
  data_summary <- utils::read.csv(summary_path)
  checker$check_in("full-data colonization positives",
                   data_summary$n_positive_full[[1]], c(4000, 4200))
}

sampling_path <- file.path(DIR_OUT, "model_B_companion_sampling.csv")
checker$check("companion sampling CSV exists", file.exists(sampling_path),
              sampling_path)
if (file.exists(sampling_path) && !is.null(data_summary)) {
  companion_sampling <- utils::read.csv(sampling_path)
  checker$check(
    "companion retains every full-data positive",
    "n_positive" %in% names(companion_sampling) &&
      sum(companion_sampling$n_positive) == data_summary$n_positive_full[[1]],
    sprintf("companion %d; full data %d",
            sum(companion_sampling$n_positive),
            data_summary$n_positive_full[[1]])
  )
}

metrics_path <- file.path(DIR_OUT, "model_B_blocked_metrics.csv")
checker$check("blocked metrics CSV exists", file.exists(metrics_path), metrics_path)
if (file.exists(metrics_path)) {
  metrics <- utils::read.csv(metrics_path)
  pooled <- metrics[metrics$scope == "pooled", , drop = FALSE]
  checker$check("pooled blocked GAM AUC reported",
                nrow(pooled) == 1L && is.finite(pooled$gam_auc))
  if (nrow(pooled) == 1L) {
    cat(sprintf("BLOCKED GAM AUC %.4f (expected context approximately 0.75-0.90; not gated)\n",
                pooled$gam_auc))
    if (pooled$gam_auc < 0.75 || pooled$gam_auc > 0.90) {
      cat("*** NOTE: BLOCKED AUC IS OUTSIDE THE EXPECTED CONTEXT RANGE; TEST REMAINS NON-GATING ***\n")
    }
  }
  checker$check("GAM and tidysdm AUC reported side by side",
                all(c("gam_auc", "tidysdm_auc") %in% names(metrics)) &&
                  all(is.finite(metrics$gam_auc)) && all(is.finite(metrics$tidysdm_auc)))
}

comparison_path <- PATH_MODEL_B_LOGDIST_COMPARISON
checker$check("log-distance comparison CSV exists", file.exists(comparison_path),
              comparison_path)
logdist_verdict <- NA_character_
if (file.exists(comparison_path)) {
  comparison <- utils::read.csv(comparison_path, stringsAsFactors = FALSE)
  verdict_rows <- comparison[
    comparison$section == "decision" & comparison$criterion == "verdict",
    , drop = FALSE
  ]
  verdict_values <- unique(verdict_rows$verdict[
    !is.na(verdict_rows$verdict) & nzchar(verdict_rows$verdict)
  ])
  checker$check(
    "log-distance verdict line is present",
    nrow(verdict_rows) == 1L && length(verdict_values) == 1L &&
      verdict_values %in% c("ADOPT", "REJECT"),
    if (length(verdict_values)) paste(verdict_values, collapse = ",") else "missing"
  )
  if (length(verdict_values) == 1L) logdist_verdict <- verdict_values[[1]]

  criteria <- comparison[
    comparison$section == "decision" & comparison$criterion != "verdict",
    , drop = FALSE
  ]
  checker$check(
    "all three log-distance decision criteria are reported",
    nrow(criteria) == 3L && all(!is.na(criteria$passed))
  )
}

kernel_path <- file.path(DIR_OUT, "model_B_distance_kernel.csv")
checker$check("distance kernel CSV exists", file.exists(kernel_path), kernel_path)
if (file.exists(kernel_path)) {
  kernel <- utils::read.csv(kernel_path)
  checker$check("kernel covers 0-400 m by 5 m",
                isTRUE(all.equal(kernel$dist12, seq(0, 400, by = 5),
                                 tolerance = 0)))
  near <- kernel$dist12 <= 200
  primary_name <- if (identical(logdist_verdict, "ADOPT")) {
    "p9_logdist"
  } else if ("p9_k3" %in% names(kernel)) {
    "p9_k3"
  } else {
    "p9"
  }
  rho <- if (primary_name %in% names(kernel)) {
    stats::cor(kernel$dist12[near], kernel[[primary_name]][near],
               method = "spearman")
  } else {
    NA_real_
  }
  checker$check("adopted distance effect is monotone non-increasing over 0-200 m",
                is.finite(rho) && rho < -0.9,
                sprintf("%s Spearman rho = %.4f", primary_name, rho))

  checker$check("log-distance kernel column exists",
                "p9_logdist" %in% names(kernel))

  sensitivity_names <- c("p9_k3", "p9_k5", "p9_k10")
  has_sensitivity <- all(sensitivity_names %in% names(kernel))
  checker$check("all three kernel sensitivity columns exist", has_sensitivity)
  if (has_sensitivity) {
    sensitivity_rho <- stats::cor(
      kernel[near, sensitivity_names], method = "spearman"
    )
    pairwise_rho <- sensitivity_rho[upper.tri(sensitivity_rho)]
    rho_detail <- paste(sprintf("%.4f", pairwise_rho), collapse = ", ")
    same_story <- all(is.finite(pairwise_rho)) && all(pairwise_rho > 0.95)
    checker$check("kernel pairwise Spearman correlations checked (non-gating)",
                  TRUE, rho_detail)
    if (!same_story) {
      cat(paste0(
        "*** WARNING: KERNEL K-SENSITIVITY SPEARMAN CORRELATION IS NOT > 0.95 ",
        "PAIRWISE (", rho_detail, "); TEST REMAINS NON-GATING ***\n"
      ))
    }
  }
}

p9_path <- file.path(DIR_OUT, "p9_colonization.tif")
checker$check("p9 colonization raster exists", file.exists(p9_path), p9_path)
if (file.exists(p9_path)) {
  p9 <- terra::rast(p9_path)
  grid_ok <- tryCatch({
    assert_on_ref_grid(p9, "p9 colonization"); TRUE
  }, error = function(e) FALSE)
  checker$check("p9 is on the bit-exact reference grid", grid_ok)
  limits <- as.numeric(terra::global(p9, range, na.rm = TRUE)[1, ])
  checker$check("p9 values are in [0,1]",
                all(is.finite(limits)) && limits[[1]] >= 0 && limits[[2]] <= 1,
                sprintf("range [%.6f, %.6f]", limits[[1]], limits[[2]]))

  v21 <- terra::values(terra::rast(PATH_VEGE_2021), mat = FALSE)
  predictor_values <- terra::values(terra::rast(PATH_PREDICTORS))
  expected_na <- (v21 == 1) | !complete.cases(predictor_values)
  actual_na <- is.na(terra::values(p9, mat = FALSE))
  checker$check("p9 NA mask exactly matches Sasa-2021 or predictor NA",
                identical(actual_na, expected_na),
                sprintf("actual %d; expected %d", sum(actual_na), sum(expected_na)))
}

checker$check("final GAM model object exists",
              file.exists(PATH_MODEL_B_PRIMARY))
checker$check("log-distance GAM model object exists",
              file.exists(PATH_MODEL_B_LOGDIST))
checker$check("k = 5 sensitivity GAM model object exists",
              file.exists(file.path(DIR_MODELS, "model_B_gam_k5.rds")))
checker$check("k = 10 sensitivity GAM model object exists",
              file.exists(file.path(DIR_MODELS, "model_B_gam_k10.rds")))
smooth_files <- list.files(DIR_OUT, pattern = "^smooth_B_.*[.]png$", full.names = TRUE)
checker$check("all eight GAM smooth plots exist", length(smooth_files) == 8L,
              paste(length(smooth_files), "files"))

checker$report()
finish_script("tests/test_wp3.R", t0)
