# Acceptance tests for WP4 / annual cellular-automaton projection.

t0 <- Sys.time()
source(file.path("analysis", "00_config.R"))
checker <- new_checker("WP4 acceptance")

expected_scenarios <- if (SASA_SMOKE) {
  c("s0", "sm071")
} else {
  c("s0", "sm071", "sm224")
}

# ---- exact GAM decomposition gate ------------------------------------------
decomposition_path <- file.path(DIR_OUT, "ca_decomposition_validation.csv")
checker$check("decomposition validation CSV exists",
              file.exists(decomposition_path), decomposition_path)
if (file.exists(decomposition_path)) {
  decomposition <- utils::read.csv(decomposition_path)
  checker$check(
    "10,000-cell decomposition max |delta p9| < 1e-6",
    nrow(decomposition) == 1L && decomposition$n_cells[[1]] == 10000L &&
      is.finite(decomposition$max_abs_delta_p9[[1]]) &&
      decomposition$max_abs_delta_p9[[1]] < 1e-6,
    sprintf("max |delta p9| = %.9g", decomposition$max_abs_delta_p9[[1]])
  )
}

# ---- 2030 probability and example rasters ---------------------------------
vege21_values <- terra::values(terra::rast(PATH_VEGE_2021), mat = FALSE)
predictor_values <- terra::values(terra::rast(PATH_PREDICTORS))
eligible <- complete.cases(predictor_values) &
  !is.na(vege21_values) & vege21_values != 1
initial_sasa <- !is.na(vege21_values) & vege21_values == 1
never_eligible <- !eligible & !initial_sasa

for (tag in expected_scenarios) {
  probability_path <- file.path(DIR_OUT, paste0("ca_pcol_2030_", tag, ".tif"))
  example_path <- file.path(DIR_OUT, paste0("ca_example_2030_", tag, ".tif"))
  checker$check(paste("probability raster exists:", tag),
                file.exists(probability_path), probability_path)
  checker$check(paste("example realization exists:", tag),
                file.exists(example_path), example_path)
  if (!file.exists(probability_path)) next

  probability <- terra::rast(probability_path)
  grid_ok <- tryCatch({
    assert_on_ref_grid(probability, paste("WP4 probability", tag)); TRUE
  }, error = function(e) FALSE)
  checker$check(paste("probability raster on reference grid:", tag), grid_ok)
  value <- terra::values(probability, mat = FALSE)
  checker$check(
    paste("probabilities in [0,1]:", tag),
    all(is.finite(value[eligible])) &&
      all(value[eligible] >= 0 & value[eligible] <= 1),
    sprintf("eligible range [%.6f, %.6f]",
            min(value[eligible]), max(value[eligible]))
  )
  checker$check(
    paste("2021 Sasa has zero new-colonization probability:", tag),
    all(value[initial_sasa] == 0),
    paste(sum(initial_sasa), "initial cells")
  )
  checker$check(
    paste("never-eligible cells are NA:", tag),
    all(is.na(value[never_eligible])),
    paste(sum(never_eligible), "cells")
  )
}

# ---- trajectory monotonicity and scenario reporting ------------------------
trajectory_path <- file.path(DIR_OUT, "ca_trajectory.csv")
checker$check("trajectory CSV exists", file.exists(trajectory_path),
              trajectory_path)
if (file.exists(trajectory_path)) {
  trajectory <- utils::read.csv(trajectory_path, stringsAsFactors = FALSE)
  required_columns <- c(
    "scenario", "year", "expected_new_area_m2",
    "cumulative_expected_area_m2", "replicate_sd_m2"
  )
  checker$check("trajectory has the required columns",
                all(required_columns %in% names(trajectory)))
  checker$check("trajectory covers every executed scenario",
                setequal(trajectory$scenario, expected_scenarios))
  for (tag in expected_scenarios) {
    z <- trajectory[trajectory$scenario == tag, , drop = FALSE]
    z <- z[order(z$year), , drop = FALSE]
    checker$check(
      paste("cumulative expected area is non-decreasing:", tag),
      identical(as.integer(z$year), 2022:2030) &&
        all(diff(z$cumulative_expected_area_m2) >= -1e-9),
      sprintf("2030 = %.3f m2", tail(z$cumulative_expected_area_m2, 1))
    )
  }
  ordering <- aggregate(
    cumulative_expected_area_m2 ~ scenario,
    trajectory[trajectory$year == 2030, , drop = FALSE], identity
  )
  ordering <- ordering[order(ordering$cumulative_expected_area_m2,
                             decreasing = TRUE), ]
  cat("SCENARIO ORDERING (reported, not gated): ",
      paste(sprintf("%s=%.3f m2", ordering$scenario,
                    ordering$cumulative_expected_area_m2),
            collapse = " > "), "\n", sep = "")
}

# ---- hindcast calibration check --------------------------------------------
hindcast_path <- file.path(DIR_OUT, "ca_hindcast_summary.csv")
bands_path <- file.path(DIR_OUT, "ca_hindcast_band_comparison.csv")
checker$check("hindcast calibration-check summary exists",
              file.exists(hindcast_path), hindcast_path)
checker$check("hindcast distance-band comparison exists",
              file.exists(bands_path), bands_path)
if (file.exists(hindcast_path)) {
  hindcast <- utils::read.csv(hindcast_path, stringsAsFactors = FALSE)
  ratio <- hindcast$expected_to_observed_ratio[[1]]
  checker$check(
    "hindcast is labelled as a calibration check",
    nrow(hindcast) == 1L &&
      identical(hindcast$validation_type[[1]],
                "calibration_check_not_independent")
  )
  checker$check("hindcast ratio and cell AUC are finite",
                is.finite(ratio) && is.finite(hindcast$cell_auc[[1]]))
  cat(sprintf("HINDCAST CALIBRATION-CHECK RATIO %.6f (reporting band [0.5, 2.0]; not gated)\n",
              ratio))
  if (ratio < 0.5 || ratio > 2.0) {
    cat("*** WARNING: HINDCAST RATIO IS OUTSIDE [0.5, 2.0]; TEST REMAINS NON-GATING ***\n")
  }
}
if (file.exists(bands_path)) {
  bands <- utils::read.csv(bands_path, stringsAsFactors = FALSE)
  checker$check(
    "hindcast comparison has all seven distance bands",
    nrow(bands) == 7L && setequal(
      bands$distance_band_m,
      c("[0,5)", "[5,10)", "[10,20)", "[20,40)",
        "[40,80)", "[80,160)", ">=160")
    )
  )
}

# ---- held-block independent spatial validation -----------------------------
spatial_path <- file.path(DIR_OUT, "ca_spatial_validation.csv")
checker$check("spatial validation CSV exists", file.exists(spatial_path),
              spatial_path)
if (file.exists(spatial_path)) {
  spatial <- utils::read.csv(spatial_path, stringsAsFactors = FALSE)
  checker$check(
    "spatial validation has exactly four held-block rows",
    nrow(spatial) == 4L && identical(sort(as.integer(spatial$fold)), 1:4)
  )
  checker$check(
    "spatial validation reports finite AUC and areas",
    all(is.finite(spatial$auc)) &&
      all(is.finite(spatial$expected_colonized_area_m2)) &&
      all(is.finite(spatial$observed_colonized_area_m2))
  )
}

# ---- reduced sensitivity and runtime ---------------------------------------
sensitivity_path <- file.path(DIR_OUT, "ca_sensitivity.csv")
checker$check("sensitivity CSV exists", file.exists(sensitivity_path),
              sensitivity_path)
if (file.exists(sensitivity_path)) {
  sensitivity <- utils::read.csv(sensitivity_path, stringsAsFactors = FALSE)
  checker$check(
    "both required sensitivity alternatives are reported",
    nrow(sensitivity) == 2L && setequal(
      sensitivity$sensitivity,
      c("distance_min_area_m2_0", "annualization_p9_div_9")
    ) && all(is.finite(sensitivity$sensitivity_expected_area_2030_m2))
  )
}

run_summary_path <- file.path(DIR_OUT, "ca_run_summary.csv")
checker$check("run summary exists", file.exists(run_summary_path),
              run_summary_path)
if (file.exists(run_summary_path)) {
  run_summary <- utils::read.csv(run_summary_path, stringsAsFactors = FALSE)
  if (SASA_SMOKE) {
    checker$check(
      "SMOKE used five replicates in every simulation set",
      isTRUE(run_summary$smoke[[1]]) &&
        all(unlist(run_summary[1, c(
          "main_replicates", "hindcast_replicates",
          "spatial_replicates_per_fold", "sensitivity_replicates"
        )]) == N_CA_REPLICATES_SMOKE)
    )
    checker$check(
      "SMOKE runtime is under 10 minutes",
      is.finite(run_summary$runtime_seconds[[1]]) &&
        run_summary$runtime_seconds[[1]] < 600,
      sprintf("%.1f s", run_summary$runtime_seconds[[1]])
    )
  }
}

checker$report()
finish_script("tests/test_wp4.R", t0)
