# Acceptance tests for WP2 / Model A.

t0 <- Sys.time()
source(file.path("analysis", "00_config.R"))
checker <- new_checker("WP2 acceptance")

variants <- c("primary", "cut2560")
for (variant in variants) {
  path <- file.path(DIR_OUT, paste0("suitability_A_", variant, ".tif"))
  checker$check(paste("suitability exists:", variant), file.exists(path), path)
  if (file.exists(path)) {
    r <- terra::rast(path)
    grid_ok <- tryCatch({
      assert_on_ref_grid(r, paste("suitability", variant)); TRUE
    }, error = function(e) FALSE)
    checker$check(paste("bit-exact reference grid:", variant), grid_ok)
    limits <- as.numeric(terra::global(r, range, na.rm = TRUE)[1, ])
    checker$check(
      paste("probabilities in [0,1]:", variant),
      all(is.finite(limits)) && limits[[1]] >= 0 && limits[[2]] <= 1,
      sprintf("range [%.6f, %.6f]", limits[[1]], limits[[2]])
    )
  }
}

metrics_path <- file.path(DIR_OUT, "model_A_heldout_metrics.csv")
checker$check("held-out metrics CSV exists", file.exists(metrics_path), metrics_path)
if (file.exists(metrics_path)) {
  metrics <- utils::read.csv(metrics_path)
  means <- metrics[metrics$scope == "mean", ]
  checker$check("mean held-out TSS reported for both variants",
                setequal(means$variant, variants) && all(is.finite(means$tss)))
  for (i in seq_len(nrow(means))) {
    cat(sprintf("HELD-OUT TSS %-8s %.4f\n", means$variant[[i]], means$tss[[i]]))
    if (means$tss[[i]] < 0.50) {
      cat("*** WARNING: MEAN HELD-OUT TSS IS BELOW 0.50; REPORTED WITHOUT GATEKEEPING ***\n")
    }
  }
}

composition_path <- file.path(DIR_OUT, "model_A_ensemble_composition.csv")
checker$check("ensemble composition CSV exists", file.exists(composition_path),
              composition_path)
if (file.exists(composition_path)) {
  composition <- utils::read.csv(composition_path)
  counts <- table(composition$variant)
  checker$check("each ensemble lists at least one fitted member",
                all(variants %in% names(counts)) && all(counts[variants] >= 1L))
  checker$check("composition includes member algorithm and weight",
                all(c("member", "algorithm", "weight") %in% names(composition)) &&
                  all(is.finite(composition$weight)))
}

sensitivity_path <- file.path(DIR_OUT, "model_A_threshold_sensitivity.csv")
checker$check("threshold-sensitivity CSV exists", file.exists(sensitivity_path),
              sensitivity_path)
if (file.exists(sensitivity_path)) {
  sensitivity <- utils::read.csv(sensitivity_path)
  expected <- seq(0.1, 0.7, by = 0.05)
  coverage_ok <- all(vapply(variants, function(variant) {
    z <- sensitivity$cutoff[sensitivity$variant == variant]
    length(z) == 13L && isTRUE(all.equal(z, expected, tolerance = 1e-12))
  }, logical(1)))
  checker$check("both variants cover all 13 requested cutoffs", coverage_ok)
  checker$check("all suitable areas are finite and non-negative",
                all(is.finite(sensitivity$suitable_area_m2)) &&
                  all(sensitivity$suitable_area_m2 >= 0))
}

checker$report()
finish_script("tests/test_wp2.R", t0)
