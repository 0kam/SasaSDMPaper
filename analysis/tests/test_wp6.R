# =============================================================================
# tests/test_wp6.R -- acceptance checks for WP6 (snowmelt statistics)
#
# Run AFTER analysis/06_snowmelt_stats.R:
#     Rscript analysis/tests/test_wp6.R
# Exits with status 1 if any check fails.
#
# The checks read the written CSV, i.e. they validate the artefact the
# manuscript will quote, not an in-memory object.
# =============================================================================

source(file.path(Sys.getenv("SASA_REPO_ROOT",
                            unset = "/Users/okamoto/NIES/SasaSDMPaper"),
                 "analysis", "00_config.R"))
set.seed(SEED_GLOBAL)

tst <- new_checker("WP6 acceptance")

figs <- file.path(DIR_OUT, c("fig_snowmelt_annual_mean.png",
                             "fig_snowmelt_slope_hist.png",
                             "fig_snowmelt_slope_map.png"))
for (f in c(PATH_SNOW_STATS, PATH_SNOW_ANNUAL, figs)) {
  tst$check(paste("exists:", basename(f)), file.exists(f))
}
if (!file.exists(PATH_SNOW_STATS)) tst$report()

st <- read.csv(PATH_SNOW_STATS, stringsAsFactors = FALSE)
get <- function(k) {
  i <- match(k, st$quantity)
  if (is.na(i)) NA_real_ else st$value[i]
}

# ---- per-pixel OLS slopes ---------------------------------------------------
tst$check_near("n pixels with a trend", get("n_pixels_with_trend"),
               REF_SLOPE_N, TOL_SLOPE_N)
tst$check_near("mean pixel slope (d/yr)", get("pixel_slope_mean"),
               REF_SLOPE_MEAN, TOL_SLOPE_MEAN)
tst$check_near("median pixel slope (d/yr)", get("pixel_slope_median"),
               REF_SLOPE_MEDIAN, TOL_SLOPE_MEDIAN)
tst$check_in("SD of pixel slopes", get("pixel_slope_sd"), REF_SLOPE_SD_RANGE)
tst$check_in("% of pixels with a negative slope",
             get("pixel_slope_pct_negative"), REF_SLOPE_PCT_NEG_RANGE)
tst$check("% negative + % positive + % exactly zero = 100",
          isTRUE(all.equal(get("pixel_slope_pct_negative") +
                           get("pixel_slope_pct_positive") +
                           get("pixel_slope_pct_zero"), 100,
                           tolerance = 1e-6)),
          sprintf("zero-slope pixels: %.4f%%", get("pixel_slope_pct_zero")))
tst$check_near("% of pixels with p < 0.05", get("pixel_slope_pct_p_lt_0.05"),
               REF_SLOPE_PCT_P05, TOL_SLOPE_PCT_P05)
tst$check("% surviving BH-FDR q < 0.05 is exactly 0",
          identical(get("pixel_slope_pct_BH_q_lt_0.05"), 0),
          sprintf("%.6f", get("pixel_slope_pct_BH_q_lt_0.05")))
tst$check("pixel-slope CI is reported and brackets the mean",
          is.finite(get("pixel_slope_ci95_lower")) &&
          is.finite(get("pixel_slope_ci95_upper")) &&
          get("pixel_slope_ci95_lower") <= get("pixel_slope_mean") &&
          get("pixel_slope_mean") <= get("pixel_slope_ci95_upper"))

# ---- landscape-scale test ---------------------------------------------------
tst$check_near("landscape slope (d/yr)", get("landscape_slope"),
               REF_LAND_SLOPE, TOL_LAND_SLOPE)
tst$check_near("landscape slope SE", get("landscape_slope_se"),
               REF_LAND_SE, TOL_LAND_SE)
tst$check_near("landscape p value", get("landscape_p"),
               REF_LAND_P, TOL_LAND_P)
tst$check_near("landscape CI lower", get("landscape_ci95_lower"),
               REF_LAND_CI[1], TOL_LAND_CI)
tst$check_near("landscape CI upper", get("landscape_ci95_upper"),
               REF_LAND_CI[2], TOL_LAND_CI)
tst$check("landscape trend is not significant at 0.05",
          get("landscape_p") > 0.05)

# ---- annual means -----------------------------------------------------------
an <- read.csv(PATH_SNOW_ANNUAL, stringsAsFactors = FALSE)
tst$check("annual means cover the 10 available years",
          identical(as.integer(an$year), SNOW_YEARS),
          paste(an$year, collapse = ","))
tst$check(paste0(SNOW_YEAR_MISSING, " is absent (camera not operated)"),
          !(SNOW_YEAR_MISSING %in% an$year))
tst$check("annual mean DOY all inside a plausible melt window (100-250)",
          all(an$mean_doy > 100 & an$mean_doy < 250),
          sprintf("[%.2f, %.2f]", min(an$mean_doy), max(an$mean_doy)))

# The CSV must be able to regenerate the landscape test on its own.
f <- stats::lm(mean_doy ~ year, data = an)
tst$check_near("landscape slope recomputed from the annual-means CSV",
               unname(coef(f)[2]), REF_LAND_SLOPE, TOL_LAND_SLOPE)

# ---- independent recomputation from the raw rasters -------------------------
# Guards against the statistics CSV drifting away from the source data.
snow <- load_snow_matrix()
ols <- row_ols(snow$values, as.numeric(snow$years), min_n = OLS_MIN_YEARS)
b <- ols$slope[!is.na(ols$slope)]
tst$check_near("recomputed mean slope from ortho/data/snow/raw",
               mean(b), REF_SLOPE_MEAN, TOL_SLOPE_MEAN)
tst$check("recomputed n matches the CSV",
          abs(length(b) - get("n_pixels_with_trend")) < 1,
          sprintf("%d vs %d", length(b), as.integer(get("n_pixels_with_trend"))))

tst$report()
cat("\nWP6 acceptance: all checks passed.\n")
