# =============================================================================
# 06_snowmelt_stats.R -- WP6: snowmelt trend statistics package
#
# Regenerates, from ortho/data/snow/raw/ alone, every snowmelt number the
# manuscript revision reports. These replace an incorrect published figure.
#
# Data caveats (all of them belong in Methods, not in a figure footnote):
#   * 2019 is MISSING -- the time-lapse camera could not be operated during
#     lodge construction work at the site.
#   * 2010 is EXCLUDED upstream -- a different lens / field of view makes the
#     2010 imagery geometrically incompatible with 2011-2021.
#   * Pixel value 0 means SKY / invalid, not "melted on day 0". The camera only
#     starts operating in April, so no melt date can be assigned to those
#     pixels. All statistics below apply the DOY > 0 rule.
#   * The acquisition window censors melt dates to roughly DOY [120, 230]
#     (see the "120-230" tag in the file names). This is documented, not
#     corrected; the observed range is printed below.
#
# Outputs (all under analysis/out/):
#   snowmelt_statistics.csv     tidy table of every reported statistic
#   snowmelt_annual_means.csv   annual spatial mean DOY (time-series figure)
#   fig_snowmelt_annual_mean.png
#   fig_snowmelt_slope_hist.png
#   fig_snowmelt_slope_map.png
#
# Run:  Rscript analysis/06_snowmelt_stats.R
# (Independent of 01_predictors.R: it recomputes the trend from the raw
#  rasters. If 01 has already been run, the two slope surfaces are cross-
#  checked for exact agreement.)
# =============================================================================

# --figures-only never reads raw snow data or writes statistics/raster files.
if ("--figures-only" %in% commandArgs(trailingOnly = TRUE)) {
  source(file.path("analysis", "figures", "figS_snowmelt_trends.R"))
  quit(save = "no", status = 0)
}

t0 <- Sys.time()
source(file.path(Sys.getenv("SASA_REPO_ROOT",
                            unset = "/Users/okamoto/NIES/SasaSDMPaper"),
                 "analysis", "00_config.R"))
set.seed(SEED_GLOBAL)

msg("WP6: snowmelt statistics")

# -----------------------------------------------------------------------------
# 1. Data
# -----------------------------------------------------------------------------
snow <- load_snow_matrix()
V  <- snow$values           # ncell x 10, DOY > 0 rule applied (0 -> NA)
yr <- as.numeric(snow$years)
grid <- snow$template
msg("years: ", paste(snow$years, collapse = ", "),
    "  (", SNOW_YEAR_MISSING, " missing)")
msg("observed DOY range: ", paste(range(V, na.rm = TRUE), collapse = " - "))

# -----------------------------------------------------------------------------
# 2. Per-pixel OLS trends
# -----------------------------------------------------------------------------
msg("fitting per-pixel OLS (>= ", OLS_MIN_YEARS, " valid years)")
ols <- row_ols(V, yr, min_n = OLS_MIN_YEARS)
fit <- !is.na(ols$slope)
b <- ols$slope[fit]
p <- ols$p[fit]
n_pix <- length(b)
msg("pixels with a trend: ", n_pix)

q <- stats::p.adjust(p, method = "BH")

slope_mean   <- mean(b)
slope_median <- stats::median(b)
slope_sd     <- stats::sd(b)
slope_ci     <- slope_mean + c(-1, 1) * stats::qt(0.975, n_pix - 1) *
                slope_sd / sqrt(n_pix)
pct_neg  <- 100 * mean(b < 0)
pct_pos  <- 100 * mean(b > 0)
pct_zero <- 100 * mean(b == 0)   # exact ties do occur; reported for closure
pct_p05 <- 100 * mean(p < 0.05, na.rm = TRUE)
pct_fdr <- 100 * mean(q < 0.05, na.rm = TRUE)

msg(sprintf("slope: mean %.4f  median %.4f  sd %.4f", slope_mean, slope_median, slope_sd))
msg(sprintf("       %% negative %.4f  %% p<0.05 %.4f  %% BH q<0.05 %.4f",
            pct_neg, pct_p05, pct_fdr))
# NOTE: the 95%% CI of the mean pixel slope is a *descriptive* interval. It is
# not a valid inferential interval for the landscape trend, because the 1.2 M
# pixels are massively spatially autocorrelated. The landscape-scale test below
# is the inferential one, and it is the one the manuscript should quote.

slope_rast <- rast_from_values(ols$slope, grid, "slope_d_per_yr")
assert_on_ref_grid(slope_rast, "WP6 slope raster")

# Cross-check against WP1's independently written surface, if present.
if (file.exists(PATH_SNOW_OLS)) {
  w1 <- rast(PATH_SNOW_OLS)[["slope_d_per_yr"]]
  d <- max(abs(values(w1) - values(slope_rast)), na.rm = TRUE)
  msg(sprintf("cross-check vs 01_predictors.R slope raster: max |diff| = %.3e (FLT4S rounding)", d))
  if (!is.finite(d) || d > 1e-5) {
    stop("WP1 and WP6 slope surfaces disagree (max |diff| = ", d, ")")
  }
}

# -----------------------------------------------------------------------------
# 3. Landscape-scale test
# -----------------------------------------------------------------------------
# Regression of the 10 annual SPATIAL MEANS on year. n = 10, so this test has
# very little power -- which is precisely the point: the landscape trend is not
# statistically distinguishable from zero, and the manuscript must say so.
annual_mean <- colMeans(V, na.rm = TRUE)
annual_n    <- colSums(!is.na(V))
annual <- data.frame(year = snow$years,
                     mean_doy = as.numeric(annual_mean),
                     sd_doy = apply(V, 2, stats::sd, na.rm = TRUE),
                     n_pixels = as.integer(annual_n),
                     stringsAsFactors = FALSE)
print(annual)
write.csv(annual, PATH_SNOW_ANNUAL, row.names = FALSE)
msg("wrote ", PATH_SNOW_ANNUAL)

land_fit <- stats::lm(mean_doy ~ year, data = annual)
land_sum <- summary(land_fit)$coefficients
land_ci  <- stats::confint(land_fit)
land_slope <- land_sum["year", "Estimate"]
land_se    <- land_sum["year", "Std. Error"]
land_t     <- land_sum["year", "t value"]
land_p     <- land_sum["year", "Pr(>|t|)"]
msg(sprintf("landscape trend: %.4f d/yr, SE %.4f, t %.3f, p %.4f, CI [%.4f, %.4f]",
            land_slope, land_se, land_t, land_p, land_ci["year", 1], land_ci["year", 2]))

# -----------------------------------------------------------------------------
# 4. Tidy statistics table
# -----------------------------------------------------------------------------
stats_tbl <- data.frame(
  quantity = c(
    "n_pixels_with_trend",
    "pixel_slope_mean", "pixel_slope_median", "pixel_slope_sd",
    "pixel_slope_ci95_lower", "pixel_slope_ci95_upper",
    "pixel_slope_pct_negative", "pixel_slope_pct_positive",
    "pixel_slope_pct_zero",
    "pixel_slope_pct_p_lt_0.05", "pixel_slope_pct_BH_q_lt_0.05",
    "landscape_slope", "landscape_slope_se", "landscape_t", "landscape_p",
    "landscape_ci95_lower", "landscape_ci95_upper",
    "n_years", "year_missing", "doy_min_observed", "doy_max_observed"),
  value = c(
    n_pix, slope_mean, slope_median, slope_sd, slope_ci[1], slope_ci[2],
    pct_neg, pct_pos, pct_zero, pct_p05, pct_fdr,
    land_slope, land_se, land_t, land_p, land_ci["year", 1], land_ci["year", 2],
    length(snow$years), SNOW_YEAR_MISSING,
    min(V, na.rm = TRUE), max(V, na.rm = TRUE)),
  unit = c(
    "pixels", rep("days per year", 5), rep("percent", 5),
    "days per year", "days per year", "", "", "days per year",
    "days per year", "years", "year", "DOY", "DOY"),
  note = c(
    sprintf("DOY > 0, >= %d valid years", OLS_MIN_YEARS),
    rep("per-pixel OLS snowmelt ~ year", 3),
    rep("descriptive CI of the mean pixel slope; pixels are not independent", 2),
    rep("sign of the per-pixel slope", 3),
    "uncorrected per-pixel t test",
    "Benjamini-Hochberg FDR across pixels",
    rep("OLS of the 10 annual spatial means on year (n = 10)", 6),
    "years with usable imagery",
    "camera not operated (lodge construction)",
    rep("acquisition window censors DOY to about [120, 230]", 2)),
  stringsAsFactors = FALSE)
write.csv(stats_tbl, PATH_SNOW_STATS, row.names = FALSE)
msg("wrote ", PATH_SNOW_STATS)
print(stats_tbl[, c("quantity", "value")])

# -----------------------------------------------------------------------------
# 5. Figures
# -----------------------------------------------------------------------------
source(file.path("analysis", "figures", "figS_snowmelt_trends.R"))

finish_script("06_snowmelt_stats.R", t0)
