# =============================================================================
# review/trend_sensitivity/00_prepare_data.R
#
# Builds the shared data frame used by both SI sensitivity analyses:
#   (1) correlation of the per-pixel snowmelt trend with the snowmelt
#       climatology and with elevation
#   (2) adding s(trend) to the adopted Model B specification
#
# Reads only; nothing under analysis/ is modified. Mirrors the cell selection
# and column construction of analysis/04_model_B.R exactly, then appends the
# per-pixel OLS snowmelt slope (analysis/out/snowmelt_ols_trend.tif).
# =============================================================================

t0 <- Sys.time()
REPO <- Sys.getenv("SASA_REPO_ROOT", unset = "/Users/okamoto/NIES/SasaSDMPaper")
source(file.path(REPO, "analysis", "00_config.R"))
source(file.path(DIR_ANALYSIS, "R", "distance.R"))
source(file.path(DIR_ANALYSIS, "R", "model_utils.R"))

DIR_REVIEW <- file.path(REPO, "review", "trend_sensitivity")
dir.create(DIR_REVIEW, showWarnings = FALSE, recursive = TRUE)
set.seed(SEED_MODEL_B)

msg("Reading rasters")
predictors <- terra::rast(PATH_PREDICTORS)
dist12 <- terra::rast(PATH_DIST12)
names(dist12) <- "dist12"
assert_on_ref_grid(predictors, "predictors")
assert_on_ref_grid(dist12, "dist12")
folds <- read_shared_folds()
vege12 <- terra::rast(PATH_VEGE_2012)
vege21 <- terra::rast(PATH_VEGE_2021)
trend_rast <- terra::rast(PATH_SNOW_OLS)[["slope_d_per_yr"]]
assert_on_ref_grid(trend_rast, "snowmelt OLS trend")

msg("Extracting values")
pred_values <- terra::values(predictors)
dist_values <- terra::values(dist12, mat = FALSE)
v12 <- terra::values(vege12, mat = FALSE)
v21 <- terra::values(vege21, mat = FALSE)
fold_values <- terra::values(folds, mat = FALSE)
trend_values <- terra::values(trend_rast, mat = FALSE)

# --- (A) full trend domain, for the correlation analysis ---------------------
# Every cell that has a fitted per-pixel OLS slope AND complete environmental
# predictors. This is the landscape-wide view asked for by task 1.
trend_ok <- is.finite(trend_values) & complete.cases(pred_values)
trend_cells <- which(trend_ok)
trend_df <- data.frame(
  cell = trend_cells,
  trend = trend_values[trend_cells],
  snow_mean = pred_values[trend_cells, "snow_mean"],
  elevation = pred_values[trend_cells, "elevation"],
  fold_id = as.integer(fold_values[trend_cells])
)
msg("cells with a fitted trend and complete predictors: ", nrow(trend_df))
saveRDS(trend_df, file.path(DIR_REVIEW, "trend_domain.rds"))

# --- (B) Model B modelling frame, plus the trend column ----------------------
valid <- complete.cases(pred_values) & is.finite(dist_values) &
  !is.na(v12) & !is.na(v21) & !is.na(fold_values)
eligible_cells <- which(valid & v12 != 1)
full_response <- as.integer(v21[eligible_cells] == 1)

all_data <- data.frame(
  cell = eligible_cells,
  colonized = full_response,
  fold_id = as.integer(fold_values[eligible_cells]),
  dist12 = dist_values[eligible_cells],
  pred_values[eligible_cells, PREDICTOR_NAMES, drop = FALSE],
  check.names = FALSE
)
all_data$log1p_dist12 <- log1p(all_data$dist12)
all_data$trend <- trend_values[eligible_cells]

# stratified_smoke_sample() is the identity in full mode; kept for fidelity.
sample_rows <- stratified_smoke_sample(
  all_data, all_data$colonized, all_data$fold_id, fraction = 0.10,
  seed = SEED_MODEL_B
)
dat <- all_data[sample_rows, , drop = FALSE]

msg("Model B eligible rows: ", nrow(dat),
    "  positives: ", sum(dat$colonized))
msg("rows with a missing trend: ", sum(!is.finite(dat$trend)),
    sprintf("  (%.3f%%)", 100 * mean(!is.finite(dat$trend))))
msg("positives with a missing trend: ",
    sum(!is.finite(dat$trend) & dat$colonized == 1L))

saveRDS(dat, file.path(DIR_REVIEW, "model_B_frame.rds"))

summary_tbl <- data.frame(
  quantity = c("n_trend_domain_cells", "n_model_B_rows", "n_model_B_positives",
               "n_model_B_rows_trend_missing", "n_model_B_positives_trend_missing",
               "n_model_B_rows_complete", "n_model_B_positives_complete"),
  value = c(nrow(trend_df), nrow(dat), sum(dat$colonized),
            sum(!is.finite(dat$trend)),
            sum(!is.finite(dat$trend) & dat$colonized == 1L),
            sum(is.finite(dat$trend)),
            sum(is.finite(dat$trend) & dat$colonized == 1L))
)
utils::write.csv(summary_tbl,
                 file.path(DIR_REVIEW, "data_summary.csv"), row.names = FALSE)
print(summary_tbl)

msg("00_prepare_data.R finished in ",
    sprintf("%.1f", as.numeric(difftime(Sys.time(), t0, units = "secs"))), " s")
