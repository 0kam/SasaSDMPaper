# =============================================================================
# 01_predictors.R -- WP1: build the predictor stack
#
# Outputs (all under analysis/out/):
#   predictors.tif                      7-layer stack on the vege_2012 grid
#   snowmelt_ols_trend.tif              per-pixel OLS trend (slope/se/p/r2/n)
#   snowmelt_scenarios.csv              scenario shift constants
#   snow_mean_summary.csv               the three reference means + NA counts
#   predictor_correlation_spearman.csv  Spearman correlation matrix
#   predictor_vif.csv                   variance inflation factors
#
# Run:  Rscript analysis/01_predictors.R
# =============================================================================

t0 <- Sys.time()
source(file.path(Sys.getenv("SASA_REPO_ROOT",
                            unset = "/Users/okamoto/NIES/SasaSDMPaper"),
                 "analysis", "00_config.R"))
set.seed(SEED_GLOBAL)

msg("WP1: predictor stack")

# -----------------------------------------------------------------------------
# 0. Reference grid
# -----------------------------------------------------------------------------
# vege_2012_5x5.tiff : EPSG:6690 (JGD2011 / UTM 53N), 1801 x 1753 columns/rows,
#                      res 1 x 1 m, ext (732744, 734545, 4050316.25, 4052069.25),
#                      origin (0, 0.25).
grid <- ref_grid()
msg("reference grid: ", paste(dim(grid)[1:2], collapse = " x "),
    "  res ", paste(res(grid), collapse = "x"),
    "  origin (", paste(origin(grid), collapse = ", "), ")")

# -----------------------------------------------------------------------------
# 1. Snowmelt climatology (snow_mean)
# -----------------------------------------------------------------------------
# The raw snowmelt rasters are ALREADY on the reference grid exactly (same CRS,
# extent, resolution and origin), so no resampling is needed for them; this is
# asserted inside load_snow_matrix().
msg("reading snowmelt rasters (", length(SNOW_YEARS), " years, ",
    SNOW_YEAR_MISSING, " missing)")
snow <- load_snow_matrix()
V <- snow$values          # ncell x 10, DOY > 0 rule already applied
nv <- snow$n_valid

msg("valid-year counts per cell:")
print(table(nv))

# snow_mean = per-pixel mean over the years available for that pixel.
# Pixels with no valid year become NA (not NaN).
sum_v <- rowSums(V, na.rm = TRUE)
snow_mean_vals <- ifelse(nv > 0L, sum_v / pmax(nv, 1L), NA_real_)
snow_mean <- rast_from_values(snow_mean_vals, grid, "snow_mean")

# Three global means, differing only in how invalid / partial pixels are
# handled. See 00_config.R for the definitions; all three are reported so the
# comparison with the legacy published number is explicit.
mask_all <- nv == length(SNOW_YEARS)
mean_allyears_doypos <- mean(snow_mean_vals[mask_all])

# Legacy convention: cells non-NA in every raw raster, WITHOUT the DOY > 0 rule.
raw_stack <- rast(unname(snow_files()))
Vraw <- values(raw_stack)
mask_raw_complete <- rowSums(is.na(Vraw)) == 0L
mean_allyears_zeroincl <- mean(rowMeans(Vraw[mask_raw_complete, , drop = FALSE]))
rm(Vraw, raw_stack)

mean_availyears_doypos <- mean(snow_mean_vals, na.rm = TRUE)
n_na_snow_mean <- sum(is.na(snow_mean_vals))

msg(sprintf("snow_mean | all-years-valid, DOY>0     : %.4f (n = %d)",
            mean_allyears_doypos, sum(mask_all)))
msg(sprintf("snow_mean | all-years-valid, 0 included: %.4f (n = %d)  [legacy]",
            mean_allyears_zeroincl, sum(mask_raw_complete)))
msg(sprintf("snow_mean | per-cell available, DOY>0  : %.4f (n = %d)",
            mean_availyears_doypos, sum(!is.na(snow_mean_vals))))
msg("snow_mean NA cells: ", n_na_snow_mean, " / ", length(snow_mean_vals))

# NOTE ON THE CENSORING: the camera acquisition window censors DOY to
# approximately [120, 230] (see the 120-230 tag in the file names). Observed
# range of the DOY > 0 values is reported below. This is documented, not
# corrected.
msg("observed DOY range (DOY > 0): ",
    paste(range(V, na.rm = TRUE), collapse = " - "))

write.csv(
  data.frame(
    statistic = c("mean_allyears_doy_positive",
                  "mean_allyears_zeros_included_legacy",
                  "mean_available_years_doy_positive",
                  "n_cells_allyears_valid",
                  "n_cells_raw_complete",
                  "n_cells_snow_mean_nonNA",
                  "n_cells_snow_mean_NA",
                  "doy_min_observed", "doy_max_observed"),
    value = c(mean_allyears_doypos, mean_allyears_zeroincl,
              mean_availyears_doypos, sum(mask_all), sum(mask_raw_complete),
              sum(!is.na(snow_mean_vals)), n_na_snow_mean,
              min(V, na.rm = TRUE), max(V, na.rm = TRUE)),
    stringsAsFactors = FALSE),
  PATH_SNOW_MEAN_SUMM, row.names = FALSE)

# -----------------------------------------------------------------------------
# 2. Per-pixel OLS trend surface + scenario constants
# -----------------------------------------------------------------------------
# Documentation of the trend surface. The trend itself is SCENARIO-ONLY by
# design: it never enters a model as a fitted process.
msg("fitting per-pixel OLS (snowmelt ~ year, DOY > 0, >= ", OLS_MIN_YEARS,
    " valid years)")
ols <- row_ols(V, as.numeric(snow$years), min_n = OLS_MIN_YEARS)
msg("pixels with an OLS fit: ", sum(!is.na(ols$slope)))

ols_stack <- c(
  rast_from_values(ols$slope, grid, "slope_d_per_yr"),
  rast_from_values(ols$se,    grid, "se"),
  rast_from_values(ols$p,     grid, "p_value"),
  rast_from_values(ols$r2,    grid, "r_squared"),
  rast_from_values(as.numeric(ols$n), grid, "n_years")
)
assert_on_ref_grid(ols_stack, "snowmelt OLS stack")
writeRaster(ols_stack, PATH_SNOW_OLS, overwrite = TRUE, datatype = "FLT4S",
            gdal = c("COMPRESS=DEFLATE", "PREDICTOR=3", "TILED=YES"))
msg("wrote ", PATH_SNOW_OLS)

write.csv(SNOW_SCENARIOS, PATH_SNOW_SCENARIOS, row.names = FALSE)
msg("wrote ", PATH_SNOW_SCENARIOS)

rm(V, ols); invisible(gc())

# -----------------------------------------------------------------------------
# 3. Terrain predictors
# -----------------------------------------------------------------------------
# GRID AUDIT of ortho/data/terrain_features/*.tif, performed before writing
# this script and re-asserted at run time:
#   CRS        EPSG:3099 (JGD2000 / UTM 53N)  -- DIFFERENT from the vege grid
#                                                (EPSG:6690, JGD2011)
#   size       1198 rows x 1263 cols
#   res        4.97 x 6.16 m                  -- DIFFERENT (vege grid is 1 x 1)
#   ext        (729277.6, 735554.7, 4048457, 4055837)
#   origin     (-0.2916, 0.7775)              -- DIFFERENT
# Because both the CRS and the grid differ, terra::resample() is NOT sufficient
# (it does not reproject). We use terra::project(x, template, method =
# "bilinear"), which reprojects and snaps onto the template grid in one step;
# the result is verified to be bit-identical to the reference grid.
# Bilinear is used because every terrain layer here is a continuous quantity.
msg("reading terrain rasters")
terrain_files <- c(slope     = "slope.tif",
                   elevation = "tateyamadem_small.tif",
                   TPI       = "TPI.tif",
                   twi       = "twi.tif")
# roughness.tif and TRI.tif are deliberately NOT read (collinear with slope).

terr_src <- rast(file.path(DIR_TERRAIN, terrain_files))
names(terr_src) <- names(terrain_files)   # tateyamadem_small -> elevation
src_code <- crs(terr_src, describe = TRUE)$code
msg("terrain source CRS EPSG:", src_code, "  res ",
    paste(signif(res(terr_src), 4), collapse = "x"),
    "  origin (", paste(signif(origin(terr_src), 4), collapse = ", "), ")")
if (!identical(src_code, sub("EPSG:", "", CRS_TERRAIN_SOURCE))) {
  warning("terrain CRS changed on disk: expected ", CRS_TERRAIN_SOURCE,
          ", found EPSG:", src_code)
}

# Aspect: decomposed into northness / eastness BEFORE reprojection.
# Aspect is a circular quantity (0 and 360 degrees are the same direction), so
# bilinear interpolation of raw aspect would produce nonsense at the wrap-around
# (mean of 359 and 1 would be 180, i.e. exactly the opposite direction).
# cos()/sin() are linear-interpolable, so the decomposition is done at native
# resolution and the two components are reprojected instead.
aspect_src <- rast(file.path(DIR_TERRAIN, "aspect.tif"))
northness_src <- cos(aspect_src * pi / 180)
eastness_src  <- sin(aspect_src * pi / 180)
names(northness_src) <- "northness"
names(eastness_src)  <- "eastness"

msg("reprojecting terrain layers onto the reference grid (bilinear)")
terr <- project(c(terr_src, northness_src, eastness_src), grid,
                method = "bilinear")
assert_on_ref_grid(terr, "reprojected terrain stack")

# -----------------------------------------------------------------------------
# 4. Assemble and write the predictor stack
# -----------------------------------------------------------------------------
predictors <- c(terr, snow_mean)[[PREDICTOR_NAMES]]
stopifnot(identical(names(predictors), PREDICTOR_NAMES))
assert_on_ref_grid(predictors, "predictor stack")
# Raw aspect must not be present anywhere in the stack.
stopifnot(!"aspect" %in% names(predictors))

writeRaster(predictors, PATH_PREDICTORS, overwrite = TRUE, datatype = "FLT4S",
            gdal = c("COMPRESS=DEFLATE", "PREDICTOR=3", "TILED=YES"))
msg("wrote ", PATH_PREDICTORS, " (", nlyr(predictors), " layers: ",
    paste(names(predictors), collapse = ", "), ")")

for (nm in names(predictors)) {
  msg(sprintf("  %-10s NA = %8d  range = [%.3f, %.3f]", nm,
              as.integer(global(is.na(predictors[[nm]]), "sum")[[1]]),
              global(predictors[[nm]], "min", na.rm = TRUE)[[1]],
              global(predictors[[nm]], "max", na.rm = TRUE)[[1]]))
}

# -----------------------------------------------------------------------------
# 5. Predictor diagnostics: Spearman correlations and VIF
# -----------------------------------------------------------------------------
# Computed on a seeded random sample of complete cells (all 7 layers non-NA).
# Raw aspect is absent by construction, so it cannot appear here either.
msg("predictor diagnostics on a ", N_DIAG_SAMPLE, "-cell seeded sample")
P <- values(predictors)
complete <- stats::complete.cases(P)
msg("complete cells: ", sum(complete), " / ", nrow(P))
set.seed(SEED_VIF_SAMPLE)
idx <- which(complete)
if (length(idx) > N_DIAG_SAMPLE) idx <- sample(idx, N_DIAG_SAMPLE)
S <- P[idx, , drop = FALSE]
rm(P); invisible(gc())

spearman <- stats::cor(S, method = "spearman")
write.csv(data.frame(predictor = rownames(spearman), spearman,
                     check.names = FALSE, stringsAsFactors = FALSE),
          PATH_PRED_SPEARMAN, row.names = FALSE)
cat("\nSpearman correlation matrix:\n"); print(round(spearman, 3))
off <- spearman; diag(off) <- 0
msg(sprintf("max |Spearman r| (off-diagonal) = %.4f (threshold %.2f)",
            max(abs(off)), SPEARMAN_MAX))

vif <- vif_from_matrix(S)
write.csv(data.frame(predictor = names(vif), vif = as.numeric(vif),
                     stringsAsFactors = FALSE),
          PATH_PRED_VIF, row.names = FALSE)
cat("\nVIF:\n"); print(round(vif, 3))
msg(sprintf("max VIF = %.4f (threshold %.1f)", max(vif), VIF_MAX))

finish_script("01_predictors.R", t0)
