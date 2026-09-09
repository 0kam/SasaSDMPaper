# =============================================================================
# tests/test_wp1.R -- acceptance checks for WP1 (predictor stack)
#
# Run AFTER analysis/01_predictors.R:
#     Rscript analysis/tests/test_wp1.R
# Exits with status 1 if any check fails.
# =============================================================================

source(file.path(Sys.getenv("SASA_REPO_ROOT",
                            unset = "/Users/okamoto/NIES/SasaSDMPaper"),
                 "analysis", "00_config.R"))
set.seed(SEED_GLOBAL)

tst <- new_checker("WP1 acceptance")

# ---- 0. outputs exist -------------------------------------------------------
for (f in c(PATH_PREDICTORS, PATH_SNOW_OLS, PATH_SNOW_SCENARIOS,
            PATH_PRED_SPEARMAN, PATH_PRED_VIF, PATH_SNOW_MEAN_SUMM)) {
  tst$check(paste("exists:", basename(f)), file.exists(f))
}
if (!file.exists(PATH_PREDICTORS)) tst$report()

predictors <- rast(PATH_PREDICTORS)
grid <- ref_grid()

# ---- 1. layer set -----------------------------------------------------------
tst$check("layer names and order match PREDICTOR_NAMES",
          identical(names(predictors), PREDICTOR_NAMES),
          paste(names(predictors), collapse = ", "))
tst$check("raw 'aspect' absent from the stack",
          !("aspect" %in% names(predictors)))
tst$check("roughness / TRI absent from the stack",
          !any(c("roughness", "TRI") %in% names(predictors)))

# ---- 2. grid identity, layer by layer --------------------------------------
# ext(), res(), origin() and dimensions must be identical (bit-exact) to
# vege_2012_5x5.tiff, and the EPSG code must match.
for (nm in names(predictors)) {
  ok <- tryCatch({ assert_on_ref_grid(predictors[[nm]], nm, grid); TRUE },
                 error = function(e) conditionMessage(e))
  tst$check(paste0("grid identical to vege_2012: ", nm), isTRUE(ok),
            if (isTRUE(ok)) "" else gsub("\n", " | ", ok))
}
ols <- rast(PATH_SNOW_OLS)
ok <- tryCatch({ assert_on_ref_grid(ols, "snowmelt_ols_trend", grid); TRUE },
               error = function(e) conditionMessage(e))
tst$check("grid identical to vege_2012: snowmelt_ols_trend", isTRUE(ok),
          if (isTRUE(ok)) "" else gsub("\n", " | ", ok))

# Explicit half-pixel-shift guard: the previous stars/tibble pipeline produced
# origin (0.5, -0.25). Assert the correct origin literally.
tst$check("origin is (0, 0.25), i.e. no half-pixel shift",
          identical(origin(predictors), c(0, 0.25)),
          paste(format(origin(predictors), digits = 15), collapse = ", "))

# ---- 3. snow_mean reference means ------------------------------------------
snow <- load_snow_matrix()
V  <- snow$values
nv <- snow$n_valid
mask_all <- nv == length(SNOW_YEARS)

sm <- values(predictors[["snow_mean"]])[, 1]
mean_allyears_doypos <- mean(sm[mask_all])

raw <- values(rast(unname(snow_files())))
mask_raw <- rowSums(is.na(raw)) == 0L
mean_allyears_zeroincl <- mean(rowMeans(raw[mask_raw, , drop = FALSE]))
rm(raw)

mean_avail <- mean(sm, na.rm = TRUE)

cat("\n-- snow_mean, three conventions --\n")
cat(sprintf("  all-years-valid, DOY>0      : %.4f  (n = %d)\n",
            mean_allyears_doypos, sum(mask_all)))
cat(sprintf("  all-years-valid, 0 included : %.4f  (n = %d)  [legacy]\n",
            mean_allyears_zeroincl, sum(mask_raw)))
cat(sprintf("  per-cell available, DOY>0   : %.4f  (n = %d)\n",
            mean_avail, sum(!is.na(sm))))
cat(sprintf("  snow_mean NA cells          : %d / %d\n\n",
            sum(is.na(sm)), length(sm)))

# The plan quotes 170.4257 as "the mean over cells valid in all 10 years under
# the DOY > 0 rule". That is not reproducible: 170.4257 is the mean of the same
# cell set WITHOUT the DOY > 0 rule (the legacy convention, in which the few
# thousand 0 = sky pixels are averaged in as melt dates). Both numbers are
# therefore checked: the legacy one proves the new pipeline reads exactly the
# same data as the old one, and the DOY > 0 one is the value the manuscript
# should quote.
tst$check_near("legacy mean (all years, zeros included) = 170.4257",
               mean_allyears_zeroincl, REF_SNOW_MEAN_ALLYEARS_ZERO_INCL,
               TOL_SNOW_MEAN)
tst$check_near("mean (all years valid, DOY > 0)",
               mean_allyears_doypos, REF_SNOW_MEAN_ALLYEARS_DOY_POS,
               TOL_SNOW_MEAN)
tst$check_near("mean of the snow_mean layer as written (DOY > 0)",
               mean_avail, REF_SNOW_MEAN_AVAILYEARS_DOY_POS, TOL_SNOW_MEAN)

# ---- 4. no NaN leakage ------------------------------------------------------
# GeoTIFF stores float NoData as NaN, so terra legitimately returns NaN for
# masked cells and is.nan() cannot distinguish "masked" from "leaked". What
# actually has to hold is: (a) the missing mask is exactly the intended one,
# and (b) no non-missing cell carries a non-finite value.
tst$check("snow_mean missing mask == cells with 0 valid years",
          sum(is.na(sm)) == sum(nv == 0L) &&
          all(is.na(sm) == (nv == 0L)),
          sprintf("%d missing vs %d cells with 0 valid years",
                  sum(is.na(sm)), sum(nv == 0L)))
tst$check("snow_mean: every non-missing value is finite and within the DOY range",
          all(is.finite(sm[!is.na(sm)])) &&
          min(sm, na.rm = TRUE) > 0 && max(sm, na.rm = TRUE) < 366,
          sprintf("range [%.2f, %.2f]",
                  min(sm, na.rm = TRUE), max(sm, na.rm = TRUE)))
cat("\n-- missing-value census per layer --\n")
for (nm in names(predictors)) {
  v <- values(predictors[[nm]])[, 1]
  tst$check(paste0("finite non-missing values in ", nm),
            all(is.finite(v[!is.na(v)])),
            sprintf("missing = %d (%.3f%%)", sum(is.na(v)),
                    100 * mean(is.na(v))))
}

# ---- 5. VIF and Spearman ----------------------------------------------------
vif <- read.csv(PATH_PRED_VIF, stringsAsFactors = FALSE)
tst$check("VIF table covers exactly the 7 predictors",
          setequal(vif$predictor, PREDICTOR_NAMES))
tst$check("aspect absent from the VIF table", !("aspect" %in% vif$predictor))
tst$check(sprintf("all VIF < %.1f (max = %.4f)", VIF_MAX, max(vif$vif)),
          all(vif$vif < VIF_MAX))

sp <- read.csv(PATH_PRED_SPEARMAN, stringsAsFactors = FALSE, check.names = FALSE)
M <- as.matrix(sp[, -1, drop = FALSE])
rownames(M) <- sp[[1]]
tst$check("Spearman matrix is 7 x 7 over the predictors",
          identical(dim(M), c(7L, 7L)) && setequal(rownames(M), PREDICTOR_NAMES))
off <- M; diag(off) <- 0
tst$check(sprintf("all |Spearman r| < %.1f (max = %.4f)",
                  SPEARMAN_MAX, max(abs(off))),
          max(abs(off)) < SPEARMAN_MAX)

# ---- 6. scenario constants --------------------------------------------------
sc <- read.csv(PATH_SNOW_SCENARIOS, stringsAsFactors = FALSE)
tst$check("scenario CSV holds 0 / -0.71 / -2.24 d per year",
          isTRUE(all.equal(sort(sc$shift_d_yr), c(-2.24, -0.71, 0.00))),
          paste(sc$shift_d_yr, collapse = ", "))

tst$report()
cat("\nWP1 acceptance: all checks passed.\n")
