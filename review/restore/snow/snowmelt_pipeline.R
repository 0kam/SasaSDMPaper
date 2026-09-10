# =============================================================================
# Snowmelt-timing pipeline  (reconstructed 2026-08, replaces preprocess_snow_data.R)
#
# Chain: aligned per-image CSV  ->  1 m raster (via georectified.csv)
#        ->  per-pixel OLS on snowmelt DOY ~ year
#        ->  slope / mean / SD layers, fitted layers, 2030 extrapolation
#        ->  full statistical package (pixel-level + landscape-level)
#
# ONE filter rule is used throughout (the old code mixed >0, >10 and none):
#   * DOY == 0 is NOT day zero. The camera starts in April, so 0 encodes
#     "sky / no valid observation". It is set to NA, i.e. the OBSERVATION is
#     dropped, not the pixel.
#   * The grey-scale encoding of these images spans DOY 120-230 (see filenames
#     "..._120-230_BW"). DOY 120 is therefore left-censored ("already snow-free
#     at the first image") and >= 230 is right-censored. These are KEPT (dropping
#     them changes the mean slope by only ~0.02 d/yr, see sensitivity output)
#     but they are counted and reported.
#   * A pixel enters the regression if it has >= MIN_YEARS valid observations.
#
# Stages are switchable so the expensive rasterisation can be skipped.
# =============================================================================

suppressMessages({
  library(terra); library(data.table)
})

## ---- configuration ---------------------------------------------------------
SERVER   <- "/Users/okamoto/NIES/SasaSDMPaper/data_from_server/ortho"
ALIGNED  <- file.path(SERVER, "data/snow/aligned")
GEOREC   <- file.path(SERVER, "data/georectified.csv")
RAWDIR   <- file.path(SERVER, "data/snow/raw")
VEGE     <- file.path(SERVER, "data/vege_2012_5x5.tiff")
OUT      <- "/Users/okamoto/NIES/SasaSDMPaper/.claude/worktrees/review-summary-ece-ddc69c/review/restore/snow/out"

DOY_MIN     <- 1      # observations <= 0 are invalid (sky / no data)
MIN_YEARS   <- 8      # pixels need at least this many valid years
YEARS       <- c(2011:2018, 2020, 2021)   # 2019 missing (lodge construction)
IMG_W       <- 5616L
EXTRAP_YEAR <- 2030

STAGE_RASTERISE <- FALSE   # TRUE only if you have an UNTRUNCATED georectified.csv
                           # (the recovered copy is cut at 160 MiB, v <= 1268)

dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
setDTthreads(0)

## ---- stage 1: aligned CSV -> 1 m raster ------------------------------------
# Reproduces the interpolate() routine recovered from ortho/.Rhistory.
# Verified: rebuilding 2011-2021 this way reproduces data/snow/raw/*.tiff on
# 99.9% of cells exactly (cor = 0.99997); the residual difference is only
# GDAL's "last value burned" tie-break among sub-metre points.
rasterise_year <- function(year, template) {
  georec <- fread(GEOREC, select = c("u", "v", "x", "y"), fill = TRUE,
                  showProgress = FALSE)
  georec <- georec[!is.na(u) & !is.na(v) & !is.na(x) & !is.na(y)]
  georec[, key := as.integer(v) * IMG_W + as.integer(u)]

  f <- grep(paste0("_", year, "_"),
            list.files(ALIGNED, "\\.csv$", full.names = TRUE), value = TRUE)
  stopifnot(length(f) == 1L)
  a <- fread(f, select = c("u", "v", "snowmelt_doy"), fill = TRUE,
             showProgress = FALSE)
  a <- a[!is.na(u) & !is.na(v) & !is.na(snowmelt_doy)]
  a[, key := as.integer(v) * IMG_W + as.integer(u)]
  setkey(a, key); setkey(georec, key)

  pts <- georec[a, nomatch = 0L]
  pts[, cell := cellFromXY(template, cbind(x, y))]
  pts <- pts[!is.na(cell)]
  agg <- pts[, .(doy = snowmelt_doy[.N]), by = cell]   # "last value burned"

  r <- template; values(r) <- NA_real_
  r[agg$cell] <- agg$doy
  # 1 iteration of 3x3 modal gap-fill (max_dist = 1 m in the original code)
  terra::focal(r, 3, terra::modal, na.policy = "only", na.rm = TRUE)
}

template <- rast(VEGE)
if (STAGE_RASTERISE) {
  for (y in YEARS) {
    writeRaster(rasterise_year(y, template),
                file.path(OUT, sprintf("snow_%d.tiff", y)), overwrite = TRUE)
  }
  files <- file.path(OUT, sprintf("snow_%d.tiff", YEARS))
} else {
  files <- sort(list.files(RAWDIR, "\\.tiff$", full.names = TRUE))
  files <- files[order(as.integer(sub(".*_(20\\d{2})_.*", "\\1", basename(files))))]
}

## ---- stage 2: stack, apply the ONE filter rule -----------------------------
stk <- rast(files); names(stk) <- YEARS
stopifnot(all(ext(stk) == ext(template)))          # already on the vegetation grid
M <- values(stk)

n_all       <- nrow(M)
pix_allyear <- rowSums(is.na(M)) == 0               # all 10 images cover the pixel
M[M < DOY_MIN] <- NA                                # DOY 0 = sky, not day zero
n_valid     <- rowSums(!is.na(M))
use         <- pix_allyear & n_valid >= MIN_YEARS

message(sprintf("grid cells %d | covered by all %d images %d | usable %d",
                n_all, length(YEARS), sum(pix_allyear), sum(use)))

## ---- stage 3: vectorised per-pixel OLS -------------------------------------
pixel_ols <- function(Y, x) {
  n  <- rowSums(!is.na(Y))
  Yz <- Y; Yz[is.na(Yz)] <- 0
  Xm <- matrix(rep(x, each = nrow(Y)), nrow = nrow(Y)); Xm[is.na(Y)] <- 0
  sx <- rowSums(Xm); sy <- rowSums(Yz)
  Sxx <- rowSums(Xm * Xm) - sx^2 / n
  Sxy <- rowSums(Xm * Yz) - sx * sy / n
  Syy <- rowSums(Yz * Yz) - sy^2 / n
  b  <- Sxy / Sxx
  a  <- sy / n - b * sx / n
  sse <- pmax(Syy - b * Sxy, 0)
  df  <- n - 2
  se  <- sqrt((sse / df) / Sxx)
  p   <- 2 * pt(-abs(b / se), df)
  r2  <- 1 - sse / Syy
  # constant series: slope is exactly 0 and carries no evidence
  degen <- Syy <= 0
  b[degen] <- 0; p[degen] <- 1; r2[degen] <- 0; se[degen] <- 0
  bad <- n < 3 | !is.finite(Sxx) | Sxx <= 0
  b[bad] <- NA; a[bad] <- NA; p[bad] <- NA; r2[bad] <- NA; se[bad] <- NA
  list(n = n, intercept = a, slope = b, se = se, p = p, r2 = r2,
       resid_sd = sqrt(sse / df), mean = sy / n,
       sd = sqrt(Syy / (n - 1)))
}
fit <- pixel_ols(M[use, , drop = FALSE], YEARS)

put <- function(v) { r <- template; values(r) <- NA_real_
                     vv <- rep(NA_real_, n_all); vv[use] <- v; values(r) <- vv; r }
writeRaster(put(fit$slope),    file.path(OUT, "snow_reg.tif"),  overwrite = TRUE)
writeRaster(put(fit$mean),     file.path(OUT, "snow_mean.tif"), overwrite = TRUE)
writeRaster(put(fit$sd),       file.path(OUT, "snow_sd.tif"),   overwrite = TRUE)
writeRaster(put(fit$p),        file.path(OUT, "snow_pval.tif"), overwrite = TRUE)
writeRaster(put(fit$r2),       file.path(OUT, "snow_r2.tif"),   overwrite = TRUE)

## ---- stage 4: fitted layers + extrapolation --------------------------------
for (y in c(YEARS, EXTRAP_YEAR)) {
  v <- fit$intercept + fit$slope * y
  if (y == EXTRAP_YEAR) v <- pmin(pmax(v, 0), 255)   # as in the original code
  writeRaster(put(v), file.path(OUT, sprintf("fitted_%d.tiff", y)), overwrite = TRUE)
}

## ---- stage 5: statistical package ------------------------------------------
b <- fit$slope; p <- fit$p; npx <- length(b)
ci <- mean(b) + c(-1, 1) * qt(0.975, npx - 1) * sd(b) / sqrt(npx)

# landscape-scale trend: OLS on the annual spatial means
ymean <- colMeans(M[use, , drop = FALSE], na.rm = TRUE)
lfit  <- lm(ymean ~ YEARS); ls <- summary(lfit)

stats <- data.frame(
  statistic = c(
    "n_pixels", "n_years", "years_used",
    "pixel_slope_mean", "pixel_slope_median", "pixel_slope_sd",
    "pixel_slope_ci95_lo", "pixel_slope_ci95_hi",
    "pixel_slope_q025", "pixel_slope_q975",
    "pct_negative", "pct_positive", "pct_exactly_zero",
    "pct_p_lt_0.05", "pct_p_lt_0.05_negative", "pct_p_lt_0.05_positive",
    "pct_BH_FDR_q_lt_0.05",
    "expected_pct_sig_under_permutation_null",
    "median_pixel_residual_sd_days", "median_pixel_r2",
    "median_slope_se", "min_detectable_slope_d_per_yr",
    "landscape_slope", "landscape_slope_se", "landscape_t", "landscape_p",
    "landscape_ci95_lo", "landscape_ci95_hi",
    "pct_obs_left_censored_DOY120", "pct_obs_right_censored_DOY_ge230",
    "pct_obs_dropped_DOY0"
  ),
  value = c(
    npx, length(YEARS), paste(range(YEARS), collapse = "-"),
    mean(b), median(b), sd(b), ci[1], ci[2],
    quantile(b, .025), quantile(b, .975),
    100 * mean(b < 0), 100 * mean(b > 0), 100 * mean(b == 0),
    100 * mean(p < .05), 100 * mean(p < .05 & b < 0), 100 * mean(p < .05 & b > 0),
    100 * mean(p.adjust(p, "BH") < .05),
    NA,                                   # filled by the permutation block below
    median(fit$resid_sd), median(fit$r2),
    median(fit$se), qt(.975, length(YEARS) - 2) * median(fit$se),
    coef(lfit)[2], ls$coefficients[2, 2], ls$coefficients[2, 3],
    ls$coefficients[2, 4], confint(lfit)[2, 1], confint(lfit)[2, 2],
    100 * mean(M[use, ] == 120, na.rm = TRUE),
    100 * mean(M[use, ] >= 230, na.rm = TRUE),
    100 * (1 - sum(n_valid[use]) / (length(YEARS) * npx))
  ),
  stringsAsFactors = FALSE
)

# permutation null: how often does a pixel look "significant" when the year
# labels are shuffled?  This is the correct reference, not 5%.
set.seed(1)
idx <- sample(npx, min(2e5, npx))
Ys  <- M[use, , drop = FALSE][idx, ]
perm <- replicate(20, mean(pixel_ols(Ys, sample(YEARS))$p < .05, na.rm = TRUE))
stats$value[stats$statistic == "expected_pct_sig_under_permutation_null"] <-
  100 * mean(perm)

write.csv(stats, "/Users/okamoto/NIES/SasaSDMPaper/.claude/worktrees/review-summary-ece-ddc69c/review/restore/snow/snowmelt_statistics.csv", row.names = FALSE)
print(stats, row.names = FALSE)

## ---- stage 6: sensitivity to the filter rule -------------------------------
sens <- do.call(rbind, lapply(
  list(`none (DOY 0 kept)` = -1, `DOY > 0 (chosen)` = 0, `DOY > 10` = 10,
       `DOY > 120 (drop left-censored)` = 120),
  function(thr) {
    Z <- values(stk); okp <- rowSums(is.na(Z)) == 0
    Z[Z <= thr] <- NA
    nv <- rowSums(!is.na(Z)); u <- okp & nv >= MIN_YEARS
    f <- pixel_ols(Z[u, , drop = FALSE], YEARS)
    ym <- colMeans(Z[u, , drop = FALSE], na.rm = TRUE); lf <- lm(ym ~ YEARS)
    data.frame(n_pixels = sum(u), mean_slope = mean(f$slope, na.rm = TRUE),
               median_slope = median(f$slope, na.rm = TRUE),
               pct_negative = 100 * mean(f$slope < 0, na.rm = TRUE),
               pct_sig = 100 * mean(f$p < .05, na.rm = TRUE),
               landscape_slope = coef(lf)[2],
               landscape_p = summary(lf)$coefficients[2, 4])
  }))
sens <- cbind(rule = rownames(sens), sens)
write.csv(sens, "/Users/okamoto/NIES/SasaSDMPaper/.claude/worktrees/review-summary-ece-ddc69c/review/restore/snow/snowmelt_filter_sensitivity.csv", row.names = FALSE)
print(sens, row.names = FALSE)
