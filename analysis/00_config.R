# =============================================================================
# 00_config.R -- shared configuration and helpers for the HSM redesign analysis
#
# Sourced by every analysis script and every test. Contains the ONLY hardcoded
# absolute path in the pipeline (REPO_ROOT), all constants, all random seeds,
# and the helper functions that are shared between work packages.
#
# Usage:  source(file.path(REPO_ROOT, "analysis", "00_config.R"))
#         ...or simply source("analysis/00_config.R") from the repository root.
#
# Everything is written so that scripts run front to back non-interactively:
#     Rscript analysis/01_predictors.R
# No setwd() to a user home directory, no interactive prompts.
# =============================================================================

# ---- repository root --------------------------------------------------------
# Single point of truth. Override with the environment variable SASA_REPO_ROOT
# if the repository is checked out elsewhere (e.g. a git worktree).
REPO_ROOT <- Sys.getenv("SASA_REPO_ROOT", unset = "/Users/okamoto/NIES/SasaSDMPaper")
if (!dir.exists(REPO_ROOT)) {
  stop("REPO_ROOT does not exist: ", REPO_ROOT,
       " (set the SASA_REPO_ROOT environment variable)")
}

# ---- packages ---------------------------------------------------------------
suppressPackageStartupMessages({
  library(terra)
  library(ggplot2)
})

# ---- paths ------------------------------------------------------------------
DIR_ORTHO_DATA <- file.path(REPO_ROOT, "ortho", "data")
DIR_SNOW_RAW   <- file.path(DIR_ORTHO_DATA, "snow", "raw")
DIR_TERRAIN    <- file.path(DIR_ORTHO_DATA, "terrain_features")
DIR_ANALYSIS   <- file.path(REPO_ROOT, "analysis")
DIR_OUT        <- file.path(DIR_ANALYSIS, "out")
dir.create(DIR_OUT, showWarnings = FALSE, recursive = TRUE)

# Reference grid. EVERY raster written by this pipeline must sit exactly on it.
PATH_VEGE_2012 <- file.path(DIR_ORTHO_DATA, "vege_2012_5x5.tiff")
PATH_VEGE_2021 <- file.path(DIR_ORTHO_DATA, "vege_2021_5x5.tiff")

# Output files produced by WP1 / WP6.
PATH_PREDICTORS      <- file.path(DIR_OUT, "predictors.tif")
PATH_SNOW_OLS        <- file.path(DIR_OUT, "snowmelt_ols_trend.tif")
PATH_SNOW_SCENARIOS  <- file.path(DIR_OUT, "snowmelt_scenarios.csv")
PATH_PRED_SPEARMAN   <- file.path(DIR_OUT, "predictor_correlation_spearman.csv")
PATH_PRED_VIF        <- file.path(DIR_OUT, "predictor_vif.csv")
PATH_SNOW_MEAN_SUMM  <- file.path(DIR_OUT, "snow_mean_summary.csv")
PATH_SNOW_STATS      <- file.path(DIR_OUT, "snowmelt_statistics.csv")
PATH_SNOW_ANNUAL     <- file.path(DIR_OUT, "snowmelt_annual_means.csv")

# Shared WP2/WP3 artefacts.
PATH_DIST12          <- file.path(DIR_OUT, "dist12.tif")
PATH_FOLDS_RASTER    <- file.path(DIR_OUT, "folds.tif")
PATH_FOLDS_VECTOR    <- file.path(DIR_OUT, "folds.gpkg")
DIR_MODELS           <- file.path(DIR_OUT, "models")
dir.create(DIR_MODELS, showWarnings = FALSE, recursive = TRUE)
PATH_MODEL_B_PRIMARY <- file.path(DIR_MODELS, "model_B_gam.rds")
PATH_MODEL_B_LOGDIST <- file.path(DIR_MODELS, "model_B_gam_logdist.rds")
PATH_MODEL_B_LOGDIST_COMPARISON <- file.path(
  DIR_OUT, "model_B_logdist_comparison.csv"
)

# Resolve the Model-B inference object from Patch 4's explicit verdict. This is
# evaluated when a downstream script runs, after 04_model_B.R has rewritten the
# comparison artefact in the current execution mode.
model_b_inference_path <- function() {
  if (!file.exists(PATH_MODEL_B_LOGDIST_COMPARISON)) {
    return(PATH_MODEL_B_PRIMARY)
  }
  comparison <- utils::read.csv(
    PATH_MODEL_B_LOGDIST_COMPARISON, stringsAsFactors = FALSE
  )
  verdict <- unique(comparison$verdict[
    !is.na(comparison$verdict) & nzchar(comparison$verdict)
  ])
  if (length(verdict) != 1L || !verdict %in% c("ADOPT", "REJECT")) {
    stop("Model-B log-distance comparison has no unique ADOPT/REJECT verdict")
  }
  if (identical(verdict, "ADOPT")) PATH_MODEL_B_LOGDIST else PATH_MODEL_B_PRIMARY
}

# ---- coordinate reference system -------------------------------------------
# Analysis CRS = CRS of the vegetation / snow rasters.
CRS_ANALYSIS <- "EPSG:6690"   # JGD2011 / UTM zone 53N
# The terrain_features/*.tif rasters are stored in a DIFFERENT CRS:
CRS_TERRAIN_SOURCE <- "EPSG:3099"  # JGD2000 / UTM zone 53N
# ... so they must be reprojected (terra::project), not merely resampled.

# ---- random seeds -----------------------------------------------------------
SEED_GLOBAL       <- 20260813L  # generic seed set at the top of each script
SEED_VIF_SAMPLE   <- 1001L      # sample of cells used for correlation / VIF
SEED_FOLDS        <- 2001L      # shared spatial-block assignment
SEED_MODEL_A      <- 3001L      # Model A sampling, tuning and blending
SEED_MODEL_B      <- 4001L      # Model B sampling, tuning and blending
SEED_CA           <- 5001L      # 2021-2030 cellular-automaton projection
SEED_CA_HINDCAST  <- 5101L      # 2012-2021 calibration-check simulations
SEED_CA_SPATIAL   <- 5201L      # held-block spatial-validation simulations
SEED_CA_SENSITIVITY <- 5301L    # reduced sensitivity simulations
set.seed(SEED_GLOBAL)

# ---- execution mode ---------------------------------------------------------
# The reviewer runs the default mode. SASA_SMOKE=1 is deliberately small enough
# for an end-to-end integration check while retaining every pipeline stage.
SASA_SMOKE <- identical(Sys.getenv("SASA_SMOKE", unset = "0"), "1")
CV_FOLDS_FULL <- 4L
CV_FOLDS_SMOKE <- 2L
TUNE_GRID_A <- if (SASA_SMOKE) 2L else 18L
TUNE_GRID_B <- if (SASA_SMOKE) 2L else 8L
# Full-mode background sample size for the Model B robustness companion.
N_COMPANION_NEG <- 150000L
MODEL_THREADS <- 10L

# ---- cellular-automaton projection -----------------------------------------
# The 2011-2021 snow climatology is anchored at its 2016 midpoint. Scenario
# shifts are applied relative to that midpoint, rather than treating either
# endpoint of the climatology window as its representative year.
ANCHOR_YEAR <- 2016L
N_CA_REPLICATES <- 200L
N_CA_REPLICATES_SMOKE <- 5L
N_CA_HINDCAST_REPLICATES <- 200L
N_CA_SPATIAL_REPLICATES <- 50L
N_CA_SENSITIVITY_REPLICATES <- 50L
CA_MAX_WORKERS <- 10L
# PSOCK is the production default because terra/GDAL state is not fork-safe.
# The sequential option is only for restricted runners that prohibit the local
# server socket required by PSOCK (for example, a sandboxed integration check).
CA_PARALLEL_BACKEND <- Sys.getenv(
  "SASA_CA_PARALLEL_BACKEND", unset = "PSOCK"
)
if (!CA_PARALLEL_BACKEND %in% c("PSOCK", "sequential")) {
  stop("SASA_CA_PARALLEL_BACKEND must be PSOCK or sequential")
}

# ---- snowmelt data constants ------------------------------------------------
# Time-lapse camera derived snowmelt day-of-year (DOY) rasters.
#
#   * 10 years available: 2011-2018, 2020, 2021.
#   * 2019 is MISSING: the camera could not be operated because of lodge
#     construction work at the site.
#   * 2010 is EXCLUDED upstream: a different lens / field of view makes the
#     2010 image geometrically incompatible with the rest of the series.
#   * Pixel value 0 means SKY / invalid (the camera starts operating in April,
#     so no melt date can be assigned); it is NOT a melt date and is treated
#     as NA everywhere in this pipeline (the "DOY > 0" rule).
#   * Values are effectively censored to roughly [120, 230] by the acquisition
#     window of the camera (file names carry the 120-230 tag). This censoring
#     is documented but NOT "corrected" for.
SNOW_YEARS       <- c(2011L, 2012L, 2013L, 2014L, 2015L, 2016L, 2017L, 2018L,
                      2020L, 2021L)
SNOW_YEAR_MISSING <- 2019L
DOY_MIN_VALID    <- 0L   # values <= DOY_MIN_VALID are set to NA
OLS_MIN_YEARS    <- 8L   # minimum valid years for a per-pixel OLS trend

# Snowmelt shift scenarios (days per year) used by the projection layer.
# The trend is used as a SCENARIO ONLY (decisions.md section 7): the landscape
# scale trend is not statistically significant, so it parameterises scenarios
# rather than entering the model as a fitted process.
#   0.00  : no change
#  -0.71  : mean of the per-pixel OLS slopes (also ~ the landscape point est.)
#  -2.24  : lower 95% confidence limit of the landscape-scale trend
SNOW_SCENARIOS <- data.frame(
  scenario   = c("none", "point_estimate", "ci_lower"),
  shift_d_yr = c(0.00, -0.71, -2.24),
  source     = c("no change",
                 "mean of per-pixel OLS slopes / landscape point estimate",
                 "lower 95% CI limit of the landscape-scale OLS trend"),
  stringsAsFactors = FALSE
)

# ---- predictor set ----------------------------------------------------------
# roughness.tif and TRI.tif are deliberately EXCLUDED (collinear with slope;
# decisions.md section 7). Raw `aspect` never enters the stack -- it is
# decomposed into northness / eastness, which are the modelled quantities.
PREDICTOR_NAMES <- c("slope", "elevation", "TPI", "twi",
                     "northness", "eastness", "snow_mean")

# Diagnostics thresholds (acceptance criteria).
VIF_MAX      <- 5
SPEARMAN_MAX <- 0.7
N_DIAG_SAMPLE <- 200000L

# ---- verified reference values ---------------------------------------------
# WP1: three different global means of the snowmelt climatology. They differ
# only in how invalid (0) pixels and partially observed pixels are handled.
#
#   REF_SNOW_MEAN_ALLYEARS_ZERO_INCL
#       Legacy value carried in the manuscript / previous pipeline. Computed
#       over cells that are non-NA in all 10 rasters, WITHOUT applying the
#       DOY > 0 rule (i.e. the few thousand 0 = sky pixels are averaged in as
#       if they were melt dates). Reproduced here to prove the new pipeline
#       reads the same data as the old one.
#   REF_SNOW_MEAN_ALLYEARS_DOY_POS
#       Same cell set, but WITH the DOY > 0 rule. This is the scientifically
#       correct number for "cells valid in all 10 years"; it is 0.83 d larger
#       than the legacy value, because dropping 0s removes a downward bias.
#   REF_SNOW_MEAN_AVAILYEARS_DOY_POS
#       Global mean of the snow_mean layer as actually written (per-pixel mean
#       over the years available for that pixel, DOY > 0 rule).
REF_SNOW_MEAN_ALLYEARS_ZERO_INCL <- 170.4257
REF_SNOW_MEAN_ALLYEARS_DOY_POS   <- 171.2591
REF_SNOW_MEAN_AVAILYEARS_DOY_POS <- 170.8567
TOL_SNOW_MEAN <- 0.05

# WP6: per-pixel OLS trend statistics (DOY > 0, >= 8 valid years) and the
# landscape-scale test. These replace an incorrect published number.
REF_SLOPE_N            <- 1206063L; TOL_SLOPE_N <- 500L
REF_SLOPE_MEAN         <- -0.7146;  TOL_SLOPE_MEAN <- 0.002
REF_SLOPE_MEDIAN       <- -0.6264;  TOL_SLOPE_MEDIAN <- 0.002
# The plan quotes the SD range as 0.6323-0.6344; the realised value is
# 0.634448, i.e. 0.6344 at the quoted 4-decimal precision. The upper bound is
# widened by one unit in the last quoted place so that the rounded reference
# is not rejected by its own rounding.
REF_SLOPE_SD_RANGE     <- c(0.6323, 0.63450)
REF_SLOPE_PCT_NEG_RANGE <- c(91.65, 91.97)
REF_SLOPE_PCT_P05      <- 1.96;     TOL_SLOPE_PCT_P05 <- 0.05
REF_SLOPE_PCT_FDR      <- 0.0

REF_LAND_SLOPE <- -0.6885; TOL_LAND_SLOPE <- 0.005
REF_LAND_SE    <-  0.6747; TOL_LAND_SE    <- 0.005
REF_LAND_P     <-  0.337;  TOL_LAND_P     <- 0.005
REF_LAND_CI    <- c(-2.244, 0.868); TOL_LAND_CI <- 0.005

# =============================================================================
# Helpers
# =============================================================================

msg <- function(...) {
  cat(format(Sys.time(), "[%H:%M:%S] "), ..., "\n", sep = "")
}

#' Read the reference grid (values dropped, geometry only)
ref_grid <- function() {
  r <- terra::rast(PATH_VEGE_2012)
  terra::rast(r)  # geometry-only copy: same ext/res/crs, no values
}

#' Assert that a SpatRaster sits exactly on the reference grid.
#'
#' Compares extent, resolution, origin and dimensions using identical() on the
#' raw doubles -- i.e. bit-exact, not "close enough". CRS is compared by EPSG
#' code because WKT serialisation is not stable across GDAL versions.
assert_on_ref_grid <- function(x, label = deparse(substitute(x)),
                               template = ref_grid()) {
  problems <- character(0)
  if (!identical(as.vector(terra::ext(x)), as.vector(terra::ext(template)))) {
    problems <- c(problems, sprintf(
      "ext differs: %s vs %s",
      paste(format(as.vector(terra::ext(x)), digits = 15), collapse = ","),
      paste(format(as.vector(terra::ext(template)), digits = 15), collapse = ",")))
  }
  if (!identical(terra::res(x), terra::res(template))) {
    problems <- c(problems, sprintf("res differs: %s vs %s",
                                    paste(terra::res(x), collapse = ","),
                                    paste(terra::res(template), collapse = ",")))
  }
  if (!identical(terra::origin(x), terra::origin(template))) {
    problems <- c(problems, sprintf("origin differs: %s vs %s",
                                    paste(format(terra::origin(x), digits = 15), collapse = ","),
                                    paste(format(terra::origin(template), digits = 15), collapse = ",")))
  }
  if (!identical(dim(x)[1:2], dim(template)[1:2])) {
    problems <- c(problems, sprintf("dim differs: %s vs %s",
                                    paste(dim(x)[1:2], collapse = "x"),
                                    paste(dim(template)[1:2], collapse = "x")))
  }
  code_x <- terra::crs(x, describe = TRUE)$code
  code_t <- terra::crs(template, describe = TRUE)$code
  if (!identical(code_x, code_t)) {
    problems <- c(problems, sprintf("EPSG differs: %s vs %s", code_x, code_t))
  }
  if (length(problems)) {
    stop("grid mismatch for '", label, "':\n  ", paste(problems, collapse = "\n  "))
  }
  invisible(TRUE)
}

#' List the raw snowmelt rasters, ordered by year.
snow_files <- function() {
  fs <- list.files(DIR_SNOW_RAW, pattern = "[.]tiff?$", full.names = TRUE)
  yr <- as.integer(sub(".*_L_([0-9]{4})_.*", "\\1", basename(fs)))
  if (anyNA(yr)) stop("could not parse a year from: ",
                      paste(basename(fs)[is.na(yr)], collapse = ", "))
  o <- order(yr)
  fs <- fs[o]; yr <- yr[o]
  if (!identical(yr, SNOW_YEARS)) {
    stop("snowmelt years on disk (", paste(yr, collapse = ","),
         ") do not match SNOW_YEARS (", paste(SNOW_YEARS, collapse = ","), ")")
  }
  stats::setNames(fs, paste0("snow_", yr))
}

#' Read the raw snowmelt stack, apply the DOY > 0 rule, and return the value
#' matrix together with the geometry template.
#'
#' Everything downstream works on this matrix; the pipeline never round-trips
#' through stars/tibbles. (A previous pipeline used
#' `stars::as_tibble(add_max = TRUE)` and re-imported cell CORNERS as cell
#' CENTRES, shifting every derived raster by half a pixel -- origin (0.5, -0.25)
#' instead of (0, 0.25). Staying inside terra and reusing the source geometry
#' makes that class of bug impossible.)
#'
#' @return list(values = matrix [ncell x nyear], years = integer, template =
#'   geometry-only SpatRaster, n_valid = integer vector of valid years / cell)
load_snow_matrix <- function() {
  fs <- snow_files()
  s  <- terra::rast(unname(fs))
  names(s) <- names(fs)
  assert_on_ref_grid(s, "raw snow stack")
  v <- terra::values(s)
  v[v <= DOY_MIN_VALID] <- NA_real_        # DOY > 0 rule: 0 = sky / invalid
  storage.mode(v) <- "double"
  list(values   = v,
       years    = SNOW_YEARS,
       template = terra::rast(s[[1]]),
       n_valid  = as.integer(rowSums(!is.na(v))))
}

#' Vectorised per-row OLS fit of y ~ x with missing values allowed.
#'
#' Closed-form weighted sums; equivalent to running lm() per row but ~4 orders
#' of magnitude faster for 1.2 million rows. Rows with fewer than `min_n`
#' finite observations, or with zero variance in x among the observed points,
#' return NA.
#'
#' @param Y numeric matrix [n_row x n_obs], NA allowed
#' @param x numeric vector of length n_obs (the predictor, here: year)
#' @param min_n minimum number of valid observations per row
#' @return data.frame with columns n, intercept, slope, se, t, p, r2
row_ols <- function(Y, x, min_n = OLS_MIN_YEARS) {
  stopifnot(is.matrix(Y), ncol(Y) == length(x))
  W  <- !is.na(Y)
  Yz <- Y; Yz[!W] <- 0
  Wn <- W * 1.0

  n   <- as.vector(Wn %*% rep(1, length(x)))
  Sx  <- as.vector(Wn %*% x)
  Sxx <- as.vector(Wn %*% (x^2))
  Sy  <- as.vector(rowSums(Yz))
  Sxy <- as.vector(Yz %*% x)
  Syy <- as.vector(rowSums(Yz^2))

  Sxxc <- Sxx - Sx^2 / n          # centred sum of squares of x
  Sxyc <- Sxy - Sx * Sy / n
  Syyc <- Syy - Sy^2 / n

  ok <- n >= min_n & is.finite(Sxxc) & Sxxc > 0
  slope <- rep(NA_real_, nrow(Y))
  slope[ok] <- Sxyc[ok] / Sxxc[ok]
  intercept <- rep(NA_real_, nrow(Y))
  intercept[ok] <- (Sy[ok] - slope[ok] * Sx[ok]) / n[ok]

  sse <- rep(NA_real_, nrow(Y))
  sse[ok] <- pmax(Syyc[ok] - slope[ok] * Sxyc[ok], 0)
  df <- n - 2L
  se <- rep(NA_real_, nrow(Y))
  good_se <- ok & df > 0
  se[good_se] <- sqrt(sse[good_se] / df[good_se] / Sxxc[good_se])

  tval <- slope / se
  pval <- rep(NA_real_, nrow(Y))
  fin  <- is.finite(tval) & df > 0
  pval[fin] <- 2 * stats::pt(-abs(tval[fin]), df = df[fin])

  r2 <- rep(NA_real_, nrow(Y))
  pos <- ok & is.finite(Syyc) & Syyc > 0
  r2[pos] <- 1 - sse[pos] / Syyc[pos]

  data.frame(n = ifelse(ok, n, NA_integer_),
             intercept = intercept, slope = slope, se = se,
             t = ifelse(is.finite(tval), tval, NA_real_),
             p = pval, r2 = r2)
}

#' Build a SpatRaster from a value vector using an explicit geometry template.
#' Guarantees the output geometry is the template's geometry, byte for byte.
rast_from_values <- function(values, template, name) {
  r <- terra::rast(template)
  terra::values(r) <- values
  names(r) <- name
  r
}

#' Variance inflation factors computed directly from R^2 of each predictor
#' regressed on the others (no dummy response variable needed).
vif_from_matrix <- function(X) {
  X <- as.matrix(X)
  nm <- colnames(X)
  out <- vapply(seq_along(nm), function(j) {
    fit <- stats::lm.fit(x = cbind(1, X[, -j, drop = FALSE]), y = X[, j])
    r2  <- 1 - sum(fit$residuals^2) / sum((X[, j] - mean(X[, j]))^2)
    1 / (1 - r2)
  }, numeric(1))
  stats::setNames(out, nm)
}

#' Print sessionInfo() plus the seeds, to be called at the end of every script.
finish_script <- function(script_name, t0) {
  msg(script_name, " finished in ",
      sprintf("%.1f", as.numeric(difftime(Sys.time(), t0, units = "secs"))), " s")
  cat("\n--- seeds ---\n")
  cat("SEED_GLOBAL      =", SEED_GLOBAL, "\n")
  cat("SEED_VIF_SAMPLE  =", SEED_VIF_SAMPLE, "\n")
  cat("SEED_FOLDS       =", SEED_FOLDS, "\n")
  cat("SEED_MODEL_A     =", SEED_MODEL_A, "\n")
  cat("SEED_MODEL_B     =", SEED_MODEL_B, "\n")
  cat("SEED_CA          =", SEED_CA, "\n")
  cat("SEED_CA_HINDCAST =", SEED_CA_HINDCAST, "\n")
  cat("SEED_CA_SPATIAL  =", SEED_CA_SPATIAL, "\n")
  cat("SEED_CA_SENSITIVITY =", SEED_CA_SENSITIVITY, "\n")
  cat("SASA_SMOKE       =", SASA_SMOKE, "\n")
  cat("\n--- sessionInfo() ---\n")
  print(utils::sessionInfo())
  invisible(NULL)
}

#' Small test harness: record a check, and stop with a non-zero exit status if
#' anything failed. Used by analysis/tests/*.R.
new_checker <- function(title) {
  results <- list()
  list(
    check = function(name, pass, detail = "") {
      results[[length(results) + 1L]] <<-
        list(name = name, pass = isTRUE(pass), detail = detail)
      cat(sprintf("%-4s %-52s %s\n", if (isTRUE(pass)) "PASS" else "FAIL",
                  name, detail))
      invisible(isTRUE(pass))
    },
    check_near = function(name, value, target, tol) {
      ok <- is.finite(value) && abs(value - target) <= tol
      results[[length(results) + 1L]] <<-
        list(name = name, pass = ok, detail = "")
      cat(sprintf("%-4s %-52s %.6f (target %.6f +/- %g)\n",
                  if (ok) "PASS" else "FAIL", name, value, target, tol))
      invisible(ok)
    },
    check_in = function(name, value, range) {
      ok <- is.finite(value) && value >= range[1] && value <= range[2]
      results[[length(results) + 1L]] <<-
        list(name = name, pass = ok, detail = "")
      cat(sprintf("%-4s %-52s %.6f (target [%g, %g])\n",
                  if (ok) "PASS" else "FAIL", name, value, range[1], range[2]))
      invisible(ok)
    },
    report = function() {
      n <- length(results)
      nf <- sum(!vapply(results, `[[`, logical(1), "pass"))
      cat(sprintf("\n%s: %d checks, %d failed\n", title, n, nf))
      if (nf > 0) {
        for (r in results) if (!r$pass) cat("  FAILED: ", r$name, "\n", sep = "")
        quit(status = 1, save = "no")
      }
      invisible(TRUE)
    }
  )
}
