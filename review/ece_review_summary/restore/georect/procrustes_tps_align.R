# ---------------------------------------------------------------------------
# procrustes_tps_align.R
#
# Runnable reconstruction of the Procrustes + thin-plate-spline block recovered
# from data_from_server/ortho/.Rhistory (history lines 81-190).
#
# WHAT THIS CODE ACTUALLY IS
# --------------------------
# It is NOT the georectification of this manuscript.  It is the *image-to-image
# alignment* step of the earlier snowmelt project
# (/media/okamoto/HDD3TB/tateyama/mrd_snowmelt/step2_ortho/): it warps one
# camera frame ("org") onto a reference frame ("sim") and then transfers map
# coordinates from a pre-existing per-pixel lookup table ("georectificated.csv",
# columns pix_num, x, y) that was itself produced by alproj.
#
# The control-point file it expects has columns org_x, org_y, sim_x, sim_y.
# data_from_server/ortho/data/gcp.csv has columns u, v, x, y, z (alproj format)
# and CANNOT be fed to this code as written.  No file with org_*/sim_* columns
# survives anywhere in the recovered tree.
#
# To exercise the code we therefore build an equivalent control-point set from
# the material that does survive:
#   org = the GCP positions observed in the photograph      (gcp.csv u, v)
#   sim = the same GCPs projected with the recovered camera (alproj project())
# This is the same kind of 2-D point-pair problem the original solved, so the
# reconstruction is genuinely executed rather than merely inspected, and the
# residuals it reports quantify what a TPS warp would add on top of alproj.
#
# Steps, verbatim in structure from .Rhistory:
#   1. mean translation org -> sim
#   2. shapes::procOPA  -> rotation R and scale s (full ordinary Procrustes)
#   3. fields::Tps      -> two thin-plate splines (x and y) on the residual
#   4. apply 1-3 to the full 5616 x 3744 image lattice, round to integer pixels
#   5. pix_num = (corr_y - 1) * width + corr_x, join to the lookup table
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse); library(shapes); library(fields)
})

SRV      <- "/Users/okamoto/NIES/SasaSDMPaper/data_from_server/ortho/data/"
OUT      <- "/Users/okamoto/NIES/SasaSDMPaper/.claude/worktrees/review-summary-ece-ddc69c/review/restore/georect/out/"
PAIRS    <- file.path(OUT, "tps_control_points.csv")   # written by make_tps_pairs.py
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

org_width  <- 5616; org_height <- 3744
sim_width  <- 5616; sim_height <- 3744

control_points <- read_csv(PAIRS, show_col_types = FALSE)
cat("control points:", nrow(control_points), "\n")

## --- 1. parallel translation -----------------------------------------------
org_m <- matrix(c(control_points$org_x, control_points$org_y), ncol = 2)
org_m <- cbind(org_m, rep(1, nrow(org_m)))
sim_m <- matrix(c(control_points$sim_x, control_points$sim_y), ncol = 2)
sim_m <- cbind(sim_m, rep(1, nrow(sim_m)))

xdiff <- mean(sim_m[, 1] - org_m[, 1])
ydiff <- mean(sim_m[, 2] - org_m[, 2])
p <- matrix(c(1, 0, xdiff, 0, 1, ydiff, 0, 0, 1), 3, 3)
org_p <- org_m %*% p

## --- 2. ordinary Procrustes (rotation + scale) -----------------------------
proc     <- procOPA(sim_m, org_p)
rotation <- proc$R
scale    <- rbind(diag(c(proc$s, proc$s)), c(0, 0)) %>% cbind(c(0, 0, 1))
org_aff  <- org_p %*% rotation %*% scale

rms <- function(a, b) sqrt(mean((a[, 1] - b[, 1])^2 + (a[, 2] - b[, 2])^2))
cat(sprintf("RMSE raw            : %8.3f px\n", rms(org_m,   sim_m)))
cat(sprintf("RMSE after translate: %8.3f px\n", rms(org_p,   sim_m)))
cat(sprintf("RMSE after Procrustes: %7.3f px  (scale %.6f)\n", rms(org_aff, sim_m), proc$s))

## --- 3. thin-plate splines on the residual ---------------------------------
t0 <- Sys.time()
fit_x <- Tps(org_aff[, c(1, 2)], sim_m[, 1])
fit_y <- Tps(org_aff[, c(1, 2)], sim_m[, 2])
cat(sprintf("Tps fit time        : %8.1f s\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))

org_tps <- matrix(c(predict(fit_x, org_aff[, c(1, 2)]),
                    predict(fit_y, org_aff[, c(1, 2)])), ncol = 2)

# the .Rhistory "rmse" object is a MEAN Euclidean distance, not an RMSE
rmse_tbl <- tibble(tps_x = org_tps[, 1], tps_y = org_tps[, 2],
                   sim_x = sim_m[, 1],   sim_y = sim_m[, 2]) %>%
  mutate(se = (tps_x - sim_x)^2 + (tps_y - sim_y)^2) %>%
  summarise(mean_dist = mean(sqrt(se)), rmse = sqrt(mean(se)))
print(rmse_tbl)
write_csv(rmse_tbl, file.path(OUT, "TPS_rmse.csv"))

## --- 4. apply to the whole image lattice -----------------------------------
# .Rhistory built the whole 21,026,304-row grid at once; we chunk it by image
# row so peak memory stays bounded, which changes nothing numerically.
t0 <- Sys.time()
CHUNK <- 200L
corr <- vector("list", ceiling(org_height / CHUNK))
k <- 0L
for (v0 in seq(1L, org_height, by = CHUNK)) {
  k <- k + 1L
  v1 <- min(v0 + CHUNK - 1L, org_height)
  cells <- expand.grid(1:org_width, v0:v1)
  cells <- cbind(cells, rep(1, nrow(cells))) %>% as.matrix()
  aff   <- cells %*% p %*% rotation %*% scale
  corr[[k]] <- tibble(
    corr_x = round(as.vector(predict(fit_x, aff[, c(1, 2)])), digits = 0),
    corr_y = round(as.vector(predict(fit_y, aff[, c(1, 2)])), digits = 0)
  )
  if (k %% 5 == 0) cat("  rows", v1, "/", org_height,
                       sprintf("(%.0f s)\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
}
corrected <- bind_rows(corr)
corrected <- corrected %>% mutate(pix_num = (corr_y - 1) * sim_width + corr_x)
el <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
cat(sprintf("full 5616x3744 lattice warp: %.1f s (%d rows)\n", el, nrow(corrected)))
write_csv(corrected, file.path(OUT, "tps_lattice.csv.gz"))
cat("wrote", file.path(OUT, "tps_lattice.csv.gz"), "\n")
