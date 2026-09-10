# ---------------------------------------------------------------------------
# Reconstruction of the Procrustes + thin-plate-spline IMAGE ALIGNMENT that is
# preserved only as fragments in data_from_server/ortho/.Rhistory (lines ~81-176).
#
# WHAT THIS IS *NOT*
#   It is not the georectification.  It never touches gcp.csv (u,v,x,y,z), the camera
#   parameters, or the DEM, and it cannot produce ortho/data/georectified.csv.  The
#   .Rhistory block runs in  /media/okamoto/HDD3TB/tateyama/mrd_snowmelt/step2_ortho/ ,
#   a different project (the snowmelt paper), and it READS a table it calls
#   "georectificated.csv" (columns x, y, indexed by a running pixel number) as an
#   already-existing input.
#
# WHAT IT IS
#   An image-to-image alignment: it warps one camera frame ("org") onto the reference
#   frame ("sim") in which the georectified lookup table is defined, by
#     1. a parallel translation that equalises the GCP centroids,
#     2. a Procrustes rotation + isotropic scale (shapes::procOPA),
#     3. a thin-plate spline residual warp (fields::Tps), fitted separately for u and v,
#   then evaluates the composed transform on the full 5616 x 3744 pixel grid and joins
#   each warped pixel to the reference table by pixel number.
#
# INPUTS (both required; NEITHER is present in the recovered server dump)
#   gcp.csv               columns org_x, org_y, sim_x, sim_y  -- image-to-image GCPs.
#                         NOTE the recovered ortho/data/gcp.csv is a DIFFERENT file
#                         (u, v, x, y, z: image-to-map GCPs for alproj).
#   georectificated.csv   columns x, y, one row per reference-frame pixel in
#                         row-major order.
#
# Run the built-in self-test instead, which needs no inputs:
#   Rscript 00_align_tps.R --selftest
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(shapes)   # procOPA
  library(fields)   # Tps
})

ORG_WIDTH <- 5616
ORG_HEIGHT <- 3744
SIM_WIDTH <- 5616
SIM_HEIGHT <- 3744

#' Fit the translation + Procrustes + TPS transform from matched GCPs.
#'
#' @param control_points data frame with org_x, org_y, sim_x, sim_y
#' @return list(p, rotation, scale, fit_x, fit_y, rmse)
fit_alignment <- function(control_points) {
  org_m <- matrix(c(control_points$org_x, control_points$org_y), ncol = 2)
  org_m <- cbind(org_m, rep(1, nrow(org_m)))          # homogeneous, for affine
  sim_m <- matrix(c(control_points$sim_x, control_points$sim_y), ncol = 2)
  sim_m <- cbind(sim_m, rep(1, nrow(sim_m)))

  xdiff <- mean(sim_m[, 1] - org_m[, 1])
  ydiff <- mean(sim_m[, 2] - org_m[, 2])
  p <- matrix(c(1, 0, xdiff, 0, 1, ydiff, 0, 0, 1), 3, 3)
  org_p <- org_m %*% p                                 # parallel translated

  proc <- procOPA(sim_m, org_p)
  rotation <- proc$R
  scale <- rbind(diag(c(proc$s, proc$s)), c(0, 0)) %>% cbind(c(0, 0, 1))
  org_aff <- org_p %*% rotation %*% scale

  fit_x <- Tps(org_aff[, c(1, 2)], sim_m[, 1])
  fit_y <- Tps(org_aff[, c(1, 2)], sim_m[, 2])

  org_tps <- matrix(c(predict(fit_x, org_aff[, c(1, 2)]),
                      predict(fit_y, org_aff[, c(1, 2)])), ncol = 2)
  # NB the .Rhistory calls this "rmse" but computes the MEAN residual distance,
  # not the root mean square.  Both are returned here.
  d <- sqrt((org_tps[, 1] - sim_m[, 1])^2 + (org_tps[, 2] - sim_m[, 2])^2)
  list(p = p, rotation = rotation, scale = scale,
       fit_x = fit_x, fit_y = fit_y,
       mean_residual_px = mean(d), rmse_px = sqrt(mean(d^2)), n_gcp = nrow(org_m))
}

#' Apply a fitted alignment to arbitrary points (n x 2 matrix of org pixel coords).
apply_alignment <- function(fit, pts) {
  m <- cbind(pts, rep(1, nrow(pts)))
  aff <- m %*% fit$p %*% fit$rotation %*% fit$scale
  cbind(as.vector(predict(fit$fit_x, aff[, c(1, 2)])),
        as.vector(predict(fit$fit_y, aff[, c(1, 2)])))
}

#' Evaluate the transform on every pixel of the source image and attach the
#' reference-frame map coordinates.
warp_full_grid <- function(fit, points_sim,
                           org_width = ORG_WIDTH, org_height = ORG_HEIGHT,
                           sim_width = SIM_WIDTH) {
  org_cells <- expand.grid(1:org_width, 1:org_height)
  org_cells <- cbind(org_cells, rep(1, nrow(org_cells))) %>% as.matrix()
  org_cells_affine <- org_cells %*% fit$p %*% fit$rotation %*% fit$scale
  tibble(
    corr_x = round(as.vector(predict(fit$fit_x, org_cells_affine[, c(1, 2)])), digits = 0),
    corr_y = round(as.vector(predict(fit$fit_y, org_cells_affine[, c(1, 2)])), digits = 0)
  ) %>%
    mutate(pix_num = (corr_y - 1) * sim_width + corr_x) %>%
    left_join(points_sim, by = "pix_num") %>%
    dplyr::select(x, y)
}

# --------------------------------------------------------------------- driver
main <- function(gcp_path = "gcp.csv",
                 georect_path = "georectificated.csv",
                 aligned_dir = "snowmelt_aligned/",
                 out_dir = "snowmelt_ortho/") {
  control_points <- read_csv(gcp_path, show_col_types = FALSE)
  points_sim <- read_csv(georect_path,
                         col_types = cols(x = col_double(), y = col_double())) %>%
    rename(pix_num = ...1)
  fit <- fit_alignment(control_points)
  message(sprintf("GCPs %d | mean residual %.4f px | RMSE %.4f px",
                  fit$n_gcp, fit$mean_residual_px, fit$rmse_px))
  tibble(rmse = fit$mean_residual_px) %>% write_csv("TPS_rmse.csv")

  corrected <- warp_full_grid(fit, points_sim)

  files <- list.files(aligned_dir, full.names = TRUE)
  for (f in files) {
    snow <- imager::load.image(f)                  # requires the 'imager' package
    melt <- snow[, , 1, 1] %>% as.vector()
    melt2 <- melt * 255 %>% round(digits = 0)      # imager 0..1 -> DOY 1..255
    result <- mutate(corrected, snow_melt = as.integer(melt2)) %>%
      filter(x != 0, snow_melt != 0)
    out <- str_replace(f, aligned_dir, out_dir) %>% str_replace("png", "csv")
    write_csv(result, out)
    gc()
  }
}

# ------------------------------------------------------------------ self-test
selftest <- function(n_gcp = 480, seed = 1) {
  set.seed(seed)
  # a known ground-truth warp: rotate 1.3 deg, scale 1.004, translate, plus a
  # smooth quadratic bow of ~15 px amplitude that only the TPS stage can absorb
  th <- 1.3 * pi / 180; s <- 1.004; tx <- 37; ty <- -21
  truth <- function(P) {
    x <- P[, 1]; y <- P[, 2]
    xr <- s * (cos(th) * x - sin(th) * y) + tx
    yr <- s * (sin(th) * x + cos(th) * y) + ty
    xn <- (x - ORG_WIDTH / 2) / (ORG_WIDTH / 2)
    yn <- (y - ORG_HEIGHT / 2) / (ORG_HEIGHT / 2)
    cbind(xr + 15 * (xn^2 - yn^2), yr + 15 * (2 * xn * yn))
  }
  org <- cbind(runif(n_gcp, 1, ORG_WIDTH), runif(n_gcp, 1, ORG_HEIGHT))
  sim <- truth(org)
  cp <- tibble(org_x = org[, 1], org_y = org[, 2], sim_x = sim[, 1], sim_y = sim[, 2])

  t0 <- proc.time()[["elapsed"]]
  fit <- fit_alignment(cp)
  t_fit <- proc.time()[["elapsed"]] - t0
  cat(sprintf("fit on %d GCPs: %.2f s | mean residual %.4f px | RMSE %.4f px\n",
              n_gcp, t_fit, fit$mean_residual_px, fit$rmse_px))

  # accuracy on 5000 held-out points
  test <- cbind(runif(5000, 1, ORG_WIDTH), runif(5000, 1, ORG_HEIGHT))
  pred <- apply_alignment(fit, test)
  tr <- truth(test)
  d <- sqrt((pred[, 1] - tr[, 1])^2 + (pred[, 2] - tr[, 2])^2)
  cat(sprintf("held-out: mean %.4f px, RMSE %.4f px, max %.4f px\n",
              mean(d), sqrt(mean(d^2)), max(d)))

  # timing of the full-grid evaluation, the expensive step
  t0 <- proc.time()[["elapsed"]]
  grid <- expand.grid(1:ORG_WIDTH, 1:ORG_HEIGHT) %>% as.matrix()
  chunk <- 2e6
  n <- nrow(grid)
  for (i in seq(1, n, by = chunk)) {
    j <- min(i + chunk - 1, n)
    m <- cbind(grid[i:j, ], rep(1, j - i + 1)) %*% fit$p %*% fit$rotation %*% fit$scale
    invisible(predict(fit$fit_x, m[, c(1, 2)]))
    invisible(predict(fit$fit_y, m[, c(1, 2)]))
  }
  cat(sprintf("full %d x %d grid TPS evaluation: %.1f s\n",
              ORG_WIDTH, ORG_HEIGHT, proc.time()[["elapsed"]] - t0))
}

args <- commandArgs(trailingOnly = TRUE)
if (length(args) && args[1] == "--selftest") {
  selftest()
}
