# trial_fig05_diff_1m.R -- Trial comparison of Fig. 5 scenario differences.
# Run from the repository root: Rscript analysis/figures/trial_fig05_diff_1m.R
# Only the trial PNG and noise-statistics CSV are written; Fig. 5 is not sourced.

source(file.path("analysis", "figures", "fig_common.R"))
suppressPackageStartupMessages({
  library(tidyterra)
  library(patchwork)
  library(sf)
  library(scico)
})

# ---- Inputs and noise statistics ------------------------------------------
core <- sasa_core_bbox()
pc_s0 <- crop(rast(file.path(DIR_ANALYSIS_OUT, "ca_pcol_2030_s0.tif")), core)
pc_sm071 <- crop(rast(file.path(DIR_ANALYSIS_OUT, "ca_pcol_2030_sm071.tif")), core)
stopifnot(compareGeom(pc_s0, pc_sm071), nlyr(pc_s0) == 1,
          all(res(pc_s0) == 1))

pc_diff_1m <- pc_sm071 - pc_s0
names(pc_diff_1m) <- "d"
pc_diff_10m <- aggregate(pc_diff_1m, fact = 10, fun = "mean", na.rm = TRUE)
diff_rasters <- list(pc_diff_1m, pc_diff_10m)
diff_values <- lapply(diff_rasters, function(r) {
  v <- values(r, mat = FALSE)
  v[is.finite(v)]
})
stopifnot(all(lengths(diff_values) > 1), all(res(pc_diff_10m) == 10))

# Count finite differences, including zeros, in each 10 m mean. Empty blocks
# are excluded, exactly as in the 10 m SD. Use the actual mean count even when
# its departure from 100 is small; partial edge/masked blocks can contain less.
finite_1m <- pc_diff_1m
values(finite_1m) <- as.integer(is.finite(values(pc_diff_1m, mat = FALSE)))
count_10m <- aggregate(finite_1m, fact = 10, fun = "sum", na.rm = TRUE)
counts <- values(count_10m, mat = FALSE)
valid_10m <- is.finite(values(pc_diff_10m, mat = FALSE))
mean_count <- mean(counts[valid_10m])
stopifnot(all((counts > 0) == valid_10m),
          sum(counts) == length(diff_values[[1]]),
          mean_count >= 1, mean_count <= 100)
message(sprintf("Mean finite 1 m cells per finite 10 m cell: %.6f (nominal 100)",
                mean_count))

# For p = finite s0 probabilities in the core, a 200-replicate binomial mean
# has variance p*(1-p)/200. The difference of two independent such means has
# expected SD sqrt(2 * mean(p*(1-p)) / 200). At 10 m, divide by sqrt(mean_count),
# assuming independent cell noise and using the requested mean-count approximation.
p <- values(pc_s0, mat = FALSE)
p <- p[is.finite(p)]
stopifnot(length(p) > 1, all(p >= 0 & p <= 1))
noise_1m <- sqrt(2 * mean(p * (1 - p)) / 200)
noise_stats <- data.frame(
  resolution_m = c(1, 10),
  # Sample SD across finite difference cells: sqrt(sum((d-mean(d))^2)/(n-1)).
  sd_cell_level = vapply(diff_values, stats::sd, numeric(1)),
  expected_sd_pure_replicate_noise = noise_1m / sqrt(c(1, mean_count))
)
# Requested observed-to-noise ratio; no noise variance is subtracted from SD.
noise_stats$ratio_signal <- noise_stats$sd_cell_level /
  noise_stats$expected_sd_pure_replicate_noise
stopifnot(all(is.finite(as.matrix(noise_stats))), noise_1m > 0)
print(noise_stats, row.names = FALSE, digits = 7)

# Compare with the approximate values documented in fig05_projection.R.
observed <- c(sd_1m = noise_stats$sd_cell_level[1],
              sd_10m = noise_stats$sd_cell_level[2],
              noise_10m = noise_stats$expected_sd_pure_replicate_noise[2])
reference <- c(sd_1m = 0.0092, sd_10m = 0.0047, noise_10m = 0.0009)
for (key in names(reference)) {
  if (observed[[key]] / reference[[key]] > 1.5 ||
      observed[[key]] / reference[[key]] < 1 / 1.5) {
    message(sprintf("WARNING: %s = %.7f differs by more than a factor of 1.5 from %.7f",
                    key, observed[[key]], reference[[key]]))
  }
}

# Match Fig. 5: the 99.5th percentile of absolute finite core differences,
# rounded to the nearest 0.005, supplies symmetric limits separately per column.
diff_limits <- vapply(diff_values, function(v) {
  round(stats::quantile(abs(v), 0.995, names = FALSE) / 0.005) * 0.005
}, numeric(1))
stopifnot(all(is.finite(diff_limits)), all(diff_limits > 0))
message(sprintf("Symmetric colour limits: 1 m +/- %.3f; 10 m mean +/- %.3f",
                diff_limits[1], diff_limits[2]))

# ---- Map styling copied from Fig. 5, with shared fig_common.R constants ----
outline21 <- sf::st_as_sf(crop(sasa_outline_2021(), core))
outline21 <- sf::st_cast(outline21, "POLYGON", warn = FALSE)
outline21 <- outline21[as.numeric(sf::st_area(outline21)) > 5, ]
outline21_zoom <- sf::st_crop(outline21, sf::st_bbox(
  c(xmin = ZOOM_WINDOW[["xmin"]], xmax = ZOOM_WINDOW[["xmax"]],
    ymin = ZOOM_WINDOW[["ymin"]], ymax = ZOOM_WINDOW[["ymax"]]),
  crs = sf::st_crs(6690)))
# Preserve the raw boundary in the zoom; simplify only the full-core outline.
outline21 <- sf::st_simplify(outline21, dTolerance = 2, preserveTopology = TRUE)
outline21 <- outline21[!sf::st_is_empty(outline21), ]
stopifnot(nrow(outline21) > 0, nrow(outline21_zoom) > 0)
dem <- crop(rast(file.path(DIR_ANALYSIS_OUT, "predictors.tif"))[["elevation"]], core)
contours <- sf::st_as_sf(as.contour(
  dem, levels = seq(floor(minmax(dem)[1] / CONTOUR_INTERVAL_M) * CONTOUR_INTERVAL_M,
                    ceiling(minmax(dem)[2] / CONTOUR_INTERVAL_M) * CONTOUR_INTERVAL_M,
                    by = CONTOUR_INTERVAL_M)))

xr <- c(core$xmin, core$xmax); yr <- c(core$ymin, core$ymax)
bar <- data.frame(
  xmin = xr[2] - 0.06 * diff(xr) - 200, xmax = xr[2] - 0.06 * diff(xr),
  ymin = yr[1] + 0.055 * diff(yr), ymax = yr[1] + 0.069 * diff(yr)
)
map_theme <- theme(legend.position = "top",
                   legend.title = element_text(vjust = 1),
                   axis.text = element_text(size = 9.5),
                   plot.margin = margin(2, 8, 2, 2, "mm"))
signed_sqrt <- scales::trans_new(
  "signed_sqrt", transform = function(x) sign(x) * sqrt(abs(x)),
  inverse = function(x) sign(x) * x^2
)
diff_scale <- function(limit, resolution_label) {
  scale_fill_gradientn(
    colours = scico::scico(255, palette = "vik"), na.value = "transparent",
    limits = c(-limit, limit), oob = scales::squish, transform = signed_sqrt,
    breaks = c(-limit, 0, limit),
    labels = function(x) ifelse(abs(abs(x) - limit) < 1e-9,
      sprintf("%s%.3f", ifelse(x < 0, "≤ −", "≥ +"), abs(x)),
      sprintf("%s%.3f", ifelse(x < 0, "−", ifelse(x > 0, "+", "")), abs(x))),
    name = bquote(atop(Delta*"P (s = −0.71 minus s = 0),", .(resolution_label))),
    guide = guide_colourbar(barwidth = unit(46, "mm"), barheight = unit(2.6, "mm"),
                            title.position = "top", ticks.colour = "grey20")
  )
}

full_map <- function(r, scale, tag, zoom_tag) {
  ggplot() +
    geom_sf(data = contours, colour = CONTOUR_COLOUR, linewidth = CONTOUR_LINEWIDTH) +
    geom_spatraster(data = r, maxcell = Inf) + scale +
    geom_sf(data = outline21, fill = NA, colour = SASA_OUTLINE_COLOUR,
            linewidth = SASA_OUTLINE_LINEWIDTH) +
    annotate("rect", xmin = bar$xmin, xmax = bar$xmax, ymin = bar$ymin, ymax = bar$ymax,
             fill = "black", colour = "black", linewidth = 0.2) +
    annotate_text_pt(x = mean(c(bar$xmin, bar$xmax)), y = bar$ymax + 0.03 * diff(yr),
                     label = "200 m", size_pt = 10, vjust = 0) +
    zoom_box_layers(label = zoom_tag) +
    coord_sf(xlim = xr, ylim = yr, expand = FALSE, datum = sf::st_crs(6690)) +
    scale_x_continuous(breaks = c(733000, 733500)) +
    scale_y_continuous(breaks = seq(4050800, 4051800, by = 400)) +
    labs(x = LAB[["easting"]], y = LAB[["northing"]], tag = tag) + map_theme
}
zoom_map <- function(r, scale, tag) {
  z <- zoom_ext()
  ggplot() +
    # Include every cell intersecting the shared window, then clip the display.
    # The 10 m grid does not coincide with the 150 m window's boundaries.
    geom_spatraster(data = crop(r, z, snap = "out"), maxcell = Inf) + scale +
    geom_sf(data = outline21_zoom, fill = NA, colour = SASA_OUTLINE_COLOUR,
            linewidth = 0.45) +
    zoom_scalebar_layers() +
    coord_sf(xlim = c(z$xmin, z$xmax), ylim = c(z$ymin, z$ymax),
             expand = FALSE, datum = sf::st_crs(6690)) +
    scale_x_continuous(breaks = ZOOM_X_BREAKS) +
    scale_y_continuous(breaks = ZOOM_Y_BREAKS) +
    labs(x = LAB[["easting"]], y = LAB[["northing"]], tag = tag) + map_theme
}

# Each column collects its own legend and uses the same scale in both rows.
scale_1m <- diff_scale(diff_limits[1], "1 m (unaggregated)")
scale_10m <- diff_scale(diff_limits[2], "10 m mean")
p_a <- full_map(pc_diff_1m, scale_1m, "a", "c")
p_b <- full_map(pc_diff_10m, scale_10m, "b", "d")
p_c <- zoom_map(pc_diff_1m, scale_1m, "c")
p_d <- zoom_map(pc_diff_10m, scale_10m, "d")
col_left <- (p_a / p_c) + plot_layout(guides = "collect", heights = c(1, 1)) &
  theme(legend.position = "top")
col_right <- (p_b / p_d) + plot_layout(guides = "collect", heights = c(1, 1)) &
  theme(legend.position = "top")
fig <- (col_left | col_right) & theme(plot.tag = element_text(size = 13, face = "bold"))

out_dir <- file.path(DIR_ANALYSIS_OUT, "trial")
save_figure(fig, file.path(out_dir, "fig05_diff_1m_trial.png"),
            width_mm = 190, height_mm = 200, dpi = 300)
write.csv(noise_stats, file.path(out_dir, "fig05_diff_noise_stats.csv"), row.names = FALSE)
message("Fig. 5 difference trial and noise statistics written to ", out_dir)
