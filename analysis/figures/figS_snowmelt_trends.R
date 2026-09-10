# Replot S1 and the two 70 mm S3 panels from saved outputs only.
# Run from the repository root: Rscript analysis/figures/figS_snowmelt_trends.R
source(file.path("analysis", "00_config.R"))
source(file.path("analysis", "figures", "fig_common.R"))
annual <- read.csv(PATH_SNOW_ANNUAL)
stats_tbl <- read.csv(PATH_SNOW_STATS)
stat <- setNames(stats_tbl$value, stats_tbl$quantity)
# Full WP6 runs can supply their in-memory slope raster; standalone plotting
# reads the cache and never requires the raw snow stack.
if (!exists("slope_rast", inherits = FALSE)) {
  slope_rast <- rast(PATH_SNOW_OLS)[["slope_d_per_yr"]]
}
b <- values(slope_rast, mat = FALSE)
b <- b[is.finite(b)]
n_pix <- length(b)
slope_mean <- stat[["pixel_slope_mean"]]
stopifnot(n_pix == stat[["n_pixels_with_trend"]],
          abs(mean(b) - slope_mean) < 1e-6)
save_snow <- function(p, filename, width_mm, height_mm) {
  out <- file.path(DIR_OUT, filename)
  save_figure(p, out, width_mm = width_mm, height_mm = height_mm, dpi = 300)
  stopifnot(file.copy(out, file.path("paper", "files", "si", filename), overwrite = TRUE))
  message(sprintf("Wrote %s and paper/files/si/%s (%g x %g mm; 300 dpi)",
                  out, filename, width_mm, height_mm))
}

# English labels, no in-plot titles, text >= 9.5 pt at final size, 300 dpi.
theme_snow <- theme_bw(base_size = 11) +
  theme(axis.text  = element_text(size = 9.5, colour = "black"),
        axis.title = element_text(size = 10),
        legend.text = element_text(size = 9.5),
        legend.title = element_text(size = 10),
        plot.margin = margin(2, 2, 2, 2, "mm"),
        panel.grid.minor = element_blank(),
        plot.title = element_blank())

# (a) annual mean DOY with OLS fit and 95% CI band
# Reconstruct the saved landscape OLS line and its confidence band without refitting.
pred_x <- seq(min(annual$year), max(annual$year), length.out = 100)
xbar <- mean(annual$year)
sxx <- sum((annual$year - xbar)^2)
fit <- mean(annual$mean_doy) + stat[["landscape_slope"]] * (pred_x - xbar)
se_fit <- stat[["landscape_slope_se"]] * sqrt(sxx / nrow(annual) + (pred_x - xbar)^2)
ci <- qt(0.975, df = nrow(annual) - 2) * se_fit
pred <- data.frame(year = pred_x, fit = fit, lwr = fit - ci, upr = fit + ci)

p_a <- ggplot(annual, aes(x = year, y = mean_doy)) +
  geom_ribbon(data = pred, aes(x = year, ymin = lwr, ymax = upr),
              inherit.aes = FALSE, fill = "grey70", alpha = 0.45) +
  geom_line(data = pred, aes(x = year, y = fit), inherit.aes = FALSE,
            linewidth = 0.7) +
  geom_point(size = 2.4) +
  scale_x_continuous(breaks = annual$year) +
  labs(x = "Year", y = "Mean snowmelt date (DOY)") +
  theme_snow +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
save_snow(p_a, "fig_snowmelt_annual_mean.png", 140, 90)

# (b) histogram of per-pixel slopes with a zero reference line.
# The x range is trimmed to the 0.1-99.9 percentiles: a few hundred pixels have
# |slope| > 5 d/yr (steep, poorly constrained snow-patch edges) and would
# otherwise stretch the axis to +/- 15 and flatten the distribution to a spike.
# The number of pixels outside the plotted range is reported on stdout.
hist_lim <- as.numeric(stats::quantile(b, c(0.001, 0.999)))
n_outside <- sum(b < hist_lim[1] | b > hist_lim[2])
msg(sprintf("slope histogram trimmed to [%.2f, %.2f]; %d pixels (%.3f%%) outside",
            hist_lim[1], hist_lim[2], n_outside, 100 * n_outside / n_pix))
p_b <- ggplot(data.frame(slope = b), aes(x = slope)) +
  geom_histogram(binwidth = diff(hist_lim) / 80, fill = "grey45", colour = NA) +
  geom_vline(xintercept = 0, linewidth = 0.6, colour = "black") +
  geom_vline(xintercept = slope_mean, linewidth = 0.6, linetype = "dashed",
             colour = "black") +
  coord_cartesian(xlim = hist_lim) +
  labs(x = "Pixelwise snowmelt trend\n(days/year)",
       y = "Number of pixels") +
  scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
  theme_snow +
  theme(aspect.ratio = 1, plot.margin = margin(16, 2, 2, 2, "mm"))
save_snow(p_b, "fig_snowmelt_slope_hist.png", 70, 85)

# (c) map of the slope raster (symmetric diverging scale about 0)
lim <- as.numeric(stats::quantile(abs(b), 0.99, na.rm = TRUE))
# na.rm = FALSE keeps the full rectangular grid so that geom_raster() sees an
# evenly spaced lattice (NA cells simply render transparent); dropping NAs first
# would leave gaps in x and make ggplot shift the cells.
map_df <- as.data.frame(slope_rast, xy = TRUE, na.rm = FALSE)
names(map_df)[3] <- "slope"
p_c <- ggplot(map_df, aes(x = x, y = y, fill = slope)) +
  geom_raster() +
  coord_equal(expand = FALSE) +
  scale_x_continuous(breaks = c(733000, 734000)) +
  scale_y_continuous(breaks = c(4050500, 4051500)) +
  scale_fill_gradient2(low = "#2166ac", mid = "grey95", high = "#b2182b",
                       midpoint = 0, limits = c(-lim, lim), oob = scales::squish,
                       na.value = "transparent",
                       name = "days/year") +
  labs(x = "Easting (m)", y = "Northing (m)") +
  theme_snow +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        legend.position = "top", legend.title.position = "top") +
  guides(fill = guide_colourbar(barwidth = unit(35, "mm"),
                                barheight = unit(2, "mm")))
save_snow(p_c, "fig_snowmelt_slope_map.png", 70, 85)

msg("wrote 3 figures to ", DIR_OUT)

