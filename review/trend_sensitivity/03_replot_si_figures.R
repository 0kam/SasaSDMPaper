# Replot Figure S4 at its final 70 mm panel width; no model loading or fitting.
# Run from the repository root: Rscript review/trend_sensitivity/03_replot_si_figures.R
# CSVs supply the summaries, effect and display limits. The saved cell table is
# also needed for the 2D counts: marginal bin summaries cannot reconstruct them.
source(file.path("analysis", "figures", "fig_common.R"))
DIR_REVIEW <- file.path("review", "trend_sensitivity")
read_saved <- function(name) read.csv(file.path(DIR_REVIEW, name))
snow_tbl <- read_saved("trend_by_snow_bin.csv")
dist_tbl <- read_saved("trend_distribution.csv")
effect_tbl <- read_saved("model_B_trend_effect_size.csv")
d <- read_saved("model_B_trend_partial_effect.csv")
trend_df <- readRDS(file.path(DIR_REVIEW, "trend_domain.rds"))

# Text sizes are physical points on a 70 mm canvas, without later reduction.
theme_si <- theme_bw(base_size = 11) +
  theme(axis.text = element_text(size = 9.5, colour = "black"),
        axis.title = element_text(size = 10),
        legend.text = element_text(size = 9.5),
        legend.title = element_text(size = 10),
        legend.position = "top", legend.title.position = "top",
        panel.grid.minor = element_blank(),
        plot.margin = margin(2, 2, 2, 2, "mm"))

ylim <- as.numeric(quantile(trend_df$trend, c(0.001, 0.999)))
heat <- trend_df[trend_df$trend >= ylim[1] & trend_df$trend <= ylim[2], ]
# Parse centres from the saved interval labels; preserve the >=1000-cell rule.
bin_edges <- strsplit(gsub("[^0-9,.-]", "", snow_tbl$snow_mean_bin_doy), ",")
snow_tbl$x <- vapply(bin_edges, function(z) mean(as.numeric(z)), numeric(1))
ov <- snow_tbl[snow_tbl$well_sampled, ]
mean_trend <- dist_tbl$value[dist_tbl$quantity == "trend_mean"]
p1 <- ggplot(heat, aes(snow_mean, trend)) +
  geom_bin2d(bins = c(80, 90)) +
  scale_fill_viridis_c(trans = "log10", name = "Cells", option = "mako",
                       direction = -1, breaks = c(10, 1000, 100000),
                       labels = c("10", "1,000", "100,000"),
                       guide = guide_colourbar(barwidth = unit(38, "mm"),
                                               barheight = unit(2, "mm"))) +
  geom_hline(yintercept = 0, colour = "grey30", linewidth = 0.5) +
  geom_hline(yintercept = mean_trend, colour = "grey30", linewidth = 0.5,
              linetype = "dashed") +
  geom_linerange(data = ov, aes(x = x, ymin = trend_mean - trend_sd,
                                ymax = trend_mean + trend_sd),
                  inherit.aes = FALSE, colour = "#b2182b", linewidth = 0.5) +
  geom_point(data = ov, aes(x = x, y = trend_mean), inherit.aes = FALSE,
              colour = "#b2182b", size = 1.4) +
  scale_x_continuous(breaks = c(120, 160, 200)) +
  labs(x = "Mean snowmelt date, 2011–2021\n(DOY)",
       y = "Pixelwise snowmelt trend\n(days/year)") +
  coord_cartesian(ylim = ylim) + theme_si

qlim <- effect_tbl$value[match(c("trend_q05", "trend_q95"), effect_tbl$quantity)]
inner <- d[d$x >= qlim[1] & d$x <= qlim[2], ]
stopifnot(length(qlim) == 2, all(is.finite(qlim)), nrow(inner) > 0,
          abs(diff(range(inner$fit)) - effect_tbl$value[
            effect_tbl$quantity == "partial_effect_span_inner"]) < 1e-10)
# Keep the saved effect and the original script's ribbon calculation unchanged.
p2 <- ggplot(d, aes(x, fit)) +
  geom_ribbon(aes(ymin = fit - 2 * se, ymax = fit + 2 * se),
                fill = "#9ECAE1", alpha = 0.5) +
  geom_line(linewidth = 0.8, colour = "#08519C") +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey45") +
  labs(x = "Pixelwise snowmelt trend\n(days/year)",
       y = "Partial effect (logit scale)") +
  coord_cartesian(xlim = qlim,
                   ylim = range(c(inner$fit - 2 * inner$se, inner$fit + 2 * inner$se))) +
  theme_si + theme(plot.margin = margin(20, 2, 2, 2, "mm"))

for (name in c("fig_trend_vs_snow_mean", "fig_smooth_trend_zoom")) {
  p <- if (name == "fig_trend_vs_snow_mean") p1 else p2
  out <- file.path("paper", "files", "si", paste0(name, ".png"))
  save_figure(p, out, width_mm = 70, height_mm = 85, dpi = 300)
  message("Wrote ", out, " (70 x 85 mm; 300 dpi)")
}
