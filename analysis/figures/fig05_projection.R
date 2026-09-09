# fig05_projection.R -- Fig. 5: 2030 projection of new Sasa colonization.
#
# Message: new colonization stays close to the current front, and the effect of
# earlier snowmelt is small and spatially uneven.
#   (a) P(colonized by 2030) under s = 0
#   (b) difference map, s = -0.71 minus s = 0 (diverging, centred on zero)
#   (c) (a) enlarged over the shared 150 m zoom window (fig_common.R ZOOM_WINDOW,
#       the same box Fig. 2(c) and Fig. 4(c,d) enlarge), at the native 1 m grid
#   (d) (b) enlarged over the same window; still the 10 m mean, because the 1 m
#       scenario difference is Monte-Carlo noise (see DIFF_AGG_M below)
#   (e) cumulative expected new colonized area, three snowmelt scenarios
#
# Run from the repository root:
#   Rscript analysis/figures/fig05_projection.R
# Design reference: review/figure_redesign_plan_ja.md (Fig. 5).

set.seed(1)
source(file.path("analysis", "figures", "fig_common.R"))

suppressPackageStartupMessages({
  library(tidyterra)
  library(patchwork)
  library(sf)
  library(scico)
})

# ---- Inputs ---------------------------------------------------------------
core <- sasa_core_bbox()

pc_s0    <- crop(rast(file.path(DIR_ANALYSIS_OUT, "ca_pcol_2030_s0.tif")), core)
pc_sm071 <- crop(rast(file.path(DIR_ANALYSIS_OUT, "ca_pcol_2030_sm071.tif")), core)
names(pc_s0) <- names(pc_sm071) <- "p"

pc_diff_1m <- pc_sm071 - pc_s0
names(pc_diff_1m) <- "d"

# At 1 m the scenario difference is dominated by Monte-Carlo noise across the 200
# replicates (cell-level sd 0.0092, salt-and-pepper), so the map is averaged to
# 10 m. The 10 m sd (0.0047) is five times what pure replicate noise would leave
# (0.0009), i.e. the aggregation removes noise and keeps the spatial signal.
DIFF_AGG_M <- 10
pc_diff <- aggregate(pc_diff_1m, fact = DIFF_AGG_M, fun = "mean", na.rm = TRUE)
names(pc_diff) <- "d"

pc_s0_zoom <- crop(pc_s0, zoom_ext())
pc_diff_zoom <- crop(pc_diff, zoom_ext())
message(sprintf("zoom window P(col by 2030, s = 0): min %.4f, max %.4f",
                min(values(pc_s0_zoom), na.rm = TRUE),
                max(values(pc_s0_zoom), na.rm = TRUE)))

# Diverging limits: symmetric, set from the 99.5th percentile of |difference|
# so that a handful of extreme cells does not flatten the map.
diff_vals <- values(pc_diff)
diff_vals <- diff_vals[is.finite(diff_vals)]
diff_lim <- unname(round(stats::quantile(abs(diff_vals), 0.995, names = FALSE) / 0.005) * 0.005)
message(sprintf("1 m difference range in core: [%.3f, %.3f]",
                min(values(pc_diff_1m), na.rm = TRUE), max(values(pc_diff_1m), na.rm = TRUE)))
message(sprintf("%d m mean difference range: [%.4f, %.4f]; symmetric limit +/- %.3f",
                DIFF_AGG_M, min(diff_vals), max(diff_vals), diff_lim))

# 2021 Sasa outline and 100 m contours, both clipped to the core extent.
# The raw 1 m patch boundaries are so convoluted that a 0.3 mm line reads as a
# black fill at print size (the problem Reviewer 3 flagged), so the outline keeps
# only patches > 5 m2 (the manuscript's patch rule) and is simplified to 2 m.
outline21 <- sf::st_as_sf(crop(sasa_outline_2021(), core))
outline21 <- sf::st_cast(outline21, "POLYGON", warn = FALSE)
outline21 <- outline21[as.numeric(sf::st_area(outline21)) > 5, ]
# The zoom panels keep the raw 1 m boundary: at 150 m across it is the reference
# the one-cell colonization halo is read against, and simplifying it there would
# erase exactly the structure the panel exists to show.
outline21_zoom <- sf::st_crop(outline21, sf::st_bbox(
  c(xmin = ZOOM_WINDOW[["xmin"]], xmax = ZOOM_WINDOW[["xmax"]],
    ymin = ZOOM_WINDOW[["ymin"]], ymax = ZOOM_WINDOW[["ymax"]]),
  crs = sf::st_crs(6690)))
outline21 <- sf::st_simplify(outline21, dTolerance = 2, preserveTopology = TRUE)
outline21 <- outline21[!sf::st_is_empty(outline21), ]
stopifnot(nrow(outline21) > 0, nrow(outline21_zoom) > 0)
dem <- crop(rast(file.path(DIR_ANALYSIS_OUT, "predictors.tif"))[["elevation"]], core)
contours <- sf::st_as_sf(as.contour(
  dem, levels = seq(floor(minmax(dem)[1] / CONTOUR_INTERVAL_M) * CONTOUR_INTERVAL_M,
                    ceiling(minmax(dem)[2] / CONTOUR_INTERVAL_M) * CONTOUR_INTERVAL_M,
                    by = CONTOUR_INTERVAL_M)))

traj <- read.csv(file.path(DIR_ANALYSIS_OUT, "ca_trajectory.csv"))

# ---- Shared map furniture -------------------------------------------------
xr <- c(core$xmin, core$xmax); yr <- c(core$ymin, core$ymax)

# Axis breaks are identical to Fig. 4 (analysis/figures/fig04_potential_vs_
# establishment.R) so that the two figures' maps are read the same way; the
# labels stay horizontal.
X_BREAKS <- seq(732800, 734000, by = 400)
Y_BREAKS <- seq(4050800, 4051800, by = 400)

# Scale bar: 200 m, lower right, drawn in map units so it is exact.
BAR_M <- 200
bar <- data.frame(
  xmin = xr[2] - 0.06 * diff(xr) - BAR_M, xmax = xr[2] - 0.06 * diff(xr),
  ymin = yr[1] + 0.055 * diff(yr), ymax = yr[1] + 0.055 * diff(yr) + 0.014 * diff(yr)
)

map_furniture <- function(p) {
  p +
    geom_sf(data = outline21, fill = NA, colour = SASA_OUTLINE_COLOUR,
            linewidth = SASA_OUTLINE_LINEWIDTH) +
    annotate("rect", xmin = bar$xmin, xmax = bar$xmax, ymin = bar$ymin, ymax = bar$ymax,
             fill = "black", colour = "black", linewidth = 0.2) +
    annotate_text_pt(x = mean(c(bar$xmin, bar$xmax)), y = bar$ymax + 0.03 * diff(yr),
                     label = paste0(BAR_M, " m"), size_pt = 10, vjust = 0) +
    coord_sf(xlim = xr, ylim = yr, expand = FALSE, datum = sf::st_crs(6690)) +
    scale_x_continuous(breaks = X_BREAKS) +
    scale_y_continuous(breaks = Y_BREAKS) +
    labs(x = LAB[["easting"]], y = LAB[["northing"]]) +
    theme(legend.position = "top",
          legend.title = element_text(vjust = 1),
          axis.text = element_text(size = 11),
          # Right pad: at 11 pt the 734000 tick label overruns the panel.
          plot.margin = margin(2, 8, 2, 2, "mm"))
}

# Zoom-panel frame: same furniture, own breaks. The colour scale is the object
# the matching core-extent panel uses, so patchwork's guides = "collect" keeps
# one bar per column.
zoom_furniture <- function(p, outline_colour = SASA_OUTLINE_COLOUR,
                           bar_colour = "black") {
  z <- zoom_ext()
  p +
    geom_sf(data = outline21_zoom, fill = NA, colour = outline_colour,
            linewidth = 0.45) +
    zoom_scalebar_layers(colour = bar_colour) +
    coord_sf(xlim = c(z$xmin, z$xmax), ylim = c(z$ymin, z$ymax),
             expand = FALSE, datum = sf::st_crs(6690)) +
    scale_x_continuous(breaks = ZOOM_X_BREAKS) +
    scale_y_continuous(breaks = ZOOM_Y_BREAKS) +
    labs(x = LAB[["easting"]], y = LAB[["northing"]]) +
    theme(legend.position = "top",
          legend.title = element_text(vjust = 1),
          axis.text = element_text(size = 11),
          plot.margin = margin(2, 8, 2, 2, "mm"))
}

base_map <- function() {
  ggplot() +
    geom_sf(data = contours, colour = CONTOUR_COLOUR, linewidth = CONTOUR_LINEWIDTH)
}

# Bar widened from 38 mm so that the 10.8 pt tick labels do not touch.
cbar <- function(...) guide_colourbar(barwidth = unit(52, "mm"), barheight = unit(2.6, "mm"),
                                      title.position = "top", ticks.colour = "grey20", ...)

# ---- (a) P(colonized by 2030), s = 0 --------------------------------------
# Colonization probability must look the same in every figure: this is exactly
# the scale of Fig. 4(b) (rocket reversed, fourth-root stretch, published
# breaks). Keeping it also stops the pale-orange low end from being confused
# with the warm (positive) side of the diverging ramp in panel (b).
# PROB_LIM / PROB_BREAKS / PROB_LABELS come from fig_common.R so that the three
# figures cannot drift apart.
root4 <- if (utils::packageVersion("scales") >= "1.3.0") {
  scales::new_transform("root4", function(x) x^0.25, function(x) x^4)
} else {
  scales::trans_new("root4", function(x) x^0.25, function(x) x^4)
}

# One scale object, reused by (a) and its zoom (c) so that guides = "collect"
# recognises the two legends as identical and keeps one.
prob_scale <- scale_fill_viridis_c(
  option = "rocket", direction = -1, name = LAB[["p_col_2030"]],
  na.value = "transparent", limits = PROB_LIM, transform = root4,
  breaks = PROB_BREAKS, labels = PROB_LABELS,
  guide = cbar()
)

p_a <- base_map() +
  geom_spatraster(data = pc_s0, maxcell = Inf) +
  prob_scale
p_a <- map_furniture(p_a) + zoom_box_layers(label = "c")

# ---- (b) scenario difference map ------------------------------------------
# The differences are small and heavily concentrated near zero, so the diverging
# vik ramp is stretched symmetrically (signed square root) about zero; without
# the stretch the panel is a flat neutral field.
signed_sqrt <- scales::trans_new(
  "signed_sqrt",
  transform = function(x) sign(x) * sqrt(abs(x)),
  inverse   = function(x) sign(x) * x^2
)
# Three labels only: the stretch makes intermediate ticks crowd the bar ends.
diff_breaks <- c(-diff_lim, 0, diff_lim)

diff_scale <- scale_fill_gradientn(
    colours = scico::scico(255, palette = "vik"), na.value = "transparent",
    limits = c(-diff_lim, diff_lim), oob = scales::squish,
    transform = signed_sqrt,
    breaks = diff_breaks,
    labels = function(x) ifelse(abs(abs(x) - diff_lim) < 1e-9,
                                sprintf("%s%.3f", ifelse(x < 0, "≤ −", "≥ +"), abs(x)),
                                sprintf("%s%.3f", ifelse(x < 0, "−", ifelse(x > 0, "+", "")), abs(x))),
  name = expression(Delta*"P (s = −0.71 minus s = 0), 10 m mean"),
  guide = cbar()
)

p_b <- base_map() +
  geom_spatraster(data = pc_diff, maxcell = Inf) +
  diff_scale
p_b <- map_furniture(p_b) + zoom_box_layers(label = "d")

# ---- (c)(d) zoom panels ---------------------------------------------------
# At 150 m across a 1 m cell is about 0.4 mm wide in print, so panel (c) resolves
# the one-cell band of raised colonization probability that hugs the 2021 front
# and that the core-extent panel above renders as a single dark pixel line.
p_c <- ggplot() +
  geom_spatraster(data = pc_s0_zoom, maxcell = Inf) +
  prob_scale
p_c <- zoom_furniture(p_c)

# (d) stays on the 10 m mean: at 1 m the scenario difference is replicate noise
# (see DIFF_AGG_M above), so an enlarged 1 m difference map would show noise, not
# signal. Fifteen 10 m cells across the window is what the data actually support.
p_d <- ggplot() +
  geom_spatraster(data = pc_diff_zoom, maxcell = Inf) +
  diff_scale
p_d <- zoom_furniture(p_d)

# ---- (e) cumulative expected new colonized area ---------------------------
SCEN_COL <- c(s0    = unname(okabeito_colours["bluishgreen"]),
              sm071 = unname(okabeito_colours["vermillion"]),
              sm224 = unname(okabeito_colours["blue"]))
SCEN_TEXT <- c(s0    = "s == 0",
               sm071 = "s == -0.71~d~yr^-1",
               sm224 = "s == -2.24~d~yr^-1")

traj$scenario <- factor(traj$scenario, levels = names(SCEN_COL))
ends <- traj[traj$year == max(traj$year), ]
ends$label <- SCEN_TEXT[as.character(ends$scenario)]
# Nudge the two upper labels apart; their 2030 values differ by only ~200 m2.
ends$ylab <- ends$cumulative_expected_area_m2 +
  c(s0 = -60, sm071 = 170, sm224 = -170)[as.character(ends$scenario)]

p_e <- ggplot(traj, aes(year, cumulative_expected_area_m2,
                        colour = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = cumulative_expected_area_m2 - replicate_sd_m2,
                  ymax = cumulative_expected_area_m2 + replicate_sd_m2),
              alpha = 0.25, colour = NA) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 0.7) +
  geom_text_pt(data = ends, aes(x = year + 0.18, y = ylab, label = label),
               hjust = 0, size_pt = 11, parse = TRUE, show.legend = FALSE) +
  scale_colour_manual(values = SCEN_COL, guide = "none") +
  scale_fill_manual(values = SCEN_COL, guide = "none") +
  scale_x_continuous(breaks = seq(2022, 2030, by = 2), limits = c(2022, 2034.2)) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05)),
                     labels = scales::label_comma()) +
  # Two-line y title: at 12 pt the one-line version is taller than the panel and
  # is clipped top and bottom.
  labs(x = "Year",
       y = expression(atop("Cumulative expected new", "colonization (" * m^2 * ")"))) +
  theme_paper(base_size = 12, style = "classic") +
  theme(axis.text = element_text(size = 11))

# ---- Assemble -------------------------------------------------------------
# Each map column is its own nested patchwork so that guides = "collect" merges
# the core-extent panel and its zoom into one colour bar per column; tags are
# therefore set by hand (automatic tagging skips nested patchworks).
p_a <- p_a + labs(tag = "a")
p_b <- p_b + labs(tag = "b")
p_c <- p_c + labs(tag = "c")
p_d <- p_d + labs(tag = "d")
p_e <- p_e + labs(tag = "e")

col_left  <- (p_a / p_c) + plot_layout(guides = "collect", heights = c(1, 1)) &
  theme(legend.position = "top")
col_right <- (p_b / p_d) + plot_layout(guides = "collect", heights = c(1, 1)) &
  theme(legend.position = "top")

fig <- ((col_left | col_right) / p_e) +
  plot_layout(heights = c(2.7, 1)) &
  theme(plot.tag = element_text(size = 13, face = "bold"))

# Height raised from 170 mm: the figure gained a whole row of zoom panels.
save_figure(fig, file.path(DIR_OUT_FIG, "fig05_projection.pdf"),
            width_mm = W_2COL, height_mm = 244)
save_figure(fig, file.path(DIR_OUT_FIG, "fig05_projection.png"),
            width_mm = W_2COL, height_mm = 244, dpi = 300)

# ---- Self-checks ----------------------------------------------------------
chk <- traj[traj$year == 2030, c("scenario", "cumulative_expected_area_m2")]
chk$rounded <- round(chk$cumulative_expected_area_m2)
print(chk)
stopifnot(identical(chk$rounded[match(c("s0", "sm071", "sm224"), chk$scenario)],
                    c(4901, 5730, 5524)))

if (requireNamespace("colorspace", quietly = TRUE)) {
  cat("\n-- vik ramp (diverging) colour-vision check --\n")
  print(check_palette(scico::scico(9, palette = "vik")))
  cat("\n-- sequential probability ramp check --\n")
  print(check_palette(viridisLite::rocket(5, direction = -1)))
  cat("\n-- scenario colours --\n")
  print(check_palette(unname(SCEN_COL)))
}
message("Fig. 5 written to ", DIR_OUT_FIG)
