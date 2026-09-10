# fig06_risk_communities.R -- Fig. 6: which plant communities are at risk.
#
# Message: about nine tenths of the expected 2030 colonization inside herbaceous
# ("other") vegetation falls in two snowbed communities of the national
# (Ministry of the Environment, MOE) 1:25,000 vegetation map, and it sits within
# a few metres of the current Sasa front.
#   (a) P(colonized by 2030 | s = 0) restricted to 2021 "other vegetation",
#       with MOE community boundaries and the 2021 Sasa outline overlaid.
#   (b) Expected colonized area per community, two snowmelt scenarios shown as
#       two point symbols on one row (no dodged bars).
#
# Run from the repository root:
#   Rscript analysis/figures/fig06_risk_communities.R
# 6).
#
# Output: paper/files/fig06_risk_communities.pdf / .png (190 mm wide).
# Width note: this figure was 140 mm (1.5 column) while its text was 8 pt. At
# the 9-10 pt sizes Reviewer 3 asked for, the two-line community names in panel
# (b) alone claim about 40 mm of the axis, which left panel (b) roughly 25 mm of
# data area and clipped panel (a)'s line legend. It is therefore drawn at the
# full double-column width (190 mm); nothing else about the panels changed.
# Type note: the co-authors then asked for larger type throughout, so this
# figure now inherits base_size 12 from fig_common.R and its height went from
# 100 mm to 122 mm. Its layout is otherwise unchanged -- it has no zoom panel,
# because panel (a) is a community map, not a 1 m process map.

set.seed(1)
source(file.path("analysis", "figures", "fig_common.R"))

suppressPackageStartupMessages({
  library(tidyterra)
  library(patchwork)
  library(sf)
})

HAS_GGTEXT <- requireNamespace("ggtext", quietly = TRUE)
if (!HAS_GGTEXT) {
  message("NOTE: package 'ggtext' is not installed; panel (b) community names ",
          "are drawn as plain text and the species epithets are NOT italic.")
}

PATH_MOE <- file.path("data_external", "veg_murodo.gpkg")

# ---- Community dictionary --------------------------------------------------
# The MOE geopackage carries the community name directly in the 凡例名 field
# (the same field analysis/07_risky_areas.R uses to build
# analysis/out/moe_legend_lookup.csv, which maps legend_id -> 凡例名). The
# scientific names below are the standard readings of those Japanese community
# names; "Other communities" pools every remaining legend.
COMM_JA <- c(
  fauria    = "イワイチョウ－ショウジョウスゲ群集",
  anaphalis = "タカネヤハズハハコ－アオノツガザクラ群集",
  pinus     = "コケモモ－ハイマツ群集"
)
COMM_MD <- c(
  fauria    = "(i) *Fauria crista-galli*–<br>*Carex blepharicarpa* comm.",
  anaphalis = "(ii) *Anaphalis alpicola*–<br>*Phyllodoce aleutica* comm.",
  pinus     = "*Vaccinium vitis-idaea*–<br>*Pinus pumila* comm.",
  other     = "Other communities"
)
COMM_PLAIN <- c(
  fauria    = "(i) Fauria crista-galli-\nCarex blepharicarpa comm.",
  anaphalis = "(ii) Anaphalis alpicola-\nPhyllodoce aleutica comm.",
  pinus     = "Vaccinium vitis-idaea-\nPinus pumila comm.",
  other     = "Other communities"
)
COMM_ORDER <- c("fauria", "anaphalis", "pinus", "other")

# ---- Inputs ----------------------------------------------------------------
core <- sasa_core_bbox()

risky <- crop(rast(file.path(DIR_ANALYSIS_OUT, "risky_2030_s0.tif")), core)
names(risky) <- "p"

# 2021 Sasa outline, restricted to patches larger than 5 m2 -- the manuscript's
# front definition (analysis/R/distance.R) and the same filter Fig. 4 applies.
# fig_common.R's sasa_outline_2021() keeps every Sasa cell, which scatters
# isolated single-cell dots across the eastern half of the map.
SASA_MIN_AREA_M2 <- 5
outline21 <- local({
  v <- rast(VEGE_2021)
  ids <- patches(ifel(v == CLASS_SASA, 1, NA), directions = 4, zeroAsNA = TRUE)
  polys <- as.polygons(ids, aggregate = TRUE, na.rm = TRUE)
  polys$area_m2 <- as.numeric(expanse(polys, unit = "m", transform = TRUE))
  kept <- polys[polys$area_m2 > SASA_MIN_AREA_M2, ]
  stopifnot(nrow(kept) > 0)
  sf::st_as_sf(crop(aggregate(kept), core))
})

dem <- crop(rast(file.path(DIR_ANALYSIS_OUT, "predictors.tif"))[["elevation"]], core)
contours <- sf::st_as_sf(as.contour(
  dem, levels = seq(floor(minmax(dem)[1] / CONTOUR_INTERVAL_M) * CONTOUR_INTERVAL_M,
                    ceiling(minmax(dem)[2] / CONTOUR_INTERVAL_M) * CONTOUR_INTERVAL_M,
                    by = CONTOUR_INTERVAL_M)))

# MOE community polygons: keep only the name field, project onto the analysis
# CRS, dissolve by community, then clip to the core extent.
moe <- vect(PATH_MOE)
stopifnot("凡例名" %in% names(moe))
moe <- moe[, "凡例名", drop = FALSE]
moe$comm_ja <- trimws(as.character(moe[["凡例名"]][, 1]))
moe <- project(moe, crs(risky))
moe <- aggregate(moe, by = "comm_ja")
moe_core <- crop(moe, core)
moe_sf <- sf::st_as_sf(moe_core)

# Label anchors for the two focal communities: the interior centroid of their
# largest polygon part inside the core extent.
label_point <- function(comm_ja) {
  parts <- disagg(moe_core[moe_core$comm_ja == comm_ja, ])
  parts <- parts[order(expanse(parts, unit = "m"), decreasing = TRUE), ][1, ]
  as.data.frame(crds(centroids(parts, inside = TRUE)))
}
anchors <- do.call(rbind, lapply(c("fauria", "anaphalis"), function(k) {
  p <- label_point(COMM_JA[[k]])
  p$label <- c(fauria = "(i)", anaphalis = "(ii)")[[k]]
  p
}))
# Nudge (i) off the densest part of the Sasa front (checked visually).
anchors$x[anchors$label == "(i)"] <- anchors$x[anchors$label == "(i)"] + 60

# ---- (a) risk map ----------------------------------------------------------
xr <- c(core$xmin, core$xmax); yr <- c(core$ymin, core$ymax)

BAR_M <- 200
bar <- data.frame(
  xmin = xr[2] - 0.06 * diff(xr) - BAR_M, xmax = xr[2] - 0.06 * diff(xr),
  ymin = yr[1] + 0.055 * diff(yr), ymax = yr[1] + 0.055 * diff(yr) + 0.014 * diff(yr)
)

# Colonization probability must look the same in every figure: this is exactly
# the scale of Fig. 4(b) and Fig. 5(a) -- viridis rocket reversed, fourth-root
# stretch, published breaks.
# PROB_LIM / PROB_BREAKS / PROB_LABELS are defined in fig_common.R.

root4 <- if (utils::packageVersion("scales") >= "1.3.0") {
  scales::new_transform("root4", function(x) x^0.25, function(x) x^4)
} else {
  scales::trans_new("root4", function(x) x^0.25, function(x) x^4)
}

# Axis breaks identical to Fig. 4 and Fig. 5; labels stay horizontal.
X_BREAKS <- c(733000, 733500)
Y_BREAKS <- seq(4050800, 4051800, by = 400)

LINE_KEYS <- c("Community boundary", if (HAS_GGTEXT) "2021 *Sasa*" else "2021 Sasa")
LINE_COLS <- c("grey50", SASA_OUTLINE_COLOUR)
LINE_TYPES <- c("22", "solid")
names(LINE_COLS) <- names(LINE_TYPES) <- LINE_KEYS

p_a <- ggplot() +
  geom_sf(data = contours, colour = CONTOUR_COLOUR, linewidth = CONTOUR_LINEWIDTH) +
  geom_spatraster(data = risky, maxcell = Inf) +
  scale_fill_viridis_c(
    option = "rocket", direction = -1, name = LAB[["p_col_2030"]],
    na.value = "transparent", limits = PROB_LIM, transform = root4,
    # Short break labels on purpose: ggplot2 widens a horizontal colourbar until
    # the tick labels stop colliding, and "0.00"/"0.20" would stretch the key
    # across the whole panel.
    breaks = PROB_BREAKS, labels = PROB_LABELS,
    guide = guide_colourbar(
      ticks.colour = "grey20", order = 1,
      theme = theme(legend.key.width = unit(46, "mm"),
                    legend.key.height = unit(2.6, "mm"),
                    legend.title.position = "top")
    )
  ) +
  geom_sf(data = moe_sf, aes(colour = LINE_KEYS[1], linetype = LINE_KEYS[1]),
          fill = NA, linewidth = 0.2, show.legend = "line") +
  geom_sf(data = outline21, aes(colour = LINE_KEYS[2], linetype = LINE_KEYS[2]),
          fill = NA, linewidth = SASA_OUTLINE_LINEWIDTH, show.legend = "line") +
  scale_colour_manual(values = LINE_COLS, breaks = LINE_KEYS, name = NULL) +
  scale_linetype_manual(values = LINE_TYPES, breaks = LINE_KEYS, name = NULL) +
  geom_label(data = anchors, aes(x = x, y = y, label = label),
             inherit.aes = FALSE, size = 11, size.unit = "pt",
             fill = "white", colour = "black", alpha = 0.85,
             linewidth = 0.15, label.padding = unit(0.6, "mm")) +
  annotate("rect", xmin = bar$xmin, xmax = bar$xmax, ymin = bar$ymin, ymax = bar$ymax,
           fill = "black", colour = "black", linewidth = 0.2) +
  annotate_text_pt(x = mean(c(bar$xmin, bar$xmax)), y = bar$ymax + 0.03 * diff(yr),
                   label = paste0(BAR_M, " m"), size_pt = 10, vjust = 0) +
  coord_sf(xlim = xr, ylim = yr, expand = FALSE, datum = sf::st_crs(6690)) +
  scale_x_continuous(breaks = X_BREAKS) +
  scale_y_continuous(breaks = Y_BREAKS) +
  labs(x = LAB[["easting"]], y = LAB[["northing"]]) +
  theme(legend.position = "bottom", legend.box = "vertical",
        legend.box.spacing = unit(1, "mm"), legend.spacing.y = unit(0.5, "mm"),
        axis.text = element_text(size = 9.5),
        # Right pad so the 734000 tick label (the core extent's edge) is not
        # clipped: this panel is narrower than the map panels of Figs. 4 and 5,
        # and wrap_elements() below stops patchwork from finding the room itself.
        plot.margin = margin(2, 11, 2, 2, "mm"))

# The line legend gets its own guide theme. Markdown legend text is applied only
# here: setting element_markdown() on the whole plot theme also affects the
# colourbar and makes ggplot2 stretch the bar across the panel.
line_guide_theme <- theme(legend.key.width = unit(7, "mm"),
                          legend.key.height = unit(3, "mm"))
if (HAS_GGTEXT) {
  line_guide_theme <- line_guide_theme +
    theme(legend.text = ggtext::element_markdown(size = rel(0.9)))
}
# Two rows, not one: at 10.8 pt the two keys side by side are wider than the
# panel and the second label ("2021 Sasa") is clipped.
p_a <- p_a +
  guides(colour = guide_legend(order = 2, nrow = 2,
                               override.aes = list(fill = NA),
                               theme = line_guide_theme),
         linetype = guide_legend(order = 2, nrow = 2,
                                 theme = line_guide_theme))

# ---- (b) expected area per community ---------------------------------------
comp <- read.csv(file.path(DIR_ANALYSIS_OUT, "risky_composition.csv"),
                 check.names = FALSE, fileEncoding = "UTF-8")
comp <- comp[comp$scenario %in% c("s0", "sm071"), ]
comp$comm <- names(COMM_JA)[match(comp[["凡例名"]], COMM_JA)]
comp$comm[is.na(comp$comm)] <- "other"

agg <- stats::aggregate(expected_area_m2 ~ scenario + comm, data = comp, FUN = sum)
totals <- stats::aggregate(expected_area_m2 ~ scenario, data = comp, FUN = sum)
agg$share <- 100 * agg$expected_area_m2 /
  totals$expected_area_m2[match(agg$scenario, totals$scenario)]

agg$comm <- factor(agg$comm, levels = rev(COMM_ORDER))
agg$scenario <- factor(agg$scenario, levels = c("s0", "sm071"))

wide <- reshape(agg[, c("comm", "scenario", "expected_area_m2")],
                direction = "wide", idvar = "comm", timevar = "scenario")
names(wide) <- c("comm", "x_s0", "x_sm071")

# Share range printed next to each row, matching the wording in Results.
share_lab <- do.call(rbind, lapply(levels(agg$comm), function(k) {
  s <- sort(agg$share[agg$comm == k])
  lab <- if (round(s[1]) == round(s[2])) sprintf("%.0f%%", mean(s)) else
    sprintf("%.0f–%.0f%%", round(s[1]), round(s[2]))
  data.frame(comm = k, x = max(agg$expected_area_m2[agg$comm == k]), label = lab)
}))
share_lab$comm <- factor(share_lab$comm, levels = rev(COMM_ORDER))

SCEN_COL <- c(s0    = unname(okabeito_colours["bluishgreen"]),
              sm071 = unname(okabeito_colours["vermillion"]))
SCEN_SHP <- c(s0 = 21, sm071 = 24)
SCEN_LAB <- c(s0 = "s = 0", sm071 = "s = −0.71 days/year")

x_max <- max(agg$expected_area_m2)
comm_labels <- if (HAS_GGTEXT) COMM_MD else COMM_PLAIN

p_b <- ggplot(agg, aes(x = expected_area_m2, y = comm)) +
  geom_segment(data = wide, aes(x = x_s0, xend = x_sm071, y = comm, yend = comm),
               inherit.aes = FALSE, colour = "grey65", linewidth = 0.3) +
  geom_point(aes(fill = scenario, shape = scenario), colour = "white",
             size = 1.7, stroke = 0.25) +
  geom_text_pt(data = share_lab, aes(x = x, y = comm, label = label),
               inherit.aes = FALSE, hjust = 0, nudge_x = 0.035 * x_max,
               size_pt = 9.5, colour = "grey30") +
  scale_fill_manual(values = SCEN_COL, labels = SCEN_LAB, name = NULL, guide = guide_legend(ncol = 1)) +
  scale_shape_manual(values = SCEN_SHP, labels = SCEN_LAB, name = NULL,
                     guide = guide_legend(ncol = 1)) +
  scale_x_continuous(limits = c(0, x_max * 1.85), expand = expansion(mult = c(0.02, 0)),
                     breaks = c(0, 2000),
                     labels = scales::label_comma()) +
  scale_y_discrete(labels = comm_labels) +
  labs(x = "Expected establishment\narea (m²)", y = NULL) +
  theme_paper(base_size = 12, style = "classic") +
  theme(legend.position = "bottom", legend.box.spacing = unit(1, "mm"),
        plot.margin = margin(2, 4, 2, 2, "mm"),
        axis.text.y = if (HAS_GGTEXT) ggtext::element_markdown(size = 9.5,
                                                               lineheight = 1.05,
                                                               colour = "black")
                      else element_text(size = 9.5, lineheight = 1.0,
                                        colour = "black"))

# ---- Assemble --------------------------------------------------------------
# wrap_elements() keeps each panel's own legend inside its own cell; without it
# patchwork aligns the two legend rows and the map legend runs under panel (b).
fig <- (wrap_elements(full = p_a) | wrap_elements(full = p_b)) +
  plot_layout(widths = c(1, 1.1)) +
  plot_annotation(tag_levels = "a")

save_figure(fig, file.path(DIR_OUT_FIG, "fig06_risk_communities.pdf"),
            width_mm = 170, height_mm = 112)
save_figure(fig, file.path(DIR_OUT_FIG, "fig06_risk_communities.png"),
            width_mm = 170, height_mm = 112, dpi = 300)

# ---- Self-checks -----------------------------------------------------------
cat("\n-- expected establishment area by community (m2) and share (%) --\n")
print(agg[order(agg$scenario, -agg$expected_area_m2),
          c("scenario", "comm", "expected_area_m2", "share")], row.names = FALSE)
cat("\n-- scenario totals (m2) --\n"); print(totals, row.names = FALSE)

# Manuscript (paper/results.qmd): 2,429 m2 (s = 0) and 3,383 m2 (s = -0.71);
# 73-74% Fauria-Carex, 15-16% Anaphalis-Phyllodoce, about a tenth Pinus pumila.
stopifnot(
  round(totals$expected_area_m2[totals$scenario == "s0"]) == 2429,
  round(totals$expected_area_m2[totals$scenario == "sm071"]) == 3383
)
sh <- function(cm, sc) agg$share[agg$comm == cm & agg$scenario == sc]
stopifnot(
  all(round(c(sh("fauria", "s0"), sh("fauria", "sm071"))) %in% 73:74),
  all(round(c(sh("anaphalis", "s0"), sh("anaphalis", "sm071"))) %in% 15:16),
  all(round(c(sh("pinus", "s0"), sh("pinus", "sm071"))) %in% 9:11),
  round(sh("fauria", "s0") + sh("anaphalis", "s0")) == 89
)
cat("\nManuscript consistency check: OK\n")

if (requireNamespace("colorspace", quietly = TRUE)) {
  cat("\n-- probability ramp (rocket reversed) colour-vision check --\n")
  print(check_palette(viridisLite::rocket(5, direction = -1)))
  cat("\n-- scenario colours --\n")
  print(check_palette(unname(SCEN_COL)))
}
message("Fig. 6 written to ", DIR_OUT_FIG)
