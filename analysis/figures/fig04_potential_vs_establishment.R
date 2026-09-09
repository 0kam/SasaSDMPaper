# =============================================================================
# fig04_potential_vs_establishment.R -- Fig. 4 (key figure)
# =============================================================================
# Message: the environment permits Sasa far beyond the current patches, but in
# nine years establishment only reached a narrow band a few metres wide.
#
#   (a) Model A habitat suitability + TSS-maximising threshold isoline
#   (b) Model B nine-year colonization probability (distance from the 2021 front)
#   (c) (a) enlarged over the shared 150 m zoom window (fig_common.R ZOOM_WINDOW)
#   (d) (b) enlarged over the same window -- at this scale the one-cell-wide
#       colonization halo hugging the 2021 front is resolved, which is the point
#       the core-extent panels are too coarse to make
#   (e) Distance kernel: observed vs fitted nine-year colonization rate
#
# The zoom window is the same 150 m box that Fig. 2(c) enlarges, so Figs. 2, 4
# and 5 can be read against each other cell by cell.
#
# Run from the repository root:
#   Rscript analysis/figures/fig04_potential_vs_establishment.R
#
# Design reference: review/figure_redesign_plan_ja.md, section "Fig. 4".
# -----------------------------------------------------------------------------

set.seed(20260830)

source(file.path("analysis", "figures", "fig_common.R"))

suppressPackageStartupMessages({
  library(ggplot2)
  library(tidyterra)
  library(patchwork)
  library(sf)
})

# ---- Inputs ---------------------------------------------------------------
PATH_SUIT_A  <- file.path(DIR_ANALYSIS_OUT, "suitability_A_primary.tif")
PATH_P9      <- file.path(DIR_ANALYSIS_OUT, "p9_colonization.tif")
PATH_THRESH  <- file.path(DIR_ANALYSIS_OUT, "model_A_thresholds.csv")
PATH_BANDS   <- file.path(DIR_ANALYSIS_OUT, "model_B_logdist_comparison.csv")
PATH_DEM     <- file.path("ortho", "data", "terrain_features", "tateyamadem_small.tif")

OUT_PDF <- file.path(DIR_OUT_FIG, "fig04_potential_vs_establishment.pdf")
OUT_PNG <- file.path(DIR_OUT_FIG, "fig04_potential_vs_establishment.png")

CRS_MAP <- "EPSG:6690"
core <- sasa_core_bbox()

# ---- Shared map furniture -------------------------------------------------
# Contours: 100 m interval, grey80, 0.2 mm (fig_common.R constants).
contours_core <- local({
  dem <- terra::rast(PATH_DEM)
  dem <- terra::project(dem, CRS_MAP)
  pad <- terra::ext(core$xmin - 100, core$xmax + 100,
                    core$ymin - 100, core$ymax + 100)
  dem <- terra::crop(dem, pad)
  rng <- terra::minmax(dem)
  lv <- seq(ceiling(rng[1] / CONTOUR_INTERVAL_M) * CONTOUR_INTERVAL_M,
            floor(rng[2] / CONTOUR_INTERVAL_M) * CONTOUR_INTERVAL_M,
            by = CONTOUR_INTERVAL_M)
  sf::st_as_sf(terra::as.contour(dem, levels = lv))
})

# 2021 Sasa outline, restricted to patches larger than 5 m2 -- the same patch
# rule the manuscript's front definition uses (sasa_distance() in
# analysis/R/distance.R: patches(directions = 4) -> geodesic expanse > 5 m2).
# fig_common.R's sasa_outline_2021() keeps every Sasa cell, which scatters
# isolated single-cell dots across the eastern half of the map.
SASA_MIN_AREA_M2 <- 5
sasa21_sf <- local({
  v <- terra::rast(VEGE_2021)
  ids <- terra::patches(terra::ifel(v == CLASS_SASA, 1, NA),
                        directions = 4, zeroAsNA = TRUE)
  polys <- terra::as.polygons(ids, aggregate = TRUE, na.rm = TRUE)
  polys$area_m2 <- as.numeric(terra::expanse(polys, unit = "m", transform = TRUE))
  kept <- polys[polys$area_m2 > SASA_MIN_AREA_M2, ]
  stopifnot(nrow(kept) > 0)
  terra::aggregate(kept)
})
sasa21_core <- sf::st_as_sf(terra::crop(sasa21_sf, core))
sasa21_zoom <- sf::st_as_sf(terra::crop(sasa21_sf, zoom_ext()))
stopifnot(nrow(sasa21_core) > 0, nrow(sasa21_zoom) > 0)

X_BREAKS <- seq(732800, 734000, by = 400)
Y_BREAKS <- seq(4050800, 4051800, by = 400)

# Hand-drawn scale bar: guarantees >= 10 pt text (annotation_scale sizes text in
# `cex`, which is easy to drop below the journal minimum without noticing).
SCALEBAR_M <- 200
scalebar_layers <- function(colour = "black") {
  x1 <- core$xmax - 90
  x0 <- x1 - SCALEBAR_M
  y0 <- core$ymin + 85
  list(
    annotate("segment", x = x0, xend = x1, y = y0, yend = y0,
             colour = colour, linewidth = 0.4),
    annotate("segment", x = c(x0, x1), xend = c(x0, x1),
             y = y0 - 12, yend = y0 + 12, colour = colour, linewidth = 0.4),
    annotate_text_pt(x = (x0 + x1) / 2, y = y0 + 30,
                     label = paste0(SCALEBAR_M, " m"),
                     size_pt = 10, colour = colour, vjust = 0)
  )
}

# In-panel key: a white plate keeps the text legible over both the dark viridis
# field and the empty (NA) corners of the analysis domain.
panel_key <- function(label, parse = FALSE) {
  annotate("label", x = core$xmin + 25, y = core$ymax - 25, label = label,
           parse = parse, size = 10, size.unit = "pt", hjust = 0, vjust = 1,
           colour = "black", fill = "white", alpha = 0.8,
           label.r = unit(0, "mm"),
           label.padding = unit(0.5, "mm"))
}

map_frame <- function(p) {
  p +
    coord_sf(xlim = c(core$xmin, core$xmax),
             ylim = c(core$ymin, core$ymax),
             expand = FALSE, datum = sf::st_crs(CRS_MAP)) +
    scale_x_continuous(breaks = X_BREAKS) +
    scale_y_continuous(breaks = Y_BREAKS) +
    labs(x = LAB[["easting"]], y = LAB[["northing"]]) +
    theme(
      legend.position = "top",
      legend.title.position = "top",
      legend.justification = "left",
      axis.text = element_text(size = 11),
      # Right pad: at 11 pt the "734000" tick label (the core extent's own edge)
      # is most of a label wider than the panel and is otherwise clipped.
      plot.margin = margin(1, 8, 1, 1, "mm")
    )
}

# Zoom-panel frame: same furniture, own breaks, and no colour guide -- the bar
# above the matching core-extent panel is shared through guides = "collect".
zoom_frame <- function(p) {
  z <- zoom_ext()
  p +
    coord_sf(xlim = c(z$xmin, z$xmax), ylim = c(z$ymin, z$ymax),
             expand = FALSE, datum = sf::st_crs(CRS_MAP)) +
    scale_x_continuous(breaks = ZOOM_X_BREAKS) +
    scale_y_continuous(breaks = ZOOM_Y_BREAKS) +
    labs(x = LAB[["easting"]], y = LAB[["northing"]]) +
    theme(legend.position = "top",
          legend.title.position = "top",
          legend.justification = "left",
          axis.text = element_text(size = 11),
          plot.margin = margin(1, 8, 1, 1, "mm"))
}

contour_layer <- geom_sf(data = contours_core, colour = CONTOUR_COLOUR,
                         linewidth = CONTOUR_LINEWIDTH, inherit.aes = FALSE)
sasa_layer <- geom_sf(data = sasa21_core, fill = NA, colour = SASA_OUTLINE_COLOUR,
                      linewidth = SASA_OUTLINE_LINEWIDTH, inherit.aes = FALSE)
# In the zoom panels the 1 m patch boundary is drawn a little heavier: it is the
# reference the colonization halo is measured against.
sasa_layer_zoom <- geom_sf(data = sasa21_zoom, fill = NA,
                           colour = SASA_OUTLINE_COLOUR, linewidth = 0.45,
                           inherit.aes = FALSE)

# Bar widened from 46 mm: at 10.8 pt tick labels the old width let neighbouring
# labels touch (see the break choices below).
colourbar <- guide_colourbar(
  theme = theme(legend.key.width = unit(54, "mm"),
                legend.key.height = unit(2.6, "mm"),
                legend.ticks = element_blank())
)

# ---- (a) Model A habitat suitability --------------------------------------
suit <- terra::crop(terra::rast(PATH_SUIT_A), core)
names(suit) <- "suitability"

thresholds <- utils::read.csv(PATH_THRESH)
THR_A <- thresholds$threshold[thresholds$variant == "primary"]
stopifnot(length(THR_A) == 1, abs(THR_A - 0.158378627850608) < 1e-12)

thr_sf <- sf::st_as_sf(terra::as.contour(suit, levels = THR_A))

suit_rng <- as.numeric(terra::minmax(suit))

# One scale object, reused by (a) and its zoom (c), so that patchwork's
# guides = "collect" recognises the two legends as identical and keeps one.
suit_scale <- scale_fill_viridis_c(
  name = LAB[["suitability"]], na.value = "transparent",
  limits = c(floor(suit_rng[1] * 20) / 20, ceiling(suit_rng[2] * 20) / 20),
  # Two-decimal labels only where they carry information: at 10.8 pt the
  # "0.16"/"0.30" pair (18 % of the bar apart) would otherwise touch.
  breaks = c(THR_A, 0.3, 0.5, 0.7),
  labels = c("0.16", "0.3", "0.5", "0.7"),
  guide = colourbar
)

p_a <- ggplot() +
  geom_spatraster(data = suit, maxcell = Inf) +
  contour_layer +
  geom_sf(data = thr_sf, colour = "white", linewidth = 0.25,
          inherit.aes = FALSE) +
  sasa_layer +
  suit_scale +
  zoom_box_layers(label = "c") +
  scalebar_layers("white")
# Two lines: at 10 pt the one-line version of this key is wider than the panel.
p_a <- map_frame(p_a) +
  panel_key('atop("TSS threshold (white line),", "2021 "*italic("Sasa")*" (black)")',
            parse = TRUE)

# ---- (b) Model B nine-year colonization probability ------------------------
# Source: analysis/out/p9_colonization.tif, written by analysis/04_model_B.R --
# the adopted log-distance GAM predicted with distance recomputed from the 2021
# Sasa front and masked to 2021 non-Sasa cells. This is exactly the raster that
# analysis/08_manuscript_figures.R drew as panel (b) of fig_models_AB.png.
p9 <- terra::crop(terra::rast(PATH_P9), core)
names(p9) <- "p9"

# Probabilities are extremely skewed (median ~5e-4, max 0.64): keep the fourth-
# root stretch and the published breaks, but swap the OrRd ramp for rocket
# reversed so that low values stay pale and the front reads dark.
root4 <- if (utils::packageVersion("scales") >= "1.3.0") {
  scales::new_transform("root4", function(x) x^0.25, function(x) x^4)
} else {
  scales::trans_new("root4", function(x) x^0.25, function(x) x^4)
}

# Shared by (b) and its zoom (d), for the same reason as suit_scale above.
p9_scale <- scale_fill_viridis_c(
  option = "rocket", direction = -1, name = LAB[["p_col_9yr"]],
  na.value = "transparent", limits = PROB_LIM, transform = root4,
  # Four breaks, not five: under the fourth-root stretch 0.01 and 0.05 sit
  # 17 % of the bar apart, which no longer separates two 10.8 pt labels. The
  # same four breaks are used in Fig. 5(a) and Fig. 6(a).
  breaks = PROB_BREAKS, labels = PROB_LABELS, guide = colourbar
)

p_b <- ggplot() +
  geom_spatraster(data = p9, maxcell = Inf) +
  contour_layer +
  sasa_layer +
  p9_scale +
  zoom_box_layers(label = "d") +
  scalebar_layers("black")
p_b <- map_frame(p_b) +
  panel_key('"2021 "*italic("Sasa")*" (black line)"', parse = TRUE)

# ---- (c)(d) Zoom panels ----------------------------------------------------
# Same two rasters, same two colour scales, cropped to ZOOM_WINDOW. At 150 m
# across in a roughly 65 mm panel a 1 m cell is about 0.43 mm wide, so the
# one-cell halo of raised colonization probability around the 2021 patches --
# invisible at the core extent -- is resolved here.
suit_zoom <- terra::crop(terra::rast(PATH_SUIT_A), zoom_ext())
names(suit_zoom) <- "suitability"
thr_zoom_sf <- sf::st_as_sf(terra::as.contour(suit_zoom, levels = THR_A))
p9_zoom <- terra::crop(terra::rast(PATH_P9), zoom_ext())
names(p9_zoom) <- "p9"
message(sprintf("zoom window P(col in 9 yr): min %.4f, max %.4f, cells %d",
                min(terra::values(p9_zoom), na.rm = TRUE),
                max(terra::values(p9_zoom), na.rm = TRUE),
                sum(!is.na(terra::values(p9_zoom)))))

p_c <- ggplot() +
  geom_spatraster(data = suit_zoom, maxcell = Inf) +
  geom_sf(data = thr_zoom_sf, colour = "white", linewidth = 0.3,
          inherit.aes = FALSE) +
  sasa_layer_zoom +
  suit_scale +
  zoom_scalebar_layers(colour = "white")
p_c <- zoom_frame(p_c)

p_d <- ggplot() +
  geom_spatraster(data = p9_zoom, maxcell = Inf) +
  sasa_layer_zoom +
  p9_scale +
  zoom_scalebar_layers(colour = "black")
p_d <- zoom_frame(p_d)

# ---- (e) Distance kernel ---------------------------------------------------
# Two series only: the observed nine-year colonization rate per distance band and
# Model B's fitted mean for the same band. The kernel curve exported by
# analysis/04_model_B.R is evaluated at the domain medians of the other
# predictors, so it sits systematically below the band aggregates and reads as a
# bad fit; the manuscript's claim is about the band-level agreement.
bands <- utils::read.csv(PATH_BANDS)
bands <- bands[bands$section == "distance_band" & bands$model == "logdist", ]
# Drop the open-ended ">=160" band: it has no midpoint on a distance axis.
band_mid <- c("[0,5)" = 2.5, "[5,10)" = 7.5, "[10,20)" = 15,
              "[20,40)" = 30, "[40,80)" = 60, "[80,160)" = 120)
bands$mid <- unname(band_mid[bands$distance_band_m])
bands <- bands[!is.na(bands$mid), ]

y_min <- min(bands$observed_colonization_rate) / 2

VERMILLION <- unname(okabeito_colours["vermillion"])

p_e <- ggplot() +
  geom_vline(xintercept = c(5, 10), colour = "grey85", linewidth = 0.3) +
  geom_line(data = bands, aes(mid, predicted_mean_p9),
            colour = VERMILLION, linewidth = 0.35) +
  geom_point(data = bands, aes(mid, predicted_mean_p9),
             colour = VERMILLION, shape = 21, fill = "white",
             size = 2.1, stroke = 0.5) +
  geom_point(data = bands, aes(mid, observed_colonization_rate),
             colour = "black", size = 1.1) +
  annotate_text_pt(x = 2.7, y = 0.0032,
                   label = "Observed 2012–2021", size_pt = 11, hjust = 0) +
  annotate_text_pt(x = 17, y = 0.016,
                   label = "Model B (fitted)",
                   size_pt = 11, colour = VERMILLION, hjust = 0) +
  annotate_text_pt(x = 5.3, y = y_min * 1.3,
                   label = "5 m", size_pt = 10, colour = "grey45", hjust = 0) +
  annotate_text_pt(x = 11.0, y = y_min * 1.3,
                   label = "10 m", size_pt = 10, colour = "grey45", hjust = 0) +
  scale_x_continuous(
    transform = scales::transform_log1p(),
    breaks = c(0, 5, 10, 20, 40, 80, 160),
    limits = c(0, 200), expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_log10(
    breaks = c(1e-4, 1e-3, 1e-2, 1e-1),
    labels = c("0.01%", "0.1%", "1%", "10%"),
    limits = c(y_min, 0.2)
  ) +
  labs(x = expression("Distance to the 2012 "*italic("Sasa")*" front (m)"),
       y = "Nine-year colonization rate") +
  theme_paper(base_size = 12, style = "classic") +
  theme(axis.text = element_text(size = 11),
        plot.margin = margin(2, 2, 1, 1, "mm"))

# ---- Compose ---------------------------------------------------------------
# ---- Compose ---------------------------------------------------------------
# Each map column is its own nested patchwork so that guides = "collect" merges
# the core-extent panel and its zoom into a single colour bar per column; the
# tags are therefore set by hand (automatic tagging skips nested patchworks).
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
  plot_layout(heights = c(2.9, 1)) &
  theme(plot.tag = element_text(size = 13, face = "bold"))

# Height raised from 163 mm: the figure gained a whole row of zoom panels.
save_figure(fig, OUT_PDF, width_mm = W_2COL, height_mm = 244)
save_figure(fig, OUT_PNG, width_mm = W_2COL, height_mm = 244, dpi = 300)

cat("Wrote", OUT_PDF, "and", OUT_PNG, "\n")
