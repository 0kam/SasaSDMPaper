# fig_common.R -- shared constants and helpers for all manuscript figures.
# Every figure script starts with:
#   source(file.path("analysis", "figures", "fig_common.R"))
#

source(file.path("analysis", "figures", "figure_recipes.R"))
# Base size 12 pt. Reviewer 3 asked for at least 9-10 pt; the co-authors then
# reported that the printed figures still read as "small type throughout", so the
# base was raised from 10 to 12 pt. axis.text is rel(0.9) -> 10.8 pt, axis and
# legend titles 12 pt, panel tags rel(1.1) -> 13.2 pt.
theme_set(theme_paper(base_size = 12, style = "box"))

suppressPackageStartupMessages({
  library(terra)
})

# ---- Paths ---------------------------------------------------------------
DIR_OUT_FIG <- file.path("paper", "files")          # final figures (fig01_*.pdf/tiff)
DIR_ANALYSIS_OUT <- file.path("analysis", "out")

VEGE_2012 <- file.path("ortho", "data", "vege_2012_5x5.tiff")
VEGE_2021 <- file.path("ortho", "data", "vege_2021_5x5.tiff")

# Vegetation class codes (scripts/sdm/plot_vegetation_map.R)
CLASS_SASA <- 1; CLASS_OTHERVEG <- 2; CLASS_NOVEG <- 3
CLASS_ROWAN <- 4; CLASS_MAPLE <- 5; CLASS_ALDER <- 6; CLASS_PINE <- 7

# ---- Core map extent -----------------------------------------------------
# All Sasa-focused maps share one extent: bounding box of 2021 Sasa patches
# larger than 5 m2 (the manuscript's patch rule, analysis/R/distance.R) plus a
# 150 m buffer, snapped outward to 50 m. Isolated noise cells are excluded.
sasa_core_bbox <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) return(cache)
    v <- rast(VEGE_2021)
    sasa <- ifel(v == CLASS_SASA, 1, NA)
    ids <- patches(sasa, directions = 4, zeroAsNA = TRUE)
    polys <- as.polygons(ids, aggregate = TRUE, na.rm = TRUE)
    polys$area_m2 <- as.numeric(expanse(polys, unit = "m", transform = TRUE))
    kept <- polys[polys$area_m2 > 5, ]
    e <- ext(kept)
    snap <- function(x, up) if (up) ceiling(x / 50) * 50 else floor(x / 50) * 50
    cache <<- ext(snap(e$xmin - 150, FALSE), snap(e$xmax + 150, TRUE),
                  snap(e$ymin - 150, FALSE), snap(e$ymax + 150, TRUE))
    cache
  }
})

# ---- Shared zoom window --------------------------------------------------
# One 150 m x 150 m window is enlarged in Figs. 2, 4 and 5 so that the three
# figures can be read against each other cell by cell. It is the window that
# Fig. 2's mechanical search selects as the strongest expansion front (the
# maximum count of cells gained between 2012 and 2021 over a 25 m search grid);
# fig02_study_area_change.R asserts that its search still returns this box.
# At 150 m across, a 1 m cell is about 0.4 mm wide on a 60 mm panel, so the
# one-cell-wide colonization halo along the front is resolved.
ZOOM_WINDOW <- c(xmin = 733325, xmax = 733475,
                 ymin = 4051125, ymax = 4051275)

zoom_ext <- function() terra::ext(ZOOM_WINDOW[["xmin"]], ZOOM_WINDOW[["xmax"]],
                                  ZOOM_WINDOW[["ymin"]], ZOOM_WINDOW[["ymax"]])

# Locator box for the zoom window, drawn on an overview map. It is a black frame
# on a white halo: the same box has to be legible over the dark end of the
# viridis suitability field (Fig. 4a) and over the near-white low end of the
# probability ramp (Figs. 4b, 5a, 5b), and neither a plain black nor a plain
# white rectangle survives both.
zoom_box_layers <- function(e = zoom_ext(), label = NULL, linewidth = 0.45,
                            label_size_pt = 11) {
  rect <- function(colour, lw) {
    annotate("rect", xmin = e$xmin, xmax = e$xmax, ymin = e$ymin, ymax = e$ymax,
             fill = NA, colour = colour, linewidth = lw)
  }
  out <- list(rect("white", linewidth * 3), rect("black", linewidth))
  if (!is.null(label)) {
    out <- c(out, list(
      annotate("label", x = e$xmin, y = e$ymax + 8, label = label,
               size = label_size_pt, size.unit = "pt", hjust = 0, vjust = 0,
               fontface = "bold", colour = "black", fill = "white",
               alpha = 0.85, label.r = unit(0, "mm"),
               label.padding = unit(0.4, "mm"))))
  }
  out
}

# Scale bar for a zoom panel, drawn in map units in the lower-left corner.
zoom_scalebar_layers <- function(e = zoom_ext(), len_m = 50, colour = "black",
                                 size_pt = 10) {
  w <- e$xmax - e$xmin; h <- e$ymax - e$ymin
  x0 <- e$xmin + 0.06 * w; y0 <- e$ymin + 0.07 * h
  list(
    annotate("segment", x = x0, xend = x0 + len_m, y = y0, yend = y0,
             colour = colour, linewidth = 0.5),
    annotate("segment", x = c(x0, x0 + len_m), xend = c(x0, x0 + len_m),
             y = y0 - 0.012 * h, yend = y0 + 0.012 * h,
             colour = colour, linewidth = 0.5),
    annotate_text_pt(x0 + len_m / 2, y0 + 0.028 * h, paste0(len_m, " m"),
                     size_pt = size_pt, colour = colour, vjust = 0)
  )
}

# Coordinate breaks for the 150 m zoom window: one every 50 m, inner ticks only.
ZOOM_X_BREAKS <- seq(733350, 733450, by = 50)
ZOOM_Y_BREAKS <- seq(4051150, 4051250, by = 50)

# ---- Shared map cosmetics ------------------------------------------------
# Contours: grey80, 0.2 mm, 100 m interval (drawn from the DEM, decluttered).
CONTOUR_COLOUR <- "grey80"
CONTOUR_LINEWIDTH <- 0.2
CONTOUR_INTERVAL_M <- 100

# 2021 Sasa outline: one thin line style everywhere (never a fill).
SASA_OUTLINE_COLOUR <- "black"
SASA_OUTLINE_LINEWIDTH <- 0.3

# Returns the 2021 Sasa distribution as polygons for outline overlay.
sasa_outline_2021 <- function() {
  v <- rast(VEGE_2021)
  as.polygons(ifel(v == CLASS_SASA, 1, NA), aggregate = TRUE, na.rm = TRUE)
}

# ---- English label dictionary (axis / legend text) -----------------------
LAB <- c(
  snow_mean = "Mean snowmelt date (DOY)",
  elevation = "Elevation (m)",
  slope     = "Slope (°)",
  TPI       = "Topographic position index",
  twi       = "Topographic wetness index",
  northness = "Northness (cos aspect)",
  eastness  = "Eastness (sin aspect)",
  suitability = "Habitat suitability",
  p_col_9yr = "P(established within 9 years)",
  p_col_2030 = "P(established by 2030)",
  easting   = "Easting (m)",
  northing  = "Northing (m)"
)

# Terminology: use "expansion"/"encroachment", never "invasion".
# Suitability is "habitat suitability", never "probability of occurrence".

# ---- Colonization-probability colour bar ---------------------------------
# Figs. 4(b), 5(a) and 6(a) share one probability ramp (viridis rocket reversed,
# fourth-root stretch) so that the three maps are read the same way. The breaks
# live here because they must stay identical across the three scripts: under the
# fourth-root stretch, 0.01 and 0.05 sit only 17 % of the bar apart, which is too
# little for two 9 pt tick labels, so 0.05/0.2 are replaced by a single 0.1.
PROB_LIM <- c(0, 0.7)
PROB_BREAKS <- c(0, 0.01, 0.1, 0.5)
PROB_LABELS <- c("0", "0.01", "0.1", "0.5")

# ---- Scenario labels -----------------------------------------------------
SCEN_LABELS <- c(
  s0    = "No change (s = 0)",
  sm071 = "Observed trend (s = −0.71 days/year)",
  sm224 = "CI lower bound (s = −2.24 days/year)"
)
