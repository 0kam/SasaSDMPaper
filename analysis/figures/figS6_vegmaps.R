# figS6_vegmaps.R -- Figure S6: seven-class vegetation maps for 2012 and 2021.
#
# Reviewer 1 asked for the two maps to be stacked vertically, to share a single
# legend, and to be enlarged. This script replaces the old side-by-side
# paper/files/vegemap.jpg with a two-row figure over the full analysis extent
# (not the Fig. 2 core window). No contours: at this extent they only add ink
# over the class colours.
#
# Output: paper/files/si/figS6_vegmaps.png (140 mm wide, 300 dpi).
# Run from the project root:  Rscript analysis/figures/figS6_vegmaps.R

source(file.path("analysis", "figures", "fig_common.R"))

suppressPackageStartupMessages({
  library(ggplot2)
  library(tidyterra)
  library(patchwork)
  library(sf)
})

# ---- Palette -------------------------------------------------------------
# Copied verbatim from analysis/figures/fig02_study_area_change.R so that the
# SI maps carry exactly the same class names and colours as Fig. 2(a).
VEG_LEVELS <- c("Sasa", "Other vegetation", "No vegetation", "Rowans",
                "Maple", "Montane alder", "Dwarf pine")
VEG_COLS <- c(
  "Sasa"             = "#D55E00",  # Okabe-Ito vermillion
  "Other vegetation" = "#EEE8D5",  # pale beige (as in vegemap.jpg)
  "No vegetation"    = "#BFBFBF",
  "Rowans"           = "#F0E442",  # Okabe-Ito yellow
  "Maple"            = "#56B4E9",  # Okabe-Ito sky blue
  "Montane alder"    = "#0072B2",  # Okabe-Ito blue
  "Dwarf pine"       = "#00654A"   # Okabe-Ito bluish green, darkened
)
VEG_CODE <- c(CLASS_SASA, CLASS_OTHERVEG, CLASS_NOVEG, CLASS_ROWAN,
              CLASS_MAPLE, CLASS_ALDER, CLASS_PINE)

# ---- Data ----------------------------------------------------------------
v12 <- rast(VEGE_2012)
v21 <- rast(VEGE_2021)
stopifnot(compareGeom(v12, v21, stopOnError = FALSE))

# Code 0 marks cells outside the classified image (sky mask / no data).
as_classes <- function(r) {
  out <- classify(r, cbind(VEG_CODE, seq_along(VEG_CODE)), others = NA)
  levels(out) <- data.frame(value = seq_along(VEG_LEVELS), class = VEG_LEVELS)
  names(out) <- "class"
  out
}
veg12 <- as_classes(v12)
veg21 <- as_classes(v21)

e <- ext(v21)

# ---- Panels --------------------------------------------------------------
# One shared scale object for both panels so that patchwork's
# guides = "collect" recognises the two legends as identical and keeps one.
veg_scale <- scale_fill_manual(
  values = VEG_COLS, limits = VEG_LEVELS, drop = FALSE,
  labels = c(expression(italic("Sasa")), VEG_LEVELS[-1]),
  na.value = "transparent", na.translate = FALSE,
  name = "Vegetation class",
  # Three columns, title above: four columns overrun the 140 mm width and the
  # last label ("Rowans") is clipped.
  guide = guide_legend(ncol = 3, byrow = TRUE, title.position = "top",
                       title.hjust = 0.5))

# Graticule breaks must be explicit numbers: under coord_sf() a breaks
# *function* yields no labels (same reason as in fig02_study_area_change.R).
inner_breaks <- function(lo, hi, n = 3) {
  b <- scales::breaks_pretty(n)(c(lo, hi))
  b[b > lo + 0.02 * (hi - lo) & b < hi - 0.02 * (hi - lo)]
}

scale_bar <- function(len_m = 500, label = "500 m") {
  w <- e$xmax - e$xmin; h <- e$ymax - e$ymin
  x0 <- e$xmin + 0.05 * w; y0 <- e$ymin + 0.06 * h
  list(
    annotate("segment", x = x0, xend = x0 + len_m, y = y0, yend = y0,
             colour = "black", linewidth = 0.5),
    annotate("segment", x = c(x0, x0 + len_m), xend = c(x0, x0 + len_m),
             y = y0, yend = y0 + 0.015 * h, colour = "black", linewidth = 0.5),
    annotate_text_pt(x0 + len_m / 2, y0 + 0.03 * h, label,
                     size_pt = 10, vjust = 0))
}

veg_panel <- function(r, tag) {
  ggplot() +
    geom_spatraster(data = r, maxcell = Inf) +
    veg_scale +
    scale_bar() +
    scale_x_continuous(breaks = inner_breaks(e$xmin, e$xmax)) +
    scale_y_continuous(breaks = inner_breaks(e$ymin, e$ymax)) +
    coord_sf(xlim = c(e$xmin, e$xmax), ylim = c(e$ymin, e$ymax),
             expand = FALSE, datum = sf::st_crs(v21)) +
    labs(x = LAB[["easting"]], y = LAB[["northing"]], tag = tag)
}

# The upper panel drops its x-axis title: the two panels are aspect-locked, so
# patchwork cannot collect the axis titles into one, and repeating "Easting (m)"
# between the maps only pushes them apart.
pa <- veg_panel(veg12, "a") + labs(x = NULL)
pb <- veg_panel(veg21, "b")

fig <- (pa / pb) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom", legend.box = "vertical")

# 140 mm wide; the extent is nearly square (1801 x 1753 cells at 1 m), so two
# stacked, aspect-locked panels plus the shared legend need most of a page.
save_figure(fig, file.path("paper", "files", "si", "figS6_vegmaps.png"),
            width_mm = 140, height_mm = 225, dpi = 300)

# ---- Reported numbers ----------------------------------------------------
# Class shares over the classified cells, printed so that the caption can be
# checked against the maps rather than written from memory.
shares <- function(r, year) {
  f <- freq(r)
  f$share <- 100 * f$count / sum(f$count)
  cat(sprintf("-- %s --\n", year))
  for (i in order(-f$share)) {
    cat(sprintf("  %-18s %9d  %5.1f%%\n", f$value[i], f$count[i], f$share[i]))
  }
}
shares(veg12, "2012")
shares(veg21, "2021")
