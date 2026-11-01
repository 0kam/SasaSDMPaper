# figS5_vegmaps.R -- Figure S5: seven-class vegetation maps for 2012 and 2021.
#
# Reviewer 1 asked for the two maps to be stacked vertically, to share a single
# legend, and to be enlarged. This script replaces the old side-by-side
# paper/files/vegemap.jpg with a two-row figure over the full analysis extent
# (not the Fig. 2 core window). No contours: at this extent they only add ink
# over the class colours.
#
# Output: paper/files/si/figS5_vegmaps.png (140 mm wide, 300 dpi).
# Run from the project root:  Rscript analysis/figures/figS5_vegmaps.R

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

sasa_patches <- function(r) {
  ids <- patches(ifel(r == CLASS_SASA, 1, NA), directions = 4, zeroAsNA = TRUE)
  polys <- as.polygons(ids, aggregate = TRUE, na.rm = TRUE)
  a <- as.numeric(expanse(polys, unit = "m", transform = TRUE))
  sf::st_as_sf(polys[a > 5, ])
}

veg_panel <- function(r, raw, tag) {
  sasa_cells <- as.data.frame(ifel(raw == CLASS_SASA, 1, NA), xy = TRUE)
  ggplot() +
    geom_spatraster(data = r, maxcell = Inf) +
    # Redraw focal patches above the raster, with the manuscript >5 m2 rule.
    geom_sf(data = sasa_patches(raw), fill = VEG_COLS[["Sasa"]],
            colour = "grey15", linewidth = 0.22, show.legend = FALSE) +
    geom_tile(data = sasa_cells, aes(x, y), width = res(raw)[1], height = res(raw)[2],
              fill = VEG_COLS[["Sasa"]], show.legend = FALSE) +
    veg_scale +
    map_scalebar_layers(e, 500) +
    coord_sf(xlim = c(e$xmin, e$xmax), ylim = c(e$ymin, e$ymax),
             expand = FALSE, datum = NA) +
    labs(tag = tag) + theme_map()
}

pa <- veg_panel(veg12, v12, "a") + north_arrow_layers(e)
pb <- veg_panel(veg21, v21, "b")

fig <- (pa / pb) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom", legend.box = "vertical",
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "mm"))

# 140 mm wide; the extent is nearly square (1801 x 1753 cells at 1 m), so two
# stacked, aspect-locked panels plus the shared legend need most of a page.
save_figure(fig, file.path("paper", "files", "si", "figS5_vegmaps.png"),
            width_mm = 140, height_mm = 190, dpi = 300)

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
