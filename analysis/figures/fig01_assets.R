# fig01_assets.R -- raster/schematic assets embedded in Fig. 1 (workflow +
# model concept figure). The figure itself is assembled in PowerPoint
# (paper/files/SasaPaper_Figures_v2.pptx); this script only produces the
# bitmaps that are placed into panels (b), (c) and (d).
#
# Run from the project root:
#   Rscript analysis/figures/fig01_assets.R
#
# Outputs (paper/files/fig01_assets/):
#   thumb_suitability.png  -- panel (b) output: Model A habitat suitability
#   schematic_modelB.png   -- panel (c): Model B concept grid
#   thumb_pcol2030.png     -- panel (d) output: CA P(colonized by 2030), s = 0
#
# 1".

source(file.path("analysis", "figures", "fig_common.R"))

suppressPackageStartupMessages({
  library(ggplot2)
  library(tidyterra)
  library(scales)
})

DIR_ASSETS <- file.path("paper", "files", "fig01_assets")
dir.create(DIR_ASSETS, showWarnings = FALSE, recursive = TRUE)

set.seed(20260830)

# Thumbnails are drawn at their final printed size (Fig. 1 is 190 mm wide and
# the PowerPoint slide is 338.7 mm wide, so slide EMU are scaled by ~0.561).
THUMB_W_MM <- 40
SCHEM_W_MM <- 54   # printed width of the Model B schematic in Fig. 1
DPI <- 600

mm_to_px <- function(mm) round(mm / 25.4 * DPI)

# Bare thumbnail: no axes, no legend, no margin -- the raster fills the frame.
theme_thumb <- function() {
  theme_void() +
    theme(legend.position = "none",
          plot.margin = margin(0, 0, 0, 0),
          panel.border = element_rect(colour = "grey30", fill = NA,
                                      linewidth = 0.4))
}

save_thumb <- function(p, path, width_mm, height_mm) {
  ragg::agg_png(path, width = mm_to_px(width_mm), height = mm_to_px(height_mm),
                units = "px", res = DPI, background = "white")
  on.exit(grDevices::dev.off(), add = TRUE)
  print(p)
  invisible(path)
}

# ---- Shared crop ----------------------------------------------------------
core <- sasa_core_bbox()
core_w <- as.numeric(core$xmax - core$xmin)
core_h <- as.numeric(core$ymax - core$ymin)
message(sprintf("core bbox: x %.0f-%.0f  y %.0f-%.0f  (%.0f x %.0f m, aspect %.3f)",
                core$xmin, core$xmax, core$ymin, core$ymax,
                core_w, core_h, core_w / core_h))

# Thumbnail height follows the crop aspect so the map fills the frame exactly.
thumb_h_mm <- THUMB_W_MM * core_h / core_w

crop_core <- function(path) {
  r <- terra::rast(path)
  terra::crop(r, core)
}

# ---- (b) Model A habitat suitability --------------------------------------
suit <- crop_core(file.path(DIR_ANALYSIS_OUT, "suitability_A_primary.tif"))
names(suit) <- "value"

p_suit <- ggplot() +
  geom_spatraster(data = suit, maxcell = Inf) +
  scale_fill_viridis_c(na.value = "transparent") +
  coord_sf(expand = FALSE) +
  theme_thumb()

save_thumb(p_suit, file.path(DIR_ASSETS, "thumb_suitability.png"),
           THUMB_W_MM, thumb_h_mm)

# ---- (d) Cellular-automaton output ----------------------------------------
# Same scale as Fig. 5(a) / Fig. 4(b): rocket reversed with the fourth-root
# stretch, so the thumbnail reads as the same quantity as the full figure.
PROB_LIM <- c(0, 0.7)
root4 <- if (utils::packageVersion("scales") >= "1.3.0") {
  scales::new_transform("root4", function(x) x^0.25, function(x) x^4)
} else {
  scales::trans_new("root4", function(x) x^0.25, function(x) x^4)
}

pcol <- crop_core(file.path(DIR_ANALYSIS_OUT, "ca_pcol_2030_s0.tif"))
names(pcol) <- "value"

p_pcol <- ggplot() +
  geom_spatraster(data = pcol, maxcell = Inf) +
  scale_fill_viridis_c(option = "rocket", direction = -1,
                       na.value = "transparent", limits = PROB_LIM,
                       transform = root4, oob = scales::squish) +
  coord_sf(expand = FALSE) +
  theme_thumb()

save_thumb(p_pcol, file.path(DIR_ASSETS, "thumb_pcol2030.png"),
           THUMB_W_MM, thumb_h_mm)

# ---- (c) Model B concept grid ---------------------------------------------
# Illustrative only: a 15 x 12 lattice with a 2012 Sasa patch, the distance
# field it generates, and the handful of cells that establish by 2021 within
# one or two cells of the front. The numbers are invented; the point is that
# establishment hugs the front.
NX <- 15; NY <- 12
grid <- expand.grid(x = seq_len(NX), y = seq_len(NY))

patch <- rbind(
  expand.grid(x = 2:4, y = 3:8),
  data.frame(x = c(5, 5, 5, 6, 6, 1, 1), y = c(4, 5, 6, 5, 6, 5, 6))
)
patch_key <- paste(patch$x, patch$y)
grid$patch <- paste(grid$x, grid$y) %in% patch_key

# Euclidean distance (in cells) to the nearest patch cell.
grid$dist <- apply(grid[, c("x", "y")], 1, function(p) {
  if (paste(p[1], p[2]) %in% patch_key) return(0)
  min(sqrt((patch$x - p[1])^2 + (patch$y - p[2])^2))
})

# Establishment concentrated at 1-2 cells from the front.
colonized <- data.frame(
  x = c(6, 7, 6, 5, 4, 7, 2, 3),
  y = c(4, 5, 7, 8, 9, 6, 2, 9)
)
grid$colonized <- paste(grid$x, grid$y) %in% paste(colonized$x, colonized$y)
stopifnot(all(grid$dist[grid$colonized] <= 2.1))

COL_PATCH <- "#1B5E20"   # 2012 patch
COL_COL   <- "#E69F00"   # colonized by 2021 (Okabe-Ito orange)

# A key on the RIGHT of the lattice, not leader lines: the schematic sits in a
# panel that is much wider than it is tall, so a landscape layout lets the 7 pt
# key text stay 7 pt at the printed size instead of being scaled away.
KEY_X    <- 16.8   # swatch centre
KEY_TEXT <- 17.7   # left edge of the key text
X_MAX    <- 24.0   # right edge of the drawing area

key <- data.frame(
  y      = c(9.5, 6.5, 3.5),
  fill   = c(COL_PATCH, COL_COL, "#c7d5e2"),
  label  = c("2012\npatch", "colonized\nby 2021", "distance\nto front"),
  colour = c(COL_PATCH, COL_COL, "grey15")
)

p_schem <- ggplot(grid, aes(x, y)) +
  geom_tile(aes(fill = dist), colour = "white", linewidth = 0.15) +
  scale_fill_gradient(low = "#b9cadb", high = "#f6f9fb", guide = "none") +
  geom_tile(data = subset(grid, patch), fill = COL_PATCH,
            colour = "white", linewidth = 0.15) +
  geom_tile(data = subset(grid, colonized), fill = COL_COL,
            colour = "white", linewidth = 0.15) +
  annotate("tile", x = KEY_X, y = key$y, width = 1.1, height = 1.1,
           fill = key$fill, colour = "grey60", linewidth = 0.15) +
  annotate("text", x = KEY_TEXT, y = key$y, label = key$label,
           colour = key$colour, hjust = 0, vjust = 0.5, lineheight = 0.95,
           fontface = "bold", size = 7, size.unit = "pt") +
  coord_fixed(xlim = c(0.5, X_MAX), ylim = c(0.5, NY + 0.5), expand = FALSE,
              clip = "off") +
  theme_void() +
  theme(legend.position = "none", plot.margin = margin(0.5, 0.5, 0.5, 0.5, "mm"))

# Printed size of this asset in Fig. 1 (see analysis/figures/fig01_build_pptx.py).
schem_h_mm <- SCHEM_W_MM * (NY) / (X_MAX - 0.5)
save_thumb(p_schem, file.path(DIR_ASSETS, "schematic_modelB.png"),
           SCHEM_W_MM, schem_h_mm)

for (f in c("thumb_suitability.png", "thumb_pcol2030.png",
            "schematic_modelB.png")) {
  p <- file.path(DIR_ASSETS, f)
  d <- dim(png::readPNG(p))
  message(sprintf("%-24s %d x %d px", f, d[2], d[1]))
}
