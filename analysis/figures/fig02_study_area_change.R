# fig02_study_area_change.R -- Figure 2: study area and the nine-year change in
# Sasa distribution (2012 -> 2021).
#
# Message: "Sasa expanded along the margins of existing patches, and loss was
# concentrated at the dwarf-pine boundary."
#
#   (a) Context: 2021 vegetation over the whole analysis domain, with the core
#       map extent outlined and a small inset locating the site in Japan.
#   (b) Core extent: persisting / gained / lost Sasa cells over the pale
#       2021 dwarf-pine matrix, with the (c) and (d) zoom windows outlined.
#   (c) Zoom on the strongest expansion front.
#   (d) Zoom on the strongest Sasa -> dwarf-pine loss front.
#
# Output: paper/files/fig02_study_area_change.pdf / .png (190 mm wide).
# Run from the project root:  Rscript analysis/figures/fig02_study_area_change.R
# 2".

source(file.path("analysis", "figures", "fig_common.R"))

suppressPackageStartupMessages({
  library(tidyterra)
  library(patchwork)
  library(sf)
})

set.seed(20260830)

DEM_PATH <- file.path("ortho", "data", "mrd_dem_1m.tiff")
PREDICTORS <- file.path(DIR_ANALYSIS_OUT, "predictors.tif")

# ---- Palettes ------------------------------------------------------------
# (a) Seven vegetation classes: categorical, so Okabe-Ito based. Dwarf pine is
# the Okabe-Ito bluish green darkened to #00654A so that it separates from Sasa
# (vermillion) in greyscale as well as in colour; the two focal classes must be
# distinguishable under every check. The pale beige / grey for "other
# vegetation" and "no vegetation" follow the current vegemap.jpg.
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

# (b)-(d) Change classes. Green / orange / purple as in the current
# fig_sasa_change.png, taken from Okabe-Ito; the green is darkened from #009E73
# so that the three classes also separate in greyscale (see check at the end).
CHG_LEVELS <- c("Persisting", "Gained", "Lost", "Dwarf pine (2021)")
CHG_COLS <- c(
  "Persisting"        = "#007559",  # Okabe-Ito bluish green, darkened
  "Gained"            = "#E69F00",  # Okabe-Ito orange
  "Lost"              = "#CC79A7",  # Okabe-Ito reddish purple
  "Dwarf pine (2021)" = "#C6DCD3"   # pale context layer, never a focal class
)

# ---- Data ----------------------------------------------------------------
v12 <- rast(VEGE_2012)
v21 <- rast(VEGE_2021)
stopifnot(compareGeom(v12, v21, stopOnError = FALSE))

# Analysis domain = cells with a complete predictor stack (the same domain that
# Models A and B use); the manuscript's cell counts refer to this domain.
dom <- !is.na(sum(rast(PREDICTORS)))

s12 <- v12 == CLASS_SASA
s21 <- v21 == CLASS_SASA

n_persist <- global(s12 & s21 & dom, "sum", na.rm = TRUE)[[1]]
n_gained  <- global(!s12 & s21 & dom, "sum", na.rm = TRUE)[[1]]
n_lost    <- global(s12 & !s21 & dom, "sum", na.rm = TRUE)[[1]]
message(sprintf("cell counts | persisting %d | gained %d | lost %d",
                n_persist, n_gained, n_lost))

# Change raster, coded in legend order.
chg <- ifel(s12 & s21 & dom, 1,
       ifel(!s12 & s21 & dom, 2,
       ifel(s12 & !s21 & dom, 3,
       ifel(v21 == CLASS_PINE & dom, 4, NA))))
levels(chg) <- data.frame(value = 1:4, class = CHG_LEVELS)
names(chg) <- "class"

veg <- classify(v21, cbind(VEG_CODE, seq_along(VEG_CODE)), others = NA)
levels(veg) <- data.frame(value = seq_along(VEG_LEVELS), class = VEG_LEVELS)
names(veg) <- "class"

bb <- sasa_core_bbox()

# ---- Contours ------------------------------------------------------------
dem <- rast(DEM_PATH)
contour_sf <- function(e) {
  d <- crop(dem, e)
  rng <- unlist(global(d, range, na.rm = TRUE))
  lv <- seq(ceiling(rng[1] / CONTOUR_INTERVAL_M) * CONTOUR_INTERVAL_M,
            floor(rng[2] / CONTOUR_INTERVAL_M) * CONTOUR_INTERVAL_M,
            by = CONTOUR_INTERVAL_M)
  if (!length(lv)) return(NULL)
  st_as_sf(as.contour(d, levels = lv))
}
cont_full <- contour_sf(ext(v21))
cont_core <- contour_sf(bb)

# ---- Zoom-window selection ----------------------------------------------
# Both windows are chosen mechanically, not by eye: a 150 m x 150 m window is
# slid over the core extent on a 25 m grid and scored by cell count.
#   (c) maximum number of GAINED cells                  -> expansion front
#   (d) maximum number of Sasa(2012) -> dwarf pine(2021) cells, among windows
#       that do not overlap (c)                         -> loss at the pine edge
ZOOM_W <- 150
ZOOM_STEP <- 25

best_window <- function(r, exclude = NULL) {
  xs <- seq(bb$xmin, bb$xmax - ZOOM_W, by = ZOOM_STEP)
  ys <- seq(bb$ymin, bb$ymax - ZOOM_W, by = ZOOM_STEP)
  grid <- expand.grid(x = xs, y = ys)
  if (!is.null(exclude)) {
    keep <- grid$x + ZOOM_W <= exclude$xmin | grid$x >= exclude$xmax |
            grid$y + ZOOM_W <= exclude$ymin | grid$y >= exclude$ymax
    grid <- grid[keep, , drop = FALSE]
  }
  rc <- crop(r, bb)
  grid$n <- vapply(seq_len(nrow(grid)), function(i) {
    e <- ext(grid$x[i], grid$x[i] + ZOOM_W, grid$y[i], grid$y[i] + ZOOM_W)
    global(crop(rc, e), "sum", na.rm = TRUE)[[1]]
  }, numeric(1))
  grid[which.max(grid$n), ]
}

wc <- best_window(!s12 & s21 & dom)
zoom_c <- ext(wc$x, wc$x + ZOOM_W, wc$y, wc$y + ZOOM_W)
wd <- best_window(s12 & v21 == CLASS_PINE & dom,
                  exclude = list(xmin = wc$x, xmax = wc$x + ZOOM_W,
                                 ymin = wc$y, ymax = wc$y + ZOOM_W))
zoom_d <- ext(wd$x, wd$x + ZOOM_W, wd$y, wd$y + ZOOM_W)
message(sprintf("zoom c: E %.0f-%.0f N %.0f-%.0f (%d gained cells)",
                zoom_c$xmin, zoom_c$xmax, zoom_c$ymin, zoom_c$ymax, wc$n))
# Figs. 4 and 5 enlarge the same window (fig_common.R's ZOOM_WINDOW) so that the
# three figures' zoom panels show the same ground. Fail loudly if the mechanical
# search here ever moves, rather than letting the figures drift apart silently.
stopifnot(max(abs(c(zoom_c$xmin, zoom_c$xmax, zoom_c$ymin, zoom_c$ymax) -
                  unname(ZOOM_WINDOW[c("xmin", "xmax", "ymin", "ymax")]))) < 1e-6)
message(sprintf("zoom d: E %.0f-%.0f N %.0f-%.0f (%d Sasa->pine cells)",
                zoom_d$xmin, zoom_d$xmax, zoom_d$ymin, zoom_d$ymax, wd$n))

# ---- Map helpers ---------------------------------------------------------
map_theme <- function(p, e, n_breaks = 3) {
  # coord_sf() draws graticule labels, so the breaks must be supplied as
  # explicit numbers: a breaks *function* yields no labels under coord_sf().
  inner <- function(lo, hi) {
    b <- scales::breaks_pretty(n_breaks)(c(lo, hi))
    b[b > lo + 0.02 * (hi - lo) & b < hi - 0.02 * (hi - lo)]
  }
  p +
    scale_x_continuous(breaks = inner(e$xmin, e$xmax)) +
    scale_y_continuous(breaks = inner(e$ymin, e$ymax)) +
    coord_sf(xlim = c(e$xmin, e$xmax), ylim = c(e$ymin, e$ymax),
             expand = FALSE, datum = sf::st_crs(v21)) +
    labs(x = LAB[["easting"]], y = LAB[["northing"]])
}

# Plain scale bar drawn in map units (no dependency on ggspatial).
scale_bar <- function(e, len_m, label, frac_x = 0.06, frac_y = 0.06) {
  w <- e$xmax - e$xmin; h <- e$ymax - e$ymin
  x0 <- e$xmin + frac_x * w; y0 <- e$ymin + frac_y * h
  list(
    annotate("segment", x = x0, xend = x0 + len_m, y = y0, yend = y0,
             colour = "black", linewidth = 0.5),
    annotate("segment", x = c(x0, x0 + len_m), xend = c(x0, x0 + len_m),
             y = y0, yend = y0 + 0.015 * h, colour = "black", linewidth = 0.5),
    annotate_text_pt(x0 + len_m / 2, y0 + 0.035 * h, label,
                     size_pt = 10, vjust = 0)
  )
}

box_ann <- function(e, colour = "black", linewidth = 0.4, linetype = 1) {
  annotate("rect", xmin = e$xmin, xmax = e$xmax, ymin = e$ymin, ymax = e$ymax,
           fill = NA, colour = colour, linewidth = linewidth,
           linetype = linetype)
}

# ---- (a) Context map -----------------------------------------------------
# The core bbox reaches 144 m west of the vegetation raster, so panel (a) is
# drawn on the union of the two extents plus an 80 m pad. Without the pad the
# core-extent rectangle would fall on the panel border and read as three lines
# instead of a closed box.
ext_a <- local({
  u <- union(ext(v21), bb)
  ext(u$xmin - 80, u$xmax + 80, u$ymin - 80, u$ymax + 80)
})

# Locator inset goes in this window, verified to contain no vegetation cells
# (contours only), so it never covers mapped data.
INSET_BOX <- ext(732640, 733060, 4051360, 4051830)

pa <- ggplot() +
  geom_spatraster(data = veg, maxcell = Inf, alpha = 0.9) +
  scale_fill_manual(values = VEG_COLS, limits = VEG_LEVELS, drop = FALSE,
                    labels = c(expression(italic("Sasa")), VEG_LEVELS[-1]),
                    na.value = "transparent", na.translate = FALSE,
                    name = "2021 vegetation") +
  { if (!is.null(cont_full))
      geom_sf(data = cont_full, colour = CONTOUR_COLOUR,
              linewidth = CONTOUR_LINEWIDTH, inherit.aes = FALSE) } +
  box_ann(bb, linewidth = 0.5) +
  # Label sits just above the box so that it does not clash with the inset.
  annotate_text_pt(bb$xmin, bb$ymax + 30, "Core extent (b)",
                   size_pt = 10, hjust = 0, vjust = 0, fontface = "bold") +
  scale_bar(ext_a, 500, "500 m") +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE, order = 1,
                             override.aes = list(alpha = 1)))
pa <- map_theme(pa, ext_a)

# Locator inset: Honshu outline plus the study site. `maps` is used because
# rnaturalearth is not installed in this environment.
site_ll <- st_coordinates(st_transform(
  st_sfc(st_point(c(mean(c(bb$xmin, bb$xmax)), mean(c(bb$ymin, bb$ymax)))),
         crs = crs(v21)), 4326))
inset <- NULL
if (requireNamespace("maps", quietly = TRUE)) {
  jp <- st_as_sf(maps::map("world", "Japan", fill = TRUE, plot = FALSE))
  inset <- ggplot() +
    geom_sf(data = jp, fill = "grey92", colour = "grey55", linewidth = 0.15) +
    geom_point(aes(x = site_ll[1], y = site_ll[2]), shape = 21, size = 1.1,
               stroke = 0.4, fill = "#D55E00", colour = "black") +
    coord_sf(xlim = c(128, 146), ylim = c(30, 46), expand = FALSE) +
    theme_void() +
    theme(panel.background = element_rect(fill = "white", colour = "black",
                                          linewidth = 0.25),
          plot.margin = margin(0, 0, 0, 0))
} else {
  message("NOTE: package 'maps' unavailable -- locator inset omitted.")
}

# Convert INSET_BOX from map units to panel fractions.
inset_pos <- list(
  left   = (INSET_BOX$xmin - ext_a$xmin) / (ext_a$xmax - ext_a$xmin),
  right  = (INSET_BOX$xmax - ext_a$xmin) / (ext_a$xmax - ext_a$xmin),
  bottom = (INSET_BOX$ymin - ext_a$ymin) / (ext_a$ymax - ext_a$ymin),
  top    = (INSET_BOX$ymax - ext_a$ymin) / (ext_a$ymax - ext_a$ymin))

# ---- (b) Core-extent change map -----------------------------------------
# One shared scale object for (b), (c) and (d) so that patchwork's
# guides = "collect" recognises the three legends as identical and keeps one.
chg_scale <- scale_fill_manual(
  values = CHG_COLS, limits = CHG_LEVELS, drop = FALSE,
  na.value = "transparent", na.translate = FALSE,
  name = expression(italic("Sasa")~"2012"*"–"*"2021"),
  guide = guide_legend(nrow = 1, order = 2))

pb <- ggplot() +
  geom_spatraster(data = crop(chg, bb), maxcell = Inf) +
  chg_scale +
  { if (!is.null(cont_core))
      geom_sf(data = cont_core, colour = CONTOUR_COLOUR,
              linewidth = CONTOUR_LINEWIDTH, inherit.aes = FALSE) } +
  box_ann(zoom_c, linewidth = 0.4) +
  box_ann(zoom_d, linewidth = 0.4) +
  annotate_text_pt(zoom_c$xmin + 2, zoom_c$ymax + 12, "c",
                   size_pt = 11, hjust = 0, vjust = 0, fontface = "bold") +
  annotate_text_pt(zoom_d$xmin + 2, zoom_d$ymax + 12, "d",
                   size_pt = 11, hjust = 0, vjust = 0, fontface = "bold") +
  scale_bar(bb, 200, "200 m")
pb <- map_theme(pb, bb)

# ---- (c)(d) Zoom panels --------------------------------------------------
zoom_panel <- function(e) {
  cz <- contour_sf(e)
  p <- ggplot() +
    geom_spatraster(data = crop(chg, e), maxcell = Inf) +
    chg_scale +
    { if (!is.null(cz))
        geom_sf(data = cz, colour = CONTOUR_COLOUR,
                linewidth = CONTOUR_LINEWIDTH, inherit.aes = FALSE) } +
    scale_bar(e, 50, "50 m")
  map_theme(p, e, n_breaks = 3)
}
pc <- zoom_panel(zoom_c)
pd <- zoom_panel(zoom_d)

# ---- Assemble ------------------------------------------------------------
# Tags are set by hand rather than with tag_levels = "a": the locator inset
# makes panel (a) a nested patchwork, and automatic tagging then skips it.
pa <- pa + labs(tag = "a")
pa_full <- if (is.null(inset)) pa else
  pa + inset_element(inset, left = inset_pos$left, bottom = inset_pos$bottom,
                     right = inset_pos$right, top = inset_pos$top,
                     align_to = "panel", ignore_tag = TRUE)
pb <- pb + labs(tag = "b")
pc <- pc + labs(tag = "c")
pd <- pd + labs(tag = "d")

fig <- (pa_full | pb) / (pc | pd) +
  plot_layout(guides = "collect", heights = c(1, 1),
              widths = c(0.9, 1), axis_titles = "collect") &
  theme(legend.position = "bottom", legend.box = "vertical",
        legend.spacing.y = unit(1, "mm"))

# Height raised from 196 mm: the two-row legend and the axis labels grew with
# the type. The maps are aspect-locked by coord_sf(), so a much taller canvas
# only opens white space between the rows; 202 mm is the point where the legend
# and the axis titles clear each other without a gap appearing.
save_figure(fig, file.path(DIR_OUT_FIG, "fig02_study_area_change.pdf"),
            width_mm = W_2COL, height_mm = 182)
save_figure(fig, file.path(DIR_OUT_FIG, "fig02_study_area_change.png"),
            width_mm = W_2COL, height_mm = 182, dpi = 300)

# ---- Colour-vision / greyscale self-check --------------------------------
if (requireNamespace("colorspace", quietly = TRUE)) {
  message("-- vegetation classes --"); print(check_palette(VEG_COLS))
  message("-- change classes --");     print(check_palette(CHG_COLS))
}
