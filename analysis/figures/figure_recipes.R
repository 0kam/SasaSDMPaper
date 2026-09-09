# figure-recipes.R -- Ready-to-use helpers for journal-quality ggplot2 figures.
#
# USAGE
#   source("references/figure-recipes.R")
#   theme_set(theme_paper(base_size = 8))
#   p <- ggplot(...) + scale_colour_okabeito()
#   save_figure(p, "figures/fig1.pdf", width_mm = W_1COL, height_mm = 70)
#
# REQUIRED packages : ggplot2, ragg (only for TIFF/PNG output)
# OPTIONAL packages : patchwork, scico, viridisLite, colorspace, grDevices (cairo_pdf)
#
# All helpers are written with base implementations so that no extra colour
# package (khroma, ggokabeito, see) is needed. Every function used here exists in
# ggplot2 / grDevices / ragg; nothing is invented.
#
# Companion checklist: figure-rules.md

library(ggplot2)

# ---------------------------------------------------------------------------
# 1. Journal column widths (mm). Elsevier: 90 / 140 / 190 mm. Wiley: 80-180 mm.
#    https://www.elsevier.com/about/policies-and-standards/author/artwork-and-media-instructions/artwork-sizing
# ---------------------------------------------------------------------------

W_1COL   <- 90    # single column
W_1_5COL <- 140   # 1.5 column
W_2COL   <- 190   # full width (double column)

# ---------------------------------------------------------------------------
# 2. theme_paper() -- base theme for manuscript figures.
#    base_size is in POINTS. Use 7-9 for 90 mm figures; never leave it at the
#    ggplot2 default of 11 or the cowplot default of 14.
#    style = "classic" -> axis lines, no grid (scatter / line plots)
#    style = "box"     -> panel border, no grid (maps, rasters, facets)
# ---------------------------------------------------------------------------

theme_paper <- function(base_size = 8,
                        base_family = "",
                        style = c("classic", "box"),
                        legend_position = "top") {
  style <- match.arg(style)
  lw <- 0.25  # mm; ~0.7 pt line weight for axes and ticks

  base <- if (style == "classic") {
    theme_classic(base_size = base_size, base_family = base_family)
  } else {
    theme_bw(base_size = base_size, base_family = base_family)
  }

  out <- base +
    theme(
      text             = element_text(colour = "black"),
      axis.text        = element_text(size = rel(0.9), colour = "black"),
      axis.title       = element_text(size = rel(1.0), colour = "black"),
      axis.ticks       = element_line(colour = "black", linewidth = lw),
      legend.position  = legend_position,
      legend.key.size  = unit(3, "mm"),
      legend.title     = element_text(size = rel(0.9)),
      legend.text      = element_text(size = rel(0.9)),
      legend.margin    = margin(0, 0, 0, 0),
      strip.background = element_blank(),
      strip.text       = element_text(size = rel(1.0), hjust = 0, face = "bold"),
      plot.title       = element_text(size = rel(1.0), face = "plain"),
      plot.margin      = margin(2, 2, 2, 2, "mm"),
      plot.tag         = element_text(size = rel(1.1), face = "bold")
    )

  if (style == "classic") {
    out <- out + theme(axis.line = element_line(colour = "black", linewidth = lw))
  } else {
    out <- out + theme(
      panel.border = element_rect(fill = NA, colour = "black", linewidth = lw),
      panel.grid   = element_blank()
    )
  }
  out
}

# ---------------------------------------------------------------------------
# 3. Okabe-Ito categorical palette (colour-vision-deficiency safe).
#    Values verified with grDevices::palette.colors(9, "Okabe-Ito").
#    Order below puts black last so that the first colours are the most distinct.
# ---------------------------------------------------------------------------

okabeito_colours <- c(
  orange         = "#E69F00",
  skyblue        = "#56B4E9",
  bluishgreen    = "#009E73",
  yellow         = "#F0E442",
  blue           = "#0072B2",
  vermillion     = "#D55E00",
  reddishpurple  = "#CC79A7",
  grey           = "#999999",
  black          = "#000000"
)

# Return n Okabe-Ito colours. Errors above 9 categories on purpose: more than
# nine categories cannot be distinguished by colour alone (see figure-rules.md).
okabeito_pal <- function(n = 9, order = NULL) {
  cols <- unname(okabeito_colours)
  if (!is.null(order)) cols <- cols[order]
  if (n > length(cols)) {
    stop("Okabe-Ito provides at most 9 colours; recode categories or encode ",
         "them with shape/linetype instead of colour.", call. = FALSE)
  }
  cols[seq_len(n)]
}

scale_colour_okabeito <- function(..., order = NULL, aesthetics = "colour") {
  pal <- function(n) okabeito_pal(n, order = order)
  # discrete_scale() dropped the mandatory `scale_name` argument in ggplot2 3.5.0.
  if (utils::packageVersion("ggplot2") >= "3.5.0") {
    discrete_scale(aesthetics = aesthetics, palette = pal, ...)
  } else {
    discrete_scale(aesthetics = aesthetics, scale_name = "okabeito",
                   palette = pal, ...)
  }
}
scale_color_okabeito <- scale_colour_okabeito

scale_fill_okabeito <- function(..., order = NULL) {
  scale_colour_okabeito(..., order = order, aesthetics = "fill")
}

# ---------------------------------------------------------------------------
# 4. Point-size helpers for geom_text() / annotate().
#    TRAP: the `size` aesthetic of geom_text()/geom_label() and `linewidth` are
#    in MILLIMETRES, not points. size = 5 renders at about 14.2 pt.
#    ggplot2 >= 3.5.0 accepts size.unit = "pt"; older versions need pt_to_mm().
# ---------------------------------------------------------------------------

PT_PER_MM <- 72.27 / 25.4   # 2.845276 -- this is ggplot2's internal .pt

pt_to_mm <- function(pt) pt / PT_PER_MM   # e.g. pt_to_mm(7) -> 2.46

has_size_unit <- function() {
  utils::packageVersion("ggplot2") >= "3.5.0"
}

# Version-safe geom_text() wrapper: size_pt is always in points.
geom_text_pt <- function(..., size_pt = 7) {
  if (has_size_unit()) {
    geom_text(..., size = size_pt, size.unit = "pt")
  } else {
    geom_text(..., size = pt_to_mm(size_pt))
  }
}

annotate_text_pt <- function(x, y, label, size_pt = 7, ...) {
  if (has_size_unit()) {
    annotate("text", x = x, y = y, label = label,
             size = size_pt, size.unit = "pt", ...)
  } else {
    annotate("text", x = x, y = y, label = label,
             size = pt_to_mm(size_pt), ...)
  }
}

# ---------------------------------------------------------------------------
# 5. save_figure() -- ggsave wrapper enforcing real-world size in mm.
#    .pdf  -> grDevices::cairo_pdf (embeds fonts; plain pdf() does not)
#    .tif  -> ragg::agg_tiff with LZW compression (agg default is "none")
#    .png  -> ragg::agg_png
#    dpi defaults: 1000 for line art, use 500 for combination art and 300 for
#    halftone (Elsevier). Vector PDF ignores dpi except for rasterised layers.
# ---------------------------------------------------------------------------

save_figure <- function(plot, filename,
                        width_mm, height_mm,
                        dpi = 1000,
                        bg = "white",
                        ...) {
  ext <- tolower(tools::file_ext(filename))
  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)

  if (ext == "pdf") {
    ggsave(filename, plot, device = grDevices::cairo_pdf,
           width = width_mm, height = height_mm, units = "mm",
           bg = bg, ...)
  } else if (ext %in% c("tif", "tiff")) {
    if (!requireNamespace("ragg", quietly = TRUE)) {
      stop("Package 'ragg' is required for TIFF output.", call. = FALSE)
    }
    ggsave(filename, plot, device = ragg::agg_tiff,
           width = width_mm, height = height_mm, units = "mm",
           dpi = dpi, bg = bg, compression = "lzw", ...)
  } else if (ext == "png") {
    if (!requireNamespace("ragg", quietly = TRUE)) {
      stop("Package 'ragg' is required for PNG output.", call. = FALSE)
    }
    ggsave(filename, plot, device = ragg::agg_png,
           width = width_mm, height = height_mm, units = "mm",
           dpi = dpi, bg = bg, ...)
  } else {
    stop("Use .pdf (vector), .tif/.tiff or .png. JPEG must not be used for ",
         "line art or text.", call. = FALSE)
  }
  invisible(normalizePath(filename))
}

# ---------------------------------------------------------------------------
# 6. Colour scale usage -- pick by DATA TYPE, not by taste.
# ---------------------------------------------------------------------------

# 6a. Sequential continuous (NDVI, LST, backscatter, acoustic index, spectrogram)
#     ggplot2 ships viridis; scico gives the Crameri scientific colour maps.
#     Run scico::scico_palette_names() to list the palettes in your install.
#     p + scale_fill_viridis_c(option = "viridis", name = "NDVI")
#     p + scico::scale_fill_scico(palette = "batlow", name = "NDVI")

# 6b. Diverging (change detection, anomaly, difference -- zero is meaningful)
#     NEVER use viridis here: it has no midpoint.
#     p + scico::scale_fill_scico(palette = "vik", midpoint = 0,
#                                 name = expression(Delta*"NDVI"))
#     p + scale_fill_distiller(palette = "RdBu", limits = c(-1, 1))

# 6c. Elevation / bathymetry
#     p + scico::scale_fill_scico(palette = "oleron", midpoint = 0)

# 6d. Categorical (sites, species, land-cover classes, treatments)
#     p + scale_colour_okabeito() + scale_shape_manual(values = c(16, 17, 15))
#     Redundant coding with shape or linetype is required, not optional.

# ---------------------------------------------------------------------------
# 7. Colour-vision and black-and-white checks (colorspace).
#    Run this on every figure before submission.
# ---------------------------------------------------------------------------

check_palette <- function(cols) {
  if (!requireNamespace("colorspace", quietly = TRUE)) {
    stop("Package 'colorspace' is required.", call. = FALSE)
  }
  data.frame(
    original  = cols,
    deutan    = colorspace::deutan(cols),
    protan    = colorspace::protan(cols),
    tritan    = colorspace::tritan(cols),
    greyscale = colorspace::desaturate(cols),
    stringsAsFactors = FALSE
  )
}
# check_palette(okabeito_pal(5))
# colorspace::swatchplot(list(normal = okabeito_pal(5),
#                             deutan = colorspace::deutan(okabeito_pal(5)),
#                             grey   = colorspace::desaturate(okabeito_pal(5))))

# ---------------------------------------------------------------------------
# 8. Multi-panel figures with patchwork.
#    guides = "collect" merges duplicate legends; axis_titles = "collect"
#    removes repeated axis titles. Tags are lower-case a, b, c.
# ---------------------------------------------------------------------------

# library(patchwork)
# fig <- (p1 | p2) / p3 +
#   patchwork::plot_layout(guides = "collect", axis_titles = "collect",
#                          heights = c(1, 1.2)) +
#   patchwork::plot_annotation(tag_levels = "a") &
#   theme(legend.position = "bottom")
# save_figure(fig, "figures/fig2.pdf", width_mm = W_2COL, height_mm = 140)

# ---------------------------------------------------------------------------
# 9. Minimal end-to-end example.
# ---------------------------------------------------------------------------

if (identical(Sys.getenv("FIGURE_RECIPES_DEMO"), "1")) {
  set.seed(1)  # always seed before jitter / bootstrap / random label placement

  d <- data.frame(
    ndvi = runif(120, 0.2, 0.9),
    site = factor(rep(c("Forest", "Plantation", "Grassland"), each = 40))
  )
  d$adi <- 1.5 + 2.0 * d$ndvi + rnorm(120, 0, 0.2)

  p <- ggplot(d, aes(ndvi, adi, colour = site, shape = site)) +
    geom_point(size = 1.2, alpha = 0.8) +
    geom_smooth(method = "lm", formula = y ~ x, se = TRUE, linewidth = 0.4) +
    scale_colour_okabeito(name = NULL) +
    scale_shape_manual(values = c(16, 17, 15), name = NULL) +
    labs(x = "NDVI (-)", y = expression("Acoustic diversity index"~(bits))) +
    annotate_text_pt(0.25, 3.4, "n = 120", size_pt = 7, hjust = 0) +
    theme_paper(base_size = 8)

  save_figure(p, "figures/demo.pdf", width_mm = W_1COL, height_mm = 70)
}
