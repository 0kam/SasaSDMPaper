# figS_maps.R -- two supplementary maps:
#   S2: mean snowmelt date (2011-2021 climatology) on the core extent
#   S7: seed-dispersal risk stratum = Model A suitable area (TSS-max threshold)
#       within cells classified as other vegetation in 2021
# Outputs: paper/files/si/figS2_snowmelt_mean_map.png, figS7_seed_layer.png
source(file.path("analysis", "figures", "fig_common.R"))
suppressPackageStartupMessages({library(ggplot2)})

bb <- sasa_core_bbox()
contours <- local({
  dem <- rast(file.path("ortho", "data", "terrain_features", "tateyamadem_small.tif"))
  dem <- project(dem, "EPSG:6690")
  dem <- crop(dem, ext(bb$xmin - 100, bb$xmax + 100, bb$ymin - 100, bb$ymax + 100))
  rng <- minmax(dem)
  lv <- seq(ceiling(rng[1] / CONTOUR_INTERVAL_M) * CONTOUR_INTERVAL_M,
            floor(rng[2] / CONTOUR_INTERVAL_M) * CONTOUR_INTERVAL_M,
            CONTOUR_INTERVAL_M)
  as.contour(dem, levels = lv)
})
sasa21 <- {
  v <- rast(VEGE_2021); s <- ifel(v == CLASS_SASA, 1, NA)
  ids <- patches(s, directions = 4, zeroAsNA = TRUE)
  p <- as.polygons(ids, aggregate = TRUE, na.rm = TRUE)
  p$a <- as.numeric(expanse(p, unit = "m", transform = TRUE))
  aggregate(p[p$a > 5, ])
}

map_theme <- theme(axis.text = element_text(size = 11),
                   legend.position = "top",
                   legend.title.position = "top",
                   legend.title = element_text(margin = margin(b = 2, unit = "mm")),
                   plot.margin = margin(2, 8, 2, 2, "mm"))
xb <- seq(732800, 733600, 400); yb <- seq(4050800, 4051800, 400)

# ---- S2: mean snowmelt date ----------------------------------------------
sm <- crop(rast(file.path(DIR_ANALYSIS_OUT, "predictors.tif"))[["snow_mean"]], bb)
d <- as.data.frame(sm, xy = TRUE, na.rm = FALSE); names(d)[3] <- "doy"
p2 <- ggplot() +
  geom_raster(data = d, aes(x, y, fill = doy)) +
  geom_sf(data = sf::st_as_sf(contours), colour = CONTOUR_COLOUR,
          linewidth = CONTOUR_LINEWIDTH) +
  geom_sf(data = sf::st_as_sf(sasa21), fill = NA,
          colour = SASA_OUTLINE_COLOUR, linewidth = SASA_OUTLINE_LINEWIDTH) +
  scale_fill_viridis_c(name = "Mean snowmelt date (DOY)", limits = c(120, 230),
                       breaks = c(120, 160, 200),
                       guide = guide_colourbar(barwidth = grid::unit(50, "mm"))) +
  coord_sf(xlim = c(bb$xmin, bb$xmax), ylim = c(bb$ymin, bb$ymax), expand = FALSE, datum = sf::st_crs("EPSG:6690")) +
  scale_x_continuous(breaks = xb) + scale_y_continuous(breaks = yb) +
  labs(x = LAB["easting"], y = LAB["northing"]) + map_theme
save_figure(p2, file.path("paper", "files", "si", "figS2_snowmelt_mean_map.png"),
            width_mm = 140, height_mm = 130, dpi = 300)

# ---- S7: seed-dispersal stratum ------------------------------------------
thr <- read.csv(file.path(DIR_ANALYSIS_OUT, "model_A_thresholds.csv"))
thr <- thr$threshold[thr$variant == "primary"]
suit <- rast(file.path(DIR_ANALYSIS_OUT, "suitability_A_primary.tif"))
v21 <- rast(VEGE_2021)
seed <- ifel(v21 == CLASS_OTHERVEG & suit >= thr, 1, NA)
d5 <- as.data.frame(crop(seed, bb), xy = TRUE)
p5 <- ggplot() +
  geom_sf(data = sf::st_as_sf(contours), colour = CONTOUR_COLOUR,
          linewidth = CONTOUR_LINEWIDTH) +
  geom_tile(data = d5, aes(x, y), width = res(seed)[1], height = res(seed)[2],
            fill = okabeito_colours[["orange"]]) +
  geom_sf(data = sf::st_as_sf(sasa21), fill = NA,
          colour = SASA_OUTLINE_COLOUR, linewidth = SASA_OUTLINE_LINEWIDTH) +
  coord_sf(xlim = c(bb$xmin, bb$xmax), ylim = c(bb$ymin, bb$ymax), expand = FALSE, datum = sf::st_crs("EPSG:6690")) +
  scale_x_continuous(breaks = xb) + scale_y_continuous(breaks = yb) +
  labs(x = LAB["easting"], y = LAB["northing"]) + map_theme
save_figure(p5, file.path("paper", "files", "si", "figS7_seed_layer.png"),
            width_mm = 140, height_mm = 130, dpi = 300)
area_m2 <- sum(!is.na(values(seed)))
cat("Seed-layer cells (nominal m2):", area_m2, "\n")
