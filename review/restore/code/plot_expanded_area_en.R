# Scripted replacement for manuscript Figure 4 (files/expanded_area.pdf), which was a
# hand-made QGIS/GSI composite with Japanese place names, callout boxes and an inset.
# Data layer verified: sasa_inc.tiff == (vege_2021==1 & vege_2012!=1), 4,097 px, 100% match.
suppressPackageStartupMessages({library(terra); library(stars); library(ggplot2); library(sf); library(ggspatial); library(tidyterra)})
D   <- "/Users/okamoto/NIES/SasaSDMPaper/data_from_server/ortho/data/"
OUT <- "/Users/okamoto/NIES/SasaSDMPaper/.claude/worktrees/review-summary-ece-ddc69c/review/restore/figures/"

v12 <- rast(paste0(D,"vege_2012_5x5.tiff")); v21 <- rast(paste0(D,"vege_2021_5x5.tiff"))
st  <- ifel(v12==1 & v21==1, 1, ifel(v12!=1 & v21==1, 2, ifel(v12==1 & v21!=1, 3, NA)))
names(st) <- "status"
cat("cell counts  persisted / gained / lost:\n"); print(table(values(st), useNA="no"))
cat("geodesic areas (m2):\n")
for (k in 1:3) cat("  class", k, ":", round(expanse(ifel(st==k,1,NA), unit="m")[,2]), "\n")

dem  <- read_stars(paste0(D,"dem_small.tiff"))
demc <- st_crop(dem, st_bbox(st_as_stars(st)))
cont <- st_contour(demc, contour_lines = TRUE, breaks = seq(2200, 3050, by = 25))

stf <- as.factor(st)
levels(stf) <- data.frame(ID=1:3, status=c("Sasa in 2012 and 2021","Expansion (gained 2012-2021)","Loss (2012 only)"))

p <- ggplot() +
  geom_sf(data = cont, colour = "grey70", linewidth = 0.15) +
  geom_spatraster(data = stf) +
  scale_fill_manual(values = c("#2e7d32", "#f2c200", "#7e57c2"), na.value = "transparent",
                    na.translate = FALSE, name = NULL) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "tl", style = north_arrow_minimal) +
  labs(x = "Longitude", y = "Latitude") +
  theme_minimal(base_size = 16) +
  theme(legend.position = "bottom", plot.background = element_rect(fill="white", colour=NA))
ggsave(paste0(OUT,"fig4_expanded_area_RESCRIPTED.png"), p, width = 10, height = 9, dpi = 400)
cat("wrote fig4_expanded_area_RESCRIPTED.png\n")
