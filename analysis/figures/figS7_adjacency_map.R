# figS7_adjacency_map.R -- readable replacement for the Sasa->pine adjacency
# example map (SI). 300 m window around the densest Sasa->pine boundary, 1-m
# cells rendered large enough to read at 140 mm width.
# Output: paper/files/si/figS7_adjacency_map.png
source(file.path("analysis", "figures", "fig_common.R"))
suppressPackageStartupMessages({library(ggplot2)})

v12 <- rast(VEGE_2012); v21 <- rast(VEGE_2021)
win <- ext(732950, 733250, 4050950, 4051250)  # 300 m window, fig. 2(d) vicinity

cls <- function(r, code) ifel(r == code, 1, NA)
lay <- c(
  pine21  = crop(cls(v21, CLASS_PINE), win),
  sasa_keep = crop(ifel(v12 == CLASS_SASA & v21 == CLASS_SASA, 1, NA), win),
  loss_pine = crop(ifel(v12 == CLASS_SASA & v21 == CLASS_PINE, 1, NA), win)
)
to_df <- function(r, label) {
  d <- as.data.frame(r, xy = TRUE)
  if (nrow(d)) d$class <- label
  d[, c("x", "y", "class")]
}
d <- rbind(to_df(lay$pine21, "Dwarf pine (2021)"),
           to_df(lay$sasa_keep, "Sasa (2012 & 2021)"),
           to_df(lay$loss_pine, "Sasa (2012) to pine (2021)"))
d$class <- factor(d$class, levels = c("Dwarf pine (2021)", "Sasa (2012 & 2021)",
                                      "Sasa (2012) to pine (2021)"))
pal <- c("Dwarf pine (2021)" = "#C6DCD3",
         "Sasa (2012 & 2021)" = "#007559",
         "Sasa (2012) to pine (2021)" = "#CC79A7")

p <- ggplot(d, aes(x, y, fill = class)) +
  geom_raster() +
  scale_fill_manual(values = pal, name = NULL,
                    guide = guide_legend(nrow = 2, byrow = TRUE)) +
  coord_sf(xlim = c(win$xmin, win$xmax), ylim = c(win$ymin, win$ymax),
           expand = FALSE, datum = sf::st_crs("EPSG:6690")) +
  scale_x_continuous(breaks = seq(733000, 733200, 100)) +
  scale_y_continuous(breaks = seq(4051000, 4051200, 100)) +
  annotate("segment", x = win$xmax - 60, xend = win$xmax - 10,
           y = win$ymin + 12, yend = win$ymin + 12, linewidth = 1) +
  annotate("text", x = win$xmax - 35, y = win$ymin + 22, label = "50 m",
           size = 10 / 2.845276) +
  labs(x = LAB[["easting"]], y = LAB[["northing"]]) +
  theme(axis.text = element_text(size = 11),
        legend.position = "top",
        legend.text = element_text(size = 11))
save_figure(p, file.path("paper", "files", "si", "figS7_adjacency_map.png"),
            width_mm = 140, height_mm = 150, dpi = 300)
cat("cells:", nrow(d[d$class == "Sasa (2012) to pine (2021)", ]), "loss-to-pine in window\n")
