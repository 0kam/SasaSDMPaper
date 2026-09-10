# figS6_adjacency_map.R -- readable replacement for the Sasa->pine adjacency
# example map (SI). 150 m window around the densest Sasa->pine boundary, 1-m
# cells rendered large enough to read at 140 mm width.
# Output: paper/files/si/figS6_adjacency_map.png
source(file.path("analysis", "figures", "fig_common.R"))
suppressPackageStartupMessages({library(ggplot2)})

v12 <- rast(VEGE_2012); v21 <- rast(VEGE_2021)
# Search 150 m windows on a 5 m grid over the full classified extent;
# maximise the number of observed Sasa-to-pine transition cells.
WINDOW_M <- 150
transition <- ifel(v12 == CLASS_SASA & v21 == CLASS_PINE, 1, NA)
xy <- as.data.frame(transition, xy = TRUE)[, c("x", "y")]
candidates <- expand.grid(
  x = seq(ceiling(xmin(v21) / 5) * 5, floor((xmax(v21) - WINDOW_M) / 5) * 5, 5),
  y = seq(ceiling(ymin(v21) / 5) * 5, floor((ymax(v21) - WINDOW_M) / 5) * 5, 5))
counts <- vapply(seq_len(nrow(candidates)), function(i) {
  x <- candidates$x[i]; y <- candidates$y[i]
  sum(xy$x >= x & xy$x < x + WINDOW_M & xy$y >= y & xy$y < y + WINDOW_M)
}, integer(1))
best <- candidates[which.max(counts), ]
win <- ext(best$x, best$x + WINDOW_M, best$y, best$y + WINDOW_M)

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
  geom_tile(data = d[d$class == "Sasa (2012) to pine (2021)", ],
            width = res(v21)[1], height = res(v21)[2],
            colour = "grey15", linewidth = 0.16, show.legend = FALSE) +
  scale_fill_manual(values = pal, name = NULL,
                    guide = guide_legend(nrow = 2, byrow = TRUE)) +
  coord_sf(xlim = c(win$xmin, win$xmax), ylim = c(win$ymin, win$ymax),
           expand = FALSE, datum = sf::st_crs("EPSG:6690")) +
  scale_x_continuous(breaks = seq(win$xmin + 25, win$xmax - 25, 50)) +
  scale_y_continuous(breaks = seq(win$ymin + 25, win$ymax - 25, 50)) +
  annotate("rect", xmin = win$xmax - 65, xmax = win$xmax - 5,
           ymin = win$ymin + 5, ymax = win$ymin + 30,
           fill = "white", colour = "grey25", linewidth = 0.2) +
  annotate("segment", x = win$xmax - 60, xend = win$xmax - 10,
           y = win$ymin + 12, yend = win$ymin + 12, linewidth = 1) +
  annotate("text", x = win$xmax - 35, y = win$ymin + 22, label = "50 m",
           size = 10 / 2.845276) +
  labs(x = LAB[["easting"]], y = LAB[["northing"]]) +
  theme(axis.text = element_text(size = 11),
        legend.position = "top",
        legend.text = element_text(size = 11))
save_figure(p, file.path("paper", "files", "si", "figS6_adjacency_map.png"),
            width_mm = 140, height_mm = 150, dpi = 300)
stopifnot(sum(!is.na(values(lay$loss_pine))) == max(counts))
cat(sprintf("Window: %d x %d m; easting %.0f–%.0f; northing %.0f–%.0f\n",
            WINDOW_M, WINDOW_M, win$xmin, win$xmax, win$ymin, win$ymax))
cat("Transition cells:", nrow(d[d$class == "Sasa (2012) to pine (2021)", ]), "loss-to-pine in window\n")
