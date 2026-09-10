# Restored producing code for manuscript Figure 2 (files/2012_5x5_en.png, 2021_5x5_en.png).
# The archived scripts/sdm/plot_vegetation_map.R loads ggspatial but never calls it and
# writes Japanese labels; the version that made the published figure used English labels
# plus annotation_scale() + annotation_north_arrow(). This reconstructs that version.
# NOTE: class 5 is labelled "Erman's birch" here (ダケカンバ in the ground-truth label
# JSONs), not "Maple" as in the archived script and the manuscript.
suppressMessages({library(stars); library(tidyverse); library(ggspatial)})
D   <- "/Users/okamoto/NIES/SasaSDMPaper/data_from_server/ortho/data/"
OUT <- "/Users/okamoto/NIES/SasaSDMPaper/.claude/worktrees/review-summary-ece-ddc69c/review/restore/figures/"

cmap <- c("#2aa198", "#859900", "#dc322f", "#b58900", "#6c71c4", "#eee8d5", "#c0c0c0")
lv <- c("Dwarf Pine", "Dwarf Bamboo", "Rowans", "Maple", "Montane Alder",
        "Other Vegetation", "No Vegetation")

dem <- read_stars(paste0(D, "dem_small.tiff"))

draw <- function(tif, out) {
  ras <- read_stars(paste0(D, tif)) %>%
    setNames("vegetation") %>%
    mutate(vegetation = as.integer(vegetation)) %>%
    mutate(vegetation = case_when(
      vegetation == 1 ~ "Dwarf Bamboo",
      vegetation == 2 ~ "Other Vegetation",
      vegetation == 3 ~ "No Vegetation",
      vegetation == 4 ~ "Rowans",
      vegetation == 5 ~ "Maple",
      vegetation == 6 ~ "Montane Alder",
      vegetation == 7 ~ "Dwarf Pine"
    )) %>%
    mutate(vegetation = factor(vegetation, levels = lv))
  d <- dem %>% st_crop(ras) %>% setNames("Elevation")
  cont <- st_contour(d, contour_lines = TRUE, breaks = seq(2270, 3020, by = 20))
  p <- ggplot() +
    geom_sf(data = cont, size = 0.1) +
    geom_stars(mapping = aes(x = x, y = y, fill = vegetation), data = ras) +
    scale_fill_manual(values = cmap, na.value = "transparent") +
    annotation_scale(location = "bl", width_hint = 0.3, text_cex = 1.6) +
    annotation_north_arrow(location = "tl", style = north_arrow_minimal) +
    labs(x = "Longitude", y = "Latitude", fill = "Vegetation") +
    theme_minimal() +
    theme(axis.text = element_text(size = 18), text = element_text(size = 20),
          plot.background = element_rect(fill = "white"))
  ggsave(paste0(OUT, out), p, width = 12, height = 8)
  cat("wrote", out, "\n")
}
draw("vege_2012_5x5.tiff", "2012_5x5_en_restored.png")
draw("vege_2021_5x5.tiff", "2021_5x5_en_restored.png")
