library(stars)
library(tidyverse)
library(ggspatial)

setwd("~/Projects/jasms2023f//")

cmap = c(
  "#2aa198", # Dwarf pine
  "#859900", # Dwarf bamboo
  "#dc322f", # Rowans
  "#b58900", # Maple
  "#6c71c4", # Montane Alder
  "#eee8d5", # Other vegetation
  "#c0c0c0" # No vegetation
)

vegetation_levels <- c(
  "ハイマツ",
  "ササ類",
  "ナナカマド類",
  "ミネカエデ",
  "ミヤマハンノキ",
  "その他植生",
  "無植生"
)


ras <- read_stars("ortho/data/vege_2012_5x5.tiff") %>%
  rename(vegetation = vege_2012_5x5.tiff) %>%
  mutate(vegetation = as.integer(vegetation)) %>%
  mutate(
    vegetation = case_when(
      vegetation == 1 ~ "ササ類",
      vegetation == 2 ~ "その他植生",
      vegetation == 3 ~ "無植生",
      vegetation == 4 ~ "ナナカマド類",
      vegetation == 5 ~ "ミネカエデ",
      vegetation == 6 ~ "ミヤマハンノキ",
      vegetation == 7 ~ "ハイマツ"
    )
  ) %>%  mutate(vegetation = factor(vegetation, levels = vegetation_levels))

dem <- read_stars("ortho/data/dem_small.tiff") %>%
  st_crop(ras) %>%
  rename(Elevation = dem_small.tiff)

cont <- st_contour(dem, contour_lines = T)

p1 <- ggplot() +
  geom_sf(data = cont, size = 0.1) +
  geom_stars(
    mapping = aes(x = x, y = y, fill = vegetation), 
    data = ras
  ) +
  scale_fill_manual(values = cmap, na.value = "transparent") +
  labs(x = "Longitude", y = "Latitude", fill = "Vegetation") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 18),
    text = element_text(size = 20),
    plot.background = element_rect(fill = "white")
  )

ggsave("ortho/data/2012_5x5.png", p1, width = 12, height = 8)


#-------------------------------------------------------------------------------
ras <- read_stars("ortho/data/vege_2021_5x5.tiff") %>%
  rename(vegetation = vege_2021_5x5.tiff) %>%
  mutate(vegetation = as.integer(vegetation)) %>%
  mutate(
    vegetation = case_when(
      vegetation == 1 ~ "ササ類",
      vegetation == 2 ~ "その他植生",
      vegetation == 3 ~ "無植生",
      vegetation == 4 ~ "ナナカマド類",
      vegetation == 5 ~ "ミネカエデ",
      vegetation == 6 ~ "ミヤマハンノキ",
      vegetation == 7 ~ "ハイマツ"
    )
  ) %>%
  mutate(vegetation = factor(vegetation, levels = vegetation_levels))

cont <- st_contour(dem, contour_lines = T)

p1 <- ggplot() +
  geom_sf(data = cont, size = 0.1) +
  geom_stars(
    mapping = aes(x = x, y = y, fill = vegetation), 
    data = ras
  ) +
  scale_fill_manual(values = cmap, na.value = "transparent") +
  labs(x = "Longitude", y = "Latitude", fill = "Vegetation") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 18),
    text = element_text(size = 20),
    plot.background = element_rect(fill = "white")
  )

ggsave("ortho/data/2021_5x5.png", p1, width = 12, height = 8)
