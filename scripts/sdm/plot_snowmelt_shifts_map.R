library(stars)
library(tidyverse)

snow_2012 <- read_stars("data/snow/fitted_2012.tiff")
snow_2021 <- read_stars("data/snow/fitted_2021.tiff") %>%
  st_warp(snow_2012)
snow_2030 <- read_stars("data/snow/fitted_2030.tiff") %>%
  st_warp(snow_2012)

snow <- c(snow_2012, snow_2021, snow_2030, along = "year") |>
  st_set_dimensions("year", values = c(2012,2021,2030)) |>
  st_warp(cellsize = 2, crs = st_crs(snow_2012)) |>
  st_transform(4326)

ggplot() +
  geom_stars(data = snow) +
  facet_wrap(~year) + 
  theme(
    plot.title = element_text(size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 16, hjust = 0.5),
    legend.title = element_text(size = 14),
    axis.text = element_text(angle = 30, size = 14),
    axis.title = element_text(size = 16),
    strip.text = element_text(size = 16)
  ) +
  labs(
    fill = "Snowmelt DOY",
    x = "Longitude",
    y = "Latitude",
    title = "Predicted snowmelt DOY",
    subtitle = "(linear regression for each cell)"
  ) +
  scale_fill_viridis_c(na.value = "transparent", limits = c(100, 250), alpha = 1, option = "turbo") -> p1

ggsave("snowmelt_doy.png", p1, width = 12, height = 6, device = png)

snow_shifts <- c(snow_2021 - snow_2012) |>
  mutate(shift = ifelse(abs(fitted_2021.tiff) > 50, NA, fitted_2021.tiff)) |>
  select(shift) |>
  st_warp(cellsize = 2, crs = st_crs(snow_2012)) |>
  st_transform(4326)

ggplot() +
  geom_stars(data = snow_shifts) +
  scale_fill_gradient2(
    low = "royalblue", high = "red", mid = "lightyellow", 
    midpoint = 0, na.value = "transparent"
  ) +
  theme(
    plot.title = element_text(size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 16, hjust = 0.5),
    legend.title = element_text(size = 14),
    axis.text = element_text(angle = 30, size = 14),
    axis.title = element_text(size = 16),
    strip.text = element_text(size = 16)
  ) +
  labs(
    fill = "DOY",
    x = "Longitude",
    y = "Latitude",
    title = "Changes in predicted snowmelt DOY",
    subtitle = "(2021-2012; negative values indicate earlier snowmelt)"
  ) -> p2


ggsave("snowmelt_shift_map.png", p2, width = 6, height = 6, device = png)


