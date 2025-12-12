setwd("~/doctoral_thesis/chap2/ortho/")

library(sf)
library(tidyverse)
library(stars)
library(broom)

vege <- "data/vege_2012_5x5.tiff" %>%
  read_stars()

snow <- list.files("data/snow/raw/", full.names = T) %>%
  str_subset(".tiff$") %>%
  map(
    function(path) {
      read_stars(path) %>%
        st_warp(vege)
    }
  ) %>%
  reduce(c)

crs <- st_crs(snow)


# ------------------------------------------------------------------
snow %>% as_tibble() %>%
  rename_at(
    vars(starts_with("MRD")),
    ~ str_extract(., "20\\d{2}")
  ) %>%
  drop_na() %>%
  pivot_longer(
    cols = -c(x, y),
    names_to = "year",
    values_to = "snowmelt"
  ) %>%
  mutate(year = as.integer(year)) %>%
  filter(snowmelt > 10) %>%
  group_by(year) %>%
  summarise(snowmelt_mean = mean(snowmelt)) %>%
  ggplot(aes(x = year, y = snowmelt_mean)) +
  geom_point() +
  geom_smooth(method=lm, se=T)

#-------------------------------------------------------------------------------
snow_mean <- snow %>%
  as_tibble() %>%
  rename_at(
    vars(starts_with("MRD")),
    ~ str_extract(., "20\\d{2}")
  ) %>%
  drop_na() %>%
  pivot_longer(
    cols = -c(x, y),
    names_to = "year",
    values_to = "snowmelt"
  ) %>%
  group_by(x, y) %>%
  summarise(
    mean_snowmelt = mean(snowmelt)
  )

snow_mean_ras <- snow_mean %>%
  st_as_stars() %>%
  st_set_crs(crs)

snow_mean_ras %>%
  write_stars("data/terrain_features/snow_mean.tif")

ggplot() +
  geom_stars(data = snow_mean_ras)

#-------------------------------------------------------------------------------
snow_sd <- snow %>%
  as_tibble() %>%
  rename_at(
    vars(starts_with("MRD")),
    ~ str_extract(., "20\\d{2}")
  ) %>%
  drop_na() %>%
  #sample_n(1) %>%
  pivot_longer(
    cols = -c(x, y),
    names_to = "year",
    values_to = "snowmelt"
  ) %>%
  mutate(year = as.integer(year)) %>%
  group_by(x, y) %>%
  summarise(sd = sd(snowmelt))
  
snow_sd_ras <- snow_sd %>%
  drop_na() %>%
  st_as_stars() %>%
  st_set_crs(crs)

snow_sd_ras %>%
  write_stars("data/terrain_features/snow_sd.tif")

ggplot() +
  geom_stars(data = snow_sd_ras)

#-------------------------------------------------------------------------------
snow_reg <- snow %>%
  as_tibble(add_max = T) %>%
  rename_at(
    vars(starts_with("MRD")),
    ~ str_extract(., "20\\d{2}")
  ) %>%
  drop_na() %>%
  #sample_n(1000) %>%
  pivot_longer(
    cols = -c(x, y, x_max, y_max),
    names_to = "year",
    values_to = "snowmelt"
  ) %>%
  filter(snowmelt > 0) %>%
  mutate(year = as.integer(year))

library(multidplyr)
s <- snow_reg %>%
  group_by(x, y) %>%
  partit
  summarise(
    lm.coef = (lm(snowmelt ~ year, data = .))$coeffisients["year"],
    lm.r2 = (lm(snowmelt ~ year, data = .))$adj.r.squared,
    lm.pval = summary(lm(snowmelt ~ year, data = .))$coefficients[2,4]
  )

snow_reg_ras <- s %>%
  drop_na() %>%
  filter(lm.r2 > 0.2) %>%
  select(-lm.r2) %>%
  rename(snow_reg = lm.coef) %>%
  st_as_stars() %>%
  st_set_crs(crs)

snow_reg_ras %>%
  write_stars("data/terrain_features/snow_reg.tif")

ggplot() +
  geom_stars(data = snow_reg_ras)


fitted <- snow_reg %>%
  nest_by(x, y, x_max, y_max) %>%
  mutate(
    fit = list(lm(snowmelt ~ year, data = data))
  ) %>%
  reframe(broom::augment(fit))

write_csv(fitted, "data/snow/fitted.csv")

fitted <- read_csv("data/snow/fitted.csv") %>%
  drop_na() %>%
  rename(snowmelt_fitted = .fitted) %>%
  select(x, y, year, snowmelt_fitted) %>%
  group_by(year) %>%
  nest()

for (y in fitted$year) {
  data <- fitted %>%
    filter(year == y) %>%
    pull(data)
  
  fname <- str_c("data/snow/fitted_", y, ".tiff")
  
  data[[1]] %>%
    rast() %>%
    terra::`crs<-`("epsg:6690") %>%
    terra::writeRaster(fname)
}

cluster <- new_cluster(22)

fit_30 <- snow_reg %>%
  group_by(x, y) %>%
  partition(cluster) %>%
  summarise(
    snowmelt_fitted = predict(
      lm(snowmelt ~ year), newdata = data.frame(year = 2030))
  ) %>%
  collect()

fit_30 %>%
  mutate(snowmelt_fitted = ifelse(snowmelt_fitted < 0, 0, snowmelt_fitted)) %>%
  mutate(snowmelt_fitted = ifelse(snowmelt_fitted > 255, 255, snowmelt_fitted)) %>%
  rast() %>%
  terra::`crs<-`("epsg:6690") %>%
  terra::writeRaster("data/snow/fitted_2030.tiff", overwrite = T)


# regoression for each pixel

snow_sample <- snow %>% as_tibble() %>%
  rename_at(
    vars(starts_with("MRD")),
    ~ str_extract(., "20\\d{2}")
  ) %>%
  drop_na() %>%
  pivot_longer(
    cols = -c(x, y),
    names_to = "year",
    values_to = "snowmelt"
  ) %>%
  mutate(year = as.integer(year)) %>%
  filter(snowmelt > 10) %>%
  group_by(x, y) %>%
  nest() %>%
  ungroup() %>%
  sample_n(1000)

p <- snow_sample %>%
  mutate(id = row_number() %>% as.factor()) %>%
  unnest(cols = c(data)) %>%
  ggplot(aes(x = year, y = snowmelt, color = id)) +
  geom_point() +
  geom_smooth(method=lm, se = F) +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "none"
  ) +
  labs(
    x = "Year",
    y = "Snowmelt Day of Year"
  )

ggsave("snowmelt_shifting.png", p, width = 6, height = 4)

