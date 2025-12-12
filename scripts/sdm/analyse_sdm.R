setwd("~/Projects/jasms2023f/ortho/")
library(tidyverse)
library(tidysdm)
library(terra)
library(tidyterra)
library(DALEX)

sasa_pred_2021 <- rast("data/sasa_pred_sdm_21.tiff")
sasa_pred_2012 <- rast("data/sasa_pred_sdm_12.tiff") %>%
  terra::resample(sasa_pred_2021)
sasa_pred_2030 <- rast("data/sasa_pred_sdm_30.tiff") %>%
  terra::resample(sasa_pred_2021)

sasa_coms <- rast("data/selected_comms.tiff")
dist <- sasa_coms %>%
  terra::distance() %>%
  rename(dist = selected_comms) %>%
  terra::resample(sasa_pred_2021)

risk <- sasa_pred_2012 %>%
  c(sasa_pred_2021, dist) %>%
  terra::aggregate(fact = 5) %>%
  filter((dist != 0) & (dist < 10)) %>%
  mutate(has_risk = pred_sasa_21 - pred_sasa_12) %>%
  #mutate(has_risk = ifelse(has_risk > 0, has_risk, NA)) %>%
  select(has_risk)

sasa_inc <- rast("data/sasa_inc.tiff") %>%
  terra::resample(sasa_pred_2021) %>%
  c(dist) %>%
  terra::aggregate(fact = 5) %>%
  filter((dist != 0) & (dist < 10)) %>%
  select(-dist) 

c(sasa_inc, risk) %>%
  as_tibble() %>%
  drop_na() %>%
  ggplot(aes(x = has_risk, y = sasa_inc)) +
  geom_point()

c(sasa_inc, risk) %>%
  as_tibble() %>%
  drop_na() %>%
  mutate(sasa_inc = as.factor(sasa_inc)) %>%
  ggplot(aes(x = sasa_inc, y = has_risk)) +
  geom_violin()

sasa_pred_2012 %>%
  c(sasa_pred_2021, dist, sasa_inc) %>%
  filter((dist != 0) & (dist < 5)) %>%
  as_tibble() %>%
  drop_na() %>%
  pivot_longer(cols = starts_with("pred"), names_to = "year", values_to = "sasa_hs") %>%
  mutate(year = str_remove(year, "pred_sasa_")) %>%
  mutate(sasa_inc = as.factor(sasa_inc)) %>%
  ggplot(aes(x = sasa_inc, y = sasa_hs)) +
  geom_violin( ) +
  facet_wrap(~year)
  
