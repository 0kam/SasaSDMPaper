.libPaths(c("/private/tmp/claude-501/-Users-okamoto-NIES-SasaSDMPaper--claude-worktrees-review-summary-ece-ddc69c/98ac7ecf-eafb-49c6-aa2d-9aab2998c13e/scratchpad/Rlib", .libPaths()))
suppressPackageStartupMessages({library(xgboost);library(tidyverse);library(terra);library(tidyterra);library(sf);library(tidysdm);library(tidymodels)})
setwd("/Users/okamoto/NIES/SasaSDMPaper/ortho")
v12 <- rast("data/vege_2012_5x5.tiff"); v21 <- rast("data/vege_2021_5x5.tiff")
inc <- rast("data/sasa_inc.tiff") %>% terra::extend(v12) %>% terra::resample(v12, method="near")
ws <- readRDS("models_all_5m.rds"); tr <- ws$result[[1]]$splits[[1]]$data
pts <- vect(sf::st_coordinates(tr$geometry), crs = crs(v12))
d <- tibble(sasa = tr$sasa,
            v12 = terra::extract(v12, pts, ID=FALSE)[[1]], v21 = terra::extract(v21, pts, ID=FALSE)[[1]],
            inc = terra::extract(inc, pts, ID=FALSE)[[1]])
print(d %>% count(sasa, s12 = v12==1, s21 = v21==1) %>% arrange(desc(n)), n=20)
print(d %>% count(sasa, inc), n=20)
