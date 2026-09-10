.libPaths(c("/private/tmp/claude-501/-Users-okamoto-NIES-SasaSDMPaper--claude-worktrees-review-summary-ece-ddc69c/98ac7ecf-eafb-49c6-aa2d-9aab2998c13e/scratchpad/Rlib", .libPaths()))
suppressPackageStartupMessages({
  library(xgboost); library(tidyverse); library(terra); library(tidyterra); library(sf)
  library(tidysdm); library(tidymodels); library(stacks)
})
setwd("/Users/okamoto/NIES/SasaSDMPaper/ortho")
vege12 <- rast("data/vege_2012_5x5.tiff"); vege21 <- rast("data/vege_2021_5x5.tiff")
tf <- list.files("data/terrain_features/", full.names=TRUE) %>% str_subset(".tif$")
mkt <- function(f){ r <- rast(f) %>% rename(elevation=tateyamadem_small) %>% resample(vege12); crs(r) <- crs(vege12); r }
terr <- mkt(tf %>% str_subset("twi", negate=TRUE))
sasa12 <- vege12 %>% mutate(sasa=ifelse(layer==1,1,0)) %>% select(sasa)
sasa21 <- vege21 %>% mutate(sasa=ifelse(layer==1,1,0)) %>% select(sasa)
d_all12 <- sasa12 %>% filter(sasa==1) %>% distance() %>% rename(dist=sasa)
polys <- function(r) r %>% filter(sasa==1) %>% stars::st_as_stars() %>% sf::st_as_sf(merge=TRUE) %>%
  mutate(area=sf::st_area(.)) %>% filter(area>units::set_units(5,m^2)) %>% select(sasa) %>% vect()
d_pol12 <- terra::rasterize(polys(sasa12), d_all12) %>% filter(layer==1) %>% distance() %>% rename(dist=layer)
sc  <- rast("data/selected_comms.tiff") %>% resample(sasa12, method="near")
d_sc <- sc %>% distance() %>% rename(dist=selected_comms)
sn <- function(y) rast(sprintf("data/snow/fitted_%d.tiff", y)) %>% resample(vege12) %>% rename(snow=snowmelt_fitted)

ws <- readRDS("models_all_5m.rds")
tr <- ws$result[[1]]$splits[[1]]$data
cat("models_all_5m training frame:", nrow(tr), "rows; cols:", paste(colnames(tr), collapse=", "), "\n")
print(tr %>% st_drop_geometry() %>% count(sasa))
xy <- sf::st_coordinates(tr$geometry)
st <- tr %>% st_drop_geometry() %>% as_tibble()
ex <- function(r) terra::extract(r, xy)[[1]]
cmpf <- function(stored, v, nm) { d <- abs(stored-v)
  cat(sprintf("   %-24s maxabs=%.6g meanabs=%.6g exact=%d/%d\n", nm, max(d,na.rm=TRUE), mean(d,na.rm=TRUE), sum(d<1e-6,na.rm=TRUE), length(d))) }
for (nm in intersect(names(terr), colnames(st))) cmpf(st[[nm]], ex(terr[nm]), paste0("terrain:",nm))
for (y in c(2012, 2021)) cmpf(st$snow, ex(sn(y)), paste0("snow=fitted_", y))
cmpf(st$dist, ex(d_all12), "dist=ALL 2012 px")
cmpf(st$dist, ex(d_pol12), "dist=2012 pol>5m2")
cmpf(st$dist, ex(d_sc),    "dist=selected_comms")
cat("   stored dist: min", min(st$dist), " n==0:", sum(st$dist==0), " max", max(st$dist), "\n")
cat("   elevation max:", max(st$elevation), "\n")
