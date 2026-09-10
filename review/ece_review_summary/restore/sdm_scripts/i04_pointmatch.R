.libPaths(c("/private/tmp/claude-501/-Users-okamoto-NIES-SasaSDMPaper--claude-worktrees-review-summary-ece-ddc69c/98ac7ecf-eafb-49c6-aa2d-9aab2998c13e/scratchpad/Rlib", .libPaths()))
suppressPackageStartupMessages({
  library(tidyverse); library(terra); library(tidyterra); library(sf); library(stacks); library(xgboost)
})
setwd("/Users/okamoto/NIES/SasaSDMPaper/ortho")

vege12 <- rast("data/vege_2012_5x5.tiff"); vege21 <- rast("data/vege_2021_5x5.tiff")
tf <- list.files("data/terrain_features/", full.names=TRUE) %>% str_subset(".tif$")
mkt <- function(f){ r <- rast(f) %>% rename(elevation = tateyamadem_small) %>% resample(vege12); crs(r) <- crs(vege12); r }
terr_twi   <- mkt(tf)
terr_notwi <- mkt(tf %>% str_subset("twi", negate=TRUE))
snow21 <- rast("data/snow/fitted_2021.tiff") %>% resample(vege12) %>% rename(snow = snowmelt_fitted)
snow12 <- rast("data/snow/fitted_2012.tiff") %>% resample(vege12) %>% rename(snow = snowmelt_fitted)
# correctly registered snow (fix the add_max half-pixel write bug)
snow21c <- (rast("data/snow/fitted_2021.tiff") %>% terra::shift(dx=0.5, dy=-0.5)) %>% resample(vege12) %>% rename(snow = snowmelt_fitted)

sasa12 <- vege12 %>% mutate(sasa = ifelse(layer==1,1,0)) %>% select(sasa)
sasa21 <- vege21 %>% mutate(sasa = ifelse(layer==1,1,0)) %>% select(sasa)
d_all12 <- sasa12 %>% filter(sasa==1) %>% distance() %>% rename(dist=sasa)
d_all21 <- sasa21 %>% filter(sasa==1) %>% distance() %>% rename(dist=sasa)
polys <- function(r) r %>% filter(sasa==1) %>% stars::st_as_stars() %>% sf::st_as_sf(merge=TRUE) %>%
  mutate(area=sf::st_area(.)) %>% filter(area > units::set_units(5,m^2)) %>% select(sasa) %>% vect()
p12 <- polys(sasa12); p21 <- polys(sasa21)
d_pol12 <- terra::rasterize(p12, d_all12) %>% filter(layer==1) %>% distance() %>% rename(dist=layer)
d_pol21 <- terra::rasterize(p21, d_all12) %>% filter(layer==1) %>% distance() %>% rename(dist=layer)
# distance from the "selected_comms" raster used by sdm_sasainc.R / analyse_sdm.R
sc <- rast("data/selected_comms.tiff") %>% resample(sasa12)
d_sc <- sc %>% distance() %>% rename(dist = selected_comms)

report <- function(stackfile, tag) {
  cat("\n=====", tag, "(", stackfile, ") =====\n")
  s <- readRDS(stackfile)
  tr <- s$train
  cat(" train n =", nrow(tr), " crs =", sf::st_crs(tr)$epsg, " geom class:", paste(class(tr$geometry), collapse=","), "\n")
  xy <- sf::st_coordinates(tr$geometry)
  cat(" xy range:", paste(round(apply(xy,2,range),3), collapse=" "), "\n")
  ex <- function(r, nm) terra::extract(r, xy)[[1]]
  cmp <- function(stored, v, nm) {
    d <- abs(stored - v)
    cat(sprintf("   %-16s : maxabs=%.6g  meanabs=%.6g  n_exact(<1e-6)=%d/%d\n", nm, max(d,na.rm=TRUE), mean(d,na.rm=TRUE), sum(d<1e-6,na.rm=TRUE), length(d)))
  }
  st <- tr %>% st_drop_geometry() %>% as_tibble()
  for (nm in intersect(names(terr_notwi), colnames(st))) cmp(st[[nm]], ex(terr_notwi[nm]), paste0("terrain:",nm))
  if ("snow" %in% colnames(st)) {
    cmp(st$snow, ex(snow21), "snow=fitted2021(as published, half-px off)")
    cmp(st$snow, ex(snow12), "snow=fitted2012")
    cmp(st$snow, ex(snow21c), "snow=fitted2021 RE-REGISTERED")
  }
  if ("dist" %in% colnames(st)) {
    cmp(st$dist, ex(d_all12), "dist=ALL 2012 px")
    cmp(st$dist, ex(d_pol12), "dist=2012 pol>5m2")
    cmp(st$dist, ex(d_all21), "dist=ALL 2021 px")
    cmp(st$dist, ex(d_pol21), "dist=2021 pol>5m2")
    cmp(st$dist, ex(d_sc),    "dist=selected_comms")
    cat("   stored dist: min", min(st$dist), " n==0:", sum(st$dist==0), " quantiles:",
        paste(round(quantile(st$dist, c(0,.25,.5,.75,1)),3), collapse=" "), "\n")
  }
  # twi at those points (not a predictor, but check the value would have been available)
  cat("   twi at train pts: mean", mean(ex(terr_twi["twi"]), na.rm=TRUE), " NA frac",
      mean(is.na(ex(terr_twi["twi"]))), "\n")
  invisible(st)
}
report("model_stack.rds", "TDM")
report("model_stack_wo_dist.rds", "TBM")
