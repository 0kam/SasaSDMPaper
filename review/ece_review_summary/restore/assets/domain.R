suppressPackageStartupMessages({library(terra);library(tidyterra);library(dplyr);library(stringr);library(sf);library(tidysdm)})
D <- "/Users/okamoto/NIES/SasaSDMPaper/data_from_server/ortho/"
v12 <- rast(paste0(D,"data/vege_2012_5x5.tiff")); v21 <- rast(paste0(D,"data/vege_2021_5x5.tiff"))
terrain <- list.files(paste0(D,"data/terrain_features/"), full.names=TRUE) %>% str_subset(".tif$") %>%
  rast() %>% rename(elevation=tateyamadem_small) %>% resample(v12)
snow21 <- rast(paste0(D,"data/snow/fitted_2021.tiff")) %>% resample(v12) %>% rename(snow=snowmelt_fitted)
crs(terrain) <- crs(v12); crs(snow21) <- crs(v12)
s12 <- v12 %>% mutate(sasa=ifelse(layer==1,1,0)) %>% select(sasa)
s21 <- v21 %>% mutate(sasa=ifelse(layer==1,1,0)) %>% select(sasa)
dist <- s12 %>% filter(sasa==1) %>% distance() %>% rename(dist=sasa)   # exactly sdm_include_distance.R
st <- c(s21, terrain, snow21, dist)
a <- as.data.frame(st, na.rm=TRUE)
cat("cells with ALL covariates + response (i.e. after drop_na):", nrow(a), "\n")
b <- a %>% filter(elevation < 2560)
cat("after elevation < 2560:", nrow(b), "  presences (2021 Sasa):", sum(b$sasa==1), "\n")
cat("  of those presences, dist==0 (i.e. ALSO Sasa in 2012):", sum(b$sasa==1 & b$dist==0),
    sprintf(" (%.1f%%)", 100*mean(b$dist[b$sasa==1]==0)), "\n")
cat("TDM domain after filter(dist>0):", sum(b$dist>0), "  presences:", sum(b$sasa==1 & b$dist>0),
    "  absences:", sum(b$sasa==0 & b$dist>0), "\n")
cat("TBM domain (no dist filter)   :", nrow(b), "  presences:", sum(b$sasa==1), "  absences:", sum(b$sasa==0), "\n")
cat("=> filter(dist>0) removes", sum(b$sasa==1 & b$dist==0), "presences and", sum(b$sasa==0 & b$dist==0), "absences\n")
cat("prevalence TBM:", sprintf("%.4f", mean(b$sasa==1)), "  TDM:", sprintf("%.4f", mean(b$sasa[b$dist>0]==1)), "\n")
