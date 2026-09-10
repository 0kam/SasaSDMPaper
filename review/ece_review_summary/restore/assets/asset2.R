suppressPackageStartupMessages({library(terra); library(sf); library(stars)})
D <- "/Users/okamoto/NIES/SasaSDMPaper/data_from_server/ortho/data/"
sc <- rast(paste0(D,"selected_comms.tiff"))
v12 <- rast(paste0(D,"vege_2012_5x5.tiff")); v21 <- rast(paste0(D,"vege_2021_5x5.tiff"))
cat("### What is selected_comms.tiff?\n")
m <- !is.na(values(sc))
cat("non-NA cells:", sum(m), "\n")
cat("vege_2012 class composition UNDER selected_comms:\n"); print(table(values(v12)[m], useNA="ifany"))
cat("vege_2021 class composition UNDER selected_comms:\n"); print(table(values(v21)[m], useNA="ifany"))
cat("\n-> if 100% class 1 in 2012, selected_comms is a SASA mask, not a community map.\n")

cat("\n### Reproduce select_sasa_communities.R (2012 Sasa polygons >=5 m2 touching 10 m cells with >25 m2 cover)\n")
sasa12 <- v12; sasa12[sasa12 != 1] <- NA
pol <- st_as_stars(sasa12) |> st_as_sf(merge=TRUE)
names(pol)[1] <- "vege"
pol$area_2012 <- as.numeric(st_area(pol))
pol <- pol[pol$area_2012 >= 5, ]
cat("polygons >= 5 m2:", nrow(pol), " total area:", sum(pol$area_2012), "\n")
bb <- st_bbox(pol)
r10 <- rast(xmin=bb["xmin"], xmax=bb["xmax"], ymin=bb["ymin"], ymax=bb["ymax"], resolution=c(10,10),
            crs=crs(v12))
cov <- terra::rasterize(vect(pol), r10, cover=TRUE) * 100
isects <- st_intersects(pol, st_as_sf(st_as_stars(cov[cov>25, drop=FALSE])))
aoi <- pol[lengths(isects) > 0, ]
cat("polygons retained (AOI):", nrow(aoi), " distinct areas:", length(unique(aoi$area_2012)),
    " total area:", sum(aoi$area_2012), "\n")
cat("archived selected_comms: cells", sum(m), " distinct values", length(unique(values(sc)[m])), "\n")
cat("archived distinct values:", paste(sort(unique(values(sc)[m])), collapse=","), "\n")
cat("repro polygon areas    :", paste(sort(unique(aoi$area_2012)), collapse=","), "\n")

cat("\n### Cross-tab: selected_comms vs the three risky-area rasters\n")
P <- "/Users/okamoto/NIES/SasaSDMPaper/ortho/"
S <- "/Users/okamoto/NIES/SasaSDMPaper/data_from_server/sasa_increase/"
for (f in c(paste0(P,"risky_area_wo_dist.tiff"), paste0(P,"risky_area_tdm.tiff"), paste0(S,"risky_area.tiff"))) {
  r <- rast(f); crs(r) <- crs(sc)
  rr <- terra::extend(r, sc); rr <- terra::resample(rr, sc, method="near")
  rm_ <- !is.na(values(rr))
  cat("\n", basename(f), " risky cells:", sum(rm_), "\n")
  cat("   overlap with selected_comms:", sum(rm_ & m), "cells\n")
  cat("   vege_2021 composition of risky cells:\n"); print(table(values(v21)[rm_], useNA="ifany"))
}
