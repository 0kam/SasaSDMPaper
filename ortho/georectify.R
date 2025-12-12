setwd("~/Projects/jasms2023f/ortho/")

library(sf)
library(tidyverse)
library(stars)
library(terra)

concat_df <- function(in_path, target = "data/georectified.csv") {
  print(target)
  georec_df <- target %>%
    read_csv() %>%
    select(u, v, x, y, z)
  df <- read_csv(in_path)
  georec_df %>%
    left_join(df, by = c("u", "v"))
}

interpolate <- function(in_path, out_path, res, max_dist, fun=terra::modal, target = "data/georectified.csv") {
  print(str_c("Reading ", in_path, " ......"))
  points <- in_path %>%
    concat_df(target = target) %>%
    st_as_sf(coords = c("x", "y")) %>%
    st_set_crs(6690) %>%
    mutate(z = as.integer(z)) %>%
    select(-c(u, v, z))
  
  print("Rasterizing point data ......")
  ras <- st_rasterize(points, dx = res, dy = res)
  rm(points)
  gc()
  
  ras <- ras %>% 
    as("Raster") %>%
    terra::rast()
  
  times <- ceiling(max_dist / res)
  for (i in 1:times) {
    print(str_c("Interpolating ......", i, " of ", times, " iterations"))
    ras <- ras %>%
      terra::focal(3, fun, na.policy="only", na.rm=TRUE)
  }
  print(str_c("Saving file to ", out_path, " ......"))
  terra::writeRaster(ras, out_path, overwrite=TRUE)
  rm(ras)
  gc()
  print("Finished !")
}

files <- c("data/2012_5x5.csv", "data/2021_5x5.csv")
out_paths <- stringr::str_replace(files, "csv", "tiff") %>% str_replace("data/", "data/vege_")

for (i in 1:length(files)) {
  interpolate(
    files[i], 
    out_paths[i], 
    1.0, 
    1.0
  )
}


files <- list.files("data/snow/aligned/", "*.csv", full.names = T)
out_paths <- stringr::str_replace(files, "csv", "tiff") %>% str_replace("aligned/", "raw/")

for (i in 1:length(files)) {
  interpolate(
    files[i], 
    out_paths[i], 
    1.0, 
    1.0
  )
}
