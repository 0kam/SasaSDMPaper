# =============================================================================
# 02_folds_and_distance.R -- shared distance surface and spatial CV blocks
# =============================================================================

t0 <- Sys.time()
source(file.path("analysis", "00_config.R"))
source(file.path(DIR_ANALYSIS, "R", "distance.R"))

suppressPackageStartupMessages(library(sf))
set.seed(SEED_FOLDS)

msg("Building dist12 from 2012 Sasa patches larger than 5 m2")
vege12 <- terra::rast(PATH_VEGE_2012)
dist12 <- sasa_distance(vege12, min_area_m2 = 5)
names(dist12) <- "dist12"
assert_on_ref_grid(dist12, "dist12")
terra::writeRaster(dist12, PATH_DIST12, overwrite = TRUE,
                   datatype = "FLT4S", gdal = "COMPRESS=DEFLATE")

msg("Building one seeded four-fold block scheme over the full reference domain")
template <- ref_grid()
e <- terra::ext(template)
ev <- as.vector(e)
bbox <- sf::st_bbox(c(xmin = ev[[1]], ymin = ev[[3]],
                      xmax = ev[[2]], ymax = ev[[4]]),
                    crs = sf::st_crs(terra::crs(template)))
# spatial_block_cv() uses st_make_grid() over the bounding box when no cellsize
# is supplied. n = c(10, 10) makes that default explicit and reproducible.
blocks <- sf::st_sf(
  block_id = seq_len(100L),
  geometry = sf::st_make_grid(sf::st_as_sfc(bbox), n = c(10L, 10L),
                              what = "polygons", square = TRUE)
)
blocks$fold_id <- sample(rep(seq_len(CV_FOLDS_FULL),
                             length.out = nrow(blocks)))

fold_raster <- terra::rasterize(terra::vect(blocks), template,
                                field = "fold_id", touches = TRUE)
names(fold_raster) <- "fold_id"
assert_on_ref_grid(fold_raster, "fold raster")
if (anyNA(terra::values(fold_raster, mat = FALSE))) {
  stop("the shared fold raster does not cover the full reference grid")
}
terra::writeRaster(fold_raster, PATH_FOLDS_RASTER, overwrite = TRUE,
                   datatype = "INT1U", gdal = "COMPRESS=DEFLATE")
sf::st_write(blocks, PATH_FOLDS_VECTOR, layer = "spatial_blocks",
             delete_dsn = TRUE, quiet = TRUE)

fold_counts <- as.data.frame(terra::freq(fold_raster))
utils::write.csv(fold_counts, file.path(DIR_OUT, "fold_cell_counts.csv"),
                 row.names = FALSE)
msg("Fold cell counts: ", paste(fold_counts$count, collapse = ", "))

finish_script("02_folds_and_distance.R", t0)
