# Shared Sasa-front distance calculation.

#' Distance from Sasa patches larger than a minimum geodesic area.
#'
#' Connected Sasa cells are converted to polygons before filtering. Area is
#' calculated with terra::expanse(transform = TRUE), the project-wide geodesic
#' convention. Four-neighbour connectivity reproduces raster polygonization:
#' cells that touch only at a corner remain separate polygons.
#'
#' @param rast single-layer vegetation SpatRaster; class 1 is Sasa
#' @param min_area_m2 retain patches with area strictly greater than this value
#' @return SpatRaster of distance in metres, on the input grid
sasa_distance <- function(rast, min_area_m2 = 5) {
  if (!inherits(rast, "SpatRaster") || terra::nlyr(rast) != 1L) {
    stop("rast must be a single-layer SpatRaster")
  }
  if (!is.numeric(min_area_m2) || length(min_area_m2) != 1L ||
      !is.finite(min_area_m2) || min_area_m2 < 0) {
    stop("min_area_m2 must be one finite non-negative number")
  }

  sasa <- terra::ifel(rast == 1, 1, NA)
  # With a zero threshold every non-empty Sasa patch is retained. Skipping
  # polygonization is exactly equivalent and avoids a severe slowdown as CA
  # realizations accumulate many small patches in the sensitivity run.
  if (min_area_m2 == 0) {
    out <- terra::distance(sasa)
    names(out) <- "distance"
    return(out)
  }
  patch_ids <- terra::patches(sasa, directions = 4, zeroAsNA = TRUE)
  patches <- terra::as.polygons(patch_ids, aggregate = TRUE, na.rm = TRUE)
  if (nrow(patches) == 0L) stop("no Sasa patches were found")

  patches$area_m2 <- as.numeric(terra::expanse(
    patches, unit = "m", transform = TRUE
  ))
  kept <- patches[patches$area_m2 > min_area_m2, ]
  if (nrow(kept) == 0L) {
    stop("no Sasa patches exceed min_area_m2 = ", min_area_m2)
  }

  source <- terra::rasterize(kept, terra::rast(rast), field = 1,
                             background = NA, touches = TRUE)
  out <- terra::distance(source)
  names(out) <- "distance"
  out
}
