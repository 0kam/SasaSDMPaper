# End-to-end check of the recovered georectification transform.
# Reproduces vege_2012_5x5.tiff from georectified.csv + 2012_5x5.csv using the
# archived interpolate() code (data_from_server/ortho/georectify.R), over the
# image-row band that survived the server transfer, and compares cell by cell.
suppressPackageStartupMessages({
  library(readr); library(dplyr); library(sf); library(stars); library(terra)
})
SRV <- "/Users/okamoto/NIES/SasaSDMPaper/data_from_server/ortho/data/"
OUT <- "/Users/okamoto/NIES/SasaSDMPaper/.claude/worktrees/review-summary-ece-ddc69c/review/restore/georect/out/"

georec <- read_csv(paste0(SRV, "georectified.csv"),
                   col_types = cols_only(u = "i", v = "i", x = "d", y = "d", z = "d")) |>
  filter(!is.na(x), !is.na(y), !is.na(z))
cat("georectified.csv rows read:", nrow(georec), "  v range:", range(georec$v), "\n")

veg <- read_csv(paste0(SRV, "2012_5x5.csv"), col_types = cols_only(u = "i", v = "i", data = "i")) |>
  filter(!is.na(data))
cat("2012_5x5.csv rows read:", nrow(veg), "  v range:", range(veg$v), "\n")

j <- left_join(georec, veg, by = c("u", "v"))
cat("joined rows:", nrow(j), " unmatched:", sum(is.na(j$data)), "\n")
j <- filter(j, !is.na(data))

pts <- j |>
  st_as_sf(coords = c("x", "y")) |>
  st_set_crs(6690) |>
  select(data)

ras <- st_rasterize(pts, dx = 1.0, dy = 1.0)
r_new <- terra::rast(as(ras, "Raster"))

ref <- terra::rast("/Users/okamoto/NIES/SasaSDMPaper/ortho/data/vege_2012_5x5.tiff")
cat("\nreference:", paste(dim(ref), collapse="x"), " ext:", paste(round(as.vector(terra::ext(ref)),2), collapse=" "), "\n")
cat("reproduced:", paste(dim(r_new), collapse="x"), " ext:", paste(round(as.vector(terra::ext(r_new)),2), collapse=" "), "\n")

# align reproduced onto reference grid (nearest, no resampling of values)
r_al <- terra::resample(r_new, ref, method = "near")
a <- terra::values(r_al); b <- terra::values(ref)
ok <- !is.na(a) & !is.na(b)
cat("\ncells with a directly-rasterised value in BOTH:", sum(ok), "\n")
cat("exact class agreement: ", sprintf("%.4f%%", 100*mean(a[ok] == b[ok])), "\n")
tb <- table(reproduced = a[ok], archived = b[ok])
print(tb)
# Sasa-only confusion
cat("\nSasa (class 1) reproduced:", sum(a[ok]==1), " archived:", sum(b[ok]==1),
    " both:", sum(a[ok]==1 & b[ok]==1), "\n")
terra::writeRaster(r_al, paste0(OUT, "vege_2012_5x5_reproduced_band.tiff"), overwrite = TRUE)
saveRDS(tb, paste0(OUT, "e2e_confusion.rds"))
