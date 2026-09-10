# Independent re-run of ortho/georectify.R::interpolate() on the recovered CSVs,
# comparing the result against the archived vege_2012_5x5.tiff.
suppressPackageStartupMessages({library(sf); library(readr); library(dplyr); library(stars); library(terra)})
base <- "/Users/okamoto/NIES/SasaSDMPaper/data_from_server/ortho/data"
out  <- "/Users/okamoto/NIES/SasaSDMPaper/.claude/worktrees/review-summary-ece-ddc69c/review/restore/figures"

geo <- read_csv(file.path(base, "georectified.csv"), col_types = cols(.default = col_double(),
        u = col_integer(), v = col_integer()), progress = FALSE)
cat("georectified.csv rows:", nrow(geo), " cols:", paste(names(geo), collapse=","), "\n")
cat("  v range:", range(geo$v), " u range:", range(geo$u), "\n")
geo <- geo |> select(u, v, x, y, z)

cls <- read_csv(file.path(base, "2012_5x5.csv"),
        col_types = cols(u=col_integer(), v=col_integer(), data=col_integer()), progress=FALSE)
cat("2012_5x5.csv rows:", nrow(cls), " cols:", paste(names(cls), collapse=","),
    " v range:", range(cls$v, na.rm=TRUE), "\n")
cat("class table:\n"); print(table(cls$data, useNA="ifany"))

df <- geo |> left_join(cls, by = c("u","v"))
cat("joined non-NA data rows:", sum(!is.na(df$data)), "\n")

pts <- df |> filter(!is.na(data)) |> st_as_sf(coords=c("x","y")) |> st_set_crs(6690) |>
       select(-c(u,v,z))
ras <- st_rasterize(pts, dx=1.0, dy=1.0)
r <- terra::rast(as(ras, "Raster"))
names(r) <- "data"

# published raster
pub <- terra::rast(file.path(base, "vege_2012_5x5.tiff"))
cat("published grid:", dim(pub), " res:", res(pub), " crs:", crs(pub, describe=TRUE)$code, "\n")
cat("repro grid    :", dim(r), " res:", res(r), "\n")

# align repro onto published grid by nearest
r2 <- terra::resample(r, pub, method="near")
a <- values(r2); b <- values(pub)
ok <- !is.na(a) & !is.na(b)
cat("\n--- st_rasterize only ---\n")
cat("overlapping cells:", sum(ok), " exact agreement:",
    sprintf("%.4f%%", 100*mean(a[ok]==b[ok])), "\n")
print(table(repro=a[ok], published=b[ok]))

# one focal modal pass (max_dist/res = 1)
rf <- terra::focal(r, 3, "modal", na.policy="only", na.rm=TRUE)
rf2 <- terra::resample(rf, pub, method="near")
a2 <- values(rf2); ok2 <- !is.na(a2) & !is.na(b)
cat("\n--- st_rasterize + 1 focal modal fill ---\n")
cat("overlapping cells:", sum(ok2), " exact agreement:",
    sprintf("%.4f%%", 100*mean(a2[ok2]==b[ok2])), "\n")
terra::writeRaster(rf, file.path(out, "vege_2012_5x5_reproduced_partial_v2.tiff"), overwrite=TRUE)
cat("\nterra::modal as a function (original code path):\n")
print(tryCatch(terra::modal(c(1,2,2,3)), error=function(e) paste("ERROR:", conditionMessage(e))))
