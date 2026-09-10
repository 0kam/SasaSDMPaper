suppressPackageStartupMessages({library(sf); library(readr); library(dplyr); library(stars); library(terra)})
base <- "/Users/okamoto/NIES/SasaSDMPaper/data_from_server/ortho/data"
out  <- "/Users/okamoto/NIES/SasaSDMPaper/.claude/worktrees/review-summary-ece-ddc69c/review/restore/figures"
geo <- read_csv(file.path(base,"georectified.csv"),
        col_types=cols(.default=col_double(), u=col_integer(), v=col_integer()), progress=FALSE) |> select(u,v,x,y,z)
cls <- read_csv(file.path(base,"2012_5x5.csv"),
        col_types=cols(u=col_integer(), v=col_integer(), data=col_integer()), progress=FALSE)
df  <- geo |> left_join(cls, by=c("u","v")) |> filter(!is.na(data))

pub  <- rast(file.path(base,"vege_2012_5x5.tiff")); names(pub) <- "pub"
pts  <- st_as_sf(df, coords=c("x","y")) |> st_set_crs(6690) |> select(data)
tmplS <- st_as_stars(pub)                      # exact published grid
rs <- st_rasterize(pts, template = tmplS)      # same GDAL call as georectify.R, on the right grid
r  <- rast(as(rs, "Raster"))
a <- values(r); b <- values(pub); ok <- !is.na(a) & !is.na(b)
cat(sprintf("st_rasterize(template=published grid): cells=%d  agreement=%.4f%%\n", sum(ok), 100*mean(a[ok]==b[ok])))
print(table(repro=a[ok], published=b[ok]))
rf <- terra::focal(r, 3, "modal", na.policy="only", na.rm=TRUE)
a2 <- values(rf); ok2 <- !is.na(a2) & !is.na(b)
cat(sprintf("  +1 focal modal fill: cells=%d  agreement=%.4f%%\n", sum(ok2), 100*mean(a2[ok2]==b[ok2])))
writeRaster(rf, file.path(out,"vege_2012_5x5_reproduced_partial.tiff"), overwrite=TRUE)
# focal with the ORIGINAL fun=terra::modal argument
cat("\nfocal(fun=terra::modal) on this raster: ")
print(tryCatch({x <- terra::focal(r, 3, terra::modal, na.policy="only", na.rm=TRUE); "OK"},
               error=function(e) paste("ERROR:", conditionMessage(e))))
