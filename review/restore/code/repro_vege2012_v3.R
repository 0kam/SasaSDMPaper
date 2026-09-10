# v3: rasterise the recovered points onto the *published* grid so the comparison is
# not confounded by the half-cell origin offset that st_rasterize's own bbox produces.
suppressPackageStartupMessages({library(sf); library(readr); library(dplyr); library(stars); library(terra)})
base <- "/Users/okamoto/NIES/SasaSDMPaper/data_from_server/ortho/data"
out  <- "/Users/okamoto/NIES/SasaSDMPaper/.claude/worktrees/review-summary-ece-ddc69c/review/restore/figures"

geo <- read_csv(file.path(base,"georectified.csv"),
        col_types=cols(.default=col_double(), u=col_integer(), v=col_integer()), progress=FALSE) |>
       select(u,v,x,y,z)
cls <- read_csv(file.path(base,"2012_5x5.csv"),
        col_types=cols(u=col_integer(), v=col_integer(), data=col_integer()), progress=FALSE)
df  <- geo |> left_join(cls, by=c("u","v")) |> filter(!is.na(data))
cat("points with a class:", nrow(df), "\n")

pub <- rast(file.path(base,"vege_2012_5x5.tiff")); names(pub) <- "pub"
tmpl <- rast(pub); values(tmpl) <- NA

v <- vect(as.data.frame(df[,c("x","y","data")]), geom=c("x","y"), crs="EPSG:6690")
for (f in c("last","modal","min")) {
  r <- terra::rasterize(v, tmpl, field="data", fun=f)
  a <- values(r); b <- values(pub); ok <- !is.na(a) & !is.na(b)
  cat(sprintf("fun=%-6s overlapping cells=%7d  exact agreement=%.4f%%\n", f, sum(ok), 100*mean(a[ok]==b[ok])))
  if (f=="modal") {
    print(table(repro=a[ok], published=b[ok]))
    rf <- terra::focal(r, 3, "modal", na.policy="only", na.rm=TRUE)
    a2 <- values(rf); ok2 <- !is.na(a2) & !is.na(b)
    cat(sprintf("  + 1 focal modal fill: cells=%7d  exact agreement=%.4f%%\n", sum(ok2), 100*mean(a2[ok2]==b[ok2])))
    writeRaster(rf, file.path(out,"vege_2012_5x5_reproduced_partial_v3.tiff"), overwrite=TRUE)
  }
}
# how many published cells exist in the covered v-band at all
cat("\npublished non-NA cells total:", global(!is.na(pub), "sum")[1,1], "\n")
