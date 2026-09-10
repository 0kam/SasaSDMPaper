suppressPackageStartupMessages({library(sf); library(readr); library(dplyr); library(stars); library(terra)})
base <- "/Users/okamoto/NIES/SasaSDMPaper/data_from_server/ortho/data"
out  <- "/Users/okamoto/NIES/SasaSDMPaper/.claude/worktrees/review-summary-ece-ddc69c/review/restore/figures"
for (yr in c("2012","2021")) {
  pub <- rast(file.path(base, paste0("vege_", yr, "_5x5.tiff")))
  cat("== published vege_", yr, "_5x5.tiff class counts:\n", sep="")
  print(table(values(pub), useNA="no"))
  cat("   non-NA cells:", sum(!is.na(values(pub))), "\n")
}
geo <- read_csv(file.path(base,"georectified.csv"),
        col_types=cols(.default=col_double(), u=col_integer(), v=col_integer()), progress=FALSE) |> select(u,v,x,y)
cls <- read_csv(file.path(base,"2021_5x5.csv"),
        col_types=cols(u=col_integer(), v=col_integer(), data=col_integer()), progress=FALSE)
cat("\n2021_5x5.csv v range:", range(cls$v), " rows:", nrow(cls), "\n")
df  <- geo |> left_join(cls, by=c("u","v")) |> filter(!is.na(data))
cat("joined points:", nrow(df), " (v range", range(df$v), ")\n")
pub <- rast(file.path(base,"vege_2021_5x5.tiff")); names(pub) <- "pub"
pts <- st_as_sf(df, coords=c("x","y")) |> st_set_crs(6690) |> select(data)
rs  <- st_rasterize(pts, template = st_as_stars(pub))
r   <- rast(as(rs,"Raster"))
a <- values(r); b <- values(pub); ok <- !is.na(a) & !is.na(b)
cat(sprintf("2021 st_rasterize(template): cells=%d  agreement=%.4f%%\n", sum(ok), 100*mean(a[ok]==b[ok])))
print(table(repro=a[ok], published=b[ok]))
writeRaster(r, file.path(out,"vege_2021_5x5_reproduced_partial.tiff"), overwrite=TRUE)
