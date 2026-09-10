# FINAL: reproduce vege_{2012,2021}_5x5.tiff from the recovered CSVs, on the published grid,
# with a BLANKED template (st_rasterize leaks template values otherwise - verified).
suppressPackageStartupMessages({library(sf); library(readr); library(dplyr); library(stars); library(terra)})
base <- "/Users/okamoto/NIES/SasaSDMPaper/data_from_server/ortho/data"
out  <- "/Users/okamoto/NIES/SasaSDMPaper/.claude/worktrees/review-summary-ece-ddc69c/review/restore/figures"
geo <- read_csv(file.path(base,"georectified.csv"),
        col_types=cols(.default=col_double(), u=col_integer(), v=col_integer()), progress=FALSE) |> select(u,v,x,y)

for (yr in c("2012","2021")) {
  cls <- read_csv(file.path(base, paste0(yr,"_5x5.csv")),
          col_types=cols(u=col_integer(), v=col_integer(), data=col_integer()), progress=FALSE)
  df  <- geo |> left_join(cls, by=c("u","v")) |> filter(!is.na(data))
  pub <- rast(file.path(base, paste0("vege_",yr,"_5x5.tiff"))); names(pub) <- "pub"
  tmpl <- st_as_stars(pub); tmpl[[1]][] <- NA_real_
  pts <- st_as_sf(df, coords=c("x","y")) |> st_set_crs(6690) |> select(data)
  rs  <- st_rasterize(pts, template = tmpl)
  r   <- rast(as(rs,"Raster"))
  a <- values(r); b <- values(pub)
  cat("\n########", yr, "########\n")
  cat("class-carrying points joined:", nrow(df), " (image rows v", paste(range(df$v),collapse="-"), ")\n")
  cat("repro non-NA cells:", sum(!is.na(a)), "  published non-NA cells:", sum(!is.na(b)), "\n")
  ok <- !is.na(a) & !is.na(b)
  cat(sprintf("st_rasterize only  : overlap=%d  agreement=%.4f%%\n", sum(ok), 100*mean(a[ok]==b[ok])))
  print(table(repro=a[ok], published=b[ok]))
  rf <- terra::focal(r, 3, "modal", na.policy="only", na.rm=TRUE)
  a2 <- values(rf); ok2 <- !is.na(a2) & !is.na(b)
  cat(sprintf("+1 focal modal fill: overlap=%d  agreement=%.4f%%\n", sum(ok2), 100*mean(a2[ok2]==b[ok2])))
  cat("  Sasa (class 1): repro", sum(a2[ok2]==1), " published-in-overlap", sum(b[ok2]==1),
      " published-total", sum(b==1, na.rm=TRUE), "\n")
  writeRaster(rf, file.path(out, paste0("vege_",yr,"_5x5_reproduced.tiff")), overwrite=TRUE)
}
