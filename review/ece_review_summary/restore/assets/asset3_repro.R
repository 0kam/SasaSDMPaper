# Reproduce vege_{2012,2021}_5x5.tiff EXACTLY as data_from_server/ortho/georectify.R does,
# starting from results/use_this/*.npy -> ortho/data/*_5x5.csv (verified identical) + georectified.csv
suppressPackageStartupMessages({library(sf);library(readr);library(dplyr);library(stars);library(terra)})
B <- "/Users/okamoto/NIES/SasaSDMPaper/data_from_server/ortho/data"
O <- "/Users/okamoto/NIES/SasaSDMPaper/.claude/worktrees/review-summary-ece-ddc69c/review/restore/assets/out"

geo <- read_csv(file.path(B,"georectified.csv"),
   col_types=cols(.default=col_double(), u=col_integer(), v=col_integer()), progress=FALSE) |> select(u,v,x,y,z)
cat("georectified.csv rows:", nrow(geo), "\n")

for (yr in c("2012","2021")) {
  cls <- read_csv(file.path(B, paste0(yr,"_5x5.csv")),
     col_types=cols(u=col_integer(), v=col_integer(), data=col_integer()), progress=FALSE)
  df <- geo |> left_join(cls, by=c("u","v"))
  cat("\n#####", yr, " joined rows:", nrow(df), " with non-NA class:", sum(!is.na(df$data)),
      " class==0 (mask):", sum(df$data==0, na.rm=TRUE), "\n")
  pts <- st_as_sf(df, coords=c("x","y")) |> st_set_crs(6690) |> mutate(z=as.integer(z)) |> select(-c(u,v,z))
  ras <- st_rasterize(pts, dx=1.0, dy=1.0)
  r <- rast(as(ras,"Raster"))
  cat("raw st_rasterize grid: dim", paste(dim(r),collapse="x"), "ext", paste(signif(as.vector(ext(r)),12),collapse=","),"\n")
  r1 <- terra::focal(r, 3, terra::modal, na.policy="only", na.rm=TRUE)   # max_dist/res = 1 iteration
  writeRaster(r1, file.path(O, paste0("repro_vege_",yr,"_5x5.tiff")), overwrite=TRUE)

  pub <- rast(file.path(B, paste0("vege_",yr,"_5x5.tiff")))
  cat("published grid       : dim", paste(dim(pub),collapse="x"), "ext", paste(signif(as.vector(ext(pub)),12),collapse=","),"\n")
  cat("ext identical:", isTRUE(all.equal(as.vector(ext(r1)), as.vector(ext(pub)))), " dim identical:", identical(dim(r1),dim(pub)),"\n")
  # align repro onto published grid for comparison (near = no resampling if identical)
  rr <- if (identical(dim(r1),dim(pub)) && isTRUE(all.equal(as.vector(ext(r1)),as.vector(ext(pub))))) r1 else resample(r1, pub, method="near")
  a <- values(rr); b <- values(pub)
  cat("repro non-NA:", sum(!is.na(a)), " published non-NA:", sum(!is.na(b)), "\n")
  ok <- !is.na(a) & !is.na(b)
  cat(sprintf("EXACT-CODE agreement: overlap=%d  pixel agreement=%.4f%%\n", sum(ok), 100*mean(a[ok]==b[ok])))
  print(table(repro=a[ok], published=b[ok]))
  cat("Sasa(1): repro", sum(a==1,na.rm=TRUE), " published", sum(b==1,na.rm=TRUE), "\n")
}
