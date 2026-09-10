# Reproduce vege_2012_5x5.tiff from data/2012_5x5.csv + data/georectified.csv
# using the interpolate() recipe recovered from data_from_server/ortho/.Rhistory.
# NOTE: the original passed fun = terra::modal to terra::focal. On terra 1.9.34
# terra::modal no longer has a numeric method, so we use focal's built-in "modal".
suppressMessages({library(tidyverse); library(sf); library(stars); library(terra)})
D   <- "/Users/okamoto/NIES/SasaSDMPaper/data_from_server/ortho/data/"
OUT <- "/Users/okamoto/NIES/SasaSDMPaper/.claude/worktrees/review-summary-ece-ddc69c/review/restore/"

target <- rast(paste0(D, "vege_2012_5x5.tiff"))

georec <- read_csv(paste0(D, "georectified.csv"),
                   col_types = cols_only(u = "i", v = "i", x = "d", y = "d", z = "d"))
veg <- read_csv(paste0(D, "2012_5x5.csv"), col_types = cols(u = "i", v = "i", data = "d"))
cat("georectified rows:", nrow(georec), " v range:", range(georec$v), "\n")
cat("2012_5x5 rows:", nrow(veg), " v range:", range(veg$v), "\n")

df <- georec %>% left_join(veg, by = c("u", "v"))
cat("joined:", nrow(df), " with data:", sum(!is.na(df$data)), "\n")

pts <- df %>% filter(!is.na(data)) %>%
  st_as_sf(coords = c("x", "y")) %>% st_set_crs(6690) %>% select(data)

tmpl <- st_as_stars(st_bbox(target), dx = 1, dy = 1, values = NA_real_)
st_crs(tmpl) <- 6690
ras <- st_rasterize(pts, template = tmpl)
ras <- rast(ras)
names(ras) <- "data"
cat("rasterized non-NA cells:", global(!is.na(ras), "sum")[1, 1], "\n")

ras_f <- terra::focal(ras, 3, "modal", na.policy = "only", na.rm = TRUE)

cmp <- function(r, tag) {
  b <- c(r, target); names(b) <- c("repro", "orig")
  d <- as.data.frame(b, na.rm = FALSE)
  d <- d[!is.na(d$repro) & !is.na(d$orig), ]
  cat(sprintf("\n%s: overlapping cells = %d, exact agreement = %.4f%%\n",
              tag, nrow(d), 100 * mean(d$repro == d$orig)))
  print(table(orig = d$orig, repro = d$repro))
}
cmp(ras,   "st_rasterize only")
cmp(ras_f, "st_rasterize + 1 focal modal fill")
writeRaster(ras_f, paste0(OUT, "figures/vege_2012_5x5_reproduced_partial.tiff"), overwrite = TRUE)
cat("\nwritten\n")
