.libPaths(c("/private/tmp/claude-501/-Users-okamoto-NIES-SasaSDMPaper--claude-worktrees-review-summary-ece-ddc69c/98ac7ecf-eafb-49c6-aa2d-9aab2998c13e/scratchpad/Rlib", .libPaths()))
suppressPackageStartupMessages({
  library(xgboost); library(tidyverse); library(terra); library(tidyterra)
  library(tidysdm); library(tidymodels); library(stacks); library(sf)
})
setwd("/Users/okamoto/NIES/SasaSDMPaper/ortho")
OUT <- "/Users/okamoto/NIES/SasaSDMPaper/.claude/worktrees/review-summary-ece-ddc69c/review/restore/out"
dir.create(OUT, recursive=TRUE, showWarnings=FALSE)

vege12 <- rast("data/vege_2012_5x5.tiff"); vege21 <- rast("data/vege_2021_5x5.tiff")
tf <- list.files("data/terrain_features/", full.names=TRUE) %>% str_subset(".tif$") %>% str_subset("twi", negate=TRUE)
terrain <- rast(tf) %>% rename(elevation = tateyamadem_small) %>% resample(vege12)
crs(terrain) <- crs(vege12)

snow_pub <- function(y) rast(sprintf("data/snow/fitted_%d.tiff", y)) %>% resample(vege12) %>% rename(snow = snowmelt_fitted)
snow_fix <- function(y) rast(sprintf("data/snow/fitted_%d.tiff", y)) %>% terra::shift(dx=0.5, dy=-0.5) %>%
  resample(vege12) %>% rename(snow = snowmelt_fitted)

# --- sanity: which shift actually re-registers? compare to raw 2012 warped to vege grid
raw12 <- list.files("data/snow/raw", pattern="2012.*tiff$", full.names=TRUE)
cat("raw 2012 file:", raw12, "\n")
if (length(raw12) == 1) {
  rr <- rast(raw12[1]) %>% resample(vege12, method="near")
  vr <- as.vector(values(rr)); vr[vr <= 0] <- NA
  f0 <- rast("data/snow/fitted_2012.tiff")
  for (s in list(c(0,0), c(0.5,-0.5), c(-0.5,0.5), c(0.5,0.5), c(-0.5,-0.5), c(1,0), c(-1,0), c(0,1), c(0,-1))) {
    fsr <- terra::shift(f0, dx=s[1], dy=s[2])
    fs <- resample(fsr, vege12, method="near")
    vf <- as.vector(values(fs))
    ok <- !is.na(vf) & !is.na(vr)
    cat(sprintf("  shift(%+.1f,%+.1f)  origin=(%.2f,%.2f)  n=%d  cor=%.6f  sd(diff)=%.4f\n",
        s[1], s[2], origin(fsr)[1], origin(fsr)[2], sum(ok),
        cor(vf[ok], vr[ok]), sd(vf[ok]-vr[ok])))
  }
}

sasa12 <- vege12 %>% mutate(sasa = ifelse(layer==1,1,0)) %>% select(sasa)
sasa21 <- vege21 %>% mutate(sasa = ifelse(layer==1,1,0)) %>% select(sasa)
d_all12 <- sasa12 %>% filter(sasa==1) %>% distance() %>% rename(dist=sasa)
polys <- function(r) r %>% filter(sasa==1) %>% stars::st_as_stars() %>% sf::st_as_sf(merge=TRUE) %>%
  mutate(area=sf::st_area(.)) %>% filter(area > units::set_units(5,m^2)) %>% select(sasa) %>% vect()
pol12 <- polys(sasa12); pol21 <- polys(sasa21)
d_pol12 <- terra::rasterize(pol12, d_all12) %>% filter(layer==1) %>% distance() %>% rename(dist=layer)
d_pol21 <- terra::rasterize(pol21, d_all12) %>% filter(layer==1) %>% distance() %>% rename(dist=layer)

tdm <- readRDS("model_stack.rds"); tbm <- readRDS("model_stack_wo_dist.rds")
cols_tdm <- setdiff(colnames(tdm$train), c("sasa","geometry"))
cols_tbm <- setdiff(colnames(tbm$train), c("sasa","geometry"))

mk <- function(stack, cols, layers, nm) {
  env <- do.call(c, layers) %>% filter(elevation < 2560) %>% select(all_of(cols))
  r <- predict_raster(stack, env, type="prob") %>% select(.pred_presence)
  names(r) <- nm; r
}
ar <- function(r){ e <- expanse(ifel(r>0.5,1,NA), unit="m"); if(nrow(e)==0) 0 else e[1,2] }
newly <- function(p21, p30) { n <- c(p30, p21); names(n) <- c("f","b")
  n %>% filter(b<0.5) %>% filter(f>0.5) %>% select(f) %>% expanse(unit="m") %>% pull(area) }
lostf <- function(p30, pol) { pol %>% terra::rasterize(p30) %>% rename(s21=layer) %>% c(p30) %>%
  setNames(c("s21","f")) %>% filter(s21==1) %>% filter(f<0.5) %>% select(f) %>% expanse(unit="m") %>% pull(area) }

res <- list()
runset <- function(tag, snowfn) {
  s21 <- snowfn(2021); s30 <- snowfn(2030)
  tdm21 <- mk(tdm, cols_tdm, list(terrain, s21, d_pol12), "p")
  tdm30 <- mk(tdm, cols_tdm, list(terrain, s30, d_pol21), "p")
  tbm21 <- mk(tbm, cols_tbm, list(terrain, s21), "p")
  tbm30 <- mk(tbm, cols_tbm, list(terrain, s30), "p")
  writeRaster(tdm21, file.path(OUT, paste0("reg_", tag, "_tdm21.tiff")), overwrite=TRUE)
  writeRaster(tdm30, file.path(OUT, paste0("reg_", tag, "_tdm30.tiff")), overwrite=TRUE)
  writeRaster(tbm21, file.path(OUT, paste0("reg_", tag, "_tbm21.tiff")), overwrite=TRUE)
  writeRaster(tbm30, file.path(OUT, paste0("reg_", tag, "_tbm30.tiff")), overwrite=TRUE)
  cat("\n#### snow registration:", tag, "\n")
  cat(sprintf("  TDM 2021 suitable = %10.2f   newly2030 = %9.2f   lost2030 = %8.2f\n",
              ar(tdm21), newly(tdm21, tdm30), lostf(tdm30, pol21)))
  cat(sprintf("  TBM 2021 suitable = %10.2f   newly2030 = %9.2f   lost2030 = %8.2f\n",
              ar(tbm21), newly(tbm21, tbm30), lostf(tbm30, pol21)))
  flush.console()
}
cat("\n### manuscript: TDM 12766 / 4387 / 717 ; TBM 47253 / 27049 / 2257\n")
# baseline check straight off the archived rasters
a_t21 <- rast("data/sasa_pred_tdm_21.tiff"); names(a_t21) <- "p"
a_t30 <- rast("data/sasa_pred_tdm_30.tiff"); names(a_t30) <- "p"
a_b21 <- rast("data/sasa_pred_sdm_21.tiff"); names(a_b21) <- "p"
a_b30 <- rast("data/sasa_pred_sdm_30.tiff"); names(a_b30) <- "p"
cat(sprintf("  ARCHIVED TDM  2021 = %10.2f   newly = %9.2f   lost = %8.2f\n", ar(a_t21), newly(a_t21,a_t30), lostf(a_t30, pol21)))
cat(sprintf("  ARCHIVED TBM  2021 = %10.2f   newly = %9.2f   lost = %8.2f\n", ar(a_b21), newly(a_b21,a_b30), lostf(a_b30, pol21)))
flush.console()

runset("pub", snow_pub)
runset("fix", snow_fix)
