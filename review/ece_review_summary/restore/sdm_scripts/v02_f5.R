.libPaths(c("/private/tmp/claude-501/-Users-okamoto-NIES-SasaSDMPaper--claude-worktrees-review-summary-ece-ddc69c/98ac7ecf-eafb-49c6-aa2d-9aab2998c13e/scratchpad/Rlib", .libPaths()))
suppressPackageStartupMessages({
  library(tidyverse); library(terra); library(tidyterra); library(sf); library(tidysdm); library(yardstick)
})
setwd("/Users/okamoto/NIES/SasaSDMPaper/ortho")

vege12 <- rast("data/vege_2012_5x5.tiff"); vege21 <- rast("data/vege_2021_5x5.tiff")
sasa12 <- vege12 %>% mutate(sasa = ifelse(layer==1,1,0)) %>% select(sasa)
sasa21 <- vege21 %>% mutate(sasa = ifelse(layer==1,1,0)) %>% select(sasa)
d_all12 <- sasa12 %>% filter(sasa==1) %>% distance() %>% rename(dist=sasa)
polys <- function(r) r %>% filter(sasa==1) %>% stars::st_as_stars() %>% sf::st_as_sf(merge=TRUE) %>%
  mutate(area=sf::st_area(.)) %>% filter(area > units::set_units(5,m^2)) %>% select(sasa) %>% vect()
d_pol12 <- terra::rasterize(polys(sasa12), d_all12) %>% filter(layer==1) %>% distance() %>% rename(dist=layer)

tdm21 <- rast("data/sasa_pred_tdm_21.tiff"); names(tdm21) <- "hs_tdm"
tbm21 <- rast("data/sasa_pred_sdm_21.tiff"); names(tbm21) <- "hs_tbm"
cat("TBM 2021 raster:", basename(sources(tbm21)), " dims", paste(dim(tbm21),collapse="x"), "\n")

# align everything to the TDM prediction grid
al <- function(r) terra::resample(r, tdm21, method="near")
st <- c(tdm21, al(tbm21), al(d_all12), al(d_pol12), al(sasa21), al(sasa12 %>% rename(sasa12=sasa)))
names(st) <- c("hs_tdm","hs_tbm","d_all12","d_pol12","sasa21","sasa12")
df <- as_tibble(st) %>% drop_na()
cat("\ncells with both HS defined:", nrow(df), "\n")

suit <- df %>% filter(hs_tdm > 0.5)
cat("\n### F-5 : TDM 2021 suitable cells\n")
cat("  n suitable cells (hs_tdm>0.5):", nrow(suit), "\n")
for (v in c("d_all12","d_pol12")) {
  z <- sum(suit[[v]] == 0); cat(sprintf("    of which %s == 0 : %d  (%.1f%%)\n", v, z, 100*z/nrow(suit)))
}
cat(sprintf("  mean HS where d_all12==0 : %.4f   elsewhere : %.4f\n",
            mean(df$hs_tdm[df$d_all12==0]), mean(df$hs_tdm[df$d_all12>0])))
cat(sprintf("  mean HS where d_pol12==0 : %.4f   elsewhere : %.4f\n",
            mean(df$hs_tdm[df$d_pol12==0]), mean(df$hs_tdm[df$d_pol12>0])))
cat(sprintf("  n cells d_all12==0 (2012 Sasa footprint) : %d\n", sum(df$d_all12==0)))

ar <- function(r){ e <- expanse(ifel(r>0.5,1,NA), unit="m"); if(nrow(e)==0) 0 else e[1,2] }
out12 <- terra::mask(tdm21, al(d_all12), maskvalues=0)
cat(sprintf("  TDM suitable area OUTSIDE the 2012 all-pixel footprint : %.2f m2\n", ar(out12)))
out12p <- terra::mask(tdm21, al(d_pol12), maskvalues=0)
cat(sprintf("  TDM suitable area OUTSIDE the 2012 polygon>5m2 footprint: %.2f m2\n", ar(out12p)))
cat(sprintf("  TDM suitable area TOTAL                                 : %.2f m2\n", ar(tdm21)))
cat(sprintf("  TBM suitable area TOTAL                                 : %.2f m2  (manuscript 47253)\n", ar(tbm21)))

# ---- common-domain discrimination -------------------------------------------
tssmax <- function(truth, prob) {
  d <- tibble(truth = factor(ifelse(truth==1,"presence","absence"), levels=c("presence","absence")),
              p = prob)
  tidysdm::tss_max(d, truth = truth, p, event_level = "first")$.estimate
}
aucv <- function(truth, prob) {
  d <- tibble(truth = factor(ifelse(truth==1,"presence","absence"), levels=c("presence","absence")), p = prob)
  yardstick::roc_auc(d, truth = truth, p, event_level = "first")$.estimate
}
cat("\n### common-domain discrimination against observed 2021 Sasa (cell level)\n")
dom <- list(
  "all cells"                        = rep(TRUE, nrow(df)),
  "outside 2012 footprint (d_all12>0)" = df$d_all12 > 0,
  "inside 2012 footprint (d_all12==0)" = df$d_all12 == 0
)
for (nm in names(dom)) {
  ii <- dom[[nm]]
  cat(sprintf("  %-36s n=%7d  prevalence=%.4f  TSSmax TDM=%.4f  TBM=%.4f   AUC TDM=%.4f  TBM=%.4f\n",
              nm, sum(ii), mean(df$sasa21[ii]),
              tssmax(df$sasa21[ii], df$hs_tdm[ii]), tssmax(df$sasa21[ii], df$hs_tbm[ii]),
              aucv(df$sasa21[ii], df$hs_tdm[ii]),  aucv(df$sasa21[ii], df$hs_tbm[ii])))
}
saveRDS(df, "/private/tmp/claude-501/-Users-okamoto-NIES-SasaSDMPaper--claude-worktrees-review-summary-ece-ddc69c/98ac7ecf-eafb-49c6-aa2d-9aab2998c13e/scratchpad/sdm2/f5_df.rds")
