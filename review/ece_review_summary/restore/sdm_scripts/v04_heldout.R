.libPaths(c("/private/tmp/claude-501/-Users-okamoto-NIES-SasaSDMPaper--claude-worktrees-review-summary-ece-ddc69c/98ac7ecf-eafb-49c6-aa2d-9aab2998c13e/scratchpad/Rlib", .libPaths()))
suppressPackageStartupMessages({
  library(tidyverse); library(terra); library(tidyterra); library(sf); library(tidysdm); library(yardstick)
})
setwd("/Users/okamoto/NIES/SasaSDMPaper/ortho")
tdm21 <- rast("data/sasa_pred_tdm_21.tiff"); names(tdm21) <- "hs_tdm"
tbm21 <- rast("data/sasa_pred_sdm_21.tiff"); names(tbm21) <- "hs_tbm"
vege12 <- rast("data/vege_2012_5x5.tiff"); vege21 <- rast("data/vege_2021_5x5.tiff")
sasa12 <- vege12 %>% mutate(sasa = ifelse(layer==1,1,0)) %>% select(sasa)
sasa21 <- vege21 %>% mutate(sasa = ifelse(layer==1,1,0)) %>% select(sasa)
d_all12 <- sasa12 %>% filter(sasa==1) %>% distance() %>% rename(dist=sasa)

al <- function(r) terra::resample(r, tdm21, method="near")
st <- c(tdm21, al(tbm21), al(d_all12), al(sasa21))
names(st) <- c("hs_tdm","hs_tbm","d_all12","sasa21")

# cells used in training by EITHER archived model
xy_tdm <- sf::st_coordinates(readRDS("model_stack.rds")$train$geometry)
xy_tbm <- sf::st_coordinates(readRDS("model_stack_wo_dist.rds")$train$geometry)
cells_used <- unique(c(cellFromXY(tdm21, xy_tdm), cellFromXY(tdm21, xy_tbm)))
cat("training cells (union of both models):", length(cells_used), "\n")

df <- as_tibble(st, cell = TRUE) %>% drop_na()
df$in_train <- df$cell %in% cells_used
cat("cells with both HS:", nrow(df), "  of which in some training set:", sum(df$in_train), "\n")

tssmax <- function(truth, prob) {
  d <- tibble(truth = factor(ifelse(truth==1,"presence","absence"), levels=c("presence","absence")), p = prob)
  tidysdm::tss_max(d, truth = truth, p, event_level="first")$.estimate }
aucv <- function(truth, prob) {
  d <- tibble(truth = factor(ifelse(truth==1,"presence","absence"), levels=c("presence","absence")), p = prob)
  yardstick::roc_auc(d, truth = truth, p, event_level="first")$.estimate }

dom <- list(
  "held out from BOTH models"                       = !df$in_train,
  "held out from BOTH  &  outside 2012 footprint"   = !df$in_train & df$d_all12 > 0,
  "held out from BOTH  &  inside  2012 footprint"   = !df$in_train & df$d_all12 == 0
)
for (nm in names(dom)) {
  ii <- dom[[nm]]
  cat(sprintf("  %-46s n=%7d prev=%.4f  TSSmax TDM=%.4f TBM=%.4f   AUC TDM=%.4f TBM=%.4f\n",
      nm, sum(ii), mean(df$sasa21[ii]),
      tssmax(df$sasa21[ii], df$hs_tdm[ii]), tssmax(df$sasa21[ii], df$hs_tbm[ii]),
      aucv(df$sasa21[ii], df$hs_tdm[ii]),  aucv(df$sasa21[ii], df$hs_tbm[ii])))
}
