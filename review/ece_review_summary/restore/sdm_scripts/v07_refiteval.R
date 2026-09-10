.libPaths(c("/private/tmp/claude-501/-Users-okamoto-NIES-SasaSDMPaper--claude-worktrees-review-summary-ece-ddc69c/98ac7ecf-eafb-49c6-aa2d-9aab2998c13e/scratchpad/Rlib", .libPaths()))
suppressPackageStartupMessages({library(tidyverse);library(terra);library(tidyterra);library(sf);library(tidysdm);library(yardstick)})
setwd("/Users/okamoto/NIES/SasaSDMPaper/ortho")
O <- "/Users/okamoto/NIES/SasaSDMPaper/.claude/worktrees/review-summary-ece-ddc69c/review/restore/out"
tdm <- rast("data/sasa_pred_tdm_21.tiff"); names(tdm) <- "pub"
ref <- rast(file.path(O,"refit_pol12_p21.tiff")); names(ref) <- "ref"
alt <- rast(file.path(O,"repro_tdm_snow21_distall12.tiff")); names(alt) <- "alt"
tbm <- rast("data/sasa_pred_sdm_21.tiff")
v12 <- rast("data/vege_2012_5x5.tiff"); v21 <- rast("data/vege_2021_5x5.tiff")
s12 <- v12 %>% mutate(sasa=ifelse(layer==1,1,0)) %>% select(sasa)
s21 <- v21 %>% mutate(sasa=ifelse(layer==1,1,0)) %>% select(sasa)
d0 <- s12 %>% filter(sasa==1) %>% distance() %>% rename(dist=sasa)
al <- function(r) terra::resample(r, tdm, method="near")
st <- c(tdm, al(ref), al(alt), al(tbm), al(d0), al(s21)); names(st) <- c("pub","ref","alt","tbm","d","y")
xt <- sf::st_coordinates(readRDS("model_stack.rds")$train$geometry)
xb <- sf::st_coordinates(readRDS("model_stack_wo_dist.rds")$train$geometry)
used <- unique(c(cellFromXY(tdm, xt), cellFromXY(tdm, xb)))
df <- as_tibble(st, cell=TRUE) %>% drop_na() %>% mutate(tr = cell %in% used)
tss <- function(y,p){d<-tibble(t=factor(ifelse(y==1,"presence","absence"),levels=c("presence","absence")),p=p); tidysdm::tss_max(d,t,p,event_level="first")$.estimate}
au  <- function(y,p){d<-tibble(t=factor(ifelse(y==1,"presence","absence"),levels=c("presence","absence")),p=p); yardstick::roc_auc(d,t,p,event_level="first")$.estimate}
for (nm in c("all cells","held out from both","held out & outside 2012 footprint")) {
  ii <- switch(nm, "all cells"=rep(TRUE,nrow(df)), "held out from both"=!df$tr,
               "held out & outside 2012 footprint"=(!df$tr & df$d>0))
  cat(sprintf("%-36s n=%7d prev=%.4f | TSS pub=%.4f refit(pol)=%.4f allpix=%.4f TBM=%.4f | AUC pub=%.4f refit=%.4f allpix=%.4f TBM=%.4f\n",
    nm, sum(ii), mean(df$y[ii]), tss(df$y[ii],df$pub[ii]), tss(df$y[ii],df$ref[ii]), tss(df$y[ii],df$alt[ii]), tss(df$y[ii],df$tbm[ii]),
    au(df$y[ii],df$pub[ii]), au(df$y[ii],df$ref[ii]), au(df$y[ii],df$alt[ii]), au(df$y[ii],df$tbm[ii])))
}
