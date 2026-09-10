.libPaths(c("/private/tmp/claude-501/-Users-okamoto-NIES-SasaSDMPaper--claude-worktrees-review-summary-ece-ddc69c/98ac7ecf-eafb-49c6-aa2d-9aab2998c13e/scratchpad/Rlib", .libPaths()))
suppressPackageStartupMessages({
  library(xgboost); library(tidyverse); library(terra); library(tidyterra); library(sf)
  library(tidysdm); library(tidymodels); library(stacks); library(glmnet)
})
setwd("/Users/okamoto/NIES/SasaSDMPaper/ortho")
SCR <- "/private/tmp/claude-501/-Users-okamoto-NIES-SasaSDMPaper--claude-worktrees-review-summary-ece-ddc69c/98ac7ecf-eafb-49c6-aa2d-9aab2998c13e/scratchpad/sdm2"
OUT <- "/Users/okamoto/NIES/SasaSDMPaper/.claude/worktrees/review-summary-ece-ddc69c/review/restore/out"

s <- readRDS("model_stack.rds")
cf <- s$coefs %>% tidy() %>% filter(estimate != 0)
b0 <- cf$estimate[cf$term == "(Intercept)"]
bm <- cf %>% filter(term != "(Intercept)") %>%
  mutate(member = str_remove(term, "^\\.pred_absence_"))
print(bm)

manual <- function(memberfits, newdata, coefs, intercept) {
  P <- sapply(coefs$member, function(m) predict(memberfits[[m]], newdata, type="prob")$.pred_absence)
  eta <- intercept + as.vector(P %*% coefs$estimate)
  1 - 1/(1 + exp(-eta))          # P(presence)
}
# ---- verify the manual linear predictor against predict.model_stack -----------
nd <- s$train %>% st_drop_geometry() %>% as_tibble() %>% slice(1:2000)
p_stack <- predict(s, nd, type="prob")$.pred_presence
p_man   <- manual(s$member_fits, nd, bm, b0)
cat(sprintf("manual vs stacks predict: maxabs=%.3e  cor=%.8f\n", max(abs(p_stack-p_man)), cor(p_stack,p_man)))

# ---- refit the same members on a distance-consistent training frame ----------
prep <- readRDS(file.path(SCR, "prep_tdm_notwi_pol12.rds"))
tr_new <- prep$tr
cat("new training frame (dist = 2012 polygons >5 m2): n =", nrow(tr_new),
    " presences =", sum(tr_new$sasa == "presence"), "\n")
newfits <- list()
for (m in bm$member) {
  wf <- s$member_fits[[m]]
  t0 <- Sys.time()
  newfits[[m]] <- fit(wf, data = tr_new)
  cat("  refit", m, "in", round(as.numeric(Sys.time()-t0, units="secs"),1), "s\n"); flush.console()
}
saveRDS(newfits, file.path(SCR, "newfits_pol12.rds"))

# ---- predict 2021 with the distance-consistent model -------------------------
vege12 <- rast("data/vege_2012_5x5.tiff"); vege21 <- rast("data/vege_2021_5x5.tiff")
tf <- list.files("data/terrain_features/", full.names=TRUE) %>% str_subset(".tif$") %>% str_subset("twi", negate=TRUE)
terrain <- rast(tf) %>% rename(elevation = tateyamadem_small) %>% resample(vege12); crs(terrain) <- crs(vege12)
snow21 <- rast("data/snow/fitted_2021.tiff") %>% resample(vege12) %>% rename(snow=snowmelt_fitted)
snow30 <- rast("data/snow/fitted_2030.tiff") %>% resample(vege12) %>% rename(snow=snowmelt_fitted)
sasa12 <- vege12 %>% mutate(sasa=ifelse(layer==1,1,0)) %>% select(sasa)
sasa21 <- vege21 %>% mutate(sasa=ifelse(layer==1,1,0)) %>% select(sasa)
d0 <- sasa12 %>% filter(sasa==1) %>% distance() %>% rename(dist=sasa)
polys <- function(r) r %>% filter(sasa==1) %>% stars::st_as_stars() %>% sf::st_as_sf(merge=TRUE) %>%
  mutate(area=sf::st_area(.)) %>% filter(area>units::set_units(5,m^2)) %>% select(sasa) %>% vect()
pol12 <- polys(sasa12); pol21 <- polys(sasa21)
d_pol12 <- terra::rasterize(pol12, d0) %>% filter(layer==1) %>% distance() %>% rename(dist=layer)
d_pol21 <- terra::rasterize(pol21, d0) %>% filter(layer==1) %>% distance() %>% rename(dist=layer)
cols <- setdiff(colnames(s$train), c("sasa","geometry"))

predr <- function(distr, snow, nm) {
  env <- c(terrain, snow, distr) %>% filter(elevation < 2560) %>% select(all_of(cols))
  df <- as_tibble(env, cell = TRUE)
  ok <- complete.cases(df %>% select(all_of(cols)))
  out <- rast(env, nlyrs = 1); values(out) <- NA_real_
  out[df$cell[ok]] <- manual(newfits, df[ok, cols], bm, b0)
  names(out) <- nm
  writeRaster(out, file.path(OUT, paste0("refit_pol12_", nm, ".tiff")), overwrite=TRUE)
  out
}
p21 <- predr(d_pol12, snow21, "p21")
p30 <- predr(d_pol21, snow30, "p30")

ar <- function(r){ e <- expanse(ifel(r>0.5,1,NA), unit="m"); if(nrow(e)==0) 0 else e[1,2] }
newly <- function(a,b){ n <- c(b,a); names(n) <- c("f","bb")
  n %>% filter(bb<0.5) %>% filter(f>0.5) %>% select(f) %>% expanse(unit="m") %>% pull(area) }
lostf <- function(p, pol) pol %>% terra::rasterize(p) %>% rename(s=layer) %>% c(p) %>% setNames(c("s","f")) %>%
  filter(s==1) %>% filter(f<0.5) %>% select(f) %>% expanse(unit="m") %>% pull(area)
cat("\n### TDM refit with dist = 2012 polygons >5 m2 in BOTH training and prediction\n")
cat(sprintf("  2021 suitable = %.2f m2   (published, mismatched: 12766.04)\n", ar(p21)))
cat(sprintf("  newly suitable 2030 = %.2f m2   (published 4386.61)\n", newly(p21, p30)))
cat(sprintf("  lost by 2030 = %.2f m2   (published 716.61)\n", lostf(p30, pol21)))
