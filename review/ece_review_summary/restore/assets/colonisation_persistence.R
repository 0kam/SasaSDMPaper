# Reviewer-2 answer prototype: a COMMON EVALUATION DOMAIN with an explicit
# colonisation / persistence decomposition, using the paper's own predictors.
#   colonisation: among cells NOT Sasa in 2012 -> was it Sasa in 2021?
#   persistence : among cells     Sasa in 2012 -> was it still Sasa in 2021?
# Distance is defined exactly as in sdm_include_distance.R (dist to 2012 Sasa polygons > 5 m2).
suppressPackageStartupMessages({library(tidyverse);library(tidysdm);library(tidymodels);library(terra);
  library(spatialsample);library(ranger);library(sf);library(yardstick);library(tidyterra)})
D <- "/Users/okamoto/NIES/SasaSDMPaper/data_from_server/ortho/"
O <- "/Users/okamoto/NIES/SasaSDMPaper/.claude/worktrees/review-summary-ece-ddc69c/review/restore/assets/"
set.seed(1)

vege12 <- rast(paste0(D,"data/vege_2012_5x5.tiff")); vege21 <- rast(paste0(D,"data/vege_2021_5x5.tiff"))
terrain <- list.files(paste0(D,"data/terrain_features/"), full.names=TRUE) %>% str_subset(".tif$") %>%
  rast() %>% rename(elevation = tateyamadem_small) %>% resample(vege12)
snow_21 <- rast(paste0(D,"data/snow/fitted_2021.tiff")) %>% resample(vege12) %>% rename(snow = snowmelt_fitted)
# JGD2000 vs JGD2011 UTM53N differ by < 0.1 m; the original scripts treat them as identical. Force to match.
crs(terrain) <- crs(vege12); crs(snow_21) <- crs(vege12)

sasa12 <- vege12 %>% mutate(sasa = ifelse(layer==1,1,0)) %>% select(sasa)
sasa21 <- vege21 %>% mutate(sasa = ifelse(layer==1,1,0)) %>% select(sasa)

# distance to 2012 Sasa polygons > 5 m2  (identical construction to sdm_include_distance.R)
pol12 <- sasa12 %>% filter(sasa==1) %>% stars::st_as_stars() %>% sf::st_as_sf(merge=TRUE) %>%
  mutate(area = sf::st_area(.)) %>% filter(area > units::set_units(5, m^2)) %>% select(sasa) %>% vect()
dist12 <- terra::rasterize(pol12, sasa12) %>% filter(layer==1) %>% distance() %>% rename(dist = layer)
cat("2012 Sasa polygons > 5 m2:", nrow(pol12), "\n")

stk <- c(sasa12 %>% rename(s12=sasa), sasa21 %>% rename(s21=sasa), terrain, snow_21, dist12)
smask <- terrain["elevation"] %>% terra::aggregate(fact = 5)

# build the two domains
dom_col <- stk %>% filter(elevation < 2560) %>% filter(s12 == 0)
dom_per <- stk %>% filter(elevation < 2560) %>% filter(s12 == 1)

mk <- function(dom, label) {
  cat("\n=================", label, "=================\n")
  cat("domain cells:", sum(!is.na(values(dom$s21))), "  events (s21==1):", sum(values(dom$s21)==1, na.rm=TRUE), "\n")
  pts <- dom %>% select(s21) %>% as.points() %>% as_sf()
  ev  <- pts %>% filter(s21 == 1)
  nev <- pts %>% filter(s21 == 0) %>% thin_by_cell(smask)
  cat("after thinning non-events on the 5 m sampling mask:", nrow(ev), "events,", nrow(nev), "non-events\n")
  p <- bind_rows(ev, nev) %>% bind_cols(terra::extract(c(terrain, snow_21, dist12), ., ID=FALSE)) %>% drop_na()
  p$y <- factor(ifelse(p$s21==1, "yes", "no"), levels=c("yes","no"))
  cat("modelling frame n =", nrow(p), " prevalence =", sprintf("%.3f", mean(p$y=="yes")), "\n")
  cat("dist (m) summary in this domain:\n"); print(summary(p$dist))
  p
}
p_col <- mk(dom_col, "COLONISATION domain: cells NOT Sasa in 2012")
p_per <- mk(dom_per, "PERSISTENCE  domain: cells     Sasa in 2012")
saveRDS(p_col, paste0(O,"out/frame_colonisation.rds")); saveRDS(p_per, paste0(O,"out/frame_persistence.rds"))

envvars <- c("elevation","slope","aspect","roughness","TPI","TRI","twi","snow")
evalfit <- function(p, vars, label) {
  set.seed(1); cv <- spatial_block_cv(p, v = 4)
  d <- st_drop_geometry(p)
  pr <- rep(NA_real_, nrow(d))
  for (s in cv$splits) {
    tri <- s$in_id; tei <- setdiff(seq_len(nrow(d)), tri)
    m <- ranger(x = d[tri, vars, drop=FALSE], y = d$y[tri], probability = TRUE, num.threads = 12, seed = 1)
    pr[tei] <- predict(m, d[tei, vars, drop=FALSE])$predictions[,"yes"]
  }
  tb <- tibble(truth = d$y, .pred_yes = pr) %>% drop_na()
  auc <- roc_auc(tb, truth, .pred_yes)$.estimate
  # TSS_max over thresholds
  th <- sort(unique(round(tb$.pred_yes, 4)))
  tss <- max(sapply(th, function(t){
    pp <- tb$.pred_yes >= t
    se <- sum(pp & tb$truth=="yes")/sum(tb$truth=="yes")
    sp <- sum(!pp & tb$truth=="no")/sum(tb$truth=="no")
    se + sp - 1 }))
  cat(sprintf("%-34s | spatial-block OOF AUC = %.4f   TSS_max = %.4f   (n=%d)\n", label, auc, tss, nrow(tb)))
  invisible(c(auc=auc, tss=tss))
}
cat("\n### COMMON-DOMAIN nested comparison -- COLONISATION\n")
c1 <- evalfit(p_col, envvars,            "environment only")
c2 <- evalfit(p_col, c(envvars,"dist"),  "environment + distance")
c3 <- evalfit(p_col, "dist",             "distance only")
cat("\n### COMMON-DOMAIN nested comparison -- PERSISTENCE\n")
q1 <- evalfit(p_per, envvars,            "environment only")
q2 <- evalfit(p_per, c(envvars,"dist"),  "environment + distance")
q3 <- evalfit(p_per, "dist",             "distance only")
