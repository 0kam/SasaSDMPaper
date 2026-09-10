suppressMessages({library(tidyverse); library(terra); library(tidyterra); library(sf); library(stars)})
D <- "/Users/okamoto/NIES/SasaSDMPaper/data_from_server/ortho/data/"
vege21 <- rast(paste0(D,"vege_2021_5x5.tiff"))
sasa21_ras <- vege21 %>% mutate(sasa = ifelse(layer == 1, 1, 0)) %>% select(sasa)

sasa_pol_21 <- sasa21_ras %>% filter(sasa == 1) %>% stars::st_as_stars() %>% sf::st_as_sf(merge = TRUE) %>%
  mutate(area = sf::st_area(.)) %>% filter(area > units::set_units(5, m^2)) %>% select(sasa) %>% vect()

run <- function(tag, f21, f30){
  p21 <- rast(paste0(D,f21)); p30 <- rast(paste0(D,f30))
  n21 <- names(p21); n30 <- names(p30)
  cat("\n==",tag,"== layers:",n21,"/",n30,"\n")
  names(p21) <- "pred_sasa_21"; names(p30) <- "pred_sasa_30"
  a21 <- p21 %>% mutate(pred_sasa_21 = ifelse(pred_sasa_21 > 0.5,1,0)) %>% filter(pred_sasa_21==1) %>% expanse()
  cat(" suitable 2021 (HS>0.5):", format(round(a21$area),big.mark=","), "m2\n")
  a30 <- p30 %>% mutate(pred_sasa_30 = ifelse(pred_sasa_30 > 0.5,1,0)) %>% filter(pred_sasa_30==1) %>% expanse()
  cat(" suitable 2030 (HS>0.5):", format(round(a30$area),big.mark=","), "m2\n")
  anew <- c(p30, p21) %>% filter(pred_sasa_21 < 0.5) %>% filter(pred_sasa_30 > 0.5) %>% select(pred_sasa_30) %>% expanse()
  cat(" newly suitable by 2030:", format(round(anew$area),big.mark=","), "m2   (",round(100*anew$area/a21$area,1),"% of 2021 suitable )\n")
  alost <- sasa_pol_21 %>% terra::rasterize(p30) %>% rename(sasa_21 = layer) %>% c(p30) %>%
    filter(sasa_21 == 1) %>% filter(pred_sasa_30 < 0.5) %>% select(pred_sasa_30) %>% expanse()
  cat(" 2021 Sasa becoming unsuitable:", format(round(alost$area),big.mark=","), "m2\n")
}
run("TBM (sasa_pred_sdm_*)", "sasa_pred_sdm_21.tiff", "sasa_pred_sdm_30.tiff")
run("TDM (sasa_pred_tdm_*)", "sasa_pred_tdm_21.tiff", "sasa_pred_tdm_30.tiff")
run("ALT (sasa_pred_sdm_dist_*)", "sasa_pred_sdm_dist_21.tiff", "sasa_pred_sdm_dist_30.tiff")
