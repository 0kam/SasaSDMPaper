.libPaths(c("/private/tmp/claude-501/-Users-okamoto-NIES-SasaSDMPaper--claude-worktrees-review-summary-ece-ddc69c/98ac7ecf-eafb-49c6-aa2d-9aab2998c13e/scratchpad/Rlib", .libPaths()))
suppressPackageStartupMessages({
  library(tidyverse); library(tidysdm); library(tidymodels); library(terra)
  library(tidyterra); library(sf); library(stacks); library(xgboost)
})
setwd("/Users/okamoto/NIES/SasaSDMPaper/ortho")
SCR <- "/private/tmp/claude-501/-Users-okamoto-NIES-SasaSDMPaper--claude-worktrees-review-summary-ece-ddc69c/98ac7ecf-eafb-49c6-aa2d-9aab2998c13e/scratchpad/sdm2"

vege12 <- rast("data/vege_2012_5x5.tiff")
vege21 <- rast("data/vege_2021_5x5.tiff")
cat("CRS vege12:", crs(vege12, describe=TRUE)$code, crs(vege12, describe=TRUE)$name, "\n")

tf <- list.files("data/terrain_features/", full.names = T) %>% str_subset(".tif$")
cat("terrain glob TODAY:", paste(basename(tf), collapse=", "), "\n")
r1 <- rast(tf[1]); cat("CRS terrain:", crs(r1, describe=TRUE)$code, crs(r1, describe=TRUE)$name, "\n")
sn <- rast("data/snow/fitted_2021.tiff"); cat("CRS snow:", crs(sn, describe=TRUE)$code, "\n")
cat("origin vege12:", paste(origin(vege12), collapse=","), " res:", paste(res(vege12), collapse=","), "\n")
cat("origin terrain:", paste(origin(r1), collapse=","), " res:", paste(res(r1), collapse=","), "\n")
cat("origin snow:", paste(origin(sn), collapse=","), " res:", paste(res(sn), collapse=","), "\n")

mk_terrain <- function(files) {
  r <- rast(files) %>% rename(elevation = tateyamadem_small) %>% resample(vege12)
  terra::crs(r) <- terra::crs(vege12)   # terrain carries EPSG:3099, vege 6690; relabel, do not warp
  r
}
terrain_twi  <- mk_terrain(tf)
terrain_notwi<- mk_terrain(tf %>% str_subset("twi", negate = TRUE))
cat("layer order WITH twi:", paste(names(terrain_twi), collapse=", "), "\n")
cat("layer order NO   twi:", paste(names(terrain_notwi), collapse=", "), "\n")

snow_21 <- rast("data/snow/fitted_2021.tiff") %>% resample(vege12) %>% rename(snow = snowmelt_fitted)
sasa12_ras <- vege12 %>% mutate(sasa = ifelse(layer == 1, 1, 0)) %>% select(sasa)
sasa21_ras <- vege21 %>% mutate(sasa = ifelse(layer == 1, 1, 0)) %>% select(sasa)

dist_all12 <- sasa12_ras %>% filter(sasa == 1) %>% distance() %>% rename(dist = sasa)
polys <- function(r) r %>% filter(sasa == 1) %>% stars::st_as_stars() %>% sf::st_as_sf(merge=TRUE) %>%
  mutate(area = sf::st_area(.)) %>% filter(area > units::set_units(5, m^2)) %>% select(sasa) %>% vect()
pol12 <- polys(sasa12_ras)
dist_pol12 <- terra::rasterize(pol12, dist_all12) %>% filter(layer == 1) %>% distance() %>% rename(dist = layer)

arch_tdm <- readRDS("model_stack.rds")$train %>% st_drop_geometry() %>% as_tibble()
arch_tbm <- readRDS("model_stack_wo_dist.rds")$train %>% st_drop_geometry() %>% as_tibble()

run_case <- function(terrain, distr, tag, arch) {
  sampling_mask <- terrain["elevation"] %>% terra::aggregate(fact = 5)
  layers <- list(terrain, snow_21); if (!is.null(distr)) layers <- c(layers, list(distr))
  env <- do.call(c, layers)
  df1 <- sasa21_ras %>% select(sasa) %>% as.points() %>% as_sf() %>%
    bind_cols(terra::extract(env, ., ID = FALSE)) %>%
    mutate(sasa = ifelse(sasa == 1, 'presence','absence') %>% factor(levels=c('presence','absence'))) %>%
    drop_na()
  df <- bind_rows(df1 %>% filter(sasa=="absence") %>% thin_by_cell(sampling_mask),
                  df1 %>% filter(sasa=="presence")) %>% filter(elevation < 2560)
  if (!is.null(distr)) df <- df %>% filter(dist > 0)
  set.seed(1); ini <- spatial_initial_split(df, prop = 0.2, spatial_block_cv)
  tr <- training(ini); te <- testing(ini)
  set.seed(1); cv <- tr %>% spatial_block_cv(v = 4)
  cat("\n### CASE", tag, "\n")
  cat("  nrow(df_full):", nrow(df), " pres:", sum(df$sasa=="presence"), " abs:", sum(df$sasa=="absence"), "\n")
  cat("  nrow(train):", nrow(tr), " pres:", sum(tr$sasa=="presence"), " abs:", sum(tr$sasa=="absence"), "\n")
  cat("  nrow(test) :", nrow(te), "\n")
  cat("  cols:", paste(setdiff(colnames(tr),"geometry"), collapse=", "), "\n")
  cat("  fold analysis sizes:", paste(sapply(cv$splits, function(s) length(s$in_id)), collapse=", "), "\n")
  cat("  ARCHIVED train n:", nrow(arch), " pres:", sum(arch$sasa=="presence"), "\n")
  common <- intersect(setdiff(colnames(tr),c("geometry")), colnames(arch))
  if (nrow(tr) == nrow(arch)) {
    a <- tr %>% st_drop_geometry() %>% as_tibble() %>% select(all_of(common)) %>% arrange(across(everything()))
    b <- arch %>% select(all_of(common)) %>% arrange(across(everything()))
    ok <- isTRUE(all.equal(a, b, tolerance = 1e-9))
    cat("  >>> IDENTICAL to archived train (sorted, cols", paste(common,collapse=","), "):", ok, "\n")
    if (!ok) {
      for (cc in setdiff(common,"sasa")) cat("      ", cc, "maxdiff:", max(abs(sort(a[[cc]])-sort(b[[cc]]))), "\n")
    }
  }
  saveRDS(list(df=df, tr=tr, te=te, cv=cv), file.path(SCR, paste0("prep_", tag, ".rds")))
  invisible(NULL)
}

run_case(terrain_notwi, NULL,       "tbm_notwi",       arch_tbm)
run_case(terrain_twi,   NULL,       "tbm_twi",         arch_tbm)
run_case(terrain_notwi, dist_all12, "tdm_notwi_all12", arch_tdm)
run_case(terrain_notwi, dist_pol12, "tdm_notwi_pol12", arch_tdm)
run_case(terrain_twi,   dist_all12, "tdm_twi_all12",   arch_tdm)
