setwd("~/doctoral_thesis/chap2/ortho/")
library(tidyverse)
library(tidysdm)
library(tidymodels)
library(terra)
library(tidyterra)
library(DALEX)
library(stacks)

vege12 <- rast("data/vege_2012_5x5.tiff")
vege21 <- rast("data/vege_2021_5x5.tiff")

terrain <- list.files("data/terrain_features/", full.names = T) %>%
  str_subset(".tif$") %>%
  rast() %>%
  rename(
    elevation = tateyamadem_small,
  ) %>%
  resample(vege12)

# snow_fitted contains predicterd snow melt timing fitted to lm(snow_melt ~ year)
snow_12 <- rast("data/snow/fitted_2012.tiff")%>%
  resample(vege12) %>%
  rename(snow = snowmelt_fitted)

snow_21 <- rast("data/snow/fitted_2021.tiff")%>%
  resample(vege12) %>%
  rename(snow = snowmelt_fitted)

# ------------------------------------------------------------------------------
# Modeling sasa increase
sasa12_ras <- vege12 %>%
  mutate(sasa = ifelse(layer == 1, 1, 0)) %>%
  select(sasa)

sasa12_ras %>% terra::as.polygons() %>%
  filter()

sasa21_ras <- vege21 %>%
  mutate(sasa = ifelse(layer == 1, 1, 0)) %>%
  select(sasa)

# 最寄りのササからの距離を説明変数に追加
sasa_dist <- sasa12_ras %>%
  filter(sasa == 1) %>%
  distance() %>%
  rename(dist = sasa)

sampling_mask <- terrain["elevation"] %>%
  terra::aggregate(fact = 5)

df_21_1 <- sasa21_ras %>%
  select(sasa) %>%
  as.points() %>%
  as_sf() %>%
  bind_cols(terra::extract(c(terrain, snow_21, sasa_dist), ., ID = FALSE)) %>%
  mutate(sasa = ifelse(sasa == 1, 'presence', 'absence') %>%
           factor(levels = c('presence', 'absence')))%>%
  drop_na()

df_21 <- bind_rows(
  df_21_1 %>%
    filter(sasa == "absence") %>%
    thin_by_cell(sampling_mask),
  df_21_1 %>%
    filter(sasa == "presence")
) %>%
  filter(elevation < 2560) %>%
  filter(dist > 0)

set.seed(1)
df_initial <- spatial_initial_split(
  df_21, prop = 0.2, spatial_block_cv
)

p_init <- autoplot(df_initial) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  
  
p_init
ggsave(
  "./figures/initial_split_dist.png",
  p_init,
  width = 5, height = 4
)

check_splits_balance(df_initial, sasa)

df_train <- training(df_initial)

rec <- recipe(df_train, formula = sasa ~ .)

set.seed(1)
cv <- df_train %>%
  spatial_block_cv(v = 4)

p_cv <- autoplot(cv)  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  "./figures/cv_dist.png",
  p_cv,
  width = 5, height = 4
)

models <- workflow_set(
  preproc = list(default = rec),
  models = list(
    rf = sdm_spec_rf() %>%
      set_args(
        num.threads = 18
      ),
    gam = sdm_spec_gam() %>%
      set_args(
        num.threads = 18
      ),
    maxent = sdm_spec_maxent() %>%
      set_args(
        num.threads = 18
      ),
    xgb = sdm_spec_boost_tree() %>%
      set_args(
        num.threads = 18
      )
  ),
  cross = TRUE
) %>%
  # # set formula for gams
  update_workflow_model("default_gam",
                        spec = sdm_spec_gam(),
                        formula = gam_formula(rec)) %>%
  option_add(control = control_ensemble_grid())

models <- models %>%
  workflow_map(
    "tune_grid", resamples = cv,
    metrics = metric_set(tss_max), grid = 18)

saveRDS(models, "models.rds")
#
models <- readRDS("models.rds")

p_models <- autoplot(models) +
  labs(y = "TSS", title = "Performance of TDM") +
  scale_color_discrete(labels = c("GBT", "GAM", "MaxEnt", "RF")) +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5)
  ) +
  guides(shape = "none")

ggsave("figures/model_performance_tdm.png", p_models, 
       width = 6, height = 4)

model_stack <- stacks() %>%
  add_candidates(models) %>%
  blend_predictions(metric = metric_set(tss_max)) %>%
  fit_members()

saveRDS(model_stack, "model_stack.rds")
model_stack <- readRDS("model_stack.rds")

autoplot(model_stack, type="weights")

df_test <- testing(df_initial)
df_test_pred <- df_test %>%
  bind_cols(predict(model_stack, ., type = "prob"))

metrics <- sdm_metric_set()(data = df_test_pred, truth = sasa, .pred_presence)
write_csv(metrics, "tss_tdm.csv")

# -------------------------------------
# model_ensemble <- simple_ensemble() %>%
#   add_member(models, metric = "tss_max")
# autoplot(model_ensemble)
# model_ensemble %>% collect_metrics()

sasa_double <- df_train %>%
  mutate(sasa = (as.numeric(sasa) - 2) * -1) %>%
  pull(sasa)

library(DALEXtra)

explainer <- model_stack %>%
  explain_tidymodels(
    data = df_train %>% as_tibble() %>% select(-c(sasa, geometry)), 
    y = sasa_double, 
    type = "classification",
    predict_function = stacks::predict.model_stack,
    predict_function_target_column = "presense"
  )

my_tss <- function(observed, predicted) {
  predicted <- predicted %>%
    mutate(.pred = ifelse(.pred_class == "presence", 1, 0)) %>%
    pull(.pred)
  tss_max_vec(factor(observed, levels = c("1", "0")), predicted)
}

attr(my_tss, "loss_name") <- "TSS"

feature_importance <- model_parts(explainer = explainer, loss_function = my_tss)

p_importance <- feature_importance %>%
  as_tibble() %>%
  pivot_wider(id_cols = permutation, 
              names_from = variable,
              values_from = dropout_loss) %>%
  select(-`_baseline_`) %>%
  pivot_longer(cols = -c(permutation, `_full_model_`),
               names_to = "variable", values_to = "TSS_loss") %>%
  mutate(TSS_loss = `_full_model_` - TSS_loss) %>%
  group_by(variable) %>%
  ggplot(aes(x = reorder(variable, -TSS_loss), y = TSS_loss)) +
  geom_boxplot() + 
  labs(
    title = "Variable importance of TDM",
    y = "TSS loss after permutation",
    x = "Variables"
  ) +
  theme(
    axis.text = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_blank(),
    plot.title = element_text(size = 14, hjust = 0.5)
  ) +
  ylim(c(0, 0.6))

ggsave("figures/vi_tdm.png", p_importance, width = 6, height = 4)

sasa_pol_21 <- sasa21_ras %>%
  filter(sasa == 1) %>%
  stars::st_as_stars() %>%
  sf::st_as_sf(merge = T) %>%
  mutate(area = sf::st_area(.)) %>%
  filter(area > units::set_units(5, m^2)) %>%
  select(sasa) %>%
  vect()

sasa_pol_12 <- sasa12_ras %>%
  filter(sasa == 1) %>%
  stars::st_as_stars() %>%
  sf::st_as_sf(merge = T) %>%
  mutate(area = sf::st_area(.)) %>%
  filter(area > units::set_units(5, m^2)) %>%
  select(sasa) %>%
  vect()

sasa_dist_12 <- terra::rasterize(sasa_pol_12, sasa_dist) %>%
  filter(layer == 1) %>%
  distance() %>%
  rename(dist = layer)

sasa_dist_21 <- terra::rasterize(sasa_pol_21, sasa_dist) %>%
  filter(layer == 1) %>%
  distance() %>%
  rename(dist = layer)

env_data_21 <- c(terrain, snow_21, sasa_dist_12) %>%
  filter(elevation < 2560) %>%
  select(colnames(df_21 %>% as_tibble() %>% select(-c(sasa, geometry))))

pred_rast_21 <- predict_raster(model_stack, env_data_21, type = "prob") %>%
  mutate(pred_sasa_21 = .pred_presence) %>%
  select(pred_sasa_21)

snow_30 <- rast("data/snow/fitted_2030.tiff") %>%
  resample(vege12) %>%
  rename(snow = snowmelt_fitted)

env_data_30 <- c(terrain, snow_30, sasa_dist_21) %>%
  select(colnames(df_21 %>% as_tibble() %>% select(-c(sasa, geometry)))) %>%
  filter(elevation < 2560)

pred_rast_30 <- predict_raster(model_stack, env_data_30, type = "prob") %>%
  mutate(pred_sasa_30 = .pred_presence) %>%
  select(pred_sasa_30)

# 12年にそのピクセルが「その他植生か」を考慮
is_others <- vege12 %>%
  mutate(is_others = ifelse(layer == 6, 1, 0)) %>%
  select(is_others)

p_pred_21 <- ggplot() +
  geom_spatraster(data = pred_rast_21 %>% drop_na()) +
  scale_fill_gradient2(
    low = "grey", high = "red", mid = "lightyellow", midpoint = 0.5, na.value = "transparent"
  ) +
  theme(
    plot.title = element_text(size = 18, hjust = 0.5),
    axis.text = element_text(angle = 30, size = 14),
    axis.title = element_text(size = 16)
  ) +
  geom_spatvector(
    data = sasa_pol_21, 
    color = "black",
    fill = "transparent"
  ) +
  labs(
    fill = "Habitat Suitability",
    x = "Longitude",
    y = "Latitude",
    title = "Habitat Suitability map of Sasa (TDM, 2021)"
  )

p_pred_21

ggsave("figures/hsmap_tdm_2021.png", p_pred_21,
       width = 10, height = 8)

p_pred_30 <- ggplot() +
  geom_spatraster(data = pred_rast_30 %>% drop_na()) +
  scale_fill_gradient2(
    low = "grey", high = "red", mid = "lightyellow", midpoint = 0.5, na.value = "transparent"
  ) +
  theme(
    plot.title = element_text(size = 18, hjust = 0.5),
    axis.text = element_text(angle = 30, size = 14),
    axis.title = element_text(size = 16)
  ) +
  geom_spatvector(
    data = sasa_pol_21, 
    color = "black",
    fill = "transparent"
  ) +
  labs(
    fill = "Habitat Suitability",
    x = "Longitude",
    y = "Latitude",
    title = "Habitat Suitability map of Sasa (TDM, 2030)"
  ) 

p_pred_30

ggsave("figures/hsmap_tdm_2030.png", p_pred_30,
       width = 10, height = 8)

diff <- (pred_rast_30 - pred_rast_21) %>%
  rename(HS_diff = pred_sasa_30)

p_diff <- ggplot() +
  geom_spatraster(data = diff %>% drop_na()) +
  scale_fill_gradient2(
    low = "royalblue", high = "red", mid = "lightyellow", 
    midpoint = 0, na.value = "transparent"
  ) +
  theme(
    plot.title = element_text(size = 18, hjust = 0.5),
    axis.text = element_text(angle = 30, size = 14),
    axis.title = element_text(size = 16)
  ) +
  geom_spatvector(
    data = sasa_pol_21, 
    color = "black",
    fill = "transparent"
  ) +
  labs(
    fill = "HS difference",
    x = "Longitude",
    y = "Latitude",
    title = "Habitat Suitability difference (TDM, 2030 - 2021)"
  ) 

p_diff
ggsave("figures/hsdiff_tdm.png", p_diff,
       width = 10, height = 8)

risky_area <- 
  c(pred_rast_30, pred_rast_21, sasa21_ras) %>%
  drop_na() %>%
  filter(sasa == 0) %>%
  filter(pred_sasa_30 > 0.5) %>%
  c(terra::resample(vege21, .)) %>%
  rename(vege21 = layer) %>%
  filter(vege21 == 2) %>%
  select(pred_sasa_30)


ext_risky <-  risky_area %>%
  ext()

dem <- "data/dem_small.tiff" %>%
  rast() %>%
  terra::crop(ext_risky) %>%
  stars::st_as_stars()

p_risky <- ggplot() +
  geom_sf(data = stars::st_contour(dem)) +
  geom_spatraster(data = risky_area) +
  scale_fill_gradient2(
    low = "grey", high = "red", mid = "lightyellow", midpoint = 0.5, na.value = "transparent"
  ) +
  theme(
    plot.title = element_text(size = 18, hjust = 0.5),
    axis.text = element_text(angle = 30, size = 14),
    axis.title = element_text(size = 16)
  ) +
  geom_spatvector(
    data = sasa_pol_21, 
    color = "black",
    fill = "transparent"
  ) +
  labs(
    fill = "Habitat Suitability \n in 2030",
    x = "Longitude",
    y = "Latitude",
    title = "Risky area (TBM)"
  ) 

p_risky
ggsave("figures/risky_tdm.png", p_risky,
       width = 10, height = 8)

terra::writeRaster(pred_rast_21, "data/sasa_pred_tdm_21.tiff", overwrite=T)
terra::writeRaster(pred_rast_30, "data/sasa_pred_tdm_30.tiff", overwrite=T)
terra::writeRaster(risky_area, "risky_area_tdm.tiff", overwrite = T)


pred_rast_21 %>%
  mutate(pred_sasa_21 = ifelse(pred_sasa_21 > 0.5, 1, 0)) %>%
  filter(pred_sasa_21 == 1) %>%
  expanse()

# 2021ではササに適していなかったが、30年では適すると判断された場所
c(pred_rast_30, pred_rast_21) %>%
  filter(pred_sasa_21 < 0.5) %>%
  filter(pred_sasa_30 > 0.5) %>%
  select(pred_sasa_30) %>%
  expanse()

sasa_pol_21 %>%
  terra::rasterize(pred_rast_30) %>%
  rename(sasa_21 = layer) %>%
  c(pred_rast_30) %>%
  filter(sasa_21 == 1) %>%
  filter(pred_sasa_30 < 0.5) %>%
  select(pred_sasa_30) %>%
  expanse()

pred_rast_21 <- rast("data/sasa_pred_tdm_21.tiff")
pred_rast_30 <- rast("data/sasa_pred_tdm_30.tiff")

pred_rast_30 %>%
  mutate(pred_sasa_30 = ifelse(pred_sasa_30 > 0.5, 1, 0)) %>%
  writeRaster("data/sasa_pred_tdm_30_bin.tiff")

