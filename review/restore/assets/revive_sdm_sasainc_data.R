# Faithful revival of data_from_server/ortho/sdm_sasainc.R -- DATA STEP.
# Paths repointed to the read-only archive; nothing else changed except an added set.seed
# (the original script has NO seed anywhere -> it was never reproducible as written).
suppressPackageStartupMessages({library(tidyverse);library(tidysdm);library(tidymodels);library(terra);library(tidyterra)})
D <- "/Users/okamoto/NIES/SasaSDMPaper/data_from_server/ortho/"
O <- "/Users/okamoto/NIES/SasaSDMPaper/.claude/worktrees/review-summary-ece-ddc69c/review/restore/assets/"
set.seed(1)

vege12 <- rast(paste0(D,"data/vege_2012_5x5.tiff"))
vege21 <- rast(paste0(D,"data/vege_2021_5x5.tiff"))
terrain <- list.files(paste0(D,"data/terrain_features/"), full.names=TRUE) |> str_subset(".tif$") |>
  rast() |> rename(elevation = tateyamadem_small) |> resample(vege12)
snow_21 <- rast(paste0(D,"data/snow/fitted_2021.tiff")) |> resample(vege12) |> rename(snow = snowmelt_fitted)

sasa12_ras <- vege12 |> mutate(sasa = ifelse(layer == 1, 1, 0)) |> select(sasa)
sasa21_ras <- vege21 |> mutate(sasa = ifelse(layer == 1, 1, 0)) |> select(sasa)

sasa_dist <- rast(paste0(D,"data/selected_comms.tiff")) |> resample(sasa12_ras) |> distance() |> rename(dist = selected_comms)

inc_bin <- (sasa21_ras - sasa12_ras) |> mutate(sasa_inc = ifelse(sasa == 1, 1, 0)) |> select(sasa_inc)
cat("binary colonisation cells (1 m):", global(inc_bin, "sum", na.rm=TRUE)[[1]], "\n")

sasa_inc <- inc_bin |>
  terra::aggregate(fact = 10, fun = "mean") |>
  terra::resample(sasa21_ras, method = "near") |>
  c(sasa_dist) |>
  filter((dist != 0) & (dist <= 10)) |>
  select(sasa_inc)

cat("DOMAIN: cells with 0 < dist(selected_comms) <= 10 m and defined response:",
    sum(!is.na(values(sasa_inc))), "of", ncell(sasa_inc), "\n")
cat("response (10 m mean colonisation proportion, replicated to 1 m) summary:\n")
print(summary(values(sasa_inc)[!is.na(values(sasa_inc))]))
writeRaster(sasa_inc, paste0(O,"out/sasa_inc_response_domain.tiff"), overwrite=TRUE)

sampling_mask <- terrain["elevation"] |> terra::aggregate(fact = 5)

df_inc <- sasa_inc %>% select(sasa_inc) %>% as.points() %>% as_sf() %>%
  thin_by_cell(sampling_mask) %>%
  bind_cols(terra::extract(c(terrain, snow_21), ., ID = FALSE)) %>%
  drop_na()
cat("\nafter thin_by_cell(5 m) + extract + drop_na: n =", nrow(df_inc), "\n")
cat("response after thinning: mean =", mean(df_inc$sasa_inc), " sd =", sd(df_inc$sasa_inc),
    " zeros =", sum(df_inc$sasa_inc == 0), "(", sprintf("%.1f%%", 100*mean(df_inc$sasa_inc==0)), ")\n")
print(summary(df_inc$sasa_inc))

keep <- filter_collinear(df_inc)
cat("\nfilter_collinear kept:", paste(keep, collapse=", "), "\n")
df_inc2 <- df_inc |> select(sasa_inc, all_of(keep))
saveRDS(df_inc2, paste0(O,"out/df_inc.rds"))
cat("saved df_inc.rds with columns:", paste(setdiff(names(df_inc2),"geometry"), collapse=", "), "\n")
