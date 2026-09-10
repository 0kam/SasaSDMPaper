# Faithful revival of sdm_sasainc.R -- MODEL STEP (RF regression, spatial block CV, grid=20, rmse)
suppressPackageStartupMessages({library(tidyverse);library(tidysdm);library(tidymodels);library(terra);library(DALEX);library(spatialsample)})
O <- "/Users/okamoto/NIES/SasaSDMPaper/.claude/worktrees/review-summary-ece-ddc69c/review/restore/assets/"
set.seed(1)
df_inc <- readRDS(paste0(O,"out/df_inc.rds"))
cat("n =", nrow(df_inc), " predictors:", paste(setdiff(names(df_inc), c("sasa_inc","geometry")), collapse=", "), "\n")

rec <- recipe(df_inc, formula = sasa_inc ~ .)
cv  <- df_inc %>% spatial_block_cv(v = 4)
cat("spatial_block_cv folds:", nrow(cv), " fold sizes:", paste(sapply(cv$splits, function(s) nrow(assessment(s))), collapse=","), "\n")

models <- workflow_set(
  preproc = list(default = rec),
  models  = list(rf = rand_forest() %>% set_args(num.threads = 12) %>% set_mode("regression")),
  cross = TRUE) %>% option_add(control = control_ensemble_grid())

models <- models %>% workflow_map("tune_grid", resamples = cv,
  metrics = yardstick::metric_set(rmse, rsq), grid = 20, seed = 1, verbose = FALSE)

res <- rank_results(models, rank_metric = "rmse", select_best = TRUE)
print(as.data.frame(res))
cat("\n--- ALL tuning results (best 5 by rmse) ---\n")
print(models %>% collect_metrics() %>% filter(.metric=="rmse") %>% arrange(mean) %>% head(5) %>% as.data.frame())
cat("--- rsq for the same configs ---\n")
print(models %>% collect_metrics() %>% filter(.metric=="rsq") %>% arrange(desc(mean)) %>% head(5) %>% as.data.frame())

cat("\n### BASELINE: intercept-only (predict fold-training mean) under the SAME spatial folds\n")
null_rmse <- sapply(cv$splits, function(s){
  tr <- analysis(s); te <- assessment(s)
  sqrt(mean((te$sasa_inc - mean(tr$sasa_inc))^2))
})
cat("null RMSE per fold:", paste(round(null_rmse,5), collapse=", "), " mean =", round(mean(null_rmse),5), "\n")
cat("overall SD of response:", round(sd(df_inc$sasa_inc),5), "\n")

model_ensemble <- simple_ensemble() %>% add_member(models, metric = "rmse")
print(model_ensemble %>% collect_metrics() %>% as.data.frame())
saveRDS(models, paste0(O,"out/sasainc_models.rds"))
saveRDS(model_ensemble, paste0(O,"out/sasainc_ensemble.rds"))

cat("\n### DALEX permutation variable importance (RMSE loss)\n")
expl <- model_ensemble %>% DALEX::explain(data = df_inc, y = df_inc$sasa_inc, verbose = FALSE)
set.seed(1)
vi <- model_parts(explainer = expl, B = 20)
print(vi %>% as_tibble() %>% group_by(variable) %>% summarise(mean_loss = mean(dropout_loss)) %>% arrange(desc(mean_loss)) %>% as.data.frame())
saveRDS(vi, paste0(O,"out/sasainc_varimp.rds"))
