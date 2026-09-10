suppressPackageStartupMessages({library(tidyverse);library(tidysdm);library(tidymodels);library(spatialsample);library(ranger);library(sf)})
O <- "/Users/okamoto/NIES/SasaSDMPaper/.claude/worktrees/review-summary-ece-ddc69c/review/restore/assets/"
set.seed(1)
df <- readRDS(paste0(O,"out/df_inc.rds"))
d  <- df %>% st_drop_geometry()
cv <- df %>% spatial_block_cv(v = 4)
preds <- setdiff(names(d), "sasa_inc")

oof <- function(vars) {
  p <- rep(NA_real_, nrow(d))
  for (s in cv$splits) {
    tri <- s$in_id; tei <- setdiff(seq_len(nrow(d)), tri)
    m <- ranger(x = d[tri, vars, drop=FALSE], y = d$sasa_inc[tri], num.threads = 12, seed = 1)
    p[tei] <- predict(m, d[tei, vars, drop=FALSE])$predictions
  }
  y <- d$sasa_inc
  list(rmse = sqrt(mean((y-p)^2)),
       r2   = 1 - sum((y-p)^2)/sum((y-mean(y))^2),
       cor  = cor(y, p))
}
nullp <- rep(NA_real_, nrow(d))
for (s in cv$splits) { tri <- s$in_id; tei <- setdiff(seq_len(nrow(d)), tri); nullp[tei] <- mean(d$sasa_inc[tri]) }
y <- d$sasa_inc
cat("### Spatially-blocked out-of-fold performance, n =", nrow(d), "\n")
cat(sprintf("NULL (fold-train mean) : RMSE=%.5f  R2=%.4f\n", sqrt(mean((y-nullp)^2)), 1-sum((y-nullp)^2)/sum((y-mean(y))^2)))
f <- oof(preds)
cat(sprintf("RF, all %d predictors  : RMSE=%.5f  R2=%.4f  cor=%.4f  (cor^2=%.4f)\n", length(preds), f$rmse, f$r2, f$cor, f$cor^2))
for (v in preds) { g <- oof(setdiff(preds, v)); cat(sprintf("   drop %-10s        : RMSE=%.5f (delta %+0.5f)  R2=%.4f\n", v, g$rmse, g$rmse-f$rmse, g$r2)) }
for (v in preds) { g <- oof(v); cat(sprintf("   only %-10s        : RMSE=%.5f  R2=%.4f\n", v, g$rmse, g$r2)) }
cat("\n### Non-spatial (random 4-fold) for contrast -- shows how much spatial blocking matters\n")
set.seed(1); rcv <- vfold_cv(d, v = 4)
p <- rep(NA_real_, nrow(d))
for (s in rcv$splits) { tri <- s$in_id; tei <- setdiff(seq_len(nrow(d)), tri)
  m <- ranger(x = d[tri, preds, drop=FALSE], y = d$sasa_inc[tri], num.threads = 12, seed = 1)
  p[tei] <- predict(m, d[tei, preds, drop=FALSE])$predictions }
cat(sprintf("RF random CV           : RMSE=%.5f  R2=%.4f\n", sqrt(mean((y-p)^2)), 1-sum((y-p)^2)/sum((y-mean(y))^2)))
