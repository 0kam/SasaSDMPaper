.libPaths(c("/private/tmp/claude-501/-Users-okamoto-NIES-SasaSDMPaper--claude-worktrees-review-summary-ece-ddc69c/98ac7ecf-eafb-49c6-aa2d-9aab2998c13e/scratchpad/Rlib", .libPaths()))
suppressPackageStartupMessages({library(xgboost);library(stacks);library(tidymodels);library(sf)})
cat("xgboost", as.character(packageVersion("xgboost")), "\n")
d <- "/private/tmp/claude-501/-Users-okamoto-NIES-SasaSDMPaper--claude-worktrees-review-summary-ece-ddc69c/98ac7ecf-eafb-49c6-aa2d-9aab2998c13e/scratchpad/sdm2/xgbjson"
dir.create(d, showWarnings=FALSE)
for (f in c("model_stack.rds","model_stack_wo_dist.rds")) {
  s <- readRDS(file.path("/Users/okamoto/NIES/SasaSDMPaper/ortho", f))
  nd <- s$train |> sf::st_drop_geometry() |> head(200)
  for (m in names(s$member_fits)) {
    ef <- try(workflows::extract_fit_engine(s$member_fits[[m]]), silent=TRUE)
    if (!inherits(ef, "xgb.Booster")) next
    p1 <- predict(ef, as.matrix(nd[, ef$feature_names]))
    out <- file.path(d, paste0(sub("\\.rds$","",f), "__", m, ".json"))
    xgb.save(ef, out)
    b2 <- xgb.load(out)
    p2 <- predict(b2, as.matrix(nd[, ef$feature_names]))
    cat(sprintf("  %-24s %-20s -> %s  maxabs(reload-orig)=%.3e  (%d bytes)\n",
        f, m, basename(out), max(abs(p1-p2)), file.size(out)))
  }
}
