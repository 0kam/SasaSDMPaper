lib <- commandArgs(TRUE)[1]
if (nzchar(lib)) .libPaths(c(lib, .libPaths()))
suppressPackageStartupMessages({library(xgboost); library(stacks); library(tidymodels); library(sf)})
cat("xgboost:", as.character(packageVersion("xgboost")), "\n")
for (f in c("model_stack.rds","model_stack_wo_dist.rds")) {
  s <- readRDS(file.path("/Users/okamoto/NIES/SasaSDMPaper/ortho", f))
  nd <- s$train |> sf::st_drop_geometry() |> head(5)
  r <- tryCatch({p <- predict(s, nd, type="prob"); paste("OK  first .pred_presence =", paste(round(p$.pred_presence,6), collapse=", "))},
                error=function(e) paste("FAIL:", conditionMessage(e)))
  cat(" ", f, "->", r, "\n")
}
