.libPaths(c("/private/tmp/claude-501/-Users-okamoto-NIES-SasaSDMPaper--claude-worktrees-review-summary-ece-ddc69c/98ac7ecf-eafb-49c6-aa2d-9aab2998c13e/scratchpad/Rlib", .libPaths()))
suppressPackageStartupMessages({
  library(xgboost); library(tidyverse); library(tidysdm); library(tidymodels)
  library(stacks); library(sf); library(spatialsample)
})
O <- "/Users/okamoto/NIES/SasaSDMPaper/ortho"
for (f in c("model_stack.rds", "model_stack_wo_dist.rds")) {
  cat("\n\n############ ", f, " ############\n")
  s <- readRDS(file.path(O, f))
  cat("class:", paste(class(s), collapse=","), "\n")
  cat("names(s):", paste(names(s), collapse=", "), "\n")
  cat("outcome:", s$outcome, " mode:", s$mode, "\n")
  cat("train dim:", paste(dim(s$train), collapse="x"), "\n")
  cat("train cols:", paste(colnames(s$train), collapse=", "), "\n")
  print(s$train %>% sf::st_drop_geometry() %>% count(sasa))
  cat("penalty/mixture:", s$penalty$penalty, "/", s$penalty$mixture, "\n")
  cat("member_fits:", paste(names(s$member_fits), collapse=", "), "\n")
  cat("n members:", length(s$member_fits), "\n")
  cat("--- coefs ---\n")
  print(s$coefs %>% tidy() %>% filter(estimate != 0), n = 50)
  cat("--- metrics of blend ---\n")
  print(s$metrics)
  cat("--- splits used to build data_stack ---\n")
  cat("splits:", paste(class(s$splits), collapse=","), "\n")
  if (!is.null(s$splits)) { print(s$splits) }
  cat("--- member spec detail ---\n")
  for (nm in names(s$member_fits)) {
    mf <- s$member_fits[[nm]]
    sp <- workflows::extract_spec_parsnip(mf)
    fitobj <- tryCatch(workflows::extract_fit_engine(mf), error=function(e) NULL)
    cat(" *", nm, "engine:", sp$engine, "\n")
    dep <- function(a) paste(deparse(if (rlang::is_quosure(a)) rlang::quo_get_expr(a) else a), collapse="")
    if (length(sp$args)) cat("     args:", paste(names(sp$args), sapply(sp$args, dep), sep="=", collapse=" | "), "\n")
    if (!is.null(fitobj) && inherits(fitobj, "xgb.Booster")) {
      cat("     xgb niter:", fitobj$niter, " feature_names:", paste(fitobj$feature_names, collapse=","), "\n")
    }
    if (!is.null(fitobj) && inherits(fitobj, "maxnet")) {
      cat("     maxnet betas nonzero:", sum(fitobj$betas != 0), " levels:", paste(head(names(fitobj$levels),20), collapse=","), "\n")
      cat("     maxnet varnames:", paste(unique(gsub("[^A-Za-z_]", "", names(fitobj$betas))), collapse=","), "\n")
    }
  }
}
