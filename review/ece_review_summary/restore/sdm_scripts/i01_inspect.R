.libPaths(c("/private/tmp/claude-501/-Users-okamoto-NIES-SasaSDMPaper--claude-worktrees-review-summary-ece-ddc69c/98ac7ecf-eafb-49c6-aa2d-9aab2998c13e/scratchpad/Rlib", .libPaths()))
suppressPackageStartupMessages({
  library(xgboost); library(tidyverse); library(tidysdm); library(tidymodels)
  library(stacks); library(sf); library(spatialsample)
})
cat("xgboost:", as.character(packageVersion("xgboost")), "\n")
O <- "/Users/okamoto/NIES/SasaSDMPaper/ortho"

wsfiles <- c("models.rds", "models_wo_dist.rds", "models_all_5m.rds")
for (f in wsfiles) {
  cat("\n\n############ ", f, " ############\n")
  ws <- readRDS(file.path(O, f))
  cat("class:", paste(class(ws), collapse=","), "\n")
  cat("nrow:", nrow(ws), "\n")
  print(ws$wflow_id)
  # predictors from first workflow's preprocessor
  for (i in seq_len(nrow(ws))) {
    id <- ws$wflow_id[i]
    inf <- ws$info[[i]]
    cat("\n-- ", id, " | model:", inf$model, " | preproc:", inf$preproc, "\n")
    wf <- inf$workflow[[1]]
    rc <- tryCatch(workflows::extract_preprocessor(wf), error=function(e) NULL)
    if (!is.null(rc) && inherits(rc, "recipe")) {
      vi <- rc$var_info
      cat("   recipe roles: ", paste(paste0(vi$variable,"[",vi$role,"]"), collapse=", "), "\n")
    }
    spec <- tryCatch(workflows::extract_spec_parsnip(wf), error=function(e) NULL)
    if (!is.null(spec)) {
      cat("   engine:", spec$engine, " mode:", spec$mode, "\n")
      dep <- function(a) paste(deparse(if (rlang::is_quosure(a)) rlang::quo_get_expr(a) else a), collapse="")
      if (length(spec$args)) cat("   args:", paste(names(spec$args), sapply(spec$args, dep), sep="=", collapse=" | "), "\n")
      if (length(spec$eng_args)) cat("   eng_args:", paste(names(spec$eng_args), sapply(spec$eng_args, dep), sep="=", collapse=" | "), "\n")
    }
    res <- ws$result[[i]]
    if (inherits(res, "tune_results")) {
      cat("   tune rows(resamples):", nrow(res), " ids:", paste(res$id, collapse=","), "\n")
      m <- tryCatch(collect_metrics(res), error=function(e) NULL)
      if (!is.null(m)) {
        cat("   n candidate configs:", length(unique(m$.config)), " metric:", paste(unique(m$.metric), collapse=","), "\n")
        cat("   best mean:", max(m$mean, na.rm=TRUE), " worst:", min(m$mean, na.rm=TRUE), "\n")
      }
      # resample split sizes
      sp <- res$splits[[1]]
      cat("   split1 analysis/assessment:", length(sp$in_id), "/", length(sp$out_id), " total data rows:", nrow(sp$data), "\n")
      cat("   split class:", paste(class(sp), collapse=","), "\n")
      cat("   data colnames:", paste(colnames(sp$data), collapse=", "), "\n")
    } else {
      cat("   result class:", paste(class(res), collapse=","), "\n")
      if (inherits(res,"try-error")) cat("   ERR:", as.character(res), "\n")
    }
  }
}
