# =============================================================================
# review/trend_sensitivity/02_model_B_trend.R
#
# SI sensitivity analysis 2: does adding a smooth of the per-pixel snowmelt
# trend improve Model B's blocked predictive performance?
#
# Baseline    = the adopted specification (analysis/04_model_B.R, "logdist"):
#               s(log1p_dist12, k = 5) + s(snow_mean) + s(elevation) +
#               s(slope) + s(TPI) + s(twi) + s(northness) + s(eastness)
# Trend model = baseline + s(trend, k = 10)
#
# Both are refitted here on the identical row set (cells with a fitted
# per-pixel trend, 20 of 1,197,536 rows dropped, none of them positives) and
# evaluated with the identical leave-one-shared-block-out protocol, the same
# mgcv::bam settings (discrete = TRUE, fREML, MODEL_THREADS) and the same
# seeds as analysis/04_model_B.R. Refitting the baseline here rather than
# quoting the published numbers keeps the comparison exactly like-for-like.
#
# Outputs (review/trend_sensitivity/):
#   model_B_trend_comparison.csv   blocked AUC / TSS, baseline vs trend
#   model_B_trend_smooth.csv       edf and partial effect of s(trend)
#   fig_smooth_trend.png           partial effect of s(trend)
# =============================================================================

t0 <- Sys.time()
REPO <- Sys.getenv("SASA_REPO_ROOT", unset = "/Users/okamoto/NIES/SasaSDMPaper")
source(file.path(REPO, "analysis", "00_config.R"))
source(file.path(DIR_ANALYSIS, "R", "model_utils.R"))
suppressPackageStartupMessages({
  library(mgcv)
  library(yardstick)
})
DIR_REVIEW <- file.path(REPO, "review", "trend_sensitivity")
set.seed(SEED_MODEL_B)

dat <- readRDS(file.path(DIR_REVIEW, "model_B_frame.rds"))
n_all <- nrow(dat)
dat <- dat[is.finite(dat$trend), , drop = FALSE]
msg("rows used: ", nrow(dat), " of ", n_all,
    "  positives: ", sum(dat$colonized))

model_data <- dat[, c("colonized", "dist12", "log1p_dist12", "trend",
                      PREDICTOR_NAMES), drop = FALSE]

formula_base <- colonized ~
  s(log1p_dist12, k = 5) + s(snow_mean, k = 10) +
  s(elevation, k = 10) + s(slope, k = 10) +
  s(TPI, k = 10) + s(twi, k = 10) +
  s(northness, k = 10) + s(eastness, k = 10)
formula_trend <- stats::update.formula(formula_base, . ~ . + s(trend, k = 10))

eval_folds <- evaluation_fold_ids(dat$fold_id)

run_lobo <- function(form, label) {
  oof <- rep(NA_real_, nrow(model_data))
  for (k in sort(unique(eval_folds))) {
    train <- eval_folds != k
    heldout <- eval_folds == k
    set.seed(SEED_MODEL_B + k)
    tk <- Sys.time()
    fold_model <- mgcv::bam(
      form, data = model_data[train, , drop = FALSE],
      family = stats::binomial(), discrete = TRUE,
      nthreads = MODEL_THREADS, method = "fREML"
    )
    oof[heldout] <- as.numeric(stats::predict(
      fold_model, newdata = model_data[heldout, , drop = FALSE],
      type = "response"
    ))
    msg(label, " fold ", k, " done in ",
        sprintf("%.1f", as.numeric(difftime(Sys.time(), tk, units = "secs"))),
        " s")
    rm(fold_model); gc(verbose = FALSE)
  }
  if (any(!is.finite(oof))) stop(label, ": non-finite OOF probabilities")
  oof
}

msg("Leave-one-shared-block-out: baseline (adopted logdist specification)")
oof_base <- run_lobo(formula_base, "baseline")
metrics_base <- blocked_metrics(model_data$colonized, oof_base, dat$fold_id)

msg("Leave-one-shared-block-out: baseline + s(trend)")
oof_trend <- run_lobo(formula_trend, "trend")
metrics_trend <- blocked_metrics(model_data$colonized, oof_trend, dat$fold_id)

# ---- comparison table, laid out like model_B_logdist_comparison.csv ---------
tag <- function(m, name) {
  m <- m[m$scope %in% c("fold", "mean", "pooled"), , drop = FALSE]
  data.frame(section = "blocked_metrics", model = name, scope = m$scope,
             fold = m$fold, n = m$n, auc = m$auc, tss = m$tss,
             threshold = m$threshold, stringsAsFactors = FALSE)
}
blocked <- rbind(tag(metrics_base, "baseline"), tag(metrics_trend, "trend"))

delta_rows <- do.call(rbind, lapply(c("fold", "mean", "pooled"), function(sc) {
  b <- metrics_base[metrics_base$scope == sc, , drop = FALSE]
  tr <- metrics_trend[metrics_trend$scope == sc, , drop = FALSE]
  data.frame(section = "delta", model = "trend_minus_baseline", scope = sc,
             fold = b$fold, n = b$n, auc = tr$auc - b$auc,
             tss = tr$tss - b$tss, threshold = NA_real_,
             stringsAsFactors = FALSE)
}))
comparison <- rbind(blocked, delta_rows)
utils::write.csv(comparison,
                 file.path(DIR_REVIEW, "model_B_trend_comparison.csv"),
                 row.names = FALSE, na = "")
print(comparison)

# ---- full-data fits: edf, significance and shape of s(trend) ----------------
msg("Fitting the full-data baseline")
set.seed(SEED_MODEL_B)
fit_base <- mgcv::bam(formula_base, data = model_data,
                      family = stats::binomial(), discrete = TRUE,
                      nthreads = MODEL_THREADS, method = "fREML")
msg("Fitting the full-data trend model")
set.seed(SEED_MODEL_B)
fit_trend <- mgcv::bam(formula_trend, data = model_data,
                       family = stats::binomial(), discrete = TRUE,
                       nthreads = MODEL_THREADS, method = "fREML")

s_base <- summary(fit_base)
s_trend <- summary(fit_trend)
print(s_trend)

smooth_tbl <- as.data.frame(s_trend$s.table)
smooth_tbl <- cbind(term = rownames(smooth_tbl), model = "trend", smooth_tbl,
                    stringsAsFactors = FALSE)
smooth_base_tbl <- as.data.frame(s_base$s.table)
smooth_base_tbl <- cbind(term = rownames(smooth_base_tbl), model = "baseline",
                         smooth_base_tbl, stringsAsFactors = FALSE)
smooth_all <- rbind(smooth_base_tbl, smooth_tbl)
rownames(smooth_all) <- NULL
utils::write.csv(smooth_all, file.path(DIR_REVIEW, "model_B_trend_smooth.csv"),
                 row.names = FALSE)

fit_tbl <- data.frame(
  quantity = c("n_rows", "n_positives",
               "baseline_dev_expl", "trend_dev_expl",
               "baseline_AIC", "trend_AIC", "AIC_trend_minus_baseline",
               "baseline_REML", "trend_REML",
               "s_trend_edf", "s_trend_ref_df", "s_trend_chisq", "s_trend_p"),
  value = c(nrow(model_data), sum(model_data$colonized),
            s_base$dev.expl, s_trend$dev.expl,
            stats::AIC(fit_base), stats::AIC(fit_trend),
            stats::AIC(fit_trend) - stats::AIC(fit_base),
            fit_base$gcv.ubre, fit_trend$gcv.ubre,
            s_trend$s.table["s(trend)", "edf"],
            s_trend$s.table["s(trend)", "Ref.df"],
            s_trend$s.table["s(trend)", "Chi.sq"],
            s_trend$s.table["s(trend)", "p-value"])
)
utils::write.csv(fit_tbl, file.path(DIR_REVIEW, "model_B_trend_fit.csv"),
                 row.names = FALSE)
print(fit_tbl)

# ---- partial effect of s(trend) --------------------------------------------
grDevices::pdf(NULL)
sm <- mgcv::plot.gam(fit_trend, pages = 0, se = TRUE)
grDevices::dev.off()
idx <- which(vapply(fit_trend$smooth, function(z) z$label, character(1)) ==
               "s(trend)")
d <- data.frame(x = as.numeric(sm[[idx]]$x), fit = as.numeric(sm[[idx]]$fit),
                se = as.numeric(sm[[idx]]$se))
utils::write.csv(d, file.path(DIR_REVIEW, "model_B_trend_partial_effect.csv"),
                 row.names = FALSE)

p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$x, y = .data$fit)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$fit - 2 * .data$se,
                                    ymax = .data$fit + 2 * .data$se),
                       fill = "#9ECAE1", alpha = 0.5) +
  ggplot2::geom_line(linewidth = 0.8, colour = "#08519C") +
  ggplot2::geom_hline(yintercept = 0, linetype = 2, colour = "grey45") +
  ggplot2::labs(
    x = expression(paste("Pixelwise snowmelt trend (days ", year^-1, ")")),
    y = "Partial effect (logit scale)") +
  ggplot2::theme_bw(base_size = 12) +
  ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
ggplot2::ggsave(file.path(DIR_REVIEW, "fig_smooth_trend.png"), p,
                width = 5.5, height = 4, dpi = 300)

# The full range is stretched by a few hundred poorly constrained snow-patch
# edge pixels (|trend| > 5 d/yr). A second panel restricted to the central 90%
# of the trend distribution shows the part of the smooth that actually acts on
# the landscape, and its magnitude is what the report quotes.
qlim <- as.numeric(stats::quantile(model_data$trend, c(0.05, 0.95)))
inner <- d[d$x >= qlim[1] & d$x <= qlim[2], , drop = FALSE]
msg(sprintf("central 90%% of trend: [%.3f, %.3f] d/yr", qlim[1], qlim[2]))
msg(sprintf("partial effect over that range: %.4f to %.4f logit (span %.4f)",
            min(inner$fit), max(inner$fit), diff(range(inner$fit))))
effect_tbl <- data.frame(
  quantity = c("trend_q05", "trend_q95",
               "partial_effect_min_inner", "partial_effect_max_inner",
               "partial_effect_span_inner", "odds_ratio_span_inner",
               "partial_effect_span_full", "s_log1p_dist12_span_full"),
  value = c(qlim[1], qlim[2], min(inner$fit), max(inner$fit),
            diff(range(inner$fit)), exp(diff(range(inner$fit))),
            diff(range(d$fit)),
            diff(range(as.numeric(sm[[which(
              vapply(fit_trend$smooth, function(z) z$label, character(1)) ==
                "s(log1p_dist12)")]]$fit))))
)
utils::write.csv(effect_tbl,
                 file.path(DIR_REVIEW, "model_B_trend_effect_size.csv"),
                 row.names = FALSE)
print(effect_tbl)

p_zoom <- p +
  ggplot2::coord_cartesian(
    xlim = qlim,
    ylim = range(c(inner$fit - 2 * inner$se, inner$fit + 2 * inner$se))
  )
ggplot2::ggsave(file.path(DIR_REVIEW, "fig_smooth_trend_zoom.png"), p_zoom,
                width = 5.5, height = 4, dpi = 300)

msg("02_model_B_trend.R finished in ",
    sprintf("%.1f", as.numeric(difftime(Sys.time(), t0, units = "secs"))), " s")
