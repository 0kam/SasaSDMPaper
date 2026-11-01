# Fig. S8: centred Model B smooths with pointwise 95% confidence bands.
# Run from the repository root: Rscript analysis/figures/figS8_modelB_smooths.R
# CSV x is in original predictor units, including metres for s(log1p_dist12).

source(file.path("analysis", "00_config.R"))
source(file.path("analysis", "figures", "fig_common.R"))
suppressPackageStartupMessages(library(mgcv))
theme_set(theme_paper(base_size = 12, style = "classic"))

PANEL_VARS <- c("log1p_dist12", "snow_mean", "elevation", "slope",
                "TPI", "twi", "northness", "eastness")
TERM_LABELS <- paste0("s(", PANEL_VARS, ")")
PANEL_LABELS <- c(log1p_dist12 = "Distance from 2012 front (m)",
                  LAB[PANEL_VARS[-1]])
GRID_N <- 1001L
CURVE_COLOUR <- unname(okabeito_colours["bluishgreen"])

model_path <- model_b_inference_path()
model <- readRDS(model_path)
stopifnot(identical(model_path, PATH_MODEL_B_LOGDIST),
          inherits(model, "gam"), model$family$link == "logit",
          length(model$smooth) == length(PANEL_VARS),
          setequal(vapply(model$smooth, function(s) s$label, character(1)),
                   TERM_LABELS))
message("Inference model: ", model_path)

# Match the hindcast/model_data_all eligibility mask in 05_ca_projection.R.
model_data_all <- local({
  predictors <- terra::rast(PATH_PREDICTORS)
  dist12 <- terra::rast(PATH_DIST12)
  vege12 <- terra::rast(PATH_VEGE_2012)
  vege21 <- terra::rast(PATH_VEGE_2021)
  folds <- terra::rast(PATH_FOLDS_RASTER)
  for (r in list(predictors, dist12, vege21, folds)) {
    assert_on_ref_grid(r, "Model B figure inputs", template = vege12)
  }
  stopifnot(identical(names(predictors), PREDICTOR_NAMES))
  pred_values <- terra::values(predictors)
  dist_values <- terra::values(dist12, mat = FALSE)
  v12 <- terra::values(vege12, mat = FALSE)
  v21 <- terra::values(vege21, mat = FALSE)
  fold_values <- terra::values(folds, mat = FALSE)
  eligible <- complete.cases(pred_values) & is.finite(dist_values) &
    !is.na(v12) & !is.na(v21) & is.finite(fold_values) & v12 != CLASS_SASA
  d <- data.frame(
    colonized = as.integer(v21[eligible] == CLASS_SASA),
    dist12 = dist_values[eligible],
    pred_values[eligible, PREDICTOR_NAMES, drop = FALSE], check.names = FALSE)
  d$log1p_dist12 <- log1p(d$dist12)
  d
})
stopifnot(nrow(model_data_all) > 0L, sum(model_data_all$colonized) > 1L,
          all(is.finite(as.matrix(model_data_all))),
          all(model_data_all$dist12 > 0))
message("Eligible non-Sasa (2012) cells: ", nrow(model_data_all),
        "; establishment (2012-2021) cells: ", sum(model_data_all$colonized))

# type = "terms" retains the fitted centring and excludes the intercept.
# Other additive smooths do not contribute to the selected term or its SE.
# Each smooth is evaluated over the 0.5-99.5 percentile range of the
# establishment cells (not all eligible cells), so the curves cover only the
# range in which the establishment data support the fitted associations.
established <- model_data_all[model_data_all$colonized == 1L, ]
reference <- as.data.frame(lapply(model_data_all[PANEL_VARS], stats::median))
effects <- do.call(rbind, lapply(PANEL_VARS, function(v) {
  limits <- stats::quantile(established[[v]], c(0.005, 0.995), names = FALSE)
  stopifnot(diff(limits) > 0)
  newdata <- reference[rep(1L, GRID_N), , drop = FALSE]
  newdata[[v]] <- seq(limits[1], limits[2], length.out = GRID_N)
  term <- paste0("s(", v, ")")
  prediction <- predict(model, newdata = newdata, type = "terms",
                        terms = term, se.fit = TRUE)
  fit <- as.numeric(prediction$fit[, term])
  se <- as.numeric(prediction$se.fit[, term])
  x <- if (v == "log1p_dist12") expm1(newdata[[v]]) else newdata[[v]]
  data.frame(term = term, x = x, fit_logit = fit, se = se,
             lower = fit - 1.96 * se, upper = fit + 1.96 * se)
}))
stopifnot(setequal(unique(effects$term), TERM_LABELS),
          all(table(effects$term) == GRID_N),
          all(is.finite(as.matrix(effects[-1]))), all(effects$se >= 0))

snow <- effects[effects$term == "s(snow_mean)", ]
snow_peak <- snow$x[which.max(snow$fit_logit)]
message(sprintf("snow_mean peak: DOY %.2f (rounded: %.0f); partial effect %.6f log-odds",
                snow_peak, snow_peak, max(snow$fit_logit)))
stopifnot(snow_peak >= 150, snow_peak <= 200)
distance <- effects[effects$term == "s(log1p_dist12)" & effects$x >= 5, ]
rising <- which(diff(distance$fit_logit) > 1e-8)
if (length(rising) == 0L) {
  message("Distance check: monotonically decreasing beyond 5 m (PASS).")
} else {
  # Report the saved model faithfully; do not impose a monotonic shape.
  message(sprintf(paste0("Distance check: NOT monotonically decreasing beyond 5 m; ",
                         "increasing grid intervals span %.2f-%.2f m.\n",
                         "The figure retains the saved inference model unchanged."),
                  min(distance$x[rising]), max(distance$x[rising + 1L])))
}

# Each grey strip shows establishment-cell density, as in Fig. 3. Its height
# is normalised within the panel and lies below all confidence bands.
panels <- lapply(seq_along(PANEL_VARS), function(i) {
  v <- PANEL_VARS[i]
  d <- effects[effects$term == TERM_LABELS[i], ]
  x_limits <- range(d$x)
  y_range <- range(d$lower, d$upper, 0)
  y_span <- diff(y_range)
  strip_base <- y_range[1] - 0.17 * y_span
  strip_top <- y_range[1] - 0.05 * y_span
  observed <- if (v == "log1p_dist12") log10(established$dist12) else established[[v]]
  density_limits <- if (v == "log1p_dist12") log10(x_limits) else x_limits
  density <- stats::density(observed, n = 512, from = density_limits[1],
                             to = density_limits[2], cut = 0)
  strip <- data.frame(
    x = if (v == "log1p_dist12") 10^density$x else density$x,
    y = strip_base + (strip_top - strip_base) * density$y / max(density$y))
  y_breaks <- pretty(y_range, n = 4)
  y_breaks <- y_breaks[y_breaks >= y_range[1] & y_breaks <= y_range[2]]
  title <- paste(strwrap(PANEL_LABELS[[v]], width = 25), collapse = "\n")
  if (v == "log1p_dist12") title <- "Distance from 2012\nfront (m)"
  p <- ggplot(d, aes(x = x, y = fit_logit)) +
    geom_ribbon(data = strip, inherit.aes = FALSE,
                aes(x = x, ymin = strip_base, ymax = y),
                fill = "grey55", alpha = 0.45) +
    geom_hline(yintercept = 0, colour = "grey65", linewidth = 0.25,
               linetype = "dashed") +
    geom_ribbon(aes(ymin = lower, ymax = upper),
                fill = CURVE_COLOUR, alpha = 0.20) +
    geom_line(colour = CURVE_COLOUR, linewidth = 0.4, lineend = "round") +
    scale_y_continuous(breaks = y_breaks,
                        limits = c(strip_base - 0.01 * y_span,
                                   y_range[2] + 0.06 * y_span),
                        expand = c(0, 0)) +
    labs(title = paste0("(", letters[i], ") ", title), x = NULL,
         y = "Partial effect (log-odds)") +
    theme(plot.title = element_text(size = 12, face = "plain", hjust = 0,
                                     lineheight = 1.0,
                                     margin = margin(b = 1.2, unit = "mm")),
          axis.title.y = element_text(margin = margin(r = 1.5, unit = "mm")),
          plot.margin = margin(2, 3, 2, 2, "mm"))
  if (v == "log1p_dist12") {
    # Include the requested 1 m tick without extrapolating below the 0.5% limit.
    p + scale_x_log10(breaks = c(1, 5, 10, 50, 100, 500),
                       labels = c("1", "5", "10", "50", "100", "500"),
                       limits = c(min(1, x_limits[1]), x_limits[2]),
                       expand = expansion(mult = 0.02))
  } else {
    p + scale_x_continuous(limits = x_limits, expand = expansion(mult = 0.02),
                            guide = guide_axis(check.overlap = TRUE))
  }
})
stopifnot(length(panels) == 8L)
figure <- patchwork::wrap_plots(panels, ncol = 2, axis_titles = "collect") +
  patchwork::plot_annotation(
    caption = paste("Green: fitted smooth and 95% confidence band.",
                    "Grey: establishment-cell density.", sep = "\n"),
    theme = theme(plot.caption = element_text(size = 10, hjust = 0,
                                               margin = margin(t = 2, unit = "mm"))))

# Match Fig. 3's 170 mm width and retain 10.8-12 pt text across four rows.
output_stem <- file.path(DIR_OUT_FIG, "si", "figS8_modelB_smooths")
save_figure(figure, paste0(output_stem, ".png"),
            width_mm = 170, height_mm = 220, dpi = 300)
save_figure(figure, paste0(output_stem, ".pdf"),
            width_mm = 170, height_mm = 220)
csv_path <- file.path(DIR_OUT, "model_B_partial_effects.csv")
utils::write.csv(effects, csv_path, row.names = FALSE)
message("Wrote ", output_stem, ".{png,pdf} and ", csv_path,
        " (", nrow(effects), " grid points across 8 terms).")
