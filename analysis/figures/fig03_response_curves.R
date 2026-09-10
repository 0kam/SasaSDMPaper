# fig03_response_curves.R -- Fig. 3: Model A partial-dependence response curves.
#
# Message: habitat suitability peaks at an intermediate snowmelt date; Sasa has
# a snowmelt window. 3.
#
#   Rscript analysis/figures/fig03_response_curves.R
#
# Input : analysis/out/model_A_partial_dependence_primary.csv
#         (recomputed from analysis/out/models/model_A_primary_stack.rds when
#          the CSV is absent -- the block below reproduces analysis/03_model_A.R
#          lines 148-168 exactly, same seed, same 5000-row PDP sample)
#         analysis/out/predictors.tif, ortho/data/vege_2021_5x5.tiff (rug data)
# Output: paper/files/fig03_response_curves.pdf / .png   (170 mm wide)

source(file.path("analysis", "figures", "fig_common.R"))
theme_set(theme_paper(base_size = 12, style = "classic"))
set.seed(20260830L)

PATH_PDP <- file.path(DIR_ANALYSIS_OUT, "model_A_partial_dependence_primary.csv")
PATH_PREDICTOR_STACK <- file.path(DIR_ANALYSIS_OUT, "predictors.tif")

# Panel order: snow_mean first (top-left) -- this panel carries the claim.
PANEL_VARS <- c("snow_mean", "elevation", "slope", "TPI", "twi", "northness")

# Wrap long predictor names for three columns at the final 170 mm width.
wrap_strip <- function(x) vapply(x, function(s)
  paste(strwrap(s, width = 24), collapse = "\n"), character(1))

# The shaded band marks the snowmelt window, defined from the data rather than
# hard-coded: the DOY range where Model A partial dependence is at or above
# 85% of its peak.
WINDOW_REL_HEIGHT <- 0.85

# ---- 1. Partial-dependence profiles --------------------------------------

recompute_pdp <- function() {
  message("PDP cache missing; recomputing from the saved Model A stack ",
          "(~2 min). This reproduces analysis/03_model_A.R.")
  source(file.path("analysis", "00_config.R"))
  source(file.path(DIR_ANALYSIS, "R", "model_utils.R"))
  suppressPackageStartupMessages({
    library(tidymodels); library(stacks); library(tidysdm)
    library(DALEX); library(DALEXtra)
  })
  predictors <- terra::rast(PATH_PREDICTORS)
  folds <- read_shared_folds()
  vege21 <- terra::rast(PATH_VEGE_2021)
  pred_values <- terra::values(predictors)
  vege_values <- terra::values(vege21, mat = FALSE)
  fold_values <- terra::values(folds, mat = FALSE)
  valid <- complete.cases(pred_values) & !is.na(vege_values) & !is.na(fold_values)
  presence_cells <- which(valid & vege_values == 1)
  absence_cells <- which(valid & vege_values != 1)
  rc <- terra::rowColFromCell(vege21, absence_cells)
  n_group_cols <- ceiling(terra::ncol(vege21) / 5)
  group_id <- ((rc[, 1] - 1L) %/% 5L) * n_group_cols + ((rc[, 2] - 1L) %/% 5L)
  set.seed(SEED_MODEL_A)
  random_order <- sample(seq_along(absence_cells))
  keep_order <- random_order[!duplicated(group_id[random_order])]
  model_cells <- sort(c(presence_cells, sort(absence_cells[keep_order])))
  model_data <- data.frame(
    response = factor(ifelse(vege_values[model_cells] == 1,
                             "presence", "absence"),
                      levels = c("presence", "absence")),
    pred_values[model_cells, PREDICTOR_NAMES, drop = FALSE], check.names = FALSE)

  model_stack <- readRDS(file.path(DIR_MODELS, "model_A_primary_stack.rds"))
  pdp_n <- min(nrow(model_data), if (SASA_SMOKE) 500L else 5000L)
  set.seed(SEED_MODEL_A)
  pdp_rows <- sample(seq_len(nrow(model_data)), pdp_n)
  pdp_data <- model_data[pdp_rows, PREDICTOR_NAMES, drop = FALSE]
  pdp_y <- as.integer(model_data$response[pdp_rows] == "presence")
  predict_probability <- function(model, newdata) {
    as.numeric(predict(model, newdata, type = "prob")$.pred_presence)
  }
  explainer <- DALEXtra::explain_tidymodels(
    model_stack, data = pdp_data, y = pdp_y,
    predict_function = predict_probability, label = "Model A primary",
    type = "classification", verbose = FALSE, precalculate = FALSE)
  profiles <- DALEX::model_profile(
    explainer, variables = PREDICTOR_NAMES, N = pdp_n,
    type = "partial", center = FALSE, grid_points = 31)$agr_profiles
  out <- data.frame(variant = "primary",
                    predictor = as.character(profiles$`_vname_`),
                    x = as.numeric(profiles$`_x_`),
                    yhat = as.numeric(profiles$`_yhat_`),
                    stringsAsFactors = FALSE)
  utils::write.csv(out, PATH_PDP, row.names = FALSE)
  out
}

pdp <- if (file.exists(PATH_PDP)) {
  utils::read.csv(PATH_PDP, stringsAsFactors = FALSE)
} else {
  recompute_pdp()
}
pdp <- pdp[pdp$variant == "primary" & pdp$predictor %in% PANEL_VARS, ]
pdp$predictor <- factor(pdp$predictor, levels = PANEL_VARS)
stopifnot(nlevels(droplevels(pdp$predictor)) == length(PANEL_VARS))

# ---- 2. Observed predictor distribution (2021 Sasa cells) ----------------
# Used as a density strip under each curve: it shows where the model is
# extrapolating beyond the occupied environmental space.

occupied <- local({
  predictors <- terra::rast(PATH_PREDICTOR_STACK)
  vege21 <- terra::rast(VEGE_2021)
  pred_values <- terra::values(predictors)
  vege_values <- terra::values(vege21, mat = FALSE)
  keep <- complete.cases(pred_values) & !is.na(vege_values) &
    vege_values == CLASS_SASA
  as.data.frame(pred_values[keep, PANEL_VARS, drop = FALSE])
})
message("Occupied (2021 Sasa) cells used for the density strip: ", nrow(occupied))

# ---- 3. Shared y scale and the geometry of the density strip -------------

y_range <- range(pdp$yhat)
y_span <- diff(y_range)
STRIP_BASE <- y_range[1] - 0.17 * y_span   # floor of the density strip
STRIP_TOP  <- y_range[1] - 0.05 * y_span   # ceiling: always below every curve
# Headroom above the curves: the 10 pt two-line snowmelt-window note is parked in
# this band, so it never crosses the descending limb of the snow_mean curve.
y_limits <- c(STRIP_BASE - 0.01 * y_span, y_range[2] + 0.23 * y_span)

density_strip <- do.call(rbind, lapply(PANEL_VARS, function(v) {
  x_obs <- occupied[[v]]
  x_lim <- range(pdp$x[pdp$predictor == v])
  d <- stats::density(x_obs, n = 512,
                      from = x_lim[1], to = x_lim[2],
                      cut = 0)
  data.frame(predictor = v, x = d$x,
             y = STRIP_BASE + (STRIP_TOP - STRIP_BASE) * d$y / max(d$y))
}))
density_strip$predictor <- factor(density_strip$predictor, levels = PANEL_VARS)

# ---- 4. Snowmelt window (snow_mean panel only) ---------------------------

snow_window <- local({
  d <- pdp[pdp$predictor == "snow_mean", ]
  d <- d[order(d$x), ]
  hi <- which(d$yhat >= WINDOW_REL_HEIGHT * max(d$yhat))
  c(min(d$x[hi]), max(d$x[hi]))
})
message(sprintf("Snowmelt window (>= %.0f%% of peak suitability): DOY %.0f-%.0f",
                100 * WINDOW_REL_HEIGHT, snow_window[1], snow_window[2]))

snow_pdp <- pdp[pdp$predictor == "snow_mean", ]
message(sprintf("Snowmelt peak: DOY %.0f (partial-dependence suitability %.8f)",
                snow_pdp$x[which.max(snow_pdp$yhat)], max(snow_pdp$yhat)))

window_rect <- data.frame(
  predictor = factor("snow_mean", levels = PANEL_VARS),
  xmin = snow_window[1], xmax = snow_window[2],
  ymin = -Inf, ymax = Inf)

window_label <- data.frame(
  predictor = factor("snow_mean", levels = PANEL_VARS),
  # Anchored top-RIGHT: at base_size 10 the two-line note no longer clears the
  # rising limb of the curve on the left, but the panel's right half (DOY > 200,
  # suitability ~0.09) is empty.
  x = max(pdp$x[pdp$predictor == "snow_mean"]),
  y = y_range[2] + 0.21 * y_span,
  label = sprintf("Snowmelt window\nDOY %.0f–%.0f", snow_window[1],
                  snow_window[2]))

# ---- 5. Plot -------------------------------------------------------------

CURVE_COLOUR <- unname(okabeito_colours["bluishgreen"])

p <- ggplot(pdp, aes(x = x, y = yhat)) +
  geom_rect(data = window_rect, inherit.aes = FALSE,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "grey40", alpha = 0.15) +
  geom_ribbon(data = density_strip, inherit.aes = FALSE,
              aes(x = x, ymin = STRIP_BASE, ymax = y),
              fill = "grey55", alpha = 0.45) +
  geom_line(linewidth = 0.4, colour = CURVE_COLOUR, lineend = "round") +
  geom_text_pt(data = window_label, inherit.aes = FALSE,
               aes(x = x, y = y, label = label),
               hjust = 1, vjust = 1, lineheight = 1.05, colour = "grey25",
               size_pt = 10) +
  facet_wrap(~ predictor, scales = "free_x", nrow = 2,
             labeller = labeller(predictor = wrap_strip(LAB[PANEL_VARS]))) +
  scale_x_continuous(expand = expansion(mult = 0.02),
                     guide = guide_axis(check.overlap = TRUE)) +
  # Breaks are restricted to the suitability range: no tick may fall inside the
  # density strip, which is a distribution and not a suitability value.
  scale_y_continuous(breaks = function(...) {
                       b <- scales::fullseq(y_range, 0.05)
                       b[b >= y_range[1] & b <= y_range[2]]
                     },
                     limits = y_limits, expand = c(0, 0)) +
  labs(x = NULL, y = "Partial-dependence suitability") +
  # Column gap widened from 3.5 mm: at 10.8 pt tick labels the "3000" ending the
  # elevation axis and the "0" starting the slope axis otherwise read as "30000".
  theme(panel.spacing.x = unit(7, "mm"),
        panel.spacing.y = unit(3.5, "mm"),
        strip.text = element_text(size = rel(1.0), face = "plain", hjust = 0,
                                  lineheight = 1.0,
                                  margin = margin(b = 1.2, unit = "mm")),
        axis.title.y = element_text(margin = margin(r = 1.5, unit = "mm")),
        plot.margin = margin(2, 3, 2, 2, "mm"))

# Render directly at the manuscript width so the 10–12 pt text is not reduced.
save_figure(p, file.path(DIR_OUT_FIG, "fig03_response_curves.pdf"),
            width_mm = 170, height_mm = 118)
save_figure(p, file.path(DIR_OUT_FIG, "fig03_response_curves.png"),
            width_mm = 170, height_mm = 118, dpi = 300)

message("Wrote paper/files/fig03_response_curves.{pdf,png}")
