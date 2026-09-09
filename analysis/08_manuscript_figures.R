# =============================================================================
# 08_manuscript_figures.R -- composite figures for the Results section
# =============================================================================
# Produces the manuscript figures from accepted WP outputs. Styling follows the
# reviewers' requests: no in-plot titles, shared legends, >= 11 pt text, all
# panels large. Written to paper/files/ so the Quarto sources can include them.

t0 <- Sys.time()
source(file.path("analysis", "00_config.R"))
suppressPackageStartupMessages({
  library(terra); library(ggplot2); library(tidyterra); library(patchwork); library(dplyr)
})

DIR_PAPER_FILES <- file.path(REPO_ROOT, "paper", "files")
dir.create(DIR_PAPER_FILES, showWarnings = FALSE, recursive = TRUE)

base_theme <- theme_bw(base_size = 12) +
  theme(panel.grid = element_blank(),
        axis.text  = element_text(size = 10),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 11),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, face = "bold"))

vege12 <- rast(PATH_VEGE_2012); vege21 <- rast(PATH_VEGE_2021)
sasa12 <- vege12 == 1; sasa21 <- vege21 == 1
dem <- rast(PATH_PREDICTORS)[["elevation"]]
contours <- as.contour(dem, levels = seq(2300, 3000, by = 50)) |> sf::st_as_sf()

map_axes <- function(p) {
  p + coord_sf(expand = FALSE, datum = sf::st_crs(6690)) +
    scale_x_continuous(breaks = seq(733000, 734500, by = 500)) +
    scale_y_continuous(breaks = seq(4050500, 4052000, by = 500)) +
    labs(x = "Easting (m)", y = "Northing (m)") +
    base_theme +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
}
add_contours <- function(p) p + geom_sf(data = contours, colour = "grey55", linewidth = 0.15)

# ---- Figure: Sasa change map (persist / gain / loss) -------------------------
change <- ifel(sasa12 & sasa21, 1L, ifel(!sasa12 & sasa21, 2L, ifel(sasa12 & !sasa21, 3L, NA)))
change <- as.factor(change)
levels(change) <- data.frame(id = 1:3, class = c("Persisting (2012 & 2021)", "Gained (2021 only)", "Lost (2012 only)"))
p_change <- ggplot() |> add_contours()
p_change <- p_change + geom_spatraster(data = change, maxcell = Inf) +
  scale_fill_manual(values = c("#1b7837", "#e08214", "#7b3294"), na.value = "transparent",
                    name = NULL, na.translate = FALSE)
p_change <- map_axes(p_change) + theme(legend.position = "bottom")
ggsave(file.path(DIR_PAPER_FILES, "fig_sasa_change.png"), p_change, width = 7.5, height = 8.2, dpi = 300)

# ---- Figure: Model A suitability + Model B p9 (side by side) -----------------
suitA <- rast(file.path(DIR_OUT, "suitability_A_primary.tif")); names(suitA) <- "suit"
p9 <- rast(file.path(DIR_OUT, "p9_colonization.tif")); names(p9) <- "p9"
outline21 <- as.polygons(ifel(sasa21, 1, NA)) |> sf::st_as_sf()

# Probability maps: most cells are ~0, so use a light-to-dark sequential ramp with a
# fourth-root stretch and draw the analysis domain in pale grey so the signal at the
# colonization front stands out instead of a black field.
prob_scale <- function(name, limits = c(0, 0.7)) scale_fill_gradientn(
  name = name, na.value = "transparent", limits = limits,
  colours = c("#fff7ec", "#fee8c8", "#fdbb84", "#e34a33", "#7f0000"),
  values = scales::rescale(c(0, 0.02, 0.08, 0.3, 0.7)^0.5, from = c(0, 0.7^0.5)),
  trans = scales::trans_new("root4", function(x) x^0.25, function(x) x^4),
  breaks = c(0, 0.01, 0.05, 0.2, 0.5))

pA <- ggplot() + geom_spatraster(data = suitA, maxcell = Inf) +
  scale_fill_viridis_c(name = "Suitability", na.value = "transparent", limits = c(0, 1)) +
  geom_sf(data = outline21, fill = NA, colour = "black", linewidth = 0.2)
pA <- map_axes(pA) + labs(tag = "(a)")
pB <- ggplot() + geom_spatraster(data = p9, maxcell = Inf) +
  prob_scale("P(colonized\nin 9 yr)", limits = c(0, 1)) +
  geom_sf(data = outline21, fill = NA, colour = "black", linewidth = 0.2)
pB <- map_axes(pB) + labs(tag = "(b)")
ggsave(file.path(DIR_PAPER_FILES, "fig_models_AB.png"), pA + pB, width = 14, height = 7.6, dpi = 300)

# ---- Figure: response curves of Model A (snow_mean, elevation, TPI, slope, twi) ---
# Reuse the WP2 PNGs by composing them; simplest robust route is a montage.
rc_files <- file.path(DIR_OUT, sprintf("response_A_primary_%s.png",
                      c("snow_mean", "elevation", "TPI", "slope", "twi", "northness")))
if (all(file.exists(rc_files))) {
  imgs <- lapply(rc_files, magick::image_read)
  row1 <- magick::image_append(do.call(c, imgs[1:3])); row2 <- magick::image_append(do.call(c, imgs[4:6]))
  magick::image_write(magick::image_append(c(row1, row2), stack = TRUE),
                      file.path(DIR_PAPER_FILES, "fig_response_A.png"))
}

# ---- Figure: dispersal kernel (distance-band observed vs predicted) ----------
cmp <- read.csv(file.path(DIR_OUT, "model_B_logdist_comparison.csv"))
band <- cmp[!is.na(cmp$distance_band_m) & cmp$model == "logdist", ]
band$distance_band_m <- factor(band$distance_band_m,
  levels = c("[0,5)", "[5,10)", "[10,20)", "[20,40)", "[40,80)", "[80,160)", ">=160"))
kd <- band |> select(distance_band_m, observed = observed_colonization_rate,
                     predicted = predicted_mean_p9) |>
  tidyr::pivot_longer(-distance_band_m, names_to = "type", values_to = "rate")
p_kernel <- ggplot(kd, aes(distance_band_m, rate, fill = type)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.65) +
  scale_y_log10(labels = scales::label_percent(accuracy = 0.01)) +
  scale_fill_manual(values = c(observed = "grey35", predicted = "#d95f02"),
                    labels = c("Observed 2012→2021", "Model B (fitted)"), name = NULL) +
  labs(x = "Distance to 2012 Sasa front (m)", y = "Nine-year colonization rate (log scale)") +
  base_theme + theme(legend.position = c(0.75, 0.85))
ggsave(file.path(DIR_PAPER_FILES, "fig_kernel_bands.png"), p_kernel, width = 7, height = 4.5, dpi = 300)

# ---- Figure: CA projection maps (s0, sm071) + trajectory ---------------------
pc0 <- rast(file.path(DIR_OUT, "ca_pcol_2030_s0.tif")); names(pc0) <- "p"
pc1 <- rast(file.path(DIR_OUT, "ca_pcol_2030_sm071.tif")); names(pc1) <- "p"
mk <- function(r, tag) {
  p <- ggplot() |> add_contours()
  p <- p + geom_spatraster(data = r, maxcell = Inf) + prob_scale("P(colonized\nby 2030)") +
    geom_sf(data = outline21, fill = NA, colour = "black", linewidth = 0.25)
  map_axes(p) + labs(tag = tag)
}
traj <- read.csv(file.path(DIR_OUT, "ca_trajectory.csv"))
traj <- traj[traj$scenario %in% c("s0", "sm071"), ]
traj$scenario <- factor(traj$scenario, levels = c("s0", "sm071"),
                        labels = c("s = 0", "s = −0.71 d yr⁻¹"))
p_traj <- ggplot(traj, aes(year, cumulative_expected_area_m2, colour = scenario)) +
  geom_ribbon(aes(ymin = cumulative_expected_area_m2 - replicate_sd_m2,
                  ymax = cumulative_expected_area_m2 + replicate_sd_m2, fill = scenario),
              alpha = 0.2, colour = NA) +
  geom_line(linewidth = 0.9) + geom_point(size = 1.6) +
  scale_colour_manual(values = c("#1b9e77", "#d95f02"), name = "Snowmelt scenario") +
  scale_fill_manual(values = c("#1b9e77", "#d95f02"), guide = "none") +
  scale_x_continuous(breaks = 2022:2030) +
  labs(x = "Year", y = expression("Cumulative expected new colonization (" * m^2 * ")"), tag = "(c)") +
  base_theme + theme(legend.position = c(0.25, 0.8))
p_ca <- (mk(pc0, "(a)") + mk(pc1, "(b)") + plot_layout(guides = "collect")) / p_traj +
  plot_layout(heights = c(1.6, 1))
ggsave(file.path(DIR_PAPER_FILES, "fig_ca_projection.png"), p_ca, width = 14, height = 12, dpi = 300)

# ---- Figure: risk map (s0) with MOE community composition bars --------------
rk <- rast(file.path(DIR_OUT, "risky_2030_s0.tif")); names(rk) <- "p"
p_rk <- ggplot() |> add_contours()
p_rk <- p_rk + geom_spatraster(data = rk, maxcell = Inf) +
  prob_scale("P(colonized\nby 2030)") +
  geom_sf(data = outline21, fill = NA, colour = "black", linewidth = 0.2)
p_rk <- map_axes(p_rk) + labs(tag = "(a)")
comp <- read.csv(file.path(DIR_OUT, "risky_composition.csv"))
comp <- comp[comp$scenario %in% c("s0", "sm071") & !is.na(comp$share_of_scenario_total), ]
top <- comp |> group_by(scenario) |> slice_max(share_of_scenario_total, n = 4) |> ungroup()
top$scenario <- factor(top$scenario, levels = c("s0", "sm071"), labels = c("s = 0", "s = −0.71"))
p_comp <- ggplot(top, aes(x = reorder(凡例名, share_of_scenario_total), y = 100 * share_of_scenario_total, fill = scenario)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.65) + coord_flip() +
  scale_fill_manual(values = c("#1b9e77", "#d95f02"), name = "Scenario") +
  labs(x = NULL, y = "Share of expected colonized area (%)", tag = "(b)") +
  base_theme + theme(legend.position = "bottom", axis.text.y = element_text(size = 10))
ggsave(file.path(DIR_PAPER_FILES, "fig_risk.png"), p_rk + p_comp + plot_layout(widths = c(1.2, 1)),
       width = 14, height = 7.6, dpi = 300)

# ---- Snowmelt figures: copy WP6 outputs into paper/files ---------------------
for (f in c("fig_snowmelt_annual_mean.png", "fig_snowmelt_slope_hist.png", "fig_snowmelt_slope_map.png"))
  file.copy(file.path(DIR_OUT, f), file.path(DIR_PAPER_FILES, f), overwrite = TRUE)

cat("Figures written to", DIR_PAPER_FILES, "\n")
finish_script("08_manuscript_figures.R", t0)
