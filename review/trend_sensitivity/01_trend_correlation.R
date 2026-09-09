# =============================================================================
# review/trend_sensitivity/01_trend_correlation.R
#
# SI sensitivity analysis 1: is the per-pixel snowmelt trend field structured
# with respect to the snowmelt climatology or elevation?
#
# Outputs (review/trend_sensitivity/):
#   trend_correlation.csv        Spearman rho (+ Pearson r) with n
#   trend_by_snow_bin.csv        10-day climatology bins: mean +/- SD of trend
#   trend_by_elevation_bin.csv   50 m elevation bins: mean +/- SD of trend
#   fig_trend_vs_snow_mean.png   2D bin heat map
#   fig_trend_vs_elevation.png   2D bin heat map
# =============================================================================

t0 <- Sys.time()
REPO <- Sys.getenv("SASA_REPO_ROOT", unset = "/Users/okamoto/NIES/SasaSDMPaper")
source(file.path(REPO, "analysis", "00_config.R"))
DIR_REVIEW <- file.path(REPO, "review", "trend_sensitivity")
set.seed(SEED_GLOBAL)

trend_df <- readRDS(file.path(DIR_REVIEW, "trend_domain.rds"))
msg("n = ", nrow(trend_df))

# -----------------------------------------------------------------------------
# 1. Correlations
# -----------------------------------------------------------------------------
# Spearman on 1.2 M values is computed as Pearson on the ranks (identical, and
# avoids cor()'s internal copy of a 1.2 M x 2 matrix).
spearman <- function(a, b) {
  stats::cor(rank(a), rank(b))
}
cor_tbl <- data.frame(
  pair = c("trend ~ snow_mean", "trend ~ elevation", "snow_mean ~ elevation"),
  n = nrow(trend_df),
  spearman_rho = c(
    spearman(trend_df$trend, trend_df$snow_mean),
    spearman(trend_df$trend, trend_df$elevation),
    spearman(trend_df$snow_mean, trend_df$elevation)
  ),
  pearson_r = c(
    stats::cor(trend_df$trend, trend_df$snow_mean),
    stats::cor(trend_df$trend, trend_df$elevation),
    stats::cor(trend_df$snow_mean, trend_df$elevation)
  ),
  stringsAsFactors = FALSE
)
cor_tbl$spearman_r2 <- cor_tbl$spearman_rho^2
utils::write.csv(cor_tbl, file.path(DIR_REVIEW, "trend_correlation.csv"),
                 row.names = FALSE)
print(cor_tbl)

# Overall trend distribution, restated on this cell set for the report.
dist_tbl <- data.frame(
  quantity = c("trend_mean", "trend_sd", "trend_median",
               "trend_q05", "trend_q95", "pct_negative"),
  value = c(mean(trend_df$trend), stats::sd(trend_df$trend),
            stats::median(trend_df$trend),
            as.numeric(stats::quantile(trend_df$trend, 0.05)),
            as.numeric(stats::quantile(trend_df$trend, 0.95)),
            100 * mean(trend_df$trend < 0))
)
utils::write.csv(dist_tbl, file.path(DIR_REVIEW, "trend_distribution.csv"),
                 row.names = FALSE)
print(dist_tbl)

# -----------------------------------------------------------------------------
# 2. Binned summaries
# -----------------------------------------------------------------------------
bin_summary <- function(x, breaks, labeller) {
  b <- cut(x, breaks = breaks, right = FALSE, include.lowest = TRUE)
  keep <- !is.na(b)
  s <- do.call(rbind, lapply(split(trend_df$trend[keep], droplevels(b[keep])),
                             function(v) data.frame(
                               n_cells = length(v),
                               trend_mean = mean(v),
                               trend_sd = stats::sd(v),
                               trend_median = stats::median(v)
                             )))
  s <- cbind(bin = rownames(s), s, stringsAsFactors = FALSE)
  rownames(s) <- NULL
  names(s)[1] <- labeller
  # Bins holding fewer than MIN_BIN_N cells are edge slivers of the camera
  # footprint (a few hundred cells at most). They are kept in the table for
  # completeness but excluded from the figure overlay and from the structure
  # summary, where they would otherwise dominate a between-bin spread.
  s$well_sampled <- s$n_cells >= MIN_BIN_N
  s
}
MIN_BIN_N <- 1000L

snow_breaks <- seq(floor(min(trend_df$snow_mean) / 10) * 10,
                   ceiling(max(trend_df$snow_mean) / 10) * 10, by = 10)
snow_tbl <- bin_summary(trend_df$snow_mean, snow_breaks, "snow_mean_bin_doy")
utils::write.csv(snow_tbl, file.path(DIR_REVIEW, "trend_by_snow_bin.csv"),
                 row.names = FALSE)
print(snow_tbl)

elev_breaks <- seq(floor(min(trend_df$elevation) / 50) * 50,
                   ceiling(max(trend_df$elevation) / 50) * 50, by = 50)
elev_tbl <- bin_summary(trend_df$elevation, elev_breaks, "elevation_bin_m")
utils::write.csv(elev_tbl, file.path(DIR_REVIEW, "trend_by_elevation_bin.csv"),
                 row.names = FALSE)
print(elev_tbl)

# Spread of the bin means relative to the within-bin spread: if the trend field
# carried strong structure along these axes, the bin means would vary by much
# more than a small fraction of the pixel-level SD.
snow_w <- snow_tbl[snow_tbl$well_sampled, , drop = FALSE]
elev_w <- elev_tbl[elev_tbl$well_sampled, , drop = FALSE]
structure_tbl <- data.frame(
  axis = c("snow_mean (10 d bins)", "elevation (50 m bins)"),
  n_bins = c(nrow(snow_w), nrow(elev_w)),
  bin_mean_min = c(min(snow_w$trend_mean), min(elev_w$trend_mean)),
  bin_mean_max = c(max(snow_w$trend_mean), max(elev_w$trend_mean)),
  bin_mean_range = c(diff(range(snow_w$trend_mean)),
                     diff(range(elev_w$trend_mean))),
  sd_of_bin_means = c(stats::sd(snow_w$trend_mean),
                      stats::sd(elev_w$trend_mean)),
  pixel_level_sd = stats::sd(trend_df$trend),
  stringsAsFactors = FALSE
)
# Weighted (by cell count) version, which is what a landscape-average shift
# would actually feel.
wsd <- function(tbl) {
  w <- tbl$n_cells / sum(tbl$n_cells)
  m <- sum(w * tbl$trend_mean)
  sqrt(sum(w * (tbl$trend_mean - m)^2))
}
structure_tbl$sd_of_bin_means_cellweighted <- c(wsd(snow_w), wsd(elev_w))
structure_tbl$variance_explained_by_bins <-
  structure_tbl$sd_of_bin_means_cellweighted^2 / structure_tbl$pixel_level_sd^2
utils::write.csv(structure_tbl,
                 file.path(DIR_REVIEW, "trend_structure_summary.csv"),
                 row.names = FALSE)
print(structure_tbl)

# -----------------------------------------------------------------------------
# 3. 2D bin heat maps
# -----------------------------------------------------------------------------
theme_si <- ggplot2::theme_bw(base_size = 12) +
  ggplot2::theme(
    axis.text = ggplot2::element_text(size = 11, colour = "black"),
    axis.title = ggplot2::element_text(size = 12),
    panel.grid.minor = ggplot2::element_blank(),
    plot.title = ggplot2::element_blank()
  )

# The trend axis is trimmed to the 0.1-99.9 percentiles for display, exactly as
# analysis/06_snowmelt_stats.R trims its histogram; the number of cells outside
# the plotted range is reported.
ylim <- as.numeric(stats::quantile(trend_df$trend, c(0.001, 0.999)))
n_out <- sum(trend_df$trend < ylim[1] | trend_df$trend > ylim[2])
msg(sprintf("trend axis trimmed to [%.2f, %.2f]; %d cells (%.3f%%) outside",
            ylim[1], ylim[2], n_out, 100 * n_out / nrow(trend_df)))

heat_plot <- function(xvar, xlab, bins_x, bin_tbl, bin_x_centre) {
  d <- data.frame(x = trend_df[[xvar]], y = trend_df$trend)
  d <- d[d$y >= ylim[1] & d$y <= ylim[2], ]
  ov <- data.frame(x = bin_x_centre, y = bin_tbl$trend_mean,
                   lo = bin_tbl$trend_mean - bin_tbl$trend_sd,
                   hi = bin_tbl$trend_mean + bin_tbl$trend_sd)
  ov <- ov[bin_tbl$well_sampled, , drop = FALSE]
  ggplot2::ggplot(d, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_bin2d(bins = c(bins_x, 90)) +
    ggplot2::scale_fill_viridis_c(trans = "log10", name = "Cells",
                                  option = "mako", direction = -1) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey30", linewidth = 0.5) +
    ggplot2::geom_hline(yintercept = mean(trend_df$trend), colour = "grey30",
                        linewidth = 0.5, linetype = "dashed") +
    ggplot2::geom_linerange(data = ov,
                            ggplot2::aes(x = .data$x, ymin = .data$lo,
                                         ymax = .data$hi),
                            inherit.aes = FALSE, colour = "#b2182b",
                            linewidth = 0.5) +
    ggplot2::geom_point(data = ov, ggplot2::aes(x = .data$x, y = .data$y),
                        inherit.aes = FALSE, colour = "#b2182b", size = 1.4) +
    ggplot2::labs(
      x = xlab,
      y = expression(paste("Pixelwise snowmelt trend (days ", year^-1, ")"))
    ) +
    ggplot2::coord_cartesian(ylim = ylim) +
    theme_si
}

snow_centre <- snow_breaks[-length(snow_breaks)][
  seq_len(nrow(snow_tbl))] + 5
elev_centre <- elev_breaks[-length(elev_breaks)][
  seq_len(nrow(elev_tbl))] + 25

p1 <- heat_plot("snow_mean", "Mean snowmelt date, 2011-2021 (day of year)",
                80, snow_tbl, snow_centre)
ggplot2::ggsave(file.path(DIR_REVIEW, "fig_trend_vs_snow_mean.png"), p1,
                width = 6.5, height = 4.5, dpi = 300)

p2 <- heat_plot("elevation", "Elevation (m)", 80, elev_tbl, elev_centre)
ggplot2::ggsave(file.path(DIR_REVIEW, "fig_trend_vs_elevation.png"), p2,
                width = 6.5, height = 4.5, dpi = 300)

msg("01_trend_correlation.R finished in ",
    sprintf("%.1f", as.numeric(difftime(Sys.time(), t0, units = "secs"))), " s")
