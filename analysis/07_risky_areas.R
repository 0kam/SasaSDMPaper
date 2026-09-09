# =============================================================================
# 07_risky_areas.R -- 2030 risky areas and MOE vegetation composition
#
# A risky cell is a 2030 Sasa-colonization probability cell restricted to the
# 2021 "Other Vegetation" class (class 2). Sasa does not colonize shrub
# canopies, shrub classes are not conservation targets, and the MOE vegetation
# map supplies the community identity of the remaining class-2 cells.
#
# The MOE map is used only for community names. Its naturalness attributes are
# deliberately discarded before projection and never enter an output.
# =============================================================================

t0 <- Sys.time()
source(file.path("analysis", "00_config.R"))
set.seed(SEED_GLOBAL)

PATH_MOE <- file.path(REPO_ROOT, "data_external", "veg_murodo.gpkg")
SCENARIO_TAGS <- c("s0", "sm071", "sm224")
probability_paths <- stats::setNames(
  file.path(DIR_OUT, paste0("ca_pcol_2030_", SCENARIO_TAGS, ".tif")),
  SCENARIO_TAGS
)

required_inputs <- c(
  PATH_MOE, PATH_VEGE_2012, PATH_VEGE_2021, probability_paths
)
missing_inputs <- required_inputs[!file.exists(required_inputs)]
if (length(missing_inputs)) {
  stop("missing WP5 input(s): ", paste(missing_inputs, collapse = ", "))
}

created_files <- character(0)
deviations <- "none"

# Return weighted totals and cell counts for every represented MOE legend.
summarize_by_legend <- function(mask, weights, legend_ids, lookup) {
  if (length(mask) != length(weights) || length(mask) != length(legend_ids)) {
    stop("mask, weights and legend_ids must have identical lengths")
  }
  if (any(mask & is.na(legend_ids))) {
    stop(sum(mask & is.na(legend_ids)),
         " selected cells have no MOE community assignment")
  }
  cells <- which(mask)
  if (!length(cells)) stop("cannot summarize an empty cell set")
  if (any(!is.finite(weights[cells]))) {
    stop("selected cells contain non-finite weights")
  }
  ids <- sort(unique(as.integer(legend_ids[cells])))
  totals <- vapply(ids, function(id) {
    sum(weights[cells[legend_ids[cells] == id]])
  }, numeric(1))
  counts <- vapply(ids, function(id) {
    sum(legend_ids[cells] == id)
  }, integer(1))
  names_by_id <- lookup[["凡例名"]][match(ids, lookup$legend_id)]
  data.frame(
    legend_id = ids,
    "凡例名" = names_by_id,
    weighted_total = totals,
    n_cells = counts,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

msg("Loading reference and vegetation rasters")
reference <- terra::rast(PATH_VEGE_2012)
vege21 <- terra::rast(PATH_VEGE_2021)
assert_on_ref_grid(reference, "WP5 reference")
assert_on_ref_grid(vege21, "WP5 2021 vegetation")
reference_values <- as.numeric(terra::values(reference, mat = FALSE))
vege21_values <- as.numeric(terra::values(vege21, mat = FALSE))

# ---- MOE community raster and lookup ---------------------------------------
msg("Projecting and rasterizing MOE community legends")
moe <- terra::vect(PATH_MOE)
if (!"凡例名" %in% names(moe)) stop("MOE map has no 凡例名 attribute")

# Subset before projection so that no unneeded MOE attributes can propagate.
moe <- moe[, "凡例名", drop = FALSE]
legend_for_feature <- trimws(as.character(moe[["凡例名"]][, 1]))
if (anyNA(legend_for_feature) || any(!nzchar(legend_for_feature))) {
  stop("MOE map contains missing or empty 凡例名 values")
}
legend_names <- sort(unique(legend_for_feature), method = "radix")
legend_lookup <- data.frame(
  legend_id = seq_along(legend_names),
  "凡例名" = legend_names,
  check.names = FALSE,
  stringsAsFactors = FALSE
)
moe$legend_id <- match(legend_for_feature, legend_names)
moe <- terra::project(moe, CRS_ANALYSIS)

moe_legend <- terra::rasterize(
  moe, terra::rast(reference), field = "legend_id", background = NA
)
names(moe_legend) <- "legend_id"
assert_on_ref_grid(moe_legend, "WP5 MOE legend raster")
legend_id_values <- as.integer(terra::values(moe_legend, mat = FALSE))

moe_raster_path <- file.path(DIR_OUT, "moe_legend_id.tif")
moe_lookup_path <- file.path(DIR_OUT, "moe_legend_lookup.csv")
terra::writeRaster(
  moe_legend, moe_raster_path, overwrite = TRUE, datatype = "INT2U",
  NAflag = 0, gdal = "COMPRESS=DEFLATE"
)
utils::write.csv(
  legend_lookup, moe_lookup_path, row.names = FALSE, fileEncoding = "UTF-8"
)
created_files <- c(created_files, moe_raster_path, moe_lookup_path)

# terra::cellSize(..., transform = TRUE) supplies per-cell geodesic areas even
# though the reference grid itself uses a projected CRS.
cell_area_m2 <- as.numeric(terra::values(
  terra::cellSize(reference, unit = "m", transform = TRUE), mat = FALSE
))
if (any(!is.finite(cell_area_m2) | cell_area_m2 <= 0)) {
  stop("invalid geodesic cell-area weights")
}

# Load all probability surfaces first and require a common analysis domain.
probability_values <- vector("list", length(SCENARIO_TAGS))
names(probability_values) <- SCENARIO_TAGS
for (tag in SCENARIO_TAGS) {
  probability <- terra::rast(probability_paths[[tag]])
  assert_on_ref_grid(probability, paste("WP5 probability", tag))
  value <- as.numeric(terra::values(probability, mat = FALSE))
  finite <- !is.na(value)
  if (any(!is.finite(value[finite])) ||
      any(value[finite] < 0 | value[finite] > 1)) {
    stop("probability values outside [0,1] for scenario ", tag)
  }
  probability_values[[tag]] <- value
}
analysis_domain <- !is.na(probability_values[[SCENARIO_TAGS[[1]]]])
for (tag in SCENARIO_TAGS[-1]) {
  if (!identical(!is.na(probability_values[[tag]]), analysis_domain)) {
    stop("probability rasters do not share the same analysis domain")
  }
}

# ---- risky rasters and scenario composition -------------------------------
composition_rows <- list()
scenario_summaries <- list()
common_probability_max <- 0

for (i in seq_along(SCENARIO_TAGS)) {
  tag <- SCENARIO_TAGS[[i]]
  probability <- probability_values[[tag]]
  risky_domain <- vege21_values == 2 & !is.na(vege21_values) &
    !is.na(probability)
  risky_values <- rep(NA_real_, length(probability))
  risky_values[risky_domain] <- probability[risky_domain]
  risky <- rast_from_values(risky_values, reference, "p_colonization_by_2030")
  assert_on_ref_grid(risky, paste("WP5 risky raster", tag))

  risky_path <- file.path(DIR_OUT, paste0("risky_2030_", tag, ".tif"))
  terra::writeRaster(
    risky, risky_path, overwrite = TRUE, datatype = "FLT4S",
    gdal = "COMPRESS=DEFLATE"
  )
  created_files <- c(created_files, risky_path)
  common_probability_max <- max(
    common_probability_max, risky_values[risky_domain], na.rm = TRUE
  )

  by_legend <- summarize_by_legend(
    risky_domain, probability * cell_area_m2,
    legend_id_values, legend_lookup
  )
  scenario_total <- sum(by_legend$weighted_total)
  if (!is.finite(scenario_total) || scenario_total <= 0) {
    stop("non-positive risky expected area for scenario ", tag)
  }
  composition_rows[[i]] <- data.frame(
    scenario = tag,
    legend_id = by_legend$legend_id,
    "凡例名" = by_legend[["凡例名"]],
    expected_area_m2 = by_legend$weighted_total,
    share_of_scenario_total = by_legend$weighted_total / scenario_total,
    n_cells = by_legend$n_cells,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  scenario_summaries[[tag]] <- list(total = scenario_total)
}

# The class-2 denominator includes every 2021 class-2 cell, including cells
# outside the probability rasters' analysis domain.
class2_domain <- !is.na(vege21_values) & vege21_values == 2
class2_by_legend <- summarize_by_legend(
  class2_domain, cell_area_m2, legend_id_values, legend_lookup
)
class2_total <- sum(class2_by_legend$weighted_total)
composition_rows[[length(composition_rows) + 1L]] <- data.frame(
  scenario = "class2_domain",
  legend_id = class2_by_legend$legend_id,
  "凡例名" = class2_by_legend[["凡例名"]],
  expected_area_m2 = class2_by_legend$weighted_total,
  share_of_scenario_total = class2_by_legend$weighted_total / class2_total,
  n_cells = class2_by_legend$n_cells,
  check.names = FALSE,
  stringsAsFactors = FALSE
)

risky_composition <- do.call(rbind, composition_rows)
risky_composition_path <- file.path(DIR_OUT, "risky_composition.csv")
utils::write.csv(
  risky_composition, risky_composition_path, row.names = FALSE,
  fileEncoding = "UTF-8"
)
created_files <- c(created_files, risky_composition_path)

# ---- context composition ---------------------------------------------------
# Observed colonization is the Model-B response: non-Sasa in 2012 and Sasa in
# 2021. The context is descriptive and is not a separate validation dataset.
observed_colonization <- !is.na(reference_values) & reference_values != 1 &
  !is.na(vege21_values) & vege21_values == 1
context_masks <- list(
  analysis_domain = analysis_domain,
  observed_colonization = observed_colonization
)
context_rows <- lapply(names(context_masks), function(context_name) {
  by_legend <- summarize_by_legend(
    context_masks[[context_name]], cell_area_m2,
    legend_id_values, legend_lookup
  )
  context_total <- sum(by_legend$weighted_total)
  data.frame(
    context = context_name,
    legend_id = by_legend$legend_id,
    "凡例名" = by_legend[["凡例名"]],
    area_m2 = by_legend$weighted_total,
    share_of_context_total = by_legend$weighted_total / context_total,
    n_cells = by_legend$n_cells,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
})
moe_context <- do.call(rbind, context_rows)
moe_context_path <- file.path(DIR_OUT, "moe_context_composition.csv")
utils::write.csv(
  moe_context, moe_context_path, row.names = FALSE, fileEncoding = "UTF-8"
)
created_files <- c(created_files, moe_context_path)

# ---- figures ---------------------------------------------------------------
# Plain grey outside-domain background, common probability scale, English
# labels, no in-plot titles, axis text >= 11 pt, and 300 dpi as in WP6.
msg("Rendering risky-area maps")
class2_binary <- terra::ifel(vege21 == 2, 1, 0)
class2_contours <- terra::as.contour(
  class2_binary, maxcells = terra::ncell(class2_binary), levels = 0.5
)
outline <- as.data.frame(terra::geom(class2_contours))
outline$group <- interaction(outline$geom, outline$part, drop = TRUE)
plot_extent <- as.vector(terra::ext(reference))
if (!is.finite(common_probability_max) || common_probability_max <= 0) {
  common_probability_max <- 1
}

theme_risky <- ggplot2::theme_bw(base_size = 12) +
  ggplot2::theme(
    axis.text = ggplot2::element_text(size = 11, colour = "black"),
    axis.title = ggplot2::element_text(size = 12),
    panel.grid = ggplot2::element_blank(),
    panel.background = ggplot2::element_rect(fill = "grey94", colour = NA),
    plot.title = ggplot2::element_blank(),
    legend.title = ggplot2::element_text(size = 11),
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
  )

for (tag in SCENARIO_TAGS) {
  risky_path <- file.path(DIR_OUT, paste0("risky_2030_", tag, ".tif"))
  map_data <- as.data.frame(
    terra::rast(risky_path), xy = TRUE, na.rm = TRUE
  )
  names(map_data)[3] <- "probability"
  figure <- ggplot2::ggplot(
    map_data, ggplot2::aes(x = x, y = y, fill = probability)
  ) +
    ggplot2::geom_tile(
      width = terra::res(reference)[1], height = terra::res(reference)[2]
    ) +
    ggplot2::geom_path(
      data = outline,
      ggplot2::aes(x = x, y = y, group = group),
      inherit.aes = FALSE, linewidth = 0.22, colour = "black"
    ) +
    ggplot2::coord_equal(
      xlim = plot_extent[1:2], ylim = plot_extent[3:4], expand = FALSE
    ) +
    ggplot2::scale_fill_viridis_c(
      limits = c(0, common_probability_max), oob = scales::squish,
      na.value = "transparent", name = "Colonization\nprobability"
    ) +
    ggplot2::labs(x = "Easting (m)", y = "Northing (m)") +
    theme_risky
  figure_path <- file.path(DIR_OUT, paste0("fig_risky_2030_", tag, ".png"))
  ggplot2::ggsave(
    figure_path, figure, width = 6.5, height = 5.5, dpi = 300
  )
  created_files <- c(created_files, figure_path)
}

# ---- required execution summary -------------------------------------------
elapsed_seconds <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
cat("\n=== WP5 execution summary ===\n")
cat("Mode:", if (SASA_SMOKE) "SMOKE (same computation as full)" else "FULL", "\n")
cat(sprintf("Runtime: %.1f s\n", elapsed_seconds))
cat("Per-scenario expected newly colonized area within class 2:\n")
for (tag in SCENARIO_TAGS) {
  rows <- risky_composition[risky_composition$scenario == tag, , drop = FALSE]
  rows <- rows[order(rows$expected_area_m2, decreasing = TRUE), , drop = FALSE]
  scenario_summaries[[tag]]$top3 <- rows[seq_len(min(3L, nrow(rows))), , drop = FALSE]
  cat(sprintf("  %s: %.3f m2\n", tag, scenario_summaries[[tag]]$total))
  cat("    Top 3 communities:\n")
  for (j in seq_len(nrow(scenario_summaries[[tag]]$top3))) {
    z <- scenario_summaries[[tag]]$top3[j, ]
    cat(sprintf("      %s: %.2f%%\n", z[["凡例名"]],
                100 * z$share_of_scenario_total))
  }
}
cat("Files created:\n")
for (path in unique(created_files)) cat("  ", path, "\n", sep = "")
cat("Deviations:", deviations, "\n")

finish_script("07_risky_areas.R", t0)
