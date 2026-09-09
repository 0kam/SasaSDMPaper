# Acceptance tests for WP5 / risky areas and MOE vegetation composition.

t0 <- Sys.time()
source(file.path("analysis", "00_config.R"))
checker <- new_checker("WP5 acceptance")

scenario_tags <- c("s0", "sm071", "sm224")
reference <- terra::rast(PATH_VEGE_2012)
vege21_values <- as.numeric(terra::values(
  terra::rast(PATH_VEGE_2021), mat = FALSE
))
cell_area_m2 <- as.numeric(terra::values(
  terra::cellSize(reference, unit = "m", transform = TRUE), mat = FALSE
))

composition_path <- file.path(DIR_OUT, "risky_composition.csv")
context_path <- file.path(DIR_OUT, "moe_context_composition.csv")
lookup_path <- file.path(DIR_OUT, "moe_legend_lookup.csv")
moe_raster_path <- file.path(DIR_OUT, "moe_legend_id.tif")

checker$check("MOE legend raster exists", file.exists(moe_raster_path),
              moe_raster_path)
checker$check("MOE legend lookup exists", file.exists(lookup_path), lookup_path)
checker$check("risky composition CSV exists", file.exists(composition_path),
              composition_path)
checker$check("MOE context CSV exists", file.exists(context_path), context_path)

if (file.exists(moe_raster_path)) {
  moe_raster <- terra::rast(moe_raster_path)
  grid_ok <- tryCatch({
    assert_on_ref_grid(moe_raster, "WP5 MOE raster"); TRUE
  }, error = function(e) FALSE)
  checker$check("MOE legend raster is on reference grid", grid_ok)
}

composition <- if (file.exists(composition_path)) {
  utils::read.csv(composition_path, check.names = FALSE,
                  stringsAsFactors = FALSE, fileEncoding = "UTF-8")
} else {
  data.frame()
}
required_composition_columns <- c(
  "scenario", "legend_id", "凡例名", "expected_area_m2",
  "share_of_scenario_total", "n_cells"
)
checker$check(
  "risky composition has required columns",
  all(required_composition_columns %in% names(composition))
)
if (nrow(composition)) {
  checker$check(
    "composition covers scenarios and class-2 domain",
    setequal(unique(composition$scenario),
             c(scenario_tags, "class2_domain"))
  )
}

for (tag in scenario_tags) {
  probability_path <- file.path(
    DIR_OUT, paste0("ca_pcol_2030_", tag, ".tif")
  )
  risky_path <- file.path(DIR_OUT, paste0("risky_2030_", tag, ".tif"))
  figure_path <- file.path(DIR_OUT, paste0("fig_risky_2030_", tag, ".png"))
  checker$check(paste("risky raster exists:", tag),
                file.exists(risky_path), risky_path)
  checker$check(paste("300-dpi figure exists:", tag),
                file.exists(figure_path), figure_path)
  if (!file.exists(risky_path) || !file.exists(probability_path)) next

  probability <- terra::rast(probability_path)
  risky <- terra::rast(risky_path)
  grid_ok <- tryCatch({
    assert_on_ref_grid(risky, paste("WP5 risky", tag)); TRUE
  }, error = function(e) FALSE)
  checker$check(paste("risky raster on reference grid:", tag), grid_ok)

  probability_values <- as.numeric(terra::values(probability, mat = FALSE))
  risky_values <- as.numeric(terra::values(risky, mat = FALSE))
  expected_domain <- !is.na(vege21_values) & vege21_values == 2 &
    !is.na(probability_values)
  checker$check(
    paste("NA mask is exactly class 2 intersect P domain:", tag),
    identical(!is.na(risky_values), expected_domain),
    paste(sum(expected_domain), "cells")
  )
  checker$check(
    paste("risky values are in [0,1]:", tag),
    all(is.finite(risky_values[expected_domain])) &&
      all(risky_values[expected_domain] >= 0 &
          risky_values[expected_domain] <= 1),
    sprintf("range [%.6f, %.6f]",
            min(risky_values[expected_domain]),
            max(risky_values[expected_domain]))
  )

  if (nrow(composition)) {
    table_total <- sum(composition$expected_area_m2[
      composition$scenario == tag
    ])
    raster_total <- sum(risky_values[expected_domain] *
                          cell_area_m2[expected_domain])
    checker$check_near(
      paste("legend sum equals raster expected area:", tag),
      table_total, raster_total, 1
    )
  }
}

# The prohibited text must not occur in any output CSV, in headers or values.
output_csvs <- list.files(
  DIR_OUT, pattern = "[.]csv$", full.names = TRUE, recursive = TRUE
)
prohibited_hits <- vapply(output_csvs, function(path) {
  any(grepl("自然度", readLines(path, warn = FALSE, encoding = "UTF-8"),
            fixed = TRUE))
}, logical(1))
checker$check(
  "the prohibited naturalness string is absent from all output CSVs",
  !any(prohibited_hits),
  if (any(prohibited_hits)) {
    paste(basename(output_csvs[prohibited_hits]), collapse = ", ")
  } else {
    paste(length(output_csvs), "CSV files scanned")
  }
)

if (file.exists(context_path)) {
  context <- utils::read.csv(
    context_path, check.names = FALSE, stringsAsFactors = FALSE,
    fileEncoding = "UTF-8"
  )
  required_context_columns <- c(
    "context", "legend_id", "凡例名", "area_m2",
    "share_of_context_total", "n_cells"
  )
  checker$check(
    "MOE context has required columns",
    all(required_context_columns %in% names(context))
  )
  observed <- context[
    context$context == "observed_colonization", , drop = FALSE
  ]
  if (nrow(observed)) {
    leading_share <- max(observed$share_of_context_total)
    leading_name <- observed[["凡例名"]][
      which.max(observed$share_of_context_total)
    ]
    checker$check_near(
      "observed-colonization leading share is about 66.8%",
      leading_share, 0.668, 0.02
    )
    cat(sprintf("OBSERVED-COLONIZATION LEADING SHARE: %s = %.3f%%\n",
                leading_name, 100 * leading_share))
  } else {
    checker$check("observed-colonization context rows exist", FALSE)
  }
}

checker$report()
finish_script("tests/test_wp5.R", t0)
