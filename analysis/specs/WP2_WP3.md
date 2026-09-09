# Spec: WP2 (Model A — environmental potential) + WP3 (Model B — colonization GAM)

You are implementing two analysis scripts for an ecology manuscript revision.
Authoritative design documents (read them first):

- `/Users/okamoto/NIES/SasaSDMPaper/.claude/worktrees/review-summary-ece-ddc69c/review/decisions.md` (§⑦ = the redesign)
- `/Users/okamoto/NIES/SasaSDMPaper/.claude/worktrees/review-summary-ece-ddc69c/review/implementation_plan_ja.md` (WP2/WP3 sections)

WP1 is complete and ACCEPTED. Reuse its conventions and helpers:
`analysis/00_config.R` (REPO_ROOT, paths, seeds, `assert_on_ref_grid`, `finish_script`, …)
and its output `analysis/out/predictors.tif` (7 layers: slope, elevation, TPI, twi,
northness, eastness, snow_mean — all bit-aligned to the vege reference grid).

## Ground rules

- Write ONLY under `analysis/` (scripts at top level, shared functions in `analysis/R/`,
  outputs in `analysis/out/`, tests in `analysis/tests/`). Never modify anything else.
- No git commands that write. Input data outside `analysis/` is read-only.
- Scripts must run front-to-back under `Rscript`, non-interactive, seeds from config,
  `sessionInfo()` at the end. English comments/messages. No `setwd()` to home paths.
- R 4.5.2; terra 1.9.34, sf, tidyverse, tidysdm, tidymodels, stacks, spatialsample,
  mgcv, DALEX, DALEXtra, ranger, xgboost, maxnet available. 12 cores / 24 GB.
  Use `num.threads = 10` / 10 workers maximum.
- **SMOKE mode**: every script must respect env var `SASA_SMOKE=1` → tiny tuning grid
  (grid = 2), subsampled data (~10%), 2 CV folds, so a complete smoke run of both WPs
  finishes in < 15 min. You RUN the smoke mode and the tests before finishing.
  The full run (default mode) will be executed by the reviewer — do not run it yourself.

## Shared infrastructure (build first, file `analysis/02_folds_and_distance.R`)

1. **Distance layer `dist12`**: from `ortho/data/vege_2012_5x5.tiff`, take class 1 (Sasa),
   polygonize, **keep polygons with area > 5 m² only** (geodesic area via `terra::expanse`
   on polygons; this is the project-wide convention), rasterize back to the reference
   grid, `terra::distance()`. Write `analysis/out/dist12.tif`. Same procedure must be
   reusable for any Sasa raster (make it a function `sasa_distance(rast, min_area_m2 = 5)`
   in `analysis/R/distance.R`) — later WPs reuse it with 2021/simulated distributions.
2. **Spatial blocks shared by BOTH models**: one blocking scheme (spatialsample
   `spatial_block_cv`-compatible), v = 4, seeded, defined over the full analysis domain.
   Persist the fold geometry AND the per-cell fold id raster to `analysis/out/folds.tif`
   + `analysis/out/folds.gpkg` so WP4's spatial validation can reuse the identical folds.
   Both models must draw their CV assignment from this artifact, not from independent
   `spatial_block_cv()` calls.

## WP2: `analysis/03_model_A.R` — environmental potential (successor of the TBM)

- Response: presence = 2021 Sasa cells (`vege_2021_5x5.tiff` class 1); pseudo-absence =
  non-Sasa cells thinned to one per 5 m cell (as in the legacy pipeline).
- Predictors: the 7 layers of `predictors.tif`. NO distance term. NO raw aspect.
- Two variants, both fitted and saved:
  (a) PRIMARY: no elevation cutoff;
  (b) SENSITIVITY: `elevation < 2560` filter (legacy convention).
- tidysdm workflow_set with rf, gam, maxent, boost_tree; `tune_grid` with
  `metric_set(tss_max)`, grid = 18 (full mode); stacks blend, `fit_members()`.
- Evaluation on the SHARED folds (from 02): per-fold and mean held-out TSS and AUC.
  Report the final ensemble composition (member algorithms + weights) to a CSV —
  the manuscript must state it (the old paper wrongly implied 4 algorithms survive).
- Threshold: compute max-TSS threshold ON HELD-OUT predictions (pooled across folds),
  save to CSV. Also produce a threshold-sensitivity table: suitable area (geodesic,
  `terra::expanse`) at cutoffs seq(0.1, 0.7, by = 0.05), both variants.
- Outputs: continuous suitability raster per variant (`suitability_A_{primary,cut2560}.tif`,
  on the reference grid), response curves for every predictor
  (DALEXtra partial-dependence on the fitted stack, held-out-consistent data frame;
  ggplot, English labels, 300 dpi PNGs), model objects as .rds under `analysis/out/models/`.

Acceptance (`analysis/tests/test_wp2.R`):
- suitability rasters on the reference grid (bit-exact), values in [0,1]
- held-out TSS reported; if mean TSS < 0.50, the test still PASSES but must print a
  prominent warning (honest reporting, not gatekeeping)
- ensemble composition CSV exists and lists ≥1 member
- threshold table covers both variants × 13 cutoffs

## WP3: `analysis/04_model_B.R` — colonization GAM (successor of the TDM)

- Response: among cells that were NOT Sasa in 2012 (any class ≠ 1, including bare ground),
  colonized = 1 if class 1 in 2021, else 0. Expect ~4,097 positives.
- Predictors: the 7 environmental layers + `dist12`.
- Model: `mgcv::bam(colonized ~ s(dist12) + s(snow_mean) + s(elevation) + s(slope) +
  s(TPI) + s(twi) + s(northness) + s(eastness), family = binomial, discrete = TRUE,
  nthreads = 10)` on ALL eligible cells (no background subsampling; bam handles it).
  You may adjust k per smooth with justification in comments; keep the model additive
  (no tensor interactions in the primary model).
- Evaluation: leave-one-block-out on the SHARED folds — refit on 3 blocks, predict the
  held-out block; report per-fold and pooled AUC (and TSS at the pooled max-TSS point).
  Also fit-on-all for the final inference model.
- Robustness companion: the same response/predictors through tidysdm (maxent + xgb only,
  grid = 8) on the same folds; report AUC side by side in one CSV. This exists to show
  the GAM is not leaving signal on the table; it is not the inference model.
- Outputs:
  - `analysis/out/models/model_B_gam.rds`
  - smooth-term plots for ALL smooths (`gratia` if installed, else base `plot.gam` via
    ggplot re-draw), especially `s(dist12)` — this is the dispersal kernel figure
  - `analysis/out/p9_colonization.tif`: predicted 9-year colonization probability for
    every currently (2021) non-Sasa cell, using **dist recomputed from the 2021
    distribution** via `sasa_distance()` (this is the CA's t=0 input)
  - a kernel export CSV: predicted p9 vs dist12 at the domain-median values of the other
    predictors, dist 0–400 m by 5 m — WP4 documentation uses it
- Acceptance (`analysis/tests/test_wp3.R`):
  - n positives in [4000, 4200]; blocked AUC reported (expect ≈ 0.75–0.90; print, do not
    gate); `s(dist12)` effect monotone non-increasing over 0–200 m on the kernel export
    (allow small wiggles: check Spearman rho(p9, dist) < −0.9 on the export)
  - `p9_colonization.tif` on the reference grid, values in [0,1], NA exactly where 2021
    Sasa is present or predictors are NA

## Deliverable

Run `SASA_SMOKE=1` for 02→03→04 and both tests; print a summary table (files created,
smoke test results, runtimes, deviations from this spec with reasons). Print the summary
as your final output. Do NOT run the full (non-smoke) mode.
