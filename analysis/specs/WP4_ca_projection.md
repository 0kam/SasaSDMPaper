# Spec: WP4 — cellular-automaton projection 2021→2030 (`analysis/05_ca_projection.R`)

Design authority: `review/decisions.md` §⑦ layer C (worktree path:
`.claude/worktrees/review-summary-ece-ddc69c/review/decisions.md` from the repo root).
WP1–WP3 are complete and ACCEPTED. Reuse `analysis/00_config.R` conventions, the shared
folds (`analysis/out/folds.tif`), `sasa_distance()` from `analysis/R/distance.R`, and the
fitted Model B GAM (`analysis/out/models/model_B_gam.rds`, k = 3; inference model).

Ground rules identical to previous WPs: write only under `analysis/`; no git writes;
`Rscript`-runnable end to end; seeds from config; English comments; SMOKE mode
(`SASA_SMOKE=1`) must complete in < 10 min (small replicate count, 2 scenarios, and it
may coarsen nothing else — the grid stays 1 m).

## Mechanism (per replicate, per year t = 2022 … 2030)

1. `dist_t` = `sasa_distance(current_distribution, min_area_m2 = 5)` — same rule as the
   fitted predictor (consistency with training; see sensitivity below).
2. Per-cell nine-year probability `p9` from the GAM at (env_t, dist_t); convert to the
   annual rate `p1 = 1 - (1 - p9)^(1/9)`.
3. Every currently-empty eligible cell colonizes with `rbinom(1, p1)`; colonized cells
   are permanent (no extinction — state this assumption in comments).
4. Cells NA in predictors or outside the analysis domain never colonize. Cells that are
   2021 Sasa start occupied.

## Mandatory optimization (exact, not approximate)

The GAM is additive on the logit scale:
`logit(p9) = intercept + f_dist(dist) + Σ_j f_j(env_j)`.
Therefore precompute, once per scenario and year, the environmental part
`env_lp_t = intercept + Σ_j f_j(env_j,t)` as a raster (only `snow_mean` changes across
years; the other six terms are constant), and precompute `f_dist` as a fine lookup table
(0 … max distance, 0.5 m step, via `predict(..., type = "terms")`; linear interpolation
between knots of the lookup). Each replicate-year is then:
distance transform → lookup → `plogis(env_lp_t + f_dist)` → p1 → Bernoulli draw.
Validate the decomposition once in the script: for 10,000 random cells, compare the
lookup+sum reconstruction against direct `predict(type="response")`; require
max |Δp9| < 1e-6 and stop otherwise. Parallelize over replicates (10 workers max).

## Scenarios (snow shift)

`snow_mean_t = snow_mean + s × (t − ANCHOR_YEAR)` with `ANCHOR_YEAR = 2016` (midpoint of
the 2011–2021 climatology window; put the constant and a justifying comment in config).
Scenarios `s ∈ {0, −0.71, −2.24}` d/yr from `analysis/out/snowmelt_scenarios.csv`.
DOY floors at 0: clamp `snow_mean_t` to ≥ 0.

## Replicates and outputs (full mode)

- `N_CA_REPLICATES = 200` (config; SMOKE: 5, scenarios {0, −0.71} only).
- Per scenario:
  - `analysis/out/ca_pcol_2030_s{0|m071|m224}.tif` — P(colonized by 2030) = replicate
    mean, reference grid, [0,1]; NA where never eligible.
  - expected newly-colonized area by year: `Σp` over cells (geodesic, `expanse`-based
    cell areas), written to `analysis/out/ca_trajectory.csv`
    (columns scenario, year, expected_new_area_m2, cumulative_expected_area_m2,
    replicate_sd_m2).
  - one example realization raster per scenario (seeded) for figures.
- Runtime cap: full mode must stay under ~4 h wall clock; if the measured per-replicate
  cost projects beyond that, reduce to 100 replicates and print a prominent notice.

## Hindcast (calibration check — label it as such everywhere)

Initialize from the 2012 Sasa distribution, snow scenario s = 0 (the climatology IS the
fitting window), run 9 years, 200 replicates:
- expected gross colonized area vs the observed 4,097 m² (report the ratio; the earlier
  plan's ±50% band is a REPORTING threshold, not a gate),
- distance-band comparison: observed colonization rate vs simulated colonization
  frequency in bands [0,5), [5,10), [10,20), [20,40), [40,80), [80,160), ≥160 m,
- cell-level AUC of P(colonized) against observed 2012→2021 colonization.
Write `analysis/out/ca_hindcast_summary.csv` + a band-comparison CSV.
Known from review: the GAM under-predicts the [0,5) band (obs 0.100 vs pred ~0.056) and
over-predicts [5,20) — the hindcast quantifies the net effect; report, do not tune.

## Spatial out-of-block validation

For each shared fold k: refit the GAM (same formula/data pipeline as 04) excluding
block k, hindcast 2012→2021 with 50 replicates, score ONLY cells in block k:
AUC + expected-vs-observed colonized area. One CSV
(`analysis/out/ca_spatial_validation.csv`). This is the genuinely independent check.

## Sensitivity (cheap, run after the main set)

1. `min_area_m2 = 0` in the simulation's distance update (main uses 5): scenario s=−0.71
   only, 50 replicates → one row in a sensitivity CSV comparing 2030 expected area.
2. Annualization alternative: `p1 = p9/9` (first-order); same reduced setup, one row.

## Acceptance (`analysis/tests/test_wp4.R`)

- decomposition validation < 1e-6 (this one IS a gate)
- P rasters on the reference grid, [0,1], zero P on 2021 Sasa cells (they start
  occupied — "newly colonized" excludes them) and on never-eligible NA cells
- trajectory: cumulative expected area non-decreasing in year, for every scenario
- scenario ordering REPORTED not gated (earlier snowmelt may raise or lower area —
  zonal ΔHS analysis showed sign separation; print the ordering)
- hindcast ratio printed; warning (non-gating) if outside [0.5, 2.0]
- spatial validation CSV exists with 4 rows

## Deliverable

Run SMOKE for the script and the test; print files created, smoke numbers (hindcast
ratio, per-scenario expected areas), runtime, deviations. Do NOT run full mode.
