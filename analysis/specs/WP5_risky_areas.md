# Spec: WP5 — risky areas × MOE vegetation map (`analysis/07_risky_areas.R`)

Design authority: `review/decisions.md` §⑦「リスク域の再定義」. WP1–WP4 complete and
accepted. Same ground rules as previous WPs (write only under `analysis/`, Rscript-
runnable, seeds/config conventions, English comments). This WP is light — no model
fitting; full mode must run in < 10 min; SMOKE may simply run the same thing.

## Definition

Risky area = probability of *Sasa* colonization by 2030 (`analysis/out/
ca_pcol_2030_{s0,sm071,sm224}.tif`) **restricted to cells classified "Other
Vegetation" (class 2) in 2021** (`ortho/data/vege_2021_5x5.tiff`). Rationale (comment
it): *Sasa* does not colonize shrub canopies, and shrub classes are not conservation
targets; the community identity of class-2 cells comes from the MOE map.

## Inputs

- `data_external/veg_murodo.gpkg` (MOE 1:25,000 現存植生図, Hokuriku block clip,
  356 polygons, EPSG:6668). Attribute 凡例名 is the community legend.
- The three P rasters, the 2021 vegetation raster, the reference grid.

## Processing

1. Transform the MOE polygons to EPSG:6690 and rasterize 凡例名 (as integer legend id)
   onto the reference grid. Write the id↔凡例名 lookup CSV. **Do NOT carry 植生自然度
   into any output** (explicit author decision) — not as a column, not in captions.
2. Per scenario: risky raster = P masked to class-2 cells → `analysis/out/
   risky_2030_{s0,sm071,sm224}.tif`.
3. Per scenario: expected newly-colonized area within class 2 (Σ p × geodesic cell
   area) — total and **cross-tabulated by 凡例名** →
   `analysis/out/risky_composition.csv`
   (columns scenario, legend_id, 凡例名, expected_area_m2, share_of_scenario_total,
   n_cells). Also add rows for the class-2 domain itself (denominator context:
   n_cells and area per 凡例名 within class 2).
4. Context table (supplement material): the same cross-tab over ALL analysis-domain
   cells and over the observed 2012→2021 colonization cells →
   `analysis/out/moe_context_composition.csv`. (Reference values from an earlier
   ad-hoc run, for sanity: observed-colonization cells ≈ 66.8% イワイチョウ–
   ショウジョウスゲ群集, 21.5% コケモモ–ハイマツ群集, 11.4% タカネヤハズハハコ–
   アオノツガザクラ群集.)
5. One figure per scenario (300 dpi PNG, English labels except community names, no
   in-plot titles): map of risky P over a hillshade or plain background, class-2
   domain outlined. Keep styling consistent with WP6 figures.

## Acceptance (`analysis/tests/test_wp5.R`)

- risky rasters on the reference grid, values in [0,1], NA exactly outside
  (class 2 ∩ P non-NA)
- per scenario: Σ expected_area_m2 across legend rows equals the raster-wide
  Σ p × cell-area within class 2 (tolerance 1 m²)
- the string 自然度 appears in NO output CSV (header or values) under analysis/out/
- the observed-colonization context row reproduces the ≈66.8% leading share
  (tolerance ±2 percentage points; report, gate loosely)

## Deliverable

Run the script and the test; print files created, the per-scenario totals, the top-3
communities per scenario with shares, runtime, deviations.
