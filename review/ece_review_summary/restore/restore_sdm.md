# restore_sdm.md — provenance of the habitat suitability models

Everything below was executed. Scripts and their logs are copied to
`/Users/okamoto/NIES/SasaSDMPaper/.claude/worktrees/review-summary-ece-ddc69c/review/restore/sdm_scripts/`
(`i01`–`i06` = introspection of the archived objects, `v01`–`v07` = verification,
`xgbtest.R` / `xgbexport.R` = version pin).
New rasters written to `/Users/okamoto/NIES/SasaSDMPaper/.claude/worktrees/review-summary-ece-ddc69c/review/restore/out/`:
`reg_{pub,fix}_{tdm,tbm}{21,30}.tiff` (§7) and `refit_pol12_p{21,30}.tiff` (§6, F-4).
Nothing outside `review/restore/` and the scratchpad was written.

Environment: R 4.5.2, terra 1.9-34, tidysdm 1.0.4, tidymodels 1.5.0, stacks 1.1.1,
**xgboost pinned to 1.7.6.1** in a scratch library (see §7).

---

## 0. Headline

| | verdict |
|---|---|
| The repo's `sdm_tbm.R` cannot have made the archived TBM | **confirmed**, and the version that did is **not on the server either** — it is lost |
| `twi` in any archived model | **no**, settled four independent ways |
| Distance train/predict mismatch | **real, and it is in the script as written** — a clean linear run reproduces it. Bug fix, not documentation fix |
| F-3 (90 % of 2030 expansion is the distance layer) | **CONFIRMED to the decimal** |
| F-4 (consistent distance ⇒ 12,766 → 18,103 m², +41.8 %) | **CONFIRMED to the decimal**. A second, gentler repair (refit on polygon distance) gives 14,928 m² — also away from the observed 10,170 |
| F-5 (52 % of suitable cells sit on excluded `dist==0` pixels) | **CONFIRMED**; counter-claim **partly** supported — TDM still beats TBM everywhere, but its own TSS *falls*, it does not strengthen |
| Half-pixel snowmelt misregistration | **cosmetic for the five published areas** (≤0.6 % except one at +7.4 %); not cosmetic for per-cell HS in the TBM (2.6 % of cells cross 0.5) |

---

## 1. Script inventory and diff table

`diff` exit status, byte level.

| server file | repo counterpart | result |
|---|---|---|
| `data_from_server/ortho/sdm.R` (489 L) | `scripts/sdm/sdm_tbm.R` (489 L) | **identical** (`diff` exit 0) |
| `data_from_server/ortho/sdm_include_distance.R` (444 L) | `scripts/sdm/sdm_tdm.R` (444 L) | **identical** |
| `data_from_server/ortho/analyse_sdm.R` | `scripts/sdm/analyse_sdm.R` | **identical** |
| `data_from_server/ortho/preprocess_snow_data.R` | `scripts/sdm/preprocess_snow_data.R` | **identical** |
| `data_from_server/ortho/plot_vegetation_map.R` | `scripts/sdm/plot_vegetation_map.R` | **identical** |
| `data_from_server/ortho/plot_smnowmelt_shifts_map.R` | `scripts/sdm/plot_snowmelt_shifts_map.R` | **identical** (typo fixed on rename) |
| `data_from_server/ortho/sdm_sasainc.R` (143 L) | — | absent from repo |
| `data_from_server/ortho/analyse.R` (288 L) | — | absent from repo |
| `data_from_server/ortho/analyse_chamges.R` (71 L) | — | absent from repo |
| `data_from_server/ortho/select_sasa_communities.R` (56 L) | — | absent from repo |
| `data_from_server/ortho/plot_snowmelt_shift.R` | — | absent from repo |
| `data_from_server/scripts/models/model_comparison.R` | — | **not an SDM script** — it is the vegetation-classifier CV plot (`runs/cv/*stratified_cv.csv`, `~/VegetationMapPaper`) |

`md5` of `ortho/models.rds` and `ortho/model_stack.rds` are identical between the repo and the
server dump. **The server dump adds no SDM code and no new fitted object.** The script version
that produced the archived TBM is genuinely gone; §2 reconstructs what it must have said.

### Cross-diff between the two SDM scripts

`sdm.R` and `sdm_include_distance.R` are the same file forked. Differences that matter:

| | `sdm.R` (TBM) | `sdm_include_distance.R` (TDM) |
|---|---|---|
| distance predictor | none | `sasa_dist` from **all** 2012 Sasa pixels (L44-47), added at L56 |
| extra row filter | — | `filter(dist > 0)` (L69) |
| algorithms | `rf` only; `gam`/`maxent`/`xgb` **commented out** (L100-112); `update_workflow_model("default_gam", …)` also commented (L117-119) | all four live (L108-124), gam formula update live (L128-130) |
| workflow-set save | `saveRDS(models, "models_wo_dist_twi.rds")` (L127), read back L129 | `saveRDS(models, "models.rds")` (L138) |
| stack save | `model_stack_wo_dist.rds` (L149) | `model_stack.rds` (L158) |
| test-metric file | `tss_score_tbm.csv` (L161) | `tss_tdm.csv` (L168) |
| DALEX explainer | `DALEX::explain` on **test** set (L173-180) | `DALEXtra::explain_tidymodels` on **train** set (L182-189) |
| prediction rasters | `data/sasa_pred_sdm_{21,30}.tiff` (L382-383) | `data/sasa_pred_tdm_{21,30}.tiff` (L412-413) |
| `autoplot` legend | `labels = c("GBT","GAM","MaxEnt","RF")` — **four** labels, though only RF is trained | same four labels, four trained |

The four-label legend surviving in the RF-only `sdm.R` is the fingerprint of the edit: the file
was cut down *after* the published figure was made.

---

## 2. Which script produced which archived object

Evidence taken from **inside** the fitted objects, not filenames (`i01_inspect.R`,
`i02_stacks.R`, `i04_pointmatch.R`, `i05_all5m.R`, `i06_resp.R`).

### `models.rds` → `model_stack.rds` (the published **TDM**)

```
models.rds : workflow_set, 4 rows: default_rf, default_gam, default_maxent, default_xgb
  recipe roles: aspect roughness slope elevation TPI TRI snow dist | X,Y coords | sasa outcome
  metric set   : tss_max only
  resamples    : spatial_block_cv v=4, analysis sizes 13310 / 11379 / 13542 / 11557 of 16596
  candidates   : rf 8, gam 1, maxent 18, xgb 18   (rf grid = mtry 1..8 -> EXACTLY 8 predictors)
model_stack.rds$splits : identical 13310 / 11379 / 13542 / 11557
```

⇒ `model_stack.rds` was blended from `models.rds`; both come from
`sdm_include_distance.R` L105-158 **as archived, unmodified**. Confirmed downstream: predicting
with that stack reproduces `data/sasa_pred_tdm_21.tiff` to `maxabs 4.6e-8` and
`sasa_pred_tdm_30.tiff` to `4.5e-8` (`review/restore/out/repro_tdm_snow21_distpol12.tiff`,
`…_snow30_distpol21.tiff`).

### `models_wo_dist.rds` → `model_stack_wo_dist.rds` (the published **TBM**)

```
models_wo_dist.rds : 4 rows, all four algorithms present
  recipe roles: aspect roughness slope elevation TPI TRI snow   (7 predictors, no dist, no twi)
  candidates  : rf 7, gam 1, maxent 18, xgb 18  = 44   (rf grid = mtry 1..7 -> 7 predictors)
  resamples   : analysis 17282 / 14112 / 17899 / 14874 of 21389
model_stack_wo_dist.rds$splits : identical
ortho/figures/model_performance_tbm.png : 44 workflow ranks, legend GBT/GAM/MaxEnt/RF
```

The archived `sdm.R` trains **only** RF and writes **`models_wo_dist_twi.rds`** — a file that does
not exist anywhere. So the archived TBM was produced by an earlier state of `sdm.R` in which

1. the `models = list(rf, gam, maxent, xgb)` block (L96-113) was **uncommented**,
2. `update_workflow_model("default_gam", spec = sdm_spec_gam(), formula = gam_formula(rec))`
   (L117-119) was **uncommented**, and
3. L127 read `saveRDS(models, "models_wo_dist.rds")` and L129
   `readRDS("models_wo_dist.rds")`.

The commenting-out and the `_twi` rename came **afterwards**, for a TWI experiment that was
started and abandoned (§3). Everything downstream of L129 in the current file is unchanged from
the version that made the published figures — which is why `model_performance_tbm.png`,
`vi_tbm.png`, `tss_score_tbm.csv` and `sasa_pred_sdm_{21,30}.tiff` are all mutually consistent
with the four-algorithm object.

### `models_all_5m.rds` — an orphan; no published number depends on it

```
6 predictors, in filter_collinear() output order : slope snow dist elevation aspect TPI
  (roughness and TRI dropped -- the collinear pair)
metric set : sdm_metric_set()  = boyce_cont + roc_auc + tss_max   (NOT tss_max alone)
train n = 17991,  presence 1076 / absence 16915
rf candidates = 6   (mtry 1..6 -> 6 predictors)
```

Point-matched at the stored geometries (`i05`, `i06`):
`dist` = distance from **all** 2012 Sasa pixels (maxabs 0 over 17,991 points),
`snow` = `fitted_2021.tiff` (maxabs 0), `elevation < 2560`, `min(dist) = 1`, no `dist == 0`.
**Response identified**: all 1,076 presences are cells with `vege2012 ≠ Sasa & vege2021 = Sasa`
and `sasa_inc.tiff == 1`; all 16,915 absences are `≠ Sasa` in both years and `sasa_inc == 0`.
So this is the *Sasa-increase* model recast as classification with `filter_collinear` — a
sibling of `sdm_sasainc.R` (which is regression on `sasa_inc` with `rmse`). **No script in the
repo or the dump produces it**, it was never blended into a stack, and nothing in the manuscript
uses it. Recommend excluding it from the deposit or labelling it "exploratory, unused".

### Two other loose artefacts found

* `ortho/tss_score_tbm.rds` reports `tss_max = 0.53484`, while `ortho/tss_score_tbm.csv` reports
  **0.55559** (the manuscript's 0.55). Same script writes both names at different times
  (`sdm.R:161` writes the CSV only). The `.rds` is from a different TBM fit. Do not deposit both.
* `data/sasa_pred_sdm_dist_{21,30}.tiff` (and `data/sasa_pred_sdm_12.tiff`, 1157×1213 — a
  different grid) are an **older TDM vintage**. None of the eight snow × distance combinations
  reproduces them (min `meanabs` 0.062). They are the layers loaded in the archived QGIS projects
  `data_from_server/sasa_increase/sasa_sdm.qgz` (datasource `../../../ダウンロード/…`) — i.e. a
  superseded run kept for figure work. They are not the published maps and should not be deposited
  as such.

---

## 3. `twi.tif` — settled

**It is in no archived model.** Four independent lines of evidence:

1. **Recipe `var_info`** of `models.rds` / `models_wo_dist.rds` / `models_all_5m.rds` — no `twi`.
2. **RF candidate counts** (a fingerprint the audit did not use). `tune_grid(grid = 18)` on
   `rand_forest(mtry = tune())` can only propose as many distinct `mtry` values as there are
   predictors. Observed: **8** for the TDM, **7** for the TBM, **6** for `all_5m` — exactly the
   predictor counts *without* `twi`.
3. **Published variable-importance figures**: `vi_tdm.png` has exactly 8 boxes
   (dist, snow, elevation, aspect, TRI, roughness, slope, TPI); `vi_tbm.png` exactly 7.
4. **Layer order.** Today `list.files("data/terrain_features/") |> str_subset(".tif$")` returns
   `aspect, roughness, slope, tateyamadem_small, TPI, TRI, twi`, so `twi` lands **last**:
   `aspect roughness slope elevation TPI TRI twi`. The archived order ends `… TPI TRI [snow] [dist]`
   with no `twi` inserted. `twi.tif` was not in that directory when the models were fitted.

It is not that `twi` was read and dropped: extracting `twi.tif` at the archived training points
gives mean 5.712 (TDM) / 5.530 (TBM) with **0 % NA**. The layer is perfectly usable; it simply
was not there yet. Combined with the dead `models_wo_dist_twi.rds` name in `sdm.R`, the sequence
is: publish → add `twi.tif` → cut `sdm.R` down to RF for a quick TWI test → save under a new
name so the published object is not clobbered → abandon.

### What a corrected clean run must exclude

```r
# NOT list.files("data/terrain_features/") |> str_subset(".tif$")
terrain_files <- file.path("data/terrain_features",
  c("aspect.tif","roughness.tif","slope.tif","tateyamadem_small.tif","TPI.tif","TRI.tif"))
```

Two files must be kept out, not one:

* `twi.tif` — never in the published models;
* `snow_mean.tif`, `snow_sd.tif`, `snow_reg.tif` — `preprocess_snow_data.R:67, 96, 137` writes
  these **into `data/terrain_features/`**. Running the snow preprocessing before the SDM (the
  natural order) inflates the glob to 9-10 layers. `ortho/data/snow/` also holds copies.

Also required for a clean run, though outside this question: `terra::crs(terrain) <- terra::crs(vege12)`
after `resample()` — the DEM products carry EPSG:3099 and the vegetation/snow rasters EPSG:6690, and
without the relabel `bind_rows()` aborts with `Error: CRS mismatch` (reproduced in `i03.log`).
The archived training geometry is on the 6690 grid, so the original run did not hit this.

---

## 4. Exact predictor lists (Methods-ready)

Both models: response `sasa` = Sasa presence/absence in the **2021** vegetation map, as a factor
with `presence` as the first level; predictors extracted at the 1 m vegetation grid; absences
thinned with `thin_by_cell()` on a 5×-aggregated elevation grid; `filter(elevation < 2560)`;
`spatial_initial_split(prop = 0.2, spatial_block_cv)`; tuning by `spatial_block_cv(v = 4)`,
`grid = 18`, metric `tss_max`; ensemble by `stacks::blend_predictions()` (LASSO, `mixture = 1`),
chosen `penalty = 0.1`, then `fit_members()`.

### TBM — `ortho/model_stack_wo_dist.rds`

7 predictors, all resampled (terra default = **bilinear**) to `vege_2012_5x5.tiff`:

`aspect`, `roughness`, `slope`, `elevation` (= `tateyamadem_small`), `TPI`, `TRI`
— all from the 4.97 × 6.16 m DEM product — plus `snow` = `data/snow/fitted_2021.tiff`.

Training frame n = **21,389** (7,820 presence / 13,569 absence). Blend keeps **4 of 44** candidates:

| member | spec |
|---|---|
| `default_maxent_1_17` | maxnet, `feature_classes = "lqph"`, `regularization_multiplier = 0.5914`; coef 0.178 |
| `default_maxent_1_12` | maxnet, `"lqpht"`, `reg = 1.9846`; coef 0.532 |
| `default_xgb_1_05` | mtry 3, trees 94, depth 2, lr 0.12188, loss_reduction 1.457e-10, stop_iter 13; coef 1.958 |
| `default_xgb_1_09` | mtry 4, trees 522, depth 3, lr 0.07480, loss_reduction 1.130, stop_iter 18; coef 0.311 |

intercept −1.313. **No RF, no GAM survives**, though Methods (`matmet.qmd:117`) lists four algorithms.

### TDM — `ortho/model_stack.rds`

The same 7 plus `dist`; additional row filter `filter(dist > 0)`.
Training frame n = **16,596** (3,123 presence / 13,473 absence). Blend keeps **7 of 45**:

| member | spec |
|---|---|
| `default_maxent_1_02` | `"lqph"`, `reg = 1.5086`; coef 0.681 |
| `default_maxent_1_14` | `"lqph"`, `reg = 1.8683`; coef 0.0374 |
| `default_maxent_1_03` | `"lqpht"`, `reg = 2.3152`; coef 0.0153 |
| `default_xgb_1_02` | mtry 2, trees 893, depth 3, lr 0.030963, loss_red 1.264e-4, stop_iter 17 (niter 893); coef 0.153 |
| `default_xgb_1_04` | mtry 2, trees 1094, depth 12, lr 0.023794, loss_red 29.545, stop_iter 12 (**niter 525**, early stop); coef 1.014 |
| `default_xgb_1_08` | mtry 4, trees 789, depth 10, lr 0.0015234, loss_red 6.007, stop_iter 5 (niter 789); coef 1.076 |
| `default_xgb_1_10` | mtry 5, trees 564, depth 1, lr 0.0036100, loss_red 3.091e-8, stop_iter 13 (niter 564); coef 1.283 |

intercept −1.573. Again **no RF, no GAM**.

Blend-selection curves (`s$metrics`): TDM `tss_max` 0.6979-0.6987 across penalties, TBM 0.5290-0.5370.
Published test-set values (`tss_tdm.csv`, `tss_score_tbm.csv`): TDM 0.70097, TBM 0.55559 — both confirmed.

### Verification that these are the right layers, not just the right names

`i04_pointmatch.R` re-extracts each candidate raster at the **16,596 / 21,389 stored training
geometries** and compares with the stored column:

| stored column | candidate | max abs diff | exact (<1e-6) |
|---|---|---|---|
| TDM `aspect/elevation` | `terrain_features/*.tif` resampled | 0 | 16596/16596 |
| TDM `roughness/slope/TPI/TRI` | same | ≤9.5e-7 (float32) | 16596/16596 |
| TDM `snow` | `fitted_2021.tiff` **as published** | **0** | **16596/16596** |
| TDM `snow` | `fitted_2021.tiff` re-registered (§6) | 53.63 | 120/16596 |
| TDM `snow` | `fitted_2012.tiff` | 80.66 | 1/16596 |
| TDM `dist` | **distance from ALL 2012 Sasa pixels** | **0** | **16596/16596** |
| TDM `dist` | distance from 2012 polygons > 5 m² | 234.98 (mean 13.88) | 5577/16596 |
| TDM `dist` | distance from 2021 polygons > 5 m² | 164.55 | 1158/16596 |
| TDM `dist` | distance from `selected_comms.tiff` | 611.27 (mean 68.90) | 788/16596 |
| TBM all 7 | as above | 0 / float32 noise | 21389/21389 |

`min(dist) = 1 m`, `sum(dist == 0) = 0` → `filter(dist > 0)` confirmed on the object itself.

---

## 5. The distance predictor across all script versions — bug fix, not doc fix

Three different distance definitions exist in the corpus:

| definition | built where | used where |
|---|---|---|
| **A** distance from every 2012 Sasa **pixel** | `sdm_include_distance.R:44-47` | **training** (L56) |
| **B** distance from 2012 / 2021 Sasa **polygons > 5 m²** | `sdm_include_distance.R:247-255` | **prediction** — 2021 map uses the 2012 polygons (L257), 2030 map uses the 2021 polygons (L269) |
| **C** distance from `selected_comms.tiff` (2012 communities ≥ 5 m², `select_sasa_communities.R`) | `sdm_sasainc.R:40-43`, `analyse_chamges.R`, `analyse_sdm.R:15-18` | the Sasa-increase side analyses only |

Verified against the archived objects: training = **A** (exact, table above); the 2021 published map
= **B with the 2012 polygons** (repro `maxabs 4.6e-8`); the 2030 published map = **B with the 2021
polygons** (repro `maxabs 4.5e-8`). Definition **C** matches nothing in the published models.

**No script version is self-consistent.** `sdm.R` has no distance at all. `sdm_include_distance.R`
contains A and B in one linear file, ~200 lines apart, and a clean top-to-bottom run of it
reproduces the mismatch exactly. **This is therefore not an artefact of interactive re-running or
a stale workspace** — it is the code as committed.

**Which way was it meant to go?** Lines 36-37 of `sdm_include_distance.R`:

```r
sasa12_ras %>% terra::as.polygons() %>%
  filter()
```

an unassigned, no-op expression sitting immediately above the training-distance block — an
abandoned attempt to build the *training* distance from polygons. The > 5 m² threshold is the
author's standing definition of a real *Sasa* community (`select_sasa_communities.R:17`,
`area_2012 >= units::set_units(5, m^2)`), used again for the black outlines in every published
HS figure. The intent was polygon-based throughout; the training branch was never updated.

⇒ **Report this as a bug fix.** But note the two possible repairs move the headline number in the
same (unhelpful) direction, by different amounts — see §6, F-4.

Separate, and also undocumented: the 2021 map is projected with the **2012** distance layer and
the 2030 map with the **2021** distance layer. That is defensible as "propagate the source
population forward", but it is nowhere stated in the manuscript, and it is what F-3 is about.

---

## 6. Verification of F-3, F-4, F-5

All areas are `terra::expanse()` geodesic areas on the archived / regenerated rasters
(`v01_areas.R`, `v02_f5.R`, `v04_heldout.R`, `v05_refit.R`, `v07_refiteval.R`).

Baseline reproduced first, straight off the archived rasters — **all five published numbers are exact**:

```
ARCHIVED TDM  2021 = 12766.04   newly2030 = 4386.61   lost2030 =  716.61   (ms 12766 / 4387 / 717)
ARCHIVED TBM  2021 = 47253.26   newly2030 = 27049.27  lost2030 = 2257.77   (ms 47253 / 27049 / 2257)
```

### F-3 — **CONFIRMED, to the decimal**

2 × 2 of snowmelt vintage × distance layer, archived TDM stack, polygon distances:

| variant | suitable (m²) | newly suitable vs archived 2021 |
|---|---|---|
| snow 2021 × dist 2012 (= published 2021 map) | 12,766.04 | — |
| snow 2030 × dist 2012 (**snowmelt only**) | 12,190.35 | **533.71 (12.2 %)** |
| snow 2021 × dist 2021 (**distance only**) | 14,888.88 | **3,937.85 (89.8 %)** |
| snow 2030 × dist 2021 (= published 2030 map) | 14,474.11 | 4,386.61 (100 %) |

Exactly the audit's 533.7 / 12.2 % and 3,937.9 / 89.8 %. And holding the distance layer at 2012
**shrinks** total suitable area 12,766 → 12,190 m²: under the model, the 2030 snowmelt on its own
makes the landscape *less* suitable, and the entire projected expansion is the distance layer
being swapped. The manuscript's causal sentence
(`results.qmd:95`, "These reductions suggest shifts … due to earlier snowmelt timing") is not
supported by the model that produced the numbers.

### F-4 — **CONFIRMED, to the decimal**, and a second repair added

Making prediction use the *training* definition (distance from all 2012 pixels):

```
as published  (dist = 2012 polygons > 5 m2)  2021 suitable = 12766.04 m2
training-consistent (dist = ALL 2012 pixels) 2021 suitable = 18103.13 m2   (+41.8 %)
observed 2021 Sasa                                          10170     m2
```

I also ran the *other* repair — the one the dead code at L36-37 implies was intended: refit the
seven surviving stack members (hyperparameters and blend coefficients held fixed) on a training
frame whose `dist` is the polygon > 5 m² distance, then predict with the same definition
(`v05_refit.R`; the manual linear predictor was first verified against
`stacks:::predict.model_stack`, `maxabs 2.2e-16`):

```
TDM, dist = 2012 polygons > 5 m2 in BOTH training and prediction
  2021 suitable        = 14927.86 m2   (published 12766.04)
  newly suitable 2030  =  5029.26 m2   (published  4386.61)
  lost by 2030         =   605.67 m2   (published   716.61)
```

**Under both self-consistent definitions the TDM's 2021 suitable area is larger than published**
— 14,928 m² or 18,103 m² against the observed 10,170 m². The published 12,766 m², the number
`results.qmd:67` calls "much closer to observations", is the *smallest* of the three and it is the
one produced by the inconsistency. This affects the manuscript.

### F-5 — **CONFIRMED**; the counter-claim is only partly supported

On the domain where both HS maps are defined (390,065 cells):

```
TDM 2021 suitable cells                                = 12630
  of which dist(2012 polygons > 5 m2) == 0             =  6675  (52.9 %)
  of which dist(ALL 2012 pixels)     == 0              =  6754  (53.5 %)
mean HS on dist_pol12 == 0 : 0.5975      elsewhere : 0.1123
mean HS on dist_all12 == 0 : 0.5292      elsewhere : 0.1120
TDM suitable area OUTSIDE the 2012 polygon footprint  = 6094.68 m2
TDM suitable area OUTSIDE the 2012 all-pixel footprint = 6015.72 m2
TDM suitable area TOTAL                                = 12766.04 m2
```

6,675 cells and 0.5975 / 0.1123 reproduce the audit exactly; the audit's 52.3 % used the full
12,773-cell count (before intersecting with the TBM raster's coverage), mine is 52.9 % of 12,630 —
same cells, different denominator. "~6,098 m² outside the 2012 footprint" reproduces as 6,094.68 m².

So roughly **half of the TDM's suitable area is the 2012 *Sasa* footprint being handed back**,
on pixels `sdm_tdm.R:69` removed from training.

**Counter-claim** ("on a common held-out domain the conclusion survives and strengthens,
TDM 0.698 vs TBM 0.454"). I could not reconstruct the original test split (the absence thinning is
unseeded), so I evaluated both archived HS maps cell-wise against the observed 2021 map, on cells
used in **neither** model's training frame (34,046 training cells removed):

| domain | n | prevalence | TSS<sub>max</sub> TDM | TBM | AUC TDM | TBM |
|---|---|---|---|---|---|---|
| all cells | 390,065 | 0.0256 | 0.7860 | 0.6212 | 0.9564 | 0.8888 |
| held out from both models | 356,019 | 0.0060 | 0.7458 | 0.5422 | 0.9334 | 0.8254 |
| held out **and** outside the 2012 footprint | 352,492 | 0.0024 | **0.5736** | **0.4381** | 0.8526 | 0.7675 |
| held out **and** inside the 2012 footprint | 3,527 | 0.3675 | 0.4244 | 0.1312 | 0.7371 | 0.5361 |

**Survives: yes. Strengthens: no.** The TDM beats the TBM on every domain, including the strictly
non-circular one — that part of the counter-claim holds and is worth putting in the rebuttal. But
the TDM's own TSS *falls* from 0.746 to 0.574 once the 2012 footprint is removed, and the
TDM−TBM gap goes 0.204 → 0.136, i.e. back to about the published gap (0.701 − 0.556 = 0.145), not
wider. Do not write "strengthens".

The same evaluation for the two repaired TDMs (`v07_refiteval.R`) — the discrimination claim is
robust to the distance repair, only the *area* claim is not:

| domain | published TDM | refit, polygon dist | predict with all-pixel dist | TBM |
|---|---|---|---|---|
| all cells | 0.7860 | 0.8021 | 0.8408 | 0.6212 |
| held out from both | 0.7458 | 0.7244 | 0.7958 | 0.5422 |
| held out & outside 2012 footprint | 0.5736 | 0.5629 | 0.6608 | 0.4381 |

---

## 7. The half-pixel snowmelt misregistration — material or cosmetic?

**Direction of the fix, determined empirically** (`v03_reg.R`). `fitted_*.tiff` has origin
(0.5, −0.25); `vege_2012_5x5.tiff` has (0, 0.25). Candidate half-pixel shifts scored against the
raw 2012 snowmelt raster on the vegetation grid (nearest-neighbour, no smoothing):

```
shift( +0.0,+0.0)  origin=(0.50,-0.25)  n=1185568  cor=0.972047  sd(diff)=6.9275   <- as published
shift( +0.5,-0.5)  origin=(0.00, 0.25)  n=1206064  cor=0.977567  sd(diff)=6.2305   <- best, and n
shift( -0.5,+0.5)  origin=(0.00, 0.25)  n=1185568  cor=0.972047  sd(diff)=6.9275      matches the
shift( +0.5,+0.5)  origin=(0.00, 0.25)  n=1191890  cor=0.970378  sd(diff)=7.1410      1,206,063
shift( -0.5,-0.5)  origin=(0.00, 0.25)  n=1190467  cor=0.977118  sd(diff)=6.2740      snowmelt-pixel
shift( +0.0,-1.0)  origin=(0.50,-0.25)  n=1190467  cor=0.977118  sd(diff)=6.2740      count
```

`terra::shift(dx = +0.5, dy = −0.5)` is the correction (equivalently: use plain `as_tibble()`
instead of `as_tibble(add_max = TRUE)` at `preprocess_snow_data.R:103`).

**Effect on the predictor** (after the script's own `resample()`, bilinear):

| layer | mean Δ | sd Δ | max abs | % of cells > 1 DOY |
|---|---|---|---|---|
| fitted_2012 | +0.058 | 1.843 | 68.9 | 30.8 % |
| fitted_2021 | +0.045 | 2.162 | 64.2 | 33.2 % |
| fitted_2030 | +0.045 | 3.639 | 145.0 | 49.8 % |

**Effect on model output.** Rebuilt `fitted_2021/2030` on the correct grid and re-predicted with
both archived stacks (`review/restore/out/reg_pub_*.tiff`, `reg_fix_*.tiff`):

| | 2021 suitable | newly suitable 2030 | lost by 2030 |
|---|---|---|---|
| TDM, snow as published | 12,766.04 | 4,386.61 | 716.61 |
| TDM, snow re-registered | **12,844.99** (+0.62 %) | **4,257.68** (−2.94 %) | **769.58** (+7.39 %) |
| TBM, snow as published | 47,253.26 | 27,049.27 | 2,257.77 |
| TBM, snow re-registered | **47,281.25** (+0.06 %) | **27,026.28** (−0.08 %) | **2,272.76** (+0.66 %) |

Per-cell HS change:

```
tdm21  mean +0.00006  sd 0.00476  max|d| 0.253  cells crossing 0.5 =   321 (0.08 %)  cor 0.99880
tdm30  mean -0.00016  sd 0.00595  max|d| 0.281  cells crossing 0.5 =   509 (0.13 %)  cor 0.99830
tbm21  mean -0.00009  sd 0.02246  max|d| 0.489  cells crossing 0.5 =  4543 (1.17 %)  cor 0.98705
tbm30  mean -0.00089  sd 0.03246  max|d| 0.483  cells crossing 0.5 = 10248 (2.57 %)  cor 0.97593
```

**Verdict: cosmetic for the five published headline areas** — every one moves by less than 1 %
except the TDM's "lost by 2030", which moves 717 → 770 m² (+7.4 %) because it is a small number
sitting on a threshold. It is *not* cosmetic for the TBM's per-cell map, where 1.2-2.6 % of cells
cross the 0.5 threshold.

**Caveat that must not be dropped.** The models were **trained** on the misregistered layer
(§4 table: stored `snow` matches `fitted_2021.tiff` as published to `maxabs 0`, and matches the
re-registered layer only at 120 of 16,596 points). The numbers above therefore apply a model to a
predictor it was not fitted on. A complete fix regenerates `fitted_*`, refits, and re-predicts;
this run bounds the size of the effect and shows it is small, which is the honest thing to say
in the response letter — but "the fix is cosmetic" should be stated about the *areas*, not about
the pipeline.

---

## 8. xgboost version pin

Tested with `xgbtest.R` on both stacks:

| xgboost | `predict(model_stack, …)` |
|---|---|
| **3.2.1.1** (current CRAN, the default library here) | **FAILS** — `In index: 4. With name: default_xgb_1_02. Caused by error in xgb.get.handle(): 'xgb.Booster' object is corrupted or is from an incompatible XGBoost version.` |
| **1.7.6.1** | **works** — TDM `.pred_presence` = 0.093859, 0.093930, 0.099919, 0.090976, 0.087651 |
| **1.7.7.1** | **works** — bit-identical to 1.7.6.1 on those rows |

**Pin: `xgboost 1.7.7.1`** (last release of the 1.7 series; 1.7.6.1 also works and was used for
every number in this report). Both emit a harmless `learner.cc:553` "loading a serialized model"
warning on each member. Installed from source into a scratch library:

```r
install.packages("https://cran.r-project.org/src/contrib/Archive/xgboost/xgboost_1.7.7.1.tar.gz",
                 repos = NULL, type = "source", lib = "<scratch>")
.libPaths(c("<scratch>", .libPaths()))
```

**Recommendation for the Zenodo deposit — do not ship a dependency on an EOL xgboost.** The
member boosters can be re-exported to the version-stable JSON model format from inside 1.7.x, and
they round-trip exactly (`xgbexport.R`):

```
model_stack__default_xgb_1_02.json   maxabs(reload - original) = 0.000e+00   (1,021,395 B)
model_stack__default_xgb_1_04.json   0.000e+00  (18,505,161 B)
model_stack__default_xgb_1_08.json   0.000e+00  (24,026,643 B)
model_stack__default_xgb_1_10.json   0.000e+00  (   304,987 B)
model_stack_wo_dist__default_xgb_1_05.json  0.000e+00  (72,816 B)
model_stack_wo_dist__default_xgb_1_09.json  0.000e+00  (631,359 B)
```

Deposit the `.rds` **plus** these six `.json` boosters, the MaxEnt member specs, and the blend
coefficients (§4) — then the ensemble is reconstructible without any xgboost 1.7 at all. The
blend is a plain logistic linear predictor and I verified it reproduces `predict.model_stack`
to 2.2e-16:

```
P(presence) = 1 − 1/(1 + exp(−η)),   η = β0 + Σ βi · P_i(absence)
TDM: β0 = −1.5732;  maxent_1_02 0.6810, maxent_1_14 0.03736, maxent_1_03 0.01527,
     xgb_1_02 0.1530, xgb_1_04 1.0139, xgb_1_08 1.0764, xgb_1_10 1.2831
TBM: β0 = −1.3134;  maxent_1_17 0.1783, maxent_1_12 0.5323, xgb_1_05 1.9583, xgb_1_09 0.3110
```

---

## 9. What a clean re-run cannot reproduce, and why (for the response letter)

`i03_prep.R` re-ran the full preprocessing five ways. Row counts against the archived frames:

| case | df rows | presences | training rows | training presences | archived training |
|---|---|---|---|---|---|
| TBM, no twi | 27,058 | 9,974 | 21,426 | 7,837 | **21,389 / 7,820** |
| TBM, with twi | 27,052 | 9,974 | 21,397 | 7,815 | 21,389 / 7,820 |
| TDM, dist = all 2012 px | 20,897 | 3,973 | 16,552 | 3,111 | **16,596 / 3,123** |
| TDM, dist = 2012 pol > 5 m² | 21,349 | 4,375 | 16,930 | 3,443 | 16,596 / 3,123 |

The all-pixel TDM case lands within 0.3 % of the archived frame; the polygon case is 2 % off in
rows and **10 % off in presences** — a second, independent confirmation that training used
definition **A**. The residual 0.3 % is `thin_by_cell()`: `set.seed(1)` is called *after* the
thinning (`sdm_tdm.R:61-71`, `sdm_tbm.R:52-61`), so the absence sample and hence the block split
are not reproducible. **Two-line fix**: move `set.seed()` above the `bind_rows()` block.

---

## 10. Recommended changes, in priority order

1. **Distance definition (bug).** Pick one definition and use it in training *and* prediction.
   State in Methods which, and state that the 2021 projection uses the 2012 source layer and the
   2030 projection the 2021 source layer. Report the corrected 2021 suitable area
   (14,928 m² polygon-consistent, or 18,103 m² pixel-consistent) and **rewrite `results.qmd:67`
   and the `fig-hs-maps` caption**: the TDM no longer "closely matches" 10,170 m². The
   discrimination claim (TDM ≫ TBM) is unaffected — say that instead.
2. **F-3 attribution.** Report the decomposition (12.2 % snowmelt / 89.8 % distance layer, and
   that snowmelt alone *reduces* suitable area 12,766 → 12,190 m²). Remove or heavily qualify the
   "due to earlier snowmelt timing" sentence in the `fig-future-prediction` caption.
3. **F-5 circularity.** Add the "outside the 2012 footprint" evaluation (TDM 0.574 vs TBM 0.438)
   as the honest test, and state that ~52 % of the TDM's suitable area is the 2012 footprint.
4. **Algorithms.** Methods says four algorithms; the published ensembles contain **only MaxEnt and
   XGBoost** members. Either say "four were tuned, LASSO blending retained MaxEnt and GBT", or
   list what is in the objects. Give the member table from §4.
5. **Restore `sdm_tbm.R`** to the four-algorithm form and the `models_wo_dist.rds` filename (§2),
   with a comment that `models_wo_dist_twi.rds` was an abandoned TWI test.
6. **Replace both terrain globs with an explicit file vector** (§3), add the CRS relabel, move
   `set.seed()` above `bind_rows()`, and fix `preprocess_snow_data.R:103`
   (`as_tibble(add_max = TRUE)` → `as_tibble()`), regenerating `fitted_*`.
   Report the registration effect as bounded and small (§7) rather than silently fixing it.
7. **Deposit hygiene.** Remove `twi.tif` from the repository (or add it to the models and describe
   it). Do not deposit `models_all_5m.rds`, `tss_score_tbm.rds`, `sasa_pred_sdm_dist_*.tiff` or
   `sasa_pred_sdm_12.tiff` as if they were the published models/maps — label them exploratory or
   drop them. Ship the six JSON boosters plus the blend coefficients alongside the `.rds`.
