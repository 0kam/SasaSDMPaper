# Adversarial verification — `sdm-models` subsystem

Verifier: independent re-run of every claim. Nothing below is quoted from the original
audit; every number was recomputed in this session. Scripts used live in
`/private/tmp/claude-501/.../scratchpad/vfy2/` (`v01`–`v24`).

Environment: R 4.5.2, terra 1.9.34, tidysdm 1.0.4, stacks 1.1.1, DALEX 2.5.4,
system xgboost 3.2.1.1. A scratch library with **xgboost 1.7.11.1**
(`R_LIBS=<scratchpad>/Rlib`) was needed for anything that touches the archived
ensembles — see finding 3.

---

## Summary of verdicts

| id | verdict |
|---|---|
| tdm-2030-distance-drives-projection | CONFIRMED |
| tdm-dist-train-pred-mismatch | CONFIRMED |
| xgboost-boosters-unreadable | CONFIRMED |
| scripts-abort-crs-mismatch | CONFIRMED |
| tbm-script-drift | CONFIRMED |
| twi-silent-extra-predictor | CONFIRMED |
| vi-different-splits | CONFIRMED |
| vi-unseeded-not-reproducible | CONFIRMED |
| **thin-by-cell-unseeded** | **PARTLY_CONFIRMED — the TSS claim is refuted** |
| collinearity-tri-roughness-slope | CONFIRMED |
| aspect-circular-treated-linear | CONFIRMED |
| different-response-domains | CONFIRMED |
| vi-explains-hard-classes | CONFIRMED |
| presense-typo-is-noop (retraction) | CONFIRMED — retraction is correct |
| **unsuitable-percentage-denominator** | **PARTLY_CONFIRMED — the TDM 7% does reproduce** |
| fig8-shows-2021-maps | CONFIRMED |
| rf-gam-zero-weight | CONFIRMED |
| prediction-domain-mismatch | CONFIRMED |
| orphan-artefacts | CONFIRMED (list incomplete) |
| elevation-2560-claim | CONFIRMED |
| undocumented-block-geometry-and-seeds | CONFIRMED |
| dist-extrapolation | CONFIRMED (but misses the much larger low-end extrapolation) |
| terrain-features-no-code | CONFIRMED |

All five retracted leads were re-checked; all five retractions are correct.

---

## Reproduction of the archived artefacts (baseline)

With the CRS patch (`crs(terrain) <- crs(vege12)`) and `twi.tif` excluded, the archived
rasters regenerate bit-for-bit from `model_stack.rds` under xgboost 1.7.11.1:

```
repro check 2021: maxabsdiff = 5.960464e-08     (data/sasa_pred_tdm_21.tiff)
repro check 2030: maxabsdiff = 5.960464e-08     (data/sasa_pred_tdm_30.tiff)
```

Every published area statistic recomputes exactly from the archived rasters:

```
TDM suit21: 12766.04   suit30: 14474.11   newly: 4386.61   unsuitable: 716.61
TBM suit21: 47253.26   suit30: 69176.33   newly: 27049.27  unsuitable: 2257.77
valid px  2021: 397403        2030: 407658
```

Archived training frames:

```
model_stack.rds          n=16596  presence=3123  absence=13473  prev=0.1882  preds: aspect roughness slope elevation TPI TRI snow dist
model_stack_wo_dist.rds  n=21389  presence=7820  absence=13569  prev=0.3656  preds: aspect roughness slope elevation TPI TRI snow
CV assessment sizes  TDM: 3286 5217 3054 5039   TBM: 4107 7277 3490 6515
```

`set.seed(1); spatial_block_cv(<archived train>, v=4)` reproduces **both** of those fold
vectors exactly, so the spatialsample default 10×10 block grid is confirmed.
TDM train bbox 732744.5 4050651 733916.5 4051796 → blocks 117.2 m × 114.5 m.

---

## Finding-by-finding

### 1. tdm-2030-distance-drives-projection — CONFIRMED

Ran the archived TDM stack over five environmental stacks:

```
tag       suitable(m2)  validpx
s21_V2      12766.04     397403     published 2021 (snow21 + dist-to-2012-pol>5m2)
s30_V3      14474.11     407658     published 2030 (snow30 + dist-to-2021-pol>5m2)
s30_V2      12190.35     407658     snow advances, distance held at 2012
s21_V3      14888.88     397403     distance updated, snow held at 2021
s21_V1      18103.13     397403     snow21 + the TRAINING distance layer

newly suitable, published (s30_V3 vs s21_V2):  4386.61 m2
newly suitable, snow only  (s30_V2 vs s21_V2):   533.71 m2   (12.2%)
newly suitable, dist only  (s21_V3 vs s21_V2):  3937.85 m2   (89.8%)
```

Holding distance fixed the total suitable area **shrinks** (12766 → 12190 m²). Every
number in the original finding reproduces to 2 dp.

Manuscript side confirmed by reading the source:
- `paper/matmet.qmd:137` — "we predicted *Sasa* distribution for 2030 based on the
  estimated snowmelt DOYs". No mention of rebuilding the distance layer.
- `paper/discussion.qmd:7` — "predictions for 2030, assuming earlier snowmelt than in
  2021, suggested continued *Sasa* expansion".
- Abstract — "Scenario-based projections under continued snowmelt advance".
- `paper/files/overview.jpg` (Fig. 1): the only extra arrow into "Prediction of Sasa
  invasion" is labelled "Future prediction of snowmelt DOY".

### 2. tdm-dist-train-pred-mismatch — CONFIRMED

```
elev<2560 domain, n = 1,296,195
frac V1 != V2 : 0.8525   mean|d| 18.58 m   max|d| 238.52 m
meanV1 163.73   meanV2 182.31
(V2-V1) quantiles  25% 4.45 | 50% 13.29 | 75% 27.80 | 95% 55.22 | 99% 96.41
2012 polygons: 1171 (8547 m2)  ->  >5 m2: 196 (7074 m2 planar; 975 dropped)
2021 polygons: 1440 (10176 m2) ->  >5 m2: 231 (8333 m2 planar / 8328.49 m2 expanse)
TDM 2021 with V1 (train-matched): 18103.13 m2 vs published 12766.04 m2  (+41.8%)
threshold flips: 5340 / 397403 = 1.344%;  mean|dHS| 0.01698, max 0.5272
```

Only nit: the source area after the filter is 7,074 m² planar (the original said
7,070 — that is `expanse()`, i.e. terra's ellipsoidal area). Immaterial.

### 3. xgboost-boosters-unreadable — CONFIRMED

Under system xgboost 3.2.1.1:

```
predict(readRDS("model_stack.rds"), X[1:5,], type="prob")
Error in purrr::map(...): In index: 4. With name: default_xgb_1_02.
Caused by error in `xgb.get.handle()`:
! 'xgb.Booster' object is corrupted or is from an incompatible XGBoost version.
```

With `R_LIBS=<scratch>` (xgboost 1.7.11.1) the same call succeeds (emitting the
"loading a serialized model … generated by older XGBoost" warning per booster). Every
result in this report that touches an ensemble required that workaround.

### 4. scripts-abort-crs-mismatch — CONFIRMED

```
data/terrain_features/*.tif  crs = 3099 (JGD2000 / UTM 53N), res 4.97 x 6.16
vege_2012_5x5.tiff           crs = 6690 (JGD2011 / UTM 53N), res 1 x 1
after resample(vege12)       crs = 3099        <- no warning emitted
sampling_mask crs: 3099
df_21_1 crs: 6690
thin_by_cell output crs: 3099   (tidysdm:::thin_by_cell does st_transform(terra::crs(raster)))
bind_rows(...)  -> Error : CRS mismatch: PROJCRS["JGD2000 / UTM zone 53N", ...] vs JGD2011 / UTM zone 53N
```

Archived `model_stack$train$geometry` is EPSG:6690, so the historical run did not hit
this; the committed code does. Every archived prediction raster carries EPSG:3099.
(The datum shift itself is numerically nil here — a point at 733300/4051200 maps to
itself to within 0 m — so the misalignment is a class error, not a positional one.)

### 5. tbm-script-drift — CONFIRMED

`sdm_tbm.R:97-112` has only `rf = sdm_spec_rf()` active; GAM/MaxEnt/XGB and the
`update_workflow_model("default_gam", …)` call are commented out (`:101-112`, `:117-119`).
`:127` saves and `:129` reads `models_wo_dist_twi.rds`, which does not exist
(`ls ortho/*.rds` → model_stack.rds, model_stack_wo_dist.rds, models.rds,
models_all_5m.rds, models_wo_dist.rds, tss_score_tbm.rds). `models_wo_dist.rds` has
`wflow_id = default_rf default_gam default_maxent default_xgb`; no committed line writes
it. `:132` still labels four series. Candidate counts in `models_wo_dist.rds`
(rf 7, gam 1, maxent 18, xgb 18) match `model_stack_wo_dist.rds`, so that is indeed the
parent object.

### 6. twi-silent-extra-predictor — CONFIRMED

`list.files("data/terrain_features/") |> str_subset(".tif$")` returns
aspect, roughness, slope, tateyamadem_small, TPI, TRI, **twi**. Neither fitted stack
contains `twi`; `grep -rn twi paper/*.qmd` returns nothing. `.gitignore` excludes
`*.tiff` but not `*.tif`, and `git ls-files` confirms `ortho/data/terrain_features/twi.tif`
plus `ortho/data/snow/snow_{mean,sd,reg}.tif` are tracked and public.
`preprocess_snow_data.R` writes `data/terrain_features/snow_mean.tif`,
`snow_sd.tif`, `snow_reg.tif` into the globbed directory (confirmed at the three
`write_stars` calls).

### 7. vi-different-splits — CONFIRMED

Recomputed on the archived stacks, three seeds each:

```
TBM, TEST  (as sdm_tbm.R):  snow .179-.193  elevation .129-.149  TRI .048-.066
                            aspect .025-.038  roughness .031-.036  slope .006-.012  TPI ~0
TBM, TRAIN:                 snow .390-.398  elevation .191-.210  aspect .070-.081
                            TRI .040-.051  roughness .030-.038  slope .008-.016  TPI .003-.009
TDM, TRAIN (as sdm_tdm.R):  dist .4497  snow .1973  elevation .0486  TRI .0150
                            aspect .0142  slope .0090  roughness .0075  TPI .0072
TDM, TEST:                  dist .3513  snow .1183  elevation .0115  roughness .0082
                            TPI .0066  aspect .0045  slope .0031  TRI .0023
```

`paper/files/vi.jpg` shows TBM snow ≈ 0.195 / elevation ≈ 0.14 and TDM dist ≈ 0.46 /
snow ≈ 0.21. Only the TBM-**test** + TDM-**train** combination matches. TBM-train
importances are ~2× TBM-test, so the two panels are on incomparable scales.

### 8. vi-unseeded-not-reproducible — CONFIRMED

`formals(DALEX::model_parts)$N` is `n_sample`, whose default is `1000`;
`attributes(fi)$B == 10`. Neither script seeds. Three seeds on the same TBM/test
configuration give TRI 0.048/0.066/0.056 and aspect 0.038/0.025/0.032 — the order of
TRI/aspect/roughness is not stable. Compared the two archived images directly:

- `ortho/figures/vi_tbm.png`: order snow, elevation, TRI, **roughness, aspect**, slope,
  TPI; y-axis 0 – 0.6 (the script's `ylim(c(0,0.6))`).
- `paper/files/vi_tbm.jpg` (left half of `vi.jpg`): order snow, elevation, TRI,
  **aspect, roughness**, slope, TPI; free y-axis topping out ~0.25 with the TPI box
  dipping below zero — which `ylim(c(0,0.6))` would have clipped.

Two different runs of two different versions of the script.

### 9. thin-by-cell-unseeded — **PARTLY_CONFIRMED**

The mechanism is confirmed: `tidysdm:::thin_by_cell` contains
`data <- data[sample(seq_len(nrow(data))), ]` and neither script seeds before it;
`set.seed(1)` at `sdm_tdm.R:71` comes *after* the thinning, so the split is
irrecoverable. Reproduced `df_21` sizes vary run to run because which point survives in
each 5×5 cell changes its elevation and hence the `elevation < 2560` filter:

```
TDM df_21 20,876 - 20,923 (3,973 presence)  train 16,540 - 16,606   [archive train 16,596]
TBM df_21 27,041 - 27,062 (9,974 presence)  train 21,392 - 21,436   [archive train 21,389]
archive TDM train presences found in reproduced presence pool: 3123 / 3123
archive TBM train presences found in reproduced presence pool: 7820 / 7820
```

**The TSS claim is REFUTED.** Five *independent* absence draws (re-seeded before each
thinning — note that re-running the block without re-seeding reproduces the previous
draw, because `set.seed(1)` inside `spatial_initial_split` resets the stream, which is
probably how the original audit got an artificially narrow band):

```
TDM tss_max: 0.6984 0.7014 0.7074 0.7020 0.6989   range 0.6984-0.7074   published 0.70097
TBM tss_max: 0.5551 0.5517 0.5529 0.5526 0.5477   range 0.5477-0.5551   published 0.55559
```

The published TDM 0.70 sits in the middle of the reconstructed range, not above it. It
should **not** be restated as ~0.68. The published TBM 0.5556 is 0.0005 above my highest
draw — effectively reproduced.

### 10. collinearity-tri-roughness-slope — CONFIRMED (exact)

```
TDM VIF: TRI 25.545  roughness 16.194  slope 11.200  snow 1.371  TPI 1.256  elevation 1.093  aspect 1.085  dist 1.028
TBM VIF: TRI 25.445  roughness 17.603  slope 10.650  snow 1.493  TPI 1.209  aspect 1.101  elevation 1.082
Pearson (TDM): roughness-TRI 0.970, slope-TRI 0.943, roughness-slope 0.932,
               elevation-dist 0.410, slope-snow -0.365, TRI-snow -0.359, roughness-snow -0.355, TPI-snow -0.334
```

`models_all_5m.rds` predictors: slope, snow, dist, elevation, aspect, TPI — a
6-predictor de-collinearised variant, confirmed.

### 11. aspect-circular-treated-linear — CONFIRMED

Sweeping aspect 0→360 with all other predictors at their training median:

```
TBM: HS(0)=0.4966  HS(359)=0.5505  HS(360)=0.5505  seam = 0.0538
     range over aspect 0.4966-0.6330 (span 0.1364) -> seam is 39.5% of the aspect effect,
     and it straddles the 0.5 cut-off.
TDM: seam = 0.0028, span 0.0040 (swamped by dist)
```

maxnet betas of `default_maxent_1_02`:
`hinge(aspect):351.909:359.224 = +0.1797`, `hinge(aspect):242.187:359.224 = -0.1466`.
MaxEnt + XGBoost carry 100% of both ensembles.

### 12. different-response-domains — CONFIRMED

Prevalence 0.3656 (TBM) vs 0.1882 (TDM) in the archived frames; presences 9,974 → 3,973
in the reproduced pools. Common held-out set (`dist > 0`, unseen by *both* archived
models), n = 4,304 with 839 presences:

```
TDM: boyce 0.9477  roc_auc 0.9139  tss_max 0.6981
TBM: boyce 0.9865  roc_auc 0.7816  tss_max 0.4535
```

Same conclusion as the original audit (0.70 vs 0.45); the small numeric differences are
the stochastic absence draw.

### 13. vi-explains-hard-classes — CONFIRMED

`explainer$y_hat` is a `tbl_df` whose only column is `.pred_class` (factor
`absence`/`presence`). Consequently the model_parts `_full_model_` loss is TSS at the
implicit 0.5 cut-off: **0.5218** in my TDM/train run (seed 11), versus `tss_tdm.csv`
`tss_max = 0.7010`. Two different quantities under one label.

### 14. presense-typo-is-noop (retraction) — CORRECT

`DALEXtra:::yhat.model_stack` reads `attr(X.model,"predict_function_target_column")`
only *after* calling `predict(X.model, newdata, type="prob")`, and is bypassed entirely
when `predict_function` is supplied. Empirically, on the real TDM stack:

```
y_hat identical  "presense" vs "presence": TRUE     vs argument omitted: TRUE
model_parts dropout losses identical (seed 42): TRUE / TRUE
without predict_function: y_hat is numeric -> 0.0939 0.0939 0.0999 ...
```

### 15. unsuitable-percentage-denominator — **PARTLY_CONFIRMED**

The numerator/denominator mismatch is real: the numerators are computed over
`sasa_pol_21` (2021 polygons with area > 5 m²), whose total area is **8,328.46 m²**
(`expanse`) / 8,333 m² (planar `st_area`), while the manuscript's Sasa area is 10,170 m².

```
TDM 716.61 m2 -> /8328.46 = 8.60%   /10170 = 7.05%
TBM 2257.77 m2 -> /8328.46 = 27.11%  /10170 = 22.20%
Manuscript: "TBM predicted 2,257 m2 (21%) ... TDM predicted 717 m2 (7%)"
```

Correction to the original finding: the published **7% reproduces exactly** against the
10,170 m² denominator (7.05%). Only the TBM's **21%** matches nothing — 22.2% is what
the 10,170 denominator gives and 27.1% is what the correct (filtered) denominator gives.
So this is one wrong percentage plus one systematically-mismatched denominator, not two
unexplained numbers. (For contrast, the *newly suitable* percentages are correct:
27,049/47,253 = 57.2%, 4,387/12,766 = 34.4%.)

### 16. fig8-shows-2021-maps — CONFIRMED

`paper/results.qmd`, chunk `fig-future-prediction`:
`hsmap <- c(image_read("files/hsmap_tbm_2021.jpg"), image_read("files/hsmap_tdm_2021.jpg"))`.
`paper/files/hsmap_tbm_2030.jpg` and `hsmap_tdm_2030.jpg` exist and are referenced by no
chunk. The caption says "Habitat suitability (HS) maps for 2030 and differences from the
2021 HS maps".

### 17. rf-gam-zero-weight — CONFIRMED (exact)

```
model_stack.rds          (penalty 0.1, mixture 1)
  maxent_1_02 0.6808  maxent_1_14 0.0374  maxent_1_03 0.0153
  xgb_1_02 0.1527  xgb_1_04 1.0110  xgb_1_08 1.0813  xgb_1_10 1.2821
  -> maxent 17.21%   xgb 82.79%
model_stack_wo_dist.rds
  maxent_1_17 0.1775  maxent_1_12 0.5317  xgb_1_05 1.9613  xgb_1_09 0.3105
  -> maxent 23.79%   xgb 76.21%
non-zero candidates: rf 0/8 and gam 0/1 (TDM); rf 0/7 and gam 0/1 (TBM)
```

### 18. prediction-domain-mismatch — CONFIRMED

```
data/snow/fitted_2021.tiff  dim 1703x1801 (50 rows short)   fitted_2012/2030 1753x1801
after resample to the vege grid: snow2021 valid 1,178,227 ; snow2030 valid 1,206,076
2030-only 27,849 ; 2021-only 0
prediction domain: valid21 397,403  valid30 407,658  in30not21 10,255  in21not30 0
```

And the retraction is correct: because the area statistics are computed with
`c(pred30, pred21) %>% filter(...)`, which drops any pixel NA in either layer, all six
manuscript figures reproduce exactly. Only the maps differ in extent.

### 19. orphan-artefacts — CONFIRMED, list incomplete

```
tss_score_tbm.rds : boyce 0.99703  roc_auc 0.83902  tss_max 0.53484
tss_score_tbm.csv : boyce 0.99165  roc_auc 0.83691  tss_max 0.55559   <- the paper's 0.55
models_all_5m.rds : predictors slope snow dist elevation aspect TPI (no roughness/TRI)
risky_area.tiff   : layer "risk", 10,287 valid px, range 0.200-0.823
potential_sasa_area_21.tiff : 17,674 valid px, all >= 0.5
data/sasa_pred_sdm_dist_21.tiff 28,910 px > 0.5 ; _30 34,319 px > 0.5
data/sasa_pred_sdm_12.tiff : 407,649 valid, only 75 px > 0.5
sdm_tdm.R:443 writeRaster("data/sasa_pred_tdm_30_bin.tiff") has no overwrite=TRUE
```

Missed by the original list: `data/sasa_inc.tiff` and `data/selected_comms.tiff` are
*read* (`sdm_tbm.R:446`, `analyse_sdm.R:14,28`) and produced by no committed code, so a
clean run of `sdm_tbm.R` dies at line 446 even after the CRS patch.

### 20. elevation-2560-claim — CONFIRMED

```
elevation quantiles at sasa2021==1: 0% 2364.48 | 50% 2446.43 | 90% 2514.22
                                    99% 2554.43 | 99.9% 2647.50 | 100% 2711.46
N Sasa2021 above 2560: 71 of 10,176 (0.70%)   N Sasa2012 above 2560: 22 of 8,547
terrain non-NA 3,156,718 px; < 2560 m: 1,296,195 (41.1%)
```

### 21. undocumented-block-geometry-and-seeds — CONFIRMED

`tidysdm::spatial_initial_split` body: `v <- round(1/prop)` = 5, then
`rsample::get_rsplit(initial_rset, sample(nrow(initial_rset), 1))`, i.e. one of five
spatial folds becomes the assessment (test) set → ~20% test. Realised: TDM
train 16,540–16,606 / test 4,315–4,357 ≈ 79/21. Block geometry confirmed by the exact
fold-size match reported above. The 5 m² filter (1,171→196 polygons for 2012;
1,440→231 for 2021) appears nowhere in the manuscript, nor do the seeds or the block size.

### 22. dist-extrapolation — CONFIRMED, but it understates the problem

```
training dist 1 - 142.088 m (q99 106.572)
V2 in the 2021 prediction domain: 0 | 29.83 | 86.28 | 131.70 | 231.21 | 283.23
frac of prediction px above the training max: 0.0438
snow: 2021 domain 3.3e-05 outside training range; 2030 domain 36 px below + 11 px above of 407,658
```

See "missed" item M1 — the extrapolation *below* the training range is far larger and
far more consequential.

### 23. terrain-features-no-code — CONFIRMED

`grep -rn terrain_features scripts ortho/georectify.R paper/*.qmd` returns only two
reads (`sdm_tbm.R:13`, `sdm_tdm.R:13`), three writes from `preprocess_snow_data.R`
(snow_mean/sd/reg), and one prose mention (`matmet.qmd:75`). No `terra::terrain()`,
no `gdaldem`, no TWI computation anywhere in the repository.

### Retracted lead: `num.threads = 18` — retraction CORRECT

`default_xgb_1_04`'s booster params:

```
eta 0.0238  max_depth 12  gamma 29.5  colsample_bytree 1  colsample_bynode 0.25
min_child_weight 1  subsample 1  num_threads 18  nthread 1  objective binary:logistic
```

so xgboost received `num_threads` as an unrecognised parameter and actually ran with
`nthread = 1`. One refinement: the GAM spec ends up with **no** engine args at all,
because `update_workflow_model("default_gam", spec = sdm_spec_gam(), …)` replaces the
spec wholesale — so the dead argument survives on rf, maxent and xgb only.

### Retracted lead: `is_others` — retraction CORRECT

`is_others` is defined at `sdm_tdm.R:278-280` and never referenced again
(`grep -n is_others scripts/sdm/*.R` → only those three lines). The real risky-area
filter is `vege21 == 2` at `sdm_tbm.R:343` / `sdm_tdm.R:373`, and class 2 is
"Other Vegetation" (451,770 px in 2021) while class 6 is Montane Alder (24,943 px).
Regenerating both risky-area rasters from the archived predictions gives
**max abs diff = 0** and identical valid-pixel counts (36,797 and 3,149).

---

## What the original audit MISSED

### M1 (critical) — over half of the headline "TDM predicted 12,766 m²" is the 2012 *Sasa* footprint the TDM was never trained on

`sdm_tdm.R:69` removes every `dist == 0` pixel from training (training `dist` runs
1 – 142.088 m), but `env_data_21` (`:257-259`) applies no such filter, so the published
2021 map is predicted over all 397,403 px including 6,954 with `dist == 0`.

```
TDM 2021 map, hs > 0.5 : 12,773 cells   (= 12,766.04 m2)
   of which dist == 0  :  6,675 cells   (52.3%)
   of which dist > 142 :      0 cells
mean HS at dist == 0        : 0.5975
mean HS at 0 < dist <= 142  : 0.1132
```

So the Results sentence "the TDM predicted 12,766 m², much closer to observations
(10,170 m²)" and the Fig. 7 caption's "closely matching the observed distribution" rest
mainly on the model extrapolating *below* its training support onto the very pixels that
were deliberately excluded as trivially occupied — it is reproducing its own baseline.
Outside the 2012 footprint the TDM predicts only ~6,098 m² suitable. The original audit's
`dist-extrapolation` finding looked only at the 4.4% tail above 142 m and missed this.

This is also the cleanest possible answer to Reviewer 2's "the two models were fitted to
different response domains" comment.

### M2 (important) — `aspect` is bilinearly resampled across the 0/360° seam

Both scripts do `rast(...) %>% resample(vege12)` at line 19. `terra::resample()`
defaults to **bilinear** for continuous layers, and `aspect.tif` is 4.97 × 6.16 m, so
every 1 m aspect value is a linear interpolation of neighbouring bearings — including
across the 0/360 discontinuity, where interpolating 359° and 1° yields ~180°.

Measured against a correct circular resample (interpolate sin and cos, then `atan2`):

```
n = 3,154,478 px
circular |bilinear - correct| : median 0.003 deg, q99 151.57 deg, max 180.00 deg
frac > 45 deg : 0.0510  (160,881 px)
frac > 90 deg : 0.0325
```

This corrupts the predictor itself and is independent of the model's linear treatment of
aspect (finding 11) — a northness/eastness decomposition alone would not fix it unless
the decomposition is done *before* resampling. Reviewer 3 asks for the terrain
derivatives to be explained; this is part of that answer.

I checked whether the same defect bites the categorical layers: it does not.
`terra::resample(vege21, .)` inside both risky-area blocks lands on a grid-aligned crop,
so it is value-preserving (0% non-integer output, 8 unique classes). No finding there.

### M3 (minor) — two more read-but-never-produced inputs

`data/sasa_inc.tiff` (`sdm_tbm.R:446`, `analyse_sdm.R:28`) and
`data/selected_comms.tiff` (`analyse_sdm.R:14`) are consumed by committed code and
produced by none, so `sdm_tbm.R` cannot run to completion even after the CRS patch.
They belong in the orphan/provenance list alongside the seven already named.

### M4 (minor) — the manuscript's TSS definition is wrong

`paper/matmet.qmd`: "the True Skill Statistic (TSS …), which ranges from 0 to 1".
TSS = sensitivity + specificity − 1 ranges from −1 to 1. The same block says
"\(TP\), \(FP\), and \(FN\) denote …" but the specificity formula uses `TN`, which is
never defined. Reviewer-visible in a methods section three reviewers already criticised.

### M5 (minor) — the TDM risky-area figure is titled "Risky area (TBM)"

`sdm_tdm.R:405` sets `title = "Risky area (TBM)"` and saves to
`figures/risky_tdm.png`, so `ortho/figures/risky_tdm.png` / `paper/files/risky_tdm.jpg`
carry the wrong model name. The figure used in the paper (`fig-risky-map` →
`files/risky_tbm.jpg`) is the genuine TBM one, so nothing published is wrong, but the
archived TDM figure is mislabelled.
