# Adversarial verification — "downstream and figures" audit

Verifier: independent re-run of every claim. All commands executed in
`/Users/okamoto/NIES/SasaSDMPaper` (data present) with R 4.5.2 / terra / ImageMagick 7.
Scratch scripts: `/private/tmp/claude-501/.../scratchpad/v/chk*.R`.

Summary: **13 CONFIRMED, 2 PARTLY_CONFIRMED, 0 REFUTED.** All five retractions are
correct in substance (one of them, and the corresponding finding, rests on a factually
wrong statement about raster geometry — corrected below). Nine additional problems the
auditor missed are documented at the end; two of them are more serious than anything in
the original list.

---

## 1. `fig8-shows-2021-not-2030` — **CONFIRMED** (and worse than stated)

`paper/results.qmd:98` reads `hsmap_tbm_2021.jpg` / `hsmap_tdm_2021.jpg`.

```
$ magick paper/files/future.jpg -crop 2400x960+0+0 +repage top.png
$ magick compare -metric RMSE top.png paper/files/hsmap_2021.jpg null:
0 (0)                                   <- byte-identical to Figure 7
$ magick paper/files/hsmap_tbm_2030.jpg paper/files/hsmap_tdm_2030.jpg +append -resize 2400x t30.png
$ magick compare -metric RMSE top.png t30.png null:
5464.01 (0.0833755)
```
Rendered panel titles in the top row read
`Habitat Suitability map of Sasa (TBM, 2021)` / `(TDM, 2021)`.

**Extra evidence the auditor did not supply:** the same defect is in the *submitted*
file. `paper/submit_files/future-eps-converted-to.pdf`, rendered at 600 dpi, shows the
identical `(TBM, 2021)` / `(TDM, 2021)` titles. The reviewers therefore never saw a 2030
map, which changes how Reviewer 3's Figure 8 comment must be answered.

`grep -n "hsmap_tbm_2030\|hsmap_tdm_2030" paper/*.qmd paper/index.tex` → no hits.

---

## 2. `paper-figures-not-from-committed-code` — **CONFIRMED** (one evidence line corrected)

md5 of `ortho/figures/*.png` vs `paper/files_original_size/*.png`:

```
SAME cv_dist, cv_wo_dist, initial_split_dist, initial_split_wo_dist,
     model_performance_tbm, model_performance_tdm, risky_tdm
DIFF hsdiff_tbm, hsdiff_tdm, hsmap_tbm_2021, hsmap_tbm_2030,
     hsmap_tdm_2021, hsmap_tdm_2030, risky_tbm, vi_tbm, vi_tdm
```
Side-by-side render of `ortho/figures/risky_tbm.png` and
`paper/files_original_size/risky_tbm.png`: the paper version carries a north arrow and a
`200 m` scale bar; the committed-code version has neither. Same for the four hsmap
panels and the two hsdiff panels.

**Correction to the auditor's evidence.** The claim
`grep -rn 'ggspatial|annotation_scale|north_arrow' scripts/ -> no hits in scripts/sdm/`
is false: `scripts/sdm/plot_vegetation_map.R:3` is `library(ggspatial)`. What is true —
and is the load-bearing point — is that **no `annotation_scale()` or
`annotation_north_arrow()` call exists anywhere in the repository**, including in the
file that loads the package. The stray `library(ggspatial)` is in fact positive evidence
that the deleted English version of `plot_vegetation_map.R` used those annotations.

---

## 3. `risky-area-tautological` — **CONFIRMED**, numbers reproduce exactly

```
risky_area_wo_dist.tiff  n=36797  2021: {2:36797}
   2012: 0:1 1:366 2:33953 3:45 4:519 5:209 6:1141 7:563
risky_area_tdm.tiff      n=3149   2021: {2:3149}
   2012: 1:260 2:2709 3:3 4:42 5:22 6:17 7:96
risky_area.tiff          n=10287  2021: {2:10287}
   2012: 2:9353 3:94 4:110 5:104 6:279 7:347
```
`sdm_tbm.R:343` `filter(vege21 == 2)` makes this true by construction.

---

## 4. `black-outlines-filtered-subset` — **CONFIRMED** numerically

```
total polys 1440 | >5 m2: 231 | <=5 m2: 1209
area >5 m2 (planar) 8333 m2 ; geodesic 8328.5 m2   (auditor's 8328.493 = geodesic)
area <=5 m2 1843 m2
fraction of 2021 Sasa area NOT outlined: 18.111 %
```
No legend entry for the outlines exists (verified in the rendered legend of
`paper/files/hsmap_2021.jpg`: the only legend key is "Habitat Suitability"), and
`matmet.qmd` never mentions them.

Caveat: the attribution to Reviewer 3's "Figure 7: black areas are unclear" is a
plausible reading, not a demonstrated fact. A 200 % zoom of the Figure 7 TDM panel shows
the outlines still resolvable at screen scale; they coalesce at print scale.

---

## 5. `fig7-fig8-colour-scales-differ` — **CONFIRMED**, with one nuance and a bad reviewer link

Raster ranges reproduce exactly:
```
TBM2021 0.1601363 .. 0.7395326    TDM2021 0.0856436 .. 0.6425344
TBM diff -0.4597578 .. 0.4702161  TDM diff -0.5096740 .. 0.5279855
```
Rendered legends cropped out of the published JPEGs: TBM `0.2..0.7`, TDM `0.1..0.6`;
diff panels `-0.25..0.25` vs `-0.50..0.50`. Confirmed.

**Nuance the auditor missed:** both calls fix `midpoint = 0.5`, so the pale-yellow
inflection sits at HS = 0.5 in *both* panels. The suitable/unsuitable read is therefore
consistent; what differs is the stretch on either side (TBM below-midpoint span 0.34 vs
TDM 0.41; above-midpoint 0.24 vs 0.14). The claim "identical colours encode different HS
values" holds, but the figure is not as misleading as a naive reading suggests.

**Bad reviewer link.** Reviewer 1's "Figure 5 should use a single shared legend … the
duplicated legends consume space" is about Figure 5 (`model_performance`), which really
does carry two identical GBT/GAM/MaxEnt/RF legends (verified by rendering
`paper/files/model_performance.jpg`). It is not about Figures 7–8.

---

## 6. `vi-panels-different-datasets` — **CONFIRMED**

`sdm_tbm.R:169-180` uses `df_test` + `DALEX::explain`; `sdm_tdm.R:176-189` uses
`df_train` + `DALEXtra::explain_tidymodels`. `grep -n set.seed` returns only lines 61/81
(tbm) and 71/92 (tdm) — all before `spatial_block_cv`, none before `model_parts`
(tbm:190, tdm:200).

Instability verified visually:
* TBM — `ortho/figures/vi_tbm.png`: snow, elevation, TRI, **roughness, aspect**, slope, TPI.
  `paper/files_original_size/vi_tbm.png`: snow, elevation, TRI, **aspect, roughness**, slope, TPI.
* TDM — committed: dist, snow, elevation, aspect, TRI, **roughness, slope**, TPI.
  published: dist, snow, elevation, aspect, TRI, **slope, roughness**, TPI.

The `ylim(c(0, 0.6))` is present in the committed output and absent from the published
one, confirming again that the published figures come from a different code version.

---

## 7. `lost-area-percentage-wrong` — **CONFIRMED**, and the effect is larger than stated

```
TBM lost (sasa_pol_21 >5 m2, pred30<0.5)   2259 px  expanse 2257.768   [ms: 2,257]
TDM lost                                    717 px  expanse  716.609   [ms:   717]
2257.768 / 10170.454 = 22.199 %   <- manuscript says 21 %
 716.609 / 10170.454 =  7.046 %   <- manuscript says  7 %  (correct)
```
So "21 %" is simply wrong; it should be 22 %.

**Correction / extension.** The auditor's like-for-like alternative (filtered numerator
over filtered denominator, 27.1 % / 8.6 %) is arithmetically right but is the *less*
interesting of the two repairs. Recomputing the numerator on the **unfiltered** 2021
Sasa raster — matching the 10,170 m² denominator the manuscript actually uses — gives:

```
TBM lost (all 2021 Sasa px)  3102 px  expanse 3100.309  -> 30.5 %
TDM lost (all 2021 Sasa px)  2360 px  expanse 2358.715  -> 23.2 %
```

The 5 m² filter suppresses the reported TDM loss by a factor of 3.3 (717 → 2,359 m²).
Small patches are disproportionately predicted to become unsuitable (843 of 1,843 px,
46 %). This is a much bigger deal than "21 % should be 22 %" and it lands directly on
Reviewer 1's and Reviewer 2's gross-vs-net complaints.

---

## 8. `hs-threshold-uncalibrated` — **CONFIRMED**, table reproduces exactly

```
0.40 : 70368 px   0.45 : 52429   0.50 : 36797
0.55 : 20374      0.60 :  8918   0.65 :  3222
```
(+42.5 % at 0.45, −44.6 % at 0.55.)
TBM 2021 HS>0.5 = 47,279 px; TDM = 12,773 px. `matmet.qmd:137` gives no calibration
rationale; `tss_max` appears only as a tuning metric.

---

## 9. `2021-2030-domain-mismatch` — **CONFIRMED**, causal story refined

```
sasa_pred_sdm_21 nonNA 397403 ; sasa_pred_sdm_30 nonNA 407658
in30not21 10255 ; in21not30 0
snow resampled to vege grid: s12 1206057, s21 1178227, s30 1206076
   in30not21 27849 ; in21not30 0
TBM 2021 HS>0.5 = 47279 ; 2030 = 69214 ; difference = 21935
TBM (pred21<0.5 & pred30>0.5) = 27064   <- the manuscript's 27,049 m2
21935 / 27064 = 0.81  -> the alternative definition is 19 % smaller
```
**Refinement.** Part of the cause is not a mask but a raster *extent*:
`data/snow/fitted_2021.tiff` is 1703 × 1801 whereas `fitted_2012.tiff` and
`fitted_2030.tiff` are 1753 × 1801 (ymax 4052019.75 vs 4052069.75). However only 2,025
of the 27,849 differing snow cells lie in that missing 50 m strip; the remaining 25,824
are a genuine NA-mask difference inside the common extent. 1,171 of the 10,255 extra
2030-only prediction cells have HS > 0.5, i.e. they contribute to the 69,214 px 2030
suitable area but can never appear in the difference map.

---

## 10. `figs-1-2-4-no-code` — **CONFIRMED**

* `plot_vegetation_map.R` hard-codes `c("ハイマツ","ササ類",…)` and writes
  `ortho/data/2012_5x5.png` / `2021_5x5.png`; `ls ortho/data/ | grep png` → only
  `matched.png`. Its `cmap` (#2aa198 / #859900 / #dc322f / #b58900 / #6c71c4 / #eee8d5 /
  #c0c0c0) matches the published English panels exactly, so the script is the Japanese
  ancestor of a deleted English version.
* Rendering `paper/files_original_size/2012_5x5_en.png` confirms English class names, a
  north arrow and a `500 m` scale bar — i.e. ggspatial annotations that are not in the code.
* `paper/files/expanded_area.pdf` embeds `IPAexGothic`; the render shows 玉殿岩屋,
  立山室堂山荘, 地理院地図, the green "Sasa distribution in 2012" box, the yellow
  "Expanded Area" box, white snow areas and a right-hand magnified inset. No producing code.
* Source panel sizes 1081×721 and 1081×720 — the 1 px mismatch is real.

---

## 11. `analyse-sdm-dead` — **CONFIRMED**, numbers reproduce exactly

Executed the script's logic:
```
selected_comms unique: 5 6 8 9 11 14 15 17 19 20 21 22 26 28 29 30 32 34 35 38 … (51 values, 5159 cells)
dist range 0 .. 1322.89
risk nonNA 1258 ; sasa_inc nonNA 1261 ; joined rows 1239
```
`grep -c "ggsave\|writeRaster" scripts/sdm/analyse_sdm.R` → 0. `sasa_pred_2030` (L11-12)
is never referenced again; `tidysdm` and `DALEX` are loaded and unused.
`terra::aggregate(fact = 5)` (default `fun = "mean"`) precedes `filter(dist < 10)`.

**Minor correction:** the repo contains **four** distinct `setwd()` roots, not three:
`~/doctoral_thesis/chap2/ortho/` (sdm_tbm, sdm_tdm, preprocess_snow_data),
`~/Projects/jasms2023f/ortho/` (analyse_sdm, ortho/georectify.R),
`~/Projects/jasms2023f//` (plot_vegetation_map), and
`~/VegetationMapPaper/` (vegetation_classification/utils/interpolate.R).

---

## 12. `orphan-risky-rasters` — **CONFIRMED**

```
risky_area.tiff              names=risk           nonNA 10287  0.2000373 .. 0.8234023 (full 1753x1801 extent)
potential_sasa_area_21.tiff  names=pred_sasa_21   nonNA 17674  0.5 .. 0.92498
risky_area.tiff  matched 0/10287 against all 7 candidate prediction rasters
potential_sasa_area_21.tiff  matched only itself (17674/17674)
```
`grep -rn 'risky_area\.tiff\|potential_sasa' scripts/` → no hits (the scripts write
`risky_area_wo_dist.tiff` and `risky_area_tdm.tiff` only).

Note the two real risky rasters are 1147 × 1213 (trimmed); `risky_area.tiff` is the full
1753 × 1801 grid, another sign of a different code path.

---

## 13. `risky-tdm-wrong-title` — **CONFIRMED**

`grep -n "Risky area" scripts/sdm/*.R` → `sdm_tbm.R:375` and `sdm_tdm.R:405`, both
`title = "Risky area (TBM)"`. `sdm_tdm.R:409` saves to `figures/risky_tdm.png`.

---

## 14. `tbm-script-cannot-run` — **PARTLY_CONFIRMED**

Confirmed: `sdm_tbm.R:96-113` activates only `rf = sdm_spec_rf()`; gam/maxent/xgb are
commented out. `ls ortho/*.rds` contains no `models_wo_dist_twi.rds`. The archived
`models_wo_dist.rds` has 44 tuning candidates with best TSS maxent 0.5768, xgb 0.5741,
gam 0.5540, rf 0.5369 — and the rendered left panel of
`paper/files/model_performance.jpg` runs to Workflow Rank 44 with a top teal (MaxEnt)
point at ~0.577 and a single olive (GAM) point at ~0.554. So Figure 5's left panel does
come from `models_wo_dist.rds`.

**Wrong in the title/claim:** "then reads a tuning object that does not exist". On a
clean top-to-bottom run, `L127 saveRDS(models, "models_wo_dist_twi.rds")` *creates* the
file two lines before `L129 readRDS()` reads it back. The read does not fail.

Corrected statement of the defect:
1. only RF is trained, so the 44-candidate four-learner Figure 5 panel cannot be reproduced;
2. the script writes/reads `models_wo_dist_twi.rds` while the artefact that made the
   figure is `models_wo_dist.rds`, so a clean run never touches it;
3. `data/terrain_features/` now contains `twi.tif`, and the glob at L13-19 would pick it
   up. Verified from the archived recipe that the fitted TBM used **seven** predictors —
   `aspect roughness slope elevation TPI TRI snow` — with no `twi` (and the TDM eight,
   adding `dist`). A clean re-run today would fit a different model from the paper's.
   (`X`/`Y` are present in the recipe with role `coords`, not as predictors — that part
   is fine.)

---

## 15. `expanse-geodesic-undocumented` — **CONFIRMED** exactly

```
Sasa2012 expanse 8542.341  px 8547     Sasa2021 expanse 10170.454 px 10176
gain     expanse 4094.768  px 4097     loss     expanse  2466.655 px  2468
ratio expanse/px = 0.999455 for all
TBM2021 47253.26 | TDM2021 12766.04 | TBM new 27049.27 | TDM new 4386.61
```
All eight manuscript areas reproduce to the rounding. Retraction upheld.

---

## 16. `resample-categorical-latent` — **PARTLY_CONFIRMED** (conclusion right, evidence wrong)

The conclusion is right and I verified it harder than the auditor did: I re-executed the
whole `sdm_tbm.R:336-344` block and the result is **bit-identical** to the archived raster.
```
final dim 1147 1213  ext 732744 733957 4050650.25 4051797.25  nonNA 36797
archived  1147 1213  ext identical                             nonNA 36797
identical values? TRUE   max abs diff 0
```

**The stated reason is factually wrong.** The auditor wrote "Both `vege_2021_5x5.tiff`
and `sasa_pred_sdm_30.tiff` are 1753x1801 … so resample is a no-op." The resample target
is not `sasa_pred_sdm_30.tiff`: `tidyr::drop_na()` at L338 **trims** the three-layer
stack to 1147 × 1213 (ext 732744–733957 / 4050650.25–4051797.25), so `vege21` is
resampled from 1753 × 1801 *onto that crop*. It reduces to identity only because the crop
sits on the same 1 m lattice. The same correction applies to retracted lead #4.

Two additional latent hazards at the same spot, not noted by the auditor:
* `crs(vege_2021_5x5.tiff)` is EPSG:6690 while `crs(sasa_pred_sdm_*.tiff)` is EPSG:3099
  — `terra::same.crs()` returns `FALSE`. `resample()` does not reproject, it silently
  assumes the coordinates are comparable. Harmless here (JGD2011 vs JGD2000 UTM 53N), but
  it means a `project()`-based fix would shift the grid.
* The same undeclared-CRS resample is applied to the terrain stack in both SDM scripts.

---

## 17. `methods-risky-definition-drift` — **CONFIRMED**

```
manuscript-literal (vege21==2 & TBM30>0.5)        37138 px  expanse 37117.79
code (adds pred21 non-NA via drop_na)             36797 px  expanse 36776.97
```
341 cells. `grep` confirms no risky-area m² figure appears anywhere in `paper/*.qmd`.

**Refinement:** the `filter(sasa == 0)` in the chain is entirely redundant — stepping
through, `literal 37138 → +pred21 non-NA 36797 → +sasa==0 36797`. The sole effective
extra restriction is the 2021-prediction non-NA mask, exactly as the auditor said, but
the mechanism is worth stating precisely for the fix.

---

## Retracted leads — all five retractions upheld

| retraction | verdict | my evidence |
|---|---|---|
| expanse geodesic, not a discrepancy | **correct** | ratio 0.999455 on all four base areas; all 10 manuscript areas reproduce |
| `scale_color_discrete(labels=…)` is right | **correct** | `ggplot_build` on `autoplot(models_wo_dist.rds)`: colour scale limits are `boost_tree \| gen_additive_mod \| maxent \| rand_forest`, labels `GBT \| GAM \| MaxEnt \| RF` — correct mapping |
| `prop = 0.2` gives 80 % training | **correct** | `tidysdm::spatial_initial_split` computes `v <- round(1/prop)` = 5 blocks and holds one out. Empirical: 1635 train / 365 test on n = 2000 |
| categorical resample is currently harmless | **correct in substance, wrong in stated geometry** | see §16 — target grid is 1147×1213, not 1753×1801; result still bit-identical |
| snow-2030 0–255 range is 3 outliers | **correct** | within the 407,658-cell 2030 prediction domain: 1 cell ≤ 0, 2 cells ≥ 250, and only 5 cells outside the full 2021 snow range [82.8, 227.9] |

---

# What the auditor missed

## M1 (critical) — the archived fitted models cannot produce a prediction at all

Every ensemble member of type xgboost fails to deserialise against the installed
XGBoost 3.2.1.1:

```
== model_stack.rds        predict -> ERROR: In index: 4.
    default_maxent_1_02 : OK   default_maxent_1_14 : OK   default_maxent_1_03 : OK
    default_xgb_1_02 : ERROR   default_xgb_1_04 : ERROR
    default_xgb_1_08 : ERROR   default_xgb_1_10 : ERROR
== model_stack_wo_dist.rds predict -> ERROR: In index: 3.
    default_maxent_1_17 : OK   default_maxent_1_12 : OK
    default_xgb_1_05 : ERROR   default_xgb_1_09 : ERROR
```
Underlying error: `'xgb.Booster' object is corrupted or is from an incompatible XGBoost
version.` Both ensembles contain xgb members, so **neither model can be re-run**.

This is the deepest version of `paper-figures-not-from-committed-code`: restoring the
ggspatial plotting blocks would still not regenerate Figures 7–9, because
`predict_raster(model_stack, …)` cannot execute. Any permanent-repository deposit must
either re-fit from scratch (see M2 — which is not deterministic) or archive
`xgb.save()`-format boosters plus a `renv.lock`.

## M2 (critical) — the training sample is not reproducible: `thin_by_cell` is unseeded

`tidysdm::thin_by_cell` begins `data <- data[sample(seq_len(nrow(data))), ]` and keeps
the first point per mask cell. In `sdm_tbm.R` the call is at L52-59 and in `sdm_tdm.R` at
the corresponding block — **both before the first `set.seed(1)`** (tbm L61, tdm L71).

Demonstrated on a 300 × 300 m window of the real 2021 raster, four repeats:
```
absence points in window: 73114
rep1 n=3141 mean elev 2432.836
rep2 n=3141 mean elev 2432.800
rep3 n=3141 mean elev 2432.811
rep4 n=3141 mean elev 2432.825
identical rep1 vs rep2 selections? FALSE
overlap rep1 & rep2: 170 of 3141   (5.4 %)
```
The absence set is roughly 63 % of the training frame (archived TBM `df_train` = 21,389
rows; presences inside the elevation < 2560 domain = 9,974 px). Two runs share ~5 % of
their absences, so every downstream number — TSS, variable importance, the four HS
rasters, all eight areas — is a single draw from a distribution nobody has characterised.
This is the single most damaging answer to Reviewer 3's reproducibility complaint, and
the cheapest to fix (`set.seed()` before the thinning, plus report the seed).

## M3 (important) — the TDM's `dist` predictor differs between training and prediction

`sdm_tdm.R:44-47` builds the training `dist` from **all** 2012 Sasa pixels:
`sasa12_ras %>% filter(sasa == 1) %>% distance()`.
`sdm_tdm.R:247-250` builds the 2021-prediction `dist` from 2012 Sasa **polygons > 5 m²**
only, and the 2030-prediction `dist` from 2021 polygons > 5 m².

Which one the fitted model saw is decidable from the archived object:
```
n train pts 16596
cor(model_stack$train$dist, distance(all 2012 Sasa px)) = 1        max|diff| = 0
cor(model_stack$train$dist, distance(2012 polys >5 m2)) = 0.8179   max|diff| = 234.98, mean|diff| = 13.88
train dist range: 1 .. 142.088
```
So training used all 8,547 pixels; the code that made the published 2021 TDM map uses
7,074 pixels. Over the whole grid the two layers differ on 88.4 % of cells (median 37 m,
max 343 m). `dist` is the TDM's dominant predictor (permutation TSS loss ≈ 0.46 vs 0.21
for snow), so this is a first-order train/predict feature mismatch on the model the
manuscript presents as the accurate one.

Reviewer 2 already noticed half of this ("The public code appears to calculate the
future-distance layer from the 2021 distribution"). The 5 m² filter is the half they did
not see, and neither the filter nor the 2012→2021 switch is in `matmet.qmd`.

*(Extrapolation is a lesser concern: within the prediction domain only 4.4 % of 2021
cells and 3.0 % of 2030 cells exceed the training max of 142 m.)*

## M4 (important) — Figures 5 and 6 put panels with different y-axes side by side

Published `paper/files/vi.jpg`: the TBM panel's y-axis tops at ≈ 0.26, the TDM panel's at
≈ 0.55. The TBM's most important predictor (snow, TSS loss 0.20) therefore draws a box
*taller* than the TDM's second predictor (snow, 0.21) and nearly as tall as the TDM's
dominant `dist` (0.46). Same problem in `model_performance.jpg` (TBM 0.35–0.70, TDM
0.62–0.74). The committed code applies `ylim(c(0, 0.6))` to both VI plots — the published
versions do not, so the shared scale was actively lost. Directly compounds Reviewer 3's
"panels are all way too small" and Reviewer 1's shared-legend request.

## M5 (important) — Figures 1, 2 and 4 are below the journal's 300 dpi requirement

Effective resolution of the submitted EPS/PDF (raster pixels ÷ BoundingBox points × 72):
```
overview            2634x1482 px in 1317x741 pt  -> 144.0 dpi
vegemap             2400x800  px in  945x315 pt  -> 182.9 dpi
expanded_area.pdf   embedded images at 128 and 217 ppi
--- all others ---
initial_split, model_performance, vi, hsmap_2021, future, risky_tbm -> 299.5 dpi
```
Exactly the three figures with no producing code are the three that miss the standard.
Figure 2 is doubly degraded: `paper/files_original_size/2012_5x5_en.png` is 3600 × 2400,
but `matmet.qmd:57` reads the 1081 × 721 JPEG derivative (RMSE 0.037 against a
resampled PNG — same image), appends to 2162 × 721, then **upscales** to 2400 × 800.
Rebuilding Figure 2 from the PNGs already in the repo fixes Reviewer 1's "Figure 2 is
difficult to read at its current size" at zero analytical cost.

## M6 (important) — six of the seven TBM predictors are ~5–6 m data upsampled to 1 m

```
aspect.tif roughness.tif slope.tif tateyamadem_small.tif TPI.tif TRI.tif twi.tif
    all 1198 x 1263, res 4.9700 x 6.1600 m
vege_2021_5x5.tiff  res 1 x 1
```
`sdm_tbm.R:19` / `sdm_tdm.R:19` `resample(vege12)` bilinearly interpolates all of them
onto the 1 m grid. The Methods say the imagery was georectified "at 1 m spatial
resolution" and never say the topographic predictors are not. Reviewer 3's central
criticism is precisely about the 1 m claim ("high spatial resolutions (like 1 m in the
presented example) hamper the consideration of other potential drivers"), and Reviewer 2
asks for the collinearity structure — which is partly manufactured by this upsampling.
Bilinear interpolation of `aspect` (raw degrees 0–360) additionally produces wrap-around
artefacts at the north-facing seam.

## M7 (minor) — Figure 7's caption describes a figure that was not drawn

`results.qmd:75`: "The maps show areas predicted as suitable for *Sasa* habitation
(HS > 0.5)." The maps show a **continuous** HS surface over the whole prediction domain
with no threshold contour and no 0.5 isoline; the 47,253 / 12,766 m² numbers are not
readable off them. This is the same complaint as Reviewer 3's "I suggest a single panel
showing the suitability (yes/no instead of having many classes)".

## M8 (minor) — "47,253 vs 10,170" is not a like-for-like comparison

The predicted areas are computed over the HSM domain (397,403 cells: valid terrain ∩
valid snow ∩ elevation < 2560), whereas the observed 10,170 m² is over the full
vegetation-map extent. 202 of the 10,176 observed 2021 Sasa pixels (2.0 %) lie outside
the prediction domain, so the overprediction ratio is computed against a slightly
different area than the one the model could have predicted.

## M9 (minor) — `data/snow/fitted_2019.tiff` does not exist

`ls ortho/data/snow/` yields fitted_2011–2018, 2020, 2021, 2030 — no 2019. Reviewer 2
guessed this ("apparently lacks observations for 2019"); it is confirmed. Worth stating
explicitly in the response rather than leaving the reviewer to infer it.
