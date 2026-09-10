# Audit of the habitat-suitability-model subsystem (`sdm_tbm.R`, `sdm_tdm.R`)

Scope: `scripts/sdm/sdm_tbm.R`, `scripts/sdm/sdm_tdm.R` and the artefacts under
`ortho/`. All numbers below were computed by executing code against the real data in
`/Users/okamoto/NIES/SasaSDMPaper`; commands and outputs are quoted.

Software actually used for the audit: R 4.5.2, terra 1.9.34, tidysdm 1.0.4,
tidymodels 1.5.0, stacks 1.1.1, spatialsample 0.6.1, DALEX 2.5.4.
**xgboost 3.2.1.1 (the installed version) cannot read the archived boosters at all**
(see F1). All predictions in this report were produced after installing
xgboost 1.7.11.1 into a scratch library
(`/private/tmp/.../scratchpad/Rlib`, used via `R_LIBS=`).

Throughout I distinguish
(a) what the code as written would do on a clean run,
(b) what the archived artefacts show actually happened,
(c) what the manuscript says.

---

## 0. Executive summary of the most serious items

| # | Finding | Affects published numbers? |
|---|---|---|
| F1 | Archived ensembles are unreadable with current xgboost — nothing in the paper can be regenerated on a clean machine | No number changes, but reproducibility is nil |
| F2 | Both scripts abort on a clean run today (`CRS mismatch` at `bind_rows`) | – |
| F3 | TDM 2030 projection: ~90 % of the projected expansion comes from swapping the distance layer 2012→2021, not from earlier snowmelt | **Yes** — the central 2030 result |
| F4 | TDM training distance ≠ TDM prediction distance (different definitions). Fixing it changes 2021 suitable area from 12,766 to 18,103 m² (+42 %) | **Yes** — Fig. 7 and the "TDM matches observation" claim |
| F5 | `sdm_tbm.R` as committed trains only RF and reads/writes `models_wo_dist_twi.rds`, which does not exist. The published TBM came from a different version of the file | Provenance |
| F6 | Variable importance: TBM computed on the **test** set, TDM on the **training** set; two different explainer calls; unseeded `N=1000` subsample. The two published panels are not comparable and not reproducible | Fig. 6 |
| F7 | A clean re-run now picks up `terrain_features/twi.tif` as an 8th/9th predictor. `twi` is in neither the fitted models nor the manuscript | Would change everything on re-run |
| F8 | Absence thinning (`thin_by_cell`) is stochastic and unseeded → the exact training/test split is unrecoverable | ±0.01–0.03 TSS |
| F9 | Severe collinearity in the actual modelling frame: VIF TRI 25.4, roughness 17.6, slope 10.7 | Reviewer 2's request |
| F10 | `aspect` (0–360°) enters as a linear/ordinal variable; TBM habitat suitability jumps 0.055 across the 0°/360° seam, straddling the 0.5 cut-off | Reviewer-exposed |
| F11 | Reviewer 2 is right that the two models are fitted to different response domains (TBM prevalence 36.6 %, TDM 18.8 %) | TSS comparison |
| F12 | Fig. 8 in the manuscript shows the **2021** HS maps in the row the caption calls 2030 | Fig. 8 |

---

## 1. Predictor sets actually used (read out of the `.rds`, not inferred)

```r
ms <- readRDS("ortho/model_stack.rds");          names(ms$train)
# [1] "sasa" "geometry" "aspect" "roughness" "slope" "elevation" "TPI" "TRI" "snow" "dist"
ms <- readRDS("ortho/model_stack_wo_dist.rds");  names(ms$train)
# [1] "sasa" "geometry" "aspect" "roughness" "slope" "elevation" "TPI" "TRI" "snow"
```

The recipes inside every workflow of `models.rds` / `models_wo_dist.rds` agree:

```
models.rds          default_rf|gam|maxent|xgb : aspect,roughness,slope,elevation,TPI,TRI,snow,dist
models_wo_dist.rds  default_rf|gam|maxent|xgb : aspect,roughness,slope,elevation,TPI,TRI,snow
```

* **TBM = 7 predictors**, **TDM = 8**. This matches @matmet.qmd exactly
  ("elevation, slope, aspect, roughness, TRI, TPI, snowmelt DOY" + distance for the TDM).
* **`twi` was NOT in either fitted model** and appears nowhere in `paper/*.qmd`
  (`grep -rn twi paper/*.qmd` → no hits).

**But a clean re-run today would include it.** Both scripts build the predictor stack by
globbing the directory:

```r
terrain <- list.files("data/terrain_features/", full.names=T) %>% str_subset(".tif$") %>% rast()
# [1] "aspect.tif" "roughness.tif" "slope.tif" "tateyamadem_small.tif" "TPI.tif" "TRI.tif" "twi.tif"
```

→ `terrain_now layers: aspect,roughness,slope,elevation,TPI,TRI,twi` (7 → TBM would train
on 8 predictors, TDM on 9). **F7.**

It gets worse: `scripts/sdm/preprocess_snow_data.R` writes three more rasters *into the same
globbed directory*:

```
preprocess_snow_data.R:67   write_stars("data/terrain_features/snow_mean.tif")
preprocess_snow_data.R:96   write_stars("data/terrain_features/snow_sd.tif")
preprocess_snow_data.R:137  write_stars("data/terrain_features/snow_reg.tif")
```

(Those three files currently sit in `data/snow/`, not in `terrain_features/` — so they were
moved by hand at some point, or the path was edited afterwards.) Anyone who runs the
pipeline "in order" gets 10–11 predictors instead of 7–8, silently.

**Data-provenance gap:** *no script in the repository generates
`data/terrain_features/*.tif`.* `grep -rn "terrain_features" scripts/` only ever *reads*
them (plus the three snow writes above). The DEM derivatives (aspect, slope, roughness,
TRI, TPI, `tateyamadem_small`, `twi`) have no code. Note also that `*.tif` is **not** in
`.gitignore` (only `*.tiff`), so `terrain_features/twi.tif` is published on GitHub — a
reviewer who looks at the repo sees a predictor the paper never mentions.

### Grid geometry / CRS defect (F2 root cause)

```
data/vege_2012_5x5.tiff       1753x1801, res 1 m, EPSG:6690 (JGD2011 / UTM 53N)
data/terrain_features/*.tif   1198x1263, res 4.97 x 6.16 m, EPSG:3099 (JGD2000 / UTM 53N)
data/snow/fitted_2012.tiff    1753x1801, res 1 m, EPSG:6690
data/snow/fitted_2021.tiff    1703x1801  <-- 50 rows short
data/snow/fitted_2030.tiff    1753x1801
```

`terra::resample(terrain, vege12)` silently resamples across the CRS mismatch **and keeps
the source CRS**:

```r
crs(resample(rast("data/terrain_features/TPI.tif"), vege12), describe=TRUE)$code
# [1] "3099"     (no warning emitted)
```

so the whole environmental stack is labelled JGD2000 while the response and snow layers are
JGD2011, and `c(terrain, snow)` takes the first CRS. Consequences:

* every archived prediction raster (`data/sasa_pred_*.tiff`, `risky_area*.tiff`) carries
  **EPSG:3099** although the vegetation maps it is compared against are EPSG:6690;
* on a clean run the pipeline now dies (see §2);
* the DEM is bilinearly interpolated from ~5×6 m to 1 m, i.e. six of the seven predictors
  carry no information below ~5 m. The manuscript states this ("resampled to 1 m
  resolution to match the vegetation maps") but does not note that the models are therefore
  effectively 5 m models with a 1 m response.

---

## 2. `sdm_tbm.R` / `sdm_tdm.R` do not run as written (F2)

Executed the head of `sdm_tdm.R` verbatim. It fails at lines 61–67
(`sdm_tbm.R` 52–58), i.e. the first `bind_rows`:

```
CRS of sampling_mask: 3099
CRS of points:        6690
CRS after thin_by_cell: 3099           # thin_by_cell st_transform()s to the mask CRS
bind_rows(thinned_absence, presence): ERROR -> CRS mismatch: PROJCRS["JGD2000 / UTM zone 53N",
```

`tidysdm:::thin_by_cell` begins with `data <- data %>% sf::st_transform(terra::crs(raster))`
and returns the reprojected object, so the thinned absences come back as EPSG:3099 while the
untouched presences are EPSG:6690; `bind_rows` on two `sfc` with different CRS aborts.

Everything below was produced with **one** patch to the scripts:
`crs(terrain) <- crs(vege12)` immediately after `resample()`, and `select(-twi)`.
With that patch the pipeline reproduces the archived artefacts bit-for-bit (§8, §10), which
confirms that the patch is the only divergence.

---

## 3. Response variable and sampling design

Reconstructed from the rasters (patched pipeline) and cross-checked against `ms$train`.

|  | TBM | TDM |
|---|---|---|
| Response | `vege_2021_5x5.tiff == 1` → presence, else absence | same |
| Candidate points | 1,159,498 (all non-NA 1 m pixels) — 10,032 presence / 1,149,466 absence | same |
| Absence thinning | `thin_by_cell(sampling_mask)`, mask = elevation aggregated `fact=5` → 5 m | same |
| Presence thinning | **none** (kept at 1 m) | none |
| Elevation filter | `elevation < 2560` | `elevation < 2560` |
| Extra filter | – | **`dist > 0`** (drops every pixel that was *Sasa* in 2012) |
| `df_21` (reproduced) | 27,055 = 9,974 pres / 17,081 abs, prevalence **36.9 %** | 20,891 = 3,973 pres / 16,918 abs, prevalence **19.0 %** |
| `ms$train` (archived) | 21,389 = 7,820 / 13,569, prevalence **36.6 %** | 16,596 = 3,123 / 13,473, prevalence **18.8 %** |
| Split | `set.seed(1); spatial_initial_split(df_21, prop=0.2, spatial_block_cv)` | same |
| CV | `set.seed(1); spatial_block_cv(df_train, v=4)` | same |

All 7,820 (TBM) and 3,123 (TDM) archived training presences match my reconstructed candidate
presences **exactly by coordinate**, and my reproduced CV fold sizes for the TDM

```
CV fold assessment sizes: 3286 5217 3054 5039
```

are **identical** to the fold sizes stored in `models.rds`. So the covariate extraction and
the block CV are exactly reproducible; only the absence draw is not (§7, F8).

### Adjudicating Reviewer 2 ("the two models were fitted to different response domains")

**Reviewer 2 is correct.** The TDM's `filter(dist > 0)` removes every pixel inside the 2012
*Sasa* distribution: presences fall from 9,974 to 3,973 (−60 %), prevalence from 36.9 % to
19.0 %. The TBM is a model of *occupancy in 2021* (persistence + colonisation); the TDM is a
model of *colonisation among previously unoccupied pixels*. The published TSS values
(0.5556 vs 0.7010) are therefore computed on different populations.

The manuscript does state this in "Target variable" ("pixels located directly within the
2012 distribution … were excluded from the TDM"), but §Results then compares the two TSS
values head-to-head ("indicating clear performance gains from incorporating distance")
without noting the domain change.

**However, the direction of the conclusion survives.** I built a held-out set that is
`dist > 0` and unseen by *both* archived models (n = 4,236; 844 presences) and scored both
ensembles on it:

```
TDM on common domain:  boyce 0.9193  roc_auc 0.9134  tss_max 0.6992
TBM on common domain:  boyce 0.9877  roc_auc 0.7796  tss_max 0.4468
```

So on a common domain the gap is *larger* (0.699 vs 0.447), not smaller. The published TBM
0.556 is flattered by the dist = 0 pixels (2012 *Sasa* cores) that are trivially easy
positives.

---

## 4. The distance predictor: three different definitions (F3, F4)

`sdm_tdm.R` builds the distance layer three times, in three different ways:

| Variant | Where | Definition |
|---|---|---|
| **V1** | lines 44–47, used for **training** | `sasa12_ras %>% filter(sasa==1) %>% distance()` — distance to the nearest 2012 *Sasa* **pixel** (no area filter) |
| **V2** | lines 238–250, used for the **2021 prediction** (`env_data_21`) | distance to the nearest 2012 *Sasa* **polygon with area > 5 m²** |
| **V3** | lines 229–236, 252–255, used for the **2030 projection** (`env_data_30`) | distance to the nearest **2021** *Sasa* polygon with area > 5 m² |

The 5 m² filter is not cosmetic: it deletes **975 of the 1,171** 2012 *Sasa* polygons
(83 % of patches, 8,547 m² → 7,070 m² of source area) and 2021 10,176 m² → 8,328 m².

### 4.1 Training vs prediction (V1 vs V2) — they are NOT the same definition

Restricted to the modelling domain (`elevation < 2560`, n = 1,296,195 pixels):

```
V1 -> V2 : frac of pixels differing = 0.8525   mean|d| = 18.58 m   max|d| = 238.5 m
           mean V1 = 163.7 m,  mean V2 = 182.3 m
V2 - V1 quantiles: 1% 0.0 | 25% 4.5 | 50% 13.3 | 75% 27.8 | 95% 55.2 | 99% 96.4 | max 238.5
```

**85 % of pixels get a different distance value at prediction time than the model was
trained on**, systematically inflated (median +13.3 m). Feeding the training-consistent V1
into the archived TDM stack instead:

```
TDM 2021, V2 (as published) : 12,766.0 m² suitable (HS>0.5)   [reproduces the paper exactly]
TDM 2021, V1 (train-matched):  18,103.1 m² suitable            (+41.8 %)
pixels flipping the 0.5 threshold: 5,340 / 397,403 (1.34 %)   mean|ΔHS| 0.017, max 0.527
```

The manuscript's headline comparison — "the TDM predicted 12,766 m², much closer to
observations (10,170 m²)" — becomes **18,103 m² (78 % over-prediction)** once training and
prediction use the same predictor. **This is a genuine bug, not a modelling choice.**

### 4.2 The 2030 projection is driven by the distance swap, not by snowmelt (F3)

Reviewer 1 asked how the 2030 distance was built; Reviewer 2 correctly observed the code
uses the 2021 distribution. I decomposed the published 4,387 m² of "newly suitable by 2030":

```
archived (snow 2021->2030 AND dist 2012->2021):
        2021 area 12,766.0   2030 area 14,474.1   newly suitable 4,386.6 m²
distance held FIXED at 2012, only snowmelt advances 2021->2030:
                             2030 area 12,190.4   newly suitable   533.7 m²
snowmelt held FIXED at 2021, only distance swapped 2012->2021:
                             area      14,888.9   newly suitable 3,937.9 m²
```

* Earlier snowmelt alone yields **534 m²** of newly suitable habitat and actually **shrinks**
  the total TDM suitable area (12,766 → 12,190 m²).
* Swapping the distance layer alone yields **3,938 m²** — i.e. **~90 % of the projected
  2030 expansion is the observed 2012–2021 expansion fed back in as a predictor**, not a
  climate signal.

The abstract ("Scenario-based projections under continued snowmelt advance suggested further
expansion of *Sasa* by 2030") and Results ("These reductions suggest shifts in habitat
suitability and *Sasa* distribution due to earlier snowmelt timing") attribute the
projection to snowmelt. For the TDM that attribution is not supported by the code.
(The TBM has no distance predictor, so its 27,049 m² *is* purely snowmelt-driven.)

### 4.3 Extrapolation

Training `dist` range: 1.0 – 142.1 m (99th pct 106.6). In the prediction domain V2 reaches
283 m and **4.4 % of predicted pixels lie beyond the training maximum**. By contrast the
snowmelt extrapolation is negligible: only 47 of 407,658 pixels (0.01 %) fall outside the
trained snow range in 2030 — so the *snow* extrapolation worry can be dismissed.

---

## 5. Ensemble composition (F: partly undocumented)

```r
readRDS("ortho/model_stack.rds")$member_fits |> names()
# maxent_1_02, maxent_1_14, maxent_1_03, xgb_1_02, xgb_1_04, xgb_1_08, xgb_1_10   (7)
readRDS("ortho/model_stack_wo_dist.rds")$member_fits |> names()
# maxent_1_17, maxent_1_12, xgb_1_05, xgb_1_09                                    (4)
```

Non-zero LASSO coefficients (penalty 0.1, mixture 1) and normalised weights:

**TDM** — maxent 17.2 %, xgboost 82.8 %
```
maxent_1_02 0.6808 (16.0%)  maxent_1_14 0.0374 (0.9%)  maxent_1_03 0.0153 (0.4%)
xgb_1_02    0.1527 ( 3.6%)  xgb_1_04    1.0110 (23.7%) xgb_1_08 1.0813 (25.4%) xgb_1_10 1.2821 (30.1%)
```
**TBM** — maxent 23.8 %, xgboost 76.2 %
```
maxent_1_17 0.1775 (6.0%)  maxent_1_12 0.5317 (17.8%)  xgb_1_05 1.9613 (65.8%)  xgb_1_09 0.3105 (10.4%)
```

**Random forest and GAM receive exactly zero weight in both ensembles.** The manuscript says
"We applied four classification algorithms" and shows all four in Fig. 5, which implies all
four contribute. They do not. Worth stating explicitly, especially as Results says "within
the TDM, GBT, MaxEnt, and RF outperformed GAM" — RF is in the figure but not in the model.

Tuned hyper-parameters of the surviving members (for the methods section):
```
TDM  maxent  fc=lqph  reg=1.509 | fc=lqph reg=1.868 | fc=lqpht reg=2.315
     xgb     mtry 2 trees 893  depth 3  lr 0.0310 loss_red 1.26e-4 stop_iter 17
             mtry 2 trees 1094 depth 12 lr 0.0238 loss_red 29.5   stop_iter 12
             mtry 4 trees 789  depth 10 lr 0.00152 loss_red 6.01  stop_iter 5
             mtry 5 trees 564  depth 1  lr 0.00361 loss_red 3.1e-8 stop_iter 13
TBM  maxent  fc=lqph reg=0.591 | fc=lqpht reg=1.985
     xgb     mtry 3 trees 94  depth 2 lr 0.1219 loss_red 1.46e-10 stop_iter 13
             mtry 4 trees 522 depth 3 lr 0.0748 loss_red 1.13     stop_iter 18
```

Number of tuned configurations actually evaluated (`grid = 18`):
```
models.rds          rf 8 configs | gam 1 | maxent 18 | xgb 18
models_wo_dist.rds  rf 7 configs | gam 1 | maxent 18 | xgb 18
```
RF only tunes `mtry`, so the grid saturates at 7–8. The manuscript's "up to 18 trials" is
fair, but "For all models except GAM, hyperparameters were tuned via grid search with up to
18 trials" understates that RF got 7–8 and its single tuned parameter was `mtry`.

Minor: `set_args(num.threads = 18)` is applied to the xgboost, maxnet and mgcv specs as
well as ranger. `num.threads` is a *ranger* argument; xgboost received it as an unknown
`num_threads` parameter and actually ran with `nthread = 1`
(`fit$params` shows `$num_threads 18` alongside `$nthread 1`). Harmless, but it means the
xgboost fits were single-threaded, and it is dead configuration in the published code.

### CV / evaluation metric
`workflow_map("tune_grid", metrics = metric_set(tss_max))` and
`blend_predictions(metric = metric_set(tss_max))`. Best CV TSS:
```
models.rds (TDM):        xgb 0.7053 | rf 0.6982 | maxent 0.6980 | gam 0.6882
models_wo_dist.rds (TBM): maxent 0.5768 | xgb 0.5741 | gam 0.5540 | rf 0.5369
```
Results text says "Within the TBM, GBT and MaxEnt performed best, while within the TDM,
GBT, MaxEnt, and RF outperformed GAM" — consistent with the above.

---

## 6. Variable importance (F6) — and the `"presense"` question, settled

### 6.1 The misspelled argument is a **no-op**. Retract the lead as a bug.

`predict_function_target_column` is consumed in exactly one place in the whole
DALEX/DALEXtra stack: inside `DALEXtra:::yhat.model_stack`, which is only reached when
`predict_function` is **not** supplied:

```r
yhat.model_stack <- function(X.model, newdata, ...) {
  ... response <- as.data.frame(predict(X.model, newdata, type="prob"))
  if (!is.null(attr(X.model,"predict_function_target_column")))
      return(response[, attr(X.model,"predict_function_target_column")])
  ...
}
```

Both scripts pass `predict_function = stacks::predict.model_stack` explicitly, so
`yhat.model_stack` is never called and the attribute is never read. Demonstrated on the real
TDM stack:

```
y_hat class (pf supplied, 'presense'): tbl_df,tbl,data.frame   -> a .pred_class column
identical(bad, good) : TRUE            # "presense" vs "presence"
identical(bad, none) : TRUE            # vs omitting the argument entirely
model_parts() dropout losses identical: TRUE
```

So the published importance figure is **not** computed on the wrong column. This lead is
**wrong** and should be retracted.

### 6.2 What *is* wrong: the explainer explains hard classes, not probabilities

Because `predict_function = stacks::predict.model_stack` is called with **no `type`
argument**, it returns `.pred_class` (a factor), not probabilities:

```r
stacks::predict.model_stack(ms, X[1:5,])
# A tibble: 5 x 1   .pred_class <fct>  absence absence absence absence absence
```

The custom loss then maps that factor to 0/1 and calls `tss_max_vec` on a binary vector, so
"TSS" is TSS at the implicit 0.5 cut-off, not the maximised TSS over thresholds that the
same script reports as the headline metric (`sdm_metric_set()` → `tss_max`). The importance
figure and the reported model performance therefore use two different definitions of TSS.
This should be stated, or the explainer should be given `type = "prob"`.

### 6.3 The two panels of Fig. 6 are not comparable (this is the real problem)

| | `sdm_tdm.R` (lines 176–200) | `sdm_tbm.R` (lines 169–190) |
|---|---|---|
| explainer | `DALEXtra::explain_tidymodels` | `DALEX::explain` |
| data | `df_train` (16,596 rows) | `df_test` (~5,600 rows) |
| `y` | from `df_train` | from `df_test` |
| `model_parts` | defaults `N = 1000`, `B = 10` | same |
| seed | **none** | **none** |

(The two explainer generics behave identically here once `predict_function` is supplied, so
the only substantive difference is train vs test.)

Reproduced (seed fixed only for my own runs):

```
TDM, TRAIN  (as in sdm_tdm.R)   dist .4634  snow .2107  elevation .0464  aspect .0165  TRI .0144  slope .0061  roughness .0048  TPI .0041
TDM, TEST                       dist .3680  snow .1419  elevation .0126  TPI .0122  TRI .0120  roughness .0101  aspect .0078  slope .0063
TBM, TEST   (as in sdm_tbm.R)   snow .1922  elevation .1467  TRI .0700  roughness .0360  aspect .0288  slope .0125  TPI -.0009
TBM, TRAIN                      snow .3922  elevation .1896  aspect .0715  TRI .0382  roughness .0270  slope .0145  TPI .0016
```

These reproduce `paper/files/vi.jpg` closely (TBM panel ≈ snow 0.195 / elevation 0.14 /
TRI 0.06 …; TDM panel ≈ dist 0.46 / snow 0.21 / elevation 0.055 …), confirming the published
figure is TBM-on-test beside TDM-on-train. Because the TBM importances are ~2× smaller on
test than on train, the visual impression that "distance dominates the TDM far more strongly
than snow dominates the TBM" is partly an artefact of the different data split.

The manuscript's *ordering* claims are nevertheless supported:
snow is top in the TBM, dist then snow in the TDM, in every run I did.

### 6.4 The published figure cannot be regenerated, and the repo copy differs from the paper copy

`model_parts()` uses `N = 1000` rows sampled at random and `B = 10` permutations, with no
`set.seed`. Three runs of the same TBM/test configuration:

```
seed 1: snow .2002 elevation .1599 TRI .0710 roughness .0413 aspect .0394 slope .0140 TPI -.0001
seed 2: snow .1955 elevation .1247 TRI .0602 roughness .0383 aspect .0326 slope .0137 TPI  .0015
seed 3: snow .2031 elevation .1228 TRI .0529 roughness .0353 aspect .0262 slope .0117 TPI -.0001
```

The low-importance ranks (TRI / roughness / aspect) shuffle between runs. Consistently,
`ortho/figures/vi_tbm.png` (order snow, elevation, TRI, **roughness, aspect**, slope, TPI;
y-axis 0–0.6 from the script's `ylim(c(0,0.6))`) is **not** the figure in the paper —
`paper/files/vi_tbm.jpg` has order snow, elevation, TRI, **aspect, roughness**, slope, TPI
and a free y-axis to 0.25 with the TPI box straddling 0, which `ylim(c(0,0.6))` would have
clipped. So the manuscript figure was produced by a run of a *different* version of the
script than the one committed, and neither can be reproduced.

---

## 7. Statistical properties of the predictors (F9, F10)

Computed on `ms$train` itself (the real modelling frame), not a fresh raster sample.

**TDM (n = 16,596) Pearson**
```
          aspect roughness  slope elevation    TPI    TRI   snow   dist
aspect     1.000     0.014  0.042     0.111 -0.053 -0.033 -0.162 -0.087
roughness  0.014     1.000  0.932     0.207 -0.006  0.970 -0.355  0.281
slope      0.042     0.932  1.000     0.229 -0.015  0.943 -0.365  0.216
elevation  0.111     0.207  0.229     1.000 -0.007  0.197 -0.067  0.410
TPI       -0.053    -0.006 -0.015    -0.007  1.000  0.012 -0.334 -0.087
TRI       -0.033     0.970  0.943     0.197  0.012  1.000 -0.359  0.309
snow      -0.162    -0.355 -0.365    -0.067 -0.334 -0.359  1.000  0.116
dist      -0.087     0.281  0.216     0.410 -0.087  0.309  0.116  1.000
```
**VIF** (logistic regression on the same frame):
```
TDM:  TRI 25.55  roughness 16.19  slope 11.20  snow 1.37  TPI 1.26  elevation 1.09  aspect 1.09  dist 1.03
TBM:  TRI 25.45  roughness 17.60  slope 10.65  snow 1.49  TPI 1.21  aspect 1.10  elevation 1.08
```

**Redundant set: {roughness, slope, TRI}** — pairwise |r| 0.93–0.97, all VIF > 10. One of
them should be kept (TRI or slope), the other two dropped. `elevation`, `TPI`, `snow`,
`aspect`, `dist` are all fine (VIF < 1.5). Note this is exactly what Reviewer 2 asked for,
and note that `models_all_5m.rds` (an unused archived experiment) already drops roughness
and TRI — the predictor set there is `slope, snow, dist, elevation, aspect, TPI`.

Permutation importance is precisely the method that Reviewer 2 says becomes unreliable under
this kind of collinearity: TRI/roughness/slope split their shared signal three ways, which is
why they sit at 0.005–0.07 and shuffle rank between runs (§6.4).

### `aspect` is circular but treated as linear (F10)

`aspect` is in raw degrees, range 0.38–359.2 in the training frame, no sin/cos
decomposition anywhere in the repo. The MaxEnt members fit hinge features **on the raw
degree scale**; e.g. `default_maxent_1_02` has 7 non-zero aspect terms including
`hinge(aspect):351.909:359.224  beta = +0.180` and
`hinge(aspect):242.187:359.224  beta = −0.147`. XGBoost can only split the axis, never wrap it.

Consequence, measured on the archived ensembles by sweeping aspect 0→360° with all other
predictors at their training median:

```
TBM:  HS(359°) = 0.5505   HS(360°) = 0.5505   HS(0°) = 0.4966   -> discontinuity 0.0538
      full range of HS over aspect = 0.4958 .. 0.6330 (span 0.137)
TDM:  discontinuity 0.0029 (span 0.004)  -- swamped by dist
```

For the TBM the artificial seam at due north is **39 % of the entire aspect effect**, and it
crosses the 0.5 cut-off that defines "suitable" and "risky": a 359°-facing pixel is called
suitable, an otherwise identical 1°-facing pixel is not. This is exposed to any reviewer who
looks at the code. It matters specifically for the MaxEnt and XGBoost members, which carry
100 % of the ensemble weight.

---

## 8. Thresholds and magic numbers (F: mostly undocumented)

| Value | Where | Provenance | Documented in MS? |
|---|---|---|---|
| `elevation < 2560` | tdm 68, tbm 59 | ad hoc | **Yes** — "areas above 2,560 m—where *Sasa* does not occur—were excluded". But 71 of 10,176 (0.7 %) 2021 *Sasa* pixels are above 2,560 m and *Sasa* reaches 2,711 m in the map. Also excludes 59 % of the terrain domain (1,296,195 of 3,156,718 non-NA pixels remain) |
| `terra::aggregate(fact = 5)` for `sampling_mask` | tdm 50, tbm 41 | 5 m absence grid | **Yes** — "absence data were downsampled to 5 m resolution" |
| `dist > 0` | tdm 69 | drops 2012-*Sasa* pixels | **Yes** — "Target variable" paragraph |
| `prop = 0.2` | tdm 73, tbm 63 | `tidysdm::spatial_initial_split` sets `v = round(1/prop) = 5` and picks one of 5 spatial folds as the test set → ~20 % test | **Yes** (80/20). Realised: TDM 16,572/4,319 = 79.3/20.7 |
| `v = 4` | tdm 94, tbm 83 | fourfold spatial block CV | **Yes** |
| block grid | *implicit* | `spatial_block_cv` default 10×10 grid over the bbox → ~117 m × 114 m blocks (TDM train bbox 732744.5–733916.5 × 4050651–4051796) | **No** — no block size stated anywhere |
| `grid = 18` | tdm 136, tbm 125 | tuning grid size | **Yes** ("up to 18 trials") |
| `set.seed(1)` ×2 | tdm 71/92, tbm 61/81 | before split and CV only | **No** |
| **no seed before `thin_by_cell`** | tdm 61–67, tbm 52–58 | `tidysdm:::thin_by_cell` does `data[sample(seq_len(nrow(data))), ]` before de-duplicating by cell — **stochastic** | **No**. Consequence: the archived training set cannot be regenerated (F8). My reconstruction gave TDM 20,891 rows vs the archive's implied ~20,9xx; presences match exactly, absences differ by ~20–40 rows |
| `area > units::set_units(5, m^2)` | tdm 234/243, tbm 248 | patch-size filter for the distance source and for the black outlines on every map | **No, nowhere in the manuscript.** Drops 975/1,171 (83 %) of 2012 polygons and 1,477 m² of source area; drops 2021 10,176 → 8,328 m² |
| `HS > 0.5` | tdm 370/418/424/434, tbm 340/387/392/399/406 | default probability cut-off | Stated as the definition of suitable/risky, **not justified**; no sensitivity analysis (Reviewer 1 #5) |
| `num.threads = 18` | tdm 110–122, tbm 99 | dev machine had ≥18 cores | n/a — dead for xgb/maxent/gam (§5) |
| `midpoint = 0.5` in the colour scales | both | cosmetic | n/a |

**Denominator inconsistency in the published percentages.** "of the areas occupied by *Sasa*
in 2021, TBM predicted 2,257 m² (21 %) to become unsuitable by 2030, and TDM 717 m² (7 %)".
The numerators are computed over `sasa_pol_21`, i.e. the **5 m²-filtered** polygons, whose
total area is **8,328.5 m²** (8,333 rasterised cells) — not the 10,170 m² quoted elsewhere.
Against the correct denominator the figures are **27.1 %** (TBM) and **8.6 %** (TDM);
against 10,170 m² they are 22.2 % and 7.0 %. The published "21 %" matches neither.

---

## 9. Prediction rasters: extents, valid-pixel counts, reproduction (F: domain mismatch)

```
file                                   dim        crs   n_valid   min       max      n>0.5
data/sasa_pred_sdm_21.tiff  (TBM 2021)  1753x1801  3099  397,403  0.1601  0.7395   47,279
data/sasa_pred_sdm_30.tiff  (TBM 2030)  1753x1801  3099  407,658  0.1599  0.7397   69,214
data/sasa_pred_tdm_21.tiff  (TDM 2021)  1753x1801  3099  397,403  0.0856  0.6425   12,773
data/sasa_pred_tdm_30.tiff  (TDM 2030)  1753x1801  3099  407,658  0.0857  0.6401   14,482
risky_area_wo_dist.tiff     (Fig. 9)    1147x1213  3099   36,797  0.5000  0.7380   36,797
risky_area_tdm.tiff                     1147x1213  3099    3,149  0.5001  0.6401    3,149
```

**All four prediction rasters and both risky-area rasters were regenerated from the archived
stacks and match bit-for-bit** (max abs difference 4.5e-08 to 5.9e-08, i.e. float32 rounding;
risky areas max abs difference exactly 0). So the artefacts are genuinely the product of the
archived models, and the audit pipeline is faithful.

**The 2021 and 2030 layers cover different domains: 397,403 vs 407,658 valid pixels
(+10,255, +2.6 %).** Cause: `data/snow/fitted_2021.tiff` is 1703×1801 (50 rows short at the
top) *and* has 27,849 more NA pixels than `fitted_2030.tiff` scattered through the upper half
of the scene; 10,255 of those fall inside the modelling domain. The difference is entirely a
snow-preprocessing artefact, not an SDM one. Practical effect: the published *maps* for 2030
show ground that the 2021 map does not, but the "newly suitable" and "becoming unsuitable"
statistics are computed with `c(pred30, pred21) %>% filter(...)`, which drops any pixel that
is NA in either layer, so **those numbers are not biased by the mismatch**.

Raster CRS is EPSG:3099 (see §1) even though the vegetation maps they are overlaid on are
EPSG:6690.

### Manuscript numbers, recomputed

```
TBM 2021 suitable  47,253.3 m²   (MS: 47,253)  ✓
TDM 2021 suitable  12,766.0 m²   (MS: 12,766)  ✓
TBM newly suitable 2030   27,049.3 m²  (MS: 27,049; 57 % of 47,253 ✓)
TDM newly suitable 2030    4,386.6 m²  (MS:  4,387; 34 % of 12,766 ✓)
TBM becoming unsuitable    2,257.8 m²  (MS:  2,257) ✓ but see the denominator issue in §8
TDM becoming unsuitable      716.6 m²  (MS:    717) ✓
```
All headline HSM areas in the Results reproduce exactly.

---

## 10. Test TSS reproduced (F: reproduces for TBM, ~0.02 low for TDM)

`ms$train` *is* `df_train`, so the test set is `df_21 \ df_train`. Test presences are exactly
recoverable (they are deterministic); test absences are the thinned representatives of the
5 m cells not represented in `df_train` — the *cell set* is exact, only the 1 m point within
each cell is unknown (F8). Five independent reconstructions:

```
TBM (published tss_score_tbm.csv: boyce 0.9916  roc_auc 0.8369  tss_max 0.5556)
 draw n_abs    n  boyce  roc_auc  tss_max
    1  3545 5699 0.9885   0.8392   0.5586
    2  3536 5690 0.9932   0.8374   0.5539
    3  3535 5689 0.9927   0.8390   0.5524
    4  3533 5687 0.9902   0.8398   0.5622
    5  3534 5688 0.9931   0.8388   0.5542
 -> published 0.5556 sits inside the reconstructed range 0.552-0.562.  REPRODUCES.

TDM (published tss_tdm.csv: boyce 0.9246  roc_auc 0.9146  tss_max 0.7010)
 draw n_abs    n  boyce  roc_auc  tss_max
    1  3575 4425 0.9260   0.9018   0.6791
    2  3580 4430 0.9459   0.8995   0.6759
    3  3585 4435 0.9215   0.9005   0.6820
    4  3581 4431 0.9513   0.9008   0.6768
    5  3582 4432 0.9098   0.9011   0.6792
 -> published 0.7010 is ~0.022 above the reconstructed range 0.676-0.682.
```

Manuscript reports "0.55 for the TBM and 0.70 for the TDM" — consistent with the CSVs.
The TDM's 0.70 is at the optimistic end of what re-draws of the unseeded absence sample
produce; a defensible restatement is 0.68 ± 0.01.

Training-set metrics for reference (overfitting check):
```
TDM on its own training set: boyce 0.9683  roc_auc 0.9453  tss_max 0.7606
TBM on its own training set: boyce 0.9995  roc_auc 0.8975  tss_max 0.6369
```
Train–test gaps of 0.06–0.08 TSS — modest, spatial blocking is doing its job.

**Inconsistent archived scalar:** `ortho/tss_score_tbm.rds` contains
`boyce 0.99703, roc_auc 0.83902, tss_max 0.53484` — a *different* run from
`ortho/tss_score_tbm.csv` (`0.99165 / 0.83691 / 0.55559`). No line in either script writes
the `.rds`. The manuscript's 0.55 matches the CSV.

---

## 11. `sdm_tbm.R` as written (F5) and orphan artefacts

**Algorithms.** `sdm_tbm.R` lines 96–120 have GAM, MaxEnt and XGB **commented out**, along
with the `update_workflow_model("default_gam", …, formula = gam_formula(rec))` call. As
committed the script trains **RF only**. The archived `models_wo_dist.rds` contains all four
(`default_rf`, `default_gam`, `default_maxent`, `default_xgb`), and line 132's
`scale_color_discrete(labels = c("GBT","GAM","MaxEnt","RF"))` assumes four. The committed
file is therefore not the version that produced the published TBM. `sdm_tdm.R` does train all
four.

**Files referenced that do not exist:**
```
sdm_tbm.R:127  saveRDS(models, "models_wo_dist_twi.rds")
sdm_tbm.R:129  models <- readRDS("models_wo_dist_twi.rds")
```
`ls ortho/*.rds` → `model_stack.rds, model_stack_wo_dist.rds, models.rds, models_all_5m.rds,
models_wo_dist.rds, tss_score_tbm.rds`. **No `models_wo_dist_twi.rds`.** No line in either
script produces `models_wo_dist.rds`, which is the file the published TBM stack came from.
(The `_twi` suffix is further evidence that a TWI-containing variant was being experimented
with at some point.)

Everything else read by the two scripts exists: `data/vege_20{12,21}_5x5.tiff`,
`data/terrain_features/*.tif`, `data/snow/fitted_20{12,21,30}.tiff`, `data/dem_small.tiff`,
`data/sasa_inc.tiff`.

**Artefacts in `ortho/` that no committed script produces** (dead outputs of code that no
longer exists — flag before archiving to Zenodo/Dryad):
```
models_all_5m.rds                 workflow_set on 6 predictors: slope,snow,dist,elevation,aspect,TPI
                                  (i.e. an already-de-collinearised variant; n = 17,991)
tss_score_tbm.rds                 different run from tss_score_tbm.csv
risky_area.tiff                   layer named "risk", 10,287 valid px, range 0.200-0.823
potential_sasa_area_21.tiff       17,674 valid px, all >= 0.5
data/sasa_pred_sdm_12.tiff        1157x1213, referenced only by analyse_sdm.R
data/sasa_pred_sdm_dist_21.tiff   1753x1801, 28,910 px > 0.5  (an earlier TDM)
data/sasa_pred_sdm_dist_30.tiff   1753x1801, 34,319 px > 0.5
data/sasa_pred_tdm_30_bin.tiff    written by sdm_tdm.R:443 (no overwrite=TRUE -> would error on rerun)
```

**Dead code inside the scripts:**
* `sdm_tdm.R:36-37` `sasa12_ras %>% terra::as.polygons() %>% filter()` — result discarded.
* `sdm_tdm.R:278-280` `is_others <- vege12 %>% mutate(is_others = ifelse(layer == 6, 1, 0))`
  — never used downstream. (Also mislabelled: class 6 is Montane Alder; "Other Vegetation"
  is class 2, which is what the risky-area filter correctly uses.)
* `sdm_tbm.R:240-241` `lyr_names` — never used.
* `sdm_tbm.R:412-488` exploratory violin/HS-change block; line 431's
  `aspect = if_else(aspect < -180, aspect + 360, aspect)` is a no-op on 0–360° data and
  implies the aspect raster was once −180…180.
* `sdm_tbm.R:441` a stray `scale_color_brewer(...)` after the `ggplot` chain has been
  terminated by `facet_wrap(...)` — the last three layers are evaluated and discarded.
* `sdm_tbm.R:474-476` `snow_21 %>% rename(snow_12 = snow) %>% c(snow_21)` — labels the
  2021 layer as "snow_12"; the plotted "2012" panel is actually 2021.
* Both scripts open with `setwd("~/doctoral_thesis/chap2/ortho/")`, a path that does not
  exist. (`analyse_sdm.R` uses a third root, `~/Projects/jasms2023f/ortho/`.)

---

## 12. Manuscript/figure defects found while checking the above

* **Figure 8 shows the wrong maps (F12).** `paper/results.qmd` builds `fig-future-prediction`
  from `hsmap_tbm_2021.jpg` + `hsmap_tdm_2021.jpg` in the top row, while the caption reads
  "Habitat suitability (HS) maps for 2030 and differences from the 2021 HS maps". The
  rendered `paper/files/future.jpg` visibly carries the titles "Habitat Suitability map of
  Sasa (TBM, **2021**)" / "(TDM, **2021**)". The same two 2021 panels are also Figure 7.
  `hsmap_tbm_2030.jpg` and `hsmap_tdm_2030.jpg` exist in `paper/files/` but are never used.
* The two panels of Figure 7 use different colour-scale ranges (TBM 0.2–0.7, TDM 0.1–0.6)
  and carry duplicate legends — exactly Reviewer 1's and Reviewer 3's complaint.
* `fig-spatial-split` shows `initial_split_dist.jpg` + `cv_dist.jpg`, i.e. the **TDM** split
  only, presented as the split for both models. The TBM has its own
  (`initial_split_wo_dist.png`, `cv_wo_dist.png`) and its `df_21` is 30 % larger, so the
  blocks are not the same.
* The manuscript never states which algorithms survived the LASSO blend (§5), the spatial
  block size (§8), the 5 m² patch filter (§8), or that the 2030 distance layer was rebuilt
  from the 2021 distribution (§4.2).

---

## 13. Leads from `known_leads.md` — verdicts

| Lead | Verdict |
|---|---|
| terrain glob now picks up `twi.tif`; fitted models may not contain it | **CONFIRMED.** Fitted models have no `twi`; a clean re-run would add it (§1) |
| `preprocess_snow_data.R` injects extra predictors into `terrain_features/` | **CONFIRMED** in code (lines 67/96/137); the three files currently live in `data/snow/`, so the injection has not happened in the present tree |
| `sdm_tbm.R` may train only RF and save/load `models_wo_dist_twi.rds` | **CONFIRMED** (§11) |
| TDM training distance ≠ prediction distance | **CONFIRMED and quantified** — 85 % of pixels differ, mean 18.6 m; suitable area 12,766 → 18,103 m² (§4.1) |
| VI computed on different data / different explainers | **CONFIRMED** (§6.3) |
| `predict_function_target_column = "presense"` silently changed what is explained | **WRONG — RETRACTED.** The argument is never read because `predict_function` is supplied; results are bit-identical with the correct spelling (§6.1). The real defect is that the explainer sees hard classes, not probabilities (§6.2) |
| `aspect` treated as linear, no sin/cos | **CONFIRMED and quantified** — 0.054 HS discontinuity at the 0°/360° seam in the TBM (§7) |
| Ensembles retain only MaxEnt + XGBoost (TDM 7, TBM 4); RF and GAM get zero weight | **CONFIRMED from `member_fits`** (§5) |
| risky area = `vege21 == 2` ∧ HS2030 > 0.5; Fig. 9 is the TBM version | **CONFIRMED** — regenerated `risky_area_wo_dist.tiff` exactly (36,797 px, 36,777 m²) |
| 2021 vs 2030 rasters have different valid-pixel counts (~397k vs ~408k) | **CONFIRMED**: 397,403 vs 407,658 (+10,255). Cause identified (snow 2021 layer); the published areal statistics are *not* biased by it (§9) |
| Fig. 7's two panels use different colour ranges | **CONFIRMED** (§12) |
| `is_others` uses `layer == 6`, contradicting the class codes, and is dead | **CONFIRMED dead**; class 6 is Montane Alder (24,943 px in 2021), "Other Vegetation" is class 2 (451,770 px) |
| Prior VIF numbers (TRI 34.3, roughness 19.5, slope 12.9, twi 1.91 on a 200k-pixel sample) | **Directionally right, numerically superseded.** On the actual modelling frame: TRI 25.5, roughness 16.2, slope 11.2, snow 1.37, TPI 1.26, elevation 1.09, aspect 1.09, dist 1.03. `twi` is not in the models so it has no VIF there |

---

## 14. Recommended actions, in priority order

1. **Re-run the TDM 2021 prediction with the training-consistent distance layer (V1)**, or
   retrain on V2. Either way the 12,766 m² figure and the "TDM closely matches observation"
   claim in Results and in the Fig. 7 caption must be revised (18,103 m² with V1).
2. **State explicitly in Methods, in the workflow diagram, and in the abstract** that the
   2030 TDM distance layer is rebuilt from the observed 2021 distribution, and report the
   decomposition in §4.2 (534 m² from snowmelt, 3,938 m² from the distance update). Both
   Reviewer 1 (#5) and Reviewer 2 asked for exactly this; the honest answer changes the
   interpretation of the 2030 projection.
3. **Seed everything** (`set.seed` before `thin_by_cell`, before `model_parts`, before
   `spatial_initial_split`) and pin package versions (`renv`). Re-export the xgboost boosters
   with `xgb.save`/`xgb.save.raw` so the archive survives an xgboost major version.
4. **Fix the CRS**: reproject `terrain_features/*.tif` to EPSG:6690 once, up front. This also
   makes the scripts runnable again.
5. **Drop two of {roughness, slope, TRI}** and refit; report the correlation matrix and VIFs
   (Reviewer 2). `models_all_5m.rds` shows this was already tried.
6. **Decompose `aspect` into northness/eastness** (`cos`/`sin`) or drop it.
7. **Recompute both variable-importance panels on the same split** (test set for both, or
   train for both), with a seed and a larger `N`, and with `type = "prob"` so the loss is the
   same TSS that is reported elsewhere.
8. **Document** the 5 m² patch filter, the 10×10 spatial block grid, the seeds, and justify
   or sensitivity-test the 0.5 cut-off (Reviewer 1 #5).
9. **Fix Figure 8** to use `hsmap_*_2030.jpg`; unify the colour scales in Figure 7; fix the
   21 % / 22 % / 27 % denominator inconsistency.
10. **Remove `twi.tif`** from `terrain_features/` (or add it to the model and the paper), and
    change the predictor glob to an explicit `c("elevation","slope",...)` list so the model
    can never silently change when a file is dropped into the directory. Delete or clearly
    label the orphan artefacts listed in §11 before depositing to a permanent repository
    (the AE requires one).
