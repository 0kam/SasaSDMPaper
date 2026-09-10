# Independent verification of `restore_assets.md` (recovered-assets investigation)

Verifier run: 2026-08-10. Everything below was re-executed from scratch on this machine.
Scripts and logs under
`/private/tmp/claude-501/-Users-okamoto-NIES-SasaSDMPaper--claude-worktrees-review-summary-ece-ddc69c/98ac7ecf-eafb-49c6-aa2d-9aab2998c13e/scratchpad/v/`.
All R invocations used `GDAL_PAM_ENABLED=NO` so that nothing was written into the read-only archive.

Headline: **10 of 12 findings survive; 2 mechanisms are wrong and 1 proposed action is redundant.**
The single most consequential correction is that the georectification pipeline's residual
disagreement is **not** caused by `st_rasterize`, which is bit-exact, but by an **unseeded
random tie-break** in the gap-fill — which also means the published vegetation rasters
(and the snowmelt predictor rasters) are not bit-reproducible at all.

---

## 1. Verdict table

| id | verdict | note |
|---|---|---|
| A4-TDM-DOMAIN | CONFIRMED | every number reproduces exactly; **but the proposed Methods action is redundant** |
| A1-COLONISATION-WEAK | CONFIRMED | every number reproduces exactly |
| A2-NOT-A-COMMUNITY-MAP | CONFIRMED | every number reproduces exactly |
| A1-DRAFT-STATUS | CONFIRMED | every number reproduces exactly |
| A1-PRED-VINTAGE | CONFIRMED | incl. the 12,766 m² geodesic match |
| A1-ARCHIVE-IDENTITY | CONFIRMED | |
| ENV-XGBOOST-PIN | CONFIRMED | one quoted prediction value is newdata-dependent and differs |
| A1-COMMON-DOMAIN | PARTLY_CONFIRMED | six metrics reproduce exactly, but **`dist` is not defined as in the published TDM**; under the paper's own definition the numbers move a lot |
| A3-CHAIN-CLOSED | PARTLY_CONFIRMED | `.npy`↔CSV identity exact; "99.83 %" is a stochastic draw, not a property |
| A3-TRANSITION-STABLE | PARTLY_CONFIRMED | matrix reproduces; **the explanation of the `*_masked.npy` discrepancy is wrong** |
| A2-SCRIPT-BROKEN | PARTLY_CONFIRMED | two independent blockers; the first one fires before line 50 |
| A3-RASTERIZE-LASTWINS | REFUTED (mechanism) | `st_rasterize` is last-point-wins **and 100.000000 % exact**; it is not the source of the disagreement and is not the nondeterministic step |

---

## 2. What reproduced exactly

### A4-TDM-DOMAIN — the `filter(dist > 0)` diagnosis

`Rscript review/restore/assets/domain.R`:

```
cells with ALL covariates + response (i.e. after drop_na): 1159498
after elevation < 2560: 390065   presences (2021 Sasa): 9974
  of those presences, dist==0 (i.e. ALSO Sasa in 2012): 6001  (60.2%)
TDM domain after filter(dist>0): 381704   presences: 3973   absences: 377731
TBM domain (no dist filter)   : 390065   presences: 9974   absences: 380091
=> filter(dist>0) removes 6001 presences and 2360 absences
prevalence TBM: 0.0256   TDM: 0.0104
```

Archived frames corroborate: `trainingdata_models.rds` n=16,596 (3,123 presence / 13,473 absence,
`dist==0` rows 0, min dist 1); `trainingdata_models_wo_dist.rds` n=21,389 (7,820 / 13,569).
Source confirmed: `sdm_include_distance.R:44-47` builds `sasa_dist` from **all** 2012 Sasa cells,
`:68-69` applies `filter(elevation < 2560) %>% filter(dist > 0)`; `sdm.R:59` has only the
elevation filter. **CONFIRMED.**

> **But**: the proposed action ("State in Methods that the TDM training set excludes 2012 Sasa
> cells") is already done. `paper/matmet.qmd:81` and the *submitted* `paper/submit_files/index.tex:562-566`
> both read: "pixels located directly within the 2012 distribution (i.e., at 0 m distance) were
> excluded from the TDM. Accordingly, the TDM was designed to predict newly colonized locations".
> Reviewer 2 read that and objected anyway. Writing it again would read as evasive. What is new
> and worth reporting is the **quantification** (6,001 / 9,974 = 60.2 %; prevalence 0.0256 → 0.0104)
> and the fact that the manuscript still draws attributive conclusions from the pair.

### A1-COLONISATION-WEAK — the revived `sdm_sasainc.R`

Data step reproduced exactly: 4,097 binary colonisation cells; domain 24,912 cells; n=1,411 after
thinning; mean 0.04417, sd 0.05482; `filter_collinear` kept `roughness, snow, twi, elevation, TPI,
sasa_inc, aspect` (i.e. the response is in the screen; `slope` and `TRI` were dropped).

```
NULL (fold-train mean) : RMSE=0.05488  R2=-0.0030
RF, all 6 predictors  : RMSE=0.05376  R2=0.0374  cor=0.2374  (cor^2=0.0564)
   drop twi           : RMSE=0.05355 (delta -0.00021)  R2=0.0450
   drop TPI           : RMSE=0.05351 (delta -0.00026)  R2=0.0465
   only <any single>  : RMSE 0.06327-0.06438  R2 -0.333 to -0.380
RF random CV          : RMSE=0.04563  R2=0.3067
```

Every figure matches. RMSE improvement over the null is 2.04 %; 1 − 0.0374/0.3067 = 87.8 % of the
random-CV skill is spatial autocorrelation. **CONFIRMED, including the unfavourable direction.**

### A2-NOT-A-COMMUNITY-MAP

```
non-NA cells: 5159
vege_2012 under selected_comms:  1:4118  2:470  3:1  4:78  5:30  6:3  7:435  NaN:24     (79.82 % class 1)
polygons >= 5 m2: 229  total area: 7239
polygons retained (AOI): 71  distinct areas: 51  total area: 5211
archived distinct values: 5,6,8,...,216,341,445,674
repro polygon areas    : 5,6,8,...,216,341,445,674     [identical]
risky_area_wo_dist.tiff 36797 cells -> vege21 class 2: 36797 (100 %)
risky_area_tdm.tiff      3149 cells -> class 2: 3149 (100 %)
risky_area.tiff (server)10148 cells -> class 2: 10148 (100 %)
```

**CONFIRMED.** `selected_comms.tiff` feeds only `sdm_sasainc.R`, `analyse_sdm.R` and
`analyse_chamges.R` — never the published TDM (grep across `ortho/`, `data_from_server/ortho/`).

### A1-DRAFT-STATUS, A1-PRED-VINTAGE, A1-ARCHIVE-IDENTITY, ENV-XGBOOST-PIN

- `sdm_sasainc.R`: 37 expressions, last is `c(sasa_2012, )`, `eval` → "オブジェクト 'sasa_2012' がありません";
  `grep -cE 'writeRaster|saveRDS|write_csv|ggsave|set\.seed'` → **0**. **CONFIRMED.**
- `pred_sdm_dist_21 vs pred_tdm_21: n=397403 cor=0.8247 meandiff=-0.0043 |d|>0.05 63.7 % binary agree 96.58 %; >0.5 archive=26343 published=12773`
  and `pred_sdm_dist_30`: `cor=0.8170 |d|>0.05 65.2 %; >0.5 archive=31360 published=14482`.
  `expanse()` of the published `sasa_pred_tdm_21 > 0.5` = **12,766 m²** (archive vintage would be 26,329 m²).
  **CONFIRMED.**
- `sasa_inc.tiff` = `(v21==1) & (v12!=1)` at 100.000000 % of 1,206,233 cells, n=4,097.
  Server `risky_area.tiff` = `p30 − p21` masked, max abs diff 5.96e-08, 100 % class 2,
  `p30>0.5` in 10,148/10,148. The two `.qgz` are QGIS workspaces whose layer sources are
  `../../../ダウンロード/*.tiff` plus a GSI XYZ basemap. **CONFIRMED.**
- xgboost: 3.2.1.1 → both stacks FAIL; **1.7.7.1 → both OK**; 1.7.6.1 → both OK with the
  "older XGBoost … Booster.save_model" serialization warning. **Pin CONFIRMED.**
  (The report's quoted `model_stack_wo_dist` first prediction 0.27806 differs from my 0.268384 —
  the value depends on which `newdata` row is used and the report does not say. Immaterial.)

---

## 3. Corrections

### 3.1 A3-RASTERIZE-LASTWINS is wrong about the mechanism — **REFUTED**

The report says `st_rasterize`'s last-point-wins "is the sole source of the 0.17 % residual
disagreement and is a non-deterministic step". Both halves are wrong.

I saved the raster **before** the focal pass and compared it to the published product:

```
PRE-FOCAL 2012: cells rasterized=1065641  disagreements vs published=0  (agreement 100.000000%)
   cells NA pre-focal but non-NA published (gap-filled): 140592
   cells non-NA pre-focal but NA published: 0
PRE-FOCAL 2021: cells rasterized=1065641  disagreements vs published=0  (agreement 100.000000%)
   cells NA pre-focal but non-NA published (gap-filled): 140592
```

`st_rasterize` is **bit-exact** — zero disagreements out of 1,065,641 cells, both years — and it *is*
deterministic (last-point-wins, verified directly: four points in one cell in orders 1,2,3,4 /
4,3,2,1 / 2,4,1,3 rasterize to 4 / 1 / 3).

The whole residual lives in the 140,592 cells (11.66 % of the map) filled by the single
`terra::focal(3, terra::modal, na.policy="only")` pass, and **that** is the stochastic step:
`modal` breaks ties at random and `georectify.R` sets no seed. Demonstration on a synthetic
raster with 100 exact 4-vs-4 ties: 30 identical `focal` calls → **30 distinct outcomes**;
identical under a fixed `set.seed(1)`.

Consequence, measured on the real data — 12 independent unseeded runs of the gap-fill:

```
        sasa12  sasa21  gain  loss   net  dp_share
mean    8553.0 10176.2  4094  2470.8 1623.2  46.860
sd         5.3     9.7   9.8     4.8   13.0   0.149
range 8545-8560 10159-10196 4081-4116 2465-2478 1603-1651 46.69-47.12
PUBLISHED 8547   10176   4097   2468   1629   46.840
```

Every published figure lies **inside** the Monte-Carlo range. This is a *stronger* reproducibility
result than "99.83 % of pixels agree": the deterministic part of the pipeline is exact, and the
published product is reproducible up to a documented ±~10 px stochastic tolerance.
The report's own 8,551 / 10,172 and my 8,554 / 10,167 are simply two draws from this distribution.

**Additional, previously unreported:** the same unseeded `interpolate()` is applied at
`georectify.R:62-72` to `data/snow/aligned/*.csv` → `data/snow/raw/*.tiff`. All ten snow raw
rasters have exactly 1,206,233 non-NA cells, so the same 140,592 cells are modal-filled there too —
on *continuous* snowmelt DOY, where an 8-distinct-neighbour cell is an 8-way tie and `modal` picks a
uniformly random neighbour. At those cells the real 3×3 neighbourhood range of snowmelt DOY has
median 3 d, mean 5.5 d, q90 14 d. That is the same order as the ~6.4 d the paper's own
−0.7146 d/yr trend produces over nine years, and it compounds the already-documented half-pixel
shift in `fitted_*.tiff`.

**Also unreported, and relevant to the Methods sentence the report wants written:** the
rasterization discards ~90 % of the classified image. Mean 11.10 points per 1 m cell (median 6,
p90 21, max 11,707), and only the last one in `georectified.csv` order survives. A majority vote
would assign a different class to **4.33 % / 4.35 %** of cells (2012 / 2021), changing pre-focal
Sasa from 8,043 → 7,925 and 9,622 → 9,554 — though the *net* change is barely affected
(+1,579 last-point vs +1,629 majority). 1.1 % of cells have no strict majority at all.

### 3.2 A1-COMMON-DOMAIN: the distance variable is not the paper's — **PARTLY_CONFIRMED**

All six metrics reproduce to four decimals (frames n=20,630 prevalence 0.193; n=7,098 prevalence
0.845; colonisation 0.7733/0.8713/0.8218, persistence 0.6725/0.7725/0.7168). **But** the evidence
line "Distance defined exactly as in `sdm_include_distance.R` (196 polygons > 5 m2)" is false.
`sdm_include_distance.R:44-47` is

```r
sasa_dist <- sasa12_ras %>% filter(sasa == 1) %>% distance() %>% rename(dist = sasa)
```

— distance to **every** 2012 Sasa cell, no area filter anywhere. The >5 m² polygon construction
appears in `sdm.R:243-250` only to draw outlines with `geom_spatvector`, and in
`select_sasa_communities.R` (with an extra AOI step). The two rasters differ substantially:
cor 0.954, mean |A−B| = 67.8 m, max 342.8 m.

Re-running the colonisation comparison on the identical frame under **both** definitions:

```
environment only                         | AUC = 0.7733  TSS_max = 0.4311
environment + dist (PUBLISHED defn)      | AUC = 0.9083  TSS_max = 0.6868
dist only          (PUBLISHED defn)      | AUC = 0.8886  TSS_max = 0.6576
environment + dist (polygons>5m2)        | AUC = 0.8713  TSS_max = 0.5881
dist only          (polygons>5m2)        | AUC = 0.8218  TSS_max = 0.5265
```

The qualitative conclusion is unchanged and in fact strengthened (distance ≫ environment). But the
figures the report proposes to print in a six-row table are those of a distance variable the paper
does not use, and the specific rhetorical parallel — "the TBM-to-TDM gain survives at +0.157 TSS vs
the published +0.145" — does not survive: under the paper's own definition the gain is **+0.2557**.
Under the published definition, adding environment to distance buys only +0.0197 AUC, so the
environmental-suitability narrative is *more* demoted, not less. Rerun before publishing.

(The `+0.157 vs +0.145` comparison is in any case incoherent with the report's own A4 finding,
which says the published +0.145 is not interpretable. And the two TSS are different estimators —
pooled OOF TSS_max here vs held-out test `sdm_metric_set()` there.)

### 3.3 A3-TRANSITION-STABLE: the `*_masked.npy` explanation is wrong

The transition-matrix reproduction is **confirmed** (published 8547/10176/4097/2468/+1629/46.84 %;
from the investigator's rasters 8551/10172/4089/2468/+1621/46.90 %; and see the Monte Carlo above).

But the stated reason for the prior audit's "+1.0 % vs +19.1 %" — "an artefact of those arrays being
float32 0-origin (0-6) with the mask collapsed onto class 0 (Sasa)" — is not what is going on.
Recoding cannot change a ratio. Cross-tabulating on the 11,779,750 non-mask cells:

```
2012: use_this == masked+1 at 89.3390 %      2021: 89.3769 %
Sasa: use_this(==1) 334096 / 361588   vs   masked(==0, non-mask) 275536 / 278379
```

`*_masked.npy` is a genuinely **different classifier vintage** — it disagrees with the published
arrays at ~10.7 % of pixels after the 0-origin shift — which is consistent with
`restore_classifier.md` (published 200-epoch weights gone; 50-epoch reruns agree 93.8 % / 95.4 %).
The report's retraction of the prior audit stands; its reason does not. Say "different classifier
run", not "coding artefact".

(Related and worth knowing: `use_this` gives +8.2 % Sasa change in *image* space but +19.1 % in
*map* space. Sasa sits close to the camera — 39 px per map cell against a 9.8 px average — so
image-space percentages are not comparable to the published ones. Map space is the correct space;
just do not let an image-space number into a response letter.)

### 3.4 A3-CHAIN-CLOSED: identity exact, agreement figure is a draw

`results/use_this/{2012,2021}_5x5.npy` are int64 (3744, 5616) with 0=mask and 1-7 the settled codes;
the CSVs are **element-identical** (`csv == npy[v,u]` → True, both years, 21,026,304 rows, v-major).
None of `results/{yr}.npy`, `_masked`, `_composite_5x5`, `_normalized` matches. The commented block
at `image_to_csv.py:16-23` names them. Grid, extent and non-NA count (1,206,233) reproduce exactly.
**Confirmed.** Only the "99.83 % / 99.84 %" figure should be restated per §3.1.

### 3.5 A2-SCRIPT-BROKEN: two blockers, not one

I ran the archived `select_sasa_communities.R` with only `setwd()` and the output path repointed:

```
UseMethod("mutate") でエラー: 'mutate' をクラス "SpatRaster" のオブジェクトに適用できるようなメソッドがありません
```

It dies at lines 37-38 (`mutate(layer = layer*10*10) %>% rename(area_2012 = layer)` on a
`SpatRaster`) because the script's own `library()` list omits **tidyterra**. Only after adding
`library(tidyterra)` does it reach line 50 and fail as the report describes:

```
エラー: オブジェクト 'vege2021' がありません
```

Conclusion (unrunnable as archived, deposit blocker) holds; fix both.

---

## 4. Process note

`data_from_server/ortho/data/selected_comms.tiff.aux.xml` (361 B, mtime 2026-08-09 23:42) was
created inside the **read-only** archive during the asset investigation — a GDAL PAM side effect of
`terra::summary(r, size=Inf)` in `inspect1.R`. It is the only file in the archive with a 2026
timestamp. Harmless, but it is a write into a directory the brief declares read-only, and it will
show up in any checksum manifest for the Zenodo deposit. Set `GDAL_PAM_ENABLED=NO` (as I did) or
copy rasters out before inspecting them.

---

## 5. What the report missed

1. **The manuscript already discloses the `dist > 0` exclusion** (`matmet.qmd:81`, and in the
   submitted `index.tex:562-566` under a heading literally called "Target variable"). A4's
   proposed Methods action is redundant.
2. **The most prominent affected number is not the TSS pair.** `results.qmd:67` and the
   `fig-hs-maps` caption argue that "the TBM predicted 47,253 m² … the TDM predicted 12,766 m²,
   much closer to observations". Training prevalence is 0.0256 (TBM) vs 0.0104 (TDM) — a 2.5×
   difference created by the domain filter — which mechanically shrinks the TDM's predicted
   suitable area. This sentence is invalidated by A4 just as the TSS comparison is, and it is the
   one a reviewer will quote. A4's `which_numbers` should name it.
3. **The published rasters are not bit-reproducible** and `georectify.R` has no `set.seed` — §3.1.
   Any Zenodo deposit must either ship the rasters or add a seed; a reviewer re-running the script
   will get 8,545-8,560 rather than 8,547.
4. **The same unseeded stochastic fill sits under the snowmelt predictors** — §3.1. This touches
   the −0.7146 d/yr result and the already-flagged half-pixel shift, not just the vegetation maps.
5. **The rasterization discards ~90 % of the classified pixels** (11.1 points per cell, last one
   wins). Quantified in §3.1: a majority vote would move 4.3 % of cells. If a Methods sentence is
   being written about this step anyway, print the sensitivity rather than only the mechanism.
6. **`sdm_sasainc.R` is not the only unrunnable archived script.** `select_sasa_communities.R`
   needs two fixes (§3.5), and `sdm_include_distance.R:36-37` contains a dangling
   `sasa12_ras %>% terra::as.polygons() %>% filter()` whose result is discarded — worth a sweep of
   every script destined for the deposit, not just the three assets.
