# Independent verification — SDM provenance (SDM-1 … SDM-10)

Verifier: independent re-run. Every number below was recomputed from the archived objects; no
figure was carried over from the investigator's report or logs. Scripts live in
`/private/tmp/claude-501/-Users-okamoto-NIES-SasaSDMPaper--claude-worktrees-review-summary-ece-ddc69c/98ac7ecf-eafb-49c6-aa2d-9aab2998c13e/scratchpad/vfy3/`
(`A_inspect.R`, `B_stack.R`, `C1.R` … `C11.R`, `xgbver.R`).

Environment: R 4.5.2, terra 1.9.34, tidyterra 1.2.0, tidysdm 1.0.4, glmnet 4.1-10.
xgboost pinned via `.libPaths()` to a scratch library.

---

## Verdict table

| id | status | one-line |
|----|--------|----------|
| SDM-1 | CONFIRMED | server scripts byte-identical; TBM script genuinely lost; 44 candidates / 4 algorithms in the archived object |
| SDM-2 | PARTLY_CONFIRMED | twi in no model — solid. "Not in the directory yet" is unproven; the "clean run aborts on CRS mismatch" claim is **false** |
| SDM-3 | CONFIRMED | train/predict distance mismatch reproduces to the digit; one sub-explanation unproven |
| SDM-4 | CONFIRMED | 2×2 decomposition exact; percentages are non-additive (sum 102%) |
| SDM-5 | CONFIRMED | 18,103.13 (+41.8%) and refit 14,927.86 both exact |
| SDM-6 | PARTLY_CONFIRMED | all TSS values exact, but 52.9% is domain-dependent and the footprint definition is mixed between rows |
| SDM-7 | PARTLY_CONFIRMED | every area exact, but the headline "<1%" is contradicted by its own −2.94% |
| SDM-8 | CONFIRMED (+strengthened) | version pin exact; JSON boosters also verified forward-portable to xgboost 3.2.1.1 |
| SDM-9 | PARTLY_CONFIRMED | membership/conclusion exact; the deposited blend coefficients are wrong at the 4th sig. fig. |
| SDM-10 | CONFIRMED | all three stale artefacts identified correctly |

---

## SDM-1 — the TBM script is lost

```
$ diff data_from_server/ortho/sdm.R scripts/sdm/sdm_tbm.R                 -> exit 0 (489 lines)
$ diff data_from_server/ortho/sdm_include_distance.R scripts/sdm/sdm_tdm.R -> exit 0 (444 lines)
md5 repo == md5 dump for models.rds, model_stack.rds, models_wo_dist.rds,
           model_stack_wo_dist.rds, models_all_5m.rds, tss_score_tbm.rds
```

`A_inspect.R` on the archived workflow sets:

| object | wflow_ids | rf / gam / maxent / xgb candidates | total | predictors | split data n |
|---|---|---|---|---|---|
| models.rds (TDM) | rf, gam, maxent, xgb | 8 / 1 / 18 / 18 | **45** | 8 (incl. dist) | 16,596 |
| models_wo_dist.rds (TBM) | rf, gam, maxent, xgb | 7 / 1 / 18 / 18 | **44** | 7 | 21,389 |
| models_all_5m.rds | rf, gam, maxent, xgb | 6 / 1 / 18 / 18 | 43 | 6 | 17,991 |

`ortho/figures/model_performance_tbm.png` read directly: workflow ranks 1–44, legend
GBT / GAM / MaxEnt / RF, and exactly **7 purple RF points** — an independent read of the
predictor count off the published figure.

Line numbers in the surviving `sdm_tbm.R` check out: model list L96–113, commented
`update_workflow_model` L117–119, `saveRDS(models, "models_wo_dist_twi.rds")` L127,
`readRDS(...)` L129, `scale_color_discrete(labels = c("GBT","GAM","MaxEnt","RF"))` L132.

Extra check the report does not mention: `data_from_server/ortho/.Rhistory` (314 lines) contains
**no** SDM code — `grep -i "models_wo_dist|saveRDS(models|sdm_spec|workflow_set|twi"` returns
nothing. The only other server `.R` files referencing `workflow_set` are `sdm.R`,
`sdm_include_distance.R`, `sdm_sasainc.R`. The lost script is genuinely lost.

The reconstruction itself (uncomment three blocks, change the filename) is an **inference**,
well supported but not proved. Say so in the response letter.

## SDM-2 — twi

CONFIRMED: no archived recipe contains `twi`. Recipe predictor lists (`A_inspect.R`):
TDM `aspect, roughness, slope, elevation, TPI, TRI, snow, dist`;
TBM the same minus `dist`; all_5m `slope, snow, dist, elevation, aspect, TPI`.
RF candidate counts 8 / 7 / 6 = number of predictors, so `tune_grid(grid = 18)` on
`mtry = tune()` independently pins the predictor count. `ortho/figures/vi_tdm.png` read
directly: exactly 8 boxes (dist, snow, elevation, aspect, TRI, roughness, slope, TPI), no twi.
`twi` at the 16,596 TDM training points: mean **5.712072**, NA fraction 0.

CONFIRMED: `preprocess_snow_data.R:67,96,137` write `snow_mean.tif`, `snow_sd.tif`,
`snow_reg.tif` into `data/terrain_features/` — the directory `sdm_tbm.R:13` globs. Today's glob
returns `aspect, roughness, slope, tateyamadem_small, TPI, TRI, twi`.

**REFUTED — the CRS-abort claim.** `C9.R` / `C10.R` run the exact combine the script writes,
with no `crs()` reassignment:

```
crs terrain: 3099   snow: 6690   dist: 6690
EXACT sdm_tdm.R combine -> c() OK nlyr 9
EXACT sdm_tdm.R extract -> extract OK rows 1206233 NA rows 46735
   (warning only: "[extract] transforming vector data to the CRS of the raster")
```

`C11.R` quantifies the transform: JGD2011→JGD2000 UTM 53N at these points is
`dx mean 0.0000 m, dy mean 0.0000 m, max |shift| 0.0000 m`, and extracted elevation/slope/TPI
match the stored training values identically whether or not the relabel is applied
(16,596/16,596 exact both ways). The CRS relabel is cosmetic. Do **not** report it as a
reproducibility bug.

**UNPROVEN — "twi was not in `data/terrain_features/` yet."** All seven terrain files carry the
same Dec 12 2025 copy timestamp, so nothing dates them. The surviving `sdm_tbm.R` writes
`models_wo_dist_twi.rds`, i.e. the author ran a deliberate TWI variant; and the investigator's
own `i03.log` shows the twi-included clean run (n = 21,397) lands *closer* to the archived
21,389 than the no-twi run (21,426). Either ordering is consistent with the evidence. State
the fact (twi is in no model), not the story.

## SDM-3 — distance train/predict mismatch

`C1.R`, re-extracting every candidate raster at the 16,596 stored TDM training geometries:

```
dist=all12      maxabs=0        meanabs=0        exact=16596/16596
dist=pol12      maxabs=234.981  meanabs=13.880   exact= 5577/16596
dist=all21      maxabs= 86.839  meanabs= 7.764   exact= 1218/16596
dist=pol21      maxabs=164.545  meanabs=12.840   exact= 1158/16596
dist=selcomms   maxabs=611.267  meanabs=68.904   exact=  788/16596
stored dist: min 1   n==0: 0   max 142.088
snow=fitted2021 (as published) maxabs=0  exact=16596/16596
snow=fitted2021 RE-REGISTERED  maxabs=53.633  exact=120/16596
```

Prediction side (`C2.R`, against the archived published maps):

```
snow21 x distpol12 vs sasa_pred_tdm_21   maxabs=5.96e-08  meanabs=1.26e-11
snow30 x distpol21 vs sasa_pred_tdm_30   maxabs=5.96e-08  meanabs=1.27e-11
snow21 x distall12 vs sasa_pred_tdm_21   maxabs=0.5272    meanabs=0.01698
repro TBM 2021     vs sasa_pred_sdm_21   maxabs=5.96e-08  meanabs=3.05e-11
```

(The report quotes 4.6e-8 / 4.5e-8; I get 5.96e-8. Both are float32 round-off — same conclusion.)

Script structure verified: dead no-op `sasa12_ras %>% terra::as.polygons() %>% filter()` at
`sdm_tdm.R:36-37`; training distance from all 2012 pixels at `:44-47`; `filter(dist > 0)` at
`:69`; `set.seed(1)` at `:71`, i.e. after the thinning; prediction distances from polygons
>5 m² at `:247-250` and `:252-255`. `select_sasa_communities.R:17` uses the same >5 m² rule.

**Unproven sub-claim.** The report attributes the residual 16,552/3,111 vs 16,596/3,123 to
`thin_by_cell` being unseeded. `thin_by_cell` is applied only to the absence subset; presences
are never thinned, and the full presence pool (3,973) is deterministic. A 12-presence
difference in the training split can therefore only arise indirectly, via the thinned absences
shifting the `spatial_block_cv` block grid. That is plausible but was not demonstrated. Do not
assert the mechanism in the response letter.

## SDM-4 — F-3 decomposition

`C2.R`, `terra::expanse()` geodesic areas at HS > 0.5:

```
archived TDM 2021 suitable 12766.04 | newly2030 4386.61 | lost 716.61   (paper 12766 / 4387 / 717)
archived TBM 2021 suitable 47253.26 | newly2030 27049.27 | lost 2257.77 (paper 47253 / 27049 / 2257)

snow21 x dist12 : 12766.04  (baseline)
snow30 x dist12 : 12190.35   newly vs baseline  533.71
snow21 x dist21 : 14888.88   newly vs baseline 3937.85
snow30 x dist21 : 14474.11   newly vs baseline 4386.61  (= published)
```

Every figure exact. Snowmelt alone: 533.71 / 4386.61 = 12.2%. Distance swap alone:
3937.85 / 4386.61 = 89.8%.

**Caveat the report should carry:** 12.2% + 89.8% = 102%. The decomposition is not additive
(−2% interaction). Present it as the 2×2 table, not as two percentages.

Corroboration the report never cites: the *published* `vi_tdm.png` already shows permutation
importance of `dist` ≈ 0.44 TSS loss versus `snow` ≈ 0.22 — the dominance of distance is
visible in a figure that is already in the manuscript.

## SDM-5 — F-4

```
repro_tdm_snow21_distall12 suitable = 18103.13 m2   (vs 12766.04, +41.8%)
refit (polygon dist in BOTH train and predict): 2021 = 14927.86 | newly2030 = 5029.26 | lost = 605.67
observed Sasa 2021 = 10170.45 m2 ; observed Sasa 2012 = 8542.34 m2
```

All exact. Both self-consistent repairs move the TDM's 2021 area **further from** the observed
10,170 m². The published 12,766 m² is the smallest of the three and is the one produced by the
inconsistency. `results.qmd:67` ("much closer to observations") and `results.qmd:75`
("closely matching the observed distribution") do not survive.

## SDM-6 — F-5

`C3.R` / `C4.R`. Using `tidysdm::tss_max` on the 390,065-cell common domain (the report's
domain), with the **all-pixel** 2012 footprint:

```
all cells                                    n=390065 prev=0.0256  TDM=0.7860  TBM=0.6212
held out from BOTH training frames           n=356019 prev=0.0060  TDM=0.7458  TBM=0.5422
held out & outside 2012 ALL-PIXEL footprint  n=352492 prev=0.0024  TDM=0.5736  TBM=0.4381
held out & inside  2012 ALL-PIXEL footprint  n=  3527 prev=0.3675  TDM=0.4244  TBM=0.1312
```

Exact to four decimals. Training-cell union = 34,046. My own cumulative-TSS implementation
agrees to ≤0.0008 (tie handling), so the values are not an artefact of `tidysdm`.

**Two accounting problems.**

1. *Domain-dependent denominator.* On the 390,065-cell domain there are 12,630 suitable cells,
   of which 6,675 have polygon-distance 0 → 52.9%, and 6,754 have all-pixel distance 0 → 53.5%.
   On the domain that actually generates the published 12,766.04 m² (HS ∩ dist defined,
   397,403 cells) there are **12,773** suitable cells and the same 6,675 → **52.3%**. The
   honest sentence for the manuscript is "about 52% of the published suitable area", using
   12,773 as the denominator because that is the number the published area is computed from.
2. *Mixed footprint definitions.* The 52.9% / mean-HS row uses the **polygon** footprint while
   every held-out TSS row uses the **all-pixel** footprint. Held consistently to the polygon
   definition the held-out numbers change materially:

```
held out & outside 2012 POLYGON footprint  n=353522 prev=0.0026  TDM=0.5766  TBM=0.4253
held out & inside  2012 POLYGON footprint  n=  2497 prev=0.4910  TDM=0.1967  TBM=0.0406
```

   Note 0.1967, not 0.4244, for the inside domain. Pick one definition before anything goes in
   print.

Confirmed unchanged: mean HS 0.5975 (polygon footprint) vs 0.1123 elsewhere; 0.5292 vs 0.1120
under the all-pixel footprint. Suitable area outside the polygon footprint 6,094.68 m²; outside
the all-pixel footprint 6,015.72 m². On the polygon-outside held-out domain my re-evaluation of
the variants gives TDM 0.5766, polygon-refit 0.5668, all-pixel-prediction 0.6700, TBM 0.4253 —
same ordering as the report, values differ because the report used the all-pixel domain.

**The substantive conclusion is right and must not be softened:** the TDM still beats the TBM
on every common domain, but its own TSS falls from 0.746 to 0.574 once the 2012 footprint is
removed and the margin narrows to roughly the published 0.145. "Strengthens" is not supported.

## SDM-7 — half-pixel snowmelt misregistration

`C2.R` on the investigator's `reg_pub_*` / `reg_fix_*` rasters:

```
TDM pub: 12766.04 / newly 4386.61 / lost 716.61
TDM fix: 12844.99 / newly 4257.68 / lost 769.58     (+0.62% / -2.94% / +7.39%)
TBM pub: 47253.26 / newly 27049.27 / lost 2257.77
TBM fix: 47281.25 / newly 27026.28 / lost 2272.76   (+0.06% / -0.08% / +0.66%)
per-cell tdm21 sd(|diff|)=0.00468  cells crossing 0.5 =    321 (0.08%)
per-cell tbm30 sd(|diff|)=0.02960  cells crossing 0.5 = 10,248 (2.57%)
```

Every area figure exact; cell-crossing counts exact. Origins confirmed: `vege_2012_5x5.tiff`
origin (0, 0.25), `fitted_2021.tiff` origin (0.5, −0.25).

**The summary sentence is wrong.** "moves every published area by less than 1% except the TDM's
'lost by 2030'" and "all move by <1% except 717 → 770" are contradicted by the report's own
number: 4,386.61 → 4,257.68 is **−2.94%**, and 4,387 m² is a headline figure in
`results.qmd:87` and in the `fig-future-prediction` caption. Correct wording: two of the six
published areas move by more than 1% (−2.9% and +7.4%); the other four move by ≤0.7%.

My sd values are the sd of |Δ|; the report's 0.00476 / 0.03246 are presumably the sd of signed
Δ. State which.

The caveat about the models having been *trained* on the misregistered layer is correct and
must stay.

## SDM-8 — xgboost pin

`xgbver.R`, three libraries, same archived stacks:

```
xgboost 3.2.1.1 -> FAIL "'xgb.Booster' object is corrupted or is from an incompatible
                         XGBoost version" (index 4, default_xgb_1_02 for the TDM)
xgboost 1.7.6.1 -> OK
xgboost 1.7.7.1 -> OK   TDM .pred_presence = 0.0938586042 0.0939298993 0.0999191912
                                             0.0909757494 0.0876513470
                        TBM .pred_presence = 0.2683840653 0.2598344674 0.2279474806
                                             0.3039961416 0.2400559079
1.7.6.1 and 1.7.7.1 give bit-identical output.
```

**Strengthened.** The deposit argument needs the JSON boosters to load in a *current* xgboost,
which the investigator never tested. `C8.R` does:

```
                                                1.7.7.1        3.2.1.1
model_stack__default_xgb_1_02.json   n=8  mean 0.0954499720  0.0954499720
model_stack__default_xgb_1_04.json   n=8  mean 0.1054098069  0.1054098069
model_stack__default_xgb_1_08.json   n=8  mean 0.2165595374  0.2165595374
model_stack__default_xgb_1_10.json   n=8  mean 0.1822704959  0.1822704959
model_stack_wo_dist__default_xgb_1_05.json n=7 mean 0.2146731152  0.2146731152
model_stack_wo_dist__default_xgb_1_09.json n=7 mean 0.1716896119  0.1716896119
```

All six load and predict identically under both versions. The JSON deposit genuinely removes
the xgboost 1.7 dependency.

## SDM-9 — ensemble membership

`B_stack.R`, full precision from `s$coefs %>% tidy()`:

**TDM `model_stack.rds`** — penalty 0.1, mixture 1, train n 16,596:

| term | estimate |
|---|---|
| (Intercept) | **−1.57283796** |
| default_maxent_1_02 | **0.68076638** |
| default_maxent_1_14 | **0.03739480** |
| default_maxent_1_03 | **0.01525842** |
| default_xgb_1_02 | **0.15274982** |
| default_xgb_1_04 | **1.01101452** |
| default_xgb_1_08 | **1.08133083** |
| default_xgb_1_10 | **1.28211149** |

**TBM `model_stack_wo_dist.rds`** — penalty 0.1, mixture 1, train n 21,389:

| term | estimate |
|---|---|
| (Intercept) | **−1.31308860** |
| default_maxent_1_17 | **0.17752570** |
| default_maxent_1_12 | **0.53174120** |
| default_xgb_1_05 | **1.96126170** |
| default_xgb_1_09 | **0.31053980** |

3 MaxEnt + 4 XGB and 2 MaxEnt + 2 XGB confirmed; no RF, no GAM. Published metrics confirmed:
`tss_tdm.csv` tss_max 0.7009730541, `tss_score_tbm.csv` 0.5555946421.

**The coefficients in `restore_sdm.md` §8 are wrong.** They were transcribed from a 3-significant-
figure tibble print (`v05.log` shows `0.681 0.0374 0.0153 0.153 1.01 1.08 1.28`) and then
written out to 4–5 digits that were never observed. Errors: xgb_1_08 quoted 1.0764 vs actual
1.08133 (0.46% low), xgb_1_04 quoted 1.0139 vs 1.01101, xgb_1_05 quoted 1.9583 vs 1.96126,
intercepts −1.5732 / −1.3134 vs −1.57284 / −1.31309. SDM-8 proposes depositing exactly these
numbers so a reader can rebuild the ensemble without `stacks`; use the table above instead.

Nuance on `which_numbers`: `results.qmd:30` ("within the TDM, GBT, MaxEnt, and RF outperformed
GAM"; "within the TBM, GBT and MaxEnt performed best") is about the *tuning* plot and is
**correct** — TDM best means rf 0.69824 / gam 0.68817 / maxent 0.69803 / xgb 0.70531; TBM
rf 0.53692 / gam 0.55400 / maxent 0.57685 / xgb 0.57414. Only `matmet.qmd:117` needs rewording.

## SDM-10 — stale artefacts

`C6.R` at the 17,991 stored `models_all_5m` geometries:

```
n = 17991  presences = 1076
dist=all12 maxabs=0 exact=17991/17991 ; dist=pol12 maxabs=235.2 exact=4733/17991
snow=fitted2021 maxabs=0 exact=17991/17991 ; min dist 1 ; n dist==0 : 0 ; max elev 2559.999
presences: 2012!=Sasa & 2021==Sasa  1076/1076 ;  sasa_inc==1  1076/1076
absences : non-Sasa both years    16915/16915 ;  sasa_inc==0 16915/16915
```

Response is `sasa_inc`. Metric set is `boyce_cont, roc_auc, tss_max` (the two published models
use `tss_max` alone). 43 tuned candidates.

`tss_score_tbm.rds` = 0.535 (tss_max) vs `tss_score_tbm.csv` = 0.5556 — two different TBM fits,
the csv being the published one.

`sasa_pred_sdm_12.tiff` is 1157×1213 (all the published maps are 1753×1801).
`sasa_pred_sdm_dist_21.tiff` is not reproduced by any of the eight snowmelt × distance
combinations — best agreement meanabs 0.06195 (all-pixel × snow21), versus 1.26e-11 for the
true 2021 map. `sasa_sdm.qgz` unzips to `sasa_sdm.qgs` whose datasources are
`../../../ダウンロード/{risky_area,sasa_inc,sasa_pred_sdm_dist_21,sasa_pred_sdm_dist_30,vege_2012_5x5}.tiff`.

---

# What the investigation missed

## M-1 (critical). The two published models disagree on the *sign* of the snowmelt effect

The TBM has no distance predictor, so the only thing that changes between its 2021 and 2030
maps is the snowmelt layer. `C5.R`:

```
TBM total suitable 2021 = 47253.26  ->  2030 = 69176.33   (+46.4%)
TDM total suitable 2021 = 12766.04  ->  2030 with distance frozen at 2012 = 12190.35  (-4.5%)
```

SDM-4 reports the TDM contraction but never contrasts it with the TBM's +46%. Stated together
this is far stronger and far more awkward: the same 2030 snowmelt surface expands suitable area
by 46% under one published model and contracts it by 4.5% under the other. Any sentence
attributing the projected expansion to earlier snowmelt (the `fig-future-prediction` caption,
`results.qmd:95`) is unsupported by the models as fitted, and the internal disagreement should
be disclosed rather than resolved by picking the convenient model.

## M-2 (critical). HS > 0.5 is nowhere near a calibrated threshold — and I computed the damage

The report parks this as `NEEDS_WORK`. It takes minutes. `C4.R` computes max-TSS on each
model's **own archived training frame** using the archived stack's own predictions:

```
model_stack.rds          n=16596 prev=0.188  max-TSS threshold 0.135 (TSS 0.7602)  TSS at 0.5 = 0.5380
model_stack_wo_dist.rds  n=21389 prev=0.366  max-TSS threshold 0.390 (TSS 0.6361)  TSS at 0.5 = 0.5988
```

`C5.R` re-thresholds the published maps at those cuts:

```
TDM @0.135 : 2021 38,672 m2 | newly 2030 11,235 m2 | lost 0        (@0.5: 12,766 / 4,387 / 717)
TBM @0.390 : 2021 88,333 m2 | newly 2030 44,371 m2 | lost 1,152    (@0.5: 47,253 / 27,049 / 2,258)
observed Sasa 2021 = 10,170 m2
```

The TDM's suitable area is **3.0×** the published figure at its own calibrated cut. The paper's
central comparison — "TBM 47,253 overestimates, TDM 12,766 much closer to the observed 10,170"
— is an artefact of an arbitrary 0.5 cut, compounding with the distance inconsistency of SDM-5.
Both effects push the same way: the agreement the manuscript reports is not a property of the
model. This is the most consequential unexamined item in the area and it should be resolved
before the response letter is drafted, not offered as an optional sensitivity analysis.

(Caveat: 0.135 / 0.390 are max-TSS cuts on the training frames, whose prevalence, 0.19 and 0.37,
is far above the landscape prevalence 0.006. A cut calibrated on the held-out landscape is lower
still — TDM 0.11, TBM 0.32, giving 61,958 and 121,206 m². Whichever calibration is chosen,
0.5 is not it, and every published area moves by more than any other correction discussed.)

## M-3 (important). The blend coefficients earmarked for deposit are fabricated precision

See SDM-9. Digits 4–5 were never observed; the largest error is 0.46%. Correct table above.

## M-4 (important). The CRS-mismatch "bug" does not exist

See SDM-2. `c()` and `extract()` both succeed; the datum transform is a 0.0000 m no-op and the
extracted values are identical with and without the relabel. Putting this in a response letter
to reviewers who read the code would be an unforced error.

## M-5 (minor). Corroboration already in the manuscript

`ortho/figures/vi_tdm.png` — a figure already in the paper — shows permutation importance of
`dist` at ≈0.44 TSS loss against `snow` at ≈0.22. SDM-4's conclusion can be anchored to a
published figure, not only to a re-run.

## M-6 (minor). The 2×2 decomposition is non-additive

12.2% + 89.8% = 102%. Report the four cells, or report three terms (snowmelt, distance,
interaction).

## M-7 (minor). One more place the lost script could have hidden was not checked

`data_from_server/ortho/.Rhistory` (314 lines) — checked here, contains no SDM code. Worth
stating in the restoration record so the "lost" claim is closed rather than merely asserted.
