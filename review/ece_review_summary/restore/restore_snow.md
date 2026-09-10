# Snowmelt pipeline — restoration report

**Target:** reconstruct the snowmelt pipeline and establish where the manuscript's
`−0.86 d/yr` came from.
**Date:** 2026-08-09. Everything below was produced by running code, not by inspection.

Deliverables:

- `/Users/okamoto/NIES/SasaSDMPaper/.claude/worktrees/review-summary-ece-ddc69c/review/restore/snow/snowmelt_pipeline.R` — single runnable pipeline
- `/Users/okamoto/NIES/SasaSDMPaper/.claude/worktrees/review-summary-ece-ddc69c/review/restore/snow/year_coverage_and_power.R`
- `/Users/okamoto/NIES/SasaSDMPaper/.claude/worktrees/review-summary-ece-ddc69c/review/restore/snow/snowmelt_statistics.csv` (the Reviewer-2 package)
- `/Users/okamoto/NIES/SasaSDMPaper/.claude/worktrees/review-summary-ece-ddc69c/review/restore/snow/snowmelt_filter_sensitivity.csv`
- `/Users/okamoto/NIES/SasaSDMPaper/.claude/worktrees/review-summary-ece-ddc69c/review/restore/snow/annual_scene_means.csv`
- `/Users/okamoto/NIES/SasaSDMPaper/.claude/worktrees/review-summary-ece-ddc69c/review/restore/snow/landscape_trend_power.csv`
- `/Users/okamoto/NIES/SasaSDMPaper/.claude/worktrees/review-summary-ece-ddc69c/review/restore/snow/out/` — regenerated rasters

---

## 0. Headline

**The predictors and the reported coefficient are from different datasets, and this is now
proven bit-exactly, not inferred.**

| archived product | reproduces from `data_from_server/ortho/data/snow/raw/*.tiff`? | evidence |
|---|---|---|
| `fitted_2011 … fitted_2021`, `fitted_2030` (the SDM predictors) | **YES, exactly** | `max(abs(archived − reconstructed)) = 0` on every shared cell, cor = 1.000000 |
| `snow_reg.tif` (source of the manuscript's −0.86) | **NO** | cell-wise cor = 0.5107; only 0.03 % of shared cells agree to 1e−6; cell count differs by 73,058 |

The Methods sentence (`paper/matmet.qmd:77`) —

> "we fitted a linear regression to per-pixel snowmelt DOYs for each year from 2011 to 2021
> and used the predicted 2021 values as explanatory variables in both models. The mean
> regression coefficient was −0.86"

— asserts that −0.86 is the mean coefficient **of the very regressions that produced the
predictors**. That is false. Those regressions reproduce exactly, and their mean coefficient
is **−0.7146**, i.e. **7.1 days per decade, not 8.6**.

`snow_reg.tif` is *not* an SDM input. Grep over `scripts/` and the server scripts shows the
SDM reads only `data/snow/fitted_{2012,2021,2030}.tiff`; `snow_reg.tif` / `snow_mean.tif` /
`snow_sd.tif` are read nowhere except `plot_snowmelt_shift.R`, whose last three lines exist
solely to print `mean(snow_reg.tif)`. So −0.86 is a *reported number with no downstream role*,
computed from a data vintage that no longer exists. Nothing in the models changes if it is
corrected; only the Abstract, Methods and Discussion text does.

---

## 1. Where −0.86 came from — and why it is unreproducible

### 1.1 The recipe was recovered

`data_from_server/ortho/preprocess_snow_data.R` (byte-identical to the repo copy) contains
the `snow_reg` block. Two things in it were previously unknown:

- the value filter is **`filter(snowmelt > 0)`**, not `> 10`;
- there is a **`filter(lm.r2 > 0.2)`** before writing the raster.

The r² filter can be ruled out immediately: `snow_mean.tif`, `snow_sd.tif` and `snow_reg.tif`
all have **exactly 1,133,175 non-NA cells**, and mean/sd were written without any r² filter.
An r² > 0.2 filter would have cut `snow_reg` to a small fraction of that. It was therefore
not applied in the run that produced the archive (the block as written also cannot execute —
`partit`, `coeffisients`, and `$adj.r.squared` on an `lm` object are all broken, so the
surviving file is a stale copy of an interactively-fixed script).

### 1.2 It is not a filter question

Per-pixel OLS on the ten current `raw/*.tiff`, warped to the `vege_2012_5x5.tiff` grid,
requiring all ten images to cover the pixel (1,206,233 cells):

| rule | n pixels | mean slope |
|---|---|---|
| no filter (DOY 0 kept) | 1,206,233 | **−0.9184** |
| DOY > 0 | 1,206,081 | −0.7147 |
| DOY > 10 | 1,206,081 | −0.7146 |
| DOY > 120 (drop left-censored) | 1,189,958 | −0.6972 |

−0.8636 is not any of them, and restricting to the archive's own 1,133,175-cell footprint
does not produce it either (−0.8961 / −0.7125 / −0.7123 / −0.6914 respectively).

### 1.3 It is a vintage question — confirmed cell by cell

Comparing the archived layers against reconstructions on the 1,105,900 cells they share:

| layer | archived mean | reconstructed mean | cell-wise cor | fraction agreeing |
|---|---|---|---|---|
| `snow_mean.tif` | 171.763 | 170.445 | 0.954 | 0.16 % (to 1e−4) |
| `snow_sd.tif` | 9.677 | 10.351 | 0.361 | 0.01 % |
| `snow_reg.tif` | −0.8636 | −0.7147 | 0.511 | 0.03 % |

Spatially similar, numerically different everywhere. The archived layers were computed from
per-year rasters that are *not* the ones in `raw/`.

### 1.4 The aligned CSVs are the *same* vintage as `raw/`, so they do not supply the missing one

`ortho/.Rhistory` yielded the `interpolate()` routine: join the aligned CSV to
`data/georectified.csv` on `(u, v)`, rasterise the resulting points at 1 m, then one 3×3
modal `focal()` pass with `na.policy = "only"`. I re-ran it for all eleven aligned years and
compared to `raw/*.tiff` on the 541,229 cells the recovered `georectified.csv` still covers:

```
2011 exact=0.9990 cor=0.99997 meandiff=-0.002
2012 exact=0.9989 cor=0.99997 meandiff=-0.001
2013 exact=0.9990 cor=0.99999 meandiff=-0.002
2014 exact=0.9989 cor=0.99998 meandiff=-0.002
2015 exact=0.9989 cor=0.99999 meandiff=-0.002
2016 exact=0.9989 cor=0.99998 meandiff=-0.002
2017 exact=0.9991 cor=0.99998 meandiff=-0.002
2018 exact=0.9991 cor=0.99998 meandiff=-0.002
2020 exact=0.9994 cor=0.99995 meandiff=-0.002
2021 exact=0.9990 cor=0.99996 meandiff=-0.002
```

99.9 % of cells identical; the residual 0.1 % is GDAL's "last value burned" tie-break among
sub-metre points. **`aligned/` + `georectified.csv` → `raw/`.** So the aligned CSVs cannot be
the older vintage.

### 1.5 What the older vintage actually was

`.Rhistory` preserves *two distinct georectification workflows*, and only the second one is
represented in the recovered data:

| | old | current |
|---|---|---|
| working dir | `/media/okamoto/HDD3TB/tateyama/mrd_snowmelt/step2_ortho/` | `~/Projects/jasms2023f/ortho/` |
| lookup table | `georectificated.csv` (note the spelling), from `gcp.csv` + `procOPA` + `fields::Tps` | `data/georectified.csv` (u,v,x,y,z,B,G,R — DSM/point-cloud derived) |
| rasteriser | `ortho()`: `st_rasterize(dx=1,dy=1)`, no gap fill | `interpolate()`: rasterise + 3×3 modal `focal` |
| intermediate | `snowmelt_ortho/*.csv`, columns `x,y,snow_melt` | `data/snow/aligned/*.csv`, columns `u,v,snowmelt_doy` |
| **zero handling** | `filter(x != 0, snow_melt != 0)` — **zeros deleted at CSV stage** | zeros retained as DOY 0 |

The old route's `snowmelt_ortho/*.csv` and `georectificated.csv` are **not** in the 17 GB
recovery. `snow_reg.tif`/`snow_mean.tif`/`snow_sd.tif` are the only surviving products of it.

That table also explains the 73,058-cell deficit. In the old route zeros were removed *before*
rasterising, so a pixel that was sky/cloud in any one year became NA in that year and was then
killed for all years by `drop_na()` on the wide table. In the current route zeros survive as
0 and only the observation is dropped. The direction and rough magnitude are right: in the
current data 31,786 pixels carry ≥ 1 zero, and the old, coarser ortho (no modal gap fill, so
more holes) would lose considerably more.

**Verdict on Q1: `snow_reg.tif` is NOT REPRODUCIBLE from anything in the recovery.** It was
produced by a georectification pipeline whose intermediate files were not copied. This is a
statement backed by (a) exact reproduction of the *other* products from the surviving inputs
and (b) cell-wise disagreement of `snow_reg` with every filter/footprint combination tested.

### 1.6 A caution about −0.86's likely provenance

The nearest reproducible number is **−0.9184**, obtained by *not* filtering DOY 0. That value
is an artefact: the DOY-0 (sky/invalid) count is not uniform across years —

```
2011 160  2012 169  2013 152  2014 150  2015 157
2016 152  2017 165  2018 165  2020 3,933  2021 27,996
```

— it is concentrated in the last two years, so keeping zeros drags 2020/2021 downward and
manufactures a steeper "advance". The landscape-scale slope moves from **−0.688 (p = 0.337)**
with zeros excluded to **−0.918 (p = 0.204)** with zeros included, purely from this. Given
that −0.8636 lies between the two and that the old pipeline deleted zeros only *partially*
(`snow_melt != 0` at CSV stage, but `drop_na()` afterwards would have been applied to a
different hole pattern), the most probable explanation is that −0.86 carries some of the same
contamination. It should not be defended.

---

## 2. The recovery itself is incomplete — eight files are truncated

While testing 1.4 I found the answer to the "odd file-size clustering" question, and it is not
a data signal. **2012/2014/2017 are truncated transfers.** Their sizes are exact MiB
multiples and their last lines are cut mid-record:

```
2012 → 138,412,032 B = 132.0000 MiB   last line "…,3617,198"
2014 → 146,800,640 B = 140.0000 MiB   last line "…85,2091,194…"
2017 → 142,606,336 B = 136.0000 MiB   last line "…37,181|…"
```

A scan of all 346 files > 1 MB under `data_from_server/` found **eight** with sizes that are
exact multiples of 1 MiB — all of them in `ortho/data/`:

| file | size | MiB | consequence |
|---|---|---|---|
| `ortho/data/georectified.csv` | 167,772,160 | 160 | **severe** — `v` reaches only 1268 of 3743; 34 % of image rows survive; only 541,229 of 1,206,233 analysis cells can be rebuilt |
| `ortho/data/tateyama2.tiff` | 163,577,856 | 156 | orthophoto |
| `ortho/data/2012_5x5.csv` | 146,800,640 | 140 | classifier output, 2012 |
| `ortho/data/snow/aligned/…2014….csv` | 146,800,640 | 140 | rows above v = 2091 lost |
| `ortho/data/snow/aligned/…2017….csv` | 142,606,336 | 136 | rows above v = 2037 lost |
| `ortho/data/snow/aligned/…2012….csv` | 138,412,032 | 132 | rows above v = 1983 lost |
| `ortho/data/pointcloud.db` | 109,051,904 | 104 | point cloud |
| `ortho/data/2021_5x5.csv` | 58,720,256 | 56 | classifier output, 2021 |

**Action for the user: re-copy these eight files from the server** (`rsync -c`, or verify by
md5). Nothing else in the recovery shows the signature. Without an intact `georectified.csv`
the aligned → raster stage cannot be re-run at full extent; `raw/*.tiff` are luckily complete,
so the analysis itself is not blocked, but the provenance chain is only demonstrable over 45 %
of the domain until those files are re-fetched.

---

## 3. The reconstructed pipeline and the one filter rule

`review/restore/snow/snowmelt_pipeline.R` runs end to end: aligned CSV → 1 m raster →
per-pixel OLS → slope/mean/SD/p/r² layers → fitted layers → 2030 extrapolation → statistics.
Stage 1 is switched off by default because `georectified.csv` is truncated; with an intact
copy set `STAGE_RASTERISE <- TRUE` and it reproduces `raw/`.

**The rule, and why.**

1. **DOY 0 is missing data, not day zero.** The camera runs from April; the grey-scale
   encoding is DOY 120–230 (`…_120-230_BW`). Zero means sky / no valid observation. Set the
   *observation* to NA — do not delete the pixel. A pixel enters the regression with ≥ 8 of
   10 valid years.
2. **Keep the censored extremes but declare them.** 6.89 % of observations sit exactly at
   DOY 120 (left-censored: already snow-free at the first image) and 1.32 % at ≥ 230
   (right-censored). 29.5 % of pixels contain at least one. Dropping the left-censored ones
   changes the mean slope by 0.017 d/yr, so it is not worth the loss of 110,719 pixels — but
   it must be stated in the Methods, because it is a real limitation.

**How much the answer moves between rules** (`snowmelt_filter_sensitivity.csv`):

| rule | n pixels | mean slope | median | % negative | % p<.05 | landscape slope | landscape p |
|---|---|---|---|---|---|---|---|
| none (DOY 0 kept) | 1,206,233 | −0.9184 | −0.6424 | 91.96 | 2.15 | −0.9184 | 0.204 |
| **DOY > 0 (chosen)** | 1,206,063 | **−0.7146** | −0.6264 | 91.65 | 1.96 | −0.6885 | 0.337 |
| DOY > 10 | 1,206,063 | −0.7146 | −0.6264 | 91.66 | 1.96 | −0.6884 | 0.338 |
| DOY > 120 | 1,095,344 | −0.7311 | −0.6383 | 91.95 | 2.01 | −0.6723 | 0.347 |

`>0` vs `>10` is numerically irrelevant (only 96 observations in the whole dataset lie in
1–10). The consequential choice is **zeros in or out**, worth 0.20 d/yr — 2 days per decade.

Note the identity in row 1: with no missing values the mean of the per-pixel slopes equals the
slope of the annual means exactly (−0.9184 both ways), which is why the "landscape" and
"pixel" numbers only diverge once a filter creates gaps.

**Verification that the pipeline is the right one:** its `fitted_*.tiff` outputs match the
archived ones with `maxdiff = 0` for 2011, 2015, 2018, 2021 and 2030. The predictor chain is
fully restored.

---

## 4. The statistical package Reviewer 2 asked for

From `snowmelt_statistics.csv`, n = 1,206,063 pixels, 2011–2021 (2019 absent), DOY > 0:

| quantity | value |
|---|---|
| mean pixel slope | **−0.7146 d/yr** (7.1 d/decade) |
| median | −0.6264 |
| SD across pixels | 0.6344 |
| 95 % CI of the mean | [−0.7158, −0.7135] *(see caveat)* |
| 2.5–97.5 percentile of slopes | [−2.130, +0.292] |
| % negative | **91.65 %** |
| % positive | 8.29 % |
| % exactly zero | 0.06 % |
| % individually significant (p < 0.05) | **1.96 %** (1.95 % negative, 0.008 % positive) |
| % surviving Benjamini–Hochberg FDR q < 0.05 | **0.00 %** |
| median per-pixel residual SD | 8.88 days |
| median per-pixel r² | 0.064 |
| median SE of a pixel slope | 0.889 |
| smallest slope a pixel could resolve at p < .05 | **2.05 d/yr** |
| landscape-scale slope (OLS on 10 annual means) | **−0.688 d/yr**, SE 0.675, t = −1.020, **p = 0.337**, 95 % CI [−2.244, +0.868] |

The ±0.001 CI on the mean is arithmetically correct but scientifically meaningless — it treats
1.2 M 1 m pixels from one camera as independent. **Report the landscape-scale CI
[−2.24, +0.87], not the pixel-level one.** The percentile range [−2.13, +0.29] is the honest
way to convey spread.

### 4.1 The "more pixels should be significant" question — investigated, and the answer is no

The user's expectation is not supported, and the audit's 1.96 % is not an artefact. Four
candidate artefacts were tested and all cleared:

- **zeros** — 1.96 % with `>0`, 2.15 % with zeros kept, 1.96 % with `>10`. Not the cause.
- **censoring** — pixels with no censored values are *less* significant (1.55 %), pixels with
  censoring more so (2.94 %). Removing censoring would lower, not raise, the figure.
- **cells with few valid years** — 1,174,447 of 1,206,233 pixels have all ten years; only 173
  have fewer than nine. Not the cause.
- **mixed vintages / resampling** — the rasters are already on the vegetation grid; `st_warp`
  is an identity operation here, and every year shares one NA mask (1,950,920 cells).

What is actually going on is **power**. Interannual variability swamps the trend:

```
median per-pixel residual SD  = 8.877 days
Sxx for 2011-2018,2020,2021   = 100.1,  sqrt = 10.005
=> median SE(slope)           = 0.889 d/yr
=> |slope| needed for p<.05   = t(0.975, 8) x 0.889 = 2.051 d/yr
```

Only 3.11 % of pixels have |slope| above that threshold, and 1.96 % clear the test — the two
agree. A ten-year series with 9-day noise simply cannot resolve a 0.7 d/yr trend pixel by
pixel.

**Do not compare 1.96 % to 5 %.** A 20-replicate permutation null (year labels shuffled within
each pixel, 200,000-pixel sample) gives a mean of **2.53 % significant, ranging 0.19 %–15.26 %
across replicates**. The correct null here is ≈ 2.5 %, not 5 %, and its enormous spread is the
point: because every pixel sees the same weather, the per-pixel tests are massively
pseudo-replicated and the effective number of independent tests is of order ten, not a million.
Removing the common year anomaly does not restore uniformity either (p-deciles 0.15, 0.26,
0.36, … 0.91), confirming the tests are conservative for reasons intrinsic to a 10-point
series with heavy-tailed residuals.

Three consistent framings of the same conclusion:

- observed 1.96 % vs permutation null 2.53 % → **no excess of significant pixels**;
- FDR-corrected → **zero pixels**;
- permutation test on the mean slope: 3 of 20 shuffles produced |mean slope| ≥ 0.715 →
  **p ≈ 0.15–0.19**, matching the landscape-scale p = 0.337 / 0.204 in order of magnitude.

**Robustness:** Theil–Sen on a 10,000-pixel sample gives mean −0.7165, median −0.6250, against
OLS −0.7123 on the same pixels. The point estimate is not an outlier artefact.

**Recommended wording:** the *direction* is consistent and worth reporting — 91.7 % of pixels
show earlier melt, which is far from the ~50 % a no-trend field would give. The *magnitude* is
−0.71 d/yr (7.1 d/decade) with a landscape-scale 95 % CI of [−2.24, +0.87] that includes zero.
The trend should be described as a consistent tendency that the ten-year record cannot
establish as statistically significant, and the 2030 projection explicitly labelled a
scenario. (`paper/discussion.qmd:19` already does this — the Abstract and Methods need to
match it.)

---

## 5. 2010, and 2022 onwards

### 5.1 2010 is usable and it helps

The user's note is correct — 2010 has a narrower field of view — but the cost is smaller than
feared. Working in image `(u, v)` space (independent of any georectification, so unaffected by
the truncated `georectified.csv`), over the common `v ≤ 1983` band:

| | valid pixels | % of the 2011–2021 common domain |
|---|---|---|
| 2011–2021 intersection | 6,336,119 | 100 % |
| 2010 | 5,822,932 | **91.68 %** |

Adding 2010 costs **8.32 %** of the analysis domain. Every other year covers 100 %.

Controlled comparison on the sub-domain I can rasterise (541,229 cells, same pixels, only the
year set changes):

| | n px | mean pixel slope | % negative | % p<.05 | landscape slope | landscape p | landscape 95 % CI |
|---|---|---|---|---|---|---|---|
| 2011–2021 (10 y) | 541,226 | −0.686 | 89.8 % | 2.13 % | −0.671 | 0.356 | [−2.25, +0.91] |
| **2010–2021 (11 y)** | 528,729 | **−0.761** | 90.8 % | **4.78 %** | **−0.808** | **0.190** | [−2.10, +0.48] |

2010 was a late-melt year (scene mean 163.1 vs 156.6 in 2011) and sits at the start of the
series, so it **strengthens** the estimate, more than doubles the individually-significant
fraction, narrows the CI and halves the p-value. **Recommendation: include 2010**, state the
8.3 % domain reduction and the lens change in the Methods, and show the 10-year result as a
sensitivity check. It is a genuine, defensible improvement rather than a fishing expedition —
but because it moves the headline number, it must be pre-declared as a reviewer-prompted
change, not presented as the original analysis.

### 5.2 2022+ would buy real power

Residual SD of the annual scene means is 6.75 days. Extending the series
(`landscape_trend_power.csv`):

| years | last year | SE of slope | smallest detectable slope | power at true −0.71 |
|---|---|---|---|---|
| 10 | 2021 | 0.675 | 1.556 | 0.15 |
| 11 | 2022 | 0.579 | 1.309 | 0.20 |
| 12 | 2023 | 0.506 | 1.129 | 0.25 |
| 13 | 2024 | 0.450 | 0.990 | 0.31 |
| 14 | 2025 | 0.404 | 0.881 | 0.37 |
| 16 | 2027 | 0.334 | 0.717 | 0.51 |
| 18 | 2029 | 0.283 | 0.600 | 0.66 |

Preparing 2022–2025 (four years) would raise power from 0.15 to 0.37 and drop the minimum
detectable slope below 0.9 d/yr — still not enough to make a −0.71 trend significant on its
own. Adding 2010 as well (12 points spanning 2010–2025) is the cheapest route to a materially
better test. **Reaching conventional significance for a trend this size needs ~18 years of
record.** That is worth saying plainly in the Discussion: it reframes the non-significance as
a property of the record length rather than a weakness of the method.

---

## 6. What the Methods must now say

1. Replace "The mean regression coefficient was −0.86, corresponding to an advance of 8.6 days
   per decade" with the reproducible figure (**−0.71 / 7.1 days per decade**, or the 11-year
   figure if 2010 is adopted), and state the filter rule explicitly: DOY 0 = no valid
   observation, dropped per-observation; pixels require ≥ 8 of 10 (11) years.
2. Correct the Abstract (`paper/index.qmd:53`) accordingly, and add that the landscape-scale
   trend is not statistically significant over this record.
3. State the DOY 120–230 encoding and its censoring consequences (6.89 % / 1.32 % of
   observations).
4. Report the Reviewer-2 package from `snowmelt_statistics.csv`, using the landscape-scale CI
   and not the pixel-level one, and reporting the permutation null (≈ 2.5 %) alongside the
   1.96 %, plus the zero-pixel FDR result.
5. Note that 2019 is absent (lodge construction) and, if 2010 is included, the lens change and
   its 8.3 % domain cost.
