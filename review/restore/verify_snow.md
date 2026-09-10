# Independent verification — snow-pipeline restoration

Verifier: independent agent, 2026-08-09. Everything below was re-run from the server data,
not read off the restoration report. Working scripts:
`/private/tmp/claude-501/-Users-okamoto-NIES-SasaSDMPaper--claude-worktrees-review-summary-ece-ddc69c/98ac7ecf-eafb-49c6-aa2d-9aab2998c13e/scratchpad/vsnow/v1..v16.R`

**Headline: the verdict `PARTIALLY_RESTORED` stands, and the central conclusion — that
−0.86 is unreproducible and −0.7146 is the number that belongs to the published predictors —
is CONFIRMED by independent recomputation. But three supporting claims are overstated or do
not replicate, and the restoration missed a half-pixel misregistration in the SDM's snowmelt
predictor that is a live defect in code that is still in the repository.**

---

## 1. What I confirmed exactly

I re-implemented the per-pixel OLS from scratch (mean-centred formulation, `v4_stats.R`),
independent of the delivered `pixel_ols()`. Every figure below matched to all printed digits.

| Claim | Reported | My recomputation |
|---|---|---|
| usable pixels (all-10-image, DOY>0, ≥8 yr) | 1,206,063 | 1,206,063 |
| mean pixel slope | −0.7146349 | −0.7146349 |
| median / sd | −0.6263736 / 0.6344 | −0.6263736 / 0.6344478 |
| % negative / % positive | 91.6543 / 8.2885 | 91.6543 / 8.2885 |
| % p<0.05 (neg / pos) | 1.96117 (1.95288 / 0.00829) | 1.9612 (1.9529 / 0.0083) |
| % surviving BH q<0.05 | 0 | 0 |
| median residual SD / median SE | 8.8767 / 0.88933 | 8.876709 / 0.889331 |
| min detectable slope; % exceeding | 2.0508; 3.11 % | 2.050801; 3.1124 % |
| landscape slope / SE / t / p | −0.68847 / 0.67475 / −1.0203 / 0.33743 | identical |
| landscape 95 % CI | [−2.2444, +0.8675] | [−2.2444, +0.8675] |
| DOY-0 counts by year | 160,169,152,150,157,152,165,165,3933,27996 | identical |
| obs at exactly 120 / ≥230 | 829,154 / 158,761 | 829,154 / 158,761 (all cells) |
| obs in DOY 1–10 | 96 | 96 |
| archived `snow_reg.tif` n / mean | 1,133,175 / −0.8635999 | 1,133,175 / −0.8635999 |
| `snow_reg` cell-wise cor with reconstruction | 0.511 | 0.510726 |
| fraction of shared cells agreeing to 1e−6 | 0.03 % | 0.0297 % |
| archive-footprint slopes (none / >0 / >10) | −0.8961 / −0.7125 / −0.7123 | −0.896103 / −0.712457 / −0.712265 |
| `snow_mean`, `snow_sd`, `snow_reg` share one non-NA count | yes → r²>0.2 filter not applied | all three = 1,133,175 ✔ |
| eight exact-MiB-multiple files | listed | scan reproduced the same eight, same byte sizes |

**Truncation is real and mid-record.** `georectified.csv` ends
`...\n2523,1268,733672.25,4051131.2` — the `z,B,G,R` fields of the last row are simply gone.
Aligned 2012 ends `3617,198`; an untruncated year (2011) ends cleanly at `5615,3743,0`.

**`fitted_*.tiff` reproduce exactly — and for more years than claimed.** The report cited five
years; I checked all eleven. For 2012–2018 and 2030 the value arrays are identical
(`maxdiff = 0`, cor 1.000000). For 2011, 2020 and 2021 a naive comparison fails because the
archived rasters are row-trimmed (1750 / 1745 / 1703 rows vs 1753); after cropping to the
archived extent these also give `maxdiff = 0`, cor 1.000000 (`v3_align.R`). So the claim is
CONFIRMED and slightly understated.

**`interpolate()` is characterised correctly.** `.Rhistory:251–277` — called with
`res = 1.0, max_dist = 1.0`, so `times = ceiling(1/1) = 1`: exactly one 3×3 modal `focal`
pass with `na.policy="only"`, writing into `data/snow/raw/`. Matches the report.

**Nothing reads `snow_reg`.** `data/terrain_features/` on disk contains only TPI, TRI, aspect,
roughness, slope, DEM and TWI — no snow layer. `sdm_tdm.R:13-28` globs that directory and adds
only `fitted_2012` / `fitted_2021` (and `fitted_2030` at line 265). Confirmed independently.

**Manuscript wording is quoted accurately.** `matmet.qmd:77` — "…used the predicted 2021 values
as explanatory variables in both models. The mean regression coefficient was −0.86…"; the
Abstract at `index.qmd:53` repeats "8.6 days per decade". The report's framing of the defect is
fair.

---

## 2. Claims that do NOT replicate

### 2.1 Stage-1 rasterisation agreement is overstated (DOWNGRADE)

Reported: "exact-match fraction 0.9989–0.9994, cor 0.99995–0.99999" over "541,229 cells
(44.9 %)".

I ran the delivered `rasterise_year()` verbatim (after the fix in §2.2) for 2011, 2016, 2021
(`v7_rast.R`):

```
2011: recon nonNA=628638  exact=0.98219  cor=0.9989650  meandiff=-0.0986
2016: recon nonNA=628638  exact=0.97923  cor=0.9989461  meandiff=-0.0939
2021: recon nonNA=628638  exact=0.97708  cor=0.9992244  meandiff=-0.1259
```

Exact agreement is **97.7–98.2 %, not 99.9 %**; correlation is **0.9989–0.9992, not 0.99997**.
Coverage is **628,638 cells = 52.1 %** of the 1,206,233 analysis cells, not 541,229 / 44.9 %.
(628,638 is consistent with the agent's own `restore_figures.md:196`, which reports 628,637
overlapping cells for the vegetation map — so the 541,229 figure in the snow report is
internally inconsistent with their other deliverable.)

I also tried the faithful `st_rasterize` route the original code uses (`v9_stars.R`): it lands
on its own bbox-derived grid (xmin 733116.5) and, after nearest-neighbour alignment, agrees on
only 62–68 % of cells. So 97.7–98.2 % is the *best* achievable, not a floor.

Conclusion: the aligned-CSV → raster stage is **substantially** but not near-perfectly
reproduced, over half the domain. Still a genuine restoration; the quoted precision is not.

Also: `georectified.csv`'s `v` index runs **529–1268**, not `0–1268`. The report mentions only
the upper cut.

### 2.2 The delivered pipeline's stage 1 does not run

```
> terra::focal(r, 3, terra::modal, na.policy="only", na.rm=TRUE)
Error: [focal] test failed
```
On terra 1.9.34 `terra::modal` has no method for the all-NA logical vector terra passes during
its internal probe. `snowmelt_pipeline.R:75` still contains this call. It is masked by
`STAGE_RASTERISE <- FALSE` (line 41), so the script completes — but the stage whose
verification is the report's headline restoration claim is **dead code as shipped**. The fix is
one word (`"modal"`, focal's built-in), and the same agent documented exactly this breakage in
`restore_figures.md:210-214` — it just was not carried into the snow deliverable.

### 2.3 The permutation null is Monte-Carlo noise, not a stable statistic (REFUTE as stated)

Reported: "a permutation null gives 2.53 %, not 5 %", range 0.19–15.26 %, "3 of 20 shuffles
exceeded the observed". The delivered CSV meanwhile says **2.748275**, so the report and its
own artefact already disagree.

My own 20-replicate run, same design, different seed (`v11_perm.R`, 200,000-pixel sample):

```
mean = 4.979 %   min = 0.067 %   max = 34.981 %
sorted: 0.07 0.25 0.31 0.36 0.49 0.49 0.65 0.67 0.96 0.98 1.03 1.20 1.22 3.67 5.14 5.49 9.29 10.61 21.72 34.98
6 of 20 shuffles had |mean slope| >= the observed 0.7146
```

The mean is dominated by two or three replicates; the **median is ~1.0 %**. Across three
independent 20-replicate runs the mean has come out at 2.53, 2.75 and 4.98 %. The sentence
"the correct null is ~2.5 %, not 5 %" is therefore not supported — my run gives essentially 5 %.
The *direction* of the argument survives (per-pixel tests are massively pseudo-replicated; the
mean-slope permutation p is 0.15–0.30, i.e. not significant either way), but **no specific
percentage from this permutation should go near the manuscript** without far more replicates
and a reported median plus MC interval.

### 2.4 Two smaller numeric slips

- `snow_sd` correlation: reported **0.361**, I measure **0.6766** on the shared 1,105,896 cells.
  (`snow_mean`: reported 0.954, I measure 0.9569 — fine.)
- Archive-footprint slope for the DOY>120 rule: reported **−0.6914**, I get **−0.7239**.
- The 829,154 / 355,537 censoring counts are computed over all cells but reported as
  percentages of usable-pixel observations (usable-pixel count is 829,138). Immaterial, but the
  denominator should be stated.

### 2.5 The 2010 figures were not independently reproducible here

The "91.68 % of the 2011–2021 intersection" figure depends on the truncated `georectified.csv`
and an image-space band restriction I could not re-derive cheaply. A crude direct check of the
aligned CSVs (`awk` over the full 21,026,304 rows) gives non-zero-DOY fractions of
**2010: 51.31 %, 2011: 58.25 %, 2013: 58.54 %** — i.e. 2010 carries ~12 % fewer valid pixels,
qualitatively consistent with the lens/FOV story but not a check of 91.68 %. Treat the 2010
sensitivity numbers (−0.808, p 0.190, 4.78 % significant) as **unverified**.

---

## 3. What the restoration missed — and it matters

### 3.1 CRITICAL: the SDM's snowmelt predictor is georeferenced half a pixel off

The report compares archived and reconstructed `fitted_*.tiff` by value and never looks at
their georeferencing. They are not on the same grid:

```
vege_2012_5x5.tiff   ext 732744  , 734545  , 4050316.25, 4052069.25   origin (0,   0.25)
fitted_2012.tiff     ext 732743.5, 734544.5, 4050316.75, 4052069.75   origin (0.5,-0.25)
snow_reg/mean/sd     origin (0, 0.25)  -- correct, aligned to vege
```

The published `fitted_*` rasters are **0.5 m west and 0.5 m north** of the vegetation maps,
the DEM predictors and `raw/*.tiff` — while `snow_mean` / `snow_sd` / `snow_reg` are correctly
aligned.

**Mechanism proven, not inferred.** `preprocess_snow_data.R:103` uses
`as_tibble(add_max = TRUE)` for the `snow_reg` branch (lines 45 and 73, which produce
`snow_mean` / `snow_sd`, use plain `as_tibble()`). With `add_max = TRUE`, stars returns the
cell *corner* coordinates; `rast()` at line 167 (and 186 for 2030) then treats them as cell
*centres*. Round-tripped on the actual vegetation raster (`v14.R`):

```
rast(as_tibble(vege, add_max=TRUE)) ext = 732743.5,734544.5,4050316.75,4052069.75
archived fitted_2012.tiff        ext = 732743.5,734544.5,4050316.75,4052069.75
```

Byte-for-byte the same extent. This is the bug.

**Consequence, quantified** (`v12_offset.R`). `sdm_tdm.R:22-28` and `:265` do
`rast("fitted_YYYY.tiff") %>% resample(vege12)` — terra's default is bilinear, so the half-cell
shift is not corrected, it is smeared in. Against the correctly re-registered layer:

| layer | mean diff | sd | % of cells differing >1 DOY | max |
|---|---|---|---|---|
| fitted_2012 | −0.058 | 1.84 d | 30.8 % | 68.9 d |
| fitted_2021 | −0.045 | 2.16 d | 33.2 % | 64.2 d |
| fitted_2030 | −0.045 | 3.64 d | 49.8 % | 145.0 d |

The manuscript's own headline effect is ~7–8 days per **decade**. A predictor perturbation of
sd 1.8–3.6 days, affecting a third to a half of all cells, is of the same order as the signal
being modelled. This does not invalidate the SDM — the error is small-scale and roughly
unbiased — but it is a real misregistration between predictor and response that a reviewer who
has read the code can find, it sits in a script that is still in the repository, and the
restoration reported the predictor chain as "fully restored and no model output changes".
That last clause is not safe as written: it is true of the *values*, not of the *registration*.

Recommended: fix `preprocess_snow_data.R` to use plain `as_tibble()` (or shift the extent by
+0.5 / −0.5 before writing), regenerate `fitted_*`, and re-run the SDM to show how little (or
much) moves. Until that is done, "the predictor chain is fully restored" should be phrased as
"the fitted values reproduce exactly; a half-pixel georeferencing error in the original write
step was found and corrected".

### 3.2 The misregistration is independent evidence for the two-vintage story

The report argues the vintage split from `.Rhistory` fragments and from correlations. The grid
origins are a cleaner fingerprint: `snow_reg/mean/sd` sit on the vegetation grid (they were
written by `write_stars`, which preserves the stars dimensions), `fitted_*` sit on the
`add_max` grid (written by `rast()` from a tibble). Two different write paths, visible in the
files themselves. Worth adding — it does not depend on reading a shell history.

I also tested whether the archived `snow_reg` is merely a *spatially shifted* version of the
reconstruction (`v6_shift.R`, all integer shifts −4..+4 m in x and y). Best correlation rises
only from 0.511 to 0.538 (at dx=−2). It is genuinely different data, not a registration
artefact. This strengthens the report's conclusion.

### 3.3 Minor gaps

- The 120–230 encoding claim holds: only **970** observations in the whole stack fall in
  DOY 1–119 (max value 232). Supports the censoring argument the report makes.
- `preprocess_snow_data.R` writes `snow_mean/sd/reg` to `data/terrain_features/`, which
  `sdm_tdm.R:13` globs into the predictor stack. They are not there now (they live in
  `data/snow/`), and `matmet.qmd:75` lists only the six topographic variables plus snowmelt
  DOY — consistent. But the model's predictor set is decided by what happened to be in that
  directory at run time, which is worth one sentence in the Methods.
- `preprocess_snow_data.R:119-126` is syntactically broken as committed (`partit` on its own
  line, `$coeffisients`). The `snow_reg` block in the repository could never have run — already
  consistent with the report's vintage conclusion, but it is direct evidence the report does
  not cite.

---

## 4. Bottom line for the manuscript

Unchanged and now independently confirmed: **−0.86 cannot be defended**; the coefficient of
the regressions that actually produced the published predictors is **−0.7146 d/yr (7.1 d per
decade)**, over 1,206,063 pixels, with a landscape-scale 95 % CI of **[−2.24, +0.87]** that
includes zero.

Add to the caution list: do not quote the permutation-null percentage (§2.3), and quote the
stage-1 restoration as "~98 % of cells reproduced over the 52 % of the domain the recovered
lookup table still covers" (§2.1), not 99.9 %.

Fix before resubmission: the half-pixel predictor offset (§3.1) and the one-word `focal`
breakage (§2.2).
