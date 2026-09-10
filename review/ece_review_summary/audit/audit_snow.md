# Audit: snowmelt & georectification subsystem

Scope: `scripts/sdm/preprocess_snow_data.R`, `ortho/georectify.R`, `scripts/sdm/image_to_csv.py`,
and the artefacts under `ortho/data/snow/**`, `data/snow/aligned/`, `ortho/data/gcp.csv`,
`ortho/data/params_optim.json`, `ortho/data/georectified.tiff`.

All numbers below were **recomputed**, not read off the manuscript. Commands and outputs are quoted.
Scratch scripts used: `/private/tmp/claude-501/.../scratchpad/s1.R … s18.R`.

Throughout I distinguish:
(a) what the code as written would do on a clean run,
(b) what the archived artefacts show actually happened,
(c) what the manuscript says.

---

## 0. Executive summary

| | finding |
|---|---|
| **S-1** | The manuscript's headline number (−0.86 d/yr, "8.6 days per decade") is the mean of `ortho/data/snow/snow_reg.tif`. That file is an **orphan**: nothing in the codebase reads it, it cannot have been produced by the code as written (the producing block is broken), and it is provably a **nine-layer** aggregate while the paper describes an eleven-year (ten-observation) regression. |
| **S-2** | Recomputing the same quantity from the archived `raw/` stack gives **−0.715 d/yr** (7.15 d/dec) under the rule that actually produced the model inputs, or −0.918 d/yr with zeros retained. Neither is −0.86. |
| **S-3** | The landscape-level snowmelt trend is **not statistically significant**: slope −0.92 d/yr, SE 0.66, p = 0.20, 95 % CI [−2.45, +0.61]. Only 1.96 % of pixels have an individually significant slope (below the 5 % null rate). This is exactly what Reviewer 2 asked for and the answer does not support the current framing. |
| **S-4** | `preprocess_snow_data.R` **cannot run**. Lines 119–126 are two broken top-level expressions (`partit` is not a function; a bare `summarise(...)` with `.`). |
| **S-5** | `fitted_*.tiff` **are** exactly reproducible from `raw/` (cor = 1.000000, max abs diff 1.5e−5). Good news: the actual model inputs are reproducible. |
| **S-6** | `fitted_*.tiff` sit on a grid **shifted half a pixel** (−0.5 m x, +0.5 m y) relative to the vegetation rasters. `sdm_*.R` corrects this with `resample(..., bilinear)`, which silently 2×2-averages the snow layer across the snowline. |
| **S-7** | The image→map mapping (`data/georectified.csv`) **does not exist** anywhere in the repository, and no code that produces it or `params_optim.json` is tracked. `georectify.R` and `image_to_csv.py` are both dead as written (missing input dir, missing module, wrong relative paths). |
| **S-8** | `ortho/data/georectified.tiff` is numerically corrupt (Inf values; finite values up to 2.4e33). |

---

## 1. Which years exist at each stage

```
$ ls data/snow/aligned/                      # camera-image space, 5616x3744 PNG
2010 2011 2012 2013 2014 2015 2016 2017 2018 ---- 2020 2021     (11 files)

$ ls ortho/data/snow/raw/                    # map space, 1 m GeoTIFF
---- 2011 2012 2013 2014 2015 2016 2017 2018 ---- 2020 2021     (10 files)

$ ls ortho/data/snow/fitted_*.tiff
---- 2011 2012 2013 2014 2015 2016 2017 2018 ---- 2020 2021 + 2030
```

* **2019 is absent at every stage**, including the aligned PNGs. Consistent with `paper/supplement.qmd:20`
  ("Data for 2019 are missing due to construction work at the mountain lodge"). Reviewer 2's observation
  is correct, and the Methods (`matmet.qmd:77`, "for each year from 2011 to 2021") do **not** mention it —
  only a figure caption in the supplement does.
* **2010 drops out between the aligned PNG and the GeoTIFF.** The PNG is real data, not a placeholder:

  ```
  MRD_snowfront_L_2010_0818_120-230_BW.png
      bands_identical=True  nonzero_px=10,787,950  min=120 max=231 mean=190.36  zero_px=10,238,354
  ```
  compared with e.g. 2011 (`nonzero_px=12,248,130`). 2010 has ~12 % less usable image area. No 2010 GeoTIFF,
  no 2010 fitted layer. The manuscript's stated window is 2011–2021, so this is a silent, undocumented
  exclusion of an available year rather than a contradiction — but it is a decision no reader can see.
* **No intermediate CSVs survive.** `ortho/data/snow/aligned/` does not exist; `ortho/data/snow/*.csv`
  (i.e. `fitted.csv`, written at line 150 and re-read at line 152) does not exist; `data/georectified.csv`
  does not exist. Every CSV hop in the pipeline is missing.

### Out-of-range values in the aligned PNGs

The filenames declare a DOY window of 120–230. They are not respected:

```
aligned PNG, pixels with 1 <= v < 120        aligned PNG, pixels with v > 231
  2010      0        2015  16,994              2012  120,708   (all other years 0)
  2011 19,455        2016  17,633
  2012 20,264        2017       0
  2013 22,125        2018       0
  2014 19,584        2020       0
                     2021  19,119
```

and they propagate into the rasters (`raw/`, valid cells only):

```
year  n(1..119)  n(0)     n(>231)
2011      16       160       0
2012     615       169    21,838     <- max value 232
2013       0       152       0
2014       1       150       0
2015      18       157       0
2016       8       152       0
2017       0       165       0
2018       0       165       0
2020       0     3,933       0
2021     312    27,996       0
```

Value `0` is a **no-data sentinel** (black in the PNG), not DOY 0. The script treats it inconsistently
(§2.4). Note the sharp rise in zeros in 2020 (3,933) and especially 2021 (27,996) — 2021 loses 2.3 % of
the scene, and those are exactly the pixels excluded from `fitted_2021.tiff` (§4).

---

## 2. Can `preprocess_snow_data.R` execute as written?

### 2.1 It parses, but lines 119–126 are two broken expressions

```
$ Rscript -e 'x <- parse("scripts/sdm/preprocess_snow_data.R", keep.source=TRUE); ...'
expr 19 lines 118 - 118 :  library(multidplyr)
expr 20 lines 119 - 121 :  s <- snow_reg %>% group_by(x, y) %>% partit
expr 21 lines 122 - 126 :  summarise(lm.coef = ..., lm.r2 = ..., lm.pval = ...)
```

The missing `%>%` after `partit` (line 121) splits the intended single pipeline into two top-level
statements. Executed verbatim:

```
does multidplyr export 'partit'?  FALSE
multidplyr exports matching 'parti':  cluster_assign_partition partition

--- executing lines 119-121 verbatim ---
[1] "ERROR: could not find function \"partit\""
--- executing lines 122-126 verbatim (standalone summarise) ---
[1] "ERROR: object '.' not found"
```

Consequence: **`s` is never created**, so line 128 (`snow_reg_ras <- s %>% ...`) also fails, and
`write_stars("data/terrain_features/snow_reg.tif")` at line 137 never executes.

### 2.2 Two further defects in the same block, latent behind the first

```
$ m <- lm(snowmelt ~ year, data = d)
m$coeffisients['year'] -> NULL          # line 123: typo for `coefficients`
m$adj.r.squared        -> NULL          # line 124: adj.r.squared lives on summary(m), not on lm
correct: coef -1   adj.r2 1
```

So even after fixing `partit` → `partition(cluster)` and the missing pipe, `lm.coef` and `lm.r2` would
both be `NULL` and the block would still not produce `snow_reg.tif`.

### 2.3 Other clean-run blockers

| line | problem |
|---|---|
| 1 | `setwd("~/doctoral_thesis/chap2/ortho/")` — does not exist on this machine (and differs from the roots used by `georectify.R` and `interpolate.R`). |
| 167 | bare `rast()` — `library(terra)` is never called in this file (only `terra::` prefixed calls elsewhere). Errors on a clean run. |
| 172 | `new_cluster(22)` on a 12-core machine. |
| 176 | `partition(cluster)` here **is** correct — so the typo at 121 is an isolated slip, not a systematic misunderstanding. |
| 67, 96, 137 | writes into `data/terrain_features/`, the directory `sdm_tbm.R:13` and `sdm_tdm.R` glob for predictors. A clean re-run would inject `snow_mean.tif` and `snow_sd.tif` as extra SDM predictors. That directory currently holds only `aspect, roughness, slope, tateyamadem_small, TPI, TRI, twi` — no snow file — confirming the archived snow rasters were moved to `ortho/data/snow/` by hand. |

### 2.4 Three different treatments of the `0` sentinel inside one script

| block | lines | filter |
|---|---|---|
| exploratory year-mean plot | 25–42 | `filter(snowmelt > 10)` |
| `snow_mean` | 45–67 | **none** — zeros averaged in |
| `snow_sd` | 73–96 | **none** |
| `snow_reg` / `fitted` / `fit_30` | 102–188 | `filter(snowmelt > 0)` |
| Fig. S1 sample | 193–227 | `filter(snowmelt > 10)` |

`snow_mean`/`snow_sd` are biased low by the retained zeros. (Immaterial in practice — nothing reads them —
but it is a real bug and it is on GitHub.)

---

## 3. What `snow_reg.tif` actually contains, and where −0.86 comes from

```
snow_reg.tif   ncell=3,114,549  nvalid=1,133,175  min=-12.1113  max=25.1355  mean=-0.8635999
  median = -0.8064516   SD = 0.8519743
  frac negative = 0.897488   frac positive = 0.102512   frac exactly 0 = 0
  quantiles 1/5/25/50/75/95/99 % = -3.1806 -2.1806 -1.3113 -0.8065 -0.3323 0.2113 0.7339
  mean x 10 = -8.635999
```

`mean = −0.8636` → **this is the source of `matmet.qmd:77` "The mean regression coefficient was −0.86"
and of `index.qmd:53` "advanced by an average of 8.6 days per decade".**

### 3.1 No r² filtering was applied

```
identical NA footprint mean/sd  : TRUE
identical NA footprint mean/reg : TRUE
n valid each: 1133175 1133175 1133175
```

`snow_reg.tif` has **exactly** the same footprint as `snow_mean.tif` and `snow_sd.tif`. The script's
`filter(lm.r2 > 0.2)` (line 130) would have removed most pixels — recomputed from `raw/`:

```
Per-pixel r2: mean=0.0985  median=0.0640 ; frac r2 > 0.2 = 0.1542
-> `filter(lm.r2 > 0.2)` would have retained only 185,897 of 1,205,860 pixels (15.4%)
   mean slope among r2>0.2 pixels = -1.5240
```

So: **no r² filter was applied to the archived `snow_reg.tif`**, and had one been applied the reported
coefficient would have been ≈ −1.52, not −0.86.

### 3.2 `snow_{mean,sd,reg}.tif` are nine-layer aggregates — they cannot come from the current `raw/`

Recomputing exactly what lines 11–19 + 45–60 do (read all `raw/*.tiff`, `st_warp` to the vege grid —
a no-op, since the grids are already identical — `drop_na()`, average):

```
identical NA masks across years: TRUE
complete.cases (drop_na equivalent): 1,206,233        <-- vs 1,133,175 in snow_mean.tif
```

Direct cell-by-cell comparison (grids are co-registered on the integer lattice; a ±1 m / ±0.5 m shift scan
and a vertical-flip test were run and none improved the fit):

```
snow_mean vs recomputed 10-yr mean : n=1,105,900  cor=0.9541  mean|diff| = 5.28 days
snow_sd   vs recomputed 10-yr sd   :              cor=0.3610
snow_reg  vs recomputed 10-yr slope:              cor=0.2385  (0.5106 against the >0-filtered slope)
```

**Decisive evidence that the archived files are aggregates of exactly nine layers.** Because the inputs are
integers, an N-layer mean is a multiple of 1/N and an N-layer variance satisfies `N(N-1)·var ∈ ℤ`:

```
if N = 9 years: frac(9*mean - sum10) near-integer = 1.0000     <-- exact
if N =10 years: 0.1101      if N =12: 0.3339      if N =13: 0.1101   (all = the 1/9 lattice)

sd assuming n = 8 : frac near-integer of n(n-1)var = 0.3339
sd assuming n = 9 : 1.0000                                       <-- exact
sd assuming n =10 : 0.5104
```

So `snow_mean.tif` and `snow_sd.tif` summarise **nine** yearly layers. `raw/` holds **ten**.

I then tested whether the nine layers could be "eight of the current ten, plus one unknown layer":
for the {2011…2018} + X hypothesis, the implied ninth layer X = 9·mean − Σ(2011…2018) is integer everywhere
(as it must be) but the resulting nine-value SD matches `snow_sd.tif` for only **1.6 %** of pixels. So the
underlying *yearly fields themselves* differ from the current `raw/` files, not just the year list.

**Conclusion (b vs a vs c):** `snow_{mean,sd,reg}.tif` are leftovers from an **earlier georectification
vintage** with a nine-year stack — consistent with `ortho/.Rhistory`, which preserves a completely
different pipeline (`/media/okamoto/HDD3TB/tateyama/mrd_snowmelt/step2_ortho/`, a `fields::Tps` thin-plate-spline
warp on a `gcp.csv` with schema `org_x/org_y/sim_x/sim_y`, writing `snowmelt_ortho/*.csv` and a `TPS_rmse.csv`).
None of that code is tracked. The manuscript's −0.86 therefore describes a data product that no longer
exists and that no current script can regenerate.

*(Side note from `.Rhistory`, in the untracked TPS code: `melt2 <- melt *255 %>% round(digits = 0)`.
Because `%>%` binds tighter than `*`, this is `melt * (255 %>% round(0))` — the rounding is applied to the
constant 255, and the following `as.integer()` then **truncates** the DOY instead of rounding it.)*

---

## 4. How `fitted_*.tiff` were produced — and they DO reproduce

Lines 143–170 nest per pixel, fit `lm(snowmelt ~ year)` on the `filter(snowmelt > 0)` long table,
and write `broom::augment()`'s `.fitted` per year. Lines 174–188 predict 2030 and clamp.

Because `fitted_2013 − fitted_2012` is by construction the exact per-pixel OLS slope, this is a clean test.
Matching **by array index** (not by coordinate — see §5 for why):

```
n matched: 1,206,062
no filter    cor=0.480866  max|d|=13.51     frac|d|<1e-4=0.97378   mean(recomp)=-0.9186
>0 filter    cor=1.000000  max|d|=1.483e-05 frac|d|<1e-4=1.00000   mean(recomp)=-0.7147
                                                                   mean(fitted)  =-0.7147
```

**`fitted_*.tiff` are exactly reproducible** (max discrepancy 1.5e−5 = float32 rounding) from the current
`raw/` stack with the `snowmelt > 0` rule. This is the one part of the subsystem that is clean.

### 4.1 The per-pixel model matches the manuscript, but the reported coefficient does not

`matmet.qmd:77` describes "a linear regression to per-pixel snowmelt DOYs for each year from 2011 to 2021"
and "the predicted 2021 values as explanatory variables". That is what `fitted_2021.tiff` is.
But the mean slope of that exact model is **−0.7147 d/yr (7.15 d/dec)**, not −0.86 / 8.6.
The number in the paper comes from the orphan `snow_reg.tif` (§3), which is a *different* model on
*different* data. **The reported trend and the trend actually fed to the SDMs are inconsistent.**

### 4.2 2030 extrapolation and clamping

Reproduced exactly:

```
max |fitted_2030 - clamp(a + b*2030, 0, 255)| = 7.59e-06   cor = 1
max |fitted_2030 - UNclamped prediction|      = 378
```

Clamping effect:

```
pixels with raw prediction < 0    : 12   (0.0010%)
pixels with raw prediction > 255  :  4   (0.0003%)
raw prediction range              : [-378.0, 403.1]
clamped mean 160.6232   unclamped mean 160.6229
n pixels where clamping changed value: 16
```

* The clamp changes **16 of 1,206,082 pixels** — negligible for the layer statistics, but it does hide
  16 pixels whose extrapolated DOY is physically absurd (−378, +403), i.e. it silences a diagnostic rather
  than fixing anything.
* The clamp is applied **only to 2030** (lines 184–185). The historical `fitted_*.tiff` are written
  unclamped, and two of them leave the observable range: `fitted_2011` max = 238.46 and `fitted_2020`
  max = 238.72, against an observed maximum of 232. Asymmetric treatment.

### 4.3 The 2021 and 2030 layers cover different areas

```
fitted_2012   1,206,064 valid   dim 1753x1801
fitted_2021   1,178,230 valid   dim 1703x1801
fitted_2030   1,206,083 valid   dim 1753x1801
of 1,206,083 2030-valid pixels, 27,853 are NA in fitted_2021
```

This follows directly from `filter(snowmelt > 0)`: 2021 had 27,996 no-data pixels, so those pixels have no
*observation* in 2021 and therefore no `.fitted` row for 2021 — but they do have a 2030 *prediction*.
**The 2021 and 2030 habitat-suitability maps are therefore computed over different domains**
(27,853 m² of 1 m pixels), which is the mechanism behind the ~397 k vs ~408 k prediction-raster
discrepancy noted elsewhere in this audit. `results.qmd:95` compares "27,049 m² newly suitable" against a
2021 baseline that does not exist over 27,853 m² of that same map.

---

## 5. Pixel counts, spatial domains, alignment

| layer | dim (row×col) | res | ext (xmin, xmax, ymin, ymax) | CRS | valid cells |
|---|---|---|---|---|---|
| `raw/*.tiff` (all 10) | 1753 × 1801 | 1 m | 732744.00, 734545.00, 4050316.25, 4052069.25 | 6690 | 1,206,233 |
| `vege_{2012,2021}_5x5.tiff` | 1753 × 1801 | 1 m | **identical to raw** | 6690 | 1,206,233 |
| `fitted_2012/13/…/18, 2030` | 1753 × 1801 | 1 m | 732743.**5**, 734544.5, 4050316.**75**, 4052069.75 | 6690 | ~1,206,07x |
| `fitted_2011` | 1750 × 1801 | 1 m | …, 4052066.75 | 6690 | 1,202,593 |
| `fitted_2020` | 1745 × 1801 | 1 m | …, ymin 4050324.75 | 6690 | 1,202,297 |
| `fitted_2021` | 1703 × 1801 | 1 m | …, ymax 4052019.75 | 6690 | 1,178,230 |
| `snow_{mean,sd,reg}.tif` | 1739 × 1791 | 1 m | 732750.00, 734541.00, 4050329.25, 4052068.25 | 6690 | 1,133,175 |
| `georectified.tiff` | 3505 × 3602 | 0.5 m | identical bbox to raw | 6690 | 4.77 M / band |
| `terrain_features/*.tif` | 1198 × 1263 | **4.97 × 6.16 m** | 729277.6, 735554.7, 4048457.5, 4055837.2 | **3099** | — |

### 5.1 A half-pixel misregistration in every `fitted_*.tiff`

```
raw         ext=[732744.0000 734545.0000 4050316.2500 4052069.2500] centre1=(732744.5000, 4052068.7500)
vege        ext=[732744.0000 734545.0000 4050316.2500 4052069.2500] centre1=(732744.5000, 4052068.7500)
fitted_2012 ext=[732743.5000 734544.5000 4050316.7500 4052069.7500] centre1=(732744.0000, 4052069.2500)
```

`fitted_*` cell centres are at (raw − 0.5 m, raw + 0.5 m). Cause: `stars::as_tibble(..., add_max = TRUE)`
(line 103) emits cell **bounds**, and `terra::rast(<data.frame>)` (line 167) interprets the x/y columns as
cell **centres**. The three layers with non-standard extents (2011, 2020, 2021) are a second, unrelated
symptom of the same construction: `terra::rast(xyz)` sizes the grid to the bounding box of the rows
actually present, so each year's raster is cropped to that year's `snowmelt > 0` footprint.

### 5.2 What the SDM scripts do with it

`sdm_tbm.R:22-28,228-230` and `sdm_tdm.R:22-28,265-267` all call `resample(<fitted>, vege12)` with the
default method, which is bilinear:

```
$ Rscript -e '... isTRUE(all.equal(values(resample(f,v)), values(resample(f,v,method="bilinear"))))'
default==bilinear: TRUE
```

Because the offset is exactly (±0.5, ±0.5), **each model input value is the arithmetic mean of a 2×2 block
of fitted cells**. That is a 1-m low-pass filter applied to the single most important predictor, and it is
worst exactly at the snowline, where the field is discontinuous:

```
 fitted_2012: valid=1,206,064 -> bilinear 1,206,057 ; nearest 1,206,057 ; max|bilinear-nearest| = 50.38 DOY
 fitted_2021: valid=1,178,230 -> 1,178,227 / 1,178,227 ; max|diff| = 58.51 DOY
 fitted_2030: valid=1,206,083 -> 1,206,076 / 1,206,076 ; max|diff| = 131.74 DOY
```

Layer means also shift: 2030 mean 160.6233 (nearest) → 160.6670 (bilinear).

### 5.3 Agreement with the vegetation rasters

```
vege ext == raw snow ext : TRUE
vege valid pixels              : 1,206,233
vege & snow_2021 both valid    : 1,159,498   (46,735 vegetation pixels have no 2021 snow value)
vege & snow_2030 both valid    : 1,185,614
Sasa-2021 pixels total: 8,547 ; of which snow_2021 is NA: 165
```

165 of 8,547 Sasa-2021 presence cells are silently dropped from model training by `drop_na()`
(`sdm_tbm.R:50`). `matmet.qmd:77` claims the uncertain (foreground-snow) areas "showed minimal overlap
with the *Sasa* distribution" — 165/8547 = 1.9 %, so that claim is quantitatively defensible, but it has
never been stated as a number.

The terrain predictors are on an entirely different CRS (3099) and an anisotropic 4.97 × 6.16 m grid; they
are brought onto the vege grid by `resample()` in `sdm_tbm.R:13-19`. That belongs to the terrain audit but
is noted here because it means *every* predictor in the model has been resampled at least once, and the
1 m "resolution" claimed throughout the manuscript is nominal for six of the eight predictors.

---

## 6. Georectification: what exists, what does not

### 6.1 `ortho/georectify.R` cannot run as written

* It is only the **last** step — point→raster: `st_rasterize(dx=1, dy=1)` followed by
  `ceiling(max_dist/res) = 1` iteration of `terra::focal(3, terra::modal, na.policy="only")` to fill
  one-cell gaps. It does not perform any georectification itself.
* Line 8/18: it depends on `data/georectified.csv`, the dense `u,v → x,y,z` lookup. **That file does not
  exist anywhere in the repository.** Nor does any script that writes it.
* Line 62: `list.files("data/snow/aligned/", "*.csv", ...)` resolves (after the `setwd` at line 1) to
  `ortho/data/snow/aligned/` — **that directory does not exist**; the PNGs live at `data/snow/aligned/`
  and the CSVs were never committed. `files` is `character(0)`, so `1:length(files)` is `c(1, 0)` and the
  loop errors on the second iteration after failing on the first.
* `st_rasterize` keeps one source point per output cell (last-wins), not a mean or mode of the several
  image pixels that fall in a 1 m cell. The single `focal(modal)` pass then fills remaining holes. Neither
  choice is described in the manuscript.

### 6.2 `scripts/sdm/image_to_csv.py` cannot run as written

* `import cv2` — OpenCV is not installed in this environment, and there is no requirements/env file.
* `glob("ortho/data/snow/aligned/*.png")` — wrong relative path (PNGs are at `data/snow/aligned/`).
  The glob returns `[]`, the loop body never executes, and the script exits 0 having done nothing.
* `import pandas as pd` — also unavailable here.
* Lines 15–23 (the vegetation `.npy` → CSV branch) are commented out, so the vegetation and snow branches
  of the same file are in different states.
* Reads with `cv2.IMREAD_GRAYSCALE`, i.e. a BGR→gray weighted sum. Verified harmless: all 11 aligned PNGs
  have `bands_identical = True`, so the byte value passes through unchanged and **the grayscale byte is
  the DOY directly** (no scaling). Good — but undocumented.

### 6.3 The camera model — what the transform actually is

`ortho/data/params_optim.json` (720 bytes, **not read or written by any tracked code**):

```
camera position   x = 732731, y = 4051171, z = 2458   (EPSG:6690 + m a.s.l.)
image             w = 5616, h = 3744 ; principal point cx = 2808.0, cy = 1872.0
orientation       fov = 72.8439 deg, pan = 96.6314, tilt = 0.02607, roll = 0.11609
distortion        a1 a2 ; k1..k6 (rational radial) ; p1 p2 (tangential) ; s1..s4 (thin prism)
"error"           4.342243224315759
```

This is an OpenCV-style rational + thin-prism model, i.e. a **world → image** projection. I implemented it
and tested it against the 482 correspondences in `ortho/data/gcp.csv`
(columns `,u,v,x,y,z`; u ∈ [81, 5406], v ∈ [615, 2198]):

```
best of 48 convention variants tried:
  median reprojection error = 28.17 px   (fov = horizontal, f = (w/2)/tan(fov/2),
  pan clockwise from north in degrees, distortion applied forward, v = cy + f*y'')
  without distortion: 43.30 px
```

So the convention family is right (28 px on a 5616 px image = 0.5 %), but the exact one is not recoverable
by inspection — the stored `"error": 4.34` shows the true convention reaches ~4 px. The roles of `a1`/`a2`
and the rotation order are undocumented. **There is no code in the repository that defines this model.**

### 6.4 Is the transform invertible, and what records the mapping?

* **world → image**: analytic and exact, given the convention (a projection plus a forward distortion
  polynomial).
* **image → world**: *not* a closed-form inverse. It requires (i) undistorting `(u,v)`, which for the
  rational + thin-prism model is itself iterative, and (ii) intersecting the resulting ray with terrain.
  The DEM needed for (ii) exists: `ortho/data/mrd_dem_1m.tiff` (132 MB, 1 m) and `ortho/data/dem_small.tiff`.
  So the mapping **is** reconstructible in principle, but only by writing new code and re-deriving the
  convention.

Artefacts that record the mapping today:

| artefact | what it holds | usable as the mapping? |
|---|---|---|
| `ortho/data/gcp.csv` | 482 sparse `(u,v) ↔ (x,y,z)` correspondences | only as control/validation, or to re-fit a TPS warp |
| `ortho/data/params_optim.json` | the fitted camera model | yes, **if** the convention is re-derived |
| `ortho/data/georectified.tiff` | 0.5 m 3-band ortho-RGB, 3505 × 3602, bbox identical to the vege/snow grid | **no** — it stores colour, not `(u,v)`; it cannot be inverted |
| `ortho/data/matched.png` | GCP match visualisation | no |
| `data/georectified.csv` | the dense `u,v → x,y,z` table | **absent** |
| `ortho/.Rhistory` | the only surviving record of the *earlier* TPS-based georectification | narrative only, untracked, different `gcp.csv` schema |

### 6.5 `georectified.tiff` is numerically corrupt

```
n non-finite (Inf/-Inf) per band: 250 250 3751
n NA per band: 7,859,084 7,829,103 7,809,248 ; n finite: 4,765,676 4,795,657 4,812,011
finite range (band B): -8.02e+22 ... 2.44e+33
```

An 8-bit RGB ortho should be in [0, 255]. The blow-up comes from the `focal(3, mean, na.policy="only")`
gap-filling in `scripts/vegetation_classification/utils/interpolate.R:53` operating on a layer that already
contained ±Inf. Nothing in the SDM pipeline consumes this file, but it is the published orthophoto.

---

## 7. Reviewer 2's pixel-level slope statistics — computed

Yes, everything the reviewer asks for can be computed from `ortho/data/snow/raw/`. Here it is.
Ten observation years (2011–2018, 2020, 2021), 1 m pixels, `complete.cases` across years.

### A. OLS slope per pixel, zeros retained (n = 1,206,233)

```
mean   = -0.9184 d/yr      (-9.18 days/decade)
median = -0.6424 d/yr      (-6.42 days/decade)
SD     =  1.4345
95% CI of the mean (naive, iid) = [-0.9210, -0.9159]
IQR = [-1.0969, -0.3037] ; 2.5-97.5 % = [-6.3227, 0.2787]
frac negative = 0.9202 ; frac positive = 0.0797 ; frac exactly 0 = 0.00012
```

### B. OLS slope per pixel, zeros excluded — *the rule that produced the model inputs* (n = 1,206,082)

```
mean   = -0.7147 d/yr      (-7.15 days/decade)
median = -0.6264 d/yr      (-6.26 days/decade)
SD     =  0.6353
95% CI of the mean (naive, iid) = [-0.7158, -0.7136]
IQR = [-1.0599, -0.2952] ; 2.5-97.5 % = [-2.1299, 0.2917]
frac negative = 0.9165 ; frac positive = 0.0829 ; frac exactly 0 = 0.00057

pixels by number of usable years:
  n_i:  0     1    2    4    6    7    8      9        10
      150     1    1    1    6   11    3   31,613  1,174,447
```

**The naive CIs above are worthless as inference** — 1.2 M pixels from one scene are not 1.2 M independent
observations. They are reported because the reviewer asked; they must be presented with that caveat.

### C. Per-pixel significance (model B)

```
frac of pixels with p < 0.05 : 0.0196     (of those, 99.58 % are negative)
per-pixel r2: mean 0.0985, median 0.0640 ; frac r2 > 0.2 = 0.1542
```

Only **2 %** of pixels show an individually significant trend — *less* than the 5 % expected under a pure
null. The per-pixel regressions carry essentially no evidence of a trend; what carries the signal is the
consistency of the sign (92 % negative), which is a spatially-correlated, not an independent, fact.

### D. Landscape-level trend (the honest test): regression on the 10 annual spatial means

```
annual spatial means, zeros retained:
 2011   2012   2013   2014   2015   2016   2017   2018   2020   2021
171.46 179.54 172.91 179.06 165.88 157.92 176.83 164.97 170.11 165.56

slope = -0.9184 d/yr   SE = 0.6633   t = -1.385   p = 0.2035
95% CI = [-2.4479, +0.6110]   R2 = 0.193

zeros excluded:
 171.49 179.57 172.94 179.08 165.90 157.94 176.86 164.99 170.67 169.49
slope = -0.6885 d/yr   SE = 0.6748   t = -1.020   p = 0.3374
95% CI = [-2.2445, +0.8675]   R2 = 0.115
```

**The overall temporal trend in snowmelt DOY is not statistically significant over 2011–2021, under either
treatment of the no-data sentinel.** The confidence interval spans a decade-scale advance of 24 days and a
retreat of 6–9 days. This is the direct answer to Reviewer 2's third major comment, and it is not
compatible with the current wording ("snowmelt timing advanced by an average of 8.6 days per decade",
`index.qmd:53`) unless that sentence is explicitly reframed as a descriptive mean of pixel slopes with no
claim of significance.

---

## 8. Supplementary Figure S1 was not produced by the archived code

`supplement.qmd:20` describes Fig. S1 as "Changes in snowmelt Day of Year (DOY) relative to 2011 (set as 0).
A total of 1,000 pixels were randomly sampled". The archived `ortho/snowmelt_shifting.png` is indeed a
**boxplot of anomalies relative to 2011**, with 2012–2021 on the x axis (2011 and 2019 absent).

But `preprocess_snow_data.R:211-227` produces a **scatter plot of absolute DOY with `geom_smooth(method=lm)`,
coloured by pixel id** (`aes(x = year, y = snowmelt, color = id)`, `labs(y = "Snowmelt Day of Year")`),
and it filters `snowmelt > 10`. The archived figure cannot come from this code. Another block that was
edited after the figure was made.

---

## 9. Retracted / corrected prior leads

| lead (`known_leads.md`) | verdict |
|---|---|
| "`preprocess_snow_data.R` lines ~118-126 appear to be **syntactically invalid**" | **Partly wrong, conclusion right.** The file *parses* cleanly (34 top-level expressions). The block fails at *evaluation*: `partit` is an undefined symbol and lines 122-126 form a separate top-level `summarise()` call. Net effect is as the lead guessed — the block never ran — but "syntactically invalid" is not accurate and should not be said to a reviewer. |
| "`preprocess_snow_data.R` writes `snow_{mean,sd,reg}.tif` INTO `data/terrain_features/` … running things in order may inject extra predictors" | **Confirmed for `snow_mean` and `snow_sd`.** Not for `snow_reg` — that write is unreachable. `terrain_features/` currently contains no snow layer, so the archived files were relocated by hand. |
| "`.gitignore` does not exclude `*.tif`, so `ortho/data/snow/snow_{mean,sd,reg}.tif` ARE published on GitHub" | **Confirmed and materially worse than stated.** Those three published files are unreproducible nine-layer orphans, and one of them is the source of the manuscript's headline coefficient. |
| "2021 and 2030 prediction rasters reportedly have different valid-pixel counts (~397k vs ~408k), so the maps cover different domains" | **Confirmed, with the mechanism identified**: `filter(snowmelt > 0)` removes 2021's 27,996 no-data pixels from `fitted_2021.tiff` but not from `fitted_2030.tiff` (27,853 pixels valid in 2030, NA in 2021). |

---

## 10. What to do (ordered by exposure)

1. **Decide what −0.86 is.** Either (i) drop `snow_reg.tif` and re-report the coefficient from the model
   actually used (−0.715 d/yr, 7.15 d/dec, or −0.918 / 9.18 with zeros retained), or (ii) locate the
   nine-year stack that produced it. Option (i) is the only reproducible route. This changes an abstract
   number.
2. **Add the uncertainty Reviewer 2 asked for** (§7 A–D), and reframe the trend claim to match a
   non-significant landscape-level test.
3. **Fix lines 119–126** (`partition(cluster) %>%`, `coefficients`, `summary(...)$adj.r.squared`), and
   decide explicitly whether an r² filter is wanted — it is currently in the code and absent from the
   artefact, and applying it moves the coefficient to −1.52.
4. **State the 2019 gap and the 2010 exclusion in the Methods**, not only in a figure caption.
5. **Make the no-data sentinel explicit**: set `0 → NA` once, at ingest, and delete the three different
   thresholds (`>0`, `>10`, none).
6. **Fix the half-pixel offset** in the `fitted_*` writer (`terra::rast(xyz)` expects centres) and re-run,
   or at minimum state that the snow predictor is bilinearly resampled onto the vegetation grid.
7. **Restore the georectification chain**: commit whatever produced `params_optim.json` and
   `georectified.csv`, or document the model convention well enough that §6.3 can be re-derived. Without
   this, "precise georectification" (`intro.qmd:9`) is not checkable by any reviewer.
8. **Fix `image_to_csv.py` and `georectify.R` paths**, pin `cv2`/`pandas`, and remove the commented-out
   branch.
9. **Regenerate or remove `georectified.tiff`** — it currently contains Inf and 1e33 values.
10. **Clamp consistently or not at all**, and surface the 16 absurd 2030 extrapolations rather than
    silencing them.
