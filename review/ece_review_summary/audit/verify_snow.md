# Adversarial verification — snow & georectification subsystem

All commands below were re-run from scratch in `/Users/okamoto/NIES/SasaSDMPaper`
(R 4.5.2 / terra / stars / sf; Python 3.14 + osgeo). Nothing was taken on trust from
the auditor's report. Scratch scripts live in
`/private/tmp/claude-501/.../scratchpad/v/`.

## 0. Baseline reconstruction (used by everything below)

`v2.R` reproduces lines 8–19 of `scripts/sdm/preprocess_snow_data.R`:

    vege <- read_stars("data/vege_2012_5x5.tiff")
    snow <- list.files("data/snow/raw/") ... st_warp(vege) ... reduce(c)

    nrow all: 3157153
    nrow drop_na: 1206233
    zeros per year:
     2011  2012  2013  2014  2015  2016  2017  2018  2020  2021
      160   169   152   150   157   152   165   165  3933 27996
    NA per year (identical for every year): 1950920

Important detail nobody stated: `raw/*.tiff` are **already on the vege grid**
(ext `[732744, 734545, 4050316.25, 4052069.25]`, 1753x1801, res 1), so
`st_warp(vege)` is a no-op and the reconstruction is exact, not approximate.
All ten years share the same NA footprint, so `drop_na()` loses nothing.

---

## 1. `snow-reg-orphan-086` — **CONFIRMED (and strengthened)**

    snow_reg.tif: n= 1133175  mean= -0.8635999  median= -0.8064516  sd= 0.8519743
                  frac neg= 0.897488  frac pos= 0.102512   x10 = -8.635999
    snow_mean.tif: n valid= 1133175 mean= 171.7633
    snow_sd.tif  : n valid= 1133175 mean= 9.677257
    footprint identical mean/sd : TRUE ; mean/reg: TRUE

`grep -rIn "snow_mean|snow_sd|snow_reg" --include='*.R' --include='*.py' --include='*.qmd'
--include='*.md' --include='*.tex' --include='*.Rhistory'` → only hits are inside
`preprocess_snow_data.R`. **Confirmed.** (Nuance: `sdm_tbm.R:13` / `sdm_tdm.R` glob
`data/terrain_features/*.tif`, so had the files stayed where the script writes them they
*would* have entered the predictor stack by glob, without being named.)

### Correction to the auditor's evidence

The auditor's "proof of nine layers" printout is a test on `snow_mean`/`snow_sd`, not on
`snow_reg`; a slope is not an N-layer mean, and the direct test fails:

    --- lattice test on snow_reg.tif (N*value integer?) ---
      N= 6 0.0036   N= 8 0.0068   N= 9 0.0020   N=10 0.0167   N=12 0.0068

The correct test for `snow_mean` does hold, decisively:

    --- N-layer lattice test on snow_mean.tif ---
      N= 6 0.3340  N= 7 0.1102  N= 8 0.1102  N= 9 1.0000  N=10 0.1102  N=12 0.3340

### New: *which* nine years

An OLS slope on integer y has denominator dividing `D = N*Sxx = N*sum(x^2) - (sum x)^2`.
Minimal denominators of the 7,901 unique `snow_reg` values cluster on multiples of 31:

    minimal denominator: 620 (116)  155 (73)  310 (71)  124 (34)  62 (20)  31 (16)

and `D = 620` is exactly the nine years **2011–2018 + 2020** (`9*sum(x^2)-(sum x)^2 = 620`);
the current ten years give `D = 1001`. Among all subsets of 2010–2021 with `D = 620`, the
only one containing no year for which a raster exists (2010, 2019) is that set.
**snow_reg.tif is a nine-year regression that predates the addition of 2021.**

### New: `snow_mean.tif` and `snow_sd.tif` are equally orphaned

No leave-one-out nine-year recomputation reproduces them (`v4.R`):

    drop 2011 cor=0.9527 mean|d|=5.42 ... drop 2021 cor=0.9570 mean|d|=4.95   (mean)
    drop 2011 cor=0.3560 mean|d|=2.49 ... drop 2021 cor=0.6747 mean|d|=1.50   (sd)

and no integer-pixel shift rescues it (best of dx,dy in -4..4 is dx=-3: cor 0.965,
mean|d| 4.41). For `snow_reg` (`v3/v18`):

    10yr zeros-in  cor=0.2385  10yr zeros-out cor=0.5106
    9yr  zeros-in  cor=0.5646  9yr  zeros-out cor=0.5848 (recomputed mean -0.8719/-0.8746)

So all three files were computed from **different yearly fields** than the archived
`raw/*.tiff` — an earlier georectification vintage.

### New: an independent proof that snow_reg.tif did not come from lines 102–137

`st_as_stars()` treats its x/y columns as cell **centres** (verified, `v19.R`: feeding
edge coordinates shifts the bbox by exactly 0.5). Lines 102–134 build `snow_reg` with
`as_tibble(add_max = T)`, i.e. edge coordinates, so the written raster would sit on the
0.5-phase grid, like every `fitted_*.tiff` (`xmin(fitted_2012) = 732743.5`).
It does not: `xmin(snow_reg.tif) = 732750`, the same phase as `snow_mean.tif` and
`vege_2012_5x5.tiff`.

---

## 2. `snow-trend-not-significant` — **CONFIRMED**

`v12.R` / `v13.R`, vectorised per-pixel OLS over 1.2M pixels:

    == A. all 10 years, zeros retained ==
      n=1206233 mean=-0.9184 median=-0.6424 SD=1.4345 frac<0=0.9196 frac>0=0.0797
      r2 mean=0.1046 median=0.0670 frac>0.2=0.1763
      frac p<0.05 = 0.0215 ; among sig frac neg = 0.9979
    == B. zeros excluded (>0 rule, the one used for fitted_*) ==
      n=1206081 mean=-0.7147 median=-0.6264 SD=0.6348 frac<0=0.9165 frac>0=0.0829
      r2 mean=0.0985 median=0.0640 frac>0.2=0.1542
      frac p<0.05 = 0.0196 ; among sig frac neg = 0.9958
    == C. landscape trend on n=10 annual spatial means ==
      zeros retained: slope=-0.9184 SE=0.6633 t=-1.385 p=0.2035 CI=[-2.4479,0.6110] R2=0.193
      zeros excluded: slope=-0.6885 SE=0.6748 t=-1.020 p=0.3374 CI=[-2.2445,0.8675] R2=0.115
      annual means: 171.46 179.54 172.91 179.06 165.88 157.92 176.83 164.97 170.11 165.56

Every number matches to the last digit reported. Trivial corrections: frac<0 is 0.9196
(not 0.9202) and the SD under the `>0` rule is 0.6348 (not 0.6353).

**Addition the auditor missed:** the *adjusted* R² of the per-pixel fits is **negative on
average** (mean −0.0146 under the `>0` rule, −0.0073 with zeros retained); only 6.24% of
pixels have adj. R² > 0.2. A linear year term explains, on average, less than nothing.

---

## 3. `snow-partit-broken-block` — **CONFIRMED**

    parse class: expression  n expr: 34
    expr 19 lines 118-118 : library(multidplyr)
    expr 20 lines 119-121 : s <- snow_reg %>% | group_by(x, y) %>% | partit
    expr 21 lines 122-126 : summarise( lm.coef = ... )

    multidplyr exports 'partit'?  FALSE ; matching 'parti': partition cluster_assign_partition
    eval 119-121 -> "could not find function \"partit\""
    eval 122-126 -> "object '.' not found"
    exists('s') after? FALSE
    m$coeffisients['year'] -> NULL ; m$adj.r.squared -> NULL

Also confirmed: `cluster <- new_cluster(22)` is at line 172, i.e. after the block that
would need it, and 22 workers on a 12-core machine.

---

## 4. `snow-reg-no-r2-filter` — **PARTLY_CONFIRMED**

Footprint identity and the 15.4% figure are exact:

    n valid each (mean/sd/reg): 1133175 1133175 1133175
    frac r2>0.2 = 0.1542 (185896 of 1205859) ; mean slope among them = -1.5238

But the code assigns `lm.r2 = (lm(...))$adj.r.squared` — the *intended* quantity is the
**adjusted** R², not R². Under that reading the filter keeps only **6.24%** of pixels
(75,235) and the filtered coefficient is **−1.7183**, not −1.52. Both numbers should be
in the response letter; which one applies depends on how the typo is repaired.

---

## 5. `snow-fitted-half-pixel-offset` — **CONFIRMED (upgraded to a bit-exact proof)**

    stars as_tibble()               -> x = 732744.5 (cell centre)
    stars as_tibble(add_max = TRUE) -> x = 732744, x_max = 732745 (cell bounds)
    terra::rast(<data.frame>) treats x/y as centres (verified in v19.R)

    fitted_2012 ext=[732743.50 734544.50 4050316.75 4052069.75]
    vege_2012   ext=[732744.00 734545.00 4050316.25 4052069.25]      => (-0.5, +0.5)

    terra: default==bilinear identical values? TRUE
    bilinear == exact mean of the surrounding 2x2 block (6 random probes, |d| <= 7.6e-06)
    fitted_2012: bilinear mean=173.5442 nearest mean=173.4878 max|diff|=50.379

Strongest evidence: I reproduced **every** `fitted_*.tiff` bit-exactly from the current
`raw/` stack with the `>0` rule — but only after applying the (−0.5, +0.5) shift (`v14.R`):

    fitted_2011 max|d|=0.000e+00   fitted_2012 max|d|=0.000e+00   fitted_2013 0
    fitted_2015 0   fitted_2018 0   fitted_2020 0   fitted_2021 0
    fitted_2030 (clamped) max|d|=0.000e+00

Two consequences the auditor did not draw:
(a) the `fitted_*.tiff` are **fully reproducible**; the orphan problem is confined to
`snow_{mean,sd,reg}.tif`;
(b) the offset is therefore certain, not inferred.

Caveat: "affects all HS maps and areas" is a mechanism claim, not a measured one — the
effect of the 1 m low-pass on the reported areas was never quantified by the auditor and
I did not re-fit the models either.

---

## 6. `snow-2021-2030-different-domains` — **PARTLY_CONFIRMED (impact overstated)**

The mismatch is real at the fitted-raster level:

    fitted_2021 valid 1178230 (1703x1801) ; fitted_2030 valid 1206083 (1753x1801)
    2030 valid & 2021 NA: 27853 ; 2021 valid & 2030 NA: 0

But the auditor's conclusion — "the reported *newly suitable* areas are differences
between non-comparable domains", "of the same order as the headline 27,049 m²" — is wrong
on two counts:

1. After the terrain mask and `filter(elevation < 2560)`, the gap in the actual prediction
   rasters is **10,255 cells**, not 27,853, of which only **1,171** have `pred_2030 > 0.5`.

2. `tidyterra::filter` propagates NA as exclusion (verified on a 2x2 toy raster), so the
   difference is already computed on the intersection. Re-running the archived code path
   reproduces the manuscript exactly:

        sdm : newly suitable expanse = 27049.27  2021 suitable = 47253.26  ratio = 0.5724
        tdm : newly suitable expanse =  4386.61  2021 suitable = 12766.04  ratio = 0.3436
        (paper: 27,049 m² / 57% ; 4,387 m² / 34%)

        strict (NA excluded)              : TBM 27064 cells, TDM 4389 cells
        inclusive (2021 NA = unsuitable)  : TBM 28235 cells, TDM 4389 cells

   So the maximum effect on the headline number is **+1,171 m² (+4.3%) for the TBM and
   exactly zero for the TDM**. The defect is a silent coverage gap, not an inflated area.

---

## 7. `snow-georectify-missing-mapping` — **PARTLY_CONFIRMED**

Confirmed: `data/georectified.csv` exists nowhere (`find . -name "georectified*"` returns
only `ortho/data/georectified.tiff`); no tracked script writes it; `params_optim.json` is
neither read nor written by any tracked code; `gcp.csv` (482 rows, `,u,v,x,y,z`) is
referenced only in `ortho/.Rhistory`, and there with a *different* schema
(`org_x/org_y/sim_x/sim_y`).

**Refuted sub-claim.** The auditor reported that the projection convention is "not
recoverable by inspection", best median reprojection error 28.17 px. It is recoverable,
first try, at **median 5.04 px / mean 6.41 px / RMS 8.77 px** over all 482 GCPs — entirely
consistent with the stored `"error": 4.342`:

    f  = (w/2) / tan(fov/2)                       # horizontal FOV
    R  = Rz(-pan) . Rx(tilt) . Ry(roll)           # camera looks along +Y (north) at pan=0
    c  = R^T (X - cam);  x' = c_e/c_n,  y' = c_u/c_n
    OpenCV rational (k1..k6) + tangential (p1,p2) + thin-prism (s1..s4), forward
    u = cx + f*x'' ;  v = cy - f*y''              # a1, a2 unused

    no a1/a2       median 5.040 mean 6.406 rms 8.770 p90 11.586
    no distortion  median 43.299 mean 39.270
    (a1/a2 as skew or scale make it worse: 32.0 / 81.5 px)

Also: `ortho/.Rhistory` is not "an entirely different georectification pipeline" — it is
the **image-alignment** step (`procOPA` + `fields::Tps`, `TPS_rmse.csv`), followed by a
complete `snowmelt_aligned/*.png -> snowmelt_ortho/*.csv -> *.tiff` route. It documents
the provenance of `raw/` under an older layout.

---

## 8. `snow-image-to-csv-dead` — **PARTLY_CONFIRMED**

Facts all check out:

    glob("ortho/data/snow/aligned/*.png") -> []      ; glob("data/snow/aligned/*.png") -> 11
    cv2 MISSING ; pandas MISSING ; numpy OK ; no requirements.txt / renv.lock anywhere
    list.files("ortho/data/snow/aligned/", "*.csv") -> character(0)
      1:length(files) -> 1 0 ; files[1] -> NA
    all 11 PNGs have R == G == B  (so IMREAD_GRAYSCALE is byte-preserving)

But "wrong paths" is the wrong diagnosis. `image_to_csv.py` (run from the repo root) writes
to `ortho/data/snow/aligned/`; `georectify.R` (after `setwd(".../ortho/")`) reads
`data/snow/aligned/*.csv` and writes `data/snow/raw/*.tiff` — i.e. exactly
`ortho/data/snow/raw/`, where the archived rasters are. **The two scripts agree with each
other.** They are dead because the archive relocated the PNGs to `data/snow/aligned/` at
the repo root and never committed the intermediate CSVs. Supporting evidence that
`georectify.R` really is the producer: `raw/*.tiff` are Float32 with NoData = NaN, the
signature of `terra::focal` + `writeRaster`, not of the integer `st_rasterize` route in
`.Rhistory`.

---

## 9. `snow-zero-sentinel-inconsistent` — **CONFIRMED**

Zero counts, the three filters (`>10` at 37 and 205, none at 45–60 and 73–88, `>0` at 115)
and the −0.7147 / −0.9184 pair all reproduce (sections 0 and 2 above). The `>10` rule is
numerically indistinguishable from `>0` (mean −0.7146).

Worth adding to the revision: the manuscript's sentence "some areas obscured by foreground
snow cover ... showed minimal overlap with the *Sasa* distribution" is **verifiably true** —
of the 27,996 no-observation cells in 2021, only **39** are *Sasa* in `vege_2021`
(0.38% of the 10,176 *Sasa* cells).

---

## 10. `snow-2019-2010-undocumented` — **CONFIRMED**

    aligned/ : 2010 2011 2012 2013 2014 2015 2016 2017 2018 ---- 2020 2021  (11 PNGs)
    raw/     : ---- 2011 ... 2018 ---- 2020 2021                             (10 tiffs)
    2010: bands identical, nonzero 10,787,950 (~12% fewer than 2011's 12,248,130),
          min 120 max 231 mean 190.36
    matmet.qmd:77 "for each year from 2011 to 2021" ; only supplement.qmd:20 states the gap

---

## 11. `snow-2030-clamp-asymmetric` — **CONFIRMED**

    raw 2030 prediction range: -378 403.1355 ; n<0: 12  n>255: 4  (16 of 1,206,082)
    mean clamped 160.6232 vs unclamped 160.6229
    fitted_2030 reproduced exactly only with the clamp (max|d| = 0 vs 378 unclamped)
    fitted_2011 max 238.4576 ; fitted_2020 max 238.7161 (observed max = 232)

---

## 12. `snow-figS1-not-from-code` — **CONFIRMED**

`ortho/snowmelt_shifting.png` (1800x1200) is a **boxplot of anomalies**, x = 2012…2021
(2011 and 2019 absent), y spanning +50 to −110 with the label "Snowmelt Day of Year";
lines 211–227 produce a `geom_point` + `geom_smooth(lm)` scatter of absolute DOY coloured
by pixel id over 2011…2021. Not the same plot.

The *data* are consistent, though: the figure's 2021 box sits near −2, matching the
`>10`-filtered annual means (2021 169.49 − 2011 171.49 = −2.0). Only the plotting code is
missing.

---

## 13. `snow-preprocess-writes-into-predictor-dir` — **CONFIRMED**

    preprocess_snow_data.R:67  write_stars("data/terrain_features/snow_mean.tif")
    preprocess_snow_data.R:96  write_stars("data/terrain_features/snow_sd.tif")
    sdm_tbm.R:13  list.files("data/terrain_features/") %>% str_subset(".tif$") %>% rast()
    ortho/data/terrain_features/ : TPI TRI aspect roughness slope tateyamadem_small twi
      (7 files, no snow layer)

---

## 14. `georectified-tiff-corrupt` — **CONFIRMED**

    band 1 Float32 nodata nan : nan 7859084 inf  250 finite 4765676
    band 2                    : nan 7829103 inf  250 finite 4795657
    band 3                    : nan 7809248 inf 3751 finite 4812011
    finite range -8.02156e+22 .. 2.43954e+33

Scope narrower than implied: only 146 / 146 / 792 *finite* values lie outside [0,255], so
about a thousand pixels of 4.8M are damaged, plus the 4,251 infinities.

---

## 15. `snow-doy-out-of-range` — **CONFIRMED**

PNG counts reproduce exactly (2011 19,455 / 2012 20,264 + 120,708 above 231 / 2013 22,125 /
2014 19,584 / 2015 16,994 / 2016 17,633 / 2021 19,119; 2010, 2017, 2018, 2020 clean).
Raster counts likewise: 2011 16, 2012 615 + 21,838 (max 232), **2014 1**, 2015 18, 2016 8,
2021 312. (The auditor omitted 2014.)

---

## Retractions — both correct

* **Parse vs eval.** `parse()` returns 34 expressions with no error; the failure is at
  evaluation. Re-verified above. The retraction is right and important.
* **Only two predictors could be injected.** Right: line 137 is unreachable, and
  `ortho/data/terrain_features/` holds no snow layer. I add an independent confirmation
  from the grid phase of `snow_reg.tif` (section 1).

---

## What the auditor missed

### A. `drop_na()` at line 153 silently deletes pixels from the fitted layers

`fitted <- read_csv("data/snow/fitted.csv") %>% drop_na()` operates on `broom::augment`
output, whose diagnostic columns are NaN for degenerate series. Direct test:

    d <- tibble(year = c(2011:2018,2020), snowmelt = c(132, rep(120,8)))
    broom::augment(lm(snowmelt ~ year, d)) %>% drop_na()   # the 2011 row is gone

Consequence, measured against a full reconstruction (`v15.R`):

    year 2011: expected 1206073  archived 1202593  MISSING 3480  (3342 of them 10-year pixels)
    year 2013: 2 ; 2014: 2 ; 2016: 1 ; 2017: 9 ; 2020: 3 ; 2021: 7 ; 2012/2015/2018: 0

`fitted_2011.tiff` also loses 3 raster rows at the top for the same reason
(1750x1801 instead of 1753x1801). The loss is concentrated on the **extreme years**, which
are exactly the ones with the highest leverage, so it is not a neutral thinning.

### B. `snow_mean.tif` and `snow_sd.tif` are orphans too, and the nine years are identifiable

See section 1. `snow_sd.tif` is the worst: its best correlation with any recomputation from
`raw/` is 0.67. The whole trio should be deleted from the published repository, and the
nine-year provenance (2011–2018 + 2020, i.e. pre-2021) stated plainly if the −0.86 is to
be discussed at all.

### C. The number the paper should have reported

The layers that actually enter the SDMs imply, over their own 9-year span:

    mean(fitted_2021 - fitted_2012) = -6.390 d   median -5.601   SD 5.657   91.7% negative
    -> -7.10 days per decade

against the abstract's 8.6 d/decade. Three mutually inconsistent numbers are now in play:
−8.64 (orphan raster), −7.15 (per-pixel mean slope, `>0` rule), −7.10 (the fitted layers
actually used). Only the last two are reproducible.

### D. `sdm_tbm.R:474-479` builds the snow figure from the wrong layer

    snow_21 %>% rename(snow_12 = snow) %>% c(snow_21) %>% rename(snow_21 = snow) %>% ...

As written, the "2012" facet is `snow_21`. The archived `ortho/snowmelt_doy.png` is *not*
affected — its three panels genuinely differ (mean |panel1 − panel2| = 8.22 grey levels,
comparable to |panel2 − panel3| = 8.93; 35% of pixels differ by more than 5), and Fig. S2
is in fact produced by `scripts/sdm/plot_snowmelt_shifts_map.R`, which uses
`fitted_2012/2021/2030` correctly. So this is latent dead code — but it is dead code that a
reviewer re-running the published script will hit, and it will silently produce a wrong
supplementary figure.

### E. `plot_snowmelt_shifts_map.R:38` truncates Figure S3

    mutate(shift = ifelse(abs(fitted_2021.tiff) > 50, NA, fitted_2021.tiff))

347 pixels (0.0295%) are blanked; the mean shift moves from −6.390 to −6.374. Negligible in
magnitude, undocumented in the caption. Same class of undisclosed choice as the 2030 clamp.

### F. The "1 m resolution" topographic predictors are upsampled from a ~5x6 m grid

    vege_2012_5x5.tiff   res 1x1
    aspect/slope/TPI/TRI/roughness/twi/tateyamadem_small : res 4.97 x 6.16, ext offset

`sdm_tbm.R:19` `resample(vege12)` bilinearly upsamples all seven. Reviewer 2's collinearity
comment and the manuscript's "1 m resolution" framing both need this stated. (Adjacent
subsystem, but it is the same `resample()` line that hides the snow offset.)

### G. Minor

* The 2017 and 2018 aligned PNGs have **byte-identical zero masks**
  (12,309,490 / 8,716,814 in both; every other year pair differs) while their values differ
  in 11.9M pixels — one year's no-observation mask appears to have been reused.
* No leap-year handling anywhere: DOY 120 is 29 Apr in 2012/2016/2020 and 30 Apr otherwise.
  The induced slope bias is only +0.009 d/yr (leap indicator has `Sxy = 0.9`, `Sxx = 100.1`),
  so this is a correctness nit for the Methods, not a numbers problem.
