# Adversarial verification — "artifact-provenance" subsystem

All commands were re-run from `/Users/okamoto/NIES/SasaSDMPaper` (the real repo, where the
gitignored data live). Nothing was modified. Every number below is my own output, not the
auditor's.

---

## Verdicts on the auditor's findings

### 1. `snow-reg-irreproducible` — **CONFIRMED** (and stronger than claimed)

Archived rasters:

```
$ Rscript -e 'library(terra); ...'
ortho/data/snow/snow_mean.tif  1739x1791  n=1133175  mean=171.763270  sd=28.8536
ortho/data/snow/snow_sd.tif    1739x1791  n=1133175  mean=  9.677257
ortho/data/snow/snow_reg.tif   1739x1791  n=1133175  mean= -0.863600  min=-12.111 max=25.135
```

Faithful re-run of the documented logic over the 10 archived raw rasters (I verified that
`read_stars() |> st_warp(vege_2012_5x5.tiff)` is a **no-op** — the raw tiffs are already on the
vege grid, `max abs diff = 0`, `n_notNA` unchanged):

```
drop_na cells: 1206233
snow_mean recomputed mean: 170.425734   (archived 171.763270)
snow_sd   recomputed mean:  10.355748   (archived   9.677257)
per-pixel OLS slope, snowmelt>0 filter:
  n finite: 1206082   mean: -0.714695   (archived -0.863600)
  median -0.6264   sd 0.6353   91.65% negative
  adj.R2>0.2 (as coded): n= 75235  mean=-1.7183
  raw  R2>0.2          : n=185897  mean=-1.5240
```

The auditor's numbers reproduce to 4 d.p. **Two additional facts make the finding much
stronger than stated:**

(a) **All 10 raw rasters share a byte-identical NA mask** (n = 1,206,233 each):

```
MRD_..._2011..._BW.tiff  n:1206233  same_mask_as_2011: TRUE
... (all 10) ...
MRD_..._2021..._BW.tiff  n:1206233  same_mask_as_2011: TRUE
```

This is expected: `georectify.R::interpolate()` rasterises every year onto the same
`georectified.csv` point set. Therefore `drop_na()` over **any** subset **or superset** of these
rasters yields exactly 1,206,233 cells.

(b) **27,275 of the archived `snow_*.tif` cells lie where every raw raster is NA.** Verified by
coordinate extraction (not `extend`, to rule out an alignment artefact):

```
archived non-NA: 1133175
of these, raw2011 is NA at: 27275 cells
bbox of archived-only cells: x 732750.5–734539.5  y 4050356–4052068   (scattered, not an edge strip)
raw non-NA: 1206233; of these archived is NA at: 100333
```

So `snow_mean/sd/reg.tif` cannot have come from *any* combination of the archived raw rasters.
They are the output of an **older georectification run with a different footprint**.

(c) Conversely the `fitted_*.tiff` rasters the SDMs consumed *are* derived from these 10:

```
cor(recomputed OLS slope, (fitted_2030 - fitted_2012)/18) = 0.99992351
mean difference = 0.00000226   (max 8.23, from the 14 cells clamped to [0,255] at line 184-185)
(fitted_2030-fitted_2012)/18 : n=1206064  mean=-0.714697
cor(archived snow_reg, recomputed slope) = 0.510535  over 1105905 shared cells
```

Verdict: CONFIRMED, critical. The published −0.86 comes from an artefact with no surviving
provenance; the models used a surface whose mean is −0.715.

---

### 2. `georectified-csv-missing` — **PARTLY_CONFIRMED** (the diagnosis is wrong)

Confirmed: `ortho/data/georectified.csv` is absent, is read at `ortho/georectify.R:8,18` and
`scripts/vegetation_classification/utils/interpolate.R:51`, and is written by no script
(exhaustive writer grep below). `ortho/data/georectified.tiff` (26 MB) exists, i.e. the CSV did
exist. Schema is pinned by `interpolate()`'s `select(-c(u, v, z))`: columns `u,v,x,y,z`.

**Refuted:** the claim that `ortho/.Rhistory:81-176` is "the only record of how it was produced".
Reading the history verbatim, that block *reads* a pre-existing file:

```
points_sim <- read_csv("georectificated.csv", ...) %>% rename(pix_num = ...1)
```

— note the different spelling (`georectificated`, not `georectified`) and the different schema
(`pix_num, x, y`). The Procrustes+TPS block does **not** create the pixel→world table; it warps a
*second* camera's pixels onto the reference camera's grid and then produces per-image
`snowmelt_ortho/*.csv` with columns `x, y, snow_melt`:

```
result <- mutate(corrected, snow_melt = as.integer(melt2)) %>% filter(x != 0, snow_melt != 0)
write_csv(result, out)     # out = snowmelt_ortho/<name>.csv
```

Those files are **format-incompatible** with `georectify.R::concat_df()`, which expects
`u,v,<value>` and joins to `georectified.csv` by `u,v`. So the repository contains two mutually
incompatible georectification workflows, and *neither* produces `georectified.csv`. The camera-model
step that must have produced it (`alproj`, `gcp.csv`, `params_optim.json`) is nowhere at all —
not in the scripts and not in the history.

The auditor's proposed fix ("promote .Rhistory:81-176 into `00_georectify_tps.R` that regenerates
georectified.csv") would therefore **not work**. Corrected fix: write a new script that runs the
`alproj` camera-parameter fit from `ortho/data/gcp.csv` + `ortho/data/params_optim.json`, and
either deposit the resulting CSV or the 26 MB `georectified.tiff` it rasterises to.

Also confirmed absent: `ortho/data/2012_5x5.csv`, `2021_5x5.csv`, `ortho/data/snow/aligned/`
(the whole directory), `ortho/data/snow/fitted.csv`, `data_source/aligned/2015/`.

---

### 3. `terrain-glob-collision` — **CONFIRMED** (with a new proof and a new hazard)

```
$ ls ortho/data/terrain_features/
TPI.tif TRI.tif aspect.tif roughness.tif slope.tif tateyamadem_small.tif twi.tif     (7)
$ grep -n terrain_features scripts/sdm/preprocess_snow_data.R
67: write_stars("data/terrain_features/snow_mean.tif")
96: write_stars("data/terrain_features/snow_sd.tif")
137:write_stars("data/terrain_features/snow_reg.tif")
```

and the archived snow_*.tif in fact live in `ortho/data/snow/` — moved by hand. Fitted predictor
sets contain no `twi`:

```
models.rds         vars: aspect, roughness, slope, elevation, TPI, TRI, snow, dist, X, Y, sasa
                   roles: predictor x8, coords, coords, outcome
models_wo_dist.rds vars: aspect, roughness, slope, elevation, TPI, TRI, snow,       X, Y, sasa
models_all_5m.rds  vars: slope, snow, dist, elevation, aspect, TPI,                 X, Y, sasa
```

**New positive proof that `twi.tif` was absent at fit time** (rather than merely dropped): the
predictor *order* in `models.rds` is exactly `list.files()` order under a case-insensitive
collation, minus twi:

```
$ Sys.getlocale("LC_COLLATE") -> ja_JP
list.files order here: aspect, roughness, slope, tateyamadem_small, TPI, TRI, twi
C-locale sort        : TPI, TRI, aspect, roughness, slope, tateyamadem_small, twi
models.rds order     : aspect, roughness, slope, elevation(=tateyamadem_small), TPI, TRI, snow, dist
```

**New hazard the auditor missed:** the same script on a C/POSIX-locale machine gets
`TPI, TRI, aspect, ...` — a *different predictor column order*. `ranger`'s `mtry` draw and
`xgboost`'s column sampling are order-dependent, so `set.seed(1)` does not protect against this.
Fix must pin an explicit ordered vector, not merely exclude twi.

---

### 4. `tbm-script-only-rf` — **CONFIRMED** (with independent corroboration)

`sdm_tbm.R:96-113` has gam/maxent/xgb commented out; `:127 saveRDS(models,
"models_wo_dist_twi.rds")`; `:129 readRDS` the same name; the file does not exist
(`ls: ortho/models_wo_dist_twi.rds: No such file or directory`). `models_wo_dist.rds` contains
`default_rf, default_gam, default_maxent, default_xgb`.

**New corroboration:** I rendered `ortho/figures/model_performance_tbm.png` (which is
**byte-identical** to `paper/files_original_size/model_performance_tbm.png`). It shows ~44 tuned
workflows in four colours legended GBT / GAM / MaxEnt / RF. So the *published* Fig. 5 TBM panel
provably came from the four-algorithm run, not from the committed script. The filename
`models_wo_dist_twi` also tells us the committed file is a later TWI experiment that overwrote
the real TBM script.

`model_stack_wo_dist.rds`: 4 model_defs, members `maxent x2 + xgb x2`, training columns
`sasa, geometry, aspect, roughness, slope, elevation, TPI, TRI, snow`, 21,389 rows — no twi.

---

### 5. `snow-reg-code-cannot-run` — **CONFIRMED** (all three defects)

```
$ Rscript -e 'p <- parse("scripts/sdm/preprocess_snow_data.R"); length(p)'   -> 34, no error
$ ... %>% group_by(x,y) %>% partit    -> Error in partit(.): could not find function "partit"
$ f <- lm(...); is.null(f$coeffisients)   -> TRUE   ("coeffisients" is not a prefix of
                                                     "coefficients", so $ partial matching fails)
$ is.null(f$adj.r.squared)                -> TRUE   (that lives on summary(f))
names(lm): coefficients, residuals, effects, rank, fitted.values, ...
```

Also confirmed: the archived `snow_reg.tif` NA mask is `identical()` to `snow_mean.tif`'s and
`snow_sd.tif`'s, so the `filter(lm.r2 > 0.2)` at line 130 was **not** in force in whatever run
produced the artefact.

---

### 6. `public-figures-differ-from-archive` — **PARTLY_CONFIRMED** (7 of the 10 are cosmetic)

The SAME/DIFFERENT/NO-ARCHIVE partition is exactly right (9 / 10 / 3 + `expanded_area.pdf`).
But "the differences are not cosmetic" is **wrong for seven of the ten**.

For `hsdiff_tbm`, `hsdiff_tdm`, `hsmap_tbm_2021`, `hsmap_tbm_2030`, `hsmap_tdm_2021`,
`hsmap_tdm_2030`, `risky_tbm` the published files carry an extra (fully opaque) alpha band, and
the RGB planes differ in only **0.264 % of pixels, confined to an identical 18,992-pixel region in
every one of them**:

```
hsdiff_tbm     rgb maxdiff=255 meandiff=0.5572 frac>8 = 0.264%
hsmap_tdm_2030 rgb maxdiff=255 meandiff=0.5568 frac>8 = 0.264%
risky_tbm      rgb maxdiff=235 meandiff=0.5282 frac>8 = 0.264%
diff-mask bbox identical across figures: rows 150–1994, cols 619–1076, n = 18992
```

Rendering the mask shows what it is: a **north arrow and a "200 m" scale bar**, added to the
published versions and absent from the archived ones. No script in the repository draws either
(`plot_vegetation_map.R` loads `ggspatial` but the SDM scripts do not). So these seven are a
post-hoc annotation, not a re-analysis. Still a provenance gap (an unversioned final step), but
the maps themselves are identical.

The three genuinely different figures:

* `vi_tbm.png` — **the auditor's description is exactly right.** Archived: snow > elevation > TRI
  > roughness > aspect > slope > TPI, y-axis fixed [0, 0.6] (matches `sdm_tbm.R:215 ylim(c(0,0.6))`).
  Published: snow > elevation > TRI > **aspect > roughness** > slope > TPI, y-axis auto to ~0.27,
  and the medians differ (elevation ~0.118 vs ~0.14; snow IQR much wider). Different run, and a
  script version without `ylim()`.
* `vi_tdm.png` — same story, with **slope and roughness** swapped (archived
  ...aspect > TRI > roughness > slope > TPI; published ...aspect > TRI > slope > roughness > TPI).
* `snowmelt_shifting.png` — see missed finding M3 below; neither copy matches the committed code.

---

### 7. `tss-two-copies-disagree` — **CONFIRMED**

```
$ cat ortho/tss_score_tbm.csv
boyce_cont 0.9916482236458939 / roc_auc 0.8369122791343737 / tss_max 0.5555946421107376
$ readRDS("ortho/tss_score_tbm.rds")
boyce_cont 0.9970296684388505 / roc_auc 0.8390209578541242 / tss_max 0.5348430499400409
$ grep -rn tss_score scripts/  ->  only sdm_tbm.R:159-161 (write_csv)
paper/results.qmd:30 "Ensemble TSS on the test dataset was 0.55 for the TBM and 0.70 for the TDM"
$ cat ortho/tss_tdm.csv -> tss_max 0.7009730541476817   (matches the 0.70)
```

---

### 8. `alignment-report-different-image-set` — **CONFIRMED** (one sharpening)

24 rows, 12 per year, all against `data_source/mrd_085_eos_vis_20151010_1205.png`; repo ships 7
per year named `IMG_####.JPG`. EXIF `DateTimeOriginal` of the 14 shipped JPGs:

```
2012: 08-27 09-01 09-11 09-17 09-26 10-06 10-21
2021: 08-24 08-31 09-07 09-19 09-24 10-02 10-14
```

vs `matmet.qmd:30` "images taken in September–October of 2012 and 2021" — three are August, one
(2012-10-21) is outside the report's range too. `align_photographs.py` has no `to_csv`/`concat`
anywhere; `results` is appended to and discarded.

**Sharpening:** `align_photographs.py:184` is not a crash. `targets = sorted(glob("data_source/aligned/2015/*"))`
returns `[]`, so `zip(sources, targets)` yields nothing and the **entire primary alignment loop is a
silent no-op** — a third party would see the script "succeed" and write no aligned images.

---

### 9. `public-repo-reproduces-nothing` — **CONFIRMED** (three factual corrections)

```
$ git ls-files | wc -l           -> 127
$ tracked bytes                  -> 947.2 MB   (auditor said 903.4 MB)
$ du -sh .git                    -> 905M
.gitignore: paper/ , *.tiff , *.zip , *.npy , *.rds , __pycache__/
$ git ls-files | grep '\.tif$'   -> exactly the 10 named, incl. twi.tif and the 3 stale snow_*.tif
```

Corrections:
* `__pycache__/` **is** gitignored, so `crnn.cpython-310.pyc` / `svm.cpython-38.pyc` are **not**
  published. They exist only on this disk. (They still matter: their sources `crnn.py`/`svm.py`
  are missing everywhere, which shows the model directory is a pruned copy.)
* `apply_mask.py:4` reads `data_source/mrd_085_eos_vis_20151010_1205_maskd.png`; the repo ships
  `data/images/mrd_085_eos_vis_20151010_1205_masked.png`. Confirmed typo.
* **New path break:** the 11 snowmelt PNGs are tracked at `<repo>/data/snow/aligned/`, but
  `image_to_csv.py:27` globs `ortho/data/snow/aligned/*.png` and `georectify.R:62` globs
  `data/snow/aligned/*.csv` relative to `ortho/`. `ortho/data/snow/aligned/` does not exist. So
  the deposited PNGs are in a directory neither consumer looks in, *and* `image_to_csv.py`'s
  vegetation branch (lines 15-23) is entirely commented out.

---

### 10. `tateyama2-corrupt` — **CONFIRMED**

```
$ xxd -l 32 ortho/data/tateyama2.tiff
4949 2a00 b8ed 7f84 ...    -> little-endian, first IFD at 0x847FEDB8 = 2,222,976,440
$ stat -f%z -> 163,577,856
gdal.Open -> FAIL: TIFFReadDirectory: Failed to read directory at offset 2222976440
$ grep -rn tateyama2 scripts/ ortho/*.R data/  -> no hits
```

---

### 11. `orphan-and-stale-artefacts` — **CONFIRMED** (with corrections)

Exhaustive writer grep over `scripts/`, `ortho/georectify.R`, `data/images/` confirms no producer
for: `terrain_features/*.tif` (all 7), `mrd_dem_1m.tiff`, `dem_small.tiff`, `sasa_inc.tiff`,
`selected_comms.tiff`, `georectified.csv`, `alignment_report_2012_2021.csv`, `matched.png`,
`params_optim.json`, `tateyama2.tiff`, `models_wo_dist.rds`, `models_all_5m.rds`,
`tss_score_tbm.rds`, `risky_area.tiff`, `potential_sasa_area_21.tiff`, `sasa_pred_sdm_12.tiff`,
`sasa_pred_sdm_dist_{21,30}.tiff`, `data/images/mask.npy`, `results/cv.png`, `2012_5x5_en.png`,
`2021_5x5_en.png`, `overview.png`, `expanded_area.pdf`.

`results/cv.png` rendered: facets Dwarf Pine / Dwarf Bamboo / Rowans / **Golden Birch** / Montane
Alder / Other vegetation / Non Vegetation; x-axis `2015-08-25 … 2015-10-10`, `Multidays RNN 1x1`,
`Multidays1x1`. Confirmed: previous publication. `utils.py` and `utils_old.py` are byte-identical.

**New evidence that `sasa_pred_sdm_dist_*` are stale:** identical domain, different values —

```
sasa_pred_sdm_dist_30.tiff  n=407658  mean=0.1378  >0.5: 34319
sasa_pred_tdm_30.tiff       n=407658  mean=0.1262  >0.5: 14482
```

**Correction:** `__pycache__` is gitignored (see #9), so "ships" is wrong for the .pyc files.

---

### 12. `hardcoded-environment` — **CONFIRMED**

All seven `setwd()` lines verified verbatim; `plot_snowmelt_shifts_map.R` has none but uses
`ortho/`-relative paths; `num.threads = 18` at `sdm_tbm.R:99` (+3 commented) and `sdm_tdm.R:110,114,118,122`;
`new_cluster(22)` at `preprocess_snow_data.R:172`; `device="cuda", num_workers=20` in
`nnmodel.py:17` and `rnn.py:43`; `run_rnn.py:25,28` never passes `device`; font path
`/usr/share/fonts/truetype/migmix/migmix-1p-regular.ttf` at `utils.py:20` (import-time
`FontProperties` + `plt.rcParams`); no `renv.lock`, `requirements.txt` or `DESCRIPTION`.

---

### 13. `npy-to-tiff-recode-undocumented` — **CONFIRMED and substantially sharpened**

The +1 is confirmed:

```
results/2012_masked.npy {0:9515000, 1:6611414, 2:1691081, 3:376522, 4:214259, 5:528289, 6:2089739}
vege_2012_5x5.tiff freq  0:5179 1:8547 2:486402 3:357937 4:26934 5:5700 6:20774 7:294760
```

**But it is not an arbitrary shift — I found where it comes from, and it is worse than described.**

`utils.py::read_sses` reads Semantic-Segmentation-Editor JSON and remaps SSE `classIndex == 0` to
`max+1`; `nnmodel.py:47-49` then sets `class_to_idx = {"1":0, "2":1, … "7":6}`. So

```
model index i   <->   SSE classIndex i+1        (and SSE 0 = ハイマツ becomes 7)
```

The deposited labels give the authoritative table:

```
$ python3 (classIndex, label) over data/labels/*.json
{(0,'ハイマツ'):40, (1,'ササ'):25, (2,'その他植生'):76, (3,'無植生'):29,
 (4,'ナナカマド'):16, (5,'ダケカンバ'):16, (6,'ミヤマハンノキ'):14}
```

i.e. **the GeoTIFF value *is* the SSE classIndex**, and the missing step is simply
"torch index + 1". That step exists in no script.

**Additional hard defect (verified):** `apply_mask.py:10` sets masked pixels to `0`, which is
*also* the code for Sasa. Measured collision:

```
mask black pixels (mrd_..._masked.png): 9,241,580
zeros in results/2012_masked.npy      : 9,515,000
  of which masked : 9,239,049
  of which genuine class-0 (Sasa): 275,951
```

So the deposited `results/*_masked.npy` **cannot** be used to regenerate the vegetation maps
without the separate mask; the archived npy is not the file that produced `vege_*_5x5.tiff`.

---

### 14. `snow-2010-glob-hazard` — **PARTLY_CONFIRMED** (causal claim REFUTED)

Confirmed and unchanged:

```
$ ls data/snow/aligned/          -> 11 PNGs, 2010–2021, no 2019 (all git-tracked)
$ ls ortho/data/snow/raw/        -> 10 TIFFs, 2011–2021, no 2019
georectify.R:62 and preprocess_snow_data.R:11 both glob their input directory
matmet.qmd:32 "imagery acquired from April to August between 2011 and 2021"
```

**Refuted:** "Adding 2010 both shrinks the drop_na mask … the most likely explanation of the
snow_reg discrepancy." All 10 raw TIFFs have a **byte-identical** NA mask (n = 1,206,233 each),
because `interpolate()` rasterises every year onto the same `georectified.csv` point set. A
georectified 2010 raster would have the same footprint, so adding it **cannot change the drop_na
mask at all**, cannot produce n = 1,133,175, and above all cannot produce the 27,275 archived
cells that lie **outside** the raw footprint (finding #1(b)). The glob hazard is real; the
explanation is not. The real explanation is an older georectification with a different footprint.

---

### 15. `paper-dir-still-in-public-history` — **CONFIRMED**

```
$ git log --oneline  -> 48decbd / 67fe2d5 "Delete paper directory" / 58da9ef / 4813de2 / 9a8fd36
$ git diff --stat 58da9ef 67fe2d5 | tail -1  -> 72 files changed, 14658 deletions(-)
$ git show 58da9ef:paper/covering_letter.doc | strings | grep -i okamoto
Ryotaro Okamoto
Email: okamoto.ryotaro@nies.go.jp
$ git ls-files | grep -c DS_Store -> 16
$ git ls-files | grep Rhistory    -> data/labels/.Rhistory, ortho/.Rhistory
```

---

## The four retractions — all correct

1. "the block never ran, but not for a syntax reason" — correct, verified above (#5).
2. "`device="cuda"` is a hard requirement in practice" — correct; `run_rnn.py:25,28` never passes
   `device`.
3. "X/Y have role `coords`, not `predictor`" — correct; `var_info$role` printed above (#3).
4. "`data/labels/.Rhistory` does not break `read_sses`" — correct. `glob('data/labels/*')`
   returns exactly the six `.json` files; all six parse. (Their object counts 72/7/3/14/28/92
   refer to the `objects` key, which is indeed what `read_sses` reads.)

---

## What the auditor missed

### M1. `thin_by_cell()` is called **before** `set.seed()` — the absence sample is unseeded (critical)

`tidysdm:::thin_by_cell` begins its work with

```r
data <- data[sample(seq_len(nrow(data))), ]
```

(printed from the installed package). It is stochastic. In both scripts it is called *before* the
seed is set:

```
sdm_tbm.R:55   thin_by_cell(sampling_mask)      <- unseeded
sdm_tbm.R:61   set.seed(1)
sdm_tdm.R:64   thin_by_cell(sampling_mask)      <- unseeded
sdm_tdm.R:71   set.seed(1)
```

The pseudo-absences are roughly half the modelling data (`model_stack_wo_dist.rds` has 21,389
training rows; `model_stack.rds` 16,596). Because the user ran these files interactively, block by
block, the RNG state at line 55/64 is unknowable. **The published train/test split, the CV folds,
every TSS, and both HS maps therefore cannot be reproduced exactly even by a perfect re-run of the
scripts on the same data.** This is the single cleanest answer to Reviewer 3's reproducibility
objection, and it is a two-line fix (move `set.seed()` above the `bind_rows`).

### M2. The manuscript's fourth woody class is taxonomically mislabelled relative to the deposited labels (critical)

`matmet.qmd:39` lists "Maple (*Acer tschonoskii*)" as one of the seven classes. The deposited
training labels — the only ground truth in the repository, and something a reviewer can open in a
text editor — contain **no maple**:

```
(5, 'ダケカンバ'): 16 objects across _mrd_085_eos_vis_2015{0912,0920,0926}_1205.json
```

`ダケカンバ` is *Betula ermanii* (Erman's birch), not *Acer tschonoskii*. The chain that makes
class 5 the paper's "Maple" is:

* `data/labels/*.json` SSE classIndex 5 = ダケカンバ
* `run_rnn.py:16` cmap comment for that slot = `# 5: ダケカンバ`   ← agrees with the labels
* `calculate_diff.py:10` calls the same slot `"kaede"` (maple)     ← disagrees
* `plot_vegetation_map.R:37` relabels raster value 5 as `ミネカエデ` ← disagrees
* `results/cv.png` (previous publication) facets it as **"Golden Birch"** ← agrees with the labels

So the repository's own artefacts say Erman's birch and the manuscript says maple. Either the
Methods species name is wrong, or the class was silently re-interpreted after training. Reviewer 3
explicitly went looking at the code; this is exactly the kind of thing that gets found.

### M3. `snowmelt_shifting.png` (both copies) is produced by no code in the repository (important)

`preprocess_snow_data.R:211-227` — the only `ggsave("snowmelt_shifting.png", …)` in the repo —
builds a **scatter plot** of 1,000 sampled pixels (`geom_point` + `geom_smooth(method=lm)`,
coloured by pixel id, legend suppressed).

Both `ortho/snowmelt_shifting.png` and `paper/files_original_size/snowmelt_shifting.png` are
**box plots of snowmelt anomaly by year** — nine boxes, evenly spaced at 146 px with a gap where
2019 would be, no colour, y-range ±50 (archived) / ±60 (published). I counted the boxes
programmatically (white-fill column runs): archived 9, published 9-ish.

The two copies also differ substantively (17.4 % of pixels; archived has an outlier at ≈ −113 and
tighter boxes, published has visibly wider IQRs), so they are two different runs of an unversioned
script. And only 9 year-boxes appear where 2011–2021 minus 2019 would give 10 — the first year is
missing, with no documented reason.

### M4. `model_parts()` is unseeded in both scripts (important)

`sdm_tbm.R:190` and `sdm_tdm.R:200` call `DALEX::model_parts()` with no seed. Permutation
importance is stochastic (default B = 10). This is the mechanical reason `vi_tbm.png` /
`vi_tdm.png` differ between the archive and the paper, and it means **Figure 5's importance panels
are not reproducible even given the fitted model and the exact test set**. The aspect/roughness
and slope/roughness rank flips are plausibly pure permutation noise — which is itself worth saying
in the response letter rather than defending a rank order that is not stable.

Related: the two scripts explain on **different data** — `sdm_tbm.R:175` passes `df_test`,
`sdm_tdm.R:184` passes `df_train` — and use different explainers (`DALEX::explain` vs
`DALEXtra::explain_tidymodels`). The two panels of Figure 5 are therefore not comparable.

### M5. Predictor column order is locale-dependent (important)

See #3 above. `list.files()` on `data/terrain_features/` returns
`aspect, roughness, slope, tateyamadem_small, TPI, TRI` under `ja_JP`/`en_US` collation but
`TPI, TRI, aspect, roughness, slope, tateyamadem_small` under `C`. `models.rds` proves the former
was used. Column order changes `ranger`'s `mtry` draws and `xgboost`'s `colsample`, so
`set.seed(1)` does not make the fit portable across machines.

### M6. The 2021 and 2030 prediction rasters cover different domains (important)

```
sasa_pred_sdm_21.tiff   n=397403   >0.5: 47279     (fitted_2021.tiff: n=1,178,230)
sasa_pred_sdm_30.tiff   n=407658   >0.5: 69214     (fitted_2030.tiff: n=1,206,083)
sasa_pred_tdm_21.tiff   n=397403   >0.5: 12773
sasa_pred_tdm_30.tiff   n=407658   >0.5: 14482
```

10,255 cells (2.5 %) of the 2030 map have no 2021 counterpart, because `fitted_2021.tiff` was
written from `broom::augment()` output that lost 28k pixels while `fitted_2030.tiff` came from the
`multidplyr` branch. The two panels of Figure 7 therefore show different footprints. The published
ratios happen to be safe (`27,049 / 47,253 = 57 %` is computed on the intersection via
`c(pred_rast_30, pred_rast_21)`), but nothing in the code enforces that, and the raw
"2030 suitable area" would be computed on the larger domain. `expanse()` vs cell count also
explains the small offsets (47,279 cells ↔ 47,253 m²; 12,773 ↔ 12,766) — worth stating so the
numbers are not read as errors.

### M7. `run_rnn.py` cannot find the data `prepare_data.py` writes (important)

```
prepare_data.py:3,5   set_patches(..., "../data/2012_5x5/")   and   "../data/2021_5x5/"
run_rnn.py:25         RNNClassifier(f"../data/{year}", ...)     -> ../data/2012 , ../data/2021
run_rnn.py:22         years = ["2012", "2015", "2021"]
prepare_data.py:4     the 2015 aligned line is commented out
```

So the published entry point points at directories the published preparation step never creates,
and iterates over a year whose preparation is disabled. On a clean checkout the classification
stage fails immediately for a reason unrelated to the missing data.

### M8. `NNClasifier.__init__` allocates 2.4 GB per instantiation (minor)

`nnmodel.py:22` calls `read_sses(labels_dir, (9999, 9999))` purely to obtain the labels table;
`read_sses` then does `np.zeros([9999, 9999, 3])` (float64 = 2.4 GB) and `cv2.fillPoly`s every
training polygon into it. `run_rnn.py` instantiates the classifier twice per year for three years.
Another undocumented hardware assumption on top of CUDA and `num_workers=20`.

### M9. Ready-made uncertainty statistics for Reviewer 2's point 3

Recomputed from the 10 archived raw rasters (per-pixel OLS on year, `snowmelt > 0`):

```
n = 1,206,082 pixels
mean slope   = -0.7147 d/yr      (i.e. 7.1 days/decade, not 8.6)
median slope = -0.6264 d/yr
sd           =  0.6353
% negative   = 91.65 %
restricted to adj.R2 > 0.2: n = 75,235,  mean = -1.7183
restricted to raw R2 > 0.2: n = 185,897, mean = -1.5240
```
