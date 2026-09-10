# Restoration report — figures, `data_source/`, and the rest of the recovered tree

Scope: manuscript figures and their sources; `data_from_server/ortho/figures/`, the loose
`*.jpg`/`*.png` at `ortho/` top level, `ortho/plot_*.R`, `data_from_server/data_source/`,
`ortho/data/2012_5x5.csv` / `2021_5x5.csv`, `mask.npy`, and whatever else in the recovered
tree the other agents are not covering.

Everything below was produced by running code, not by reading it. Scripts are in
`review/restore/code/`, outputs in `review/restore/figures/`.

Manuscript figure numbering (confirmed from `paper/submit_files/index.tex`):

| # | label | file assembled in the .qmd | source images |
|---|---|---|---|
| 1 | `fig-overview` | `files/overview.jpg` | hand-made composite |
| 2 | `fig-vege12-21` | `files/vegemap.jpg` | `2012_5x5_en.jpg` + `2021_5x5_en.jpg` |
| 3 | `fig-spatial-split` | `files/initial_split.jpg` | `initial_split_dist.jpg` + `cv_dist.jpg` |
| 4 | `fig-sasa-expansion` | `files/expanded_area.pdf` | hand-made GIS composite |
| 5 | `fig-hsm-performance` | `files/model_performance.jpg` | `model_performance_{tbm,tdm}.jpg` |
| 6 | `fig-variable-importance` | `files/vi.jpg` | `vi_{tbm,tdm}.jpg` |
| 7 | `fig-hs-maps` | `files/hsmap_2021.jpg` | `hsmap_{tbm,tdm}_2021.jpg` |
| 8 | `fig-future-prediction` | `files/future.jpg` | **should be** `hsmap_*_2030` + `hsdiff_*` |
| 9 | `fig-risky-map` | `files/risky_tbm.jpg` | `risky_tbm.jpg` |

---

## 1. Figure 8 is Figure 7 with a different caption — confirmed, and fixed

### Confirmed independently, from the submitted PDF itself

`results.qmd:98` reads `hsmap_tbm_2021.jpg` / `hsmap_tdm_2021.jpg` where it should read the
2030 maps. I did not take the audit's word for it; I extracted the actual rasters embedded
in the submitted PDF and compared them:

```
pdfimages -f 12 -l 13 -j paper/submit_files/index.pdf sub
# sub-001.jpg = Figure 7 (2400x960), sub-002.jpg = Figure 8 (2400x1920)
submitted PDF: fig8 top half == fig7 : True   maxdiff 0   frac!=0 0.0
```

Byte-for-byte, pixel-for-pixel identical. And the panel titles are *baked into the images*:
the top row of the submitted Figure 8 is titled **"Habitat Suitability map of Sasa (TBM, 2021)"**
and **"(TDM, 2021)"** while its caption describes 2030 projections. The submitted paper
contains no 2030 habitat-suitability map, and the error is visible on the printed page.

The same check on the working copy: `paper/files/future.jpg` top half is identical to
`paper/files/hsmap_2021.jpg` (`identical: True, max abs diff: 0`).

### The 2030 maps exist and are correct — the fix is one line

`paper/files/hsmap_tbm_2030.jpg` and `hsmap_tdm_2030.jpg` are present, are 3000×2400 like
their 2021 siblings, and differ from the 2021 maps in 17.3% (TBM) / 12.9% (TDM) of pixels —
they are genuinely different maps, not copies.

More than that: I verified that the archived prediction rasters those figures were drawn
from are the ones that produced every 2030 number in the manuscript. Re-running the area
block of `scripts/sdm/sdm_tbm.R` / `sdm_tdm.R` against
`data_from_server/ortho/data/sasa_pred_{sdm,tdm}_{21,30}.tiff`
(`review/restore/code/verify_pred_areas.R`):

| quantity | manuscript | reproduced |
|---|---|---|
| TBM suitable 2021 (HS>0.5) | 47,253 m² | **47,253 m²** |
| TBM newly suitable by 2030 | 27,049 m² (57%) | **27,049 m² (57.2%)** |
| TBM 2021 *Sasa* becoming unsuitable | 2,257 m² (21%) | 2,258 m² |
| TDM suitable 2021 | 12,766 m² | **12,766 m²** |
| TDM newly suitable by 2030 | 4,387 m² (34%) | **4,387 m² (34.4%)** |
| TDM 2021 *Sasa* becoming unsuitable | 717 m² (7%) | **717 m²** |

(The 2,257 vs 2,258 is a rounding difference of one square metre in `terra::expanse()`.)
Not previously reported: TBM total suitable area in 2030 is 69,176 m² and TDM 14,474 m².

So the 2030 rasters, and therefore the 2030 figures rendered from `pred_rast_30` in the same
script, are correct. **No regeneration is needed. The fix is `results.qmd:98`:**

```r
hsmap <- c(image_read("files/hsmap_tbm_2030.jpg"), image_read("files/hsmap_tdm_2030.jpg"))
```

### Proof

`review/restore/figures/`:
- `future_as_submitted_reproduced.jpg` — the buggy composite re-made from the .qmd code
  (agrees with the PDF's Figure 8 to JPEG re-encode noise: max diff 26, mean 0.12)
- `future_corrected.jpg` — the corrected composite. Top row now reads "(TBM, 2030)" /
  "(TDM, 2030)"; it differs from Figure 7 in 26.6% of pixels. Bottom row (the difference
  maps) unchanged.
- `preview_future_as_submitted.png`, `preview_future_corrected.png` — 900 px previews.

---

## 2. `distribution.jpg`, `haimatsu.jpg`, `sasa.jpg` — not photographs

They are **not** organism photographs and are **not** candidates for the Figure 1 photo the
editor invited. All three are Japanese-labelled ggplot histograms, and I found the script
that makes them: `data_from_server/ortho/analyse.R` (absent from the repo), whose last lines
are `ggsave("sasa.jpg", ...)`, `ggsave("haimatsu.jpg", ...)`, `ggsave("distribution.jpg", ...)`.

- `sasa.jpg` (4800×2700) — "ササ類の増減と地形特徴(2012年と2021年の比較)": area of *Sasa*
  No change / Increase / Decrease against nine environmental variables.
- `haimatsu.jpg` (4800×2700) — the same for dwarf pine (*Pinus pumila*), restricted to ≤2650 m.
- `distribution.jpg` (7200×2700) — "ササ類とハイマツの分布(2012年と2021年)": 2012 vs 2021
  distributions of both species over the same nine variables.

**Two findings of substance come out of these.**

**(a) A different snowmelt data vintage is documented in the figure caption.** All three carry
the caption `消雪日は2010年〜2020年の定点カメラ写真(2019年を除く)より算出` — "snowmelt DOY
calculated from fixed-point camera photographs 2010–2020, excluding 2019". That is
**2010–2018 + 2020**, i.e. it *includes 2010 and excludes 2021*, which is a different year set
from the paper's stated 2011–2021. `analyse.R` reads `data/snow/snow_mean.tif`,
`snow_reg.tif` and `snow_sd.tif` — and `snow_reg.tif` is the raster whose mean is the
manuscript's −0.86. This is direct documentary evidence for the snow agent's finding that
`snow_reg.tif` comes from a different vintage than the ten `snow/raw/*.tiff` currently on
disk, and it names the vintage. It also matches the otherwise-unexplained 2010 CSV in
`ortho/data/snow/aligned/`.

**(b) A richer snowmelt predictor set existed.** `analyse.R` uses three snow layers —
`snow_mean` (mean DOY), `snow_reg` (trend, d/yr) and `snow_sd` (interannual SD) — whereas the
published HSMs use only a single fitted DOY per year. If a reviewer asks whether interannual
variability matters, `snow_mean.tif` / `snow_sd.tif` are already on the server.

---

## 3. Figures 1, 2 and 4 — what now has a reproducible source

### Figure 1 (`overview.jpg`) — still hand-made, no code, and none exists

2634×1482 slide-style diagram with drawn block arrows, assembled from four rendered panels
(time-lapse photo stack, image-space + map-space vegetation classification, image-space
snowmelt DOY, and the risky-area map). Nothing in the recovered tree produces it and no
source document (.pptx/.key/.svg) is present. Its embedded vegetation legend also carries the
**"Maple"** mislabel discussed in §6.

Note the risky-area panel inside `overview.png` already has the north arrow and 200 m scale
bar, which dates the annotated plotting variant (§6) to before the overview slide was made.

### Figure 2 (`vegemap.jpg`) — **now reproducible**, script restored

The archived `scripts/sdm/plot_vegetation_map.R` writes Japanese-labelled maps to
`ortho/data/2012_5x5.png` and does `library(ggspatial)` **without ever calling it** — a dead
import. The published `2012_5x5_en.png` / `2021_5x5_en.png` are the same maps with English
labels, a north arrow and a 500 m scale bar. That variant was never archived.

I reconstructed it: `review/restore/code/plot_vegetation_map_en.R`. Running it against
`data_from_server/ortho/data/vege_{2012,2021}_5x5.tiff` produces
`review/restore/figures/{2012,2021}_5x5_en_restored.png` at exactly the published 3600×2400.
The raster layer, colour map, legend and axes reproduce; what does **not** reproduce exactly
is cosmetic and unrecoverable from the archive: `st_contour()`'s break interval (the default
in the current `stars` gives 200 m / 13 lines, the published figure used a finer interval)
and the specific `ggspatial` scale-bar and north-arrow styles. With a 20 m contour interval
pixel agreement is 84.2%; the residual is entirely contour lines and the two annotations.

### Figure 4 (`expanded_area.pdf`) — still hand-made, but its data layer is recovered

The PDF contains two embedded JPEGs (509×526 and 1022×926) — QGIS screenshots over a
GSI 地理院地図 basemap — plus vector callout boxes and leader lines added by hand. No
producing code exists anywhere. **However**, the underlying data layer is present:
`data_from_server/ortho/data/sasa_inc.tiff` is the 2012→2021 *Sasa* increase raster
(`sdm_tbm.R:446` reads it), and `select_sasa_communities.R` (recovered, see §5) makes
`selected_comms.tiff`. A fully scripted replacement for Figure 4 could be written from
`sasa_inc.tiff` if the revision wants one.

Two things about Figure 4 that bear on the revision: it has **Japanese place-name text baked
in** (玉殿岩屋, 立山室堂山荘, 地理院地図), and it is the only figure supplied as a PDF rather
than a raster.

---

## 4. `2012_5x5.csv` / `2021_5x5.csv` — the bridge is closed, and the driver script is recovered

### The missing driver was found, as a real script

`data_from_server/ortho/georectify.R` (**absent from the repo**) is the driver, not just the
`.Rhistory` fragment. Its final block is literally:

```r
files <- c("data/2012_5x5.csv", "data/2021_5x5.csv")
out_paths <- str_replace(files, "csv", "tiff") %>% str_replace("data/", "data/vege_")
for (i in 1:length(files)) interpolate(files[i], out_paths[i], 1.0, 1.0)
```

`interpolate()` left-joins `data/georectified.csv` (`u,v,x,y,z`) to the class CSV on `(u,v)`,
`st_rasterize()`s at 1 m in EPSG:6690, then runs `ceiling(max_dist/res) = 1` pass of
`terra::focal(3, modal, na.policy="only")` to fill single-cell holes. The same function then
makes every `data/snow/raw/*.tiff`. **Schema of the CSVs: `u,v,data` — image pixel column,
image pixel row, and class code 0–7.**

### Reproduced

`review/restore/code/reproduce_vege_2012.R` runs that recipe and compares against the archived
`vege_2012_5x5.tiff` on the exact same 1801×1753 grid:

```
st_rasterize only               : overlapping cells = 541,227, exact agreement = 99.9826%
st_rasterize + 1 focal modal fill: overlapping cells = 628,637, exact agreement = 99.7588%
```

The bridge from image-space classification to map-space raster is therefore **closed and
demonstrated**.

Two caveats, stated plainly:

1. **The comparison covers only part of the scene** — because the recovered CSVs are
   truncated (§7). `georectified.csv` survives only for image rows v∈[529, 1268], which is the
   upper (high-elevation) part of the frame. That band contains dwarf pine, no-vegetation,
   other vegetation, rowan, alder and birch, but **zero *Sasa* cells**, so class 1
   specifically could not be checked. The mechanism is proven; the *Sasa* pixels themselves
   were not re-derived.
2. **`terra::modal` no longer works as the original code calls it.** On terra 1.9.34,
   `terra::modal(c(1,2,2,3))` errors with *"unable to find an inherited method for function
   'modal' for signature x = 'numeric'"*. The archived `interpolate()` passes
   `fun = terra::modal` to `terra::focal`, so **the recovered pipeline does not run on a
   current R stack without a one-word edit** (`fun = "modal"`, focal's built-in, which I
   verified gives the same answer on a toy raster). Worth fixing in whatever code is
   deposited with the revision.

### And what the CSVs are *not*

`image_to_csv.py`'s commented-out block names the source: `results/use_this/2012_5x5.npy`.
That array is not in the repo and not in the recovered tree. I rebuilt the 5-fold ensemble
from `data_from_server/scripts/runs/cv/2012_5x5/fold_*/pred.npy` exactly as `run_rnn.py` does
(`torch.mode` over folds) and compared it, `+1`-shifted and masked, to the CSV:

```
agreement csv == (5x5 fold-ensemble +1, masked->0): 96.00%
```

96%, not 100%. The class-count profiles line up class-for-class (csv 2 ↔ ens 1 "other
vegetation" 4.27M/4.42M; csv 7 ↔ ens 6 "dwarf pine" 1.88M/1.82M; etc.), confirming the `+1`
offset, but the published maps came from a *selected* run (`use_this/`) that is still missing.
**Recovering `results/use_this/{2012,2021}_5x5.npy` from the server would close the last
gap between the neural network and the published vegetation maps.**

---

## 5. `mask.npy`, `aligned/`, `composite/` — the value-0 ambiguity is resolved

### What the mask is

```
mask.npy: shape (3744, 5616), dtype float64, values {0.0: 9,246,554, 1.0: 11,779,750}
```

A binary valid-pixel mask over the full camera frame; 56.02% valid. It agrees with the
non-black region of `mrd_085_eos_vis_20151010_1205_masked.png` in **99.976%** of pixels, i.e.
it *is* the mask `apply_mask.py` applies (sky removed by `mask_sky.py`'s column-wise
brightness scan, intersected with the aligned-image footprint). `data_from_server/data_source/mask.npy`
is byte-identical (md5 `636966ca…`) to the repo's `data/images/mask.npy`, and its size is
exactly the expected 5616×3744 float64 + 128-byte header, so it is **not** truncated.

### The ambiguity, and why it is resolved

The audit was right that there is an ambiguity, and it is real **in `results/*_masked.npy`**:

```
results/2012_masked.npy values: {0: 9,515,000, 1: 6,611,414, 2: 1,691,081, 3: 376,522,
                                 4: 214,259, 5: 528,289, 6: 2,089,739}
```

These are **0-based** class indices (0 = Dwarf Bamboo per `run_rnn.py`'s colormap), and
`apply_mask.py` writes **0** for masked pixels — so in those files masked sky and *Sasa* are
the same number and cannot be told apart.

**But the ortho pipeline does not use those files.** `2012_5x5.csv` is `+1`-shifted, with 0
reserved for masked. I verified this exactly: over the CSV's complete rows the count of
`data == 0` is **4,632,006**, and applying `mask.npy` to the same rows gives **4,632,006**
zeros — an exact match, cell for cell. So in `2012_5x5.csv`, `2021_5x5.csv` and hence
`vege_{2012,2021}_5x5.tiff`, **value 0 means "masked", classes are 1–7, and *Sasa* is
unambiguously 1**. The 5,191 zero-valued cells in `vege_2012_5x5.tiff` (0.43% of the map) are
masked pixels, not a vegetation class. The partial reproduction in §4 also carries 0 through
as 0 (1,206/1,206 cells agree), confirming it is a real data value in the CSV rather than a
nodata marker introduced by rasterisation.

**Recommendation for the revision:** say so explicitly in the methods, and if any code is
deposited, change `apply_mask.py` to write `-1` or `255` for masked pixels instead of 0.

### `aligned/`, `composite/`, and the alignment evidence

`data_from_server/data_source/` is largely a superset of the repo's gitignored
`data/images/` (all shared files md5-identical). What is **new**:

- `aligned/2015/` (7 images) and `aligned/old/{2012,2021}/` (22 images) — earlier alignment runs
- `aligned_composite/{2012,2021}/` and `composite/{2012,2021}/` — 7 images/year of a
  **composite** variant, matching `run_rnn_composite.py` / `run_crnn_composite.py` and the
  `2012_composite_5x5` CV runs. A model variant the paper never mentions.
- `normalized/{2012,2015,2021}/` + `normalize_images.py` — the chromatic-normalisation variant
  (R/(R+G), G/(R+G), HSV saturation), matching `run_rnn_normalized.py`
- `alignment_report.csv` — 2015 frames vs `sky_mask_2015.png`, RMSE 0.004–1.02 px
- `alignment_report_2012_2021.csv` — **quantitative alignment accuracy for every 2012 and 2021
  frame** against the 2015-10-10 reference: RMSE 0.86–1.37 px with 1,000–9,800 matched
  keypoints for the frames that were kept, and it also shows *why* three frames were dropped
  (20120922_0600: 111 matches; 20210921_1405: 155; 20210924_1105: 19, RMSE 2.06 px). This is
  exactly the co-registration evidence a reviewer would ask for and it is not in the paper.
- `align_photographs.py` — the AKAZE + FLANN + RANSAC homography with a 12-parameter
  lens-distortion optimisation. Present in the repo too (`data/images/`), md5-identical.
- `MRD_snowfront_L_2014_0818_120-230_BW.png`, `mrd_085_eos_vis_20151010_1205.png`,
  `sky_mask_2015.png`, `as_animation.py` — reference/utility assets.

**A live bug:** `scripts/vegetation_classification/apply_mask.py:4` reads
`data_source/mrd_085_eos_vis_20151010_1205_mask**d**.png`; the actual file, both on the server
and in the repo, is `..._mask**ed**.png`. The archived script cannot run as written.

---

## 6. The class-5 label conflict — **resolved: it is Erman's birch, not maple**

The manuscript (`matmet.qmd:39`) lists the seven classes as including *"Maple (Acer
tschonoskii)"*. `scripts/sdm/plot_vegetation_map.R:37` and the recovered `ortho/analyse.R`
both say ミネカエデ / "Maple". The training annotations say otherwise.

I read the class index directly out of the ground-truth label JSONs
(`data_source/labels/*.json`, the Semantic Segmentation Editor exports that `utils.py`'s
`read_sses()` consumes):

```
classIndex 0 -> ハイマツ        (remapped to 7 by read_sses)   Pinus pumila
classIndex 1 -> ササ                                           Sasa
classIndex 2 -> その他植生
classIndex 3 -> 無植生
classIndex 4 -> ナナカマド                                     Sorbus
classIndex 5 -> ダケカンバ                                     Betula ermanii
classIndex 6 -> ミヤマハンノキ                                 Alnus viridis ssp.
```

This is corroborated three more ways: the per-fold TensorBoard scalar directories are named
`val_ササ`, `val_ダケカンバ`, `val_ミヤマハンノキ`, … (never `val_ミネカエデ`);
`run_rnn.py:16` comments class 5 as ダケカンバ; and no label file anywhere contains ミネカエデ.
The mapping also matches the raster codes exactly (1 = *Sasa* = 10,176 px in 2021).

**Conclusion: class 5 is ダケカンバ = *Betula ermanii*, Erman's birch. The manuscript's
"Maple (*Acer tschonoskii*)" is wrong.** This matters for Reviewer 1's question about which
shrubs replaced *Sasa*: the 85 px in that loss category are Erman's birch, and Erman's birch
is a tree/krummholz former with a very different successional story from *Acer tschonoskii*.
`plot_vegetation_map.R`, `analyse.R`, the Figure 2 legends and the `overview` slide legend all
need the label changed together.

---

## 7. **The recovered copy is truncated.** Several large files are cut at exact MiB boundaries

This is the most important operational finding in my scope, because other agents may be
computing on these files.

| file | size | size mod 1 MiB | state |
|---|---|---|---|
| `ortho/data/2012_5x5.csv` | 146,800,640 | **0** (= 140 MiB) | truncated, last line `4720,` |
| `ortho/data/2021_5x5.csv` | 58,720,256 | **0** (= 56 MiB) | truncated, last line `676,978,3` (no newline) |
| `ortho/data/georectified.csv` | 167,772,160 | **0** (= 160 MiB) | truncated, last line `…,4051131.2` |
| `ortho/data/pointcloud.db` | 109,051,904 | **0** (= 104 MiB) | almost certainly truncated |
| `ortho/data/tateyama2.tiff` | 163,577,856 | **0** (= 156 MiB) | almost certainly truncated |
| `data_source/mask.npy` | 168,210,560 | 438,400 | **complete** (exact npy size) |
| `ortho/data/mrd_dem_1m.tiff` | 132,443,318 | 322,742 | plausibly complete |
| `ortho/data/snow/fitted.csv` | 1,926,801,956 | 567,844 | plausibly complete |

Consequences:
- `2012_5x5.csv` holds image rows v = 0…2307 of 3743 (62% of the frame); `2021_5x5.csv` holds
  v = 0…978 (26%).
- `georectified.csv` holds 2,547,807 rows covering only v = 529…1268; a complete file would be
  roughly 4–5× larger. This is why §4's verification could only cover 541k of the 3.16M map
  cells and why no *Sasa* pixel appeared in the check.
- No transfer process was running when I checked and the mtimes are 1–2 hours old, so this is
  not a copy in progress. **Re-copying these five files from the server is required before any
  full reproduction of `vege_*_5x5.tiff` or the georectification is possible.**

---

## 8. `ortho/figures/*.png` vs the published figures: same maps, different plotting code

`data_from_server/ortho/figures/` is byte-identical to the repo's gitignored `ortho/figures/`.
Comparing those to `paper/files_original_size/*.png` (which are the exact PNGs the published
JPEGs were re-encoded from — same dimensions, JPEG-artifact-level differences only):

- `cv_dist`, `cv_wo_dist`, `initial_split_dist`, `initial_split_wo_dist`,
  `model_performance_tbm`, `model_performance_tdm`, `risky_tdm` — **md5-identical**.
- `hsmap_{tbm,tdm}_{2021,2030}`, `hsdiff_{tbm,tdm}`, `risky_tbm` — differ in exactly **0.27%**
  of pixels. I localised the difference: the published versions carry a **north arrow and a
  200 m scale bar** that the server versions lack. The map content is otherwise identical.
  Neither `sdm_tbm.R` nor `sdm_tdm.R` (byte-identical between repo and server) contains any
  `ggspatial` call — so, as with Figure 2, **the plotting variant that made the published maps
  is not archived anywhere**. It is a cosmetic difference, but it means "re-run `sdm_tbm.R`"
  does not regenerate the submitted figures verbatim.
- `vi_tbm`, `vi_tdm` — differ in **10.2% / 7.2%** of pixels, and the difference is *not*
  cosmetic. The server versions have a different y-axis range (up to 0.6 vs 0.26) and a
  different variable ordering (server TBM: snow, elevation, TRI, **roughness, aspect**, slope,
  TPI; published: snow, elevation, TRI, **aspect, roughness**, slope, TPI). **The server's
  variable-importance plots are from a different model run than the published Figure 6.**
  The published ordering is the one the manuscript text describes, so the manuscript is
  self-consistent; but if `sdm_tbm.R` is re-run for the revision, expect the aspect/roughness
  order to be unstable — they are within noise of each other (medians ≈0.038 vs ≈0.030 in the
  published panel), which is worth a sentence rather than an assertion of ranking.

`ortho/snowmelt_doy.png` and `snowmelt_shift_map.png` are md5-identical to the published ones;
`snowmelt_shifting.png` differs (server 70,179 B vs published 83,877 B) — the supplement's
Figure S1 was re-rendered after the server copy.

---

## 9. Other substantive items in the recovered tree

**R scripts present on the server but absent from the repo** (all of them ran as part of the
published analysis):

| file | what it does |
|---|---|
| `ortho/georectify.R` | **the missing driver**: CSV + `georectified.csv` → `vege_*_5x5.tiff` and `snow/raw/*.tiff` (§4) |
| `ortho/analyse.R` | makes `sasa.jpg`, `haimatsu.jpg`, `distribution.jpg`; documents the 2010–2020-minus-2019 snow vintage (§2) |
| `ortho/analyse_chamges.R` | *Sasa* increase/decrease vs terrain and snow change, restricted to dist < 10 m |
| `ortho/select_sasa_communities.R` | builds `selected_comms.tiff` — the 2012 *Sasa* "communities" (≥5 m² patches, aggregated to 10 m grid, >25% cover) that the TDM's **distance predictor is measured from**. Without this the TDM cannot be reproduced. |
| `ortho/sdm_sasainc.R`, `ortho/sdm_include_distance.R` | earlier/alternative SDM variants |
| `ortho/plot_snowmelt_shift.R` | the per-pixel snowmelt-slope statistic (snow agent's scope) |

**Model artefacts** not in the repo (gitignored `*.rds`): `models.rds`, `models_wo_dist.rds`,
`model_stack.rds`, `model_stack_wo_dist.rds`, `models_all_5m.rds` (135 MB total) — the fitted
tidymodels/stacks objects behind Figures 5–8.

**Metrics that reproduce and go beyond the paper.** `ortho/tss_score_tbm.csv` and
`ortho/tss_tdm.csv`:

```
TBM: boyce_cont 0.9916, roc_auc 0.8369, tss_max 0.5556   (paper: TSS 0.55)
TDM: boyce_cont 0.9246, roc_auc 0.9146, tss_max 0.7010   (paper: TSS 0.70)
```

Both TSS values reproduce. The **Boyce index and ROC-AUC were computed but never reported** —
they are readily available if the revision needs additional discrimination/calibration metrics.

**Georectification accuracy evidence**, all recovered and none of it in the paper:
- `ortho/data/params_optim.json` — the optimised camera model: position (732731, 4051171,
  2458 m), FOV 72.84°, pan 96.63°, tilt 0.026°, roll 0.116°, full Brown–Conrady distortion
  set, and **`"error": 4.342` px** — the reprojection RMSE of the georectification.
- `ortho/data/matched.png` — side-by-side of the photograph and the DEM-rendered synthetic
  view with the matched GCPs overlaid; `initial.png` / `optimized.png` — before/after the
  parameter optimisation.
- `ortho/data/gcp.csv` (24 KB) — the ground control points themselves.
- `data_source/alignment_report*.csv` — sub-pixel inter-frame co-registration RMSE (§5).

Together these answer the "how accurate is your georectification / co-registration" question
quantitatively: **~4.3 px reprojection error for the camera model, ~1 px RMSE for frame-to-frame
alignment.** I would put this in the revised methods or supplement.

**Prediction rasters** available for any re-analysis: `sasa_pred_{sdm,tdm}_{12,21,30}.tiff`,
`sasa_pred_sdm_dist_{21,30}.tiff`, `sasa_pred_tdm_30_bin.tiff`, `risky_area*.tiff`,
`potential_sasa_area_21.tiff`, `sasa_inc.tiff`, `selected_comms.tiff`, `dem_small.tiff`,
`terrain_features/`.

---

## Summary of what a reviewer-facing fix list looks like

1. **`results.qmd:98`** — one-line change to the 2030 maps. Verified; corrected composite
   supplied. *The submitted paper has no 2030 map at all.*
2. **Class 5 is Erman's birch (*Betula ermanii*), not Maple (*Acer tschonoskii*)** — fix
   `matmet.qmd:39`, both Figure 2 legends, the `overview` slide legend, `plot_vegetation_map.R`
   and `analyse.R` together.
3. **Snowmelt vintage** — `analyse.R`'s caption states 2010–2020 excluding 2019, contradicting
   the paper's 2011–2021. Reconcile before quoting −0.86 again.
4. **Deposit the recovered scripts** — `georectify.R` and `select_sasa_communities.R` in
   particular; without them the pipeline is not reproducible even in principle.
5. **Two live bugs in the archived code**: `apply_mask.py`'s `maskd`/`masked` filename typo,
   and `fun = terra::modal` which no longer works on terra ≥ 1.9.
6. **Re-copy the five truncated files** (§7) before attempting a full reproduction.
7. **Recover `results/use_this/{2012,2021}_5x5.npy`** — the last missing link between the
   classifier and the published vegetation maps (the fold ensemble gets to 96%, not 100%).
8. **Value 0 = masked, not a class** — state it in the methods; consider changing the sentinel.
9. Optional but strong: report the recovered `params_optim.json` reprojection error (4.34 px)
   and the per-frame alignment RMSE (~1 px), and the already-computed Boyce/AUC metrics.
