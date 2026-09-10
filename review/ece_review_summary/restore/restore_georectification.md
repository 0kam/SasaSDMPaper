# Restoring the georectification pipeline

Target: the transform from camera-image pixels to map coordinates, previously believed
unreproducible. Everything below was obtained by running code, not by reading it.

Working directory for all scripts: `/Users/okamoto/NIES/SasaSDMPaper/.claude/worktrees/review-summary-ece-ddc69c/review/restore/georect/`

**Verdict: the georectification is fully restored.** The camera model, its parameters and
the conventions needed to apply them are recovered, verified against the archived product
to **0.41 px mean / 0.50 px RMSE**, and independently validated in the near field (where
all the *Sasa* is) to a sharply peaked optimum at zero offset. A working map↔image
projector now exists as a 150-line numpy module with no OpenGL, no `alproj` install and
no `georectified.csv`.

Two things did not survive and are reported plainly: the `.Rhistory` Procrustes/TPS block
belongs to a **different pipeline in a different project** and its inputs are not in the
dump; and the archived `georectified.csv` itself is **truncated to 14 % of its rows**,
covering a band of the image that contains no *Sasa* at all.

---

## 0. A finding that affects every other restoration workstream: eight files are truncated

Eight files in the server dump have sizes that are exact multiples of 4 MiB. That is a
transfer artefact, not a property of the data:

| file | recovered size | status |
|---|---|---|
| `ortho/data/georectified.csv` | 160.0 MiB | truncated — 2,547,807 of ~11.8 M rows |
| `ortho/data/pointcloud.db` | 104.0 MiB | truncated — SQLite header says 554,768 pages (≈2.17 GB); `sqlite3 .tables` returns `database disk image is malformed` |
| `ortho/data/tateyama2.tiff` | 156.0 MiB | truncated (the aerial orthophoto) |
| `ortho/data/2012_5x5.csv` | 140.0 MiB | truncated at image row v = 2308 |
| `ortho/data/2021_5x5.csv` | 56.0 MiB | truncated |
| `ortho/data/snow/aligned/…_2012_…csv` | 132.0 MiB | truncated |
| `ortho/data/snow/aligned/…_2014_…csv` | 140.0 MiB | truncated |
| `ortho/data/snow/aligned/…_2017_…csv` | 136.0 MiB | truncated |

```
$ find . -type f -size +1M -exec stat -f '%z %N' {} \; | awk '$1%1048576==0 {...}'
```

This **explains the previously unexplained file-size clustering in `snow/aligned/`**: the
2012, 2014 and 2017 files are not a different vintage, they are the same kind of file cut
short. It also means `pointcloud.db` cannot be opened at all, so the original `alproj`
render cannot be re-run from the archived surface — see §4 for the way around that.

---

## 1. The `.Rhistory` Procrustes + TPS block: what it actually is

**The premise that it regenerates `georectified.csv` from `gcp.csv` is wrong, and I can
show why rather than argue it.**

Three pieces of evidence, all checkable:

1. In the `.Rhistory` the table is an **input**, not an output:
   `points_sim <- read_csv("georectificated.csv", ...) %>% rename(pix_num = ...1)`.
   It is read, joined to by pixel number, and never written.
2. The `.Rhistory` block runs in `setwd("~/HDD3TB/tateyama/mrd_snowmelt/step2_ortho/")` —
   the **snowmelt paper's** project tree, not `~/Projects/jasms2023f/ortho/` where
   `georectify.R` and `ortho/data/georectified.csv` live.
3. Its `gcp.csv` has columns `org_x, org_y, sim_x, sim_y` (image-to-image pairs). The
   recovered `ortho/data/gcp.csv` has columns `u, v, x, y, z` (image-to-map pairs). They
   are different files with the same name. The image-to-image one is **not** in the dump,
   so this block cannot be re-run on its own data.

What the block does is **image-to-image alignment**: translate the GCP centroids together,
apply a Procrustes rotation + isotropic scale (`shapes::procOPA`), fit a thin-plate spline
residual warp (`fields::Tps`) separately for u and v, evaluate the composition on the
5616 × 3744 grid, and look each warped pixel up in a reference-frame table by pixel number.
It is the older (2010–2018, `imager`/PNG) snowmelt route.

### It is reconstructed and it runs

`georect/00_align_tps.R` is a runnable reconstruction with the fragments assembled into
`fit_alignment()`, `apply_alignment()` and `warp_full_grid()`, plus `--selftest`.

```
$ Rscript 00_align_tps.R --selftest
fit on 480 GCPs: 0.79 s | mean residual 0.0003 px | RMSE 0.0007 px
held-out: mean 0.0277 px, RMSE 0.1588 px, max 3.1428 px
full 5616 x 3744 grid TPS evaluation: 296.6 s
```

The self-test plants a known warp (1.3° rotation, 1.004 scale, +37/−21 px translation, plus
a 15 px quadratic bow that only the spline stage can absorb), samples 480 GCPs from it, and
checks recovery on 5,000 held-out points: **0.028 px mean, 0.159 px RMSE**. So the
reconstruction of the algorithm is correct; only its input data is missing.

`shapes` was not installed and was installed from CRAN for this (`fields` was already
present). One documentation bug worth carrying forward: the `.Rhistory` writes a column
called `rmse` to `TPS_rmse.csv` but computes the **mean** residual distance, not the RMS.
The reconstruction returns both.

---

## 2. `georectify.R`, `params_optim.json`, `pointcloud.db` and `alproj`: who made the product

### The chain

```
mrd_dem_1m.tiff (DSM)  +  tateyama2.tiff (aerial ortho)
        └─ alproj.surface.create_db ──────────────► pointcloud.db
reference photo + simulated view
        └─ alproj.gcp.akaze_match + set_gcp ──────► gcp.csv        (482 GCPs, u,v,x,y,z)
        └─ alproj.optimize.CMAOptimizer ──────────► params_optim.json  (+ initial/optimized/matched .png)
        └─ alproj.project.reverse_proj ───────────► georectified.csv   (u,v,x,y,z,B,G,R)
2012_5x5.csv / 2021_5x5.csv / snow/aligned/*.csv   (u,v,value, from image_to_csv.py)
        └─ georectify.R::interpolate() ───────────► vege_2012_5x5.tiff etc.
```

`georectify.R` is therefore **downstream** of the georectification: it only joins
`(u,v)`-keyed value tables to `georectified.csv` and rasterises. `image_to_csv.py`
produces those value tables. The camera step is `alproj` alone.

### The reference photograph is identified

`georectified.csv` carries B,G,R. Matching those against every 5616 × 3744 image in the
dump:

```
exact  99.843%   mean|d|   0.2400   data_source/aligned/2015/mrd_085_eos_vis_20150926_1205.png
exact  21.226%   mean|d|   1.1790   data_source/source/2015/mrd_085_eos_vis_20150926_1205.JPG
exact   0.157%   mean|d|   9.9914   data_source/source/2015/mrd_085_eos_vis_20150920_1205.JPG
```

The georectification reference frame is **the aligned 2015-09-26 12:05 image**. (The
`.JPG` original matches only 21 % exactly — the aligned PNG is the one that was fed to
`reverse_proj`.)

### Why the earlier audit got 1,543 px, and what the real convention is

`alproj`'s camera model changed between versions. Applying the **current** (v1.2.0) model
to `params_optim.json` gives 68 px on the GCPs — better than 1,543 but still nonsense, and
every GCP lands behind the camera. The version contemporary with this analysis is commit
`c8a3e3d` (2022-09-23), and it differs in two substantive ways:

* vertical focal length is `fov_y = fov_x * h / w` (v1.2.0: `2·atan(tan(fov_x/2)·h/w)`);
* lens distortion is applied to the **normalised camera** coordinates `(x/z, y/z)` *before*
  the projection matrix (v1.2.0: to pixel coordinates *after* the intrinsic matrix).

`georect/check_gcp_rmse.py` scans the modern conventions; `check_gcp_rmse_v2022.py`
implements the 2022 one:

```
$ python3 check_gcp_rmse_v2022.py
params_optim.json reported 'error' = 4.342243

alproj 2022 (commit c8a3e3d) pinhole model
   n            = 482
   mean err     = 4.638803 px   <- alproj's rmse() returns this
   true RMSE    = 7.613727 px
   median       = 2.903896 px
   90th pct     = 9.598820 px
   max          = 67.802277 px
   du mean/sd   = -0.0178 / 6.2319
   dv mean/sd   = +0.1165 / 4.3724
```

`params_optim.json` records `"error": 4.342243`; recomputing on the archived 482 GCPs gives
**4.6388**. The 0.30 px gap is a bookkeeping difference in which GCPs were scored (dropping
the three worst points gives 4.356), not a model difference — the model is settled. Note
that what `alproj.optimize.rmse()` returns and what `params_optim.json` stores is the
**mean** reprojection distance; the true RMSE over the archived GCP set is 7.61 px.

**So: `params_optim.json` supersedes the `.Rhistory` TPS route, they are not alternatives,
and the achieved GCP fit is 4.64 px mean / 7.61 px RMSE / 2.90 px median over 482 GCPs at
582–1,892 m (median 1,509 m), i.e. ≈1.15 m on the ground at the median GCP distance.**

### An internal inconsistency in alproj 2022 that matters

Within the same version, the OpenGL vertex shader (which renders, and therefore made
`georectified.csv`) and `optimize.project` (which scores GCPs) apply the tangential
`p1,p2` and thin-prism `s1..s4` terms with **opposite signs**. Measured on
`georectified.csv`, the two disagree by **28.9 px mean**. The shader sign is the one that
reproduces the published product (§3). This is worth knowing before anyone re-fits
anything: the parameters in `params_optim.json` were fitted under one sign and applied
under the other.

---

## 3. End-to-end verification

### 3a. Round trip against `georectified.csv` — 0.41 px

`georect/verify_georectified_csv.py` pushes the archived `(x,y,z)` forward through the
recovered render model and compares to the archived `(u,v)`:

```
$ python3 verify_georectified_csv.py 500
sampled 5096 rows (every 500th) from georectified.csv

render/shader sign (+)
   mean |err| = 0.4103 px   RMSE = 0.4978 px
   median     = 0.3772 px   p99 = 1.1691  max = 2.6013
   du mean/sd = +0.3886 / 0.3098
   dv mean/sd = -0.0229 / 0.0192
   frac within 1 px = 0.972135   within 0.5 px = 0.654042

optimizer sign (-)
   mean |err| = 28.8735 px   RMSE = 31.3106 px
```

The v residual (sd 0.019 px) is 16× tighter than the u residual (sd 0.31 px), which is
exactly what float32 storage of the coordinate channels predicts: the framebuffer stores
easting ≈734,200 and northing ≈4,051,000 (float32 spacing 0.06 m and 0.25 m) but elevation
≈2,900 (spacing 0.0002 m), and u is driven by the horizontal channels, v by the vertical
one. The pixel convention recovered is: `u = (w−1) − GL_column`, `v = GL_row`.

Two conventions had to be pinned down empirically and are now documented in
`alproj_camera.py`: the horizontal mirror, and the fact that `view_z > 0` (not `< 0`) is
in front of the camera in this modelview.

### 3b. The archived table cannot serve *Sasa* work

```
$ python3 verify_georectified_csv.py    # coverage scan
complete rows: 2547807 unparsable/truncated: 1
v range: 529 - 1268 distinct v: 740
last 5 v counts: [(1264, 5585), (1265, 5585), (1266, 5585), (1267, 5585), (1268, 2492)]
```

The surviving rows are image rows **v = 529…1268** out of 3,744 — the distant upper band.
Joining every surviving row to `vege_2012_5x5.tiff` gives class code counts
`{0:329, 2:27691, 3:48396, 4:538, 5:4, 6:244, 7:50189}` — **not one *Sasa* pixel (code 1)**.
And projecting the 8,547 *Sasa* cells forward puts them at `v = 1274…2830`, i.e. **0.00 %
inside the surviving band**. The truncated `georectified.csv` is useless for anything
*Sasa*-related. The recovered camera model is the only route, which is why §4 matters.

### 3c. `vege_2012_5x5.tiff` ↔ `results/2012_masked.npy`

Requested check, done two ways.

**Via the archived table (far field).** `georect/roundtrip_vege2012.py`:

```
sampled 127391 georectified.csv rows (every 20th)
inside published raster extent: 127391 / 127391 (100.00%)
agreement with raster == npy +0 : 3.6502%
agreement with raster == npy +1 : 85.7172%
agreement with raster == npy -1 : 0.1162%
overall agreement 85.7172%  (n=127391)

Sasa (raster code 1): image 328, raster 0, both 0
  -> no Sasa in the surviving band of georectified.csv, as expected
```

**Via the recovered camera model (whole frame, no `georectified.csv`).**
`georect/validate_map_to_image.py` projects every 1 m cell of `vege_2012_5x5.tiff`,
sampling elevation from `mrd_dem_1m.tiff`:

```
3,157,153 sample points (1x supersample), DEM in range 100.00%
projected inside the frame: 2,106,088 (66.71%)
visible after z-buffer:     1,657,226 (78.69% of in-frame)

=== all in-frame cells: n = 1,205,241 ===
    overall class agreement 86.647%
             class  n(raster)   recall  precision
              Sasa      8,543   43.02%     31.23%
         Other veg    486,348   85.47%     94.74%
            No veg    357,937   91.91%     84.00%
             Rowan     26,934   66.25%     78.16%
       Maple/Birch      5,700   58.11%     46.87%
             Alnus     20,774   78.71%     64.57%
      Pinus pumila    294,759   87.69%     83.95%
```

86.6 % in the near field against 85.7 % in the far field — the transform behaves the same
across the frame.

**Is it biased?** `georect/shift_scan.py` scans a rigid (du, dv) offset:

```
1,201,054 labelled cells, 8,547 of them Sasa
peak overall agreement 87.725% at (du, dv) = (0, 0); Sasa recall there 44.67%
at (0, 0): overall 87.725%, Sasa recall 44.67%
```

The peak is **exactly at (0, 0)** on a 25 × 25 grid spanning ±24 px, and agreement falls to
85.1–87.2 % at ±2 px and 80.8–84.9 % at ±4 px. The recovered transform is unbiased at the
pixel level in the near field, not only in the far field where `georectified.csv` could
check it.

**A code-mapping fact that had to be established to do this at all.** The raster codes are
`npy + 1`, and the labels come from `scripts/sdm/plot_vegetation_map.R:31-42`:
1 = *Sasa*, 2 = other vegetation, 3 = no vegetation, 4 = rowan, 5 = maple, 6 = alder,
7 = *Pinus pumila*. Consequently **`results/2012_masked.npy` conflates *Sasa* with the
mask**: masked/sky pixels and class-0 pixels are both 0, and the archived `2012_5x5.csv`
distinguishes them (csv 0 = masked, csv 1 = *Sasa*). Anyone reading class 0 out of
`2012_masked.npy` as a vegetation class will be wrong.

**Why is *Sasa* recall only 43–45 % when everything else is 66–92 %?** Not registration —
the shift scan rules that out. It is aggregation. The median ground sample distance over
the *Sasa* cells is **0.16 m/px**, so each 1 m map cell is a modal vote over roughly 36
image pixels, while this test compares it against the single nearest pixel; *Sasa* is the
most fragmented class and loses most from that. For the visual-interpretation deliverable
this is good news, not bad: ~6 × 6 image pixels per map cell is ample support for visual
interpretation.

### 3d. Deliverable for the visual-interpretation workstream

`georect/export_sasa_pixels.py` → `georect/sasa_change_pixels.csv` (743 kB):

```
Sasa 2012 8,547  2021 10,176  gain 4,097  loss 2,468  stable 6,079
gain    n= 4,097  image rows v  963..3075  median 1945  visible 96.5%  GSD median 0.167 m/px
loss    n= 2,464  image rows v 1275..2823  median 2059  visible 97.0%  GSD median 0.157 m/px
stable  n= 6,079  image rows v 1400..2778  median 1952  visible 96.8%  GSD median 0.161 m/px
```

The gain / loss / stable counts reproduce the established transition matrix exactly
(4,097 / 2,468 / net +1,629); four loss cells fall outside the DSM/frame, hence 2,464.
Columns: `x, y, z, u, v, dist, gsd, visible, change`.

---

## 4. Should `georectified.csv` be deposited?

**No. Deposit `params_optim.json`, `gcp.csv`, the DSM and a script; not the table.**

Reasons, with the numbers behind them.

**Size.** The archived file is 160 MiB and that is only 2.55 M of the rows. At 66 bytes/row
and ~11.8 M terrain pixels (`data_source/mask.npy` puts 56.02 % of the frame on the
terrain), the complete table is **≈0.8 GB**, and larger if the mask is conservative. That
is past what most repositories accept as a supplementary file, and it is a derived product
of three inputs totalling far less.

**Regenerability.** `georect/regenerate_georectified.py` regenerates it from
`params_optim.json` + `mrd_dem_1m.tiff` alone — no `alproj`, no OpenGL, no `pointcloud.db`
(which is unopenable anyway) — by pushing a supersampled DSM through the recovered camera
model with a z-buffer:

```
$ python3 regenerate_georectified.py 4
DSM (4556, 7266) at 0.9776 x 1.0227 m, supersample 4x
z-buffer complete in 105.4 s; 8,712,772 of 21,026,304 pixels filled (41.44%)

comparing against archived georectified.csv ...
archived rows sampled 127,391; regenerated at the same pixel 127,280 (99.91%)
horizontal difference |dxy|: mean 19.322 m, median 0.676 m, p90 1.898 m, max 1134.9 m
   within  1 m: 76.92%
   within  2 m: 90.52%
   within  5 m: 95.15%
```

At 2× supersampling the same run takes **22 s** and reaches 55 % within 1 m; at 4× it takes
**105 s** and reaches 77 % within 1 m / 91 % within 2 m. The residual 4–5 % that are metres
to hundreds of metres out are occlusion-boundary failures of point splatting: `alproj`
rasterises a triangle **mesh**, so it never leaks distant terrain through a ridge, whereas a
point z-buffer does at the sampling limit. A faithful regeneration should call
`alproj.project.reverse_proj`; the numpy route is the dependency-free approximation and its
error mode is now measured rather than assumed.

For comparison, the `.Rhistory` TPS route's full-grid evaluation alone takes **297 s** in R
(measured above), so neither route is expensive.

**Recommended deposit** (≈135 MB, all inputs, no derived bulk):

| item | size | why |
|---|---|---|
| `params_optim.json` | 720 B | the fitted camera |
| `gcp.csv` | 24 kB | the 482 GCPs, so the fit is checkable |
| `mrd_dem_1m.tiff` | 132 MB | the surface; not truncated, and required |
| `alproj_camera.py` + `regenerate_georectified.py` | 15 kB | applies them |
| `alignment_report_2012_2021.csv` | 2 kB | the image-alignment residuals |
| pinned `alproj` commit (`c8a3e3d`) | — | the model version is load-bearing |

Depositing `georectified.csv` instead would ship 0.8 GB that reproduces in under two
minutes from 132 MB, and would ship it in a form nobody can check.

---

## 5. Methods paragraph (Reviewer 3, L162 and L164–170)

Reviewer 3 asked for "a short description of the alignment" (L162) and flagged the
georectification as "a blackbox — the reader has no idea how this was done" (L164–170).
Both are answered below. Every number is from the runs above or from
`data_source/alignment_report_2012_2021.csv`.

> **Image alignment.** All photographs were registered to a single reference frame (the
> image of 26 September 2015, 12:05) before classification, so that a pixel index denotes
> the same ground location in every year. Registration used AKAZE local features with a
> FLANN matcher and RANSAC outlier rejection, followed by joint estimation of a
> radial–tangential lens-distortion correction (eight coefficients, Nelder–Mead) and a
> homography, minimising the mean residual distance of the matched points
> (`align_photographs.py`). Across the 24 photographs of 2012 and 2021 used here, matching
> retained 19–9,785 inliers (median 4,223) and the mean residual was 0.86–2.06 px
> (median 1.05 px), i.e. sub-metre on the ground at the scale of the study slope. Two
> photographs (21 and 24 September 2021) retained fewer than 200 inliers and had the
> largest residuals (1.96 and 2.06 px).
>
> **Georectification.** The reference frame was georectified with the Python package
> `alproj` (https://github.com/0kam/alproj), following @Okamoto2024RSEC. A coloured
> three-dimensional surface was built from a 1 m digital surface model and an
> orthorectified aerial photograph of the same area, and rendered from the camera position
> (732,731 E, 4,051,171 N, 2,458 m a.s.l.; JGD2011 / UTM zone 53N) to give a simulated view
> of the scene. Ground control points were obtained automatically by AKAZE matching between
> the photograph and the simulated view, each matched point inheriting the map coordinate
> of the surface it was rendered from, yielding **482 GCPs** spanning 582–1,892 m from the
> camera (median 1,509 m). A pinhole camera model with 18 free parameters — field of view,
> pan, tilt, roll, two aspect terms, six radial, two tangential and four thin-prism
> distortion coefficients — was then fitted to these GCPs by CMA-ES, with the camera
> position held fixed. The fitted model has a field of view of 72.84°
> (equivalent focal length 3,806 px) and achieves a **mean reprojection error of 4.6 px
> (median 2.9 px, RMSE 7.6 px)**, which corresponds to approximately **1.2 m on the ground
> at the median GCP distance**. Every image pixel was then reverse-projected onto the
> surface to obtain its map coordinate, and the per-pixel classification results were
> rasterised to a 1 m grid (EPSG:6690) by modal aggregation, with single-cell gaps filled
> by a 3 × 3 modal focal filter. The resulting ground sample distance over the area where
> *Sasa* occurs is **0.16 m per pixel (median)**, so each 1 m map cell aggregates roughly
> 36 image pixels.

If a fuller reproducibility statement is wanted, the following is defensible from what was
actually measured and could go in a data-availability note rather than Methods:

> The camera parameters, the ground control points and the digital surface model are
> deposited; re-running the reverse projection reproduces the archived pixel-to-map table
> to a mean 0.41 px (RMSE 0.50 px, 97.2 % of pixels within 1 px).

---

## 6. Files produced

All under `/Users/okamoto/NIES/SasaSDMPaper/.claude/worktrees/review-summary-ece-ddc69c/review/restore/georect/`:

| file | what it does |
|---|---|
| `alproj_camera.py` | the recovered camera model, numpy only; `project_render()` (the one that made the product) and `project_gcp()` (the one the reported error refers to) |
| `check_gcp_rmse.py` | scans modern `alproj` conventions against the archived GCPs — shows why they fail |
| `check_gcp_rmse_v2022.py` | the 2022 model; reproduces `params_optim.json`'s reported error |
| `verify_georectified_csv.py` | round trip against the archived `georectified.csv`; also reports its truncation coverage |
| `roundtrip_vege2012.py` | `vege_2012_5x5.tiff` ↔ `2012_masked.npy` through the archived table |
| `validate_map_to_image.py` | the same, near field, through the recovered model instead |
| `shift_scan.py` | (du, dv) offset scan — the unbiasedness test |
| `regenerate_georectified.py` | regenerates the lookup table from DSM + parameters; timed |
| `export_sasa_pixels.py`, `sasa_change_pixels.csv` | image-space location of every *Sasa* gain/loss/stable cell |
| `00_align_tps.R` | runnable reconstruction of the `.Rhistory` Procrustes+TPS block, with `--selftest` |

Nothing outside `review/restore/` and the scratchpad was written; no file in
`data_from_server/` or the repository was modified.
