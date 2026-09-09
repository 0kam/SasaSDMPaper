# Image interpretation of *Sasa*-loss pixels — feasibility report

**Question.** Reviewer 1 challenged the sentence in `paper/results.qmd:12`:

> "Many of these apparent decreases likely resulted from shrub encroachment and
> growth. Under such conditions, *Sasa* can persist beneath shrub canopies but
> becomes undetectable in time-lapse imagery. Therefore, we adopted the
> expansion area (4,095 m²) as the primary metric of change."

That claim is currently asserted, not evidenced. Reviewer 1 suggested field
observations or additional image interpretation. Field work is impossible, so:
**can we go back to the photographs and look?**

**Answer: yes.** The data support it well, the crops are genuinely interpretable
over most of the loss area, and a first quantitative pass on the sampled crops
already contradicts part of the paper's stated interpretation. Details, evidence
and caveats below.

---

## 1. Verdict in one paragraph

The 2012 and 2021 photographs live in a *single common image space* (every frame
of both years was warped onto one 2015 reference frame), and the residual
2012↔2021 misregistration is **1 px median, ≤2 px for 86 of the 88 sampled
locations**. Ground resolution across the line of sight is **0.03–0.42 m per
pixel (median 0.16 m/px)** at the sampled loss cells, i.e. individual dwarf-pine
cushions, *Sasa* sward and bare rock are separately resolvable. A camera model
refitted from the archived GCPs projects each 1 m analysis cell into the image
to within a few pixels, so every crop can be labelled with real UTM coordinates,
distance, a ground scale bar, and the paper's own 1 m class labels overlaid.
The crops are therefore usable evidence, not decoration. They are *not* usable
for the far-field top of the frame, and they cannot show what is *underneath* a
shrub canopy — only whether the canopy grew.

---

## 2. Does the image-space route hold? (the key insight, verified)

**Yes, with one correction.**

| check | result |
|---|---|
| `results/{2012,2021}_masked.npy` shape | `(3744, 5616)` float32 |
| `data/images/aligned/{2012,2021}/*.png` | 5616 × 3744, 8-bit RGB, all 14 files |
| same coordinate frame? | yes — every source frame was homography+lens-distortion warped onto the single reference `mrd_085_eos_vis_20151010_1205.png`, see `data/images/align_photographs.py` and `alignment_report_2012_2021.csv` (per-frame keypoint RMSE 0.86–2.06 px) |

**Correction — class 0 is ambiguous.** `scripts/vegetation_classification/apply_mask.py`
does `pred[mask == 0] = 0`, but 0 is also the *Sasa* code in the 0-based scheme
used by `run_rnn.py` / `calculate_diff.py`. The arrays contain values 0–6 only,
so masked pixels and *Sasa* are indistinguishable in `*_masked.npy` alone:

```
results/2012_masked.npy unique: 0:9,515,000  1:6,611,414  2:1,691,081
                                3:376,522  4:214,259  5:528,289  6:2,089,739
```

The mask has to be reconstructed to recover *Sasa*. Rebuilding it exactly as
`apply_mask.py` does (`mrd_085_eos_vis_20151010_1205_masked.png` AND the
non-black part of `aligned/2012/IMG_8748.png` — note the script's filename has a
typo, `_maskd.png`) gives 9,241,580 masked pixels, and

```
reconstructed-mask == 0  AND  class != 0 :      2,531 px   (0.03 % — clean)
reconstructed-mask == 1  AND  class == 0 :    275,951 px   ( = Sasa 2012 )
```

so the reconstruction is right and image-space *Sasa* 2012 = 275,951 px.
`data/images/mask.npy` is *not* the same array (9,246,554 zeros) and should not
be used for this.

**Alignment quality, measured (not assumed).** Tile-wise phase correlation
between four 2012/2021 frame pairs, 192 px tiles, fully-unmasked and textured
tiles only (n = 1,112):

```
median displacement 1.0 px | p90 2.0 px | 90.6 % of tiles ≤ 2 px
by image row:  0–1000 med 0.0  |  1000–1800 med 0.0  |  1800–2500 med 1.0
               2500–3744 med 1.4, p90 23.8   <-- near-field bottom of frame
within-year (2012 vs 2012, 2021 vs 2021): median 0.0–1.0 px
```

The only region with meaningful residual misregistration is the bottom ~1,200
rows (the closest foreground, where any camera displacement produces the largest
parallax). Per-crop residual shift is measured and written to
`sample_index.csv` (`align_dx_px`, `align_dy_px`, `align_shift_px`) so that
suspect crops can be discarded. For the 88 samples actually drawn:
**median 1.0 px, p90 1.4 px, max 6.3 px, 86/88 ≤ 2 px.**

---

## 3. The georectified route also works, and it is the one I used

Sampling purely in image space would *not* reproduce the paper's transition
matrix, because image pixels over-represent the near field. Area-weighted
image-space totals do match the rasters (see §4), but the image-space
destination split does not (§6). So samples are drawn from the georectified
rasters `ortho/data/vege_{2012,2021}_5x5.tiff` — **exactly the cells that make
up the published transition matrix** — and then projected into the photographs.

*Reproducibility note.* `ortho/georectify.R` reads `data/georectified.csv`
(the u,v → x,y,z lookup); that file is **not in the repository**, nor are
`data/2012_5x5.csv` / `2021_5x5.csv`. The manuscript (`matmet.qmd:40`) says the
georectification used the author's Python package `alproj`, and
`ortho/data/params_optim.json` + `gcp.csv` survive — but the parameter
convention in that JSON could not be reproduced (projecting the 482 GCPs with
the parameters exactly as written gives 1,543 px RMSE), so the camera was
**refitted from the 482 GCPs** inside `sample_loss_crops.py`:

```
pinhole only            : rmse 11.0 px, median  6.9 px
+ rational distortion   : rmse  6.3 px, median  2.3 px   (json claims "error": 4.34)
```

**Independent validation of the refit** — no free parameters were tuned to these:

* Projecting the whole 1 m grid and shading the photograph, the boundary of the
  mapped domain traces the summit ridge line exactly, and the projected class
  labels sit on the right terrain (see `overview_sasa_change.png`).
* Best agreement between the projected raster labels and the independent
  image-space labels is at shift (0, 0): no systematic bias.
  Agreement 79.9 % for 2012, 81.1 % for 2021 over 1.2 M cells.
* Area-weighting the image-space classification by ground area per pixel
  reproduces the raster class areas:

| class | image-space, area-weighted (m²) | `vege_2012_5x5.tiff` (m²) |
|---|---|---|
| 1 *Sasa* | 7,353 | 8,547 |
| 2 other vegetation | 433,786 | 486,402 |
| 3 no vegetation | 384,265 | 357,937 |
| 7 dwarf pine | 317,538 | 294,760 |
| **total mapped** | **1,202,783** | **1,206,233** |

and gross *Sasa* loss 2,703 m² vs 2,455–2,468 m² from the raster.

Residual per-cell localisation error is of order 10–20 px for *rare, small*
classes: 82 % of raster *Sasa* cells have image-space *Sasa* within a ±20 px
window (null expectation 12 %), but only 15 % at the exact projected pixel. That
is immaterial for crops 121–501 px wide, and the target cell is drawn explicitly
on the overlay panel so a reader can see where it landed.

**Ground resolution, measured** by projecting the 1 m grid at 0.25 m sub-sampling
and counting: **median 0.19 m² of ground per image pixel** over the mapped
domain (p5 0.06, p95 0.63, p99 1.06 m²/px).

---

## 4. What was produced

Everything is under
`/Users/okamoto/NIES/SasaSDMPaper/.claude/worktrees/review-summary-ece-ddc69c/review/audit/image_interpretation/`.

* **`sample_loss_crops.py`** — reusable, seeded (`--seed`, default 20260807),
  read-only with respect to the repository. Refits the camera, samples strata,
  cuts crops, writes the index and the contact sheets. Options: `--n-loss`,
  `--n-control`, `--no-exposure-match`.
* **`sample_index.csv`** — 88 rows, one per sample: id, stratum, 2012 and 2021
  class from the paper's raster, UTM x/y/z, distance from camera, image (u,v),
  raster (row,col), across-view m/px, m²/px, the independent image-space labels
  at the centre and their fraction in the detail window, patch greenness and
  texture in both years, measured 2012↔2021 residual shift, and file paths.
* **`crops/<id>.png`** — 88 five-row figures: context 501 px (both years,
  detail box marked) / detail 121 px green season / detail 121 px autumn /
  detail with the **paper's own 1 m labels** overlaid / the independent
  image-space labels. Scale bars, coordinates and the class transition are
  printed on every figure.
* **`crops_native/<id>_native.png`** — the same locations as a 2 × 2 mosaic of
  **completely unprocessed 301 × 301 px native crops** (rows = date pair,
  columns = year). No exposure matching, no resampling, no annotation. These are
  the files to zoom into when judging a specific location.
* **`contact_<stratum>.png`** — one montage per stratum, 2012 above 2021.
* **`overview_sasa_change.png`** — whole 2021 frame with the paper's *Sasa*
  change painted on (green = *Sasa* in both years, yellow = gain, red = loss)
  and the 88 sampled locations circled. All of the change sits in one mid-slope
  band along the Murodo trail; the dwarf-pine controls sit high on the ridges,
  which is why they are the lowest-resolution stratum.

### Sample design

Stratified on the paper's own transition matrix (`V12 == 1` etc.), minimum 6 m
ground separation between samples, rejecting cells whose projection falls within
255 px of the frame edge or touches the sky/foreground mask.

| stratum | n | pool (cells = m²) | median distance | median m/px |
|---|---:|---:|---:|---:|
| `loss_to_dwarfpine` (1→7) | 23 | 1,156 | 536 m | 0.140 |
| `loss_to_otherveg` (1→2) | 19 | 932 | 413 m | 0.108 |
| `loss_to_rowan` (1→4) | 2 | 159 | 607 m | 0.158 |
| `loss_to_maple` (1→5) | 2 | 85 | 588 m | 0.153 |
| `loss_to_alder` (1→6) | 2 | 133 | 690 m | 0.180 |
| `control_stable_sasa` (1→1) | 15 | 6,079 | 608 m | 0.158 |
| `control_stable_dwarfpine` (7→7) | 15 | 278,651 | 1,216 m | 0.317 |
| `gain_dwarfpine_to_sasa` (7→1) | 10 | 1,425 | 619 m | 0.161 |

23 : 19 is the 1,156 : 932 ratio of the two main destinations, i.e. in
proportion as requested. The 1→3 (no vegetation) transition is 3 m² in total and
was skipped. `gain_dwarfpine_to_sasa` is the mirror-image transition and is
included because it is the strongest internal test of classifier stability.

### Photograph pairs

The 2021 series is phenologically **1–2 weeks ahead** of the 2012 series (green
chromatic coordinate over all vegetated pixels), so pairing on calendar date
puts a greener 2012 frame beside a browner 2021 frame. The pairs are matched on
GCC instead:

```
green  : 2012-09-17 IMG_9038 (GCC 0.3780)  vs  2021-09-07 IMG_7900 (0.3791)
autumn : 2012-10-06 IMG_9304 (GCC 0.3422)  vs  2021-10-02 IMG_8172 (0.3424)
```

The 2021 camera was also stopped down (f/13 vs f/11) and its frames are ~10 %
darker. In the annotated figures the 2021 crop is multiplied by a single global
per-channel gain (green pair 1.25/1.20/1.14, autumn 1.08/1.06/1.03) and the
panel is labelled `[exp-matched]`. `crops_native/` is never adjusted.

---

## 5. Are the crops actually interpretable? — my judgement

**Yes, for the great majority of the loss area, and the effect being asked about
is visible.** Concretely:

* The controls separate cleanly by eye. `contact_control_stable_sasa.png` is
  smooth, pale yellow-green sward; `contact_control_stable_dwarfpine.png` is
  dark, blue-green, visibly lumpy cushion texture on steep rocky ground. Anyone
  can learn the two signatures from the contact sheets in a minute. This is what
  makes the loss crops readable — without controls they would not be.
* Real shrub growth *is* resolvable. `crops_native/loss_to_dwarfpine_11_native.png`
  shows a group of separate dark cushions in 2012 that have coalesced into one
  larger mass by 2021, with the pale sward between them gone, in both the green
  and the autumn pair. That is exactly the "shrub encroachment and growth"
  mechanism the paper asserts, seen directly.
* At the median 0.16 m/px an individual *P. pumila* cushion (typically 1–3 m
  across) is 6–20 px wide. That is enough to see it and to see it change size;
  it is not enough to see individual shoots.

**Where they are not interpretable, and you must not over-read them:**

1. **Far-field samples are weak.** `control_stable_dwarfpine` sits at a median
   1,216 m (0.32 m/px) and several tiles in that contact sheet are dim and
   low-contrast. Use `dist` / `m_per_px_horiz` in the CSV to triage. I would
   trust crops with `m_per_px_horiz < 0.25` and treat the rest as
   context only.
2. **The view is oblique, so the vertical scale is compressed and unknown.**
   The scale bars are labelled "horizontal" for that reason. On a slope facing
   the camera 1 px vertically may be tens of centimetres; near the horizon it is
   metres. Never measure an area off these crops.
3. **This cannot see under a canopy.** The crops can show that a shrub or pine
   canopy expanded over a place that used to be *Sasa*. They cannot show whether
   *Sasa* survived beneath it. The honest form of the argument is: "the loss
   pixels are where woody canopies closed over former *Sasa*, which is a
   mechanism consistent with persistence beneath, and is not consistent with
   *Sasa* mortality" — plus a citation for *Sasa* shade tolerance. Anything
   stronger requires field data we do not have.
4. **Illumination, phenology and sharpness differ between years.** Matching on
   GCC and applying a global gain reduces this but does not remove it; local
   shadowing differs because the two frames were taken at slightly different
   solar positions. The 2021 green frame is also measurably *less* sharp (see
   §6), which biases any texture comparison downwards.
5. **The overlaid labels come from a different model run than the crops' own
   classification** (§6). The overlay row is the paper's map; the bottom row is
   an independent run. They disagree, and that disagreement is itself a finding
   rather than a defect of the crops.

---

## 6. Three things found on the way that the revision has to deal with

**(a) `results/*_masked.npy` is NOT the classification the paper used.**
`run_rnn.py` writes `results/{year}.npy` from a 1 × 1-pixel model;
`apply_mask.py` turns those into `*_masked.npy`. But `calculate_diff.py` and
`image_to_csv.py` read `results/{year}_5x5.npy` / `results/use_this/{year}_5x5.npy`
— a 5 × 5-patch model — and it is those that became `ortho/data/vege_*_5x5.tiff`
and hence every number in the paper. **The 5 × 5 arrays and the intermediate
CSVs are not in the repository**; only the 1 × 1 arrays survive. The manuscript
(`matmet.qmd:39`) says the RNN "classified each pixel" and never mentions a
5 × 5 patch, although the figure filenames (`files/2012_5x5_en.jpg`) do.

The two runs disagree substantially on *Sasa*. Over the 88 samples, at the exact
projected pixel:

```
                          tiff label == 1x1 label
  loss_to_dwarfpine        2012  1/23     2021  4/23
  loss_to_otherveg         2012  0/19     2021 15/19
  control_stable_sasa      2012  1/15     2021  4/15
  control_stable_dwarfpine 2012 14/15     2021 14/15
```

Part of that is my ±10–20 px localisation error on a rare class — at the
*window* level the two runs do agree in the right direction (mean *Sasa*
fraction in the 121 px window: 0.14 for stable-*Sasa* controls, 0.00 for
stable-dwarf-pine controls; mean dwarf-pine fraction 0.13 vs 0.56). But part of
it is real: run to run, the *Sasa* class is unstable.

**(b) The two runs give different loss destination splits.** Weighting the
image-space (1 × 1) classification by ground area:

```
                      1x1 model, area-weighted     paper's 5x5 raster
  gross loss              2,703 m²                    2,455–2,468 m²
  -> other vegetation     1,548 m²  (57.3 %)          932 m²  (37.8 %)
  -> dwarf pine             848 m²  (31.4 %)        1,156 m²  (46.8 %)
```

The headline loss total is reproducible to ~10 %; **the "46.8 % went to dwarf
pine" claim is not** — an independent run of the same classifier family puts it
at 31 %. Any statement in the revision about *which* class the lost *Sasa* went
to needs to be hedged accordingly.

**(c) A first quantitative pass on the sampled crops contradicts the
"overgrown by dwarf pine" story.** Green chromatic coordinate and normalised
texture contrast in a 15 × 15 px window at each sample (exposure-matched frames):

```
stratum                     GCC12   GCC21    dGCC    tex12   tex21    dtex
loss_to_dwarfpine          0.3875  0.3843  -0.0032  0.1694  0.1481  -0.0213
loss_to_otherveg           0.3797  0.3813  +0.0016  0.1726  0.1742  +0.0015
control_stable_sasa        0.3841  0.3881  +0.0039  0.1335  0.1240  -0.0095
control_stable_dwarfpine   0.3619  0.3662  +0.0043  0.1596  0.1331  -0.0265
gain_dwarfpine_to_sasa     0.3855  0.3827  -0.0027  0.1804  0.1763  -0.0041
```

GCC does separate the endpoints (stable *Sasa* 0.384–0.388 vs stable dwarf pine
0.362–0.366). But cells the paper calls 1→7 have GCC **0.384 in 2021** — still
at the *Sasa* end, nowhere near the dwarf-pine end. Their texture *falls*
(−0.021), i.e. they get smoother, not lumpier — though texture falls in every
stratum including stable dwarf pine (−0.027), so the 2021 green frame is simply
softer and cross-year texture comparison is unreliable. In the same direction,
the independent 1 × 1 classification of the 121 px windows around the
`loss_to_dwarfpine` samples shows *Sasa* fraction going **up** (0.057 → 0.066)
and dwarf-pine fraction going **down** (0.231 → 0.207) across the same nine
years.

Caveats on (c): n = 23 vs 15, no blinding, a 15 px window mixes neighbours, and
GCC is not a species classifier. It is suggestive, not decisive — but it points
the same way as (a) and (b), namely that a substantial share of the 1→7
"transition" is classifier instability at *Sasa*/dwarf-pine boundaries rather
than nine years of pine growth. **That is a reason to look at the crops
carefully rather than to trust either number.**

---

## 7. How to review the output

1. Learn the two signatures from `contact_control_stable_sasa.png` and
   `contact_control_stable_dwarfpine.png`.
2. Skim `contact_loss_to_dwarfpine.png` and `contact_loss_to_otherveg.png`.
3. For anything ambiguous, open `crops/<id>.png` (context + overlay + labels)
   and then `crops_native/<id>_native.png` at 100 % zoom.
4. Score each sample into something like {canopy visibly closed / no visible
   change / ambiguous / crop unusable} and report the tally to the reviewer.
   Filter on `m_per_px_horiz` and `align_shift_px` first.
5. If the tally comes out mostly "no visible change", the honest revision is to
   report gross loss as classifier uncertainty rather than as shrub
   encroachment, and to keep the expansion area as the primary metric for that
   reason instead of the current unevidenced one.

## 8. Reproducing this

```
python3 -m venv --system-site-packages ~/scratch/venv   # system python has osgeo/GDAL
~/scratch/venv/bin/pip install numpy pillow matplotlib scipy
~/scratch/venv/bin/python sample_loss_crops.py --outdir .
```

Verified to run end-to-end from an empty cache. Runtime ~1 min for the full
88-sample run on 12 cores; peak RSS ~2 GB (four full-frame photographs, the
1 m grids and the 1 m DEM).
`_cache/` holds the fitted camera, the reconstructed mask, the per-pixel ground
area and the cell projection; delete it to recompute from scratch.
