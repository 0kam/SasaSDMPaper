# Audit: VEGETATION CLASSIFICATION subsystem

Auditor: subagent, investigation-only pass. No file under `/Users/okamoto/NIES/SasaSDMPaper`
was modified except inside this `review/audit` directory. No writing `git` commands were run.

Everything below marked **[RUN]** was executed; the command and output are shown.
Scratch scripts live in
`/private/tmp/claude-501/-Users-okamoto-NIES-SasaSDMPaper--claude-worktrees-review-summary-ece-ddc69c/98ac7ecf-eafb-49c6-aa2d-9aab2998c13e/scratchpad/`
(`repro_sses.py`, `align_check.py`, `dis_pyc.py`, venvs `venv/` py3.14 and `venv310/` py3.10+torch).

---

## 0. Executive summary

1. **The script that produced the published vegetation maps is not in the repository.**
   The repo contains `models/rnn.py` (an MLP+LSTM, 1x1 pixels) and `run_rnn.py` (100 epochs).
   The published rasters come from a **CRNN (Conv2d + LSTM) on 5x5 patches, 5-fold CV, 200
   epochs** — `results/cnn_lstm5x5_cv_5_ep_200/` in `utils/interpolate.R`. The CRNN source
   file was deleted but its byte-compiled form survives in
   `models/__pycache__/crnn.cpython-310.pyc`; I decompiled it (§5). There is no `run_crnn.py`.
   This is exactly Reviewer 3's "L164-170: this is a blackbox".
2. **`results/*_masked.npy` cannot distinguish masked pixels from *Sasa*.** Value `0` means
   both "outside the analysis mask" and "class 0 = Sasa". 97.1 % of the zeros are mask (§3).
   The **published GeoTIFFs are safe** — there `0` = mask and `1..7` = classes — but they were
   produced from `results/use_this/*_5x5.npy`, a file that no longer exists.
3. **A class is mis-named in the manuscript.** Training label class 5 is `ダケカンバ`
   (*Betula ermanii*, Erman's / "Golden" birch). `plot_vegetation_map.R`,
   `calculate_diff.py` and **matmet.qmd L39** all call it **Maple (*Acer tschonoskii*)**.
   The project's own accuracy figure `results/cv.png` correctly labels it "Golden Birch" (§2).
4. **`results/teacher.npy` is corrupted for class Sasa** — `draw_teacher` maps classIndex 1
   (Sasa) to code 0, colliding with 0 = unlabelled. Verified against the archive (§5).
   `results/teacher.png` is fine; the `.npy` is not.
5. **No held-out classification accuracy exists anywhere.** No `runs/`, no `*.pth`, no
   `stratified_cv.csv`, no confusion matrix. Only `results/cv.png`, a boxplot for **1x1**
   models on **2015** data — not the 5x5 CRNN used for 2012/2021. The manuscript reports no
   classification accuracy at all. I computed the only number obtainable without retraining:
   **resubstitution** OA 0.928 (2012) / 0.932 (2021), kappa 0.892 / 0.896 (§6).
6. **Sasa↔Dwarf pine is the classifier's worst confusion, and it is also the dominant
   transition behind the reported expansion.** Sasa producer's precision is the lowest of all
   seven classes (0.73 / 0.77) even on training data, and Dwarf pine → Sasa accounts for
   1,425 m² of the 4,097 m² gross gain and Sasa → Dwarf pine for 1,156 m² of the 2,468 m²
   gross loss (§7). 9.0 % of the whole map changes class over the 9 years; the *Sasa* gross
   gain is 0.34 % of the map. The change signal is small relative to the classification churn.
7. **Good news:** the manuscript's areas (8,542 / 10,170 / 4,095 / 2,467 m²) reconcile
   *exactly* with the archived GeoTIFFs once `terra::expanse()`'s UTM area scale factor
   (0.999455) is applied (§7). The earlier lead that they "are all slightly different"
   is retracted.
8. Alignment between 2012 and 2021 is genuinely good: median residual 1.2–1.9 px, p90
   3–6 px, max ~12–18 px on 5616x3744 frames (§8). But
   `alignment_report_2012_2021.csv` documents a **different image set** than the one in the
   repo and was written by a version of `align_photographs.py` that no longer exists.

---

## 1. The pipeline, source photographs → `results/*.npy` → GeoTIFF

### 1.1 What is actually on disk

**[RUN]**
```
$ ls data/images/source/2012 data/images/source/2021 data/images/aligned/2012 data/images/aligned/2021
source/2012: IMG_8748 8819 8956 9038 9164 9304 9514 .JPG        (7 files)
source/2021: IMG_7763 7827 7900 8023 8081 8172 8298 .JPG        (7 files)
aligned/2012, aligned/2021: same stems, .png                     (7 files each)
```
All are 5616 x 3744, Canon EOS 5D Mark II.

**[RUN]** EXIF acquisition dates (`PIL.ExifTags`):

| 2012 | DOY | 2021 | DOY |
|---|---|---|---|
| 2012-08-27 12:00 | 240 | 2021-08-24 11:00 | 236 |
| 2012-09-01 13:00 | 245 | 2021-08-31 12:00 | 243 |
| 2012-09-11 14:00 | 255 | 2021-09-07 12:00 | 250 |
| 2012-09-17 12:00 | 261 | 2021-09-19 12:46 | 262 |
| 2012-09-26 12:00 | 270 | 2021-09-24 10:57 | 267 |
| 2012-10-06 12:00 | 280 | 2021-10-02 12:00 | 275 |
| 2012-10-21 12:00 | 295 | 2021-10-14 12:00 | 287 |

Mean DOY 263.7 (2012) vs 260.0 (2021): the 2021 stack is on average **3.7 days earlier**, and
the final, phenologically most informative frame is **8 days earlier**. Acquisition times span
12:00–14:00 in 2012 but 10:57–12:46 in 2021 (different sun elevation). The manuscript
(matmet.qmd L30) says "images taken in **September–October**"; in fact 1 (2012) and 2 (2021)
frames are from August. No radiometric normalisation is applied on the code path that was used
(`prepare_data.py` has a `normalized/` variant, but no script in the repo produces
`data_source/normalized/`).

### 1.2 The pipeline as reconstructed

```
source/{2012,2021}/*.JPG
  └─ data/images/align_photographs.py            AKAZE + lens-distortion + homography
     → aligned/{2012,2021}/*.png                 (all warped into the 2015 reference frame)

data/labels/*.json  (Semantic Segmentation Editor)
  └─ utils.read_sses()  burns ALL polygons from ALL 6 json files into ONE
     5616x3744 label raster (classIndex, 0 = unlabelled, SSE index 0 remapped to max+1 = 7)
  └─ utils.set_patches()  → ../data/{year}_5x5/<classIndex>/<year>.npy    [MISSING]

  └─ MISSING run_crnn.py  (models/crnn.py, deleted; recovered from __pycache__)
     → runs/cv/{year}/fold_*/best.pth + pred.npy   [MISSING]
     → mode over folds → results/{year}_5x5.npy    [MISSING]

apply_mask.py  → results/{year}_masked.npy        [PRESENT, but see §3 — value 0 ambiguous]
                 results/{year}_masked.png        [PRESENT, unambiguous: mask is black]

results/use_this/{2012,2021}_5x5.npy              [MISSING — the file actually used]
  └─ scripts/sdm/image_to_csv.py (the relevant block is COMMENTED OUT, lines 15-23)
     → ortho/data/{2012,2021}_5x5.csv             [MISSING]
  └─ ortho/georectify.R  join with data/georectified.csv  [MISSING], st_rasterize, focal modal
     → ortho/data/vege_{2012,2021}_5x5.tiff       [PRESENT — the published maps]
```

Every intermediate between the aligned photographs and the final GeoTIFFs is absent, and the
two scripts that would regenerate them (`run_rnn.py`, `calculate_diff.py`) point at file names
(`results/2012_5x5.npy`, `results/2012.npy`) that do not exist.

### 1.3 Array shapes / dtypes / encodings

**[RUN]** `numpy.load`:

| file | shape | dtype | meaning of values |
|---|---|---|---|
| `results/2012_masked.npy` | (3744, 5616) | float32 | 0..6, **0 = Sasa OR masked** |
| `results/2021_masked.npy` | (3744, 5616) | float32 | idem |
| `results/teacher.npy` | (3744, 5616) | float64 | 0..6, **0 = Sasa OR unlabelled** |
| `data/images/mask.npy` | (3744, 5616) | float64 | 0/1, 1 = inside analysis mask |
| `ortho/data/vege_*_5x5.tiff` | (1753, 1801) | Float32, nodata=nan | **0 = mask, 1..7 = classes** |

**[RUN]** value histograms:
```
results/2012_masked.npy   0: 9515000 (45.25%)  1: 6611414  2: 1691081  3: 376522
                          4: 214259   5: 528289  6: 2089739
results/2021_masked.npy   0: 9517941 (45.27%)  1: 6547527  2: 1799079  3: 463817
                          4: 131340   5: 631871  6: 1934729
results/teacher.npy       0: 20446688 (97.24%) 1: 339439  2: 40632  3: 29282
                          4: 12758   5: 48130  6: 109375
data/images/mask.npy      0: 9246554 (43.98%)  1: 11779750
vege_2012_5x5.tiff        0:5179 1:8547 2:486402 3:357937 4:26934 5:5700 6:20774 7:294760
                          nan: 1950920
vege_2021_5x5.tiff        0:5191 1:10176 2:451770 3:380686 4:25189 5:4302 6:24943 7:303976
```

---

## 2. DEFINITIVE class code → label mapping

The chain, verified end-to-end:

1. **SSE JSON** (`data/labels/*.json`) stores `classIndex` and a Japanese `label` per polygon.
   **[RUN]** the seven distinct pairs are: `0 ハイマツ`, `1 ササ`, `2 その他植生`, `3 無植生`,
   `4 ナナカマド`, `5 ダケカンバ`, `6 ミヤマハンノキ`.
2. `utils.read_sses` lines 57-60: `max_class_idx = 6`; `classIndex == 0 → 7`.
   So `ハイマツ` becomes **7**.
3. `set_patches` writes one directory per classIndex: `1 … 7`.
4. `torchvision.DatasetFolder` sorts the directory names alphabetically
   (`'1'…'7'`, identical to numeric order) and assigns targets `0…6`.
   `NNClasifier` line 47-49 independently builds `class_to_idx = {'1':0, …, '7':6}`.
   **These agree**, so model code = classIndex − 1.
5. GeoTIFF value = model code + 1 = classIndex (see §3 for the proof).

### The definitive table

| model code (`results/*.npy`) | GeoTIFF value | SSE classIndex | JSON label | manuscript name | correct? |
|---|---|---|---|---|---|
| 0 | 1 | 1 | ササ | Dwarf Bamboo (*Sasa* spp.) | ok |
| 1 | 2 | 2 | その他植生 | Other Vegetation | ok |
| 2 | 3 | 3 | 無植生 | No Vegetation | ok |
| 3 | 4 | 4 | ナナカマド | Rowans (*Sorbus* spp.) | ok |
| 4 | 5 | 5 | **ダケカンバ** (*Betula ermanii*) | **Maple (*Acer tschonoskii*)** | **WRONG** |
| 5 | 6 | 6 | ミヤマハンノキ | Montane Alder (*Alnus viridis* ssp. *maximowiczii*) | ok |
| 6 | 7 | 7 | ハイマツ | Dwarf Pine (*Pinus pumila*) | ok |

The colour map in `run_rnn.py` lines 10-20 matches this ordering; **[RUN]** I confirmed the
archived `results/2012_masked.png` decodes exactly to that `ListedColormap`:
```
npy 0 -> RGB(154,205,50)   npy 1 -> (70,130,180)  npy 2 -> (192,192,192)
npy 3 -> (220,20,60)       npy 4 -> (255,215,0)   npy 5 -> (139,69,19)   npy 6 -> (0,100,0)
```

### Every place a class code is compared to a literal

| location | literal | verdict |
|---|---|---|
| `calculate_diff.py:5-13` | `sasa:0 … haimatsu:6` | 0-based, correct **except** name `"kaede":4` (should be *dakekanba* / Erman's birch). The two diffs computed use codes 0 and 6 only, so the numbers are unaffected. |
| `calculate_diff.py:30-31,38-39` | uses that dict | correct codes |
| `apply_mask.py:6,10,14` | `mask == 0`, `pred[...] = 0` | **BUG** — writes class code 0 (= Sasa) as the mask value. §3 |
| `utils/utils.py:59` (= `utils_old.py:59`) | `class_idx == 0 → max+1` | correct |
| `utils/utils.py:126` | `if label != 0` (skip unlabelled) | correct (operates on classIndex, 1-based) |
| `utils/utils.py:197-204` (`draw_teacher`) | `img[mask == label] = l`, `img[mask == 0] = 0` | **BUG** — remaps classIndex 1 (Sasa) to 0, colliding with unlabelled. §5 |
| `models/nnmodel.py:47-49` | `keys = [str(i+1) …]` | correct **only if all 7 class dirs exist**; if a class had no labelled pixels in a year, `y_dim` shrinks and the whole mapping silently shifts. Latent hazard. |
| `scripts/sdm/plot_vegetation_map.R:33-39, 73-79` | `vegetation == 1..7` | codes correct; **label for 5 is wrong** (ミネカエデ instead of ダケカンバ) |
| `scripts/sdm/sdm_tbm.R:33,37` `sdm_tdm.R:33,40,248,253` | `layer == 1` for Sasa | correct (1-based) |
| `scripts/sdm/sdm_tbm.R:343`, `sdm_tdm.R:373` | `vege21 == 2` for "Other Vegetation" | correct |
| `scripts/sdm/sdm_tdm.R:279` | `is_others = ifelse(layer == 6, 1, 0)` with comment 「その他植生か」 | **WRONG** — 6 is Montane Alder, "Other Vegetation" is 2. **[RUN]** `grep -n is_others scripts/sdm/*.R` returns only lines 278-280 → **dead code, never used downstream.** No published number affected. |

`utils/utils.py` and `utils/utils_old.py` are **byte-identical** (`diff` returns nothing).

**Evidence that "Maple" is wrong**: the project's own accuracy figure `results/cv.png` has one
panel per class, in English, and calls this class **"Golden Birch"** (a common English rendering
of ダケカンバ). No polygon anywhere in `data/labels/` carries the label ミネカエデ or any maple.

---

## 3. What `0` means, and whether the mask is separable

### In `results/*_masked.npy`: **NOT separable.**

`apply_mask.py:10` writes `pred[mask == 0] = 0`, and 0 is the code for *Sasa*.

**[RUN]** cross-tabulated against `data/images/mask.npy`:
```
2012  pred==0 total 9,515,000
      pred==0 & mask==0 : 9,239,464   (97.10% of the zeros — masked sky/foreground)
      pred==0 & mask!=0 :   275,536   ( 2.90% — genuine Sasa)
2021  pred==0 total 9,517,941
      pred==0 & mask==0 : 9,239,562
      pred==0 & mask!=0 :   278,379
```
Anyone computing a transition matrix from `results/*_masked.npy` alone would report ~9.5 M
"Sasa" pixels in each year, 34x the real number, and a spuriously perfect Sasa→Sasa stability.
The companion PNGs are safe (mask is written as pure black `(0,0,0)`, Sasa as `(154,205,50)`).

The mask is also not exactly reproducible. **[RUN]**
```
apply_mask.py reproduction (mrd..._masked.png[:,:,0], then aligned/2012/IMG_8748.png==0):
                                 9,241,581 zero px
data/images/mask.npy:            9,246,554 zero px      disagreement 4,975 px
pixels where results/2012_masked.npy != 0 but the reproduced mask == 0:  2,532
```
So at least three slightly different masks are in circulation, and none of them exactly
reproduces the archived predictions. Additionally `apply_mask.py:4` reads
`mrd_085_eos_vis_20151010_1205_maskd.png` — **the file on disk is `..._masked.png`**
(typo → the script as written cannot run). And `[:,:,0]` takes the **blue channel**;
**[RUN]** for `aligned/2012/IMG_8748.png` there are 399 pixels with B == 0 that are not black,
i.e. 399 real pixels silently masked.

The mask itself has no documented provenance: `mask_sky.py` writes `sky_mask_2015.png`, which
is **not** the mask used. **[RUN]** `sky_mask_2015.png` blacks out 4,816,892 px vs 9,241,580 px
in `mrd_085_eos_vis_20151010_1205_masked.png`; the two agree on only **79.0 %** of pixels.
The mask that was used was made by some untracked (probably manual) process.

### In `vege_*_5x5.tiff`: **separable — 0 is the mask, 1..7 are classes.**

Proof (three independent lines):

1. **[RUN]** `sasa_inc.tiff` is bit-identical to `(vege_2012 != 1) & (vege_2021 == 1)`:
   agreement `1.0`, count 4,097. Against the alternative `(!=0)&(==0)` agreement is only
   0.99869. So Sasa is **1**, not 0.
2. **[RUN]** spatial test: class 0 occupies 5,179 cells in 41 components lying on the straight
   outer edges of the georectified footprint (bbox spans the full raster, rows 0-1752) — i.e.
   the black border of the warped photograph. Class 1 (Sasa) occupies 8,547 cells in 903
   compact components confined to rows 224-1333, cols 15-1199 — nowhere near the frame edge.
   If 0 and Sasa shared a code, the frame edge would be classified Sasa.
3. `scripts/sdm/plot_vegetation_map.R` maps only 1..7 to names; 0 falls through `case_when`
   to NA.

**Consequence for the transition matrix we publish:** compute it from
`ortho/data/vege_{2012,2021}_5x5.tiff`, excluding `NaN` **and** excluding value `0`.
Do **not** compute it from `results/*_masked.npy`.

---

## 4. Could `run_rnn.py` run? Which year stacks exist? Is there 2015 data?

**No, it cannot run — it fails at import.**

**[RUN]** (py3.10 venv with torch 2.13, torchvision, torch_optimizer, sklearn, tensorboardX,
cv2, shapely, pandas, tqdm, matplotlib all installed):
```
$ cd scripts/vegetation_classification && python run_rnn.py
Traceback (most recent call last):
  File "run_rnn.py", line 1, in <module>
    from models.rnn import RNNClassifier
  ...
  File "utils/utils.py", line 22, in <module>
    plt.rcParams["font.family"] = font_prop.get_name()
FileNotFoundError: [Errno 2] No such file or directory:
  '/usr/share/fonts/truetype/migmix/migmix-1p-regular.ttf'
```
`utils/utils.py:20-22` hard-codes a Debian font path at **module import time**, so *every*
consumer of `utils.utils` (i.e. the whole package) is unusable off the author's Linux box.

Even with the font stubbed, every path is wrong. **[RUN]** from `scripts/vegetation_classification/`:
```
../data/2012                 MISSING       ../data_source/aligned/2012   MISSING
../data/2015                 MISSING       ../data_source/aligned/2015   MISSING
../data/2021                 MISSING       ../data_source/aligned/2021   MISSING
../data_source/labels        MISSING       ../results                    MISSING
runs                         MISSING
```
The repo layout is `data/labels`, `data/images/aligned/{2012,2021}`, `results/`.
There is no `data_source/` anywhere.

Additional internal inconsistencies:

* `prepare_data.py` writes `../data/2012_5x5/`, `../data/2021_5x5/`, `../data/*_normalized/`,
  `../data/*_composite_5x5/`. `run_rnn.py` reads `../data/{year}` — **a directory
  `prepare_data.py` never creates.**
* `prepare_data.py:4` (the 2015 aligned line) is **commented out**, so `../data/2015` could
  never exist even if the paths were right, yet `run_rnn.py:22` iterates over
  `["2012","2015","2021"]` and would crash on the second iteration.
* There is **no 2015 photograph stack**. **[RUN]** `find . -iname "*2015*"` returns only the
  single reference image `data/images/mrd_085_eos_vis_20151010_1205.png` (+ its masked
  version + `sky_mask_2015.png`), four 2015 label JSONs, and unrelated snow files.
  There is no `aligned/2015/` — but `align_photographs.py:184` targets
  `sorted(glob("data_source/aligned/2015/*"))`, so **`align_photographs.py` cannot be re-run
  either**.
* `apply_mask.py` and `calculate_diff.py` use repo-root-relative paths, `prepare_data.py` and
  `run_rnn.py` use `scripts/vegetation_classification`-relative paths. Three different working
  directories are assumed across five scripts (plus `~/VegetationMapPaper/` in
  `utils/interpolate.R:6` and `~/Projects/jasms2023f/ortho/` in `ortho/georectify.R:1`).
* No `requirements.txt` / `environment.yml` / `renv.lock` exists anywhere in the repo.

**Year stacks that exist:** 2012 (7 frames) and 2021 (7 frames) only, in
`data/images/aligned/`. Both are already in the 2015 reference frame.

---

## 5. Accuracy evidence, and `results/teacher.npy`

### What exists

**[RUN]** `find . -iname "*stratified*" -o -iname "*confusion*" -o -iname "runs" -o -iname "*.pth" -o -iname "*all_scalars*" -o -iname "cv*.csv"` → **nothing**.

`NNClasifier.kfold` (nnmodel.py:121-139) *does* implement 5-fold CV and writes
`runs/<log_dir>/stratified_cv.csv` with per-class precision/recall/F1 per fold. **None of that
output survives.** Neither do the checkpoints (`best.pth`), so the fitted classifier cannot be
re-applied without retraining.

The one surviving artefact is **`results/cv.png`** (5400x2700). I rendered it: it is a
`ggplot` facet of **F1-score boxplots**, one panel per class
(*Dwarf Pine, Dwarf Bamboo, Rowans, Golden Birch, Montane Alder, Other vegetation,
Non Vegetation*), with x categories `2015-08-25, 2015-09-05, 2015-09-12, 2015-09-20,
2015-09-26, 2015-10-03, 2015-10-10, Multidays RNN 1x1, Multidays1x1`.

Three things follow:
* It is about **2015** imagery and **1x1** models — **not** the 5x5 CRNN used for the
  2012/2021 maps in this manuscript. It almost certainly belongs to the prior paper
  (Okamoto 2024 RSEC).
* Its underlying `stratified_cv.csv` is absent, so the numbers cannot be quoted.
* It labels class 5 **"Golden Birch"**, confirming §2.

The manuscript reports **no** classification accuracy — **[RUN]**
`grep -i "accuracy\|F1\|confusion\|kappa" paper/matmet.qmd paper/results.qmd paper/supplement.qmd`
returns only SDM/TSS material.

### `results/teacher.npy`

`utils.draw_teacher` (utils.py:194-206):
```python
img, labels = read_sses(label_dir, image_size, label=label)   # 0 = unlabelled, 1..7 = classes
mask = img.copy()
for label in labels["classIndex"]:
    l = class_to_idx[str(label)]          # l = label - 1
    img[mask == label] = l                # classIndex 1 (Sasa) -> 0 == unlabelled
array = img[:,:,0]
plt.imsave(out_path, array, cmap=cmap)
np.save(out_path.replace(".png",".npy"), array)   # <-- saved BEFORE masking
img = cv2.imread(out_path); img[mask == 0] = 0; cv2.imwrite(out_path, img)  # PNG only
```

**[RUN]** I re-implemented `read_sses` faithfully (`repro_sses.py`) and rasterised the six JSON
files at 5616x3744:

```
polygons burned: 216   skipped: 0   max_class_idx: 6
classIndex  label        pixels     %      model code
   0        UNLABELLED  20,408,733  97.06    n/a
   1        ササ            38,370   0.18     0
   2        その他植生      339,296   1.61     1
   3        無植生           40,593   0.19     2
   4        ナナカマド       29,238   0.14     3
   5        ダケカンバ       12,738   0.06     4
   6        ミヤマハンノキ    48,086   0.23     5
   7        ハイマツ        109,250   0.52     6
```
Cross-check against the archived `results/teacher.npy`: **for every labelled pixel,
`teacher.npy == classIndex - 1`, match rate 617,571 / 617,571 = 1.000.**
And `teacher.npy == 0` decomposes as 20,408,318 unlabelled + **38,370 Sasa** — i.e.
**the Sasa training pixels are irrecoverable from `teacher.npy`**.

`results/teacher.png` *is* usable: **[RUN]** its colour histogram gives
`(154,205,50)` Sasa = 38,396 px, black (unlabelled) = 20,408,292 px, and the six other class
colours match exactly. So the ground truth can be recovered from the PNG or, better,
regenerated from the JSONs.

### What accuracy evidence can be produced *without retraining*

Only **resubstitution** (training-set) statistics: the archived maps evaluated against the
labelled polygons they were trained on. **[RUN]**, restricted to labelled pixels inside the
analysis mask (n = 617,568):

**2012** — overall accuracy **0.9282**, kappa **0.8918**
```
rows = label, cols = archived map (2012_masked.npy)
                 Sasa  OtherVeg    NoVeg    Rowan  ErmanBir    Alder DwarfPine   recall
      Sasa      34935      1722        0       19        28        9      1657    0.910
  OtherVeg       7199    315716     3273      855      1420     1385      9448    0.931
     NoVeg          4       304    38527       12        95       51      1598    0.949
     Rowan         26       658        3    26582       485      200      1284    0.909
ErmanBirch        156       457        0       45     11819      172        88    0.928
     Alder          2       464        0       71        53    47351       145    0.985
 DwarfPine       5592      1834     2632      272       162      478     98280    0.900
 precision      0.729     0.983    0.867    0.954     0.840    0.954     0.874
```

**2021** — overall accuracy **0.9315**, kappa **0.8960**
```
                 Sasa  OtherVeg    NoVeg    Rowan  ErmanBir    Alder DwarfPine   recall
      Sasa      34375      1857        0        0        73       51      2014    0.896
  OtherVeg       1940    322616     6355     1334       248     3221      3582    0.951
     NoVeg         10       200    39149      116         3        5      1108    0.964
     Rowan          0      1584       24    26119        38      603       870    0.893
ErmanBirch        144       659        2      300     11419      190        23    0.897
     Alder         97      1639        0      430         9    45480       431    0.946
 DwarfPine       8047      1457      830      263       241     2277     96135    0.880
 precision      0.771     0.978    0.844    0.914     0.949     0.878     0.923
```

These must be labelled **resubstitution / training-set** accuracy and are an **upper bound**.
The most important structure in them: **Sasa has the lowest precision of all seven classes in
both years, and the dominant contributor is Dwarf pine misclassified as Sasa** (5,592 px in
2012, 8,047 px in 2021, i.e. 5.1 % and 7.4 % of all labelled dwarf-pine pixels).

---

## 6. Training polygons: mapping onto images, pixel counts, balance

### How they map

`read_sses` concatenates **all** JSON files in `data/labels/` and burns every polygon into a
**single** 5616x3744 raster. This is only legitimate because all frames were warped into a
common (2015) reference frame; the polygons themselves were drawn on six *different*
photographs from *three* different years. `set_patches` then applies this one raster to the
2012 stack and, separately, to the 2021 stack — i.e. **the same pixel locations are used as
training data for both years.**

**[RUN]** provenance of the labelled pixels:
```
mrd_085_eos_vis_20151010_1205.png   415,431 px (67.2%)   [2015 image — ABSENT from repo]
IMG_8298.png                         92,661 px (15.0%)   [2021]
mrd_085_eos_vis_20150926_1205.png    62,318 px (10.1%)   [2015 — ABSENT]
mrd_085_eos_vis_20150920_1205.png    32,947 px ( 5.3%)   [2015 — ABSENT]
IMG_9304.png                          7,828 px ( 1.3%)   [2012]
mrd_085_eos_vis_20150912_1205.png     7,178 px ( 1.2%)   [2015 — ABSENT]
```
**Only 1.3 % of the training pixels were labelled on a 2012 photograph, and not one *Sasa*
polygon was drawn on a 2012 image** (the 25 Sasa polygons are 17 from the 2021 frame and
8 from the 2015 reference). The 2012 classifier is therefore trained on *Sasa* pixels whose
identity was established from 2015/2021 imagery — precisely in the pixels where change is
being measured. Four of the six label files refer to 2015 photographs that are **not in the
repository**, so a reviewer cannot check the labels against their source images.

### Pixel and polygon counts (balance)

**[RUN]**
```
code  classIndex  label          polygons   pixels   % of labelled   median px/polygon
 0        1       ササ                25     38,396      6.2%              430
 1        2       その他植生            76    339,786     55.0%            2,332
 2        3       無植生               29     40,633      6.6%              818
 3        4       ナナカマド            16     29,282      4.7%            1,065
 4        5       ダケカンバ            16     12,758      2.1%              657
 5        6       ミヤマハンノキ         14     48,130      7.8%            2,901
 6        7       ハイマツ              40    109,378     17.7%            1,184
                             TOTAL   216    618,012   (2.94% of the image)
pixels covered by >1 polygon: 351 (max overlap depth 2)
```

* **Strongly imbalanced**: 26.6 : 1 between the largest ("Other Vegetation", 55 %) and
  smallest ("Erman's birch", 2.1 %) class. The loss is a plain `nn.CrossEntropyLoss()`
  (`rnn.py:51`) with **no class weighting**; `StratifiedKFold` preserves the imbalance rather
  than correcting it.
* **The effective sample size is 216 polygons, not 618,012 pixels.** Median polygon is
  430–2,900 px. `NNClasifier.kfold` (nnmodel.py:121-124) splits with
  `StratifiedKFold` over **pixel indices**, so essentially every polygon is split across all
  five folds and each validation pixel has training pixels as immediate neighbours. The
  reported F1 scores are therefore inflated by spatial autocorrelation. This is *inconsistent
  with the paper's own HSM methodology*, which explicitly uses spatial block splitting
  (`spatialsample`, matmet.qmd L99/L111) for exactly this reason — a discrepancy a reviewer
  can and probably will notice.
* Only 25 *Sasa* polygons underpin the entire expansion result.
* Polygon geometry is clean: **[RUN]** 0 of 216 polygons are self-intersecting, none has
  < 4 vertices. The `MultiPolygon` branch at `utils.py:78-82` is dead code
  (`shapely.geometry.Polygon` never returns a MultiPolygon) and would crash on shapely ≥ 2
  if it were ever reached (`for poly in polygon` — needs `.geoms`).
* `read_sses` uses `glob(label_dir + "/*")` **unsorted** → the burn order, and hence the
  351 overlapping pixels, is filesystem-dependent. Minor (0.06 % of labels) but a real
  non-determinism. Note it also globs `/*`, so a stray `.Rhistory` (which **is** present in
  `data/labels/`) or `.DS_Store` would be fed to `json.load` and crash the function.

---

## 7. The published change numbers, and how much of them is classifier noise

### The manuscript's numbers reconcile exactly

**[RUN]** with `terra`:
```
      what cells expanse_m2      ratio
1 sasa2012  8547  8542.3407 0.99945486     manuscript: 8,542 m²
2 sasa2021 10176 10170.4538 0.99945497     manuscript: 10,170 m²
3     gain  4097  4094.7684 0.99945531     manuscript: 4,095 m²
4     loss  2468  2466.6553 0.99945513     manuscript: 2,467 m²
5 sasa_inc  4097  4094.7684 0.99945531
CRS: JGD2011 / UTM zone 53N (EPSG:6690)
```
The constant ratio 0.999455 is the UTM area scale factor; `terra::expanse()` returns geodesic
area, `sum(cells)` returns map-plane area. **The manuscript's four numbers are exactly
`expanse()` of the archived rasters.** No discrepancy to reconcile.

`sasa_inc.tiff` is **[RUN]** bit-identical to `(vege_2012 != 1) & (vege_2021 == 1)`, but **no
tracked script creates it** — it is only ever read (`sdm_tbm.R:446`, `analyse_sdm.R:28`).
It can be regenerated exactly.

### Full transition matrix (Reviewer 3 asked for this)

**[RUN]**, `ortho/data/vege_{2012,2021}_5x5.tiff`, NaN excluded, **0 (mask) shown but must be
dropped**; 1 m cells:

```
rows = 2012, cols = 2021
              0(mask)   1 Sasa  2 OtherV  3 NoVeg  4 Rowan  5 Birch  6 Alder  7 DwPine   total
0 (mask)         5167        0        5        3        0        0        0        4     5179
1 Sasa              0     6079      932        3      159       85      133     1156     8547
2 OtherVeg          8     2171   432237    32001     3033      380     7187     9385   486402
3 NoVeg             8       13     5096   342186       49        0       16    10569   357937
4 Rowan             1      224     3738       30    17284      871     2682     2104    26934
5 Birch             0      155      624        0     1271     2523      703      424     5700
6 Alder             0      109     3647       45     2023      234    13033     1683    20774
7 DwarfPine         7     1425     5491     6418     1370      209     1189   278651   294760
total            5191    10176   451770   380686    25189     4302    24943   303976  1206233
```

Loss of *Sasa* (2,468 cells) went to: **Dwarf pine 1,156 (46.8 %)**, Other vegetation 932
(37.8 %), Rowan 159 (6.4 %), Alder 133 (5.4 %), Erman's birch 85 (3.4 %), No vegetation 3.
Gain of *Sasa* (4,097 cells) came from: Other vegetation 2,171 (53.0 %),
**Dwarf pine 1,425 (34.8 %)**, Rowan 224, Erman's birch 155, Alder 109, No vegetation 13.

### Why this matters

* The manuscript (matmet.qmd L44) attributes apparent *Sasa* loss to **shrub canopy
  occlusion**. But the single biggest recipient of lost *Sasa* is **Dwarf pine (*Pinus
  pumila*)**, a prostrate species that does not overtop *Sasa*; and Dwarf pine ↔ Sasa is
  simultaneously the **largest confusion in the classifier's own (optimistic) resubstitution
  matrix** (§5). The most parsimonious reading of ~1,156 m² of "loss" and ~1,425 m² of "gain"
  is classification error, not vegetation change.
* **[RUN]** overall landscape stability: `1,097,160 / 1,206,233 = 90.96 %` of cells keep their
  class; **9.04 % (109,073 cells) change class over the nine years**. The reported *Sasa*
  gross gain, 4,097 cells, is **3.8 % of that total churn** and 0.34 % of the map. There is at
  present no evidence separating the *Sasa* signal from the general classification/registration
  noise floor.
* **[RUN]** spatial structure of the gain (distance transform from the 2012 *Sasa* mask):
  ```
  gain cells 4,097 — distance to nearest 2012 Sasa cell
     p0 1.0 m   p25 1.0   p50 1.4   p75 3.6   p90 11.4   p95 18.6   p99 80.8   max 242.9 m
     within 1.5 m: 57.8%    within 3 m: 72.7%    within 5 m: 81.1%
  loss cells 2,468 — 62.5% within 1.5 m of the 2021 Sasa mask
  ```
  Consistent with rhizome expansion at the margins, which supports the paper's story — **but**
  **[RUN]** connected-component analysis contradicts a specific discussion claim:
  ```
  2021 Sasa patches (8-connected): 1,082;  2012 Sasa patches: 903
  patches containing NO 2012 Sasa cell: 748 (1,420 cells, 14.0% of 2021 Sasa)
  of those, patches >= 5 cells AND > 10 m from any 2012 Sasa: 19 patches, 139 cells
  ```
  discussion.qmd L9 states "no isolated *Sasa* patches formed by seed dispersal were observed
  in the expansion areas between 2012 and 2021". The archived rasters contain 19 isolated
  patches of >= 5 m² lying > 10 m from any 2012 *Sasa*. They are either real isolated patches
  (undermining the dispersal-limitation argument) or classification noise (undermining the
  expansion estimate). Either way the sentence as written is not supported by the data.

---

## 8. Geometric consistency of the aligned photographs

### The archived report does not describe the archived images

`data/images/alignment_report_2012_2021.csv` has 24 rows; **[RUN]** every `source` is
`data_source/source/{2012,2021}/mrd_085_eos_vis_YYYYMMDD_HHMM.jpg` (12 per year) and every
`destination` is `data_source/mrd_085_eos_vis_20151010_1205.png`.

The images actually in the repo are `IMG_*.JPG`, **7 per year**. Their stems do not appear in
the report at all. Moreover `align_photographs.py:182-202` as written pairs
`sorted(glob("data_source/source/{year}/*.JPG"))` with
`sorted(glob("data_source/aligned/2015/*"))` element-by-element — a *different* destination per
source. The single fixed destination in the CSV corresponds to the **commented-out** line 181
(`#target = "data_source/mrd_085_eos_vis_20151010_1205.png"`).

**Conclusion: the archived report was produced by a version of `align_photographs.py` that no
longer exists, on a set of photographs that is not in the repository. It provides no evidence
about the geometry of `aligned/{2012,2021}/`.**

Within the report itself, RMSE ranges 0.86–2.06 px with 19–9,785 inlier matches. Three rows are
weakly constrained: `20210924_1105` (**19 matches**, RMSE 2.06), `20210921_1405` (155),
`20120922_0600` (111). `homography_lensdist` fits 8 distortion parameters *plus* an 8-DOF
homography; with 19 point correspondences that model is essentially unidentified.

### Direct measurement on the images that *are* archived

**[RUN]** `align_check.py` — AKAZE + Lowe ratio 0.75 + RANSAC(5 px) at half resolution,
residual displacements rescaled to full resolution (5616x3744). "Residual" = distance between
matched keypoints *without* applying any further transform, i.e. the leftover misregistration.

```
pair                                     ngood   ninl  med_px  p90_px  max_px
2012:IMG_8748 vs 2015 ref                 1383   1279    1.15    3.92   11.74
2012:IMG_8819 vs 2015 ref                 1188    956    0.93    3.02   10.69
2012:IMG_8956 vs 2015 ref                 1402   1327    1.00    3.04   11.01
2012:IMG_9038 vs 2015 ref                 1460   1178    1.25    4.30   11.13
2012:IMG_9164 vs 2015 ref                 1402   1256    1.39    4.60   13.29
2012:IMG_9304 vs 2015 ref                 2098   2023    1.03    3.37   11.04
2012:IMG_9514 vs 2015 ref                 2153   1862    1.58    5.16   17.77
2021:IMG_7763 vs 2015 ref                 1608   1308    1.05    3.44   10.16
2021:IMG_7827 vs 2015 ref                 1379   1326    0.98    3.11   11.75
2021:IMG_7900 vs 2015 ref                 1435   1356    1.21    4.09   10.78
2021:IMG_8023 vs 2015 ref                 1950   1662    1.13    4.00   12.50
2021:IMG_8081 vs 2015 ref                 1101    979    2.17    6.19   12.33
2021:IMG_8172 vs 2015 ref                 1939   1792    1.65    5.49   15.09
2021:IMG_8298 vs 2015 ref                 2003   1706    1.85    5.48   12.04
2012 IMG_8748 vs 2021 IMG_7763            2670   2532    1.42    4.18   14.79
2012 IMG_9038 vs 2021 IMG_8172            3500   3352    1.19    3.60   11.95
2012 IMG_9514 vs 2021 IMG_8298            5288   5119    1.34    3.99   12.41
2012 internal IMG_8748 vs IMG_9514        1547   1421    1.18    4.15    9.53
2021 internal IMG_7763 vs IMG_8298        1181    976    1.31    4.11   12.53
```

**Interpretation.** Cross-year registration (median 1.2–1.4 px) is no worse than
within-year registration (1.2–1.3 px), and both are comparable to the image-to-reference
residuals. The alignment is genuinely good and there is **no systematic 2012-vs-2021 shift**.
This is a defensible answer to a reviewer.

However the *tails* matter for change detection: p90 is 3.6–4.2 px and the maximum is 12–15 px
between year stacks. The classifier uses 5x5 patches, so a 4 px offset moves a patch off its
own footprint entirely. In ground units the georectified map has 1,206,233 valid 1 m cells
derived from 11.78 M in-mask image pixels (~10 image px per m² on average, far fewer in the
far field), so a 4 px residual is already sub-metre near the camera but several metres near the
skyline. Since 57.8 % of the reported *Sasa* gain sits within 1.5 m of the 2012 *Sasa* boundary
(§7), residual misregistration is a first-order candidate explanation for a large share of both
gross gain and gross loss. A sensitivity analysis (e.g. recompute gain/loss after eroding both
year masks by 1–2 m) is the obvious response to Reviewer 3.

---

## 9. The missing model, recovered

`utils/interpolate.R:40-43` reads `results/cnn_lstm5x5_cv_5_ep_200/{diff_sasa,diff_haimatsu,
res2012_cnn_5x5,res2021_cnn_5x5}.csv` — a **CNN-LSTM, 5x5 kernel, 5-fold CV, 200 epochs**.
`scripts/sdm/image_to_csv.py:16` names `results/use_this/{2012,2021}_5x5.npy`.
Neither directory exists.

`models/` contains only `nnmodel.py` and `rnn.py`. **[RUN]** `models/__pycache__/` contains
`crnn.cpython-310.pyc` and `svm.cpython-38.pyc` — two modules whose sources were deleted.
Header parse (PEP 552 timestamp form):
```
utils/__pycache__/utils.cpython-310.pyc     src_mtime=2023-11-13 00:16:07  src_size=8083
models/__pycache__/nnmodel.cpython-310.pyc  src_mtime=2023-11-12 14:05:50  src_size=7955
models/__pycache__/rnn.cpython-310.pyc      src_mtime=2023-11-06 03:52:14  src_size=2037
models/__pycache__/crnn.cpython-310.pyc     src_mtime=2023-11-11 12:26:35  src_size=2392   <-- DELETED
models/__pycache__/svm.cpython-38.pyc       src_mtime=2022-06-22 07:56:51  src_size=5499   <-- DELETED
```
The surviving `.py` files have byte sizes **identical** to what the 3.10 pycs record
(8083 / 7955 / 2037), so the tracked code *is* the November-2023 code — `crnn.py` is the only
thing missing from that generation. Embedded path: `/home/okamoto/Projects/jasms2023f/scripts/models/crnn.py`.

**[RUN]** disassembly (`dis_pyc.py` under CPython 3.10.20) reconstructs it exactly:

```python
# models/crnn.py  (RECOVERED from crnn.cpython-310.pyc — this is the model that produced
#                  ortho/data/vege_{2012,2021}_5x5.tiff)
class CRNN(nn.Module):
    def __init__(self, x_shape, y_dim):          # x_shape = (seq_len, 3, 5, 5)
        super().__init__()
        self.x_shape  = x_shape
        self.conv1    = nn.Conv2d(3, 8, (3, 3), 1)
        self.bn2d_1   = nn.BatchNorm2d(8)
        self.prelu1   = nn.PReLU()
        self.maxpool1 = nn.MaxPool2d((3, 3), 1)
        conv_out_shape = (x_shape[2] - 4) * (x_shape[3] - 4) * 8      # = 1*1*8 = 8 for 5x5
        self.h_dim = int((conv_out_shape + y_dim) / 2)                # = int((8+7)/2) = 7
        self.lstm  = nn.LSTM(conv_out_shape, self.h_dim, batch_first=True)
        self.bn1   = nn.BatchNorm1d(self.h_dim); self.do1 = nn.Dropout(0.0)
        self.fc1   = nn.Linear(self.h_dim, self.h_dim); self.prelu2 = nn.PReLU()
        self.bn2   = nn.BatchNorm1d(self.h_dim); self.do2 = nn.Dropout(0.0)
        self.fc2   = nn.Linear(self.h_dim, y_dim)

    def forward(self, x):
        x = x.reshape(-1, self.x_shape[1], self.x_shape[2], self.x_shape[3])
        h = self.conv1(x); h = self.prelu1(h); h = self.bn2d_1(h); h = self.maxpool1(h)
        h = h.view(-1, self.x_shape[0], h.shape[1]*h.shape[2]*h.shape[3])
        ... lstm -> bn1 -> do1 -> prelu2 -> fc1 -> bn2 -> do2 -> F.softmax(self.fc2(h), dim=1)

class CRNNClassifier(NNClasifier):
    def __init__(self, data_dir, labels_dir, batch_size, device="cuda", num_workers=20,
                 label="all", test_size=0.2, cmap="jet", kernel_size=(5, 5)):
        super().__init__(..., kernel_size)
        seq_len = int(x.shape[1] / 3 / (kernel_size[0]*kernel_size[1]))
        self.x_shape = (seq_len, 3) + kernel_size
        self.model = CRNN(self.x_shape, self.y_dim).to(self.device)
        self.optimizer = torch_optimizer.RAdam(self.model.parameters(), lr=1e-3)
        self.loss_cls  = nn.CrossEntropyLoss()
```

Two consequences:

* **`run_rnn.py` is not the script that produced the published maps.** It builds
  `RNN` (a fully-connected + LSTM on 1x1 pixels), trains 100 epochs, and predicts with
  `kernel_size=(1,1)`. It would still be *self-consistent* on 1x1 data, because
  `NNClasifier.__init__:30-31` reshapes to `(N, -1, 3)` only when `kernel_size == (1,1)` —
  which is the default and the value `RNNClassifier` never overrides.
* **Latent trap for anyone who "fixes" `run_rnn.py` to read `../data/2012_5x5`:** with 5x5
  patches and the default `kernel_size=(1,1)`, that reshape turns a `(T, 75)` sequence into a
  `(T*25, 3)` sequence, and — because `set_patches` flattens `(T,C,H,W)` channel-major — each
  resulting "RGB triple" is three *spatially adjacent same-channel* values. Training would run
  without error and produce meaningless features, while `draw(..., (1,1), ...)` would feed a
  length-`T` sequence at inference. `CRNNClassifier` avoids this by passing
  `kernel_size=(5,5)`, which skips the reshape. **The 5x5 path must go through
  `CRNNClassifier`, never `RNNClassifier`.**

`svm.cpython-38.pyc` (path `/home/okamoto/VegetationMapPaper/scripts/models/svm.py`, June 2022)
shows an SVC baseline also existed. Note **three different project roots** appear across the
codebase: `~/VegetationMapPaper/`, `~/Projects/jasms2023f/`, `~/doctoral_thesis/chap2/`.

---

## 10. Other code defects found (not affecting published numbers)

| file:line | issue |
|---|---|
| `utils/utils.py:20-22` | hard-coded Debian font path executed at import; breaks the whole package elsewhere |
| `utils/utils.py:127` | `max(0, u-kw):(min(w, u+kh)+1)` — uses kernel **height** for the horizontal bound. Harmless only because all kernels are square |
| `utils/utils.py:51`, `109`, `122`, `169`, `173` | `glob(dir + "/*")` unsorted (line 51) or globbing everything including `.DS_Store` / `.Rhistory`. `data/labels/.Rhistory` and `data/images/source/*/.DS_Store` already exist |
| `utils/utils.py:78-82` | dead `MultiPolygon` branch; would raise on shapely >= 2 (`for poly in polygon` needs `.geoms`) |
| `utils/utils.py:61-67` | `label != "all"` branch assigns classIndex 9999, which `class_to_idx` cannot map. Unused |
| `utils/utils_old.py` | byte-identical duplicate of `utils.py` — delete |
| `models/nnmodel.py:161`, `run_rnn.py:35,45`, `calculate_diff.py:34,41` | `plt.imsave(path, arr, cmap=ListedColormap)` autoscales to `arr.min()..arr.max()`. If a prediction lacks class 0 or class 6 the PNG colours silently shift. (The archived PNGs happen to contain both, verified.) |
| `models/nnmodel.py:29-33` | loads the **entire** training set into one CPU tensor before splitting; with 618 k x T x 75 uint8→float this is tens of GB for the 5x5 case |
| `models/nnmodel.py:65,88` | `loss * batch_size / len(dataset)` is only correct when the last batch is full |
| `models/nnmodel.py:17,121` | `device="cuda"` default (overridable, so not fatal); `num_workers=20` default |
| `models/rnn.py:39` | `self.gelu2(h)` — return value discarded, the activation is a no-op |
| `apply_mask.py:4` | reads `..._maskd.png`; the file is `..._masked.png` → script cannot run |
| `apply_mask.py:1,13` | `cv2.imread` of a `plt.imsave` PNG then re-write: RGB/BGR round-trip; the archived `*_masked.png` are consistent but the code is fragile |
| `calculate_diff.py:16,18` | reads `results/2012_5x5.npy` / `2021_5x5.npy` — do not exist |
| `calculate_diff.py:11` | `"kaede": 4` — should be `dakekanba` (Erman's birch) |
| `prepare_data.py:3-13` | 7 of 9 calls target directories that do not exist; the 2015 aligned call is commented out |
| `utils/interpolate.R:6` | `setwd("~/VegetationMapPaper/")`; reads `results/cnn_lstm5x5_cv_5_ep_200/*.csv` — absent. Superseded by `ortho/georectify.R` |
| `ortho/georectify.R:1` | `setwd("~/Projects/jasms2023f/ortho/")`; inputs `data/{2012,2021}_5x5.csv` and `data/georectified.csv` — both absent |
| `scripts/sdm/image_to_csv.py:15-23` | the vegetation block is entirely commented out; only the snow block runs |
| `scripts/sdm/plot_vegetation_map.R:64,101` | writes `ortho/data/{2012,2021}_5x5.png` (Japanese labels); the manuscript uses `paper/files/{2012,2021}_5x5_en.jpg` — the English figure is made by code not in the repo |
| repo-wide | no `requirements.txt`/`environment.yml`/`renv.lock`; `.gitignore` excludes `*.npy` and `*.tiff`, so **none** of `results/*.npy`, `data/images/mask.npy`, `ortho/data/vege_*.tiff` is public. What reviewers can see is `results/*.png`, `results/cv.png`, `data/labels/*.json`, `data/images/aligned/*.png`, `data/images/source/*.JPG` and the scripts |

---

## 11. Retracted / corrected prior leads

* **"5 = ミネカエデ (maple)"** — *corrected*. The code says maple, but the training labels say
  **ダケカンバ / *Betula ermanii*** and the project's own `results/cv.png` says "Golden Birch".
  The code, the figure legend and matmet.qmd L39 are all wrong about the species.
* **"Manuscript reports 8,542 / 10,170 / 4,095 / 2,467 — all slightly different. Reconcile the
  definitions."** — *retracted*. They match the archived rasters exactly via
  `terra::expanse()` (UTM area scale factor 0.999455). No definitional discrepancy.
* **"`sdm_tdm.R:279` `is_others == 6` contradicts the mapping. Confirm."** — *confirmed as a
  bug and confirmed dead*: `grep -n is_others scripts/sdm/*.R` returns only lines 278-280.
  It is computed and never used. No published number is affected.
* **"`apply_mask.py` may collapse masked areas to value 0, making 0 ambiguous."** — *confirmed
  for `results/*.npy`* (97.1 % of the zeros are mask, 2.9 % are real *Sasa*), but **the
  published GeoTIFFs are not affected**: there 0 is the mask and *Sasa* is 1, proven three
  ways in §3. The transition matrix computed from the TIFFs is sound.
* **"`models/nnmodel.py` / `rnn.py` take `device='cuda'` as a DEFAULT argument, not a hard
  requirement."** — confirmed, and it is not the blocker. The blocker is the hard-coded font
  path at `utils/utils.py:20-22`, which raises `FileNotFoundError` at import.
* **"`run_rnn.py` refers to paths/years that may not match the repo layout."** — confirmed;
  **all nine** paths it needs are missing and there is no 2015 image stack anywhere.

---

## 12. Recommended actions

**Blocking for the revision**

1. Restore or rewrite `models/crnn.py` + a `run_crnn.py`, using the recovered source in §9,
   and re-run 5-fold CV so that a **held-out, polygon-blocked** accuracy assessment
   (confusion matrix + per-class F1 + kappa) exists. Report it in the manuscript. Nothing
   short of retraining gives an honest number; §5 gives the interim resubstitution figures.
2. Change the CV split from `StratifiedKFold` over pixels to a **grouped/spatial split over
   polygons** (`GroupKFold` with polygon id, or spatial blocks), matching what the HSM section
   already does. Report both so the inflation is visible.
3. Correct the class name everywhere: **Erman's birch (*Betula ermanii*)**, not Maple
   (*Acer tschonoskii*) — matmet.qmd L39, `plot_vegetation_map.R:37,77`,
   `calculate_diff.py:11`, and the published figures `paper/files/{2012,2021}_5x5_en.jpg`.
4. Add the transition matrix of §7 to the manuscript (Reviewer 3's explicit request), stated
   as gross gain / gross loss / net, and note that Dwarf pine — not a canopy shrub — is the
   largest single destination of "lost" *Sasa*, which weakens the occlusion explanation.
5. Add a change-detection uncertainty section: the 9.0 % overall class churn, the Sasa/Dwarf
   pine confusion rate, and a boundary-erosion sensitivity analysis.
6. Fix the discussion claim about isolated patches (§7) or report the 19 isolated patches.

**Reproducibility housekeeping**

7. Fix `apply_mask.py` to write a **sentinel that is not a class code** (e.g. `255` in a uint8
   array, or NaN), and re-issue `results/*.npy` with masked = 255. Or simply distribute the
   1-based arrays that the GeoTIFF pipeline already uses.
8. Fix `draw_teacher` so `teacher.npy` uses 1-based codes with 0 = unlabelled, and regenerate.
9. Commit the mask (`data/images/mask.npy` or a 1-bit PNG) and the script that made it, or
   document that it was drawn by hand. `mask_sky.py` is not it (79 % agreement).
10. Commit the 2015 image stack (or at least the four labelled 2015 frames) so the training
    labels can be checked, and re-issue `alignment_report` for the images actually used.
11. Remove `setwd()` from every script; adopt one project root (`here::here()` / relative to
    repo root). Add `requirements.txt` / `environment.yml` and `renv.lock`.
12. Delete `utils/utils_old.py`; move `models/__pycache__` out of the repo *after* extracting
    `crnn.py`; delete or clearly mark `calculate_diff.py`, `utils/interpolate.R`,
    `prepare_data.py`'s dead lines and `sdm_tdm.R:278-280`.
13. Note `.gitignore` currently hides every `.npy` and `.tiff`. For the Ecology & Evolution
    data-archiving requirement (Zenodo/Dryad), the deposit must include
    `ortho/data/vege_{2012,2021}_5x5.tiff`, the label JSONs, the aligned PNGs, the mask, and
    the trained checkpoints.
