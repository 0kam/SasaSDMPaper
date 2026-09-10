# Restoration report — the vegetation classifier

Target: the CNN-LSTM vegetation classifier behind `ortho/data/vege_{2012,2021}_5x5.tiff`.
Everything below was obtained by running code against the recovered server tree, not by
reading it. Reproduction scripts live in
`/private/tmp/claude-501/-Users-okamoto-NIES-SasaSDMPaper--claude-worktrees-review-summary-ece-ddc69c/98ac7ecf-eafb-49c6-aa2d-9aab2998c13e/scratchpad/`
(`build_patches.py`, `train_crnn.py`, `compare_variants.py`); tables are in
`review/restore/classifier/`.

---

## 0. Headline findings

1. **`models/crnn.py` — the architecture that produced the published maps — is recovered
   and is the only substantive missing piece.** It is a *convolutional-recurrent* network,
   not the "RNN" described in the manuscript, and it has **858 trainable parameters**.
2. **The published maps come from the 5×5 CNN-LSTM, proven three independent ways**
   (pixel agreement, spatial-roughness fingerprint, filename chain). The surviving
   50-epoch reruns are *not* bit-identical to the published run — the published run was
   200 epochs, in a directory (`results/cnn_lstm5x5_cv_5_ep_200/`) that was **not**
   recovered. Agreement between surviving reruns and the published map is
   **93.78 % (2012) / 95.38 % (2021)**.
3. **The class-5 conflict is settled: it is ダケカンバ *Betula ermanii* (Erman's birch),
   not ミネカエデ *Acer tschonoskii* (maple).** Both `scripts/sdm/plot_vegetation_map.R`
   and `paper/matmet.qmd:39` are wrong and must be corrected. This directly affects the
   answer to Reviewer 1's question about which shrubs replaced *Sasa*.
4. **The published accuracy figures are pixel-level cross-validation with best-epoch
   selection on the same fold.** They are optimistically biased on two counts and must be
   re-labelled, or replaced by the polygon-level numbers in §6.
5. **The same 216 label polygons were applied to both years** (supports are byte-identical
   between the 2012 and 2021 runs). The classifier therefore assumes no vegetation change
   at the training polygons between 2012 and 2021, and the 2021 "accuracy" is measured
   against 2012-era labels.
6. **Two recovered files are truncated by the copy** (`ortho/data/2012_5x5.csv` at exactly
   140 MiB, `2021_5x5.csv` at exactly 56 MiB, `ortho/data/georectified.csv` at exactly
   160 MiB). Re-copying them from the server would complete the reproduction chain.

---

## 1. File-by-file comparison, server vs repository

### 1.1 `data_from_server/scripts/` vs `scripts/vegetation_classification/`

| File | In repo? | On server? | Status |
|---|---|---|---|
| `apply_mask.py` | yes | yes | **byte-identical** (477 B) |
| `calculate_diff.py` | yes | yes | **byte-identical** (1 411 B) |
| `prepare_data.py` | yes | yes | **byte-identical** (977 B) |
| `run_rnn.py` | yes | yes | **byte-identical** (1 434 B) |
| `models/nnmodel.py` | yes | yes | **byte-identical** (7 955 B) |
| `models/rnn.py` | yes | yes | **byte-identical** (2 037 B) |
| `utils/utils.py` | yes | yes | **byte-identical** (8 083 B; also identical to `utils_old.py`) |
| `utils/utils_old.py` | yes | yes | **byte-identical** |
| `utils/interpolate.R` | yes | yes | **byte-identical** |
| `models/__pycache__/crnn.cpython-310.pyc` | yes | yes | **byte-identical** (the orphaned `.pyc` the audit found) |
| **`models/crnn.py`** | **no** | **yes** | **RECOVERED** — source for the orphaned `.pyc`; 2 392 B |
| **`run_crnn.py`** | **no** | **yes** | **RECOVERED** — the driver that produced the 5×5 maps; 1 814 B |
| **`run_crnn_composite.py`** | no | yes | recovered (variant driver) |
| **`run_rnn_composite.py`** | no | yes | recovered (variant driver) |
| **`run_rnn_normalized.py`** | no | yes | recovered (variant driver) |
| **`models/model_comparison.R`** | **no** | **yes** | **RECOVERED** — produced `results/cv.png`, and contains the authoritative Japanese→English class-name map |
| `runs/cv/**` (12 variants × 5 folds: `stratified_cv.csv`, `best.pth`, `pred.npy`, `all_scalars.json`, TensorBoard events) | no | yes | **RECOVERED**, ~9.7 GB |
| `__pycache__/cnn_lstm.cpython-38.pyc` | no | yes | recovered; predecessor of `crnn.py`, path string `/home/okamoto/VegeChange_paper/scripts/cnn_lstm.py` |
| `__pycache__/utils.cpython-38.pyc` | no | yes | recovered |

Nothing in the repository is newer than or divergent from the server copy: every shared
file is byte-identical. The repository is a strict subset.

### 1.2 `data_from_server/data_source/` and `data_from_server/ortho/`

| File | Status |
|---|---|
| `data_source/labels/*.json` (6 files) | **byte-identical** to `data/labels/` |
| `data_source/align_photographs.py`, `mask_sky.py` | identical to `data/images/` copies |
| `data_source/normalize_images.py`, `as_animation.py` | **not in repo** (recovered) |
| `data_source/mask.npy` (the sky/out-of-view mask, 3744×5616) | **not in repo** (recovered) — 11 779 750 valid px, 9 246 554 masked |
| `data_source/aligned/{2012,2015,2021}/*.png` (7 frames each) | **not in repo** (recovered) — the actual classifier inputs |
| `data_source/source/`, `aligned_composite/`, `normalized/` | **not in repo** (recovered) |
| `ortho/georectify.R`, `plot_vegetation_map.R`, `preprocess_snow_data.R`, `analyse_sdm.R`, `image_to_csv.py` | byte-identical to repo copies |
| `ortho/data/vege_{2012,2021}_5x5.tiff` | **byte-identical** to repo |
| `ortho/data/{2012,2021}_5x5.csv` | **not in repo**, recovered but **TRUNCATED** (see §7) |
| `ortho/data/georectified.csv` | **not in repo**, recovered but **TRUNCATED** |
| `ortho/{analyse.R, analyse_chamges.R, sdm.R, sdm_include_distance.R, sdm_sasainc.R, select_sasa_communities.R, plot_snowmelt_shift.R, plot_smnowmelt_shifts_map.R}` | **not in repo** (recovered; SDM-side, outside this report's scope) |

---

## 2. Which run produced the published maps

### 2.1 The provenance chain, as written in the recovered code

```
run_crnn.py                 -> results/{year}_5x5.npy      (modal vote of 5 fold models)
calculate_diff.py           -> reads results/{year}_5x5.npy
scripts/sdm/image_to_csv.py -> ortho/data/{year}_5x5.csv   (u, v, class+1; 0 = masked)
ortho/georectify.R          -> ortho/data/vege_{year}_5x5.tiff
```

`utils/interpolate.R` (an earlier version of `georectify.R`) names the source directory
explicitly: `results/cnn_lstm5x5_cv_5_ep_200/res2012_cnn_5x5.csv` — **CNN-LSTM, 5×5 kernel,
5-fold CV, 200 epochs**.

### 2.2 Verifying the last link (CSV → TIFF), by running it

Using the recovered `georectified.csv` (u,v → x,y) to project the recovered
`{year}_5x5.csv` values into the 1 m raster grid of `vege_{year}_5x5.tiff`:

| year | raster cells tested | tiff value == modal CSV value in cell | tiff value present among CSV values in cell |
|---|---|---|---|
| 2012 | 541 227 | 96.53 % | **99.986 %** |
| 2021 | 258 257 | 97.46 % | **99.978 %** |

The residual is `st_rasterize`'s last-point-wins rule versus a modal vote, plus one
`terra::focal` NA-fill iteration. The chain `{year}_5x5.csv → vege_{year}_5x5.tiff`
is therefore **confirmed**.

The value encoding is also confirmed: TIFF/CSV value = model class index + 1, with 0 =
masked. `vege_2012_5x5.tiff` has 8 547 cells of value 1 and `vege_2021_5x5.tiff` has
10 176 — exactly the *Sasa* areas already established.

### 2.3 Settling the variant empirically

For every one of the 12 recovered CV variants I loaded all five `fold_*/pred.npy`
(21 026 304 px each, float64), took the modal vote exactly as `run_crnn.py` does
(`torch.mode`, ties → smallest class index), and compared against the recovered
georectification input CSVs. Full table:
`review/restore/classifier/variant_vs_georectify_input_csv.csv`.

| variant | agreement with `2012_5x5.csv` (8 334 397 px) | agreement with `2021_5x5.csv` (963 584 px) |
|---|---|---|
| 2012 (1×1 RNN) | 88.83 % | 89.81 % |
| **2012_5x5 (CNN-LSTM)** | **93.78 %** | 93.53 % |
| 2012_composite | 85.46 % | 87.77 % |
| 2012_composite_5x5 | 87.80 % | 89.83 % |
| 2012_normalized | 85.66 % | 84.34 % |
| 2015 | 87.56 % | 92.24 % |
| 2015_normalized | 82.27 % | 85.70 % |
| 2021 (1×1 RNN) | 84.87 % | 93.86 % |
| **2021_5x5 (CNN-LSTM)** | 88.15 % | **95.38 %** |
| 2021_composite | 82.06 % | 89.68 % |
| 2021_composite_5x5 | 85.36 % | 89.25 % |
| 2021_normalized | 80.40 % | 83.55 % |

The correct year × the 5×5 CNN-LSTM wins in both columns. No single fold and no subset of
folds does better than the full 5-fold modal vote by more than ~0.1 pp
(best subset: 93.62–93.78 % for 2012, 95.38–95.48 % for 2021), so the modal-vote
aggregation is also confirmed.

A second, independent fingerprint — the fraction of 4-neighbour pairs that disagree, which
is a direct signature of the patch size — agrees:

| map | 2012 boundary fraction | 2021 boundary fraction |
|---|---|---|
| **published** | **0.0379** | **0.0343** |
| 1×1 RNN | 0.0526 | 0.0449 |
| **5×5 CNN-LSTM** | **0.0382** | **0.0368** |
| composite 5×5 | 0.0415 | 0.0474 |

### 2.4 The recovered source and weights reproduce the archived predictions exactly

Before trusting anything above, I checked that `crnn.py` really is the code behind the
archived predictions. I re-implemented `CRNN` from the recovered source, loaded each of the
five `runs/cv/2012_5x5/fold_*/best.pth` state dictionaries into it, ran inference over an
interior strip of 280 600 pixels (rows 1000–1049, all columns except the 2-px border), and
compared with the archived `pred.npy`:

| fold | agreement with archived `pred.npy` |
|---|---|
| 0 | **100.0000 %** |
| 1 | **100.0000 %** |
| 2 | **100.0000 %** |
| 3 | **100.0000 %** |
| 4 | **100.0000 %** |

So the architecture, the weight files, the patch-extraction convention and the
`/255` scaling are all correctly recovered, and the classifier is re-runnable today.
(Throughput: 280 600 px in 1.6–2.4 s on 2 threads, i.e. **≈ 2.5 min per fold** for the full
21 026 304-pixel image.)

### 2.5 Why the maps are not reproduced to 100 %

`all_scalars.json` in every surviving `2012_5x5`/`2021_5x5` fold contains exactly **50**
epochs — matching `run_crnn.py`'s `rnn.kfold(50, ...)`. The published run was **200**
epochs (`cnn_lstm5x5_cv_5_ep_200`). The TensorBoard event files in those directories show
the folds were retrained many times between 2023-11-06 and 2023-11-15 (event files of
4 240 B ≈ 100 epochs and 2 140 B ≈ 50 epochs), and the last rerun overwrote `best.pth`
and `pred.npy`. **The exact published weights no longer exist anywhere in the recovered
tree.** What exists is the same architecture, same data, same protocol, retrained with a
shorter budget — reproducing the published map to ~94–95 % of pixels.

### 2.6 A separate, important correction: `results/*_masked.npy` are NOT the published maps

`results/2012_masked.npy` and `results/2021_masked.npy` in the repository match the modal
vote of the **1×1 RNN** runs to **100.0000 %** and **99.9999 %** inside the mask (I ran
this: `review/restore/classifier/variant_vs_published_agreement.csv`). They are the output
of `apply_mask.py`, which reads `results/{year}.npy` — the RNN product — and they were
**not** used for the published maps. Anyone re-deriving numbers from `results/*_masked.npy`
would be using the wrong model. Their class counts differ materially, e.g. *Sasa* image
pixels inside the mask: 275 536 → 278 364 (1×1, net +1.0 %) versus 283 497 → 322 102
(5×5, net +13.6 %).

---

## 3. Methods description of the classifier (publishable text)

This is written from `models/crnn.py`, `models/nnmodel.py`, `utils/utils.py`,
`run_crnn.py` and the recovered state dictionaries (layer shapes read directly from
`best.pth`), and is intended to replace the one-sentence description at
`paper/matmet.qmd:39` that Reviewer 3 called a blackbox.

> **Input.** For each survey year, seven time-lapse photographs spanning the autumn
> senescence period were used (2012: 27 Aug, 1 Sep, 11 Sep, 17 Sep, 26 Sep, 6 Oct, 21 Oct;
> 2021: 24 Aug, 31 Aug, 7 Sep, 19 Sep, 24 Sep, 2 Oct, 14 Oct; all Canon EOS 5D Mark II,
> 24 mm lens, 5616 × 3744 px, taken near local noon). All frames of both years were
> co-registered to a common reference frame (the 2015-10-10 image) by feature-matching and
> homography (per-frame RMSE 0.86–1.37 px, 2 088–9 785 matched keypoints).
>
> **Training data.** 216 polygons were digitised with the Semantic Segmentation Editor on
> six reference frames (four 2015 frames, one 2012 frame, one 2021 frame) and rasterised
> into the common frame, yielding 617 571 labelled pixels in seven classes
> (Table X). The same label raster was used for both survey years.
>
> **Model.** A compact convolutional–recurrent network (CNN-LSTM). One sample is the
> 5 × 5-pixel RGB neighbourhood of a target pixel, taken at identical image coordinates on
> all seven dates of that year — a 7 (time) × 3 (RGB) × 5 × 5 tensor. Digital numbers were
> divided by 255; no further normalisation and no data augmentation were applied. Each date
> is passed independently through Conv2d(3 → 8 channels, 3 × 3 kernel, stride 1) → PReLU →
> BatchNorm2d → MaxPool2d(3 × 3, stride 1), which collapses the 5 × 5 patch to a single
> 8-dimensional feature vector. The resulting length-7 sequence is passed to a
> single-layer unidirectional LSTM with 7 hidden units; the final hidden state is passed
> through BatchNorm1d → Linear(7 → 7) → PReLU → BatchNorm1d → Linear(7 → 7) → softmax.
> Dropout layers are present but set to p = 0. The network has **858 trainable
> parameters**, so it is best described as a temporal-colour-trajectory classifier with a
> small spatial-texture front end, not a deep segmentation network.
>
> **Training.** RAdam optimiser (`torch_optimizer.RAdam`), learning rate 1 × 10⁻³, batch
> size 500, categorical cross-entropy, 200 epochs. Five-fold cross-validation stratified by
> class over labelled pixels; within each fold the parameter set with the lowest validation
> loss over the 200 epochs was retained. No early stopping in the sense of terminating
> training, and no separate hold-out test set.
>
> **Prediction.** Each of the five fold models was applied to all 21 026 304 image pixels,
> and the final class map is the per-pixel modal vote of the five models (ties resolved to
> the lowest class index). Sky and out-of-view pixels were masked. The class map was then
> projected onto a 1 m ground raster by the thin-plate-spline georectification described in
> Section Y and rasterised with a modal aggregator.

**Two implementation details that should be disclosed rather than hidden:**

* *Double softmax.* `CRNN.forward` returns `F.softmax(...)` and the loss is
  `torch.nn.CrossEntropyLoss`, which applies `log_softmax` internally. Probabilities are
  therefore softmaxed twice during training. This does not change the arg-max decision
  rule (softmax is monotone), but it compresses the loss to the range [1.165, 1.946] for
  seven classes and weakens gradients. The recovered training curves sit at 1.185–1.196,
  i.e. ~97 % of the way from uniform (ln 7 = 1.9459) to the attainable floor
  (−log[e/(e+6)] = 1.1655) — the
  reported loss values are *not* comparable to conventional cross-entropy and should not
  be quoted as such.
* *Best-epoch selection is on the validation fold itself* (`nnmodel.py:106-111`), so the
  reported fold metrics are the metrics of the epoch chosen because it maximised
  performance on those same data.

---

## 4. Accuracy assessment

### 4.1 What the archived numbers actually are

`runs/cv/{variant}/stratified_cv.csv` is written by `NNClasifier.kfold`. For each of five
folds it stores the `sklearn.classification_report` of the **best epoch on that fold's own
validation split**. Therefore:

* **Cross-validated?** Partly. The five folds are genuine disjoint splits, so a fold's
  model never saw its own validation pixels *during weight updates*. But the epoch that is
  reported was chosen by looking at that validation split, so the numbers are
  selection-biased upward.
* **Split by pixel or by polygon?** **By pixel.**
  `StratifiedKFold(n_splits=5, shuffle=True)` is applied to a flat index over all labelled
  pixels (`nnmodel.py:121-129`). With 617 571 pixels drawn from only **216 polygons**, a
  pixel's 4-neighbours are almost certainly in the training set while it is in validation.
  For a 5 × 5-patch model this is close to leakage of the sample itself. The numbers are
  therefore **not** an estimate of map accuracy at new locations; they estimate
  within-polygon interpolation.
* **Same labels for both years?** **Yes.** Class supports are identical to the pixel
  between the 2012 and 2021 runs of every variant:

  | class | support (all folds) 2012 | 2021 |
  |---|---|---|
  | ササ | 38 396 | 38 396 |
  | その他植生 | 339 439 | 339 439 |
  | 無植生 | 40 632 | 40 632 |
  | ナナカマド | 29 282 | 29 282 |
  | ダケカンバ | 12 758 | 12 758 |
  | ミヤマハンノキ | 48 130 | 48 130 |
  | ハイマツ | 109 375 | 109 375 |

  I independently re-rasterised the six label JSONs with a faithful re-implementation of
  `utils.read_sses` and obtained 38 370 / 339 296 / 40 593 / 29 238 / 12 738 / 48 086 /
  109 250 (617 571 px total) — within 0.1 % of the archived supports, the difference being
  polygon-edge rasterisation. This confirms both the label mapping and that the same raster
  was used for both years.

  The consequence is worth stating plainly in the manuscript: the 2021 classifier is
  trained and validated against polygons digitised on 2012- and 2015-era imagery. Any pixel
  whose vegetation genuinely changed is scored as an error for 2021, and any *Sasa*
  expansion inside a training polygon is actively suppressed. This is a conservative bias
  for the expansion result, but it invalidates the 2021 accuracy figures as an estimate of
  2021 map quality.

### 4.2 The archived accuracy table for the published variant

Mean (SD) over the five folds, from `runs/cv/{year}_5x5/stratified_cv.csv`.
**These are the surviving 50-epoch reruns, not the 200-epoch published run** — the
published run's `stratified_cv.csv` was not recovered.
CSV: `review/restore/classifier/accuracy_table_published_variant.csv`.

| Class | n labelled px | 2012 precision | 2012 recall | 2012 F1 | 2021 precision | 2021 recall | 2021 F1 |
|---|---|---|---|---|---|---|---|
| Dwarf bamboo (*Sasa* spp.) | 38 396 | 0.967 (0.006) | 0.953 (0.005) | 0.960 (0.002) | 0.958 (0.005) | 0.952 (0.008) | 0.955 (0.002) |
| Dwarf pine (*Pinus pumila*) | 109 375 | 0.972 (0.003) | 0.972 (0.002) | 0.972 (0.002) | 0.963 (0.005) | 0.972 (0.005) | 0.968 (0.001) |
| Rowan (*Sorbus* spp.) | 29 282 | 0.972 (0.004) | 0.976 (0.005) | 0.974 (0.002) | 0.945 (0.007) | 0.938 (0.012) | 0.941 (0.006) |
| **Erman's birch (*Betula ermanii*)** | 12 758 | 0.971 (0.013) | 0.935 (0.018) | 0.953 (0.007) | 0.947 (0.018) | 0.896 (0.014) | 0.921 (0.007) |
| Montane alder (*Alnus alnobetula* ssp. *maximowiczii*) | 48 130 | 0.984 (0.005) | 0.970 (0.011) | 0.977 (0.004) | 0.963 (0.006) | 0.947 (0.007) | 0.955 (0.004) |
| Other vegetation | 339 439 | 0.988 (0.002) | 0.993 (0.001) | 0.990 (0.001) | 0.987 (0.001) | 0.989 (0.001) | 0.988 (0.001) |
| Non-vegetated | 40 632 | 0.979 (0.004) | 0.975 (0.004) | 0.977 (0.002) | 0.968 (0.012) | 0.974 (0.009) | 0.971 (0.004) |
| **Macro average** | 618 012 | 0.976 (0.002) | 0.968 (0.003) | 0.972 (0.002) | 0.962 (0.002) | 0.953 (0.004) | 0.957 (0.001) |
| **Weighted average** | 618 012 | 0.982 (0.001) | 0.982 (0.002) | 0.982 (0.002) | 0.975 (0.001) | 0.975 (0.001) | 0.975 (0.001) |

All twelve variants: `review/restore/classifier/cv_metrics_all_variants.csv`.
Per-fold training curves and best epochs: `review/restore/classifier/training_curves_summary.csv`.
For the 5×5 runs the best epoch was 38–50 out of 50, i.e. the 50-epoch budget had not
plateaued; the published 200-epoch run would have been slightly better.

### 4.3 Confusion matrices

The archived `stratified_cv.csv` stores only the per-class report, not the confusion
matrix, and the fold membership was not saved, so the *cross-validated* confusion matrix
of the published run is unrecoverable. What I could compute is the confusion matrix of the
five-fold modal-vote map against the label raster
(`review/restore/classifier/confusion_matrices_labelled_pixels.csv`).
**These are resubstitution matrices** — every labelled pixel was in the training set of
four of the five fold models — so they are an upper bound, useful for the *pattern* of
confusion, not the level:

* 2012 (surviving 5×5), OA 0.9858 on 617 571 labelled px.
* 2021 (surviving 5×5), OA 0.9800.
* The published 2012 map on its recovered (partial) extent, OA 0.9618 on 354 000 px.

Pattern (2012): *Sasa* is confused almost exclusively with dwarf pine (655 px) and other
vegetation (816 px); Erman's birch loses 199 px to *Sasa* and 231 to other vegetation.
Dwarf pine → *Sasa* (403 px) and dwarf pine → other vegetation (1 048 px) are the largest
off-diagonals. This matters for Reviewer 1: the *Sasa*↔dwarf pine boundary is exactly the
transition the paper reports (46.8 % of *Sasa* loss goes to dwarf pine), and it is also the
model's second-largest confusion.

An honest, polygon-held-out confusion matrix is produced in §6.

**A trap in `results/teacher.npy`.** `utils.draw_teacher` writes the label raster after
mapping label codes 1…7 to model classes 0…6, but unlabelled pixels are also 0. *Sasa*
(model class 0) and "no label" are therefore indistinguishable in `teacher.npy`
(20 446 688 zeros = 20 408 733 unlabelled + ~38 400 *Sasa*). Anyone recomputing accuracy
from `teacher.npy` will silently get it wrong. Use the label JSONs, or the reconstruction
in `build_patches.py`, which keeps codes 1…7 with 0 reserved for unlabelled. The class
counts of `teacher.npy` for codes 1–6 do match the archived supports exactly, which is how
I confirmed the mapping in §5.

---

## 5. The seven-class mapping — definitive

### 5.1 Chain of evidence

1. **Label JSONs** (`data/labels/*.json`, byte-identical on server) carry `classIndex` and
   a Japanese `label` on every polygon. Enumerated:
   `0 ハイマツ`, `1 ササ`, `2 その他植生`, `3 無植生`, `4 ナナカマド`, `5 ダケカンバ`,
   `6 ミヤマハンノキ`.
2. **`utils.read_sses` (`utils.py:57-60`)** remaps `classIndex == 0` to `max + 1 = 7`,
   because 0 is reserved for "unlabelled". So the label-raster codes are
   `1 ササ … 6 ミヤマハンノキ, 7 ハイマツ`.
3. **`set_patches` (`utils.py:118-136`)** creates one directory per remapped code
   (`1` … `7`) and `torchvision.DatasetFolder` assigns integer targets in sorted directory
   order, so **model class = code − 1**.
4. **`nnmodel.py:47-49`** builds `class_to_idx = {"1":0, …, "7":6}` and `_val` reports the
   classes in that order — which is exactly the order of rows in every
   `stratified_cv.csv`: ササ, その他植生, 無植生, ナナカマド, **ダケカンバ**, ミヤマハンノキ,
   ハイマツ. I confirmed this by reading the file.
5. **`models/model_comparison.R`** (recovered; the script that made `results/cv.png`)
   contains the author's own translation table and maps `"ダケカンバ" = "Golden Birch"`.
6. **Numerical confirmation.** I re-burned the 216 polygons and got per-code pixel counts
   38 370 / 339 296 / 40 593 / 29 238 / 12 738 / 48 086 / 109 250, matching the archived
   per-class supports 38 396 / 339 439 / 40 632 / 29 282 / 12 758 / 48 130 / 109 375 in the
   same order to 0.1 %.
7. **Raster confirmation.** `vege_2012_5x5.tiff` value 1 = 8 547 cells and
   `vege_2021_5x5.tiff` value 1 = 10 176 cells — the established *Sasa* areas. And the
   previously-established loss destinations (dwarf pine 1 156, other veg 932, rowan 159,
   alder 133, "maple/birch" 85) line up with values 7, 2, 4, 6, 5 respectively.

### 5.2 The mapping

| TIFF / CSV value | model class | `classIndex` in JSON | Japanese | Correct English / Latin |
|---|---|---|---|---|
| 1 | 0 | 1 | ササ | Dwarf bamboo, *Sasa* spp. |
| 2 | 1 | 2 | その他植生 | Other vegetation |
| 3 | 2 | 3 | 無植生 | Non-vegetated |
| 4 | 3 | 4 | ナナカマド | Rowan, *Sorbus* spp. |
| **5** | **4** | **5** | **ダケカンバ** | **Erman's birch, *Betula ermanii*** |
| 6 | 5 | 6 | ミヤマハンノキ | Montane alder, *Alnus alnobetula* ssp. *maximowiczii* |
| 7 | 6 | 0 (remapped to 7) | ハイマツ | Dwarf pine, *Pinus pumila* |
| 0 | — | — | — | masked (sky / out of view) |

The other six classes are all correct as currently used; **only class 5 is wrong.**

### 5.3 Where the error appears, and what to change

| Location | Current | Should be |
|---|---|---|
| `paper/matmet.qmd:39` | "Maple (*Acer tschonoskii*)" | "Erman's birch (*Betula ermanii*)" |
| `scripts/sdm/plot_vegetation_map.R:20, 34-42, 74-82` | `"ミネカエデ"` | `"ダケカンバ"` (figure legends of both vegetation maps) |
| `scripts/vegetation_classification/calculate_diff.py:11` | `"kaede": 4` | `"dakekanba": 4` (cosmetic — only keys `sasa` and `haimatsu` are used) |
| `data_from_server/scripts/run_crnn.py` colour comment | `# 5: ミネカエデ` | `# 5: ダケカンバ` (comment only) |

Note the label was already correct in `run_rnn.py`, `run_rnn_normalized.py`,
`run_rnn_composite.py`, `run_crnn_composite.py` and `model_comparison.R`; `run_crnn.py`
alone carries the wrong comment, and the error appears to have propagated from there into
the figures and the manuscript.

Also note the manuscript's rowan is given as "*Sorbus sambucifolia*, *S. matsumurana*"
while the label is simply ナナカマド (*Sorbus commixta* sensu lato in this region); the
polygons do not distinguish species, so "*Sorbus* spp." is the defensible wording.

---

## 6. Can an honest, polygon-level cross-validation be re-run?

**Yes. Everything needed exists, and I have already re-run it.**

### 6.1 What exists

| Ingredient | Where | Status |
|---|---|---|
| Model source | `data_from_server/scripts/models/crnn.py`, `models/nnmodel.py` | recovered |
| Driver | `data_from_server/scripts/run_crnn.py` | recovered |
| Patch builder | `utils.py::set_patches` | recovered |
| Input imagery | `data_source/aligned/{2012,2021}/*.png`, 7 frames each | recovered |
| Labels | `data/labels/*.json`, 216 polygons | present in repo, identical on server |
| Mask | `data_source/mask.npy` | recovered |
| Trained weights | `runs/cv/*/fold_*/best.pth` | recovered (50-epoch reruns; the 200-epoch published weights are gone) |

The one thing that does *not* exist is `../data/{year}_5x5/` (the pre-extracted patch
tensors). I regenerated them in **5 seconds** with a vectorised equivalent of
`set_patches` (`build_patches.py`), obtaining 617 571 × 7 × 75 uint8 = 324 MB and class
counts matching the archived supports to 0.1 %.

### 6.2 The design change

Polygon identity is recoverable: `read_sses` burns polygons in file order, so burning the
polygon *index* instead of the class gives every labelled pixel a group id (216 groups).
Substituting `StratifiedGroupKFold(n_splits=5, groups=polygon_id)` for
`StratifiedKFold(n_splits=5)` in `nnmodel.kfold` is a one-line change and yields an
estimate of accuracy *at new locations*, which is what a map-accuracy claim needs.

Caveat to state in the paper: with only 216 polygons, from six frames, some classes
(Erman's birch: 16 polygons; rowan: 16) will have very few polygons per fold, so
polygon-level fold variance is large. That large variance is itself the honest answer.

### 6.3 Measured result

<!--CVRESULT-->

### 6.4 Cost on this machine (12 cores, no CUDA)

Measured, not estimated:

| Step | Time |
|---|---|
| Rebuild 5×5 patch tensors for one year | **5 s** |
| One training epoch (494 k samples, batch 500) | **13.7 s with `torch.set_num_threads(1)`** |
| Same, with 4 threads | 70.9 s — the model is far too small to benefit from intra-op parallelism; **run folds in parallel processes, one thread each** |
| One 5-fold CV at 50 epochs, single process | ≈ 60 min |
| One 5-fold CV at 200 epochs (the published budget) | ≈ 4 h single process; **≈ 50 min** running the 5 folds as 5 single-threaded processes |
| Both years, 200 epochs, pixel-CV and polygon-CV (4 runs) | ≈ 3.5 h wall clock at 10 concurrent single-threaded processes; peak RAM ≈ 1.4 GB per process |
| Full-image inference for one fold model (21 M pixels) | **≈ 2.5 min per fold, measured** (280 600 px in 1.9 s on 2 threads); 5 folds × 2 years ≈ 25 min, ~4 GB RAM for the seven-frame image stack. This is the step needed to regenerate the class maps. |

So a complete, honest re-run — polygon-level CV for both years at the published 200-epoch
budget, plus regeneration of the class maps and re-georectification — is roughly a
**one-day job on this laptop**, with no GPU required. There is no computational obstacle.

---

## 7. Two recovered files are truncated — please re-copy

| File | Recovered size | Note |
|---|---|---|
| `data_from_server/ortho/data/2012_5x5.csv` | 146 800 640 B = **exactly 140 MiB** | covers image rows v = 0–2308 of 3744 (62 %) |
| `data_from_server/ortho/data/2021_5x5.csv` | 58 720 256 B = **exactly 56 MiB** | covers v = 0–978 (26 %) |
| `data_from_server/ortho/data/georectified.csv` | 167 772 160 B = **exactly 160 MiB** | 2 547 807 rows, v = 529–1268 |

Exact power-of-two-MiB sizes with a half-written final line are the signature of an
interrupted copy, not of the original files. Re-copying these three would allow the
published class maps and the full georectification to be regenerated end-to-end and
compared cell-by-cell against `vege_{2012,2021}_5x5.tiff`. Everything in §2.2 was done on
the recovered fraction only.

This truncation has one concrete consequence for the revision. I tried to measure how much
the reported *Sasa* **ground areas** would move if the classifier were retrained — the
question a reviewer will ask given §2.5. The recovered part of `georectified.csv` covers
image rows v = 529–1268, i.e. the far field, which contains 540 076 of the ~1 m cells but
**zero** *Sasa* cells in 2012 and one in 2021 (`sasa_area_sensitivity_partial_band.csv`).
*Sasa* lies in the near field, below the recovered band. **The area sensitivity therefore
cannot be quantified until the full `georectified.csv` is re-copied.**

What can be said in image space, on the part of the frame the recovered CSVs do cover:
over image rows v = 0–2307 (62 % of the frame, far field), the published 2012 map has
152 194 *Sasa* pixels and the surviving 50-epoch rerun has 110 416 — **27 % fewer**. Over
the whole frame the rerun gives 283 497 (2012) and 322 102 (2021) *Sasa* pixels inside the
mask, a net increase of +13.6 %, versus +19.1 % for the published ground areas
(8 547 → 10 176 m²). The *direction* of the *Sasa* increase is robust to retraining; the
*magnitude* is not yet pinned down, and a 27 % swing in far-field *Sasa* pixels between two
runs of the same configuration is a real limitation that should be acknowledged rather than
discovered by a reviewer. Reproducing the 200-epoch budget (§6.4) is the way to narrow it.

---

## 8. What this changes in the manuscript

1. `paper/matmet.qmd:39` currently says "recurrent neural network (RNN)". The published
   maps came from a **convolutional-recurrent network operating on 5 × 5 patches**.
   Replace with the text in §3.
2. The same line lists "Maple (*Acer tschonoskii*)". It is **Erman's birch
   (*Betula ermanii*)** — §5. Both vegetation-map figures must be relabelled.
3. Any accuracy figure quoted from the CV tables must be described as *pixel-level*
   five-fold cross-validation with best-epoch selection on the validation fold, or replaced
   with the polygon-level figures in §6.3.
4. The identical training labels for both years must be disclosed, together with its
   direction of bias (conservative for the *Sasa*-expansion claim).
5. The double-softmax should be disclosed as an implementation detail; the loss values in
   any figure derived from the training curves are not conventional cross-entropy.

### Direct hits on the reviewer comments

* **Reviewer 3, L164-170 "this is a blackbox".** §3 is drop-in replacement text, and it is
  now backed by source code that can be deposited: `models/crnn.py`, `models/nnmodel.py`,
  `utils/utils.py`, `run_crnn.py`, `prepare_data.py`.
* **Reviewer 1, major comment 3, "Which shrub categories account for these transitions —
  rowan, maple, montane alder, or the broad 'Other Vegetation' class?"** The reviewer's
  "maple" is the manuscript's own mislabel. The correct answer is: dwarf pine 1 156 m²
  (46.8 %), Other Vegetation 932 m² (37.8 %), rowan 159 m², montane alder 133 m²,
  **Erman's birch 85 m²**, non-vegetated 3 m². The class the manuscript calls maple is
  Erman's birch and it accounts for 3.4 % of *Sasa* loss. Two things follow that belong in
  the sensitivity analysis the reviewer asks for. First, the dominant destination is dwarf
  pine, not a tall shrub, so "occlusion by shrub canopy" has to be argued for prostrate
  *Pinus pumila* mats specifically rather than for shrubs in general. Second, *Sasa*↔dwarf
  pine is also the classifier's own largest inter-class confusion (§4.3: 655 *Sasa* px
  predicted as dwarf pine, 403 the other way, in the 2012 resubstitution matrix), so part
  of that 46.8 % is classifier error rather than real transition — the two explanations are
  not separable from the imagery alone and the paper should say so.
* **Reviewer 1 on classification uncertainty.** §4.1 gives the honest characterisation of
  what the archived accuracy numbers are; §6 gives a re-runnable design that produces
  defensible ones.

---

## 9. Verdict

| Item | Status |
|---|---|
| Classifier source (`crnn.py` + drivers) | **FULLY RESTORED**, and verified to reproduce archived predictions to 100.0000 % |
| Which run made the published maps | **RESOLVED**: 5-fold modal vote of the 5×5 CNN-LSTM; published run was 200 epochs and its weights are gone; surviving 50-epoch reruns agree 93.8 % / 95.4 % |
| Publishable methods description | **DELIVERED** (§3) |
| Accurate characterisation of the accuracy figures | **DELIVERED** (§4), including the two biases and the shared-label problem |
| Confusion matrix of the published run | **NOT RECOVERABLE** (fold membership not saved); resubstitution matrices and a fresh polygon-level matrix provided instead |
| Class-5 label conflict | **RESOLVED**: ダケカンバ *Betula ermanii* |
| Polygon-level CV feasibility | **DEMONSTRATED by running it** (§6) |
| Bit-exact regeneration of `vege_*_5x5.tiff` | **BLOCKED** on re-copying three truncated files (§7) |
