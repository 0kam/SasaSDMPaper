# scripts/vegetation_classification/

The Conv2D–LSTM (CRNN) classifier that turns seasonal sequences of aligned time-lapse
photographs into per-pixel vegetation classes for 2012 and 2021. Implemented in Python
with PyTorch. The architecture, training settings and per-class accuracy reported in the
Supporting Information (Table S2) come from these files.

| File | Role |
|---|---|
| `prepare_data.py` | Cuts 5×5-pixel patches out of each aligned image sequence at every labelled pixel |
| `models/crnn.py` | The Conv2D–LSTM model: convolutional block, single-layer LSTM, two fully connected layers, softmax; loss and optimiser |
| `models/nnmodel.py` | Training harness: stratified k-fold split, checkpointing, per-class report, prediction drawing |
| `models/rnn.py` | The plain-LSTM variant used for comparison |
| `run_crnn.py` | Entry point: 5-fold cross-validation per year, then per-pixel majority vote over the fold models |
| `run_rnn.py` | The same for the plain-LSTM variant |
| `apply_mask.py` | Applies the sky / out-of-frame mask and writes `results/{2012,2021}_masked.npy` and `.png` |
| `calculate_diff.py` | Image-space differencing between the two years |
| `utils/utils.py` | Dataset classes, patch extraction, teacher and legend rendering |
| `utils/interpolate.R` | Cross-validation result tabulation |

## Inputs

- `data/images/aligned/<year>/*.png` — seven seasonal frames per year, aligned to a
  common frame (AKAZE + RANSAC + lens-distortion fit; see
  `data/images/align_photographs.py` and `data/images/alignment_report_2012_2021.csv`).
  `aligned/2015/` is kept because the training polygons were drawn mainly on the 2015
  imagery.
- `data/labels/*.json` — 216 hand-drawn polygons over seven classes: dwarf pine
  (*Pinus pumila*), dwarf bamboo (*Sasa* spp.), rowans, maple, montane alder, other
  vegetation, no vegetation.
- `data/images/mrd_085_eos_vis_20151010_1205_masked.png`, `sky_mask_2015.png` — the sky
  mask produced by `data/images/mask_sky.py`.

## Outputs

`results/{2012,2021}_masked.npy` (image-space class rasters, 5616 × 3744) and their PNG
renderings. These feed the georectification stage (`ortho/georectify.R`) and the blind
image-interpretation package in `review/image_interpretation_package/`.

## Note on paths

These scripts were run on a GPU server and still carry that machine's relative paths
(`../data_source/labels`, `../data_source/aligned/<year>`, `../data/<year>_5x5`,
`runs/cv/...`). Map `data_source/labels` onto `data/labels/` and
`data_source/aligned/<year>` onto `data/images/aligned/<year>/` before re-running. The
cross-validation run directories (checkpoints and TensorBoard logs) were not retained;
the CV metrics transcribed from them are in
`review/ece_review_summary/restore/classifier/`.
