# SasaSDMPaper

Repository for the paper "Snowmelt regime and dispersal limitation predict the decadal
expansion of dwarf bamboo into alpine snowbed vegetation" (Ecology and Evolution, in revision)

Ground-based time-lapse photographs of the Murodo-daira alpine area (Tateyama, Japan)
are classified into seven vegetation classes for 2012 and 2021, georectified onto a
common 1 m grid, and combined with a camera-derived snowmelt day-of-year climatology
(2011-2021) to fit two habitat models — an environmental-potential model (Model A) and
a nine-year establishment model (Model B) — which are then projected to 2030 with a
cellular automaton under three snowmelt scenarios.

---

## Directory layout

| Path | Role |
|---|---|
| `analysis/` | The manuscript's analysis pipeline: `00_config.R` … `08_manuscript_figures.R`, shared helpers (`R/`), acceptance tests (`tests/`), figure scripts (`figures/`), outputs (`out/`) |
| `scripts/vegetation_classification/` | Python/PyTorch Conv2D–LSTM classifier that turns aligned photographs into per-pixel vegetation classes |
| `scripts/sdm/` | Species-distribution code of the **original (2026-04) submission**, superseded by `analysis/`. Retained because it defines the vegetation class codes used downstream |
| `ortho/` | Georectification: `georectify.R` plus the geospatial inputs under `ortho/data/` |
| `data/` | Photographic and field inputs: source and aligned photographs (`images/`), hand-drawn training polygons (`labels/`), snow-front tracings (`snow/`) |
| `data_external/` | Third-party geodata (Ministry of the Environment 1:25,000 vegetation map) |
| `results/` | Image-space classifier output, used by the blind-interpretation package in `review/` |
| `review/` | The three additional analyses behind the Supporting Information (snowmelt-trend sensitivity, *Sasa*–dwarf pine adjacency, blind image interpretation) and the scripts and tables that re-derived the classifier accuracy and georectification provenance (`restore/`) |
| `paper/` | Quarto manuscript, Supporting Information, figures |

## Reproduction

Four stages, in order. Stages 1–3 were run once to build the geospatial inputs; stage 4
is the analysis that produces every number and figure in the manuscript, and is the part
that can be re-run from the archived inputs alone.

### 1. Vegetation classification (Python / PyTorch)

`scripts/vegetation_classification/`, in order: `prepare_data.py` (5×5-pixel patches
from the aligned photographs and the label polygons), `run_crnn.py` (Conv2D–LSTM,
stratified 5-fold CV, per-pixel majority vote over the fold models), `apply_mask.py`
(sky and out-of-frame mask, writing `results/{2012,2021}_masked.npy`). Photograph
alignment (AKAZE + RANSAC + lens-distortion fit) is `data/images/align_photographs.py`;
the sky mask is `data/images/mask_sky.py`.

These scripts were run on a GPU server and carry that machine's relative paths
(`../data_source/labels`, `../data_source/aligned/<year>`, …); map them onto
`data/labels/` and `data/images/aligned/<year>/` before re-running. See
`scripts/vegetation_classification/README.md`.

### 2. Georectification (R)

`ortho/georectify.R` converts image-space class labels and snow-front tracings into
1 m rasters on EPSG:6690, using the dense image-pixel-to-world lookup table
`ortho/data/georectified.csv`. It produces `ortho/data/vege_{2012,2021}_5x5.tiff` and
`ortho/data/snow/raw/MRD_snowfront_L_<year>_*.tiff`.

### 3. Snowmelt rasters

The ten annual snowmelt day-of-year rasters in `ortho/data/snow/raw/` (2011–2018, 2020,
2021; 2019 missing because the camera could not be operated, 2010 excluded for an
incompatible field of view) are the georectified snow-front tracings from stage 2. No
further preprocessing is applied — `analysis/` reads `raw/` directly and derives the
climatology and the per-pixel trend surface itself.

### 4. Analysis (R 4.5.2)

Run from the repository root, front to back, non-interactively:

```
Rscript analysis/01_predictors.R          # predictor stack, snowmelt climatology + OLS trend
Rscript analysis/02_folds_and_distance.R  # distance-to-Sasa surface, spatial CV blocks
Rscript analysis/03_model_A.R             # Model A: environmental potential (stacked ensemble)
Rscript analysis/04_model_B.R             # Model B: nine-year establishment GAM
Rscript analysis/05_ca_projection.R       # cellular-automaton projection to 2030
Rscript analysis/06_snowmelt_stats.R      # snowmelt trend statistics
Rscript analysis/07_risky_areas.R         # 2030 risky areas, MOE vegetation composition
Rscript analysis/08_manuscript_figures.R  # manuscript and SI figures -> paper/files/
```

Acceptance tests; each exits non-zero if any check fails:

```
Rscript analysis/tests/test_wp1.R   # after 01
Rscript analysis/tests/test_wp2.R   # after 02
Rscript analysis/tests/test_wp3.R   # after 03
Rscript analysis/tests/test_wp4.R   # after 05
Rscript analysis/tests/test_wp5.R   # after 07
Rscript analysis/tests/test_wp6.R   # after 06
```

Environment variables:

- `SASA_REPO_ROOT` — repository root. It is the only hardcoded path in the pipeline;
  set it when the repository is checked out anywhere but the author's machine.
- `SASA_SMOKE=1` — small end-to-end integration run that still exercises every stage.
- `SASA_CA_PARALLEL_BACKEND=sequential` — for runners that forbid the local socket the
  default `PSOCK` cluster needs.

Every seed is defined in `analysis/00_config.R` and is printed, with `sessionInfo()`, at
the end of each script; the full-run logs are kept as `analysis/out/full_run_*.log`.

### Manuscript

```
cd paper && quarto render index.qmd
```

## Inputs and data availability

Inputs consumed by the analysis stage:

| Input | Origin |
|---|---|
| `ortho/data/vege_{2012,2021}_5x5.tiff` | stages 1–2 |
| `ortho/data/snow/raw/*.tiff` | stage 2, from `data/snow/aligned/*.png` |
| `ortho/data/terrain_features/*.tif` | GDAL `gdaldem` (slope, aspect, TPI; TRI and roughness are present but deliberately unused) and SAGA GIS via QGIS (TWI), applied to the GSI 5 m mesh DEM — which is `tateyamadem_small.tif` |
| `data_external/veg_murodo.gpkg` | clip of the Ministry of the Environment 1:25,000 vegetation map |

Large binaries are excluded from git (see `.gitignore`) and are distributed through the
Zenodo deposit instead:

- `ortho/data/`: `georectified.csv` (image-to-world lookup table, 744 MB),
  `pointcloud.db` (SfM point cloud behind the camera model), `tateyama2.tiff` (aerial
  orthophoto used for ground control), `mrd_dem_1m.tiff`, `georectified.tiff`,
  `dem_small.tiff`, and the `*.tiff` inputs above
- `analysis/out/models/*.rds` — fitted Model A ensembles and Model B GAMs (1.1 GB)
- `analysis/out/*.tif` — predictor stack and projection rasters
- `results/*.npy` — image-space classifier output
- `data_external/veg2024bk4.gpkg` — the un-clipped source of `veg_murodo.gpkg`

Zenodo concept DOI: https://doi.org/10.5281/zenodo.22745234 (resolves to the latest version).

The Zenodo record stores these files as a flat list (directory structure cannot be
preserved there). File names are unique across the repository, so each file can be
placed back at the path given in the list above before re-running the pipeline.

## Software

- **R 4.5.2** — terra, sf, ggplot2, tidyterra, patchwork, dplyr, tidymodels/tidysdm,
  stacks, spatialsample, mgcv, xgboost, ranger, maxnet, DALEX. Exact versions are in the
  `sessionInfo()` blocks at the end of `analysis/out/full_run_*.log`.
- **Python 3 with PyTorch** — the vegetation classifier; **OpenCV** for photograph
  alignment.
- **GDAL** (`gdaldem`) and **SAGA GIS via QGIS** — terrain derivatives.
- **Quarto** — manuscript rendering.

## Licence

GNU General Public License v3 — see `LICENSE`.

Not tracked in git (available from the Zenodo deposit): `analysis/out/*.tif` (predictor stack, distance surface, suitability, establishment-probability and projection rasters), `review/image_interpretation_package/{crops,crops_native,blind_overlay}/` (image crops used in the blind interpretation), and all `*.tiff`, `*.rds`, `*.npy`, `*.db` inputs and fitted models listed above.
