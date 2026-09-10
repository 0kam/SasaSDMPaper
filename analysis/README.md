# analysis/

The analysis pipeline behind the revised manuscript. Every number, table and figure in
the paper and the Supporting Information comes from here.

Run from the repository root with `Rscript analysis/NN_*.R`, in numerical order. See the
top-level `README.md` for the environment variables (`SASA_REPO_ROOT`, `SASA_SMOKE`,
`SASA_CA_PARALLEL_BACKEND`) and for the acceptance-test invocations.

| File | Role |
|---|---|
| `00_config.R` | Sourced by every script and test: the single hardcoded path, all constants, all seeds, the reference-grid assertions, and the shared helpers. Also carries the verified reference values that `tests/` check against |
| `01_predictors.R` | Snowmelt climatology and per-pixel OLS trend from `ortho/data/snow/raw/`; terrain layers reprojected onto the vegetation grid; 7-layer predictor stack plus Spearman/VIF diagnostics |
| `02_folds_and_distance.R` | Distance-to-2012-*Sasa* surface and the shared spatial cross-validation blocks |
| `03_model_A.R` | Model A — environmental potential, a stacked ensemble |
| `04_model_B.R` | Model B — nine-year establishment GAM, with the log-distance kernel comparison that decides which fit is used downstream |
| `05_ca_projection.R` | Annual cellular-automaton projection 2021–2030, plus hindcast, spatial-block and sensitivity runs |
| `06_snowmelt_stats.R` | Snowmelt trend statistics (per-pixel t tests, BH correction, landscape-scale regression) |
| `07_risky_areas.R` | 2030 risky areas and their composition in the MOE vegetation map |
| `08_manuscript_figures.R` | Builds the manuscript and SI figures into `paper/files/` |

| Directory | Contents |
|---|---|
| `R/` | Shared helpers: `distance.R`, `model_utils.R` |
| `specs/` | Work-package specifications written before implementation (WP2/WP3, WP4, WP5, and the four numbered patches) |
| `tests/` | Acceptance tests, one per work package; each exits non-zero on failure |
| `figures/` | Per-figure scripts and shared styling (`fig_common.R`, `figure_recipes.R`), plus `figure_revision_report.md` |
| `out/` | All outputs: CSV summaries, GeoTIFFs, PNG diagnostics, fitted models under `out/models/`, and the `full_run_*.log` files that carry the seeds and `sessionInfo()` |

Large outputs (`out/models/*.rds`, `out/*.tif`) are not in git; they are part of the
Zenodo deposit.

Note: `out/model_B_blocked_metrics.csv` was written before the log-distance kernel was
adopted and therefore reports the linear-distance specification (AUC 0.9365).
