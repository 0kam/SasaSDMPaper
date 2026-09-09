# Patch spec: Model B robustness companion subsampling + kernel k-sensitivity

Context: the full (non-smoke) run of `analysis/04_model_B.R` stalled in the tidysdm
robustness companion. maxnet/glmnet cannot handle the full 1.2M-row table: one candidate
died at the 24 GB vector memory limit, several hit glmnet convergence failures
(error codes -180/-188, "Model may be infeasible"), and a single tuning entry ran > 7 h.
The primary GAM (`mgcv::bam`, discrete = TRUE) is fine on the full table — do not touch
its data.

## Change 1: subsample the companion, and only the companion

In `analysis/04_model_B.R`, build the robustness companion's data as:

- ALL positive rows (colonized == 1), plus
- a stratified sample of negatives — stratified by shared fold id — of
  `N_COMPANION_NEG` rows total (put the constant in `00_config.R`;
  default 150,000 in full mode; in SMOKE mode keep current behaviour).
- Seeded (`SEED_MODEL_B`), reproducible, drawn AFTER the eligible-cell table is built so
  the GAM and the companion share identical candidate rows.
- The per-fold rset for the companion must still come from the shared folds
  (`make_shared_rset`) restricted to the sampled rows.
- Write the sampling description (n positives, n negatives per fold, seed) to
  `analysis/out/model_B_companion_sampling.csv`.
- Add a one-line comment stating WHY (maxnet memory/convergence at 1.2M rows; AUC is
  prevalence-insensitive so background subsampling does not bias the companion's
  purpose).

The blocked-metrics CSV keeps its current schema (gam_* columns from the full-data GAM,
tidysdm_* columns now from the subsampled companion). Add a `companion_n` column so the
difference in data volume is visible in the artifact.

## Change 2: dispersal-kernel k sensitivity

Still in `04_model_B.R`: after fitting the primary GAM (k = 3 on `s(dist12)`), fit two
sensitivity GAMs identical except `s(dist12, k = 5)` and `s(dist12, k = 10)` on the SAME
data, and extend the kernel export so
`analysis/out/model_B_distance_kernel.csv` has columns `dist12, p9_k3, p9_k5, p9_k10`
(+ the frozen median predictors). Save the two sensitivity models under
`analysis/out/models/` too. The inference model and `p9_colonization.tif` remain k = 3.

Update `analysis/tests/test_wp3.R`:
- kernel monotonicity check now applies to `p9_k3` (rename-compatible: accept `p9` or
  `p9_k3`)
- new check: the three kernels' Spearman correlation over 0-200 m is > 0.95 pairwise
  (they should tell the same story; if not, the test prints the value and still passes
  with a prominent warning — this is a report, not a gate)
- new check: companion sampling CSV exists and n_positive equals the full-data positive
  count.

## Verify

Run `SASA_SMOKE=1 Rscript analysis/04_model_B.R` and
`SASA_SMOKE=1 Rscript analysis/tests/test_wp3.R`; both must pass. Do NOT run full mode.
Print a diff-style summary of what you changed and the smoke results.
