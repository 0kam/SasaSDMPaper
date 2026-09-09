# Patch spec 2: drop maxent from the Model B robustness companion

Context: even after subsampling to ~154k rows (PATCH_wp3_companion.md), maxnet/glmnet
remains infeasible on this response: colonization is near-separable on dist12, so the
glmnet regularization path fails (error codes -180/-184/-188; ~79% of data-stack rows
were dropped as NA OOF predictions during blending; the final `fit_members()` refit
died with "glmnet failed to complete regularization path. Model may be infeasible.",
killing the full run at 13:23). This is a property of the data (quasi-separation), not
of the sample size.

## Change (in `analysis/04_model_B.R` only, plus test alignment)

1. Remove maxent from the robustness companion's workflow_set. The companion becomes
   XGBoost only (`tidysdm::sdm_spec_boost_tree`, engine xgboost, existing grid/threads).
   Keep the companion's subsampling exactly as implemented.
2. Replace the removal with a comment block stating the reason: near-separation on
   dist12 makes the maxnet/glmnet regularization path fail even at 1.5e5 rows (observed:
   convergence errors -180/-184/-188, 79% NA OOF rows at blending, fatal error in
   fit_members). This comment is source material for the Methods, keep it precise.
3. Keep the artifact schema: `model_B_blocked_metrics.csv` keeps its `tidysdm_*`
   columns (now XGBoost-stack based) and `companion_n`. If any saved-object filename
   contains "maxent", adjust it and keep names truthful
   (e.g. model_B_robustness_stack.rds unchanged is fine).
4. `analysis/tests/test_wp3.R`: no schema change expected; verify nothing references
   maxent. Keep all existing checks.

## Verify

`SASA_SMOKE=1 Rscript analysis/04_model_B.R` and
`SASA_SMOKE=1 Rscript analysis/tests/test_wp3.R` must pass. Do NOT run full mode.
End with a short diff summary and the smoke results.
