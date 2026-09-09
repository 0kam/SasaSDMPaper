# Patch spec 4: log-distance smooth for the colonization GAM — test, judge, adopt or reject

Background: the fitted `s(dist12, k = 3)` under-predicts hindcast colonization in the
[0,5) m band (simulated frequency 0.057 vs observed 0.095) and over-predicts in
[5,20) m. Aggregate calibration is fine (hindcast ratio 1.009). Hypothesis: on the
linear distance axis the stiff smooth cannot represent the sharp decay at the front;
`s(log1p(dist12))` concentrates resolution where the data are.

## Do

1. In `analysis/04_model_B.R`, add a VARIANT fit: identical to the primary GAM but with
   `s(log1p_dist12, k = 5)` replacing `s(dist12, k = 3)` (precompute the transformed
   column; keep every other smooth unchanged). Fit + leave-one-shared-block-out exactly
   like the primary. Save as `model_B_gam_logdist.rds`.
2. Judgement artifacts, written to `analysis/out/model_B_logdist_comparison.csv`:
   - blocked AUC/TSS per fold and pooled, primary vs logdist
   - hindcast-style distance-band table for BOTH models evaluated at observed 2012→2021
     transitions (bands as in ca_hindcast_band_comparison.csv): observed rate, predicted
     mean p9 (converted to 9-yr scale is already p9) per band
   - kernel export extended: column `p9_logdist` on the same 0–400 m grid
3. Decision rule (implement as printed verdict, not silent adoption): ADOPT logdist iff
   (a) pooled blocked AUC does not degrade by more than 0.005, AND
   (b) the [0,5) band absolute error |pred − obs| improves by ≥ 30%, AND
   (c) the [5,20) bands do not get worse in aggregate.
   Print the three criteria with numbers and the verdict ADOPT/REJECT.
4. If ADOPT: switch the inference model used downstream (p9_colonization.tif and the
   objects 05/07 read) to the logdist model, rerun the SMOKE paths of 05 and 07 to
   confirm compatibility, and leave full reruns to the reviewer. If REJECT: change
   nothing downstream; the comparison CSV itself is the supplement material.
5. Update `test_wp3.R`: comparison CSV exists; verdict line present; if ADOPT, the
   monotonicity check applies to the adopted kernel.

Run SMOKE for 04 + test_wp3 and print: the three criteria, the verdict, files created,
deviations. Do NOT run full mode (reviewer does).
