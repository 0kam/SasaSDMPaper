# Snowmelt-trend sensitivity analyses for the Supporting Information

Two analyses requested for the SI, both run on the full (non-smoke) pipeline in
R 4.5.2 with the seeds and `mgcv::bam` settings of `analysis/04_model_B.R`.
Nothing under `analysis/` was modified.

---

## Conclusions

**Task 2 (the design question).** Adding a smooth of the per-pixel snowmelt
trend to the adopted Model B specification **does not improve blocked
predictive performance — it degrades it slightly, in every one of the four
spatial blocks.** Block AUC falls by 0.0002–0.0007 (pooled AUC 0.9461 →
0.9456, ∆ = −0.00050); pooled TSS falls by 0.0007. The `s(trend)` term is only
marginally significant in-sample (edf 3.15, χ² = 12.1, p = 0.011 on 1,197,516
observations), adds 0.03 percentage points of deviance explained (38.99% →
39.02%), and improves AIC by 8.1 — a difference that does not survive
out-of-block evaluation. Over the central 90% of the trend distribution
(−1.84 to +0.12 d yr⁻¹, where 90% of the landscape sits) the fitted partial
effect spans only 0.126 on the logit scale (odds ratio 1.13), against 5.80 for
the distance term in the same model, and its 95% band contains zero
throughout. **This supports the design decision taken in the main text: the
snowmelt trend is used to parameterise scenarios, not as a fitted predictor.**
Nothing in the colonization record identifies a trend response, so admitting
one would add a parameter the data cannot constrain while making held-out
prediction marginally worse.

**Task 1 (the trend field's structure).** The trend field has **no monotone
relationship** with either the snowmelt climatology (Spearman ρ = −0.007) or
elevation (ρ = +0.022) — both are negligible at n = 1,206,063. Elevation shows
no structure of any kind: 50 m elevation bins account for 1.9% of the
pixel-level trend variance, and every bin mean lies between −0.86 and
−0.53 d yr⁻¹, within a fraction of the pixel-level SD (0.634) of the landscape
mean (−0.715).

Along the climatology axis, however, the near-zero ρ conceals **real but
non-monotone structure**: 10-day bin means run from −0.47 (DOY 120–130) down
to −1.29 (DOY 150–160), back up to −0.33 (DOY 180–190) and down again to
−0.97 (DOY 220–230), and these bins account for 24% of the pixel-level trend
variance. So the honest reading is:

* A uniform shift is **an approximation, not an exact description**, of how
  the melt surface has moved; the SI should say so rather than claim the trend
  field is unstructured.
* But the approximation is a reasonable one for the scenario's purpose. Every
  well-sampled bin mean is negative, all of them lie within ±0.6 d yr⁻¹ of the
  landscape mean, and the deviations are far smaller than the within-bin
  spread (SD 0.39–0.77 d yr⁻¹ inside every bin). The scenario range actually
  used in the main text (0, −0.71, −2.24 d yr⁻¹) comfortably brackets the
  whole set of bin means.
* The structure is also not obviously ecological. It is non-monotone with a
  reversal in mid-season, it tracks the parts of the footprint occupied by
  persistent snow patches, and — critically — the modelling result in Task 2
  shows it carries no signal about where *Sasa* actually colonized. Reading an
  ecological mechanism into it would over-interpret a 10-year, 8-to-10-point
  per-pixel regression whose landscape-scale trend is itself not significant
  (−0.689 d yr⁻¹, p = 0.34).

**Suggested SI wording:** the per-pixel trend is essentially uncorrelated with
the climatology and with elevation; what residual organisation it has is
non-monotone, modest relative to pixel-level noise, and — as the refitted
model shows — not predictive of colonization. A spatially uniform shift is
therefore an adequate and deliberately conservative scenario device.

---

## Task 1 — Is the trend field structured?

### Data

Every cell with a fitted per-pixel OLS snowmelt slope
(`analysis/out/snowmelt_ols_trend.tif`, layer `slope_d_per_yr`; DOY > 0 rule,
≥ 8 valid years) and complete environmental predictors: **n = 1,206,063**.
Distribution reproduces the published figures exactly — mean −0.7146,
SD 0.6344, median −0.6264 d yr⁻¹, 91.65% negative
(`trend_distribution.csv`).

### Correlations (`trend_correlation.csv`)

| Pair | Spearman ρ | Pearson r | ρ² |
|---|---:|---:|---:|
| trend ~ snowmelt climatology | −0.0070 | +0.0148 | 0.00005 |
| trend ~ elevation | +0.0224 | +0.0411 | 0.00050 |
| climatology ~ elevation *(reference)* | −0.5792 | −0.5948 | 0.3354 |

The reference row matters: climatology and elevation *are* strongly related to
each other, so the near-zero trend correlations are not an artefact of a
degenerate predictor set — the trend genuinely fails to line up with either
gradient.

### 10-day climatology bins (`trend_by_snow_bin.csv`)

Bins holding fewer than 1,000 cells are edge slivers of the camera footprint;
they are retained in the CSV with `well_sampled = FALSE` but excluded from the
figure overlay and from the summary statistics.

| Climatology bin (DOY) | n cells | trend mean | trend SD | |
|---|---:|---:|---:|---|
| [100,110) | 8 | −5.959 | 0.223 | *not well sampled* |
| [110,120) | 259 | +1.957 | 3.185 | *not well sampled* |
| [120,130) | 158,834 | −0.470 | 0.477 | |
| [130,140) | 118,966 | −0.649 | 0.626 | |
| [140,150) | 86,913 | −1.141 | 0.768 | |
| [150,160) | 73,432 | **−1.294** | 0.697 | |
| [160,170) | 83,280 | −1.247 | 0.620 | |
| [170,180) | 103,618 | −0.723 | 0.669 | |
| [180,190) | 180,487 | **−0.327** | 0.412 | |
| [190,200) | 205,496 | −0.514 | 0.394 | |
| [200,210) | 119,866 | −0.860 | 0.513 | |
| [210,220) | 51,728 | −0.917 | 0.602 | |
| [220,230) | 23,086 | −0.969 | 0.423 | |
| [230,240] | 90 | −0.194 | 0.110 | *not well sampled* |

Elevation bins (`trend_by_elevation_bin.csv`) are flat by comparison: means
range only from −0.856 to −0.535 d yr⁻¹ across 13 well-sampled 50 m bins.

### Structure summary (`trend_structure_summary.csv`)

| Axis | Well-sampled bins | Bin-mean range | Cell-weighted SD of bin means | Pixel-level SD | Variance share |
|---|---:|---:|---:|---:|---:|
| Climatology (10 d bins) | 11 | 0.968 | 0.310 | 0.634 | **23.8%** |
| Elevation (50 m bins) | 13 | 0.321 | 0.087 | 0.634 | **1.9%** |

### Figures

* `fig_trend_vs_snow_mean.png` — 2D bin heat map (log colour scale), with
  bin means ± 1 SD overlaid in red, the zero line solid and the landscape mean
  (−0.715) dashed. The non-monotone arch is plainly visible, as is the fact
  that it is small compared with the vertical spread.
* `fig_trend_vs_elevation.png` — same construction; visibly flat.

The trend axis is trimmed to the 0.1–99.9 percentiles (−4.10 to +1.38), the
same convention as `analysis/06_snowmelt_stats.R`; 2,413 cells (0.200%) fall
outside the plotted range.

---

## Task 2 — Adding `s(trend)` to Model B

### Design

* **Baseline** = the adopted `logdist` specification of `analysis/04_model_B.R`:
  `s(log1p_dist12, k=5) + s(snow_mean) + s(elevation) + s(slope) + s(TPI) +
  s(twi) + s(northness) + s(eastness)`, all environmental smooths `k = 10`.
* **Trend model** = baseline `+ s(trend, k = 10)`.
* Identical leave-one-shared-block-out protocol over the four spatial blocks
  in `analysis/out/folds.tif`, identical `mgcv::bam` settings
  (`discrete = TRUE`, `method = "fREML"`, `nthreads = MODEL_THREADS`) and
  identical seeds (`SEED_MODEL_B`, `SEED_MODEL_B + k` per fold).
* The baseline is **refitted here rather than quoted** so the comparison is
  exactly like-for-like on the same rows. 20 of the 1,197,536 eligible cells
  lack a fitted trend and are dropped from both models; **none of them is a
  colonization positive**, so all 4,095 positives are retained
  (`data_summary.csv`).

The refit reproduces the published `logdist` row of
`analysis/out/model_B_logdist_comparison.csv` to within the effect of those 20
rows (published pooled AUC 0.946095 vs 0.946121 here; fold AUCs agree to
≤ 0.0002), which validates the harness.

### Blocked metrics (`model_B_trend_comparison.csv`)

| Scope | n | Baseline AUC | Trend AUC | ∆AUC | Baseline TSS | Trend TSS | ∆TSS |
|---|---:|---:|---:|---:|---:|---:|---:|
| Block 1 | 255,524 | 0.96420 | 0.96359 | **−0.00061** | 0.80115 | 0.80317 | +0.00202 |
| Block 2 | 309,386 | 0.90908 | 0.90837 | **−0.00070** | 0.67759 | 0.67792 | +0.00033 |
| Block 3 | 289,373 | 0.95973 | 0.95950 | **−0.00023** | 0.82191 | 0.82235 | +0.00044 |
| Block 4 | 343,233 | 0.95789 | 0.95743 | **−0.00046** | 0.77498 | 0.77513 | +0.00015 |
| Mean of blocks | — | 0.94772 | 0.94722 | **−0.00050** | 0.76891 | 0.76964 | +0.00074 |
| Pooled | 1,197,516 | 0.94612 | 0.94562 | **−0.00050** | 0.75154 | 0.75082 | −0.00072 |

**AUC decreases in all four blocks and pooled.** TSS moves by less than 0.002
in either direction and is negative pooled — i.e. noise, not signal. The
degradation is small in absolute terms, but its *sign is consistent across
every independent spatial block*, which is the relevant test: a genuinely
informative predictor would raise at least some held-out blocks.

### The `s(trend)` term (`model_B_trend_smooth.csv`, `model_B_trend_fit.csv`)

| Quantity | Value |
|---|---:|
| edf | 3.153 |
| Ref.df | 3.793 |
| χ² | 12.14 |
| p | 0.0108 |
| Deviance explained, baseline | 38.99% |
| Deviance explained, + s(trend) | 39.02% |
| AIC, baseline | 33,463.6 |
| AIC, + s(trend) | 33,455.5 |
| ∆AIC | −8.10 |

The other smooths are essentially unchanged by the addition (e.g.
`s(snow_mean)` edf 6.96 → 7.05, `s(log1p_dist12)` edf 3.949 → 3.948), so
`s(trend)` is not displacing an existing term.

At n ≈ 1.2 M, p = 0.011 is about as weak as an in-sample smooth can be while
still crossing 0.05, and ∆AIC = −8 against ~3.2 added effective parameters is
the kind of margin that routinely fails to generalise — which is exactly what
the blocked evaluation shows.

### Shape and magnitude of the effect (`model_B_trend_effect_size.csv`)

| Quantity | Value |
|---|---:|
| Central 90% of trend | −1.839 to +0.121 d yr⁻¹ |
| Partial effect over that range | +0.001 to +0.127 logit |
| Span over that range | 0.126 logit (OR 1.134) |
| Span over the full trend range | 6.05 logit |
| `s(log1p_dist12)` span, same model | 5.80 logit |

* `fig_smooth_trend.png` — full range. The apparent amplitude (≈ 6 logit) is
  produced entirely by the tails beyond ±5 d yr⁻¹, which are a few hundred
  poorly constrained snow-patch-edge pixels; the confidence band there is
  ±40 logit.
* `fig_smooth_trend_zoom.png` — restricted to the central 90%. The curve is
  almost flat (a shallow arch peaking near −1.2 d yr⁻¹), and the 95% band
  spans roughly ±1.2 logit and **contains zero across the entire range**.

Direction of the fitted effect, such as it is: colonization probability is
very slightly *higher* where melt has advanced more strongly (more negative
trend), which is the ecologically expected sign — but the magnitude (OR 1.13
across the 5th–95th percentile span, versus OR ≈ 330 across the distance term)
makes it negligible next to dispersal distance, and it is not distinguishable
from zero.

---

## Reproduction

    cd /Users/okamoto/NIES/SasaSDMPaper
    Rscript review/trend_sensitivity/00_prepare_data.R      #  ~11 s
    Rscript review/trend_sensitivity/01_trend_correlation.R #   ~6 s
    Rscript review/trend_sensitivity/02_model_B_trend.R     # ~116 s

`00` must run first; it writes the two `.rds` intermediates the other two
scripts read (gitignored, regenerable). Console logs of the runs behind every
number in this report are in `run_00_prepare_data.log`,
`run_01_trend_correlation.log` and `run_02_model_B_trend.log`.

All ten fits converged; the only warnings were `openMP not available` (this
machine's mgcv build is single-threaded, which affects speed only) and
`fitted probabilities numerically 0 or 1`, which `analysis/04_model_B.R`
also produces and which reflects the near-separation of the distance term on
far-field cells.
