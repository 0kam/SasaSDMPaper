# Figure revision report — 2026-09-09

All requested figures were regenerated successfully from saved results. Main figures are exported directly at 170 mm width. S1/S2/S5/S6/S7 are 140 mm wide; each S3/S4 panel is 70 mm wide. All PNGs are 300 dpi. Text is at least 9 pt at these final widths (new S3/S4 axis/legend text: 9.5 pt).

## Scripts and changes

| Script | Change | Outputs |
|---|---|---|
| `analysis/figures/fig_common.R` | Shared labels use establishment/established and days/year; original data identifiers and filenames retained. | Shared by Figs. 4–6. |
| `analysis/figures/fig03_response_curves.R` | 85% peak threshold; computed range with en dash; peak message; 170 mm export and wrapped strips. | `paper/files/fig03_response_curves.{png,pdf}` |
| `analysis/figures/fig04_potential_vs_establishment.R` | Establishment labels; 170 mm export; fewer coordinate ticks and wrapped axis title. | `paper/files/fig04_potential_vs_establishment.{png,pdf}` |
| `analysis/figures/fig05_projection.R` | Establishment and days/year labels; 170 mm export; wrapped titles and adjusted colourbar/ticks. Unicode m² retains the full label font size. | `paper/files/fig05_projection.{png,pdf}` |
| `analysis/figures/fig06_risk_communities.R` | Establishment and days/year labels; 170 mm export; wrapped title, stacked scenario legend and spaced ticks/annotations. | `paper/files/fig06_risk_communities.{png,pdf}` |
| `analysis/figures/figS_maps.R` | Inner x ticks and right margin; colourbar title above the bar with spacing. Preserve the regular snow raster grid; draw sparse seed cells with explicit cell dimensions. | S2 and S5 PNGs below. |
| `analysis/06_snowmelt_stats.R` | Factored plotting into `figS_snowmelt_trends.R`; added `--figures-only` to bypass all regressions and CSV writes. | S1 and S3 PNGs below, also written to `analysis/out/`. |
| `analysis/08_manuscript_figures.R` | Added `--snowmelt-only`; uses the same plotting code and copies PNGs to `paper/files/si/`. | Same S1/S3 outputs. |
| `analysis/figures/figS_snowmelt_trends.R` (new) | Saved TIF/CSV plotting; reconstructs the landscape line/CI from saved coefficients and annual means; final-size fonts, same-height S3 canvases. | Same S1/S3 outputs. |
| `review/trend_sensitivity/03_replot_si_figures.R` (new) | Saved CSV partial effect and summaries; saved `trend_domain.rds` supplies 2D cell counts unavailable from marginal CSV summaries. No GAM loading/refitting; original effect/ribbon calculation retained. | S4 PNGs below, written directly to `paper/files/si/`. |
| `analysis/figures/figS5_vegmaps.R` | Height 225 → 190 mm; all Sasa cells drawn on top, with dark outlines around patches >5 m² using the existing four-neighbour/area rule; shared legend below. | S6 PNG below. |
| `analysis/figures/figS6_adjacency_map.R` | Search 150 m windows on a 5 m grid; choose maximum transition-cell count; dark cell borders and white-backed scale bar. | S7 PNG below. |

## Output dimensions

All paths in this table are relative to `paper/files/`. PNG dimensions were checked from the files; PDF page text was checked for clipping and small fonts.

| Output | Size (width × height, mm) | PNG pixels |
|---|---:|---:|
| `fig03_response_curves.png`, `.pdf` | 170 × 118 | 2007 × 1393 |
| `fig04_potential_vs_establishment.png`, `.pdf` | 170 × 244 | 2007 × 2881 |
| `fig05_projection.png`, `.pdf` | 170 × 244 | 2007 × 2881 |
| `fig06_risk_communities.png`, `.pdf` | 170 × 112 | 2007 × 1322 |
| `si/fig_snowmelt_annual_mean.png` | 140 × 90 | 1653 × 1062 |
| `si/figS2_snowmelt_mean_map.png` | 140 × 130 | 1653 × 1535 |
| `si/fig_snowmelt_slope_map.png` | 70 × 85 | 826 × 1003 |
| `si/fig_snowmelt_slope_hist.png` | 70 × 85 | 826 × 1003 |
| `si/fig_trend_vs_snow_mean.png` | 70 × 85 | 826 × 1003 |
| `si/fig_smooth_trend_zoom.png` | 70 × 85 | 826 × 1003 |
| `si/figS7_seed_layer.png` | 140 × 130 | 1653 × 1535 |
| `si/figS5_vegmaps.png` | 140 × 190 | 1653 × 2244 |
| `si/figS6_adjacency_map.png` | 140 × 150 | 1653 × 1771 |

The three `fig_snowmelt_*.png` files in `analysis/out/` have the same dimensions and are byte-identical to their SI copies.

## Exact messages for captions

```text
Snowmelt window (>= 85% of peak suitability): DOY 151-184
Snowmelt peak: DOY 172 (partial-dependence suitability 0.24091108)
Window: 150 x 150 m; easting 733330–733480; northing 4051155–4051305
Transition cells: 218 loss-to-pine in window
```

## Execution and verification

Executed from the repository root with `Rscript <script>`:

- `analysis/figures/fig03_response_curves.R`
- `analysis/figures/fig04_potential_vs_establishment.R`
- `analysis/figures/fig05_projection.R`
- `analysis/figures/fig06_risk_communities.R`
- `analysis/figures/figS_maps.R`
- `analysis/06_snowmelt_stats.R --figures-only`
- `analysis/08_manuscript_figures.R --snowmelt-only`
- `review/trend_sensitivity/03_replot_si_figures.R`
- `analysis/figures/figS5_vegmaps.R`
- `analysis/figures/figS6_adjacency_map.R`

All exited successfully. No requested plot remains blocked. The original sensitivity scripts `01_trend_correlation.R` and `02_model_B_trend.R` were intentionally not run or modified; the new script replaces only their requested SI rendering. Full analysis modes of 06/08 were not run. A pre-existing sf warning about spatially constant attributes during clipping in Fig. 5 is non-fatal.

Rendered PNGs were visually inspected. PDF text extraction found no text outside the page and no fonts below 9 pt. Existing Fig. 5/6 manuscript consistency checks passed; S7 independently asserts its crop count equals the search maximum. The snowmelt plotting code checks cached cell count and mean slope against saved statistics. SHA-256 checks confirmed the 49 protected CSV/TIF/model-script files checked at task start were unchanged. No `.qmd` files were edited by this revision task.
