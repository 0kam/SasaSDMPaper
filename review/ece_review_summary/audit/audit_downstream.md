# Audit — downstream analysis and figure generation

Scope: `scripts/sdm/analyse_sdm.R`, `scripts/sdm/plot_snowmelt_shifts_map.R`,
`scripts/sdm/plot_vegetation_map.R`, the plotting blocks of `scripts/sdm/sdm_tbm.R`
and `scripts/sdm/sdm_tdm.R`; artefacts in `ortho/figures/`, `ortho/*.png`,
`results/*.png`, `paper/files/`, `paper/files_original_size/`.

All commands below were run in `/Users/okamoto/NIES/SasaSDMPaper` (the main repo, where
the gitignored data lives). Nothing was modified.

Convention used throughout:
- **(a)** what the committed code would do on a clean run
- **(b)** what the archived artefacts show actually happened
- **(c)** what the manuscript says

---

## 1. Provenance map of the nine manuscript figures

Figure order taken from `paper/index.tex` (`\includegraphics` calls, lines 393, 484,
595, 695, 721, 745, 768, 799, 826) and the corresponding `.qmd` chunks.

| # | Label | File in `paper/files/` | Composed in | Panel inputs | Producing code | Model |
|---|-------|------------------------|-------------|--------------|----------------|-------|
| 1 | `fig-overview` | `overview.eps` / `.jpg` | — (single image) | — | **NONE — hand-made diagram** | — |
| 2 | `fig-vege12-21` | `vegemap.eps` / `.jpg` | `matmet.qmd` `image_append()` | `2012_5x5_en.jpg` + `2021_5x5_en.jpg` | **NONE for the `_en` versions**; `plot_vegetation_map.R` writes Japanese-labelled `ortho/data/2012_5x5.png` / `2021_5x5.png`, which do not exist on disk | classification only |
| 3 | `fig-spatial-split` | `initial_split.eps` / `.jpg` | `matmet.qmd` `image_append()` | `initial_split_dist.jpg` + `cv_dist.jpg` | `sdm_tdm.R` L80-84 and L99-103 | **TDM only** |
| 4 | `fig-sasa-expansion` | `expanded_area.eps` (`.pdf` in `.qmd`) | — | — | **NONE — hand-made composite** on a 地理院地図 (GSI) basemap; the vestigial `sdm_tbm.R` L454-470 (`p_inc`, Japanese labels) does not produce it | classification only |
| 5 | `fig-hsm-performance` | `model_performance.eps` / `.jpg` | `results.qmd` L41-44 | `model_performance_tbm.jpg` + `model_performance_tdm.jpg` | `sdm_tbm.R` L130-139 (from `models_wo_dist_twi.rds`, **which does not exist**; the archived PNG is reproducible from `models_wo_dist.rds`), `sdm_tdm.R` L142-151 (`models.rds`) | TBM + TDM |
| 6 | `fig-variable-importance` | `vi.eps` / `.jpg` | `results.qmd` L60-63 | `vi_tbm.jpg` + `vi_tdm.jpg` | `sdm_tbm.R` L192-218, `sdm_tdm.R` L202-227 — **but the paper's PNGs differ from `ortho/figures/vi_*.png`** (see §4.6) | TBM + TDM |
| 7 | `fig-hs-maps` | `hsmap_2021.eps` / `.jpg` | `results.qmd` L78-81 | `hsmap_tbm_2021.jpg` + `hsmap_tdm_2021.jpg` | `sdm_tbm.R` L252-277, `sdm_tdm.R` L282-307 (**re-run with extra `ggspatial` layers not in the committed code**) | TBM + TDM |
| 8 | `fig-future-prediction` | `future.eps` / `.jpg` | `results.qmd` L98-104 | `hsmap_tbm_2021.jpg` + `hsmap_tdm_2021.jpg` (top) and `hsdiff_tbm.jpg` + `hsdiff_tdm.jpg` (bottom) | as above + `sdm_tbm.R` L309-334, `sdm_tdm.R` L339-364 | TBM + TDM |
| 9 | `fig-risky-map` | `risky_tbm.eps` / `.jpg` | — | — | `sdm_tbm.R` L336-380 | **TBM** |

Supplementary: S1 `snowmelt_shifting.jpg` ← `preprocess_snow_data.R` L227;
S2 `snowmelt_doy.jpg` ← `plot_snowmelt_shifts_map.R` L35; S3 `snowmelt_shift_map.jpg`
← `plot_snowmelt_shifts_map.R` L66. S2 and S3 are byte-identical to `ortho/*.png`
(md5 match); S1 is **not** (`ortho/snowmelt_shifting.png` 70,179 B vs
`paper/files_original_size/snowmelt_shifting.png` 83,877 B).

### 1.1 Manuscript figures with no identifiable producing code

- **Figure 1 (`overview`)** — assembled by hand (PowerPoint/Illustrator). Embeds a
  crop of the classification map with a large unexplained black region (the
  out-of-camera-view mask) and a thumbnail of `risky_tbm.png`.
- **Figure 2 panels (`2012_5x5_en.png`, `2021_5x5_en.png`)** — the only vegetation-map
  script in the repo, `plot_vegetation_map.R`, hard-codes Japanese class names
  (`"ササ類"`, `"その他植生"`, …, L17-25, L32-41) and writes to
  `ortho/data/2012_5x5.png`. That path does not exist, and the manuscript panels are
  in English with a scale bar and north arrow (ggspatial) that the script does not add.
  The English version of the script is gone.
- **Figure 4 (`expanded_area.pdf`)** — a manual composite: GSI 地理院地図 raster
  basemap + orthophoto + green/yellow overlays + a magnified inset + two callout
  boxes. No code, and the basemap is third-party content that will need an
  attribution/licence statement for publication.

### 1.2 Generated figures that are never used

In `paper/files/` but not referenced by any `.qmd`:
`hsmap_tbm_2030.jpg`, `hsmap_tdm_2030.jpg`, `risky_tdm.jpg`,
`initial_split_wo_dist.jpg`, `cv_wo_dist.jpg` (and their `.eps` twins).

Elsewhere: `results/2012_masked.png`, `results/2021_masked.png`, `results/teacher.png`,
`results/cv.png`, `ortho/data/matched.png`.

Orphan rasters with no producing code anywhere in the repo:
`ortho/risky_area.tiff`, `ortho/potential_sasa_area_21.tiff`,
`ortho/data/sasa_pred_sdm_12.tiff`, `ortho/data/sasa_pred_sdm_dist_21.tiff`,
`ortho/data/sasa_pred_sdm_dist_30.tiff`, `ortho/data/selected_comms.tiff`,
`ortho/data/sasa_inc.tiff`, `ortho/models_all_5m.rds`.

---

## 2. CRITICAL: Figure 8 does not show the 2030 maps

`results.qmd` L98-99 builds the top row of Figure 8 from the **2021** maps:

```r
hsmap <- c(image_read("files/hsmap_tbm_2021.jpg"), image_read("files/hsmap_tdm_2021.jpg"))
```

The caption (`results.qmd` L94-95, `index.tex` L803) says
"Habitat suitability (HS) maps for **2030** and differences from the 2021 HS maps."
The rendered panel titles in `future.jpg` literally read
`Habitat Suitability map of Sasa (TBM, 2021)` and `(TDM, 2021)`.

Proof that the top half of Figure 8 is pixel-identical to the whole of Figure 7:

```
$ magick paper/files/future.jpg -crop 2400x960+0+0 +repage future_top.png
$ magick compare -metric RMSE future_top.png paper/files/hsmap_2021.jpg null:
0 (0)

# for contrast, against a correctly-built 2030 composite:
$ magick paper/files/hsmap_tbm_2030.jpg paper/files/hsmap_tdm_2030.jpg +append -resize 2400x true2030.png
$ magick compare -metric RMSE future_top.png true2030.png null:
5464.01 (0.0833755)
```

So Figure 7 is reproduced verbatim as the top half of Figure 8, and the actual 2030
suitability maps — which exist as `hsmap_tbm_2030.jpg` / `hsmap_tdm_2030.jpg` — never
appear in the manuscript. Reviewer 3's comment on Figure 8 ("I suggest one panel for
each model … unsuitable 2021 + suitable 2030 …") was written against a figure that does
not contain the 2030 layer at all.

The **numbers** in the Figure 8 caption are nevertheless correct (§6), so this is a
figure-assembly error, not a computational one — but it is visible to any reader.

---

## 3. Which model the "risky area" figure comes from, and the three risky rasters

**Answer: the manuscript's Figure 9 is the TBM.** `paper/files/risky_tbm.eps` is used
(`index.tex` L826); it derives from `sdm_tbm.R` L336-380, which writes
`ortho/risky_area_wo_dist.tiff` (L384). The Methods (`matmet.qmd`, last paragraph) and
the Results text (`results.qmd` L108) both say TBM. This confirms the prior lead.

### 3.1 Raster inventory

```
                              names          dim         nonNA    min       max
risky_area.tiff               risk        1753x1801     10287   0.20004   0.82340
risky_area_tdm.tiff           pred_sasa_30 1147x1213     3149   0.50007   0.64007
risky_area_wo_dist.tiff       pred_sasa_30 1147x1213    36797   0.50000   0.73797
potential_sasa_area_21.tiff   pred_sasa_21 1753x1801    17674   0.50000   0.92498
```

Geodesic areas (`terra::expanse`): TBM risky = **36,777 m²**, TDM risky = **3,147 m²**.
Neither number appears in the manuscript.

### 3.2 Reproducibility of the two current risky rasters

Both are exactly reproducible from the archived prediction rasters using the code as
written:

```
TBM risky recomputed: 36797   (archived 36797)
TDM risky recomputed: 3149    (archived 3149)
```

(recomputed as `!is.na(p30) & !is.na(p21) & !is.na(sasa21) & sasa21==0 & p30>0.5 & vege21==2`)

The `terra::resample(vege21, .)` at `sdm_tbm.R` L341 / `sdm_tdm.R` L371 is a **no-op**
here because both grids are the identical 1 m lattice — my recomputation used the raw
`vege21` and matched to the pixel. It remains a latent hazard: `resample()` defaults to
bilinear for a non-categorical `SpatRaster`, so if the grids ever differed the
subsequent `filter(vege21 == 2)` would silently drop nearly every cell.

### 3.3 The two orphan rasters

- `risky_area.tiff` — layer named `risk`, values 0.200–0.823, 10,287 px, **full**
  extent. Its values match **none** of the seven archived prediction rasters
  (max |diff| ≥ 0.57 for every candidate; 0 / 10,287 cells matching to 1e-6). It is a
  leftover from an earlier definition using a **0.2** threshold and a model whose
  predictions no longer exist.
- `potential_sasa_area_21.tiff` — `pred_sasa_21`, 0.500–0.925, 17,674 px, 100 %
  `vege21 == 2`, again matching no archived prediction raster. Also orphaned.

Both are on the public GitHub-adjacent tree and are the kind of artefact a reviewer
who "read the code" would trip over.

### 3.4 Definitional drift between code and Methods

Methods (`matmet.qmd`): *"cells classified as 'Other Vegetation' in 2021 whose
TBM-predicted habitat suitability (HS) for 2030 exceeded 0.5"*.

Code adds two conditions the Methods do not state: `sasa == 0` (redundant given
`vege21 == 2`) and, via `drop_na()`, `pred_sasa_21` must be non-NA. The latter is not
redundant:

```
Manuscript-literal risky (vege21==2 & TBM30>0.5): 37138 px, expanse 37117.79
Code risky (adds pred21 non-NA):                  36797 px
```

a 341-pixel (0.9 %) difference. Small, but the Methods as written do not reproduce the
figure.

---

## 4. Figure-level defects, verified against the actual images

### 4.1 Figure 7 — colour scales differ between panels meant to be compared

`scale_fill_gradient2(low="grey", high="red", mid="lightyellow", midpoint=0.5)` is
applied independently in each script, so the endpoints stretch to each panel's own
data range:

```
TBM2021 : min 0.1601363  max 0.7395326
TDM2021 : min 0.0856436  max 0.6425344
```

Rendered legends read **0.2–0.7 (TBM)** and **0.1–0.6 (TDM)**. Saturated red therefore
means HS 0.74 on the left and HS 0.64 on the right; the darkest grey means 0.16 on the
left and 0.086 on the right. The panels are placed side by side and the caption invites
a direct comparison ("The TBM predicted 47,253 m² … In contrast, the TDM predicted
12,766 m²"). This confirms the prior lead.

The same problem recurs in Figure 8's bottom row:

```
TBM diff : min -0.4597578  max 0.4702161   -> legend -0.25 … 0.25
TDM diff : min -0.5096740  max 0.5279855   -> legend -0.50 … 0.50
```

### 4.2 Figure 7 — the "black areas" Reviewer 3 asks about

They are the `geom_spatvector(data = sasa_pol_21, color = "black", fill = "transparent")`
outlines. At print size the outlines of the many small, elongated patches coalesce into
solid black blobs (verified by cropping and enlarging the TDM panel:
`magick paper/files/hsmap_2021.jpg -crop 500x400+1500+350 +repage -resize 200%`).
They appear in no legend and in no caption. See §5.

Also in Figure 7: the low end of the fill ramp (`"grey"`, ≈#BEBEBE) is nearly the same
value as the default `theme_grey()` panel background (#EBEBEB), so the mapped domain is
hard to separate from off-map area; and the white holes inside the mapped domain are
NA cells (`na.value = "transparent"` over a white device background), unexplained.

### 4.3 Duplicated legends and in-figure headings (R1 minor, R3 "Headings should be removed")

Every composite figure duplicates its legend and its axis titles because the panels are
independently-rendered PNGs stitched with `magick::image_append()`:

- Fig 2: two `Vegetation` legends (incl. an `NA` swatch), two `Longitude`/`Latitude`
  pairs, two north arrows, two scale bars.
- Fig 3: `Class` and `Fold` legends, two axis-label sets, and the two panels have
  **different latitude ranges** (left 36.5725–36.5835, right 36.5725–36.5845).
- Fig 5: two identical `model` legends (GBT/GAM/MaxEnt/RF).
- Fig 6: two y-axis titles.
- Fig 7 / 8: two (four) `Habitat Suitability` legends.

In-figure `title=` strings survive into every panel:
`"Performance of TBM"`, `"Variable importance of TBM"`,
`"Habitat Suitability map of Sasa (TBM, 2021)"`, `"Risky area (TBM)"`, etc.

### 4.4 Figure 5 — different y-axis ranges

TBM panel spans ≈0.35–0.70 TSS, TDM panel ≈0.61–0.74. Side-by-side, the TDM's spread
looks as large as the TBM's although it is 4× narrower.

**Retraction of a plausible lead:** `scale_color_discrete(labels = c("GBT","GAM","MaxEnt","RF"))`
(`sdm_tbm.R` L132, `sdm_tdm.R` L144) is **correct**, not a mislabelling.
`autoplot.workflow_set` colours by the `model` column, not `wflow_id`; alphabetical
order is `boost_tree, gen_additive_mod, maxent, rand_forest`. Verified:

```
        wflow_id  colour  n
1    default_gam #7CAE00  1
2 default_maxent #00BFC4 18
3     default_rf #C77CFF  8
4    default_xgb #F8766D 18
```

so red=xgb→"GBT", olive=gam→"GAM", teal=maxent→"MaxEnt", purple=rf→"RF". Correct.

### 4.5 Figure 4 — Japanese text, callout boxes, unexplained white, inset

Rendered `expanded_area.pdf` at 100 dpi and inspected. It contains, exactly as
Reviewer 3 describes:
- Japanese place names baked into the GSI basemap: 玉殿岩屋, 立山室堂山荘, and the
  attribution 地理院地図.
- A green box "Sasa distribution in 2012" and a yellow box "Expanded Area" with
  hand-drawn leader lines.
- Large white patches inside the orthophoto (residual snow), unexplained.
- A right-hand magnified inset with its own 50 m scale bar.

### 4.6 Figure 6 — the paper's version is a *different run* from the committed code

`ortho/figures/vi_tbm.png` (produced by the committed code) has `ylim(c(0, 0.6))`
applied and predictor order `snow, elevation, TRI, roughness, aspect, slope, TPI`.
`paper/files/vi_tbm.jpg` has **no** y-limit (axis tops out ≈0.26) and order
`snow, elevation, TRI, aspect, roughness, slope, TPI` — `aspect` and `roughness` have
swapped. md5 comparison of every generated/paper pair:

```
hsdiff_tbm       DIFF     hsmap_tbm_2021   DIFF     risky_tbm   DIFF
hsdiff_tdm       DIFF     hsmap_tbm_2030   DIFF     risky_tdm   SAME
hsmap_tdm_2021   DIFF     hsmap_tdm_2030   DIFF     vi_tbm      DIFF
cv_dist          SAME     initial_split_dist     SAME           vi_tdm      DIFF
cv_wo_dist       SAME     initial_split_wo_dist  SAME
model_performance_tbm SAME  model_performance_tdm SAME
```

Visual comparison of `ortho/figures/risky_tbm.png` against `paper/files/risky_tbm.jpg`
shows the paper version carries a **north arrow and a 200 m scale bar** that the
committed `sdm_tbm.R` never adds (no `ggspatial` call anywhere in `scripts/sdm/`).
Conclusion: **every map figure in the manuscript was produced by a later, edited
version of the plotting code that is not in the repository.** `model_performance_*`,
`initial_split_*`, `cv_*` are the only model figures that survive byte-identical.

There is also **no `set.seed()` before `model_parts()`** in either script, so
permutation importance is not reproducible run-to-run — which is exactly why `aspect`
and `roughness` swapped between the two archived versions of Figure 6. This is relevant
to Reviewer 2's point that permutation importance redistributes among correlated
predictors.

### 4.7 Print resolution of Figure 2

`vegemap.jpg` is 2400×800 px at **183 dpi**; its source panels are only 1081×721 and
1081×720 px (a 1-pixel height mismatch that `image_append` silently pads). The journal
asks for ≥300 dpi. This is the mechanical reason for Reviewer 1's "Figure 2 is
difficult to read at its current size."

### 4.8 Mislabelled TDM risky figure

`sdm_tdm.R` L405 sets `title = "Risky area (TBM)"` inside the **TDM** script.
`ortho/figures/risky_tdm.png` and `paper/files/risky_tdm.jpg` therefore both carry the
title "Risky area (TBM)" — two different figures with the same, wrong, title. Not used
in the manuscript, but published in the code repository.

---

## 5. What the black outlines are

`sasa_pol_21`, defined identically in `sdm_tbm.R` L243-250 and `sdm_tdm.R` L229-236:

```r
sasa_pol_21 <- sasa21_ras %>%
  filter(sasa == 1) %>%
  stars::st_as_stars() %>% sf::st_as_sf(merge = T) %>%
  mutate(area = sf::st_area(.)) %>%
  filter(area > units::set_units(5, m^2)) %>%   # <-- filter
  select(sasa) %>% vect()
```

**It is a filtered subset, not the full observed 2021 distribution.** Quantified:

```
total polys: 1440   >5m2: 231   <=5m2: 1209
area >5m2: 8328.493   area <=5m2: 1842.002
fraction of 2021 Sasa area NOT outlined: 18.11123 %
```

So the outlines show 231 of 1,440 observed patches (16 % of patches) covering
8,328 m² of the 10,170 m² reported in the text — **18.1 % of the mapped 2021 Sasa area
is invisible in Figures 7, 8 and 9.** No caption, legend or Methods sentence mentions
the outlines at all, let alone the 5 m² cut-off. Readers comparing the black outlines
with the stated 10,170 m² are being shown 8,328 m².

The same 5 m²-filtered polygon set is the numerator of the "become unsuitable by 2030"
statistics (§6), while the denominator is the unfiltered 10,170 m².

---

## 6. Manuscript numbers: which are reproducible, and one that is not

### 6.1 The "8,542 vs 8,547" puzzle is resolved — it is `terra::expanse()`

`terra::expanse()` defaults to `transform = TRUE`, i.e. it reprojects to lon/lat and
returns **geodesic** area, which at this latitude is 0.0545 % smaller than the planar
1 m² pixel count. Every manuscript area is the `expanse()` value:

```
Sasa2012 expanse: 8542.341   pixels: 8547     -> MS 8,542
Sasa2021 expanse: 10170.45   pixels: 10176    -> MS 10,170
gain     expanse: 4094.768   pixels: 4097     -> MS 4,095
loss     expanse: 2466.655   pixels: 2468     -> MS 2,467
TBM 2021 suitable expanse: 47253.26           -> MS 47,253
TDM 2021 suitable expanse: 12766.04           -> MS 12,766
TBM new-suitable  expanse: 27049.27           -> MS 27,049
TDM new-suitable  expanse:  4386.61           -> MS 4,387
TBM lost          expanse:  2257.77           -> MS 2,257
TDM lost          expanse:   716.61           -> MS 717
```

**This retracts the "reconcile the definitions" concern in `known_leads.md`.** The
numbers are consistent; they simply need one Methods sentence saying areas are geodesic.

### 6.2 One percentage is wrong

Manuscript (`results.qmd` L87 and the Fig 8 caption): *"of the areas occupied by Sasa in
2021, TBM predicted 2,257 m² (**21 %**) to become unsuitable by 2030, and TDM predicted
717 m² (7 %)."*

```
TBM 2257/10170 = 22.19666 %
TDM  717/10170 =  7.045903 %
```

7 % is right; **21 % should be 22 %**. Separately, the numerator is computed on the
5 m²-filtered polygon set (8,328 m²) while the denominator is the unfiltered 10,170 m²;
on a like-for-like basis the values would be 27.1 % and 8.6 %.

The other two percentages are fine: 27,049 / 47,253 = 57.2 % ("57 %");
4,387 / 12,766 = 34.4 % ("34 %").

### 6.3 Threshold sensitivity (directly answers Reviewer 1)

The 0.5 cut-off is a default probability cut-off, not a calibrated threshold — nothing
in the code calibrates it (`tss_max` is used for tuning, but the reported binarisation
is a hard `> 0.5`). Sensitivity of the risky area:

```
threshold 0.40 : 70368 px
threshold 0.45 : 52429 px
threshold 0.50 : 36797 px
threshold 0.55 : 20374 px
threshold 0.60 :  8918 px
threshold 0.65 :  3222 px
```

A ±0.05 change moves the headline risky area by **+42 % / −45 %**.

### 6.4 The 2021 and 2030 maps cover different domains

```
sasa_pred_sdm_21: nonNA 397403     sasa_pred_sdm_30: nonNA 407658
cells in 2030 domain but not 2021: 10255
cells in 2021 domain but not 2030: 0
```

Cause, traced to the snow layers:

```
nonNA snow12: 1206057  snow21: 1178227  snow30: 1206076
in30not21: 27849   in21not30: 0
snow21 range: 66.125 230.4076   snow30 range: 0 255
```

`fitted_2021.tiff` is masked over 27,849 more cells than `fitted_2030.tiff`, so the
2030 HS map is defined on 10,255 cells where the 2021 map is not. Consequences:
- "newly suitable" is computed on the intersection, so the 27,049 m² figure is
  internally consistent; but `2030 suitable − 2021 suitable` = 69,214 − 47,279 =
  21,935 px, i.e. a different and equally defensible definition gives a 19 % smaller
  number. The Methods do not say which was used.
- The 2030 panels (unused, §2) and the difference panels cover visibly different
  extents from the 2021 panels.
- The out-of-range values in `snow_30` are only 3 pixels (1 at ≤0, 2 at ≥250) within
  the `elevation < 2560` domain, so the 0–255 range is an outlier artefact, not
  systemic. Noted, not a headline problem.

---

## 7. `analyse_sdm.R`: dead code, inconsistent with the other two scripts

The script runs (I executed its full logic against the archived rasters) but:

- It produces **only three on-screen exploratory plots** (`geom_point`, two
  `geom_violin`); it calls `ggsave()` zero times and `writeRaster()` zero times.
  **No number or figure in the manuscript comes from it.**
- Its inputs are the *old* generation of artefacts:
  `data/sasa_pred_sdm_12.tiff` (an orphan — no code produces it),
  `data/selected_comms.tiff` (orphan), `data/sasa_inc.tiff` (orphan). It never touches
  the TDM outputs (`sasa_pred_tdm_*.tiff`) at all.
- `dist` (L15-18) is `terra::distance()` from `selected_comms.tiff`, a raster of
  arbitrary integer community IDs (values 5, 6, 8, 9, 11, 14, …; 5,159 non-NA cells).
  It is **distance to the nearest "selected community", not distance to Sasa** — i.e.
  a completely different quantity from the `dist` predictor in `sdm_tdm.R` (L44-47),
  despite sharing the name.
- L22 / L31 `terra::aggregate(fact = 5)` averages the distance layer, then L23 / L32
  filter `dist != 0 & dist < 10` on that **mean** distance — the filter no longer means
  what it says. After aggregation only 1,239 joined rows survive.
- `sasa_pred_2030` (L11-12) is loaded and never used. `library(tidysdm)` and
  `library(DALEX)` are loaded and never used.
- `setwd("~/Projects/jasms2023f/ortho/")` — a third root path, different again from
  `sdm_tbm.R`/`sdm_tdm.R` (`~/doctoral_thesis/chap2/ortho/`) and
  `plot_vegetation_map.R` (`~/Projects/jasms2023f/`). `ortho/.Rhistory` shows a
  fourth (`/media/okamoto/HDD3TB/tateyama/...`).

Recommendation: delete `analyse_sdm.R`, or move it to an `exploratory/` folder with a
README stating it feeds nothing in the paper. As it stands it is the first file a
reviewer opening `scripts/sdm/` alphabetically will read.

## 7.1 `plot_vegetation_map.R` and `plot_snowmelt_shifts_map.R`

- `plot_vegetation_map.R`: Japanese class labels (dead relative to the paper),
  `setwd("~/Projects/jasms2023f//")` (double slash), and the second block (L68-101)
  silently reuses `dem`/`cont` created in the first block — script-level state that
  only holds when run top-to-bottom. Its `cmap` vector is ordered to match
  `vegetation_levels` (Dwarf pine first), which is *not* the numeric code order —
  a correct but fragile coupling. Its outputs (`ortho/data/2012_5x5.png`,
  `2021_5x5.png`) do not exist on disk.
- `plot_snowmelt_shifts_map.R`: the only downstream script that is genuinely
  reproducible — S2 and S3 are byte-identical to its outputs. Two caveats: it has no
  `setwd()`, so it silently depends on being run from `ortho/`; and L38 hard-codes
  `ifelse(abs(shift) > 50, NA, shift)`, an undocumented outlier mask on the very
  quantity Reviewer 2 asks for uncertainty statistics about.

---

## 8. Reviewer 1's question: "which alpine communities or species occur within the predicted risky areas?"

### 8.1 The composition, computed

```
### ortho/risky_area_wo_dist.tiff  n=36797   (TBM, Figure 9)
 2021 class composition:      2: 36797            (100 % "Other Vegetation")
 2012 class composition:
     0     1     2     3     4     5     6     7
     1   366 33953    45   519   209  1141   563
 HS: min 0.5000  median 0.5580  mean 0.5670  max 0.7380

### ortho/risky_area_tdm.tiff  n=3149
 2021: 2: 3149                                    (100 % "Other Vegetation")
 2012:  1:260  2:2709  3:3  4:42  5:22  6:17  7:96
 HS: min 0.5001  median 0.5975  max 0.6401

### ortho/risky_area.tiff  n=10287  (orphan, threshold 0.2)
 2021: 2: 10287                                   (100 % "Other Vegetation")
 2012:  2:9353  3:94  4:110  5:104  6:279  7:347
 HS: min 0.2000  median 0.3421  max 0.8234
```

(class codes: 1 Sasa, 2 Other Vegetation, 3 No Vegetation, 4 Rowan, 5 Maple,
6 Montane Alder, 7 Dwarf Pine)

### 8.2 What this means

**The question is not answerable from the existing rasters, and the reason is
definitional.** "Risky area" is *defined* as `vege21 == 2`, so by construction 100 % of
every risky raster is the single class "Other Vegetation". The `matmet.qmd` description
of that class is "alpine shrubs and herbaceous plants" — a residual bucket in a 7-class
RNN classifier that resolves only *Sasa* and four woody taxa. Answering "the risky areas
are 100 % Other Vegetation" is a tautology, not information, and Reviewer 1 will read it
as one.

The only community-level signal actually recoverable is the **2012** class of the risky
cells: 92.3 % (33,953 / 36,797) were already "Other Vegetation" in 2012, with the rest
mostly montane alder (1,141), dwarf pine (563), rowan (519) and, notably, 366 cells that
were *Sasa* in 2012 and are classified "Other Vegetation" in 2021 — i.e. cells inside the
apparent-loss area. That is a weak answer to a question about alpine communities.

To answer Reviewer 1 properly the study needs either (i) an independent vegetation-community
layer for Murodo-daira (a published phytosociological or GBIF/vegetation-survey map)
intersected with the risky raster, or (ii) field/photo-interpretation of a sample of
risky cells. Neither exists in this repository. This should be stated plainly in the
response letter rather than papered over.

Note also that Reviewer 3 independently suggests collapsing Figures 8 and 9 ("if
encroachment can only happen into 'other vegetation' I recommend to restrict the
illustration to that type, which makes Figure 9 obsolete") — the two reviewers are
converging on the same structural weakness from opposite directions.

---

## 9. Consistency between the three downstream scripts

| Aspect | `sdm_tbm.R` | `sdm_tdm.R` | `analyse_sdm.R` |
|---|---|---|---|
| `setwd()` | `~/doctoral_thesis/chap2/ortho/` | `~/doctoral_thesis/chap2/ortho/` | `~/Projects/jasms2023f/ortho/` |
| models trained | **RF only** (GAM/MaxEnt/XGB commented out, L100-112) | all four | — |
| tuning rds | `models_wo_dist_twi.rds` (**absent**) | `models.rds` (present) | — |
| VI explainer | `DALEX::explain` on **`df_test`** (L173-180) | `DALEXtra::explain_tidymodels` on **`df_train`** (L182-189) | — |
| VI `set.seed` | none | none | — |
| risky raster | `risky_area_wo_dist.tiff` | `risky_area_tdm.tiff` | reads neither |
| risky figure title | "Risky area (TBM)" | "Risky area (TBM)" ← wrong | — |
| prediction outputs | `sasa_pred_sdm_{21,30}.tiff` | `sasa_pred_tdm_{21,30}.tiff` | reads `sasa_pred_sdm_{12,21,30}` |

The VI row is the one that reaches the manuscript: **Figure 6's two panels are computed
on different datasets** (TBM on the 20 % test split, TDM on the 80 % training split) with
two different explainer constructors, and are presented side by side under a single
caption as if comparable. Combined with the missing `set.seed`, neither panel is
reproducible and their difference is partly an artefact of the evaluation data.

Dead code inside `sdm_tdm.R` worth deleting before re-publication: L36-37
(`sasa12_ras %>% terra::as.polygons() %>% filter()` — a no-op whose result is discarded),
L277-280 (`is_others` defined as `layer == 6`, i.e. montane alder, contradicting the
class table, and never used downstream — **confirmed dead**), L438-443 (re-reads the
prediction rasters and writes `sasa_pred_tdm_30_bin.tiff`, which nothing consumes).

In `sdm_tbm.R`: L396-410 and L412-444 are exploratory (the `df %>% ... facet_wrap`
block at L438-444 has a dangling `scale_color_brewer(...)` after the pipe ends — it
evaluates as three separate no-op statements), and L446-470 is the abandoned Japanese
version of Figure 4.

---

## 10. Retracted leads

Leads from `known_leads.md` (or plausible-looking hypotheses) that I checked and found
**wrong**:

1. *"Manuscript reports 8,542 / 10,170 / 4,095 / 2,467 — all slightly different [from
   the pixel counts]. Reconcile the definitions."* — Not a discrepancy.
   `terra::expanse()` returns geodesic area (`transform = TRUE` by default), 0.0545 %
   below the planar count at this latitude. Every single manuscript area matches to the
   rounding. Needs a Methods sentence, not a re-analysis.
2. *`scale_color_discrete(labels = c("GBT","GAM","MaxEnt","RF"))` mislabels the models
   in Figure 5.* — It does not. `autoplot.workflow_set` colours by the `model` column
   whose alphabetical order is `boost_tree, gen_additive_mod, maxent, rand_forest`,
   matching the supplied labels exactly (verified against `models.rds`).
3. *`spatial_initial_split(df_21, prop = 0.2, ...)` contradicts the manuscript's
   "80 % training / 20 % testing".* — It does not. Empirically `prop = 0.2` yields
   79 % training / 21 % testing, consistent with the manuscript and with Figure 3.
4. *`terra::resample(vege21, .)` in the risky-area block bilinearly interpolates a
   categorical raster and corrupts the `vege21 == 2` filter.* — Not triggered: the two
   grids are identical, and recomputing from the raw `vege21` reproduces the archived
   pixel counts exactly (36,797 and 3,149). Latent fragility only.
5. *The 2030 snow layer's 0–255 range poisons the future predictions.* — Only 3 pixels
   in the whole `elevation < 2560` domain are outside 50–240. The real 2021-vs-2030
   issue is the 10,255-cell **domain** difference, not the value range.

---

## 11. Priority list for the revision

**Must fix before resubmission (reviewer-visible, wrong as published):**
1. Figure 8 shows the 2021 maps, not the 2030 maps (§2).
2. "21 %" should be "22 %" (§6.2).
3. The black outlines omit 18 % of the mapped 2021 Sasa and are never explained (§5).
4. Reviewer 1's risky-area question is unanswerable as the raster is defined (§8) —
   needs an explicit, honest response.
5. Threshold sensitivity must be reported; §6.3 supplies the table.
6. The manuscript's map figures cannot be regenerated from the committed code (§4.6).
   The `ggspatial` version of the plotting blocks must be restored or rewritten.

**Should fix (figure quality, all named by reviewers):**
7. Shared legends / shared colour scales / removed in-figure titles across Figs 2, 3,
   5, 6, 7, 8 (§4.1, §4.3, §4.4).
8. Figure 2 at ≥300 dpi with vertical arrangement (§4.7).
9. Figure 4: remove Japanese labels, callout boxes and inset; explain the white areas;
   supply GSI basemap attribution (§4.5).
10. Figure 6: rename `snow`→`Snowmelt DOY`, expand TRI/TPI/dist, compute both panels on
    the *same* dataset with a fixed seed (§9).

**Repository hygiene (two reviewers read the code):**
11. Delete or quarantine `analyse_sdm.R`, `risky_area.tiff`,
    `potential_sasa_area_21.tiff`, `sasa_pred_sdm_dist_*.tiff`, `models_all_5m.rds`,
    `sasa_pred_tdm_30_bin.tiff`, `is_others`, and the Japanese Figure-4 draft.
12. Fix `sdm_tdm.R` L405 title, `sdm_tbm.R`'s `models_wo_dist_twi.rds`, the four
    different `setwd()` roots, and re-enable the three commented-out learners in
    `sdm_tbm.R`.
13. Restore the English `plot_vegetation_map.R` and add a script for Figure 4 (or
    document it as a manual figure).
14. Document that areas are `terra::expanse()` geodesic values.
