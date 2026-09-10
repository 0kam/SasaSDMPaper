# Crosscheck: manuscript claims vs. code vs. archived artefacts

Phase: CROSSCHECK. Every number below was recomputed by executing R against
`/Users/okamoto/NIES/SasaSDMPaper/ortho/data/*.tiff`, `ortho/data/snow/raw/*.tiff` and the
fitted `.rds` objects in `/Users/okamoto/NIES/SasaSDMPaper/ortho/`. Where a claim could not be
recomputed, that is stated explicitly.

Throughout I distinguish:

* **(a)** what the code as written would do on a clean run,
* **(b)** what the archived artefacts show actually happened,
* **(c)** what the manuscript says.

---

## 0. Executive summary of the divergences that matter

| # | Divergence | Severity |
|---|---|---|
| D1 | "mean regression coefficient was −0.86 … 8.6 days per decade" is the mean of `ortho/data/snow/snow_reg.tif`, a raster that is **not** the predictor used by any model and that **cannot be reproduced** from the published code. Direct recomputation of the per-pixel slopes that actually generated the predictor layers gives **−0.71 d/yr (7.1 d/decade)**. The scene-mean trend is **not statistically significant** (p = 0.20). | critical |
| D2 | TDM: the `dist` predictor used for **training** (distance from *all* 2012 *Sasa* pixels) is **not** the `dist` predictor used to produce **either** prediction map. The 2021 map uses distance from 2012 *Sasa polygons > 5 m²*; the 2030 map uses distance from **2021** polygons. Training vs. 2021-prediction surfaces differ by **68 m on average** (max 343 m; 87 % of cells differ by > 1 m). Reviewer 2 caught only the 2030 half of this. | critical |
| D3 | Figure 8 does **not** show the 2030 HS maps. Its top row is literally `hsmap_tbm_2021.jpg` / `hsmap_tdm_2021.jpg` — the same two panels as Figure 7 — while the caption says "Habitat suitability (HS) maps for 2030". | critical |
| D4 | Only ~34 % of the mapped scene enters the models: `filter(elevation < 2560)` removes **799,744 of 1,206,233** mapped pixels (66.3 %). The manuscript justifies this as "where *Sasa* does not occur", but 22 (2012) and 71 (2021) *Sasa* pixels lie above 2,560 m and *Sasa* reaches 2,711 m in 2021. All reported HS areas are therefore areas within the < 2,560 m subdomain only. | critical |
| D5 | The reported TSS values (0.55 / 0.70) are `tidysdm::tss_max` = TSS **maximised over thresholds**, not TSS at the 0.5 cut-off used to define every "suitable" and "risky" area. On the archived 2021 rasters the TSS-optimal threshold is **0.36** (TBM) and **0.12** (TDM), not 0.5. | important |
| D6 | The manuscript states TSS "ranges from 0 to 1". TSS ranges from −1 to +1. | important |
| D7 | Discussion claim "no isolated *Sasa* patches formed by seed dispersal were observed in the expansion areas" is contradicted by the maps: **342 expansion patches (681 m², 17 % of the expansion area)** lie entirely > 5 m from any 2012 *Sasa* pixel; 206 patches (430 m²) lie > 10 m away; the farthest expansion pixel is 238 m from 2012 *Sasa*. | important |
| D8 | `sdm_tbm.R` as published trains **only random forest** — GAM, MaxEnt and XGBoost are commented out (lines 100–112) — and saves/loads `models_wo_dist_twi.rds`, a file that does not exist. The archived `models_wo_dist.rds` **does** contain all four, so the archived artefact was produced by a version of the file that is no longer in the repository. | critical (reproducibility) |
| D9 | Both SDM scripts glob `data/terrain_features/*.tif`, which now contains `twi.tif`. A clean re-run would fit an **8-predictor** model including TWI, which the manuscript never mentions. The archived stacks contain **no** `twi` — so (a) ≠ (b). | important |
| D10 | Variable importance is computed on the **test** set with `DALEX::explain` for the TBM (`sdm_tbm.R:169–190`) but on the **training** set with `DALEXtra::explain_tidymodels` for the TDM (`sdm_tdm.R:176–200`). `DALEX::model_parts` defaults are used, i.e. `N = 1000` rows and `B = 10` permutations. None of this is in the manuscript, which describes one procedure. | important |
| D11 | "images taken in September–October of 2012 and 2021" — the archived source images include **27 Aug 2012, 24 Aug 2021, 31 Aug 2021**, and `alignment_report_2012_2021.csv` lists 28 Aug 2012 and 31 Aug 2021. | minor–important |
| D12 | "a 5 m digital elevation model … resampled to 1 m" — the actual terrain rasters have cells of **4.97 m × 6.16 m** (non-square) and are in **EPSG:3099 (JGD2000)** while the vegetation and snow rasters are **EPSG:6690 (JGD2011)**. `terra::resample()` reconciles the grids without reprojecting. | important |
| D13 | The published Figure S1 (`ortho/snowmelt_shifting.png`) is a **boxplot of anomalies relative to 2011**; the code in `preprocess_snow_data.R:211–227` that writes that filename produces a **scatter + `geom_smooth(lm)` of raw DOY**. The archived figure was not made by the published code. | important |
| D14 | Reported "21 %" of 2021 *Sasa* becoming unsuitable under the TBM recomputes to **22.2 %**. All other percentages reproduce. | minor |
| D15 | The public GitHub repository contains **none** of the data needed to reproduce anything downstream: no `vege_2012/2021_5x5.tiff`, no `data/snow/raw/*.tiff`, no `fitted_*.tiff`, no `.rds` models, no prediction rasters. The Data Availability Statement ("The data and scripts supporting this study are openly available") overstates what is archived. | critical (AE comment) |

---

## 1. Line-number mapping (Reviewer 3 → manuscript source)

### 1.1 Method and reliability

The submitted PDF **does** carry printed line numbers: `index.qmd:36–38` injects
`\usepackage{lineno}` + `\linenumbers`, and the numbers are rendered in the left margin of
`paper/submit_files/index.pdf`.

Procedure actually executed:

```
pdftotext -layout /Users/okamoto/NIES/SasaSDMPaper/paper/submit_files/index.pdf sub_layout.txt
```

then a Python pass that accepts a line only if it begins (≤ 4 leading spaces) with the integer
`prev + 1`. Result: **609 of 609 line numbers recovered, 0 missing, monotone 1 → 609.** The only
non-empty text lines the parser rejected are page numbers, the running footer, the displayed TSS
equation, and figure captions — none of which `lineno` numbers. So the extraction is mechanical
and complete, not guessed.

Mapping from PDF line → `.qmd` source was then done by normalised 6-word substring matching
against `index.qmd`, `intro.qmd`, `matmet.qmd`, `results.qmd`, `discussion.qmd`,
`conclusion.qmd`. Because the `.qmd` files store each paragraph as one long source line, the
mapping is **PDF-line → qmd paragraph**, which is the finest granularity available.

**Cross-validation of the mapping.** Three independent sanity checks all pass:
`L21` = "as predictors in the models." (Reviewer 3: *replace "model" with "HSM"*);
`L35` = "…this study advances understanding of how alpine…" (Reviewer 3: *"study advances
understanding" — unjustified self-praise*); `L267` = "Model performance was evaluated using the
True Skill Statistic (TSS; Allouche…" (Reviewer 3: *TSS need not to be explained*).
Reliability: **high**.

One caveat: two of Reviewer 3's citations are internally odd and I report them literally rather
than "correcting" them. `L390` ("add citation") falls in *4.3 Future projections*, mid-sentence
("…climate modelling are expected to improve the realism of such projections."), and appears in
the reviewer's list *out of numeric order* (between the Figure 8 comment and `L328`), so it may
be a slip for a different line. `L424–430` ("is just repetition… I do not see the claim raised in
L424") points at the first Conclusion paragraph, and `L431–438` ("shift to section 4.5") at the
second — consistent, so those are fine.

### 1.2 The mapping table

Verbatim submitted-PDF text is given so the mapping can be checked by eye.

| Reviewer line | Source | Submitted-PDF text (start) |
|---|---|---|
| L21 | `index.qmd:50` (Abstract → Methods) | "as predictors in the models." |
| L26–29 | `index.qmd:53` (Abstract → Results) | "the prior *Sasa* distribution as the strongest determinants…" → "further expansion of *Sasa* by 2030." |
| L34–39 | `index.qmd:56` (Abstract → Main conclusions) | "ing ground-based observations. By linking…" → "planning." |
| L35 | `index.qmd:56` | "patterns at meter resolution, this study advances understanding of how alpine" |
| L50 | `intro.qmd:5` | "in community composition. For example, grass-dominated snowbed communities" |
| L57 | `intro.qmd:5` | "*Sasa* invasion into snowbed communities and resulting in substantial reductions" |
| L71–101 | `intro.qmd:9` (L71–78), `intro.qmd:11` (L79–87), `intro.qmd:13` (L88–101) | "To address these challenges, a method for generating high-resolution vegetation…" |
| L81 | `intro.qmd:11` | "alpine meadows on the western slope of Mt. Tateyama in the northern Japanese" |
| L84–89 | `intro.qmd:11` (L84–87) + `intro.qmd:13` (L88–89) | "1977 and 2015 (Yoshida et al. (2016))…" |
| L104 | `intro.qmd:15` | "acts as a key determinant of *Sasa* distribution in alpine landscapes, and (2)" |
| L105–108 | `intro.qmd:15` | "how *Sasa* distribution may shift under earlier snowmelt conditions…" |
| L110–116 | `matmet.qmd:10` | "To quantify how snowmelt timing influences *Sasa* distribution…" |
| L113–114 | `matmet.qmd:10` | "high-resolution snapshots of current distribution, they do not explicitly quantify / the relationship between environmental variables and species occurrence." |
| Figure 1 | `matmet.qmd:16–26` (`files/overview.jpg`) | — |
| L144 ff | `matmet.qmd:32` | "A major advantage of time-lapse cameras is their ability to capture fine-scale," |
| L152–153 | `matmet.qmd:32` | "each pixel. Snowmelt detection followed a workflow based on Otsu's binarization / method (Otsu (1979)) as described in Ide and Oguma (2013)." |
| L162 | `matmet.qmd:38` | "applied a local-feature-based automated alignment approach." |
| L164–170 | `matmet.qmd:39` | "• **Vegetation classification**: Temporal changes in leaf color during…" |
| L185 | `matmet.qmd:44` | "sequently, areas that appear to have decreased in *Sasa* cover may not represent" |
| Figure 2 | `matmet.qmd:47–62` (`files/vegemap.jpg`) | — |
| L190–196 | `matmet.qmd:66` | "To predict future *Sasa* expansion and understand its driver, we constructed…" |
| L204 | `matmet.qmd:68` | "a predictor, constraining the predicted 2021 distribution to areas near existing" |
| L206–207 | `matmet.qmd:68` | "Comparing the two models allows us to distinguish between the broader potential / niche under seed dispersal and the more localized realized expansion expected" |
| L209 (heading 2.4.1) | `matmet.qmd:71` | "### Explanatory variables" |
| L216–217 | `matmet.qmd:75` | "Both models incorporated elevation, slope, aspect, roughness, the Terrain / Ruggedness Index (TRI), the Topographic Position Index (TPI), and snowmelt" |
| L219–220 | `matmet.qmd:75` | "derived from a 5 m digital elevation model (DEM) provided by the Geospatial / Information Authority of Japan and resampled to 1 m resolution…" |
| L224 | `matmet.qmd:77` | "past decade. To represent this temporal trend, we fitted a linear regression to" |
| L228 | `matmet.qmd:77` | "future projections, we extended the same regressions to estimate snowmelt DOYs" |
| L232 | `matmet.qmd:77` | "on snowmelt timing (Figure S1) and its prediction (Figure S2, Figure S3) are" |
| L232–235 | `matmet.qmd:77` | "… In the TDM, distance from the 2012 / *Sasa* distribution was included as an additional explanatory variable…" |
| L239 | `matmet.qmd:81` | "The TBM was trained to predict the *Sasa* distribution in 2021. Because many" |
| L244 ff (headings 2.4.3.x) | `matmet.qmd:83, 85, 89, 109` | "Data sampling and splitting" / "Data sampling" / "Initial splitting" / "Cross-validation" |
| Figure 3 | `matmet.qmd:93–107` (`files/initial_split.jpg`) | — |
| L258 | `matmet.qmd:111` | "For hyperparameter tuning and ensemble construction, we performed fourfold" |
| L267 | `matmet.qmd:117` | "Model performance was evaluated using the True Skill Statistic (TSS; Allouche" |
| L297–299 | `results.qmd:12` | "…Many of these apparent decreases likely resulted from shrub / encroachment and growth…" |
| L300 | `results.qmd:12` | "adopted the expansion area (4,095 m²) as the primary metric of change." |
| Figure 4 | `results.qmd:15–26` (`files/expanded_area.pdf`) | — |
| L309–310 | `results.qmd:30` | "indicating clear performance gains from incorporating distance in addition to / topographic features." |
| Figure 5 | `results.qmd:32–46` (`files/model_performance.jpg`) | — |
| Figure 6 | `results.qmd:50–65` (`files/vi.jpg`) | — |
| Figure 7 | `results.qmd:69–83` (`files/hsmap_2021.jpg`) | — |
| L321 | `results.qmd:87` | "predicted 27,049 m² (57 % of the 2021 TBM suitable area) of newly suitable" |
| Figure 8 | `results.qmd:89–106` (`files/future.jpg`) | — |
| Figure 9 | `results.qmd:110–119` (`files/risky_tbm.jpg`) | — |
| L328 | `discussion.qmd:3` | "This study demonstrates how snowmelt-driven processes can be quantified and" |
| L351 | `discussion.qmd:9` | "between 2012 and 2021, supporting the idea that low dispersal ability creates a" |
| L390 | `discussion.qmd:21` | "climate modelling are expected to improve the realism of such projections. The" |
| L391 | `discussion.qmd:21` | "framework presented here can readily incorporate these developments, enabling" |
| L421–422 | `discussion.qmd:35` | "Despite these limitations, our results consistently indicate that snowmelt timing / plays a central role…" |
| L424–430 | `conclusion.qmd:3` | "This study demonstrated the effectiveness of integrating time-lapse cameras with…" → "decades." |
| L431–438 | `conclusion.qmd:5` | "The expansion of *Sasa* poses a serious threat to alpine plant diversity…" → "ecosystems worldwide." |
| L439–443 | `conclusion.qmd:7` | "In summary, this study highlights the power of combining high-frequency time-…" |

Section anchors, for orientation: L40 = §1 Introduction; L109 = §2 Materials and Methods;
L134 = §2.1; L156 = §2.2; L179 = §2.3; L189 = §2.4; L209 = §2.4.1; L238 = §2.4.2; L244 = §2.4.3;
L261 = §2.4.4; L281 = §2.4.5; L291 = §3 Results; L292 = §3.1; L304 = §3.2; L319 = §3.3;
L327 = §4 Discussion; L335 = §4.1; L364 = §4.2; L379 = §4.3; L394 = §4.4; L412 = §4.5;
L423 = §5 Conclusion; L444 = §6 Data Availability; L451 = References.

---

## 2. Every number in the manuscript, recomputed

Legend: **✔** reproduces; **✘** does not; **~** reproduces but the definition in the manuscript
does not match the definition in the code; **n/r** not computed by any code in the repository.

### 2.1 Vegetation change (Results §3.1, Abstract, Figure 4 caption)

All of these reproduce **exactly** once you know they were produced by `terra::expanse()` with
its default `transform = TRUE`, i.e. geodesic area, not a pixel count. Raw pixel counts differ by
a factor 0.99941 (the UTM scale distortion).

| Printed | Location (`file:line` / PDF line) | Code | Recomputed | Match |
|---|---|---|---|---|
| 8,542 m² (*Sasa* 2012) | `results.qmd:12` / L293; `results.qmd:21` (Fig 4 cap) | **no script in repo** | `expanse()` = **8,542.34**; pixel count = 8,547 | ✔ |
| 10,170 m² (*Sasa* 2021) | `results.qmd:12` / L293 | none | **10,170.45**; pixel count 10,176 | ✔ |
| 1,628 m² net | `results.qmd:12` / L294 | none | **1,628.11** (pixels 1,629) | ✔ |
| +19 % net | `results.qmd:12` / L294 | none | 1628.11 / 8542.34 = **19.06 %** | ✔ |
| 4,095 m² expansion | `results.qmd:12,21` / L296, L300 | none | **4,094.77** (4,097 px) | ✔ |
| +48 % | `results.qmd:12,21` / L296, L301; `index.qmd:53` / L23 | none | 4094.77 / 8542.34 = **47.93 %** | ✔ |
| 2,467 m² decrease | `results.qmd:12` / L297 | none | **2,466.66** (2,468 px) | ✔ |
| "nine-year" | `matmet.qmd:44`, `results.qmd:12` | — | 2021 − 2012 = 9 | ✔ |
| "+260 % over 38 years" (Yoshida 2016) | `results.qmd:12` / L303 | external | 2015 − 1977 = 38 | ✔ (external) |
| "44–260 % between 1977 and 2015" | `intro.qmd:11` / L83 | external | — | not checkable |

Command used:
```r
s12 <- vege12 %>% mutate(sasa = ifelse(layer==1,1,0)) %>% select(sasa)
expanse(s12 %>% filter(sasa==1))            # 8542.341   (transform=FALSE -> 8547)
```

Transition matrix (2012 → 2021, 1 m pixels, NA-excluded) — this is exactly what Reviewer 1 asked
for and it is **not in the manuscript**:

| 2012 *Sasa* → 2021 class | px | % of loss |
|---|---|---|
| 7 Dwarf pine (*Pinus pumila*) | 1,156 | 46.8 |
| 2 Other vegetation | 932 | 37.8 |
| 4 Rowans | 159 | 6.4 |
| 6 Montane alder | 133 | 5.4 |
| 5 Maple | 85 | 3.4 |
| 3 No vegetation | 3 | 0.1 |
| (stayed *Sasa*) | 6,079 | — |

| 2012 class → 2021 *Sasa* | px |
|---|---|
| 2 Other vegetation | 2,171 |
| 7 Dwarf pine | 1,425 |
| 4 Rowans | 224 |
| 5 Maple | 155 |
| 6 Montane alder | 109 |
| 3 No vegetation | 13 |

Note for the response letter: the largest single component of "*Sasa* loss" is conversion to
**dwarf pine**, not to the broad "Other Vegetation" class. The manuscript's explanation
(`matmet.qmd:44`, `results.qmd:12`) that losses are shrub-occlusion artefacts is therefore
testable and partly consistent, but *Pinus pumila* is a creeping conifer, not a tall shrub
canopy, which weakens the occlusion argument for 47 % of the loss.

### 2.2 Snowmelt trend (Methods §2.4.1, Abstract)

| Printed | Location | Code | Recomputed | Match |
|---|---|---|---|---|
| "mean regression coefficient was −0.86" | `matmet.qmd:77` / L226–227 | see below | mean of `data/snow/snow_reg.tif` = **−0.8636** | ~ (see D1) |
| "advance of 8.6 days per decade" | `matmet.qmd:77` / L227; `index.qmd:53` / L24 | — | from `snow_reg.tif` yes; from the predictor-generating regressions **−7.15 d/decade** | ✘ |
| "2011 to 2021" | `matmet.qmd:32,77` | `data/snow/raw/` | 10 files: 2011–2018, 2020, 2021 | ✔ |
| "Data for 2019 are missing" (Fig S1 cap) | `supplement.qmd:20` | — | confirmed, no 2019 file | ✔ |
| "1,000 pixels were randomly sampled" (Fig S1 cap) | `supplement.qmd:20` | `preprocess_snow_data.R:209` `sample_n(1000)` | ✔ in code | ✔ |
| "predicted 2021 values as explanatory variables" | `matmet.qmd:77` / L225 | `sdm_*.R:26–28` reads `fitted_2021.tiff` | reproduced **exactly** (cor = 1.0000, 100 % of cells identical to < 0.01) by per-pixel OLS with `filter(snowmelt > 0)` | ✔ |
| "extended the same regressions to estimate snowmelt DOYs for 2030" | `matmet.qmd:77` / L228 | `preprocess_snow_data.R:172–188` | `fitted_2030.tiff` reproduced **exactly** by the same OLS (cor = 1.0000) | ✔ |

**Detail on D1.** `snow_reg.tif` is the only object in the repository whose mean is −0.86:

```
snow_reg.tif   mean = -0.8635999   sd = 0.8519743   n = 1,133,175   grid 1739 x 1791
```

But (i) `snow_reg.tif` is *not* used by `sdm_tbm.R` / `sdm_tdm.R`; the models use
`fitted_2021.tiff`. (ii) The block that is supposed to create it,
`preprocess_snow_data.R:118–126`, is **syntactically invalid** — a bare `partit` identifier on
line 121, `$coeffisients` (misspelt) on line 123, and `$adj.r.squared` on an `lm` object on line
124. That block cannot have run as written. (iii) `preprocess_snow_data.R:130` says the raster is
filtered to `lm.r2 > 0.2`; if it had been, it would hold ~186k pixels — the archived file holds
1,133,175, the same count as the unfiltered `snow_mean.tif`. (iv) I could not reconstruct it:
recomputing the slopes on the vegetation grid and comparing cell-by-cell on `snow_reg.tif`'s own
footprint gives cor = 0.24 (unfiltered obs) or cor = 0.51 (`> 0` filter). `snow_mean.tif` is
closer (cor = 0.95) but still not identical. **`snow_reg.tif` is not reproducible from the
published code and data.**

What the pipeline that actually produced the model predictors gives (per-pixel OLS of DOY on
year, `filter(snowmelt > 0)`, 10 years, 1,205,859 pixels):

```
mean slope   = -0.7147 d/yr   ->  7.1 days/decade
median       = -0.6264
sd           =  0.6353
2.5 – 97.5 % = -2.130 to +0.292
fraction of pixels with a negative slope = 91.65 %
```

Whole-scene trend in the annual **mean** snowmelt DOY (this is the number Reviewer 2 asks for):

```
slope = -0.9184 d/yr   SE = 0.663   t = -1.385   p = 0.204   R2 = 0.193
95 % CI = [-2.448, +0.611]      -> NOT significant
```

Annual mean DOY (anomaly vs. 2011): 2011 171.5 (0.0); 2012 179.5 (+8.1); 2013 172.9 (+1.5);
2014 179.1 (+7.6); 2015 165.9 (−5.6); 2016 157.9 (−13.6); 2017 176.8 (+5.4); 2018 165.0 (−6.5);
2020 170.1 (−1.4); 2021 165.6 (−5.9). The series is dominated by interannual variability; the
first two years are the two latest-melting years in the record, which is what drives the negative
slope.

Also undocumented: the raw snowmelt rasters are named `…_120-230_BW.tiff` and their values are
bounded to **DOY 120–230** (observed range 0–232). Pixels never seen to melt inside that window,
and pixels with value 0, are handled by the `snowmelt > 0` filter — nowhere stated.

### 2.3 Model performance (Results §3.2)

| Printed | Location | Code | Recomputed | Match |
|---|---|---|---|---|
| Ensemble test TSS = 0.55 (TBM) | `results.qmd:30` / L308 | `sdm_tbm.R:159–161` → `ortho/tss_score_tbm.csv` | `tss_max = 0.5555946` | ✔ |
| Ensemble test TSS = 0.70 (TDM) | `results.qmd:30` / L308 | `sdm_tdm.R:167–168` → `ortho/tss_tdm.csv` | `tss_max = 0.7009731` | ✔ |
| "TDM consistently achieved higher TSS than TBM" | `results.qmd:30` / L305 | `models*.rds` | TBM best CV TSS 0.577, TDM best 0.705 | ✔ |
| "Within the TBM, GBT and MaxEnt performed best" | `results.qmd:30` / L306 | `models_wo_dist.rds` | MaxEnt 0.5768 > XGB 0.5741 > GAM 0.5540 > RF 0.5369 | ✔ |
| "within the TDM, GBT, MaxEnt and RF outperformed GAM" | `results.qmd:30` / L307 | `models.rds` | XGB 0.7053 > RF 0.6982 > MaxEnt 0.6980 > GAM 0.6882 | ✔ |
| "up to 18 trials" grid search | `matmet.qmd:117` / L266 | `grid = 18` | MaxEnt 18, XGB 18, RF 7 (TBM) / 8 (TDM), GAM 1 | ✔ ("up to") |
| "fourfold cross-validation" | `matmet.qmd:111` / L258 | `spatial_block_cv(v = 4)` | ✔ | ✔ |
| "80 % training and 20 % testing" | `matmet.qmd:91` / L252 | `spatial_initial_split(df_21, prop = 0.2, spatial_block_cv)` | reconstructed: TBM 21,428/27,055 = **79.2 %** train; TDM 16,572/20,891 = **79.3 %** | ✔ |
| "areas above 2,560 m … excluded" | `matmet.qmd:87` / L249–250 | `filter(elevation < 2560)` | ✔ mechanically | ~ (see D4) |
| "absence data downsampled to 5 m" | `matmet.qmd:87` / L248 | `aggregate(fact = 5)` + `thin_by_cell` | ✔ | ✔ |
| "TSS ranges from 0 to 1" | `matmet.qmd:117` / L268 | — | TSS ∈ [−1, 1] | ✘ |

**Note on `prop = 0.2`.** This *looks* like a 20 % training set but is not: `spatial_initial_split`
uses `prop` to set the number of spatial blocks, and one block becomes the test set. Recomputed
proportions are 79.2 % / 79.3 % training. **The manuscript's 80/20 claim is correct** — I am
retracting the earlier lead that suggested otherwise.

**Note on ensemble composition (b vs c).** `stacks::fit_members()` keeps only members with
non-zero blending weight. The archived stacks contain:

```
model_stack_wo_dist.rds (TBM): default_maxent_1_17, default_maxent_1_12,
                               default_xgb_1_05,    default_xgb_1_09        -> 4 members
model_stack.rds         (TDM): default_maxent_1_02/1_14/1_03,
                               default_xgb_1_02/1_04/1_08/1_10              -> 7 members
```

**Neither ensemble contains a random forest or a GAM member.** The manuscript (`matmet.qmd:117`)
lists four algorithms and never says that two of them received zero weight. Training-set sizes in
the archived stacks: TBM 21,389 rows, TDM 16,596 rows — within 0.2 % of my reconstruction
(21,428 / 16,572), confirming the archived models correspond to this pipeline **without** TWI.

**Note on D5.** `tidysdm::sdm_metric_set()` returns `boyce_cont`, `roc_auc` and `tss_max`.
`tss_max` maximises TSS over thresholds. Evaluating the archived 2021 HS rasters against the
observed 2021 *Sasa* map over the whole modelled domain (n = 390,065, prevalence 0.0256):

```
TBM 2021 raster:  TSS@0.5 = 0.561   best threshold = 0.36   best TSS = 0.621
TDM 2021 raster:  TSS@0.5 = 0.692   best threshold = 0.12   best TSS = 0.785
```

So 0.5 is a default probability cut-off, not a calibrated threshold — exactly Reviewer 1's point
5. Note also that using the TSS-optimal 0.12 threshold for the TDM would change every "suitable"
and "risky" area figure in the paper.

### 2.4 Variable importance (Results §3.2, Figure 6)

| Printed | Location | Recomputed / checked | Match |
|---|---|---|---|
| "snowmelt DOY … most influential predictor in the TBM" | `results.qmd:48` / L311–312 | `ortho/figures/vi_tbm.png`: snow ≈ 0.19 > elevation ≈ 0.12 > TRI > roughness > aspect > slope > TPI | ✔ |
| "distance … strongest predictor in the TDM, followed by snowmelt DOY" | `results.qmd:48` / L312–314 | `ortho/figures/vi_tdm.png`: dist ≈ 0.45 > snow ≈ 0.22 > elevation ≈ 0.05 > aspect > TRI > roughness > slope > TPI | ✔ |

The *ordering* claims are supported by the archived figures. The *procedure* is not what the
manuscript describes — see D10 (§3 below).

### 2.5 Habitat-suitability areas (Results §3.2–3.3, Figure 7–8 captions)

All recomputed with `terra::expanse()` on the archived prediction rasters.

| Printed | Location | Code | Recomputed | Match |
|---|---|---|---|---|
| 47,253 m² TBM 2021 suitable (HS > 0.5) | `results.qmd:67,75` / L316 | `sdm_tbm.R:386–389` | **47,253.26** | ✔ |
| 12,766 m² TDM 2021 suitable | `results.qmd:67,75` / L317 | `sdm_tdm.R:417–420` | **12,766.04** | ✔ |
| 27,049 m² newly suitable, TBM | `results.qmd:87,95` / L321 | `sdm_tbm.R:397–401` | **27,049.27** | ✔ |
| "57 % of the 2021 TBM suitable area" | `results.qmd:87` / L321 | — | 27049.27 / 47253.26 = **57.24 %** | ✔ |
| 4,387 m² newly suitable, TDM | `results.qmd:87,95` / L322 | `sdm_tdm.R:423–427` | **4,386.61** | ✔ |
| "(34 %)" | `results.qmd:87` / L322 | — | 4386.61 / 12766.04 = **34.36 %** | ✔ |
| 2,257 m² becoming unsuitable, TBM | `results.qmd:87,95` / L323 | `sdm_tbm.R:403–410` | **2,257.77** | ✔ |
| "(21 %)" | `results.qmd:87` / L323 | — | 2257.77 / 10170.45 = **22.19 %** | ✘ (should be 22 %) |
| 717 m² becoming unsuitable, TDM | `results.qmd:87,95` / L324 | `sdm_tdm.R:429–436` | **716.61** | ✔ |
| "(7 %)" | `results.qmd:87` / L324 | — | 716.61 / 10170.45 = **7.05 %** | ✔ |
| "HS > 0.5" threshold (everywhere) | `matmet.qmd:137`, `results.qmd:67,108` | `> 0.5` in code | ✔ mechanically | ~ (D5) |

Denominator check for the "21 %" / "7 %": only `10,170.45` (the observed 2021 *Sasa* area) makes
the TDM figure come out at 7 %. Under that same denominator the TBM figure is 22.2 %, so the
printed **21 % is a rounding/transcription error**. (Other candidate denominators were tested and
rejected: `sasa_pol_21` rasterised = 8,328.46 → 27.1 % / 8.6 %; `sasa_pol_21 ∩ HS2021 > 0.5` →
38.6 % / 10.6 %; model 2021 suitable area → 4.8 % / 5.6 %.)

**Definitional caveat.** The "become unsuitable" numerator is computed from `sasa_pol_21`, i.e.
2021 *Sasa* polygons **filtered to area > 5 m²** (`sdm_tbm.R:243–250`, `sdm_tdm.R:229–236`), whose
area is 8,328 m², not the 10,170 m² the manuscript says. So numerator and denominator come from
two different definitions of "the area occupied by *Sasa* in 2021". That 5 m² filter is nowhere
in the manuscript.

**Domain caveat.** `sasa_pred_*_21.tiff` has 397,403 valid pixels; `sasa_pred_*_30.tiff` has
407,658. The 2021 and 2030 maps therefore cover **different domains** (the 2021 snow layer
`fitted_2021.tiff` is 1703 rows, the 2030 layer 1753 rows). The "newly suitable" figures are
computed on the 397k intersection, but the standalone "2030 suitable" area
(69,176 m² TBM / 14,474 m² TDM — not printed in the manuscript) is over 407k.

### 2.6 Risky areas (Results §3.3, Figure 9)

| Printed | Location | Code | Recomputed |
|---|---|---|---|
| definition: "Other Vegetation" in 2021 ∧ TBM 2030 HS > 0.5 | `matmet.qmd:137` / L288–289; `results.qmd:108,116` / L325–326 | `sdm_tbm.R:336–344` (`sasa == 0`, `pred_sasa_30 > 0.5`, `vege21 == 2`) | ✔ definition matches |
| *no area given* | — | `risky_area_wo_dist.tiff` | **36,777 m²** (TBM); `risky_area_tdm.tiff` = 3,147 m² |

Figure 9 uses `files/risky_tbm.jpg`, i.e. the TBM version — consistent with the caption.
`ortho/risky_area.tiff` (10,281 m²) and `ortho/potential_sasa_area_21.tiff` (17,664 m²) are
orphan artefacts from an earlier definition; nothing in the current scripts writes them.

Note `sdm_tdm.R:405` titles the TDM risky-area plot **"Risky area (TBM)"** — a copy-paste error
in `figures/risky_tdm.png`. That file is not used in the manuscript, so it is harmless, but it
will confuse a reviewer who opens the repository.

### 2.7 Site / instrument numbers (Methods §2.1)

| Printed | Location | Source | Recomputed | Match |
|---|---|---|---|---|
| "approximately 2,450 m a.s.l." (camera) | `matmet.qmd:30` / L138 | `ortho/data/params_optim.json` | `z = 2458` | ✔ |
| "21 megapixels" | `matmet.qmd:30` / L136 | `params_optim.json` | `w = 5616, h = 3744` → 21.0 MP | ✔ |
| "2,350–3,015 m" (western slope) | `matmet.qmd:30` / L140 | DEM over mapped domain | mapped domain spans **2,361–3,005 m**; DEM `tateyamadem_small.tif` spans 1,372–3,011 m | ~ approximately |
| "hourly from 6:00 to 19:00, April–November" | `matmet.qmd:30` / L140–141 | — | not verifiable from repo; archived exposures are 10:57–14:00 | n/r |
| "Since 2010" | `matmet.qmd:30` / L138–139 | `data/snow/aligned/` contains a 2010 file | ✔ (2010 aligned but unused) | ✔ |
| "September–October of 2012 and 2021" | `matmet.qmd:30` / L142–143 | EXIF of `data/images/source/*` | 2012-**08-27**, 09-01, 09-11, 09-17, 09-26, 10-06, 10-21; 2021-**08-24**, **08-31**, 09-07, 09-19, 09-24, 10-02, 10-14 | ✘ |
| "1 m spatial resolution" | `matmet.qmd:40` / L176 | `georectify.R:52–59` `res = 1.0` | vege tiffs are exactly 1 × 1 m | ✔ |
| "5 m DEM … resampled to 1 m" | `matmet.qmd:75` / L219–220 | `data/terrain_features/*.tif` | actual cells **4.97 m × 6.16 m**, EPSG:3099 | ✘ / ~ |
| "seven categories" | `matmet.qmd:39` / L166–170 | `plot_vegetation_map.R:32–40` | raster values 1–7 ✔ … **plus a value 0** present on 5,179 (2012) / 5,191 (2021) pixels, 2,766 / 2,769 of them below 2,560 m | ~ (undocumented 8th value) |

Georectification RMS error (`params_optim.json`: `"error": 4.342`) and 482 GCPs
(`ortho/data/gcp.csv`) are not reported anywhere in the manuscript.

### 2.8 Elevation exclusion — quantified (supports D4)

```
mapped domain (both years non-NA)              1,206,233 px
elevation range over mapped domain             2361.2 – 3005.2 m
pixels at or above 2,560 m                       799,744 px  (66.3 %)
Sasa 2012 pixels above 2,560 m                        22
Sasa 2021 pixels above 2,560 m                        71
highest Sasa pixel 2012 / 2021                 2625.5 m / 2711.5 m
```

So "areas above 2,560 m—where *Sasa* does not occur" (`matmet.qmd:87` / L249–250) is factually
wrong, and the exclusion silently removes two thirds of the scene from every map in Figures 7–9.
This is the source of the grey/white area Reviewer 3 asks about ("Figure 7: black areas are
unclear"; "explain the white area").

---

## 3. Methodological statements in `matmet.qmd` that the code contradicts or does not support

This is the highest-value section. Each item quotes the manuscript sentence and the code that
disagrees.

### M1 — The TDM distance predictor is not the one described (critical)

> **Manuscript** (`matmet.qmd:77`, PDF L233–235): "In the TDM, distance from the 2012 *Sasa*
> distribution was included as an additional explanatory variable to represent dispersal
> limitation."
> **Manuscript** (`matmet.qmd:137`, PDF L282–283): "Using ensemble models built for each of the
> TBM and TDM, we predicted *Sasa* distribution for 2030 based on the estimated snowmelt DOYs".

**Code.** Three different distance surfaces are used:

```r
# sdm_tdm.R:44-47  -- TRAINING
sasa_dist <- sasa12_ras %>% filter(sasa == 1) %>% distance() %>% rename(dist = sasa)

# sdm_tdm.R:238-250 -- 2021 PREDICTION: 2012 Sasa POLYGONS filtered to area > 5 m^2
sasa_pol_12 <- ... filter(area > units::set_units(5, m^2)) ...
sasa_dist_12 <- terra::rasterize(sasa_pol_12, sasa_dist) %>% filter(layer==1) %>% distance()

# sdm_tdm.R:252-255, 269 -- 2030 PREDICTION: 2021 Sasa POLYGONS > 5 m^2
sasa_pol_21 <- ...
sasa_dist_21 <- terra::rasterize(sasa_pol_21, sasa_dist) %>% filter(layer==1) %>% distance()
env_data_30 <- c(terrain, snow_30, sasa_dist_21)
```

**Recomputed.** The > 5 m² polygon filter drops **1,473 of 8,547** 2012 *Sasa* pixels (17 %).

```
mean |train_dist - pred2021_dist| = 67.8 m   max = 342.8 m   87.3 % of cells differ by > 1 m
mean |train_dist(2012) - pred2030_dist(2021)| = 62.7 m   max = 291.2 m
cells at dist == 0 : 8,547 (train) / 7,074 (2021 map) / 8,333 (2030 map)
```

So the model is applied out-of-distribution **even for its own 2021 hindcast** — not just for the
2030 projection. Reviewer 2 spotted the 2012→2021 switch in the projection; the training-vs-2021
mismatch is a second, independent divergence that has not yet been raised.

### M2 — "we fitted a linear regression … the mean regression coefficient was −0.86" (critical)

> **Manuscript** (`matmet.qmd:77`, PDF L224–227).

**Code.** The regressions that produce the model predictors are
`preprocess_snow_data.R:143–150` (`nest_by` + `broom::augment`) and `:174–181`
(`multidplyr` + `predict(..., newdata = data.frame(year = 2030))`), both operating on `snow_reg`
which is filtered `filter(snowmelt > 0)` (line 115). These reproduce `fitted_2021.tiff` and
`fitted_2030.tiff` **exactly**, and their mean slope is **−0.7147**, not −0.86.

The −0.86 comes from `data/snow/snow_reg.tif`, whose generating block
(`preprocess_snow_data.R:118–126`) does not parse and whose documented `lm.r2 > 0.2` filter
(line 130) is not present in the archived file. So the headline "8.6 days per decade" is
attributable to an artefact that (i) is not the predictor, (ii) is not reproducible from the
published code, and (iii) is inconsistent with the regressions that *were* used.

Additionally, the manuscript reports a point estimate with **no uncertainty** anywhere; the
scene-level trend is not significant (p = 0.20, 95 % CI [−2.45, +0.61] d/yr).

### M3 — "We applied four classification algorithms" (critical, reproducibility)

> **Manuscript** (`matmet.qmd:117`, PDF L263–265): "We applied four classification algorithms:
> gradient boosted trees (GBT), maximum entropy (MaxEnt), random forest (RF), and generalized
> additive models (GAM)."

**Code as written** (`sdm_tbm.R:94–120`): GAM, MaxEnt and XGB are **commented out**; only
`rf = sdm_spec_rf()` is active. `update_workflow_model("default_gam", ...)` is also commented out.
Line 127 then saves to `models_wo_dist_twi.rds` and line 129 immediately reads it back —
**that file does not exist in the repository**, and the file the rest of the analysis depends on
(`models_wo_dist.rds`) is never written by this script.

**Archived artefact** `models_wo_dist.rds` contains all four workflows
(`default_rf`, `default_gam`, `default_maxent`, `default_xgb`). So `sdm_tbm.R` as published
cannot regenerate it. `sdm_tdm.R:105–131` does have all four active.

### M4 — Predictor list vs. what the code globs (important)

> **Manuscript** (`matmet.qmd:75`, PDF L216–218): "Both models incorporated elevation, slope,
> aspect, roughness, the Terrain Ruggedness Index (TRI), the Topographic Position Index (TPI),
> and snowmelt day of year (DOY)".

**Code** (`sdm_tbm.R:13–19`, `sdm_tdm.R:13–19`):

```r
terrain <- list.files("data/terrain_features/", full.names = T) %>% str_subset(".tif$") %>% rast()
```

`data/terrain_features/` currently holds `TPI.tif TRI.tif aspect.tif roughness.tif slope.tif
tateyamadem_small.tif` **and `twi.tif`**. A clean run today therefore fits an 8-predictor
(9 for the TDM) model including a Topographic Wetness Index the manuscript never mentions.

**Archived artefact.** `model_stack_wo_dist.rds$train` columns are
`sasa geometry aspect roughness slope elevation TPI TRI snow` and
`model_stack.rds$train` adds `dist`. **No `twi`.** So (b) matches (c); only (a) diverges. The
earlier lead that the fitted models contained TWI is **retracted**.

### M5 — Variable importance is computed two different ways (important)

> **Manuscript** (`matmet.qmd:133`, PDF L277–280): "For both the TBM and TDM, variable importance
> was assessed using permutation loss, defined as the reduction in TSS observed when each
> explanatory variable was randomly permuted."

**Code.**

```r
# sdm_tbm.R:169-190  -- TEST set, DALEX::explain
sasa_double <- df_test %>% ...
explainer <- model_stack %>% explain(data = df_test %>% ... , y = sasa_double, ...)

# sdm_tdm.R:176-200  -- TRAINING set, DALEXtra::explain_tidymodels
sasa_double <- df_train %>% ...
explainer <- model_stack %>% explain_tidymodels(data = df_train %>% ... , y = sasa_double, ...)
```

Different evaluation data (test 5,627 rows vs. train 16,596 rows) and different explainer
constructors. `DALEX::model_parts` is called with defaults, i.e. **`N = 1000`** (a 1,000-row
subsample) and **`B = 10`** permutations — the manuscript gives neither. Because the TBM's
importances come from held-out data and the TDM's from training data, the two panels of Figure 6
are **not on a comparable footing**, yet the Results and Discussion compare them directly.

Both calls also pass `predict_function_target_column = "presense"` (misspelt;
`sdm_tbm.R:179`, `sdm_tdm.R:188`). In this configuration the custom `predict_function` returns a
tibble containing `.pred_class`, which `my_tss()` consumes directly, so the argument appears to be
inert — but it is a live hazard if anyone changes `predict_function`, and it should be removed.

### M6 — "TSS … ranges from 0 to 1" and the reported metric is `tss_max` (important)

> **Manuscript** (`matmet.qmd:117`, PDF L267–270).

TSS = sensitivity + specificity − 1 ∈ [−1, 1]. Also, `metric_set(tss_max)` and
`sdm_metric_set()` compute the TSS at the **best** threshold; the maps and every area figure use
a fixed 0.5. Recomputed optimal thresholds: 0.36 (TBM), 0.12 (TDM). The manuscript's TSS equation
introduces `TN` without defining it while defining `FP` and `FN` that do not appear in the
`specificity` line as printed.

### M7 — "areas above 2,560 m—where *Sasa* does not occur" (critical)

Contradicted by the data: 22 (2012) / 71 (2021) *Sasa* pixels above 2,560 m, maximum 2,711 m
(2021). And the exclusion removes 66.3 % of the mapped scene, which the manuscript does not say.

### M8 — "images taken in September–October of 2012 and 2021" (important)

Archived EXIF timestamps include 27 Aug 2012, 24 Aug 2021, 31 Aug 2021; the alignment report
also lists 28 Aug 2012. The classification therefore uses late-August imagery, which matters
because the method keys on **autumn leaf-colour change**.

### M9 — CRS / DEM description (important)

> **Manuscript** (`matmet.qmd:75`, PDF L219–220): "derived from a 5 m digital elevation model
> (DEM) … resampled to 1 m resolution to match the vegetation maps."

Actual: `data/terrain_features/*.tif` are **EPSG:3099 (JGD2000 / UTM 53N)** with **4.97 × 6.16 m**
cells, while `vege_*.tiff` and `data/snow/*` are **EPSG:6690 (JGD2011 / UTM 53N)**.
`terra::resample(terrain, vege12)` aligns the grids without reprojecting and keeps the 3099 label.
The datum shift in Toyama is centimetric so the geolocation impact is negligible, but the mismatch
is real and it **breaks a clean run**: with the installed `tidysdm`,
`thin_by_cell(df_21_1, sampling_mask)` aborts with `Error: CRS mismatch`. I had to insert
`crs(terrain) <- crs(vege12)` to reconstruct the modelling table at all.

Also note `fitted_2021.tiff` / `fitted_2030.tiff` are offset by half a pixel from the vegetation
grid (x origin 732743.5 vs 732744), so `resample()` silently interpolates the snow predictor.

### M10 — "aspect" enters as raw degrees (undocumented modelling choice)

`data/terrain_features/aspect.tif` ranges 0–359.9997 and is used unchanged. Aspect is circular;
0° and 359° are adjacent in reality but maximally distant to every learner. No sin/cos
(northness/eastness) decomposition anywhere. `sdm_tbm.R:431` contains
`aspect = if_else(aspect < -180, aspect + 360, aspect)` — dead code implying the author once
expected a −180…180 encoding. The manuscript simply lists "aspect" as a predictor.

### M11 — Predictor collinearity is never assessed (Reviewer 2's request)

Recomputed on the **actual TDM modelling table** (n = 20,891):

```
VIF:  aspect 1.16 | roughness 18.75 | slope 10.82 | elevation 1.33
      TPI 1.20    | TRI 27.17       | snow 1.53   | dist 1.63

Spearman: roughness–TRI 0.97, slope–TRI 0.96, roughness–slope 0.93,
          snow–slope -0.40, snow–TRI -0.39, snow–TPI -0.33, dist–elevation 0.39
```

TRI, roughness and slope are three near-duplicates. This is the mechanism by which permutation
importance is diluted across them (visible in Figure 6, where all three sit near zero), and it is
exactly the objection Reviewers 2 and 3 raise. The manuscript makes no statement about
collinearity, so this is an omission rather than a contradiction — but it is a **checkable** one.

### M12 — Vegetation-classification detail the Methods omit (Reviewer 3's L164–170 "blackbox")

`scripts/vegetation_classification/run_rnn.py` shows that each year's map is a **5-fold
cross-validation ensemble combined by majority vote**:

```python
rnn.kfold(100, f"cv/{year}/", k=5, shuffle=True)      # 5 folds, 100 epochs, test_size=0.2
preds = torch.Tensor(np.stack(preds)); pred, _ = torch.mode(preds, dim=0)
```

with 5 × 5 pixel patches (`prepare_data.py:3,5`) and a 500-unit/500-length parameter. None of
this — folds, majority vote, patch size, epochs — is in `matmet.qmd:39`. **No classification
accuracy, confusion matrix or class-wise F1 is reported anywhere in the manuscript for the 2012 or
2021 maps**; the text simply cites @Okamoto2024RSEC.

Further inconsistencies inside the classification code:

* `run_rnn.py:22` iterates `years = ["2012", "2015", "2021"]` and reads `../data/{year}`, but
  `prepare_data.py` writes `../data/2012_5x5/` and `../data/2021_5x5/`. The paths do not line up,
  so the published script cannot run as-is.
* `run_rnn.py:16` comments class 5 as "ダケカンバ" (*Betula ermanii*), whereas
  `plot_vegetation_map.R:37` and `calculate_diff.py:10` both say class 5 = maple / kaede
  (*Acer tschonoskii*), which is what the manuscript reports. The comment is stale; the manuscript
  matches the two authoritative mappings.
* `apply_mask.py:10` sets masked pixels to `0`, but in `calculate_diff.py:6` `0` is the code for
  ***sasa***. Had that pair been run in sequence on 0-based arrays, every masked pixel would have
  been counted as *Sasa*. The archived rasters carry a **distinct** value 0 alongside classes 1–7,
  so the actual pipeline evidently differed — but the published scripts as they stand encode a
  silent collision, and the SDM scripts then treat those 0-pixels as *Sasa* **absence**
  (`ifelse(layer == 1, 1, 0)`), not as no-data. 2,766 such pixels lie inside the modelled
  < 2,560 m domain.

### M13 — Statements about the *Sasa* / shrub occlusion mechanism

> **Manuscript** (`matmet.qmd:44`, PDF L183–188 and `results.qmd:12`, PDF L297–300): losses are
> attributed to shrub encroachment obscuring *Sasa*.

No code tests this. The transition matrix (§2.1) shows the single largest loss category is
conversion to **dwarf pine** (46.8 %), a prostrate conifer for which the "canopy occlusion"
argument is weakest. The manuscript's own Discussion (`discussion.qmd:27`, PDF L403–405) calls
sub-canopy *Sasa* "rare at the study site", which sits awkwardly with using occlusion to discard
2,467 m² of mapped loss.

---

## 4. Manuscript ↔ code: presence/absence audit

### 4.1 Things the manuscript describes that no code in the repository does

| Manuscript statement | Status |
|---|---|
| The areas 8,542 / 10,170 / 4,095 / 2,467 m² and Figure 4 (`expanded_area.pdf`) | **No script computes them.** Nothing in `scripts/` or `ortho/georectify.R` produces `data/sasa_inc.tiff` (which `sdm_tbm.R:446` and `analyse_sdm.R:28` *read*) or `expanded_area.*`. |
| Figure 2 English vegetation maps (`files/2012_5x5_en.jpg`, `2021_5x5_en.jpg`) | `plot_vegetation_map.R` writes `2012_5x5.png` / `2021_5x5.png` with **Japanese** labels. The English versions were made by code not in the repo. |
| Figure S1 (`snowmelt_shifting.png`) as captioned ("relative to 2011 (set as 0)") | The archived PNG is a **boxplot of anomalies**; `preprocess_snow_data.R:211–227` writes that filename with a **scatter + lm smooth of raw DOY**. Divergent. |
| "The ensemble TSS was subsequently evaluated on the independent test dataset" | ✔ done (`sdm_tbm.R:155–161`, `sdm_tdm.R:163–168`). |
| Sensitivity to alternative HS thresholds | Not done anywhere (Reviewer 1 asks for it). |
| Any uncertainty on the snowmelt trend | Not computed anywhere. |
| Any classification accuracy for the 2012/2021 maps | Not computed anywhere. |
| "The data and scripts supporting this study are openly available" (`index.qmd:70–71`, PDF L445–446) | The tracked repository contains **no** `vege_*.tiff`, **no** `data/snow/raw/*.tiff`, **no** `fitted_*.tiff`, **no** `.rds` models, **no** `sasa_pred_*.tiff`. `.gitignore` excludes `*.tiff`, `*.rds`, `*.npy`, `*.zip`. Only `ortho/data/terrain_features/*.tif` (7 files, incl. the undocumented `twi.tif`) and `ortho/data/snow/snow_{mean,sd,reg}.tif` survive, because `.tif` is not excluded. |

### 4.2 Substantive things the code does that the manuscript does not describe

1. **`filter(elevation < 2560)` removes 66.3 % of the mapped scene** and is described as removing
   only areas "where *Sasa* does not occur". (`sdm_tbm.R:59`, `sdm_tdm.R:68`)
2. **`filter(dist > 0)` for the TDM** is described (L239–243), but the resulting sample is not:
   presences fall from 9,974 to 3,973 (−60 %), absences from 17,081 to 16,918. So the two models
   are fitted to substantially different presence prevalences (36.9 % vs 19.0 %) — this is
   precisely Reviewer 2's "different response domains" point, and it is quantifiable.
3. **The 5 m² minimum-polygon filter** applied to *Sasa* patches for every distance surface and
   for the "become unsuitable" denominators (`sdm_tbm.R:243–250`, `sdm_tdm.R:229–245`). Removes
   17 % of 2012 *Sasa* pixels and 18 % of 2021.
4. **The distance surface swap** (M1).
5. **`fitted_2030.tiff` values are clamped to [0, 255]** (`preprocess_snow_data.R:184–185`),
   so extrapolated snowmelt DOYs cannot go negative — an implicit floor on the projection.
   `fitted_2030.tiff` min/max = 0 / 255, i.e. the clamp is active.
6. **The snowmelt DOY source rasters are bounded to DOY 120–230** (filenames
   `…_120-230_BW.tiff`), and observations with `snowmelt <= 0` are dropped per-pixel, so different
   pixels are fitted with different numbers of years.
7. **Figure S3 clips shifts** : `plot_snowmelt_shifts_map.R:38`
   `ifelse(abs(fitted_2021 - fitted_2012) > 50, NA, ...)` blanks any pixel whose 2012→2021 shift
   exceeds 50 days. The caption does not say so.
8. **Figure 7's two panels use different colour scales** (TBM legend spans ≈ 0.2–0.7, TDM
   ≈ 0.1–0.6) because `scale_fill_gradient2` auto-scales per panel. The panels are visually
   comparable but numerically are not.
9. **Figure 8 shows the 2021 maps, not the 2030 maps** (`results.qmd:98–105` builds the top row
   from `hsmap_tbm_2021.jpg` / `hsmap_tdm_2021.jpg`; verified by opening `files/future.jpg`, whose
   panel titles read "Habitat Suitability map of Sasa (TBM, 2021)" and "(TDM, 2021)").
   `hsmap_tbm_2030.jpg` / `hsmap_tdm_2030.jpg` exist in `paper/files/` but are never included.
10. **Ensembles contain no RF and no GAM members** (§2.3).
11. **Dead / abandoned code that a reviewer will find**: `sdm_tdm.R:36–37`
    (`sasa12_ras %>% terra::as.polygons() %>% filter()` — a no-op);
    `sdm_tdm.R:277–280` `is_others <- ifelse(layer == 6, 1, 0)` (class 6 = montane alder, not
    "Other Vegetation" which is class 2) — never used downstream;
    `sdm_tbm.R:412–444` and `:446–488` exploratory plots, one of which
    (`sdm_tbm.R:441–444`) is broken by a missing `+`;
    `analyse_sdm.R` in its entirety (reads `data/selected_comms.tiff` and `sasa_pred_sdm_12.tiff`,
    produces no manuscript output);
    `ortho/models_all_5m.rds` (a 4-algorithm workflow set with 54 MaxEnt / 54 XGB candidates and
    three metrics) — an abandoned experiment not referenced by any script.
12. **Three different, non-existent `setwd()` roots**: `~/doctoral_thesis/chap2/ortho/`
    (`sdm_tbm.R:1`, `sdm_tdm.R:1`, `preprocess_snow_data.R:1`), `~/Projects/jasms2023f/ortho/`
    (`analyse_sdm.R:1`, `georectify.R:1`), `~/Projects/jasms2023f//` (`plot_vegetation_map.R:5`).
    `plot_snowmelt_shifts_map.R` has none. There is no driver script and no stated execution order.
13. **Hard-coded hardware assumptions**: `num.threads = 18` (both SDM scripts),
    `new_cluster(22)` (`preprocess_snow_data.R:172`).
14. **The archived `.rds` models can no longer be used.** Loading `model_stack.rds` and calling
    `predict()` fails with
    `'xgb.Booster' object is corrupted or is from an incompatible XGBoost version.`
    The XGBoost members were serialised by an older XGBoost. Any reviewer who downloads the models
    (were they archived) could not run them.

---

## 5. Leads from `known_leads.md` — status after verification

| Lead | Verdict |
|---|---|
| `prop = 0.2` gives a 20 % training set | **RETRACTED.** `spatial_initial_split` yields 79.2 % / 79.3 % training. The manuscript's 80/20 is correct. |
| Fitted models contain `twi` | **RETRACTED.** Neither archived stack contains `twi`. But a clean run *would* add it (M4). |
| `preprocess_snow_data.R` writes `snow_*.tif` into `data/terrain_features/`, injecting predictors | **PARTLY RETRACTED.** The code does say `data/terrain_features/snow_mean.tif` etc. (lines 67, 96, 137), but the archived files live in `data/snow/`. If the script were run as written it *would* inject them — so this is a live hazard for a clean re-run, not something that happened. |
| `preprocess_snow_data.R:118–126` is syntactically invalid | **CONFIRMED.** Bare `partit`, `$coeffisients`, `$adj.r.squared` on an `lm`. |
| `sdm_tbm.R` trains only RF; saves `models_wo_dist_twi.rds` which does not exist | **CONFIRMED.** |
| TDM training vs. prediction distance differ | **CONFIRMED and quantified** (68 m mean difference; M1). |
| VI computed on different data / different explainers | **CONFIRMED** (M5). |
| `predict_function_target_column = "presense"` | **CONFIRMED as present; assessed as inert** in this configuration. |
| aspect enters as raw 0–360 degrees | **CONFIRMED** (M10). |
| Ensembles retain only MaxEnt + XGB (TDM 7, TBM 4) | **CONFIRMED from `member_fits`.** |
| `is_others == 6` contradicts the class table | **CONFIRMED and it is dead code** (never used downstream in `sdm_tdm.R`). |
| Figure 9 is the TBM version | **CONFIRMED** (`results.qmd:119` uses `files/risky_tbm.jpg`). |
| 2021 vs 2030 rasters have different valid-pixel counts | **CONFIRMED**: 397,403 vs 407,658. |
| Figure 7 panels use different colour ranges | **CONFIRMED** (TBM ≈ 0.2–0.7, TDM ≈ 0.1–0.6). |
| Established areas 8,547 / 10,176 / 4,097 / 2,468 vs manuscript 8,542 / 10,170 / 4,095 / 2,467 | **RECONCILED.** The manuscript used `terra::expanse()` (geodesic, default `transform = TRUE`); the leads used pixel counts. Both are right; the ratio is 0.99941. |
| VIF: TRI 34.3, roughness 19.5, slope 12.9 (200k-pixel sample) | **CONFIRMED in kind**; on the actual TDM modelling table: TRI 27.2, roughness 18.8, slope 10.8. |
| Class codes 1 Sasa … 7 Dwarf pine | **CONFIRMED** by `plot_vegetation_map.R` and `calculate_diff.py`; note the extra value **0** (mask) in the archived rasters. |
| `run_rnn.py` paths / year 2015 do not match the repo layout | **CONFIRMED.** |
| `apply_mask.py` collapses masked areas to 0, ambiguous with a real class | **CONFIRMED as a latent collision** in the published scripts; the archived rasters show it did not in fact happen (a distinct 0 code exists alongside 1–7). |

---

## 6. Reproducibility: what actually happens on a clean run

Executed on this machine (R 4.5.2, terra/GDAL 3.12.1, tidysdm current):

1. All scripts `setwd()` to non-existent paths → immediate failure unless edited.
2. With the working directory fixed, `sdm_tbm.R` / `sdm_tdm.R` glob `twi.tif` → an extra predictor.
3. `thin_by_cell(df_21_1, sampling_mask)` **aborts** with `Error: CRS mismatch` because the
   terrain rasters are EPSG:3099 and the vegetation rasters EPSG:6690. Requires
   `crs(terrain) <- crs(vege12)` to proceed.
4. `sdm_tbm.R:127–129` writes then reads `models_wo_dist_twi.rds` — the rest of the script
   depends on an object the repository does not contain and the published code does not
   reconstruct (only RF is enabled).
5. `preprocess_snow_data.R:119–126` does not parse.
6. The archived `.rds` model stacks cannot `predict()` under the installed XGBoost.
7. None of the input rasters the scripts read are in the public repository.

---

## 7. Suggested minimal set of re-analyses

Ordered by how badly a reviewer will react if it is missing.

1. **Recompute the snowmelt trend properly** and replace −0.86 / 8.6 d/decade with the value
   actually derived from the layers used (−0.71 d/yr, 7.1 d/decade), with mean, median, SD, CI,
   the 91.7 % negative-slope fraction, and the non-significant scene-level test (p = 0.20).
   Delete or explain `snow_reg.tif`.
2. **Re-fit or re-document the TDM distance predictor** so training and prediction use the same
   definition, and state the 2030 dynamic update explicitly (Reviewer 2).
3. **Rebuild Figure 8** from `hsmap_*_2030.jpg`, or rewrite the caption.
4. **Report gross gain / gross loss / net change separately** everywhere, and add the transition
   matrix from §2.1 (Reviewers 1 and 2).
5. **Justify or calibrate the 0.5 threshold**, and report the sensitivity of all areas to
   alternatives (recomputed optima: 0.36 TBM, 0.12 TDM).
6. **State the < 2,560 m restriction honestly**, including that it removes 66 % of the scene and
   that *Sasa* does reach 2,711 m.
7. **Make variable importance comparable** (same data split, same explainer, state `N` and `B`)
   and add response / partial-dependence curves for snowmelt DOY (Reviewer 1).
8. **Add the collinearity assessment** (VIF table + Spearman matrix from M11) and either drop TRI
   and roughness or combine them.
9. **Fix the 21 % → 22 %**.
10. **Fix "September–October"** to "late August–October", and **"TSS ranges from 0 to 1"** to
    "−1 to 1".
11. **Deposit the actual data** (vegetation rasters, raw snow rasters, fitted snow layers,
    prediction rasters) in a DOI-issuing repository, with a driver script, an execution order, a
    `renv.lock`, and no `setwd()`.
