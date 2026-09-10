# ortho/

Georectification of the time-lapse imagery, and the geospatial inputs to `analysis/`.

`georectify.R` maps image-space products into 1 m rasters on EPSG:6690 (JGD2011 /
UTM 53N) by joining them to `data/georectified.csv`, a dense lookup table from image
pixel `(u, v)` to world coordinates `(x, y, z)`, rasterizing, and filling gaps with a
short iterative modal focal filter. It produces the two vegetation rasters and the ten
snowmelt rasters that the analysis reads.

| Path | Role |
|---|---|
| `georectify.R` | The georectification script |
| `data/vege_2012_5x5.tiff`, `data/vege_2021_5x5.tiff` | **Reference grid and response.** 1801 × 1753 cells, 1 m, EPSG:6690. Classified vegetation for the two target years |
| `data/snow/raw/*.tiff` | **Snowmelt input.** Day-of-year of snow disappearance, ten years (2011–2018, 2020, 2021). Pixel value 0 means sky/invalid, not a melt date |
| `data/terrain_features/*.tif` | **Terrain input.** `tateyamadem_small.tif` is the GSI 5 m mesh DEM projected to EPSG:3099; `slope`, `aspect`, `TPI`, `TRI`, `roughness` are `gdaldem` derivatives of it and `twi.tif` is from SAGA. The analysis uses slope, elevation, TPI, TWI and the northness/eastness decomposition of aspect; TRI and roughness are kept for the record but are collinear with slope and deliberately unused |
| `data/georectified.csv` | The image-pixel-to-world lookup table (744 MB). The single most important provenance file for the whole workflow |
| `data/pointcloud.db` | SfM point cloud from which the camera model was estimated |
| `data/gcp.csv`, `data/params_optim.json` | Ground control points and the optimised camera parameters |
| `data/tateyama2.tiff` | Aerial orthophoto of the site |
| `data/mrd_dem_1m.tiff`, `data/dem_small.tiff` | 1 m DEM of the study area, and a coarser copy |
| `data/georectified.tiff` | Georectified photograph |

Everything under `data/` except `gcp.csv` and `params_optim.json` is too large for git
and is distributed through the Zenodo deposit.

The outputs of the original submission's SDM (habitat-suitability rasters, risky-area
maps, model performance figures) previously lived here. They were removed during the
revision; they remain in git history and their provenance is documented in
`review/ece_review_summary/sdm_provenance_report_ja.md`.
