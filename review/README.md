# review/

Additional analyses carried out during the response to peer review, and the scripts
and tables that re-established the provenance of the original inputs. Results from all
of these are cited in the manuscript or the Supporting Information.

| Directory | Contents |
|---|---|
| `trend_sensitivity/` | Whether the per-pixel snowmelt trend, rather than the climatology, carries signal: `00`–`03` scripts, correlation and structure tables, the Model B refit with a trend term, the SI figures (S1, S3, S4), and `report.md` |
| `sasa_to_pine_adjacency/` | Whether apparent *Sasa* loss to dwarf pine is concentrated at boundaries: `adjacency_analysis.R`, distance-bin and isolated-cluster tables (SI Table S4, Figure S6), and `report.md` |
| `image_interpretation_package/` | Blind visual interpretation of 129 sampled cells in paired photographs (SI Table S5): sampling and overlay scripts, the browser tool, the sample key, the filled interpretation sheet, and `README.md` describing the protocol. Image crops and the `_cache/` are regenerable with `sample_loss_crops.py` and `make_blind_overlays.py` and are distributed through the Zenodo deposit rather than git |
| `restore/` | Scripts and small tables that re-derived, from the archived server copies, the classifier cross-validation metrics (`classifier/`, the source of SI Table S2), the georectification chain (`georect/`), the snowmelt rasters (`snow/`), and the original SDM (`sdm_scripts/`), with `restore_*.md` / `verify_*.md` recording what each check found |

The planning, audit and decision documents written during the revision are not part of
the deposit; they remain in the git history of this repository.
