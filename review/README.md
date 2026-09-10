# review/

Everything produced during the response to peer review: the audit of the original
submission, the decisions that followed from it, and the three additional analyses that
back the Supporting Information.

## Additional analyses (results cited in the manuscript / SI)

| Directory | Contents |
|---|---|
| `trend_sensitivity/` | Whether the per-pixel snowmelt trend, rather than the climatology, carries signal: `00`–`03` scripts, correlation and structure tables, the Model B refit with a trend term, the SI figures, and `report.md` |
| `sasa_to_pine_adjacency/` | Whether apparent *Sasa* loss to dwarf pine is a boundary phenomenon: `adjacency_analysis.R`, distance-bin and isolated-cluster tables, the SI map, and `report.md` |
| `image_interpretation_package/` | Blind visual interpretation of 129 sampled *Sasa*-loss cells in paired photographs: the sample, the blind overlays, the browser tool, the filled interpretation sheet, and `README.md` describing the protocol. The regenerable `_cache/` (camera fit, reconstructed mask, projected cell coordinates) has been removed; `make_blind_overlays.py` rebuilds it |

## Audit, provenance and decisions

`ece_review_summary/` holds the reports that establish what the original submission
actually did and what had to change:

- `review_summary_ja.md` — the reviewers' points and the response strategy
- `code_audit_report_ja.md` — audit of the original code and data, including the
  orphaned `snow_reg.tif` that produced the incorrect published −0.86 d/yr figure
- `sdm_provenance_report_ja.md` — what each original SDM artefact was, and what it
  could and could not answer
- `restoration_report_ja.md` — what could be recovered from the server copies
- `analysis_change_list_ja.md` — the full list of required changes
- `implementation_plan_ja.md`, `decisions.md`, `stage4_discussion_ja.md` — the plan and
  the decision record the new pipeline in `analysis/` implements
- `audit/`, `restore/` — the working notes, scripts and small tables behind those
  reports. `restore/classifier/` holds the classifier cross-validation metrics
  transcribed from the (since deleted) training run directories, and is the source of
  SI Table S2. The large binary reproductions have been removed; the conclusions drawn
  from them are in the reports above

## Planning and final checks

- `manuscript_restructure_plan_ja.md` — the restructuring plan the revision follows
- `figure_redesign_plan_ja.md` — the figure redesign plan
- `final_review_2026-09-09_ja.md` — the pre-submission review, its resolutions, and the
  points still needing author input
- `final_review_2026-09-09_edits.patch` — the full diff of the edits it produced
- `cleanup_plan_ja.md` — the repository cleanup plan carried out before deposit
