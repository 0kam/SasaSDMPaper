# リポジトリ整理計画（2026-09-09）

## 前提
- `paper/` は `.gitignore` で除外、`analysis/`・`review/`・`data_external/`・`data_from_server/` は未追跡。
  つまり**改訂作業の成果物は一切コミットされていない**（全ブランチが main と同一コミット）。
- 解析パイプライン（`analysis/00_config.R`）の入力は `ortho/data/{vege_2012_5x5.tiff, vege_2021_5x5.tiff, snow/raw/, terrain_features/}` と `data_external/veg_murodo.gpkg`。出力は `analysis/out/`。

## A. 実施済み（安全・再生成可能・追加的）
- 他 worktree の査読整理文書を `review/ece_review_summary/`, `review/restructure_plan/` に集約（コピー。元は残置）
- `paper-context.md` の参照パスを集約先に更新
- `__pycache__`、`.DS_Store`、空の `.Rhistory` / `.spl` を削除
- git: 追跡されていた `.DS_Store`・`__pycache__` を untrack、`.gitignore` に追加（ブランチ claude/paper-final-review-1fc65b, commit 7ea6ae2）

## B. 削除候補（要確認。合計 約 38 GB）
| # | 対象 | サイズ | 根拠 | 注意 |
|---|---|---|---|---|
| B1 | `data_from_server/scripts/runs/cv/` | 9.5 GB | 分類器 CV の学習ログ・チェックポイント。CV 結果 CSV は `review/ece_review_summary/restore/classifier/` に転記済み | サーバー原本の有無を確認 |
| B2 | `data_from_server/ortho/data/snow/` | 6.5 GB | `ortho/data/snow/raw/`（10ファイル）と `snow/aligned` の中間生成物。パイプラインは raw のみ使用 | raw 10 ファイルは `ortho/data/snow/raw` と同一（cmp 確認済み）。`terrain_features` も同一 |
| B3 | `data_from_server/ortho/data/{tateyama2.tiff, pointcloud.db, georectified.csv, 2012_5x5.csv, 2021_5x5.csv}` | 5.4 GB | 復元レポートで「転送時に切詰・破損」と判定済み（開けない） | 再コピーするなら削除してからサーバーから取り直す |
| B4 | `data_from_server/results/*.npy`, `data_from_server/data_source/{aligned,source,normalized,composite}` | 約 3.5 GB | `results/*.npy`・`data/images/` と同一（cmp 確認済み）または派生 | `aligned/2012, 2021` は git 追跡側と同一。ただし `aligned/2015`（教師ラベル年）と `aligned/old` は server 側にしかない → `aligned/2015` は `data/images/aligned/` へ移してから削除 |
| B5 | `review/ece_review_summary/restore/georect/out/` | 766 MB | 幾何補正の再生成中間ラスタ。検証済みで報告書に結論が転記済み | 元は worktree にも残る |
| B6 | `ortho/{models*.rds, model_stack*.rds, risky_area*.tiff, potential_sasa_area_21.tiff, sasa_pred_*.tiff}` | 約 190 MB | 原投稿時の旧 SDM（`scripts/sdm/`）の出力。現行解析は `analysis/out/` に置き換え済み | 原投稿との比較用に一時保持なら `archive/` へ |
| B7 | `results/*.npy`, `data/images/mask.npy` | 485 MB | B4 と同一物。git 追跡ではない | B4 と排他 |
| B8 | `paper.zip`, `paper/files.zip`, `paper/submit_files.zip`, `paper/submit_files/` | 97 MB | 原投稿（2026-04）の提出パッケージ | 原投稿の記録として 1 つだけ `archive/` に残す案 |
| B9 | `paper/files/` の旧図 67 ファイル（`*.eps`, `*.jpg`, `fig_*.png`）、`paper/files_original_size/` | 約 50 MB | 現行原稿が参照するのは `fig01–06_*` と `si/` のみ（grep 確認） | `SasaPaper_Figures*.pptx` は図1の元なので保持 |
| B10 | `paper/ja/index_ja_rev2–5.docx`, `index_ja.{pdf,tex,log}`, `index_ja.docx`（8/29） | 27 MB | 日本語確認版の旧リビジョン。rev6 が最新 | 共著者コメント付きなら保持 |
| B11 | `paper/{intro,matmet,results}.pdf`, `paper/index_files/`, `paper/.quarto/` | 36 MB | 2025-12 の節別 PDF と Quarto キャッシュ | `quarto render` で再生成可 |
| B12 | `paper/DDI_SubmissionQues.docx`, `covering_letter.doc` | 48 KB | D&D 投稿時の書類 | カバーレター雛形として使うなら保持 |
| B13 | `.claude/worktrees/{review-summary-ece-ddc69c, sasasdm-paper-content-review-c300bd}` | 3.1 GB | ブランチは main と同一。`review/` は A で集約済み | `git worktree remove` で削除 |

## C. 保持
- `analysis/`（コード・`out/`・`specs/`・`tests/`）、`paper/*.qmd`・`export.bib`・`_extensions/`・`files/fig0*`・`files/si/`・`SasaPaper_Figures*.pptx`
- `ortho/data/` の入力群、`ortho/georectify.R`、`data_external/veg_murodo.gpkg`（`veg2024bk4.gpkg` 531 MB は切出し元。再切出し不要なら B へ）
- `review/` の文書類・`trend_sensitivity/`・`sasa_to_pine_adjacency/`・`image_interpretation_package/`（SI 表 S10 の根拠）
- `data_from_server/scripts/{run_crnn.py, models/crnn.py}`（→ `scripts/vegetation_classification/` へ配置予定。paper-context チェックリスト）
- `data/`（git 追跡済み。ラベル JSON は「ダケカンバ」旧称の一掃対象）

## D. 提案: git 運用（Zenodo デポジット前提）
1. `.gitignore` の `paper/` を外し、`paper/.quarto/`, `paper/index_files/`, `paper/*.pdf`, `paper/*.tex`, `paper/ja/*.{docx,html,pdf}` を除外して **qmd・bib・図を追跡**する
2. `analysis/`（`out/models/`・大容量 tif 除く）、`review/*.md`、`data_external/veg_murodo.gpkg` をコミット
3. 上記を main へマージし、Zenodo にはリポジトリのスナップショット＋大容量入力（`ortho/data`、`analysis/out/models`）を別途添付
