# 投稿前最終レビュー集約（2026-09-09）

6体の独立エージェント（Abstract/Intro/Conclusion、Methods/SI、Results/Discussion、数値監査、応答書、PDF検査）の結果を統合。
実施した編集の全差分は `final_review_2026-09-09_edits.patch`。日本語版（paper/ja/）は未同期。

## 1. 実施した編集（意味・主張を変えないもの）
- 文体・誤植: index 2件、intro 6件、results 8件、discussion 3件、matmet 8件、supplement 3件、response 6件
- 数値・レンダリングの誤り修正（解析出力で確認済み）:
  - 図S6キャプション 10,176 → **10,174** セル（`model_A_data_summary.csv` n_presence、表S1列和と一致）
  - 図5キャプション 交差年 2029 → **2028**（`ca_trajectory.csv`: 2028は s=−2.24 が上、2029で逆転）
  - 表S11 の `\setcounter{table}{10}` 欠落を補い、キャプションの手書き "Table S11:" を削除（PDFで Table S8 と重複していた）
  - SI 隣接性の帯ラベル "2–3 m" → "1.5–3 m"（表S6と一致）
  - 図4・図6キャプションの `≥` `●` `▲` が PDF で空白化 → 平文（"at 160 m and beyond", "filled circles/triangles"）に置換
- 注意: 序論の「衛星のパターンを1 m観測を通じて検証する」文が "examined alongside" に弱まった（intro 第2段落末）。元の含意を残すなら "examined through" へ戻す。

## 2. 提案（未編集）— 投稿前に必須
| # | 内容 | 根拠 |
|---|---|---|
| P1 | Author Contributions / Conflict of Interest / Acknowledgements / Funding が原稿に無い（E&E 必須） | Abstract班・PDF班 |
| P2 | Zenodo DOI の TODO 2箇所（index.qmd, matmet.qmd）。応答書 AE-1 が「記載済み」と述べている | 全班 |
| P3 | 応答書: `[new L##]` 88件、Author check 21件、冒頭の Draft preparation note を投稿前に除去。うち内容が陳腐化した注（図S6 未完 ×2、R3-16「still needs completion」、R2-04 表S11未追加）は事実と矛盾 | 応答書班 |
| P4 | `spatialsample` は実際には未使用（`sf::st_make_grid` + `rsample::manual_rset`）。Methods の記述とソフトウェア一覧を訂正 | Methods班（コード確認済み） |
| P5 | 隣接性解析の手順が Methods に無い（Results/Discussion/SI のみ）。@sec-sasa-change に追記案あり | Methods班 |
| P6 | 融雪トレンドの検定（t検定・BH補正・景観平均回帰、有効年≥8）が Methods に無い | Methods班 |
| P7 | 表S1 の非ササ行合計 1,192,389 vs モデルB n = 1,197,536（差 5,147）。部分集合のはずが逆転。マスク差の確認と脚注が必要 | 数値監査・Methods・応答書班が独立に指摘 |
| P8 | 融雪窓の表記: 本文/Abstract 150–185、図3キャプションと**図中ラベル** 151–180（`fig03_response_curves.R` の 90% 高さ定義）。定義を一本化（本文を 150–180 に寄せるか、2段構えで記述）。PDP 上はどちらも誤りではない | 数値監査 |
| P9 | GBT 比較モデルは陰性150,000セルのサブサンプルで学習しており「same response, predictors, and folds」は不正確 | Methods班 |
| P10 | 盲検判読の盲検性（判読者は全件を減少セルと想定）の事後記述が SI に未反映（決定事項 2026-09-08） | Methods班 |
| P11 | export.bib: `Philips2006MaxEnt` の著者が別論文（Aneja ら）。Amagai2022・Thomas2004・Grabherr2003・Hock2019・Mahoney2023・Yoshida2016・Iida2006 のフィールド破損、URL 3本連結 7件、DOI 大文字/サフィックス | PDF班・Methods班 |
| P12 | SI の `[@GSIdem]` が `[? ]` で出力（SI に文献リストが無い）。応答書 R2-04 の「最大 −0.58」は表S11 の 0.63（TWI×融雪）と矛盾 | PDF班・応答書班 |
| P13 | タイトルページ: 所属の department/住所が脱落、Corresponding author の * が付かない、Ide 氏が別部署なのに同一所属記号 | PDF班 |
| P14 | index.pdf は index.qmd より古い（再レンダリング必須） | PDF班 |

## 3. 提案 — 推奨（MAJOR）
- Results に解釈が2箇所混入（"Thus, Sasa had a favourable range…"、MOE 植生図の粗さの理由）。Discussion で Results の数値再掲（10%/1%、39 cm/年 ×2）
- 「almost all apparent loss to dwarf pine was a boundary phenomenon」は機構名でなく位置の記述に限定する。§Shrubification の「相互置換」示唆も同じ留保を通す
- Conclusion にシナリオ限定句が無い。Conclusion の 5,000–6,000 m² を 4,900–5,700 に揃える
- 「nearby Murodo-daira」(intro/discussion) と「Murodo-daira and the surrounding slopes」(study site) の地理的矛盾
- アンサンブル重み「95%」は実測 96.6%（上位2メンバーなら 95.3%）。表S4「Other」10 → 11 m²。VIF「below 2.3」vs「2.30」
- SI 図表の本文からの番号参照が不足（図S4・S7、表S5–S8・S10–S12）。E&E 区分名は "Supporting Information"
- モデルA 応答曲線が partial dependence（DALEX）である旨、ブロック幾何（10×10 グリッド、約180 m）、TPI/TWI の半径・アルゴリズム・バージョン、パッケージ版、Python 環境が未記載
- k 感度表の HTML コメント TODO: A案（k 値を1文で報告し TODO 削除）を推奨
- SI 図の再作成: S3・S4（文字 4–5 pt）、S6（Sasa 不可視、ページ番号とキャプション衝突）、S7（magenta 不可視）、S2/S5（右端目盛切れ）
- 応答書トーン: R2-12 の推奨文献批判、"We do not claim" 構文の多用、AE 節の冒頭一文欠落
- 有意性の二値表現（"no significant trend" → "little evidence for a trend"）、本文の太字強調解除、キーワード "colonization model" → "establishment model"、図中 "colonization" と本文 "establishment"/英式綴りの不統一
- Abstract 374語（E&E の上限は未確認。300 なら約75語削減案あり）

## 4. 数値監査で一次情報が無く照合できなかった値
164.1×10³ m²（閾値0.158の行が `model_A_threshold_sensitivity.csv` に無い）、84.7×10³ m²（図S5）、220セル（図S7）、モデルB ピーク DOY 175（CSV 無し）、GCP 残差約2 px。Zenodo 前に出力へ追加を推奨。
`model_B_blocked_metrics.csv` は採用前（線形距離）仕様の値（AUC 0.9365）であり、README での注記が必要。

## 5. PDF班が誤認と判定した指摘
- 図6「Dashed lines」: スクリプトは linetype "22"（破線）で正しい。

---

# 反映結果（2026-09-09 後半、ユーザー承認後）

## 実施
- 必須14件のうち P1（Acknowledgements・COI・Author Contributions 節）、P3（応答書の Author check 21件・冒頭注の削除、陳腐化記述の更新、トーン修正、−0.58→0.63）、P5・P6（隣接性・トレンド検定の Methods 追記）、P7（モデルB の n に未分類5,147セルを含む旨を Methods に注記。表S1 は両年分類済みセル）、P8（融雪窓 150–185 に統一。図3 は PDP ≥85% で 151–184 を描画）、P9、P10、P11（bib 修正。inbook→incollection、MaxEnt 著者、URL/DOI 整形、固有名詞保護）、P12（SI に bibliography 追加、文献リスト出力）、P13（YAML を quarto-journals/elsevier 形式に書き換え。所属・住所・責任著者 * が出力）、P14（3 PDF 再レンダリング）。
- P4（spatialsample）はユーザー指示により変更なし（tidysdm 経由で使用）。
- 推奨項目: Results の解釈文削除・太字解除・有意性表現、Discussion の数値再掲削減・境界解釈の限定・相互置換の留保・freeze–thaw・海洋性環境の留保、Conclusion のシナリオ限定と 4,900–5,700、Abstract 352語・TSS 展開・キーワード、SI 表S4 11 m²・97%・k 値報告・地形節独立化・盲検の事後記述・品質スクリーン注記・bin 記述・Fig S3 キャプション、"Supporting Information" 改称、SI 図表の本文参照、正/ゼロ傾き 8.3%/0.06% の本文転記、図2 白地の説明、Abstract のトレンド非検出。
- 図（Codex, gpt-6-astra）: 図3 窓 0.85、図4–6 の "establishment"/"days/year"、図S2/S5 目盛切れ、図S1/S3/S4 文字サイズ（保存済み出力から再描画）、図S6 高さ 190 mm・ササ輪郭、図S7 150 m 窓・218 セル。報告書: analysis/figures/figure_revision_report.md
- 整理: B1–B6, B8–B13 実施。B7 は未実施（リポジトリ側 npy を保持）。B3 は完全版（tateyama2.tiff 2.2 GB, georectified.csv, pointcloud.db）で ortho/data を置換。data/images/aligned/2015 を保全。run_crnn.py / crnn.py を scripts/vegetation_classification/ に配置。原投稿パッケージは archive/original_submission_2026-04/。

## 未解決（著者判断・入力が必要）
1. Author Contributions の CRediT 文（index.qmd に TODO と草案）
2. Zenodo DOI（index.qmd, matmet.qmd）
3. TPI/TWI の近傍半径・アルゴリズム名・QGIS/SAGA バージョン（記録なし。応答書 R3-25 では率直に未記載と回答）
4. 地域気候の定量要約（気温・降水平年値、積雪開始からの期間）
5. GSIdem / MOEvegmap2024 の year（2026 のまま）
6. 応答書の [new L##] 88件（最終 PDF の行番号で置換）
7. E&E の要旨語数・キーワード数・タイトル語数の規定（Wiley サイトが 403 で未確認。要旨は 352 語）
8. モデルB の未分類5,147セル: 注記で対応。再フィットするなら解析変更（提案のみ）
9. 図S7 凡例の "Sasa" が立体（他は斜体）。図1 パネル(d) のラベル衝突（pptx 由来）
10. 日本語版（paper/ja/）は本日の変更を未反映

---

# 追加調査（2026-09-09 夜）

## 地形予測子の来歴（確定）
- slope / aspect / TPI / TRI / roughness は **GDAL `gdaldem`** 由来（アーカイブ DEM から再導出してビット一致。slope・aspect は Zevenbergen–Thorne、TPI は 3×3 固定＝中心セルと8近傍平均の差、TRI は Riley）。R terra::terrain ではない（TRI が Wilson 式で不一致）。SAGA でもない。
- TWI のみ別系統（terra::writeRaster の署名、NA なし、MFD 系の集水面積と最小勾配ガードに整合）。SAGA のどのモジュールかは特定不能。著者申告「SAGA のデフォルト」で記述し、モジュール名・バージョンは未記録と明記。
- DEM: GSI 5 m メッシュの 0.2″×0.2″ グリッドを EPSG:3099 に投影したもの（4.97×6.16 m 非正方セル）。
- 反映先: matmet.qmd（Shared predictors）、supplement.qmd（Terrain predictors 節）、response R3-16/R3-25、export.bib に Beven1979TWI・Zevenbergen1987ESPL・Conrad2015SAGA を追加。
- SAGA の「デフォルト」はツール（TWI 単体／One Step／SAGA Wetness Index）と QGIS 側既定（内蔵プロバイダは流量集積が D8、Slope が度）で意味が変わる。原稿では既定値を列挙せず「default settings; module unrecorded」に留めた。

## E&E 投稿規定の適合性（paper/Ecology and Evolution.html）
- 反映済み: キーワード 7→6（"establishment model" を削除）、SI 冒頭にタイトル・著者、図1 の dpi メタデータ 300、カバーレター草案 paper/cover_letter_revision.qmd。
- **要判断1: Author Contributions**。チェックリストに "Author Contributions section" が明記され CRediT が mandate。投稿画面入力に加え、原稿内にも節が必要な可能性が高い。現在は削除済み（ユーザー指示）。
- **要判断2: SI の図表番号**。規定は「出現順に番号」。現行は日本語版との対応を優先した非連番（S1→S12→S6→S1…）。振り直す場合は本文・応答書の参照（87応答）も更新が必要。
- 未解決: Zenodo DOI（改訂時に正式アーカイブとリンクが必須と明記）、応答書の [new L##]、tracked-changes 版の要否（通常改訂は規定なし。ScholarOne の指示に従う）。
- 規定なし: 語数上限（"no word limits"）、要旨形式、タイトル語数、ダブルスペース、ORCID、graphical abstract。図は EPS/TIFF が "preferably" で PDF も許容。

---

# 最終状態（2026-09-09 22:00）
- Author Contributions 節を CRediT 草案付きで復元（役割は著者確認要。qmd 内コメント参照）。
- SI 図表を出現順に連番化（図 S1–S7、表 S1–S12。対応表は本節末尾）。本文・応答書の参照、図ファイル名、図スクリプト名を更新。日本語版 SI は画像パスのみ追従（本文の番号は旧のまま）。
- リポジトリ最終整理（約 1.9 GB 削除。README 5 本を新設、.gitignore を paper/ 追跡へ変更）。保持したが要確認: ortho/data/tateyama2.tiff（2.1 GB）、pointcloud.db（2.1 GB）、data_external/veg2024bk4.gpkg（531 MB）、archive/original_submission_2026-04（submit_files.zip は git 履歴に無い）。
- paper/ の整理: バックアップ・ビルド生成物・pre_revision_backup・未参照図を削除。Ecology and Evolution.html（投稿規定）と titlepage.docx は保持。
- git: 追跡 41 件削除・36 件変更・99 件未追跡（paper/ を含む）。**未コミット**。コミットは著者判断（ブランチ revision/hsm-redesign）。

SI 番号対応（旧→新）: 図 S6→S5, S7→S6, S5→S7（S1–S4 不変）。表 S5→S1, S12→S2, S1→S3, S6→S4, S10→S5, S7→S6, S11→S7, S2→S8, S8→S9, S9→S10, S3→S11, S4→S12。
