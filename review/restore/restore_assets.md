# 復旧資産の評価 — sdm_sasainc.R / selected_comms.tiff / results/use_this

対象: サーバから新たに回収された 3 資産が、査読者の要求に直接答えられるかどうか。
すべて実行して確認した。コマンドと出力は各節に示す。

作業ディレクトリ（書き込みはここだけ）:
`/Users/okamoto/NIES/SasaSDMPaper/.claude/worktrees/review-summary-ece-ddc69c/review/restore/assets/`

---

## 結論（先に3行）

| 資産 | 判定 |
|---|---|
| **1. 定着モデル (sdm_sasainc.R)** | 本物の colonization モデルだが**未完成・未保存・シード無し**。復活はできた（実行済み）。ただし空間ブロック CV では **R² = 0.037** しか出ず、そのままでは論文を弱める。**より良い代替を実装して走らせた**（下記 §1.4）。それが Reviewer 2 への直接回答になる。 |
| **2. selected_comms.tiff** | **群落図ではない**。2012年のササパッチ（≥5 m²）のマスクで、値はポリゴン面積 (m²)。Reviewer 1 の問いには**答えられない**。環境省・富山県植生図へのフォールバック方針は妥当。 |
| **3. results/use_this/*.npy** | **公開分類図そのもの**。`image_to_csv.py` のコメントアウト部にソースが明記されており、バイト一致で確認。画像空間→地図空間の完全再現に成功、**画素一致率 99.83 % / 99.84 %**。再現性のギャップは閉じた。 |

**さらに、本作業中に最重要の manuscript-affecting な発見が1件出た**（§4）:
`sdm_include_distance.R` の 1 行 `filter(dist > 0)` が、TDM の在データから
**2012年時点で既にササだったセルを全部（9,974 中 6,001 = 60.2 %）削除している**。
つまり TBM の在 = 「2021年のササ全部」、TDM の在 = 「新規定着分のみ」。
Reviewer 2 の "the two models were fitted to different response domains" は**完全に正しく**、
TSS 0.5556 → 0.7010 の差は distance 変数に帰属できない。

---

## 1. 資産1 — 定着モデル `sdm_sasainc.R`

### 1.1 このスクリプトが何をモデル化しているか

`/Users/okamoto/NIES/SasaSDMPaper/data_from_server/ortho/sdm_sasainc.R`

応答変数の構成（原文ママ）:

```r
sasa_inc <- (sasa21_ras - sasa12_ras) %>%
  mutate(sasa_inc = ifelse(sasa == 1, 1, 0)) %>%   # 2012=非ササ かつ 2021=ササ
  select(sasa_inc) %>%
  terra::aggregate(fact = 10, fun = "mean") %>%     # 10 m ブロックの定着「割合」
  terra::resample(sasa21_ras, method = "near") %>%  # 1 m に戻す（＝100倍に複製）
  c(sasa_dist) %>%
  filter((dist != 0) & (dist <= 10)) %>%            # 既存パッチから 0<d<=10 m のみ
  select(sasa_inc)
```

- **応答は確かに定着（colonization）**: 2012年に不在 → 2021年に在。
- ただし**二値ではなく 10 m ブロックの定着割合（0–1 連続値）**で、`rand_forest(mode="regression")` を RMSE で回している。
- **サンプリング領域** = `selected_comms.tiff`（=2012年の大きなササパッチ、資産2）から **0 < 距離 ≤ 10 m のリング**。
  つまり「繁殖体供給がある場所に限定したうえで、環境が定着位置を説明するか」という設計。
  Reviewer 2 が求める colonization 側の枠組みとして、着想は正しい。
- **説明変数に distance は入っていない**（フィルタに使って `select()` で落とす）。terrain + snow_2021 のみ。

### 1.2 完成度・保存物

```
$ Rscript -e 'p <- parse(".../sdm_sasainc.R"); print(p[[length(p)]]); eval(p[[length(p)]])'
LAST EXPR: c(sasa_2012, )
eval of last expr: try-error
オブジェクト 'sasa_2012' がありません

$ grep -c "writeRaster\|saveRDS\|write_csv\|ggsave\|set.seed" .../sdm_sasainc.R
0
```

- **末尾 `c(sasa_2012, )` はエラーで落ちる**。作業途中の下書き。
- **`writeRaster` / `saveRDS` / `ggsave` / `set.seed` が 1 つも無い**。
  → **このスクリプトの成果物は 1 つも保存されていない**。再現性ゼロ（シードも無い）。
- 4 アルゴリズムのうち **RF 以外はコメントアウト**されている。
- バグ: `select(sasa_inc, filter_collinear(.))` は応答変数 `sasa_inc` 自身を共線性スクリーニングに含めている（実際に `filter_collinear` の返り値に `sasa_inc` が入る）。今回は無害だが、原理的には応答と相関した予測変数が落ちうる。

### 1.3 `sasa_increase/` に何が入っているか（＝このスクリプトの出力ではない）

.qgz は zip/XML なので展開した。**どちらも単なる QGIS 表示用ワークスペース**で、
レイヤのデータソースは `../../../ダウンロード/*.tiff`（ローカルのダウンロードフォルダ）。
provenance の情報は無い。

同定テスト（`/private/tmp/.../asset1_ident.R`）:

| ファイル | 正体 | 検証 |
|---|---|---|
| `sasa_inc.tiff` | `(vege21==1) & (vege12!=1)` そのもの | **一致率 100.000000 %**, n=4,097（＝公表 gross gain と一致） |
| `vege_2012_5x5.tiff` | `ortho/data/` のコピー | 値完全一致 |
| `risky_area.tiff`（band名 `risk`） | `pred30 - pred21` を vege21==2 かつ pred30>0.5 に限定 | max abs diff **5.96e-08**, n=10,148, **vege21 は 100 % クラス2** |
| `sasa_pred_sdm_dist_{21,30}.tiff` | **公表版とは別ヴィンテージの TDM** | 下記 |

```
pred_sdm_dist_21 vs pred_tdm_21 : n=397403  cor=0.8247  |d|>0.05: 63.7%
   cells >0.5 : archive=26343  published=12773
pred_sdm_dist_30 vs pred_tdm_30 : n=407658  cor=0.8170  |d|>0.05: 65.2%
   cells >0.5 : archive=31360  published=14482
```

→ NA マスクは同一だが**相関 0.82、適地面積は公表版の 2 倍以上**。
公表値 12,766 m² は `sasa_pred_tdm_21.tiff`（>0.5 で 12,773 セル）と整合するので、
**論文が使ったのは公表版で正しい**。ただし *TDM の適地面積は再フィットで倍以上動く* という
安定性の情報になる。Zenodo に置くなら、この Oct-2024 版は「別ヴィンテージ」と明記して除外すべき。

### 1.4 復活させた — 実行結果

`review/restore/assets/revive_sdm_sasainc_data.R` + `revive_sdm_sasainc_fit.R`
（パスを読み取り専用アーカイブに向け、`set.seed(1)` を追加した以外は原文どおり）

```
binary colonisation cells (1 m): 4097
DOMAIN: cells with 0 < dist(selected_comms) <= 10 m and defined response: 24912
response summary: Min 0  Median 0.02  Mean 0.04544  Max 0.28
after thin_by_cell(5 m) + extract + drop_na: n = 1411
response after thinning: mean = 0.0442  sd = 0.0548  zeros = 478 (33.9%)
filter_collinear kept: roughness, snow, twi, elevation, TPI, sasa_inc, aspect
```

`revive_sasainc_metrics.R`（空間ブロック 4-fold の out-of-fold 実測、null 比較つき）:

```
### Spatially-blocked out-of-fold performance, n = 1411
NULL (fold-train mean) : RMSE=0.05488  R2=-0.0030
RF, all 6 predictors   : RMSE=0.05376  R2=0.0374  cor=0.2374
   drop snow           : RMSE=0.05588 (delta +0.00211)  R2=-0.0398
   drop elevation      : RMSE=0.05526 (delta +0.00149)  R2=-0.0168
   drop aspect         : RMSE=0.05519 (delta +0.00143)  R2=-0.0144
   drop roughness      : RMSE=0.05456 (delta +0.00080)  R2= 0.0087
   drop twi            : RMSE=0.05355 (delta -0.00021)  R2= 0.0450   ← 落とした方が良い
   drop TPI            : RMSE=0.05351 (delta -0.00026)  R2= 0.0465   ← 落とした方が良い
   only <any single>   : RMSE 0.0633–0.0644  R2 -0.33 ～ -0.38
### Non-spatial (random 4-fold) for contrast
RF random CV           : RMSE=0.04563  R2=0.3067
```

**これは論文にとって不利な結果である。隠さずに書く。**

- 空間ブロック CV で **R² = 0.037**。intercept-only（null）に対する RMSE 改善は **2.0 %** しかない。
- ランダム CV だと R² = 0.307 に跳ね上がる。**見かけの説明力の約 88 % は空間自己相関**である
  （応答を 10 m → 1 m に `resample(method="near")` で 100 倍複製しているため、
  疑似反復も上乗せされている）。
- 単変数モデルはすべて null より悪い（R² 負）。
- 解釈: **繁殖体供給がある 10 m 圏内に限れば、地形・融雪は「どこが定着するか」をほとんど説明しない。**

この結果をそのまま出すと、論文の「環境適性が分布拡大を駆動する」という筋を弱める。
一方で「クローナル拡大が支配的」という主張は強くなる。
記憶にある通り、ユーザの方針は *scrupulous over strong-sounding* なので、**出すべき**だと考える。
ただしそのままの形（10 m 平均を 1 m に複製、n=1411、シード無し、RF 単独）では
査読に耐えないので、次節の設計に置き換えることを推奨する。

### 1.5 推奨する代替 — 共通評価領域 + colonization/persistence 分解（**実装・実行済み**）

`review/restore/assets/colonisation_persistence.R`

Reviewer 2 の要求文そのものに対応させた:
「同一の応答領域」「明示的な colonization–persistence の枠組み」。
予測変数は論文と同じ（terrain 7 + snow_2021）、distance は
`sdm_include_distance.R` と**完全に同じ定義**（2012年ササポリゴン >5 m² からの距離）。
非事象のみ 5 m サンプリングマスクで間引き（論文と同じ手順）、空間ブロック 4-fold の
out-of-fold で AUC と TSS_max を実測。

```
================= COLONISATION domain: cells NOT Sasa in 2012 =================
domain cells: 397964   events (s21==1): 4026
after thinning non-events on the 5 m sampling mask: 4026 events, 17795 non-events
modelling frame n = 20630  prevalence = 0.193
dist (m): Min 1.0  Median 25.0  Mean 36.4  Max 281.9

================= PERSISTENCE domain: cells Sasa in 2012 =================
domain cells: 8525   events (s21==1): 6079
after thinning: 6079 events, 1135 non-events
modelling frame n = 7098  prevalence = 0.845

### COMMON-DOMAIN nested comparison -- COLONISATION
environment only                   | spatial-block OOF AUC = 0.7733   TSS_max = 0.4311   (n=20630)
environment + distance             | spatial-block OOF AUC = 0.8713   TSS_max = 0.5881   (n=20630)
distance only                      | spatial-block OOF AUC = 0.8218   TSS_max = 0.5265   (n=20630)

### COMMON-DOMAIN nested comparison -- PERSISTENCE
environment only                   | spatial-block OOF AUC = 0.6725   TSS_max = 0.2599   (n=7098)
environment + distance             | spatial-block OOF AUC = 0.7725   TSS_max = 0.4345   (n=7098)
distance only                      | spatial-block OOF AUC = 0.7168   TSS_max = 0.4757   (n=7098)
```

**これで言えること（全部同一セル・同一 fold・同一予測変数なので帰属可能）:**

1. 定着において、**distance 単独 (AUC 0.822) は環境単独 (0.773) を上回る**。
2. 環境に distance を足すと **+0.098 AUC / +0.157 TSS**。
   distance に環境を足すと **+0.049 AUC / +0.062 TSS**。
   → **近接性（クローナル拡大）が主、環境適性が従**。これが本研究の結論として最も防御しやすい。
3. TBM→TDM の TSS 差は共通領域でも生き残る（0.431 → 0.588, +0.157）。
   公表値の差（0.5556 → 0.7010, +0.145）とほぼ同じ大きさ。
   → **「距離を入れると効く」という論文の主張自体は共通領域でも支持される。**
      壊れているのは §4 の「その差を distance に帰属する根拠」の方だけ。
4. persistence 側は全体に弱い（環境のみ AUC 0.673）。
   なお persistence の distance は「大パッチ内部かどうか」に近い指標になっており
   （中央値 0 m）、TSS_max が dist単独 > 環境+dist と逆転するのは閾値選択のノイズ。
   persistence を売りにするのは避けた方がよい。

**留保（必ず本文に書くこと）**: distance は 2012 年ササからの距離、定着は 2012 年ササの
隣接で起きるので、両者には設計上の循環がある。これは colonization–persistence 分解に
つきものの性質で、標準的な扱い（分解して両方報告する）で対応する。
また非事象のみ間引いているため prevalence が 0.193 に膨らんでおり、
**AUC/TSS の絶対値は公表値と直接比較してはいけない**。比較可能なのは同一フレーム内の
特徴量セット間の差だけ。

### 1.6 コスト評価

| 項目 | 見積り |
|---|---|
| §1.5 の解析（実装済み・実行済み） | **完了。再実行は約 4 分** |
| 4 アルゴリズム stack に拡張する場合 | +2〜4 時間（tune_grid × 2 領域） |
| 図（AUC/TSS 比較 + 変数重要度） | 半日 |
| 本文追記（Methods 1 段落 + Results 1 段落 + 表1つ） | 半日 |
| **不利に出るリスク** | **中〜高**。§1.4 のまま出せば「環境予測は空間 CV で効かない」となり、2030 予測図（Fig. 8/9）の説得力が落ちる。§1.5 の枠組みなら、環境も distance も有意に効くので不利にはならないが、**「distance が主」という結論の書き換えが必要**になる。 |

---

## 2. 資産2 — `selected_comms.tiff` は群落図ではない

### 2.1 中身

```
selected_comms.tiff : 1801 x 1753, res 1 m, EPSG:6690, 非NA = 5,159 セル
値: 51 個の離散値 = 5,6,8,9,11,14,...,341,445,674
```

値は**クラスIDではなくポリゴン面積 (m²)**。`select_sasa_communities.R` が
`mutate(area_2012 = st_area(.))` を持つ sf を `st_rasterize` しているため。

`review/restore/assets/../asset2.R` で下敷きの植生クラスを集計:

```
vege_2012 class composition UNDER selected_comms:
   1    2    3    4    5    6    7  NaN
4118  470    1   78   30    3  435   24     ← 79.8 % がクラス1（ササ）
```

→ **2012年のササパッチのマスク**であり、高山植生の群落分類ではない。

### 2.2 `select_sasa_communities.R` の再現

同スクリプトの手順（2012年ササを ≥5 m² のポリゴン化 → 10 m グリッドで被覆 >25 m² の
セルに交差するポリゴンのみ残す）を再実行:

```
polygons >= 5 m2: 229  total area: 7239
polygons retained (AOI): 71  distinct areas: 51  total area: 5211
archived selected_comms: cells 5159  distinct values 51
archived distinct values: 5,6,8,9,11,...,216,341,445,674
repro polygon areas    : 5,6,8,9,11,...,216,341,445,674   ← 完全一致
```

**51 個の面積値の集合が完全一致**。provenance は確定。
セル数の差（5,211 → 5,159, 1.0 %）は `st_rasterize`/`st_warp` の丸め。

なお、アーカイブ版スクリプトは 50 行目で未定義の `vege2021` を
`st_warp(vege2021)` に渡しており、**そのままでは実行できない**（要修正・Zenodo 前）。

### 2.3 Reviewer 1 の問いに答えられるか → **答えられない**

```
 risky_area_wo_dist.tiff  risky cells: 36797   vege21 composition:  2 -> 36797   (100 %)
 risky_area_tdm.tiff      risky cells:  3149   vege21 composition:  2 ->  3149   (100 %)
 risky_area.tiff (server) risky cells: 10148   vege21 composition:  2 -> 10148   (100 %)
```

`matmet.qmd:137` / `results.qmd:116` の定義（「2021年にOther Vegetationかつ TBM の
2030年 HS > 0.5 のセル」）どおり、**3 種類すべてで 100 % がクラス2**。
先行監査の「トートロジー」結論は実測で確認された。

`selected_comms` と risky area の重なりも 307〜360 セル（risky area の 1 % 未満）しかなく、
そもそも risky area の組成を語れる情報を持っていない。

→ **環境省・富山県植生図へのフォールバックが正しい判断。**
`selected_comms.tiff` を Reviewer 1 への回答に使うことはできない。

---

## 3. 資産3 — `results/use_this/` は公開分類図そのもの（再現性ギャップ解消）

### 3.1 配列の素性

```
results/use_this/2012_5x5.npy  (3744, 5616) int64
  0:9246554  1:334096  2:6901621  3:1394310  4:446748  5:181786  6:473934  7:2047255
results/use_this/2021_5x5.npy  (3744, 5616) int64
  0:9246554  1:361588  2:6707317  3:1512560  4:475030  5:133359  6:510775  7:2079121
```

- shape = 画像空間 (3744 × 5616)、**int64**、値 **0=マスク, 1–7=植生クラス**（確定済みのコード体系と同一）。
- `color2cat.py` がその生成器: **PNG を cmap 最近傍でカテゴリに逆変換**し、
  `mask.npy` で 0 を焼き込んで `np.save` している。argmin+1 なので 1-origin。
- 対照的に `results/*_masked.npy` は **float32 で値 0–6（0-origin）**。
  0 がマスクとクラス1（ササ）の両方に潰れている。
  先行監査が「masked.npy は公開図ではない・net +1.0 %」と結論したのは、この
  **1 ずれ + マスク衝突**が原因と考えられる。

### 3.2 provenance — コードに明記されていた

`data_from_server/ortho/image_to_csv.py`（コメントアウト済みの旧ブロック）:

```python
#in_paths = ["results/use_this/2012_5x5.npy", "results/use_this/2021_5x5.npy"]
#out_paths = ["ortho/data/2012_5x5.csv", "ortho/data/2021_5x5.csv"]
```

実測で裏づけた:

```
csv2012 == use_this/2012_5x5.npy indexed [v,u]: True
csv2021 == use_this/2021_5x5.npy indexed [v,u]: True
```

**`ortho/data/{2012,2021}_5x5.csv`（各 21,026,304 行）は use_this の .npy を
そのまま flatten したもので、値は 1 個も違わない。**

### 3.3 画像空間 → 地図空間の完全再現

`georectify.R` の `interpolate()` を原文どおり実行
（`georectified.csv` に left_join → `st_rasterize(dx=1, dy=1)` → `focal(3, modal, na.policy="only")` 1 回）。
スクリプト: `/private/tmp/.../asset3_repro.R`、出力: `review/restore/assets/out/repro_vege_{2012,2021}_5x5.tiff`

```
georectified.csv rows: 11823852

##### 2012  joined rows: 11823852  with non-NA class: 11823852  class==0 (mask): 55035
raw st_rasterize grid: dim 1753x1801 ext 732744,734545,4050316.25,4052069.25
published grid       : dim 1753x1801 ext 732744,734545,4050316.25,4052069.25
ext identical: TRUE  dim identical: TRUE
repro non-NA: 1206233  published non-NA: 1206233
EXACT-CODE agreement: overlap=1206233  pixel agreement=99.8332%
Sasa(1): repro 8551  published 8547

##### 2021
EXACT-CODE agreement: overlap=1206233  pixel agreement=99.8400%
Sasa(1): repro 10172  published 10176
```

- **グリッド（範囲・次元）が完全一致**、**非NAセル数が 1,206,233 で完全一致**。
- 画素一致率 **99.83 % / 99.84 %**。
- 残差の原因: `st_rasterize` は 1 m セル内に複数の画像点が落ちたとき
  **最後に焼いた点が勝つ（last-wins）**仕様で、点の順序に依存する。
  多数決ではない。これが唯一の非決定性で、規模も一致（クラス3↔2, 7↔3 の境界画素）。
  → 論文の Methods が「セル内多数決」と書いているなら、**last-wins に訂正が必要**
  （現行 matmet.qmd は集約規則に言及していないので、追記が望ましい）。

### 3.4 遷移行列 — 公表値と再現値

```
##### PUBLISHED vege_*_5x5.tiff  cells: 1206233
Sasa 2012=8547 2021=10176  gross gain=4097  gross loss=2468  net=+1629
LOSS destination:  OtherVeg 932 (37.8%)  DwarfPine 1156 (46.8%)  Sorbus 159 (6.4%)  Alnus 133 (5.4%)  Maple 85 (3.4%)  NonVeg 3 (0.1%)
GAIN origin:       OtherVeg 2171 (53.0%)  DwarfPine 1425 (34.8%)  Sorbus 224 (5.5%)  Maple 155 (3.8%)  Alnus 109 (2.7%)  NonVeg 13 (0.3%)

##### REPRODUCED from use_this/*.npy  cells: 1201027
Sasa 2012=8551 2021=10172  gross gain=4089  gross loss=2468  net=+1621
LOSS destination:  OtherVeg 929 (37.6%)  DwarfPine 1157 (46.9%)  Sorbus 159 (6.4%)  Alnus 133 (5.4%)  Maple 87 (3.5%)  NonVeg 3 (0.1%)
GAIN origin:       OtherVeg 2170 (53.1%)  DwarfPine 1418 (34.7%)  Sorbus 226 (5.5%)  Maple 153 (3.7%)  Alnus 109 (2.7%)  NonVeg 13 (0.3%)
```

| 量 | 公表 | 再現 | 差 |
|---|---|---|---|
| Sasa 2012 | 8,547 | 8,551 | +0.05 % |
| Sasa 2021 | 10,176 | 10,172 | −0.04 % |
| gross gain | 4,097 | 4,089 | −0.20 % |
| gross loss | 2,468 | 2,468 | **0** |
| net | +1,629 | +1,621 | −0.5 % |
| **ハイマツへの損失率** | **46.8 %** | **46.9 %** | **+0.1 pt** |

→ **ハイマツ優占の損失シグネチャは、実際の公開プロダクトでも完全に安定**。
先行監査が挙げた「net +1.0 % vs +19.1 %」の食い違いは `*_masked.npy` の
クラス番号ずれに起因する見かけの問題であり、**公開図には存在しない**。

### 3.5 Reviewer 3 への含意

これで**画像分類 → CSV → ジオレクティファイ → 公開ラスタ**の鎖が、
中間成果物つきで端から端まで通った。Reviewer 3 の再現性指摘に対して
「数値は再現するがパイプラインは再現しない」という以前の回答は、
**「分類器の重み以外は完全に再現する（画素一致率 99.83 %）」に書き換えられる**。
Zenodo には `results/use_this/{2012,2021}_5x5.npy` と `color2cat.py` を必ず含めること。

---

## 4. 派生した最重要の発見 — TBM と TDM の応答領域は同一でない（manuscript-affecting）

Reviewer 2:
> because the two models were fitted to different response domains, their performance
> cannot presently be attributed solely to the distance variable.

コードで確認した。`sdm.R`（TBM）は `filter(elevation < 2560)` のみ。
`sdm_include_distance.R` は**それに加えて `filter(dist > 0)`**（同ファイル 59 行相当）。
`dist` は `sasa12_ras %>% filter(sasa==1) %>% distance()` なので、
**`dist == 0` は「2012年にササだったセル」と厳密に同値**。

ラスタから直接数えた（`/private/tmp/.../domain.R`）:

```
cells with ALL covariates + response (after drop_na): 1159498
after elevation < 2560: 390065   presences (2021 Sasa): 9974
  of those presences, dist==0 (i.e. ALSO Sasa in 2012): 6001  (60.2%)
TDM domain after filter(dist>0): 381704   presences: 3973   absences: 377731
TBM domain (no dist filter)   : 390065   presences: 9974   absences: 380091
=> filter(dist>0) removes 6001 presences and 2360 absences
prevalence TBM: 0.0256   TDM: 0.0104
```

先行復旧の学習データでも同じ:

```
trainingdata_models_wo_dist.rds (TBM) n=21389  presence 7820  absence 13569
trainingdata_models.rds         (TDM) n=16596  presence 3123  absence 13473   dist==0 rows: 0
```

**結論:**
- TBM の在 = 2021年のササ**全部**（残存 + 新規定着）
- TDM の在 = **新規定着分のみ**（2012年ササを 60.2 % 削除済み）
- prevalence も 2.56 % vs 1.04 % と 2.5 倍違う。

→ TDM は事実上すでに定着モデルであり、TBM は分布モデル。
**公表の TSS 0.5556 vs 0.7010 の比較は、モデル間で応答が違うので distance の効果に帰属できない。**
Reviewer 2 の指摘は正しく、原因は 1 行のフィルタである。

対応案（推奨）: §1.5 の共通領域ネスト比較（実行済み）を Results に入れ、
Methods に `filter(dist > 0)` の存在と帰結を明記する。
幸い、共通領域でも distance の寄与は残る（TSS +0.157）ので、
**結論の方向は変わらず、帰属の根拠だけが正しくなる**。

---

## 5. 環境 — xgboost のバージョンピン（Zenodo 用）

```
$ Rscript -e '... predict(model_stack, nd, type="prob") ...'
xgboost 3.2.1.1
model_stack.rds         : FAIL: In index: 4.
model_stack_wo_dist.rds : FAIL: In index: 3.

$ R_LIBS=.../scratchpad/Rlib177 Rscript -e '...'
xgboost in use: 1.7.7.1
  model_stack.rds         : OK, first pred = 0.093859
  model_stack_wo_dist.rds : OK, first pred = 0.27806
```

- **ピン: `xgboost` 1.7.7.1**。両方の archived stack で警告なしに `predict()` が通る。
- 1.7.6.1 でも通るが serialization 警告が出る。
- 現行 3.2.1.1 は**両方とも失敗**。
- 他は R 4.5.2 / tidysdm 1.0.4 / tidymodels 1.5.0 / stacks 1.1.1 / ranger 0.18.0 /
  spatialsample 0.6.1 / maxnet 0.1.4 / mgcv 1.9.3 / DALEX 2.5.4 でそのまま動く。

---

## 6. 書き出したファイル

すべて `review/restore/assets/` 配下:

- `revive_sdm_sasainc_data.R` — sdm_sasainc.R のデータ工程（忠実復元 + set.seed）
- `revive_sdm_sasainc_fit.R` — 同モデル工程（RF 回帰, spatial block CV, grid=20）
- `revive_sasainc_metrics.R` — 空間ブロック OOF の RMSE/R²、null 比較、変数落とし
- `colonisation_persistence.R` — **共通領域 colonization/persistence 分解（Reviewer 2 回答本体）**
- `out/df_inc.rds` — 復元した定着モデルの学習フレーム (n=1411)
- `out/sasa_inc_response_domain.tiff` — 応答ラスタ（0<dist≤10 m 領域, 24,912 セル）
- `out/sasainc_models.rds`, `out/sasainc_ensemble.rds`, `out/sasainc_varimp.rds`
- `out/frame_colonisation.rds` (n=20,630), `out/frame_persistence.rds` (n=7,098)
- `out/repro_vege_{2012,2021}_5x5.tiff` — **use_this/*.npy から再現した公開植生図**

スクラッチ（検証スクリプト、恒久保存不要）:
`/private/tmp/claude-501/.../scratchpad/` の
`inspect1.R inspect2.R asset1_ident.R asset2.R asset3_repro.R asset3_trans.R domain.R`
