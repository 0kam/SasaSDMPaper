# 解析コード監査報告

対象: `/Users/okamoto/NIES/SasaSDMPaper` の全解析コードと保存済み成果物
実施: 5つのサブシステム監査（分類 / SDM / 融雪・幾何補正 / 下流・作図 / 成果物来歴）
＋ それぞれに対する敵対的検証パス ＋ 原稿クロスチェック ＋ 画像判読の実行可能性調査

本報告では一貫して次の3者を区別する。

- **(a)** コードをそのまま素の環境で走らせたときに起きること
- **(b)** 保存されている成果物が示す、実際に起きたこと
- **(c)** 原稿が「こうした」と書いていること

**(a) ≠ (b) ≠ (c) の乖離こそが本監査の発見物**である。

---

## 総括

### 数値は再現する。パイプラインは再現しない。

まず良い知らせから。**原稿の面積値はすべて、保存済みラスタから正確に再現する。**
8,542 / 10,170 / 1,628 / 4,095 / 2,467 m²、47,253 / 12,766 / 27,049 / 4,387 / 2,257 / 717 m²、
57% / 34% / +19% / +48% — これらは `terra::expanse()` の測地面積（UTM面積スケール
係数 0.999455）であることを理解すればすべて一致する。テスト TSS 0.55（0.5555946）と
0.70（0.7009731）も CSV と一致し、擬似欠測の再抽選を5回独立に行っても
TDM 0.6984–0.7074 / TBM 0.5477–0.5551 の範囲に収まる。80/20分割、4分割空間ブロック CV、
grid = 18、5 m 擬似欠測間引き、risky域の定義、モデル順位の記述 — いずれも正しい。
2012年と2021年の写真の位置合わせも実測で中央値 1.0–1.9 px、系統的なずれはなく、
これは査読者に対して堂々と答えられる。

**問題は、それらの数値がどこから来たかを第三者が辿れないことと、いくつかの
中心的主張が数値の再現性とは別の理由で成り立たないことである。**

### 公開リポジトリからは何一つ再現できない

`.gitignore` が `*.tiff` `*.rds` `*.npy` を除外しているため、GitHub には
**応答変数（`vege_*_5x5.tiff`）も、融雪予測子（`fitted_*.tiff`）も、学習済みモデルも
一切ない**。一方 `*.tif` は除外されていないため、地形予測子7ファイルと
`snow_{mean,sd,reg}.tif` の計10ファイルは公開されている。この非対称が最悪の形で効いている:
**公開されている `snow_reg.tif` の平均値がまさに原稿の「−0.86」であり、その値は
モデルが実際に使った予測子とは別物**である（後述 F-1）。

さらに、画像空間→地図空間の対応表 `ortho/data/georectified.csv` が
リポジトリのどこにも存在せず、それを作るコードも存在しない。この1ファイルの欠落だけで、
分類結果も融雪 DOY も地図化できない。クローンした第三者は**どの段階も完走できない**。
これが Reviewer 3 の "not ensuring reproducibility of the study" と AE の
永続リポジトリ要求が指しているものの実体である。

### 対話実行の痕跡と、コードのドリフト

スクリプトはブロック単位で対話的に実行され、長期間にわたって編集された。その結果:

- `sdm_tbm.R` は現在の版では **RF しか学習しない**（GAM/MaxEnt/XGB がコメントアウト）。
  しかし保存済み `models_wo_dist.rds` には4アルゴリズムすべてが入っている。
  公開されている TBM スクリプトは、公開された TBM を作った版ではない。
- 論文の変数重要度図（Fig 6）は、リポジトリ内のどの版のスクリプトでも再現しない
  （`ylim(c(0,0.6))` のない別の版で描かれ、しかも乱数種がないため順位が走るたび入れ替わる）。
- 地図図版7点は、リポジトリのコードには存在しない `ggspatial` の方位記号・スケールバーが
  後から加えられている（ただしこれは注記のみで、地図本体は同一 — 検証で確認済み）。
- 図1・図2・図4には生成コードが**一切ない**（図2の英語版スクリプトは削除済み、
  図4は手作業合成）。
- 保存済み `.rds` アンサンブルは、現行 xgboost（3.2.1.1）では**読めない**。
  `predict()` が "'xgb.Booster' object is corrupted or is from an incompatible
  XGBoost version" で落ちる。本監査は xgboost 1.7.11.1 を別ライブラリに入れて回避した。

これらは道徳的な失敗ではなく、**再現可能なパイプラインに整理し直すために正確に
地図化すべき対象**である。

### 改訂への含意

査読対応で最も重い作業は、査読者が指摘した項目そのものではなく、**指摘に答えようとすると
副次的に露見する項目**である。具体的には:

1. Reviewer 2 の「融雪トレンドに不確実性推定を」に正直に答えると、**トレンドは有意でない**
   （p = 0.20）と書かざるを得ず、同時に **8.6 日/10年という要旨の数値が撤回対象**になる。
2. Reviewer 1 #5 / Reviewer 2 の「2030年の距離変数を説明せよ」に正直に答えると、
   **2030年の TDM 拡大予測の約90%は融雪ではなく距離レイヤの更新に由来する**ことを
   書かざるを得ない。
3. Reviewer 1 #3 の「遷移行列を出せ」に答えると、**減少の最大転換先はハイマツ（46.8%）**
   であり、原稿が名指しした低木3クラス（ナナカマド・カエデ・ミヤマハンノキ）の合計は
   15.3% にすぎないことが出る。被覆説の主要な支柱が弱くなる。
4. Reviewer 3 の「L164-170 はブラックボックス」に答えようとすると、**公開された地図を
   作った分類器のソースがリポジトリに存在せず**（`__pycache__` からのみ復元可能）、
   **精度評価が一切存在しない**ことが露見する。

いずれも、決定済み方針（`review/decisions.md` ④「トーンダウンを面倒がらずに行い、
真摯な論文に仕上げることを最優先」）と整合する方向であり、隠すべきものではない。
ただし**要旨・結果・考察の複数の中心文がそのまま書き換えの対象になる**ことは
最初に覚悟しておく必要がある。

### どこに労力を割かなくてよいか（壊れていない部分）

以下は監査で確認した結果、**健全**である。ここに時間を使う必要はない。

| 領域 | 状態 |
|---|---|
| 原稿の面積値10種すべて | 保存済みラスタから正確に再現（`expanse()` 測地面積） |
| テスト TSS 0.55 / 0.70 | CSV と一致、擬似欠測再抽選にも頑健 |
| 予測ラスタ4枚・risky ラスタ2枚 | 保存済みアンサンブルから**ビット単位で再生成**（最大差 5.96e-08） |
| `fitted_*.tiff`（融雪予測子） | 現行 `raw/` から**厳密に再現**（半ピクセルずれを補正すれば max diff = 0） |
| 2012 vs 2021 写真の位置合わせ | 中央値 1.0–1.9 px、系統的年間差なし |
| 空間ブロック CV | fold サイズが `models.rds` と完全一致、`prop=0.2` は正しく約79%学習 |
| `expanse()` 由来の「8,542 vs 8,547」 | 定義の不一致ではない。1文の注記で足りる |
| Fig 5 の凡例ラベル（GBT/GAM/MaxEnt/RF） | 正しい |
| risky ラスタの categorical resample | 実害なし（ビット一致で再生成） |
| 2030年融雪レイヤの 0–255 範囲 | モデル領域内で3画素のみ。実害なし |
| 2021/2030 ドメイン差の面積への影響 | NA 伝播により交差集合で計算済み。TBM 最大 +4.3%、TDM ちょうど 0 |

---

## 致命的な問題

### F-1. 要旨の「8.6日/10年」は、モデルが使っていない孤児ラスタの平均値である

**何が問題か。**
原稿の `matmet.qmd:77`「The mean regression coefficient was −0.86, corresponding to an
advance of 8.6 days per decade」および `index.qmd:53`（要旨）の 8.6 日/10年は、
`ortho/data/snow/snow_reg.tif` の平均値である。このファイルは:

- **どのスクリプトからも読まれていない**（孤児）
- **現行コードでは生成できない**（生成ブロックが実行時エラー）
- **モデルが実際に使った予測子とは別のデータ製品**である
- **`.gitignore` の穴から GitHub に公開されている**（査読者が平均を計算できる）

**証拠。**

```
ortho/data/snow/snow_reg.tif   n=1,133,175  mean=-0.8635999  median=-0.8064516
                               sd=0.8519743  frac_neg=0.897488   mean×10 = -8.635999
```

生成ブロックは実行できない（`scripts/sdm/preprocess_snow_data.R:118-126`）:

```
$ Rscript -e 'p <- parse("scripts/sdm/preprocess_snow_data.R"); length(p)'
34                                    # パースは通る（構文エラーではない）
expr 20 lines 119-121 : s <- snow_reg %>% group_by(x, y) %>% partit
expr 21 lines 122-126 : summarise(lm.coef = ..., lm.r2 = ..., lm.pval = ...)

multidplyr exports 'partit'?  FALSE
eval 119-121 -> "could not find function \"partit\""
eval 122-126 -> "object '.' not found"
exists('s') after? FALSE
m$coeffisients['year'] -> NULL        # line 123 のスペルミス
m$adj.r.squared        -> NULL        # line 124: lm オブジェクトには存在しない
```

さらに、このファイルは**現行の `raw/` からは作れない**ことが3通りに示される:

```
# (1) 9層の格子上にある（N層平均は 1/N の格子になる）
N-layer lattice test on snow_mean.tif:
  N=6 0.3340  N=7 0.1102  N=8 0.1102  N=9 1.0000  N=10 0.1102  N=12 0.3340
# (2) OLS 傾きの最小分母が D=620 → 2011-2018 + 2020 の9年（現行10年なら D=1001）
minimal denominator: 620 (116)  155 (73)  310 (71)  124 (34)  62 (20)  31 (16)
# (3) 27,275 セルが、現行 raw ラスタが全年 NA の位置にある
archived non-NA: 1,133,175 ; of these raw2011 is NA at: 27,275
```

一方、**モデルが実際に使った `fitted_*.tiff` は厳密に再現する**:

```
per-pixel OLS (snowmelt>0 rule), half-pixel shift 補正後:
  fitted_2011 max|d|=0  fitted_2012 max|d|=0  ...  fitted_2030 (clamped) max|d|=0
mean slope = -0.7147 d/yr   median -0.6264   sd 0.6348   91.65% negative
mean(fitted_2021 - fitted_2012) = -6.390 d over 9 yr  ->  -7.10 d/decade
cor(snow_reg.tif, 再計算スロープ) = 0.5106
```

**論文への影響。**
要旨と Methods の中心数値が変わる。**−0.86 → −0.715 d/yr（7.1日/10年）**、
または実際にモデルに入った層の差分から **−7.10日/10年**。
現在互いに矛盾する3つの数値が流通しており、再現可能なのは後2者のみ。

**査読者との関係。**
Reviewer 2「Snowmelt trends require uncertainty estimates ... The reported mean advance
of 8.6 days per decade is central to the manuscript but insufficiently supported
statistically」。まさにこの数値。

**修正方針。**
`snow_reg.tif`（および同様に孤児の `snow_mean.tif` / `snow_sd.tif`）を公開リポジトリから
撤去し、モデルが使った層から係数を再報告する。9年版の出所を探すことは推奨しない
（後述「実施しないことを推奨する解析」）。

---

### F-2. 融雪トレンドは景観スケールで統計的に有意でない

**何が問題か。**
Reviewer 2 が明示的に要求した不確実性推定を実施したところ、**2011–2021年の景観平均
融雪 DOY にトレンドは検出されない**。現在の文言（「advanced by an average of
8.6 days per decade」）は、記述統計であることを明示しない限り維持できない。

**証拠。** `ortho/data/snow/raw/*.tiff`（10年、1 m画素）から再計算:

```
== C. 年別空間平均10点に対する回帰（正直な検定） ==
annual means: 171.46 179.54 172.91 179.06 165.88 157.92 176.83 164.97 170.11 165.56
 zeros retained: slope=-0.9184  SE=0.6633  t=-1.385  p=0.2035  CI=[-2.4479,+0.6110]  R2=0.193
 zeros excluded: slope=-0.6885  SE=0.6748  t=-1.020  p=0.3374  CI=[-2.2445,+0.8675]  R2=0.115

== 画素ごとの有意性 ==
 frac p<0.05 = 0.0196        # 5% の帰無率より低い
 per-pixel r2 mean 0.0985 median 0.0640 ; frac r2>0.2 = 0.1542
 per-pixel 調整済み R2 の平均 = -0.0146      # 平均するとマイナス
```

**論文への影響。**
95% CI は 10年で 24日の前進から 6–9日の後退までを含む。要旨・Methods・考察の
「融雪が早まった」という前提そのものの書き方が変わる。画素の 91.7% が負の傾きを
示すという事実（空間的に相関した事実であり、独立な証拠ではない）は残せる。

**査読者との関係。** Reviewer 2 主要コメント3、そのもの。

**修正方針。**
(i) 画素レベル傾きの平均・中央値・SD・符号比を記述統計として報告、
(ii) 景観レベル検定を報告し「有意でない」と明記、
(iii) 画素レベルの素朴な CI は「1シーンの120万画素は120万の独立観測ではない」と
明示したうえで参考値として付す。

---

### F-3. TDM の2030年拡大予測は、融雪ではなく距離レイヤの差し替えで生じている

**何が問題か。**
`sdm_tdm.R` は2030年予測時に、融雪層を 2021→2030 に更新すると同時に、
距離層を「2012年ササからの距離」→「2021年ササからの距離」に差し替えている。
公表された「新たに適地となる 4,387 m²」を要因分解すると、**約90%が距離層の更新**
すなわち観測された2012→2021の拡大を予測子として食わせ直した結果であり、
融雪の寄与は 12% にすぎない。

**証拠。** 保存済み TDM アンサンブルを5通りの環境スタックに適用:

```
tag       suitable(m2)  validpx   内容
s21_V2      12766.04     397403   公表 2021（snow21 + 2012ポリゴン>5m2 からの距離）
s30_V3      14474.11     407658   公表 2030（snow30 + 2021ポリゴン>5m2 からの距離）
s30_V2      12190.35     407658   融雪のみ前進、距離は2012に固定
s21_V3      14888.88     397403   距離のみ更新、融雪は2021に固定

newly suitable, 公表 (s30_V3 vs s21_V2):  4386.61 m2
newly suitable, 融雪のみ (s30_V2 vs s21_V2): 533.71 m2   (12.2%)
newly suitable, 距離のみ (s21_V3 vs s21_V2): 3937.85 m2  (89.8%)
```

距離を固定すると、**総適地面積はむしろ縮小する**（12,766 → 12,190 m²）。

原稿側（読み取りで確認）:
- `matmet.qmd:137`「we predicted *Sasa* distribution for 2030 **based on the estimated
  snowmelt DOYs**」— 距離層の再構築には一切触れていない
- `discussion.qmd:7`、要旨「Scenario-based projections under continued snowmelt advance」
- `paper/files/overview.jpg`（Fig 1）: 予測ボックスへ入る追加の矢印は
  "Future prediction of snowmelt DOY" のみ

**論文への影響。**
TDM 側の 2030 年予測に関する因果的な言い回しが全面的に成立しない。
要旨・結果・考察・図1のワークフロー図を修正。TBM は距離予測子を持たないので、
TBM 側の 27,049 m² は純粋に融雪駆動であり、この点は維持できる。

**査読者との関係。**
Reviewer 1 主要コメント5「Explain how the distance variable was constructed for the
2030 TDM projection」、Reviewer 2「The public code appears to calculate the
future-distance layer from the 2021 distribution ... it must be stated explicitly in
the Methods and represented in the workflow diagram」。**両者が既に半分気づいている。**

**修正方針。**
差し替えを明記したうえで、上記の分解表を結果または補遺に載せる。
Reviewer 2 の問い「Continued expansion from the observed 2021 edge か、
Unlimited colonization of all predicted 2021 cells か」への回答は前者。

---

### F-4. TDM の距離予測子が、学習時と予測時で別物である

**何が問題か。**
距離レイヤが3通りに作られており、**学習に使った定義が予測に使われていない**。
これはモデリングの選択ではなく、バグである。

| 変種 | 場所 | 定義 |
|---|---|---|
| **V1** | `sdm_tdm.R:44-47`（**学習**） | 2012年ササ**画素**すべてからの距離（面積フィルタなし） |
| **V2** | `sdm_tdm.R:238-250`（**2021年予測**） | 2012年ササ**ポリゴン（面積 > 5 m²）**からの距離 |
| **V3** | `sdm_tdm.R:229-236, 252-255`（**2030年予測**） | **2021年**ポリゴン（> 5 m²）からの距離 |

**証拠。** モデル領域（`elevation < 2560`、n = 1,296,195 画素）で:

```
frac V1 != V2 : 0.8525   mean|d| 18.58 m   max|d| 342.8 m
(V2-V1) quantiles  25% 4.45 | 50% 13.29 | 75% 27.80 | 95% 55.22 | 99% 96.41
2012 polygons: 1171 (8547 m2) -> >5 m2: 196 (7074 m2 planar; 975 dropped)

保存済み model_stack.rds$train$dist との照合:
  cor(train$dist, distance(2012 全ササ画素)) = 1        max|diff| = 0     <- V1
  cor(train$dist, distance(2012 ポリゴン>5m2)) = 0.8179  max|diff| = 234.98

学習整合な V1 を予測に使い直すと:
  TDM 2021, V2 (公表)      : 12,766.0 m2
  TDM 2021, V1 (学習整合)  : 18,103.1 m2   (+41.8%)
  閾値をまたぐ画素: 5,340 / 397,403 (1.34%)  mean|dHS| 0.017  max 0.527
```

**論文への影響。**
結果の主要比較文「the TDM predicted 12,766 m², much closer to observations
(10,170 m²)」が **18,103 m²（観測の78%過剰）**になる。図7のキャプション
「closely matching the observed distribution」も同様。

**査読者との関係。**
Reviewer 2 が2030年側の半分だけ気づいている。5 m² フィルタと 2021年予測側の不整合は
未指摘。

**修正方針。**
予測を学習に合わせる（V1 で再予測、再学習不要）か、学習を予測に合わせる（V2 で再学習）か。
いずれにせよ12,766 という数値と「観測とよく一致」という主張は書き換え。

---

### F-5. TDM の 12,766 m² の半分以上は、TDM が学習していない2012年ササ域である

**何が問題か。**
`sdm_tdm.R:69` は学習データから `dist == 0`（＝2012年時点でササだった画素）を
すべて除外している（学習時の `dist` 範囲は 1–142.088 m）。しかし予測時にはこの
フィルタが適用されず、**公表された2021年地図は `dist == 0` の6,954画素を含む
397,403画素すべてに対して予測されている**。

**証拠。**

```
TDM 2021 map, hs > 0.5 : 12,773 cells  (= 12,766.04 m2)
   of which dist == 0  :  6,675 cells  (52.3%)
   of which dist > 142 :      0 cells
mean HS at dist == 0        : 0.5975
mean HS at 0 < dist <= 142  : 0.1132
-> 2012年ササ域の外では、TDM の適地はわずか ~6,098 m2
```

**論文への影響。**
「TDM は観測に近い」という主張の実体は、**モデルが学習支持域の外に外挿して、
学習から意図的に除外した「自明に占有されている画素」を再現している**ことである。
これは循環に近い。

**査読者との関係。**
Reviewer 2「because the two models were fitted to different response domains, their
performance cannot presently be attributed solely to the distance variable」に対する
最も明確な定量的回答。

**なお、結論の向きは生き残る。** 両モデルが未見の共通評価域（`dist > 0`, n = 4,304,
839 presence）で採点すると:

```
TDM: boyce 0.9477  roc_auc 0.9139  tss_max 0.6981
TBM: boyce 0.9865  roc_auc 0.7816  tss_max 0.4535
```

共通域では差はむしろ**拡大**する（0.70 vs 0.45）。公表 TBM の 0.556 は、
自明に正例である 2012年ササ核が押し上げている。

**修正方針。**
共通評価域での比較を結果に加え、TDM の 12,766 m² の内訳（52.3% が 2012年ササ域）を
明記する。

---

### F-6. 図8は2030年の地図を示していない

**何が問題か。**
図8の上段は、キャプションが「2030年の HS 地図」と述べているにもかかわらず、
**図7とビット単位で同一の2021年地図**である。

**証拠。**

```
paper/results.qmd:98
  hsmap <- c(image_read("files/hsmap_tbm_2021.jpg"), image_read("files/hsmap_tdm_2021.jpg"))

$ magick paper/files/future.jpg -crop 2400x960+0+0 +repage top.png
$ magick compare -metric RMSE top.png paper/files/hsmap_2021.jpg null:
0 (0)                                          <- 図7と完全一致
$ magick compare -metric RMSE top.png <正しい2030合成> null:
5464.01 (0.0833755)

パネル内タイトル実測: "Habitat Suitability map of Sasa (TBM, 2021)" / "(TDM, 2021)"
grep -n "hsmap_tbm_2030|hsmap_tdm_2030" paper/*.qmd paper/index.tex  -> ヒットなし
```

`hsmap_tbm_2030.jpg` / `hsmap_tdm_2030.jpg` は `paper/files/` に存在するが未使用。
**投稿版 PDF（`paper/submit_files/future-eps-converted-to.pdf`）でも同じ**であり、
査読者は2030年の地図を一度も見ていない。

**論文への影響。** 図8を差し替え。数値（キャプション内）は正しいので計算の誤りではない。

**査読者との関係。**
Reviewer 3「Figure 8: I suggest one panel for each model ... unsuitable 2021 + suitable
2030 ...」— このコメントは2030層を含まない図に対して書かれている。応答時にこの事実を
述べたうえで図を作り直す必要がある。

**修正方針。** 図8を再構成。Reviewer 3 の提案（モデルごとに1パネル、3区分表示）を採用すれば
同時に解決する。

---

### F-7. 公開された植生地図を作った分類器がリポジトリに存在せず、精度評価も存在しない

**何が問題か。**
`ortho/data/vege_{2012,2021}_5x5.tiff` を作ったのは **CRNN（Conv2d + LSTM）、5×5画素
パッチ、5分割 CV、200エポック**である。そのソース `models/crnn.py` は削除されており、
`__pycache__/crnn.cpython-310.pyc`（**これも gitignore 対象で GitHub には無い**）から
逆アセンブルして初めて復元できた。リポジトリにある `run_rnn.py` / `models/rnn.py` は
**1×1画素の別モデル**である。

**証拠。**

```
utils/interpolate.R:40-43 -> results/cnn_lstm5x5_cv_5_ep_200/*.csv     [不在]
image_to_csv.py:16-17 (コメントアウト) -> results/use_this/{2012,2021}_5x5.npy [不在]
models/__pycache__/crnn.cpython-310.pyc  src_mtime=2023-11-11  src_size=2392
  埋め込みパス: /home/okamoto/Projects/jasms2023f/scripts/models/crnn.py
$ git check-ignore -v .../crnn.cpython-310.pyc  ->  .gitignore:10:__pycache__/

精度証拠の探索:
$ find . -iname "*stratified*" -o -iname "*confusion*" -o -iname "runs" -o -iname "*.pth"
(何も返らない)
$ grep -i "accuracy|F1|confusion|kappa" paper/matmet.qmd paper/results.qmd paper/supplement.qmd
(SDM/TSS のみ)
```

`results/cv.png` は**前研究のもの**である（2015年画像、1×1モデル、
"Multidays RNN 1x1" の比較、クラス名は "Golden Birch"）。

**重要な訂正。** 第一次監査が提示した再代入精度（OA 0.928 / 0.932）は
`results/*_masked.npy` から計算されたが、**この配列は公開地図の元ではない**
（1×1モデルの出力）。同じ配列自身の遷移行列は公開 GeoTIFF と大きく食い違う:

| | 保存 `.npy`（画像空間） | 公開 tiff（地図空間） |
|---|---|---|
| ササ純変化 | +1.0% | **+19.1%** |
| 総増加 / 2012年ササ | 34.7% | **47.9%** |
| 減少の最大転換先 | その他植生 59% | **ハイマツ 46.8%** |

したがって現時点で**公開された分類には、再代入精度すら存在しない**。

**論文への影響。**
分類精度がないまま、その分類から導かれる全数値が報告されている。
`matmet.qmd:39` は「classified each pixel」と書き、5×5パッチにも、5分割 CV の
多数決アンサンブルにも触れていない。

**査読者との関係。**
Reviewer 3「L164-170: this is a blackbox - the reader has no idea how this was done」。

**修正方針。** F-8 と併せて後述（変更リスト提案7）。

---

### F-8. 分類器の交差検証は画素単位で分割されており、空間リークがある

**何が問題か。**
`nnmodel.py:121-124` は `StratifiedKFold` を**画素インデックス**に対して適用する。
教師データは216ポリゴン、最小40画素、中央値 430–2,900 画素であるため、
**ほぼすべてのポリゴンが5つの fold すべてに分割される**。検証画素の隣接画素が学習に
入っており、報告される F1 は空間的自己相関で膨らむ。

しかも `nnmodel.py:104-111` は**検証損失が最小のエポックの指標**を保存し、
`kfold` がそれを `stratified_cv.csv` に書く。つまり選択に使った同じ fold 上の
100エポック中の最良値である。

**証拠。**

```
classIndex label        npoly     px   median   min
 1 ササ            25   38370     428    113
 2 その他植生      76  339643    2328    163
 ...
 7 ハイマツ        40  109253    1180     40
TOTAL 216 polygons, 617,571 px; 最小ポリゴン 40 px
```

これは**論文自身の HSM 方法論と矛盾する**。`matmet.qmd:99/111` は HSM に
`spatialsample` の空間ブロック分割を使うと明記している。

**教師ラベルの年代非対応も深刻。** 6つの JSON の全ポリゴンを1枚のラスタに焼き、
それを2012年スタックと2021年スタックの**両方**に同じ画素位置で適用している:

```
ラベル画素の出所:
  _mrd_085_eos_vis_20151010_1205.json  414,853 (67.2%)  [2015年画像 — リポジトリに無い]
  IMG_8298.json                         92,567 (15.0%)  [2021年]
  _mrd_..._20150926_1205.json           62,249 (10.1%)  [2015年 — 無い]
  _mrd_..._20150920_1205.json           32,916 ( 5.3%)  [2015年 — 無い]
  IMG_9304.json                          7,808 ( 1.3%)  [2012年]
  _mrd_..._20150912_1205.json            7,178 ( 1.2%)  [2015年 — 無い]

ササのポリゴン25個: 2021年フレームから17、2015年参照から8。2012年画像から0。
```

**2012年の画像に描かれたササのポリゴンは1つもない。** 変化を測っているまさにその画素で、
2012年の分類器は2015/2021年の画像から同定された正解に依存している。
かつ、ラベル4ファイルが参照する2015年写真はリポジトリに存在せず、査読者は
ラベルを元画像に照らして検証できない。

**論文への影響。** 精度の記載がないため直接動く数値はないが、新たに精度を報告するときは
この分割方法を変えないと数値が過大になる。

**査読者との関係。** Reviewer 3「important information on methodological approaches and
specific steps in the analyses are not detailed - not ensuring reproducibility」。

**修正方針。** ポリゴン単位（`GroupKFold`）または空間ブロック分割に変更し、両方報告して
膨らみの度合いを可視化する。

---

### F-9. 減少の最大転換先はハイマツであり、被覆説の根拠が弱い

**何が問題か。**
原稿（`matmet.qmd:44`, `results.qmd:12`）は、見かけ上のササ減少を「低木の被覆」で説明し、
それを根拠に**総増加 4,095 m² を主要な変化指標として採用**している。しかし遷移行列では
減少の最大転換先が**ハイマツ（*Pinus pumila*）46.8%** であり、ハイマツは匍匐性で
ササを上から覆う樹形ではない。

**証拠。** `ortho/data/vege_{2012,2021}_5x5.tiff`（NaN と mask 値0を除外、1 m画素）:

```
rows = 2012, cols = 2021
              1 Sasa  2 OtherV  3 NoVeg  4 Rowan  5 Birch  6 Alder  7 DwPine   total
1 Sasa           6079      932        3      159       85      133     1156     8547
2 OtherVeg       2171   432237    32001     3033      380     7187     9385   486402
3 NoVeg            13     5096   342186       49        0       16    10569   357937
4 Rowan           224     3738       30    17284      871     2682     2104    26934
5 Birch           155      624        0     1271     2523      703      424     5700
6 Alder           109     3647       45     2023      234    13033     1683    20774
7 DwarfPine      1425     5491     6418     1370      209     1189   278651   294760

減少 2,468: ハイマツ 1,156 (46.8%) > その他植生 932 (37.8%) > ナナカマド 159 (6.4%)
            > ミヤマハンノキ 133 (5.4%) > ダケカンバ 85 (3.4%) > 無植生 3
増加 4,097: その他植生 2,171 (53.0%) > ハイマツ 1,425 (34.8%) > ナナカマド 224 > ...

景観全体の安定率: 1,091,993 / 1,201,030 = 90.92%  ->  9.08% がクラス変化
ササ総増加 4,097 画素は、全変化 109,037 画素の 3.8%、地図全体の 0.34%
```

**さらに、ササ↔ハイマツは分類器自身の最大の混同でもある。**（1×1モデルの再代入行列
であることに注意しつつ）ササの適合率は全7クラス中最低（0.729 / 0.771）で、その主因は
ハイマツのササへの誤分類である。

**論文への影響。**
- 原稿が名指しした低木3クラス（ナナカマド・カエデ・ミヤマハンノキ）の合計は 15.3%。
- 被覆説を根拠に減少を割り引く論理が弱くなる。
- 考察 `discussion.qmd:27`「林冠下のササは調査地では稀」という記述とも整合が悪い
  （稀なら 2,467 m² を被覆で説明できない）。

**査読者との関係。** Reviewer 1 主要コメント3、そのもの。

**修正方針。** 遷移行列を本文に載せ、`review/decisions.md` ② の方針
（ハイマツも灌木でありハイマツ自体が拡大傾向、混在画素の分類がハイマツ側に倒れた可能性）に
沿って書き直す。同時に「その他植生」への 37.8% は真の減少である可能性を分離して扱う。

---

### F-10. 「種子散布による孤立パッチは観察されなかった」は地図と矛盾する

**何が問題か。**
`discussion.qmd:9`「no isolated *Sasa* patches formed by seed dispersal were observed in
the expansion areas between 2012 and 2021」は、保存済みラスタから反証される。

**証拠。** `terra::patches()`（8近傍）＋2012年ササからの距離変換:

```
2012年ササパッチ 903 ; 2021年ササパッチ 1,082
2021年パッチのうち2012年ササ画素を1つも含まないもの: 748（1,420画素 = 2021年ササの14.0%）
  そのうち >= 5 画素 かつ 2012年ササから > 10 m: 19 パッチ、139 画素

（増加画素側で数えた別集計）
増加画素のパッチ 1,534 のうち、2012年ササから全域が > 5 m 離れているもの: 342（681 m2、
拡大面積の17%）。最遠 238 m。増加画素の 11.0% が >10 m、4.6% が >20 m、1.4% が >50 m。
```

厳しめに数えても **19パッチ・139 m²** が存在する。

**論文への影響。**
これらが本物なら散布制限の議論が弱まり、分類ノイズなら拡大面積の推定が弱まる。
いずれにせよこの文はそのままでは維持できない。

**査読者との関係。**
Reviewer 3 L351「This might not only be dispersal limitation, but maybe also problems
during the establishment from seeds」。

**修正方針。** 文を撤回し、孤立パッチの実数を報告したうえで、分類ノイズと真の散布を
区別できないことを限界として書く。

---

### F-11. クラス5は「ダケカンバ（*Betula ermanii*）」であり、原稿の「Maple（*Acer tschonoskii*）」は誤り

**何が問題か。** 種名の取り違えが、原稿・コード・**公開された図の凡例**にまたがっている。

**証拠。** 教師ラベル JSON の全パース:

```
IMG_8298.json      ハイマツ×21 ササ×17 その他植生×19 無植生×15
IMG_9304.json      ハイマツ×3  ナナカマド×4
_mrd_..._20150912  その他植生×2 ダケカンバ×1
_mrd_..._20150920  その他植生×1 ナナカマド×2 ダケカンバ×6 ミヤマハンノキ×5
_mrd_..._20150926  ナナカマド×10 ダケカンバ×9 ミヤマハンノキ×9
_mrd_..._20151010  ハイマツ×16 ササ×8 その他植生×54 無植生×14
TOTAL: 0 ハイマツ 40 | 1 ササ 25 | 2 その他植生 76 | 3 無植生 29
       4 ナナカマド 16 | 5 ダケカンバ 16 | 6 ミヤマハンノキ 14   (216 polygons)
```

**カエデ（ミネカエデ）のポリゴンは1つも存在しない。**
前研究の精度図 `results/cv.png` はこのクラスを "Golden Birch" と正しく表示している。

誤りの所在:
- `paper/matmet.qmd:39`「Maple (*Acer tschonoskii*)」
- `scripts/sdm/plot_vegetation_map.R:37,77`「ミネカエデ」
- `scripts/vegetation_classification/calculate_diff.py:11`「"kaede": 4」
- **`paper/files/2012_5x5_en.jpg` / `2021_5x5_en.jpg`（図2）の凡例に "Maple" が焼き込まれている**

**論文への影響。** Methods のクラス一覧と図2の凡例。数値は動かない。

**査読者との関係。** 直接の指摘はないが、Reviewer 3 はコードを読んでおり、
ラベル JSON はテキストエディタで開ける公開ファイルである。

**修正方針。** 全箇所を Erman's birch（*Betula ermanii*）に修正し、図2を再生成。

---

## 重要な問題

### I-1. 公開リポジトリからは一段階も完走できない（幾何補正の鎖が切れている）

`ortho/data/georectified.csv`（画像画素 `u,v` → 世界座標 `x,y,z` の稠密対応表）が
**リポジトリのどこにも存在せず、それを書くコードも存在しない**。
`ortho/georectify.R:8,18` と `utils/interpolate.R:51` の双方がこれを読む。

**訂正（第一次監査の診断は誤り）**: `ortho/.Rhistory:81-176` は
「georectified.csv を作った記録」ではない。そのブロックは
`read_csv("georectificated.csv")`（**別の綴り・別のスキーマ** `pix_num, x, y`）を
**読んでいる**だけで、Procrustes+TPS は第2のカメラを参照カメラ格子に合わせる処理である。
出力 `snowmelt_ortho/*.csv`（`x, y, snow_melt`）は `georectify.R::concat_df()` が
期待する `u,v,<value>` 形式と**互換でない**。つまりリポジトリには互いに非互換な
2つの幾何補正ワークフローがあり、**どちらも `georectified.csv` を作らない**。
実際に作ったはずのカメラモデル当てはめ（`alproj`、`gcp.csv`、`params_optim.json`）は
スクリプトにも履歴にも存在しない。

クローンした第三者の実行結果:

| 段階 | 結果 |
|---|---|
| `align_photographs.py` | `data_source/aligned/2015/*` が空 → `zip()` が空回り、**無言で何も出力しない** |
| `prepare_data.py` / `run_rnn.py` | import 時に Debian のフォントパスで `FileNotFoundError` |
| `apply_mask.py` | 読むファイル名が `..._maskd.png`（実体は `..._masked.png`）→ `TypeError` |
| `image_to_csv.py` | 植生ブロックはコメントアウト、雪ブロックはパス不一致で空回り |
| `georectify.R` | `georectified.csv` 不在 |
| `preprocess_snow_data.R` | `data/snow/raw/` 不在（作るものがない）＋119-137行が実行時エラー |
| `sdm_tbm.R` / `sdm_tdm.R` | `vege_*.tiff` と `fitted_*.tiff` 不在 |

**カメラモデルの規約は復元できる（第一次監査の「復元不能」は誤り）。**
482個の GCP に対し、次の規約で中央値 **5.04 px**、RMS 8.77 px を達成した
（`params_optim.json` の `"error": 4.342` と整合）:

```
f  = (w/2) / tan(fov/2)                       # 水平FOV
R  = Rz(-pan) . Rx(tilt) . Ry(roll)           # pan=0 でカメラは +Y（北）向き
c  = R^T (X - cam);  x' = c_e/c_n,  y' = c_u/c_n
OpenCV rational (k1..k6) + tangential (p1,p2) + thin-prism (s1..s4), forward
u = cx + f*x' ;  v = cy - f*y'                # a1, a2 は未使用
```

画像判読調査では、この方向で再フィットして GCP 中央値 2.3 px / RMSE 6.3 px を達成し、
1 m セルをラスタと 79.9–81.1% のクラス一致で写真に投影できることを確認した。
**つまり幾何補正は新規スクリプトとして書き直せる。**

**修正方針。** `alproj` によるカメラパラメータ推定を1本のスクリプトに起こし、
`georectified.csv`（または `georectified.tiff`）を Zenodo に収める。

---

### I-2. 保存済みモデルは現行 xgboost では予測できない

```
$ predict(readRDS("model_stack.rds"), X[1:5,], type="prob")
Error: In index: 4. With name: default_xgb_1_02.
Caused by error in `xgb.get.handle()`:
! 'xgb.Booster' object is corrupted or is from an incompatible XGBoost version.
```

xgboost 1.7.11.1 を別ライブラリに入れると成功する（booster ごとに旧版警告）。
両アンサンブルとも xgb メンバを含むため、**どちらのモデルも再実行できない**。
Zenodo デポジットには `xgb.save()` 形式の booster と `renv.lock` が必須。

---

### I-3. 学習標本が再現不能（`thin_by_cell` が無シード、かつ `set.seed()` より前）

`tidysdm:::thin_by_cell` は冒頭で `data <- data[sample(seq_len(nrow(data))), ]` を行う。
両スクリプトともこの呼び出しが `set.seed(1)` より**前**にある
（`sdm_tbm.R:55` vs `:61`、`sdm_tdm.R:64` vs `:71`）。

```
TDM df_21 20,876 - 20,923 (presence 3,973)  train 16,540 - 16,606  [archive 16,596]
TBM df_21 27,041 - 27,062 (presence 9,974)  train 21,392 - 21,436  [archive 21,389]
archive の学習 presence は再構成 presence プールに 3123/3123、7820/7820 で一致
2回の抽選が共有する擬似欠測は約5%
```

**ただし TSS への影響は否定された（第一次監査の主張は誤り）。** 5回の独立抽選:

```
TDM tss_max: 0.6984 0.7014 0.7074 0.7020 0.6989   公表 0.70097  <- 分布の中央
TBM tss_max: 0.5551 0.5517 0.5529 0.5526 0.5477   公表 0.55559  <- 実質再現
```

**公表 TSS を 0.68 に訂正してはならない。** 問題は「厳密な分割が復元できない」ことのみ。
修正は2行（`set.seed()` を `bind_rows` の前へ）。

---

### I-4. 予測子ディレクトリの glob により、クリーンランでは別のモデルが学習される

```
$ list.files("data/terrain_features/") |> str_subset(".tif$")
aspect.tif roughness.tif slope.tif tateyamadem_small.tif TPI.tif TRI.tif twi.tif

保存済みモデルの予測子:
  models.rds         aspect roughness slope elevation TPI TRI snow dist   (twi なし)
  models_wo_dist.rds aspect roughness slope elevation TPI TRI snow        (twi なし)
$ grep -rn twi paper/*.qmd  ->  ヒットなし
```

`twi.tif` は `.gitignore` の穴から**公開されている**。コードを読んだ査読者は、
論文に一切現れない予測子を目にする。

さらに `preprocess_snow_data.R:67,96,137` は `snow_mean.tif` / `snow_sd.tif` /
`snow_reg.tif` を**同じ glob 対象ディレクトリに書く**（137行目は到達不能）。
自然な順序で実行すると予測子が 10–11 個に膨れる。

**新規: 予測子の列順がロケール依存。**

```
Sys.getlocale("LC_COLLATE") -> ja_JP
list.files order        : aspect, roughness, slope, tateyamadem_small, TPI, TRI, twi
C-locale sort           : TPI, TRI, aspect, roughness, slope, tateyamadem_small, twi
models.rds の順         : aspect, roughness, slope, elevation, TPI, TRI, snow, dist
```

列順は `ranger` の `mtry` 抽選と `xgboost` の列サンプリングを変えるため、
**`set.seed(1)` では移植性を担保できない**。明示的な順序ベクトルに置き換える必要がある。

---

### I-5. CRS 不一致でスクリプトが落ちる

```
data/terrain_features/*.tif  crs 3099 (JGD2000/UTM53N)  res 4.97 x 6.16
vege_2012_5x5.tiff           crs 6690 (JGD2011/UTM53N)  res 1 x 1
resample(terrain, vege12) 後の crs -> 3099（警告なし）
thin_by_cell 出力 crs -> 3099 ; presence は 6690
bind_rows(...) -> Error : CRS mismatch
```

保存済み `model_stack$train$geometry` は 6690 なので、当時の実行はこれを踏んでいない。
現行コードは踏む。また**すべての予測ラスタが EPSG:3099 を担いでいる**（重ねる植生図は 6690）。
測地系差はこの地域で実質ゼロなので位置誤差ではなく分類上の誤りだが、
`project()` ベースの修正をすると格子がずれるので注意が必要。

---

### I-6. 「1 m 解像度」は7予測子中6つで名目にすぎない

地形予測子7枚はすべて **4.97 × 6.16 m** で、`resample()`（既定 = bilinear）で 1 m に
補間されている。原稿は「5 m DEM を 1 m にリサンプル」とは書いているが、
**モデルは実質 5 m 分解能の説明変数に 1 m の応答を当てている**ことは書いていない。

**新規: `aspect` は 0/360° の継ぎ目を跨いで双一次補間されている。**

```
円環的な正しいリサンプル（sin/cos 補間 -> atan2）との差:
  n = 3,154,478 px ; median 0.003 deg ; q99 151.57 deg ; max 180.00 deg
  frac > 45 deg : 0.0510 (160,881 px) ; frac > 90 deg : 0.0325
```

359° と 1° を補間すると約180°になる。**予測子そのものが壊れており**、
モデル側で northness/eastness 分解しても、分解をリサンプル**前**に行わない限り直らない。

---

### I-7. `aspect` は円環変数だが線形として扱われている

他の予測子を学習中央値に固定して aspect を 0→360° 掃引:

```
TBM: HS(0)=0.4966  HS(359)=0.5505  HS(360)=0.5505  継ぎ目 = 0.0538
     aspect による HS の全変域 0.4966-0.6330 (幅 0.1364)
     -> 継ぎ目は aspect 効果の 39.5%、しかも 0.5 の閾値をまたぐ
TDM: 継ぎ目 0.0028（dist に埋もれる）

default_maxent_1_02 の maxnet 係数:
  hinge(aspect):351.909:359.224 = +0.1797
  hinge(aspect):242.187:359.224 = -0.1466
```

MaxEnt と XGBoost がアンサンブル重みの100%を占めるため、この欠陥は実効的である。
359° 向きの画素は「適地」、1° 向きの同一画素は「不適地」になる。

---

### I-8. 重度の共線性 {roughness, slope, TRI}

実際のモデリングフレーム上で計算（サンプリングではない）:

```
TDM VIF: TRI 25.545  roughness 16.194  slope 11.200  snow 1.371  TPI 1.256
         elevation 1.093  aspect 1.085  dist 1.028
TBM VIF: TRI 25.445  roughness 17.603  slope 10.650  snow 1.493  TPI 1.209
         aspect 1.101  elevation 1.082
Pearson: roughness-TRI 0.970, slope-TRI 0.943, roughness-slope 0.932
```

3つはほぼ重複。他はすべて VIF < 1.5 で問題ない。
Reviewer 2 が言う通り、順列重要度はこの状況で共有シグナルを3分割し、
それが Fig 6 で TRI/roughness/slope が 0.005–0.07 に沈み、実行ごとに順位が入れ替わる機構である。

**なお、これは既に試されている**: 孤児成果物 `ortho/models_all_5m.rds` の予測子は
`slope, snow, dist, elevation, aspect, TPI`（roughness と TRI を除いた6変数）である。

---

### I-9. 変数重要度の2パネルは比較可能な条件で計算されていない

| | `sdm_tdm.R:176-200` | `sdm_tbm.R:169-190` |
|---|---|---|
| explainer | `DALEXtra::explain_tidymodels` | `DALEX::explain` |
| データ | **`df_train`**（16,596行） | **`df_test`**（約5,600行） |
| `model_parts` | 既定 `N=1000`, `B=10` | 同じ |
| シード | **なし** | **なし** |

```
TBM, TEST  (公表側): snow .179-.193  elevation .129-.149  TRI .048-.066  ...
TBM, TRAIN         : snow .390-.398  elevation .191-.210  ...    <- 約2倍
TDM, TRAIN (公表側): dist .4497  snow .1973  elevation .0486  ...
TDM, TEST          : dist .3513  snow .1183  ...
```

TBM は学習データだと重要度が約2倍になるため、**「TDM で dist が突出している度合いは、
TBM で snow が突出している度合いより大きい」という視覚的印象は、部分的に分割の違いの
産物**である。順序に関する主張（TBM は snow が最上位、TDM は dist → snow）は
どの条件でも成立する。

シードがないため公表図は再現しない。`ortho/figures/vi_tbm.png`（コード通り、
`ylim(c(0,0.6))` あり、roughness > aspect）と `paper/files/vi_tbm.jpg`（`ylim` なし、
aspect > roughness）は**別の実行・別の版のスクリプト**である。

**もう1つの欠陥**: `predict_function = stacks::predict.model_stack` に `type` を渡して
いないため、explainer は確率ではなく `.pred_class`（因子）を見ている。
したがってここでの「TSS」は閾値0.5の TSS（実測 0.5218）であり、
他所で報告される `tss_max`（0.7010）とは**別の量**である。

**なお `predict_function_target_column = "presense"` のスペルミスは無害**（後述の
「誤りだったリード」参照）。

---

### I-10. 5 m² ポリゴンフィルタが未記載で、しかも複数の数値を規定している

`sdm_tbm.R:243-250` / `sdm_tdm.R:229-245` の
`filter(area > units::set_units(5, m^2))`。原稿には一切記載がない。

```
2012 polygons: 1171 (8547 m2) -> >5 m2: 196 (7074 m2)   [975 パッチ, 17% の面積を除去]
2021 polygons: 1440 (10176 m2) -> >5 m2: 231 (8328.49 m2)  [1209 パッチ, 18.1% の面積]
```

このフィルタが規定しているもの:
1. 予測時の距離レイヤ（F-4）
2. 図7・8・9 の黒い輪郭線 — **2021年ササ面積の 18.1% が図に現れない**。
   凡例にもキャプションにも説明がない（Reviewer 3「Figure 7: black areas are unclear」）
3. 「2030年に不適地化する面積」の分子

**3の影響は大きい。** フィルタなしの2021年ササラスタで分子を計算し直すと:

```
TBM lost (全2021年ササ画素)  3102 px  expanse 3100.309  -> 30.5%
TDM lost (全2021年ササ画素)  2360 px  expanse 2358.715  -> 23.2%
（公表: TBM 2,257 m2 = 21%[原文] / 22.2%[正] ; TDM 717 m2 = 7.05%）
```

**フィルタは TDM の減少予測を 3.3 倍抑えている**（717 → 2,359 m²）。
小さいパッチほど不適地化が予測されやすい（1,843画素中843画素 = 46%）。

---

### I-11. 「21%」は誤りで 22.2%（TDM の 7% は正しい）

```
TBM 2257.768 / 10170.454 = 22.199 %   <- 原稿は 21%
TDM  716.609 / 10170.454 =  7.046 %   <- 原稿は 7%（正しい）
（分母を揃えて 8,328.46 m2 にすると 27.1% / 8.6%）
```

「newly suitable」側の割合（57.2%, 34.4%）は正しい。

---

### I-12. HS > 0.5 は未較正の既定値で、感度分析がない

```
risky area の閾値感度:
  0.40 : 70,368 px   0.45 : 52,429   0.50 : 36,797
  0.55 : 20,374      0.60 :  8,918   0.65 :  3,222
  -> ±0.05 で +42.5% / -44.6%

2021年ラスタを観測ササ図に照らした最適閾値:
  TBM: best threshold 0.36, TSS@0.5 = 0.561, best TSS 0.621
  TDM: best threshold 0.12, TSS@0.5 = 0.692, best TSS 0.785
```

報告されている TSS は `tss_max`（閾値最適化後）だが、**すべての地図と面積は固定 0.5**で
作られている。2つの異なる量が同じラベルで報告されている。
TDM を TSS 最適の 0.12 にすると論文中のすべての適地・risky 面積が変わる。

**査読者との関係。** Reviewer 1 #5「Is 0.5 a calibrated ecological threshold or simply a
default probability cut-off? Sensitivity to alternative thresholds should be reported」。

---

### I-13. アンサンブルに RF と GAM は入っていない

```
model_stack.rds (TDM)          member_fits: maxent×3 + xgb×4 = 7
model_stack_wo_dist.rds (TBM)  member_fits: maxent×2 + xgb×2 = 4
非ゼロ LASSO 係数: rf 0/8, gam 0/1 (TDM) ; rf 0/7, gam 0/1 (TBM)
TDM 重み: maxent 17.21% / xgb 82.79%    TBM: maxent 23.79% / xgb 76.21%
```

Methods は「四つのアルゴリズムを適用」と述べ、Fig 5 は4本の系列を示す。
結果本文の「within the TDM, GBT, MaxEnt, and RF outperformed GAM」も、
RF は図にはあるがモデルには寄与していない。

---

### I-14. 標高 2,560 m の除外は根拠が誤りで、影響が大きい

```
sdm_tbm.R:59 / sdm_tdm.R:68 : filter(elevation < 2560)
除去される画素: 799,744 / 1,206,233 = 66.3%（地図化された画素）
2,560 m 以上のササ: 2012年 22 画素、2021年 71 画素
ササの最高標高: 2012年 2,625.5 m、2021年 2,711.5 m
```

原稿 `matmet.qmd:87`「areas above 2,560 m — where *Sasa* does not occur — were excluded」は
**両方の半分が誤り**（ササは存在するし、除去は「上部を少し」ではなく地図の2/3である）。
図7–9のすべての HS 面積は、シーンの下位1/3のみで計算されている。
これは Reviewer 3 が尋ねる「白い/灰色の未説明領域」の正体でもある。

---

### I-15. 撮影日・欠測年の記述が実データと合わない

```
EXIF DateTimeOriginal:
 2012: 08-27 09-01 09-11 09-17 09-26 10-06 10-21   (DOY 240 245 255 261 270 280 295)
 2021: 08-24 08-31 09-07 09-19 09-24 10-02 10-14   (DOY 236 243 250 262 267 275 287)
 差:    -4    -2    -5    +1    -3    -5    -8 日
```

原稿 `matmet.qmd:30`「images taken in September–October」— 14枚中3枚は8月、1枚は10月21日。
分類器の特徴量は**この7日付の RGB のみ**（紅葉の軌跡）であり、2021年系列は
最も判別力の高い最終フレームで8日早い。放射補正は行われていない。

融雪側:
- **2019年は全段階で不在**（`supplement.qmd:20` の図キャプションにのみ記載、Methods にはなし）
- **2010年は `data/snow/aligned/` に存在するが `raw/` に入っていない**（無言の除外）

---

### I-16. `georectify.R` の 1 m セル値は「最後に来た画像画素」である

```r
georectify.R:28  ras <- st_rasterize(points, dx = res, dy = res)
```

`fun = terra::modal` は 40行目の `focal()` 穴埋めにしか渡されていない。実証:

```r
d  <- data.frame(x=c(0.2,0.5,0.8), y=c(0.2,0.5,0.8), data=c(1,1,7))  # 同一1mセル
st_rasterize(...) -> 7
d2 <- ... data=c(7,1,1)                                             -> 1
```

行順が値を決める。`image_to_csv.py:6-8` はラスタ走査順で書くので、
各 1 m セルは寄与画素のうち走査順で最後のものの値を取る。
マスク内画像画素 ~1,178万 に対し地図セル ~121万（平均10:1、近景ではさらに多い）なので
**分類画素の約90%が捨てられている**。増加画素の57.8%が2012年境界から1.5 m以内にある
ことを踏まえると、これは変化統計における**最大の未記載ノイズ源**である。

関連: `run_rnn.py:42-43` の `torch.mode` は同数の場合に**最小のクラス値を返す**。
クラス0はササである。2–2–1 や全異なりの同点はすべてササに配分される。

---

### I-17. 融雪予測子は半ピクセルずれており、双一次補間で平滑化されている

```
raw / vege   ext=[732744.0 734545.0 4050316.25 4052069.25]
fitted_2012  ext=[732743.5 734544.5 4050316.75 4052069.75]   -> (-0.5, +0.5)
原因: stars::as_tibble(add_max=TRUE) がセル境界を出し、terra::rast(<df>) が中心と解釈
resample の既定 = bilinear (確認済) -> 各入力値は 2x2 ブロックの算術平均
  fitted_2012 max|bilinear-nearest| = 50.38 DOY
  fitted_2021 58.51 ; fitted_2030 131.74（雪線で最悪）
```

**なお `drop_na()`（153行）が極端年の画素を削っている**: 2011年で 3,480画素欠落
（うち3,342は10年揃った画素）、2013/2014/2016/2017/2020/2021 でも数画素。
削られるのはレバレッジが最大の端の年であり、中立な間引きではない。

---

### I-18. 2021年と2030年の予測ドメインが違う（ただし公表面積は歪んでいない）

```
sasa_pred_*_21.tiff  n=397,403     sasa_pred_*_30.tiff  n=407,658
in30not21 10,255 ; in21not30 0
原因: fitted_2021.tiff が50行短く、かつ 27,849 セル多く NA（2021年の無観測27,996画素）
```

**重要な訂正**: `tidyterra::filter` は NA を除外として伝播するので、差分は既に交差集合上で
計算されている。公表数値6種はすべて厳密に再現し、最大影響は **TBM +1,171 m²（+4.3%）、
TDM ちょうど 0**。無言のカバレッジ欠落であって面積の水増しではない。
ただし地図の範囲が上下段で異なって見えるので、注記は必要。

---

### I-19. 公開図版とリポジトリの図版の差（大半は注記のみ）

```
SAME  cv_dist, cv_wo_dist, initial_split_dist, initial_split_wo_dist,
      model_performance_tbm, model_performance_tdm, risky_tdm, snowmelt_doy, snowmelt_shift_map
DIFF  hsdiff_tbm, hsdiff_tdm, hsmap_tbm_2021/2030, hsmap_tdm_2021/2030, risky_tbm,
      vi_tbm, vi_tdm, snowmelt_shifting
```

**訂正**: 10枚中7枚（hsdiff×2, hsmap×4, risky_tbm）の差は**方位記号と200 mスケールバーのみ**で、
RGB の差は同一の18,992画素領域に限定され全画素の0.264%。地図本体は同一である。
本当に別実行なのは `vi_tbm` / `vi_tdm`（I-9）と `snowmelt_shifting`（下記）。
`annotation_scale()` / `annotation_north_arrow()` はリポジトリのどこにもない。

**図1・図2・図4には生成コードがない。**
- 図2の英語版スクリプトは削除済み（`plot_vegetation_map.R` は日本語ラベルで
  `ortho/data/2012_5x5.png` に書くが、そのファイルは存在しない）
- 図4は GSI 地理院地図ベースマップ上の手作業合成（玉殿岩屋・立山室堂山荘・地理院地図の
  日本語が焼き込まれている）
- **図1・2・4だけが 300 dpi を満たしていない**: overview 144 dpi、vegemap 183 dpi、
  expanded_area 128/217 ppi。他はすべて 299.5 dpi。
  図2は `paper/files_original_size/` に 3600×2400 の PNG があるのに、
  `matmet.qmd:57` が 1081×721 の JPEG を読んで 2400×800 に**拡大**している
  （解析コストゼロで Reviewer 1 の指摘が解消できる）

**補遺図S1もコードで再現しない**: `preprocess_snow_data.R:211-227` は
絶対 DOY の散布図＋`geom_smooth(lm)` を描くが、保存図は2011年基準の偏差の箱ひげ図である。
補遺図S3は `plot_snowmelt_shifts_map.R:38` で |shift| > 50 日を NA にしている
（347画素、0.0295%、キャプションに記載なし）。

---

### I-20. risky 域の定義は同語反復であり、Reviewer 1 の問いに答えられない

```
risky_area_wo_dist.tiff (TBM, 図9)  n=36,797  2021年クラス構成: {2: 36,797}  = 100% その他植生
   2012年クラス: 0:1 1:366 2:33,953 3:45 4:519 5:209 6:1,141 7:563
risky_area_tdm.tiff                 n=3,149   2021年: {2: 3,149}
面積: TBM 36,777 m2 ; TDM 3,147 m2  <- どちらも原稿に一度も現れない
```

risky 域は `vege21 == 2` と**定義されている**ので、構成が100%「その他植生」になるのは
自明である。「その他植生」は7クラス分類器の残余バケツ（`matmet.qmd`「alpine shrubs and
herbaceous plants」）であり、Reviewer 1 の「どの高山植物群落・種が risky 域にあるか」には
答えられない。取り出せる唯一の情報は 2012年のクラス構成（92.3% が既にその他植生）である。

なお Methods の定義（`vege21==2` かつ TBM 2030 HS > 0.5）は 37,138画素になり、
コード（`drop_na()` により2021年予測が非 NA という条件が加わる）の 36,797画素と
341画素ずれる。

---

## 軽微な問題

| 項目 | 内容 | 影響 |
|---|---|---|
| `analyse_sdm.R` 全体 | `ggsave` 0回、`writeRaster` 0回。入力は旧世代の孤児ラスタ。その `dist` は `selected_comms.tiff`（任意の群落ID）からの距離であり、`sdm_tdm.R` の `dist` とは別物 | 論文の数値・図に一切寄与しない。削除または `exploratory/` へ |
| `sdm_tdm.R:278-280` | `is_others <- ifelse(layer == 6, ...)` はクラス6＝ミヤマハンノキ。「その他植生」はクラス2。ただし `grep` で参照は3行のみ＝**デッドコード** | なし |
| `sdm_tdm.R:405` | TDM の risky 図のタイトルが `"Risky area (TBM)"` | 公開図には未使用。リポジトリ内の図が誤ラベル |
| `sdm_tbm.R:474-479` | `snow_21 %>% rename(snow_12 = snow) %>% c(snow_21)` — 「2012年」パネルが実は2021年。保存済み補遺図S2は別スクリプト製なので無害 | 潜在。再実行者は誤った補遺図を得る |
| `num.threads = 18` | ranger の引数。xgboost には未知パラメータとして渡り、実際は `nthread = 1` で単スレッド実行された。GAM 仕様は `update_workflow_model` で丸ごと置換されるため引数を持たない | なし（死んだ設定） |
| `tss_score_tbm.rds` | `.csv`（0.5556、論文の値）と食い違う 0.5348。どのスクリプトも書かない | なし。削除 |
| 孤児 risky ラスタ2枚 | `risky_area.tiff`（10,281 m²、閾値0.2の旧定義）、`potential_sasa_area_21.tiff`（17,664 m²）。どの予測ラスタとも一致しない | なし。削除 |
| `data/sasa_inc.tiff` / `selected_comms.tiff` | `sdm_tbm.R:446` と `analyse_sdm.R` が**読む**が、どのコードも作らない。前者は `(vege12 != 1) & (vege21 == 1)` と完全一致するので再生成可能 | クリーンランで `sdm_tbm.R` が446行で落ちる |
| `sasa_pred_tdm_30_bin.tiff` | `sdm_tdm.R:443` が `overwrite=TRUE` なしで書く | 再実行でエラー |
| `setwd()` 4種 | `~/doctoral_thesis/chap2/ortho/`, `~/Projects/jasms2023f/ortho/`, `~/Projects/jasms2023f//`, `~/VegetationMapPaper/`。`plot_snowmelt_shifts_map.R` は `setwd` なしで `ortho/` 相対 | 全スクリプトが素の環境で動かない |
| ドライバスクリプト不在 | 実行順序を示すものがない | 再現性 |
| `utils/utils.py:20-22` | import 時に Debian の日本語フォントパスを読む → macOS で `FileNotFoundError`。Python パッケージ全体が使えない | 分類コードの最初の障壁（`device="cuda"` ではない） |
| `apply_mask.py` | ファイル名 `..._maskd.png` のタイプミス＋マスク画素をクラス値0（＝ササ）に潰す。`results/*_masked.npy` ではマスクとササが区別できない（0の97.1%がマスク） | 公開 GeoTIFF は無事（0=マスク、1..7=クラス、3通りに証明済み） |
| `draw_teacher` | classIndex 1（ササ）を0にリマップし未ラベル0と衝突。`teacher.npy` からササ教師画素が復元不能（`teacher.png` は無事） | なし |
| `mask.npy` の来歴 | どのスクリプトも作らない。`mask_sky.py` の出力とは79.0%しか一致しない。再構成マスクとも 4,975画素食い違う。**実際に適用されたマスクはリポジトリに存在しない第4の物体** | 再現性 |
| `utils_old.py` | `utils.py` とバイト同一 | 削除 |
| `align_photographs.py` | (1) 204-240行に手書きリストの**第2段階アライメント**があり、ラベル付き2フレーム双方を含む7枚を年内参照に再合わせして上書きする。(2) 実際に使う `homography_lensdist` は `ratio = 1` で Lowe 比テストを無効化。(3) "rmse" 列は RANSAC インライアの平均ユークリッド残差でありRMSEではない。(4) `results` を作るが**書き出さない**（`alignment_report_2012_2021.csv` は孤児で、12枚/年・別命名の**別の画像集合**を記述している） | 位置合わせ自体は良好。報告書は証拠にならない |
| `2030` のクランプ | `[0,255]` は2030年のみに適用（16画素）。素の予測は −378〜+403。歴史年は未クランプで `fitted_2011` max 238.46 / `fitted_2020` 238.72（観測最大232を超過） | 非対称、かつ診断を隠している |
| 閏年処理なし | DOY 120 は閏年で4/29、他で4/30。誘導される傾きバイアスは +0.009 d/yr | Methods の正確さのみ |
| `georectified.tiff` | 4,251個の Inf と 1e33 級の値（`interpolate.R:53` の focal 平均が Inf を伝播）。約1,000画素が破損 | SDM は消費しない。公開オルソ写真 |
| `tateyama2.tiff` | 156 MB の切り詰められた TIFF（先頭 IFD オフセット 2,222,976,440 > ファイルサイズ 163,577,856）。`gdal.Open` が失敗。参照ゼロ | 削除 |
| 2017/2018 の融雪 PNG | ゼロマスクがバイト同一（他のどの年の組でも一致しない）のに値は1,190万画素で異なる。無観測マスクの使い回しの疑い | 要確認 |
| 未追跡ファイル | `ortho/data/vege_*_5x5.tiff.aux.xml`（GDAL 統計サイドカー）。`*.tiff` パターンに合致しないので `git add -A` で入る | `.gitignore` に追加 |
| 公開すべきでないもの | `ortho/.Rhistory`（外付け HDD パスを露出、かつ幾何補正の唯一の記録）、`data/labels/.Rhistory`（0 B）、`.DS_Store` 16個、`results/cv.png`（前研究の図）、`paper.zip`（`covering_letter.doc` 入り） | — |
| git 履歴 | `paper/` は `67fe2d5` で削除されたが公開履歴から完全に復元可能。`git show 58da9ef:paper/covering_letter.doc \| strings` で著者の所属メールが出る | 履歴書き換えか新規リポジトリでないと消えない |

---

## 誤りだったリード

`known_leads.md` および第一次監査から出たもので、**検証の結果 誤りだったもの**。
これらに基づいて作業してはならない。

| # | 誤ったリード | 正しい事実 |
|---|---|---|
| 1 | `spatial_initial_split(prop = 0.2)` は「80%学習」の記述と矛盾する | **矛盾しない。** `tidysdm` は `v <- round(1/prop) = 5` ブロックを作り1つを検定に回す。実測 TBM 21,428/27,055 = 79.2%、TDM 16,572/20,891 = 79.3%。**原稿の記述は正しい** |
| 2 | 原稿の 8,542 / 10,170 / 4,095 / 2,467 は保存ラスタと「わずかに違う」ので定義を突き合わせる必要がある | **突き合わせ不要。** `terra::expanse()` の測地面積（UTM面積スケール 0.999455）。全10数値が丸め誤差内で一致。Methods に1文足りるだけ |
| 3 | `predict_function_target_column = "presense"` のスペルミスが説明対象を変えている | **無害（no-op）。** この属性は `DALEXtra:::yhat.model_stack` 内でのみ読まれ、`predict_function` を明示している両スクリプトでは到達しない。`"presence"` でも引数省略でも `model_parts` の結果はビット同一 |
| 4 | 保存済みモデルは `twi` を含んでいるかもしれない | **含んでいない。** 予測子順が `ja_JP` ロケールの `list.files()` 順から twi を除いたものと完全一致 |
| 5 | `scale_color_discrete(labels = c("GBT","GAM","MaxEnt","RF"))` は Fig 5 のモデル名を取り違えている | **正しい。** `autoplot.workflow_set` は `model` 列（アルファベット順 `boost_tree, gen_additive_mod, maxent, rand_forest`）で着色する。ラベルは対応している |
| 6 | `preprocess_snow_data.R:118-126` は**構文的に無効**である | **構文は有効。** ファイルは34式にパースされる。失敗は**実行時**（`partit` 未定義、`.` 不在）。結論（このブロックは走っていない）は正しいが、「syntax error」と査読者に言ってはならない |
| 7 | `is_others = ifelse(layer == 6, ...)` がクラスコード表と矛盾し、下流に影響する | **バグではあるがデッドコード。** 参照は278-280行のみ。公開数値への影響ゼロ |
| 8 | risky 域ブロックの `terra::resample(vege21, .)` がカテゴリラスタを双一次補間で壊している | **壊していない。** ブロック全体を再実行して保存ラスタとビット一致（36,797 / 3,149）。潜在的な脆弱性のみ。（なお第一次監査が挙げた理由「両者とも 1753×1801 だから no-op」も誤り。`drop_na()` が 1147×1213 に切り詰めた格子が対象で、同じ 1 m 格子上に載っているから恒等になる） |
| 9 | 2030年融雪層の 0–255 レンジが将来予測を汚染している | **汚染していない。** `elevation < 2560` の 407,658 セル中、≤0 が1、≥250 が2 |
| 10 | 2021/2030 のドメイン不一致が「新たに適地」面積を水増ししている | **していない。** NA が除外として伝播するため差分は交差集合上。最大影響 TBM +1,171 m²（+4.3%）、TDM 0 |
| 11 | `thin_by_cell` が無シードなので、公表 TDM の 0.70 は 0.68 前後に訂正すべき | **訂正してはならない。** 5回の独立抽選で 0.6984–0.7074。公表値は分布の中央。TBM も実質再現。問題は「分割が復元できない」ことのみ |
| 12 | `run_rnn.py` が動かない主因は `device="cuda"` の既定値 | **違う。** 主因は `utils/utils.py:20-22` の Debian フォントパスを import 時に読むこと。`FileNotFoundError` で全モジュールが使えない |
| 13 | `snow_reg.tif` の食い違いは、2010年を含む11年で計算したためである | **違う。** 現行 raw ラスタ10枚は**バイト同一の NA マスク**を持つ（同じ `georectified.csv` 点集合に焼かれているため）。年を足しても `drop_na` footprint は変わらず、1,133,175 は出ない。まして 27,275セルが現行 footprint の**外**にあることを説明できない。真因は**より古い幾何補正ヴィンテージ**（2011-2018+2020 の9年） |
| 14 | `ortho/.Rhistory:81-176` を `00_georectify_tps.R` に昇格させれば `georectified.csv` を再生成できる | **できない。** そのブロックは別名・別スキーマの `georectificated.csv` を**読んでいる**。出力形式も `georectify.R` の期待と非互換。新規に `alproj` カメラ推定を書く必要がある |
| 15 | `sdm_tbm.R` は存在しない `models_wo_dist_twi.rds` を読んで落ちる | **落ちない。** 127行の `saveRDS` が2行前に作る。真の欠陥は (a) RF しか学習しない、(b) 公表図を作った `models_wo_dist.rds` をどの行も書かない、の2点 |
| 16 | 公開図版は保存図版と**実質的に**異なる（別の解析結果） | **10枚中7枚は方位記号とスケールバーの追加のみ**（差分は同一の18,992画素領域、全体の0.264%）。地図本体は同一。真に別実行なのは `vi_tbm` / `vi_tdm` / `snowmelt_shifting` の3枚 |
| 17 | `params_optim.json` の投影規約は復元できない（最良でも中央値 28 px） | **復元できる。** 上記 I-1 の規約で 482 GCP に対し中央値 5.04 px / RMS 8.77 px。`"error": 4.342` と整合。画像判読側の再フィットは中央値 2.3 px |
| 18 | 再代入精度 OA 0.928 / 0.932 は公開分類の精度上限として使える | **使えない。** これは `results/*_masked.npy`（1×1モデルの出力）から計算されたもので、公開地図の元ではない。同じ配列自身の遷移行列は公開 tiff と大きく異なる（減少の最大転換先が「その他植生 59%」になる）。**公開分類には再代入精度すら存在しない** |
| 19 | VIF は TRI 34.3 / roughness 19.5 / slope 12.9（20万画素サンプル） | 方向は正しいが数値は更新。**実際のモデリングフレーム上で** TRI 25.5 / roughness 16.2 / slope 11.2（TDM）。`twi` はモデルに入っていないので VIF を持たない |
| 20 | 減少の 46.8% がハイマツというのは頑健な事実である | **分類器の実行に依存する。** 同系統の分類器の独立な実行（1×1、`results/*_masked.npy`）では「その他植生 59% > ハイマツ 27%」になる。公開 tiff からの遷移行列そのものは正しいが、この配分は分類器の実行間で頑健ではない |
| 21 | `sdm_tbm.R` の `models_wo_dist_twi.rds` 問題は「ファイル欠落」である | 実際は**スクリプトのドリフト**。公開 TBM を作った版のスクリプトがリポジトリに存在しない |

---

## 再現性の現状

### クリーンランと保存済み成果物の対照

| | 保存済み成果物 (b) | 今日のクリーンラン (a) |
|---|---|---|
| 融雪年 | 10年（2011-2018, 2020, 2021） | 11年（`data/snow/aligned/` に2010が居るため） |
| TBM のアルゴリズム | RF + GAM + MaxEnt + XGB | **RF のみ**（3つがコメントアウト） |
| TBM の出力ファイル名 | `models_wo_dist.rds` | `models_wo_dist_twi.rds` |
| 予測子 | 7 / 8 | 8 / 9（twi）、Stage 5 を先に走らせると 10 / 11 |
| 予測子の列順 | `ja_JP` collation 順 | ロケール依存で変わりうる |
| CRS | 学習フレームは 6690 | `thin_by_cell` 後に 3099 → `bind_rows` で **abort** |
| `snow_reg` の平均 | −0.8636（原稿の −0.86） | −0.7147（そもそも生成ブロックが実行時エラー） |
| 変数重要度図 | ylim 固定なし版（公開）／あり版（リポジトリ） | シードなしのため毎回別物 |
| `sasa_inc.tiff` / `selected_comms.tiff` | 存在 | **作れない**。`sdm_tbm.R:446` で落ちる |
| 幾何補正 | 完了済み | **不可能**（`georectified.csv` 不在） |
| 分類器 | CRNN 5×5, 5-fold, 200 epoch | **ソースが無い**。`run_rnn.py` は別モデル（1×1） |
| アンサンブルでの予測 | 可能（当時） | **不可能**（xgboost メジャーバージョン非互換） |

### 今日、第三者が公開リポジトリから再現できること

**実質的に何もない。** クローンした人が受け取るのは 127ファイル・947 MB:

- 生 JPG 14枚、位置合わせ済み PNG 14枚
- ラベル JSON 6ファイル
- 融雪 PNG 11年分（2010–2021）
- 地形予測子 `.tif` 7枚（**`twi.tif` を含む**）
- **陳腐化した** `snow_{mean,sd,reg}.tif` 3枚
- `gcp.csv`、`params_optim.json`、`matched.png`
- 全出力図、`tss_*.csv` 2本
- 全スクリプト、`.DS_Store` 16個、`ortho/.Rhistory`

受け取らないもの: `vege_{2012,2021}_5x5.tiff`（**応答変数**）、`fitted_*.tiff`（**融雪予測子**）、
`snow/raw/*.tiff`、DEM 各種、`.rds` モデル5つ、`.npy` 各種、そして
**`georectified.csv`**。

公開リポジトリだけでできるのは、地形予測子7枚と陳腐化した融雪ラスタ3枚を R に読み込んで
眺めることだけである。**しかもそのうち `snow_reg.tif` の平均を計算すると −0.8636 が出て、
論文の −0.86 と一致し、モデルが使った −0.715 とは一致しない。**
現状、公開ファイルの中で最も危険なのはこの3枚である。

### Zenodo デポジットに入れるべきもの

**Tier A — 必須入力（約 482 MB、うち1点は再生成が必要）**

| 項目 | サイズ |
|---|---|
| `data/images/source/**`（生 JPG 14枚） | 271 MB |
| `data/images/mrd_085_eos_vis_20151010_1205.png`（位置合わせ基準） | 24 MB |
| `data/images/mrd_085_eos_vis_20151010_1205_masked.png`（実際に使われたマスク） | 18 MB |
| `data/labels/*.json` | 0.3 MB |
| **2015年のラベル対象写真4枚** | ★ **回収必須**（現在リポジトリに無く、ラベルの67%＋10%＋5%＋1%がこれを参照） |
| `data/snow/aligned/*.png`（11年） | 37 MB |
| `ortho/data/gcp.csv` + `params_optim.json` | 0.03 MB |
| `ortho/data/mrd_dem_1m.tiff`（元 DEM） | 126 MB |
| **`ortho/data/georectified.csv`** | ★ **再生成必須** |

**Tier B — 派生物（約 134 MB。これがあれば SDM 単体は再現可能）**

`vege_{2012,2021}_5x5.tiff` (1.9 MB) / `terrain_features/*.tif` 7枚 (41 MB) /
`snow/raw/*.tiff` 10枚 (18 MB) / `snow/fitted_*.tiff` 11枚 (48 MB) /
`dem_small.tiff` (12 MB) / `sasa_inc.tiff`, `selected_comms.tiff` (13 MB)

Tier A + B ≈ **0.6 GB**。Zenodo の 50 GB 上限に十分収まる。

**Tier C — 入れない（約 1.5 GB）**: 位置合わせ済み PNG（Tier A から再生成可）、
`mask.npy`、`results/*.npy`（Tier B の地図空間版と重複）、
`georectified.tiff`（Inf 混入）、`tateyama2.tiff`（**破損**）、`paper.zip`、ビルド成果物。

編集部が結果の厳密検証のためにモデルを求める場合は `model_stack.rds`（38 MB）と
`model_stack_wo_dist.rds`（24 MB）の2つだけでよい。ただし
**`xgb.save()` 形式で booster を書き出し直し、`renv.lock` を同梱すること**
（現状では読めない）。`models.rds` / `models_wo_dist.rds` / `models_all_5m.rds` は
チューニングオブジェクトで、後2者は陳腐化している。

**デポジット前に必ず除去すべきもの**: `ortho/.Rhistory`（正式スクリプト化してから削除）、
`data/labels/.Rhistory`、`.DS_Store` 16個、`results/cv.png`（前研究）、
`snow_{mean,sd,reg}.tif`（陳腐化・再現不能）、`tateyama2.tiff`（破損）、
孤児成果物群（`models_all_5m.rds`, `tss_score_tbm.rds`, `risky_area.tiff`,
`potential_sasa_area_21.tiff`, `sasa_pred_sdm_12.tiff`, `sasa_pred_sdm_dist_{21,30}.tiff`）。

ライセンスは現状 GPL-3.0（コード用）のみ。データには別途 CC-BY-4.0 等の明示が必要。

**なお、`paper/` は公開 git 履歴から完全復元可能**（著者の所属メールを含む
`covering_letter.doc` を含む）。ディレクトリの削除では消えていない。
履歴の書き換えか、新規リポジトリの作成が必要かどうかは判断事項である。
