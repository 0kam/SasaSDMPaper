# Adversarial verification — vegetation-classification subsystem

Everything below was re-run independently. Environment:

- `python3.12` venv at `<scratch>/vfy/venv` (numpy 2.x, pandas, shapely, pillow, opencv 5.0, scikit-learn, scipy)
- pre-existing `python3.10` venv at `<scratch>/venv310` (torch 2.x, torchvision, torch_optimizer, sklearn, tensorboardX, cv2 4.10)
- `<scratch>/venv` (python 3.14 + GDAL 3.12.1) for raster reads
- R 4.5.2 + terra/sf/stars

`<scratch>` = `/private/tmp/claude-501/-Users-okamoto-NIES-SasaSDMPaper--claude-worktrees-review-summary-ece-ddc69c/98ac7ecf-eafb-49c6-aa2d-9aab2998c13e/scratchpad`

Nothing under `/Users/okamoto/NIES/SasaSDMPaper` was modified by me. (See §M8 — the
previous auditor *did* modify three files there.)

---

## Verdicts

### 1. `crnn-model-deleted` — CONFIRMED (with three corrections)

pyc header parse (`struct.unpack("<IIII", header)`):

```
models/__pycache__/crnn.cpython-310.pyc     src_mtime=2023-11-11 21:26:35 src_size=2392  pyc_mtime=2025-12-12 11:42
models/__pycache__/nnmodel.cpython-310.pyc  src_mtime=2025-12-12 11:42:52 src_size=7955  pyc_mtime=2026-08-07 12:35
models/__pycache__/rnn.cpython-310.pyc      src_mtime=2025-12-12 11:42:52 src_size=2037  pyc_mtime=2026-08-07 12:35
utils/__pycache__/utils.cpython-310.pyc     src_mtime=2025-12-12 11:42:59 src_size=8083  pyc_mtime=2026-08-07 12:35
models/__pycache__/nnmodel.cpython-38.pyc   src_mtime=2022-06-22 19:01:03 src_size=7925
models/__pycache__/rnn.cpython-38.pyc       src_mtime=2022-06-22 19:38:08 src_size=2046
models/__pycache__/svm.cpython-38.pyc       src_mtime=2022-06-22 16:56:51 src_size=5499
utils/__pycache__/utils.cpython-38.pyc      src_mtime=2022-06-22 18:58:39 src_size=8083
```

Disassembly of `crnn.cpython-310.pyc` (python3.10 + `marshal`) confirms the architecture and
the source path:

```
LOAD_CONST 6 (<code object CRNN ... file "/home/okamoto/Projects/jasms2023f/scripts/models/crnn.py", line 7>)
CRNN.__init__  names: (super, __init__, x_shape, nn, Conv2d, conv1, BatchNorm2d, bn2d_1,
                       PReLU, prelu1, MaxPool2d, maxpool1, int, h_dim, LSTM, lstm,
                       BatchNorm1d, bn1, Dropout, do1, Linear, fc1, prelu2, bn2, do2, fc2)
               consts: [None, 3, 8, (3, 3), 1, 2, 4, True, ('batch_first',), 0.0]
CRNNClassifier consts: ['CRNNClassifier', 'cuda', 20, 'all', 0.2, 'jet', (5, 5), ...]
```

Note the conv kernel is `(3,3)`; the `(5,5)` is `CRNNClassifier`'s default *patch* `kernel_size`.

**Correction 1 — the pyc is not in the archive either.** `__pycache__/` is gitignored:

```
$ git check-ignore -v scripts/vegetation_classification/models/__pycache__/crnn.cpython-310.pyc
.gitignore:10:__pycache__/	scripts/.../crnn.cpython-310.pyc
$ git ls-files scripts/vegetation_classification
  apply_mask.py calculate_diff.py models/nnmodel.py models/rnn.py prepare_data.py
  run_rnn.py utils/interpolate.R utils/utils.py utils/utils_old.py   (+ .DS_Store)
```

So on GitHub there is no trace of the CRNN at all. The recoverable bytecode is a local
accident, not part of the deposited record.

**Correction 2 — part of the finding's evidence is circular and the underlying evidence is gone.**
`nnmodel/rnn/utils.cpython-310.pyc` carry `pyc_mtime = 2026-08-07 12:35` — they were written by
the previous auditor's own `python run_rnn.py` invocation, which compiles all three modules
before dying in `utils.py`. Their `src_size` therefore equals the current source by
construction; it proves nothing about dating. The pre-existing 3.10 pycs, which could have
dated the tracked sources, were overwritten. The surviving 3.8 pycs show the tracked
`nnmodel.py` (7955 B) and `rnn.py` (2037 B) are *not* byte-identical to the 2022-06-22
versions (7925 / 2046); only `utils.py` is unchanged in size (8083 = 8083).

**Correction 3 — the provenance chain is more specific than `interpolate.R`.**
`scripts/sdm/image_to_csv.py:16-17`, commented out, is the actual link:

```python
#in_paths  = ["results/use_this/2012_5x5.npy", "results/use_this/2021_5x5.npy"]
#out_paths = ["ortho/data/2012_5x5.csv", "ortho/data/2021_5x5.csv"]
```

and `ortho/georectify.R:49-50` turns `data/{2012,2021}_5x5.csv` into
`data/vege_{2012,2021}_5x5.tiff`. Neither `results/use_this/` nor `*_5x5.npy` exist.

---

### 2. `npy-zero-ambiguous` — CONFIRMED (numbers reproduce to the digit)

```
mask.npy zeros: 9246554
2012 values: {0:9515000, 1:6611414, 2:1691081, 3:376522, 4:214259, 5:528289, 6:2089739}
  2012 pred==0 total 9515000  & mask==0: 9239464  & mask!=0: 275536  frac_masked=0.9710
  2021 pred==0 total 9517941  & mask==0: 9239562  & mask!=0: 278379  frac_masked=0.9708
teacher.npy: {0:20446688, 1:339439, 2:40632, 3:29282, 4:12758, 5:48130, 6:109375}
```

Independent re-implementation of `read_sses` + `cv2.fillPoly` over `data/labels/*.json`:

```
my label raster (classIndex): {0:20408733, 1:38370, 2:339296, 3:40593, 4:29238, 5:12738, 6:48086, 7:109250}
total labelled px: 617571
teacher.npy == classIndex-1 on labelled px: 617571 / 617571
teacher==0 & my label==Sasa(1): 38370
teacher==0 & my label==0     : 20408318
```

`apply_mask.py:4` typo verified as fatal:

```
cv2.imread('..._maskd.png') -> None ;  None[:,:,0] -> TypeError: 'NoneType' object is not subscriptable
```

Blue-channel indexing: `aligned/2012/IMG_8748.png` has 51,969 px with B==0 but only 51,570
fully black → 399 px silently masked. Reproduced exactly.

Minor addendum (benign): `teacher.npy` has 415 labelled pixels my rasterisation does not
have. All 415 are isolated single pixels adjacent to a polygon edge (415 components, max
size 1) — an OpenCV-version boundary-rounding difference, not a provenance problem.

---

### 3. `birch-called-maple` — CONFIRMED

Every label JSON parsed:

```
IMG_8298.json    ハイマツ×21 ササ×17 その他植生×19 無植生×15
IMG_9304.json    ハイマツ×3  ナナカマド×4
_mrd_..._20150912 その他植生×2 ダケカンバ×1
_mrd_..._20150920 その他植生×1 ナナカマド×2 ダケカンバ×6 ミヤマハンノキ×5
_mrd_..._20150926 ナナカマド×10 ダケカンバ×9 ミヤマハンノキ×9
_mrd_..._20151010 ハイマツ×16 ササ×8 その他植生×54 無植生×14
TOTAL by classIndex: 0 ハイマツ 40 | 1 ササ 25 | 2 その他植生 76 | 3 無植生 29
                     4 ナナカマド 16 | 5 ダケカンバ 16 | 6 ミヤマハンノキ 14   (216 polygons)
```

No maple polygon exists. `results/cv.png` (rendered) has a panel titled **Golden Birch**.
`plot_vegetation_map.R:11/21/37/77` say `ミネカエデ` / `# Maple`; `calculate_diff.py:11`
`"kaede": 4`; `matmet.qmd:39` "Maple (*Acer tschonoskii*)".

**Addendum the finding did not state**: the error is baked into the published figure.
`paper/files/2012_5x5_en.jpg` (used to build `files/vegemap.jpg` for `@fig-vege12-21`)
carries a legend reading `Dwarf Pine / Dwarf Bamboo / Rowans / Maple / Montane Alder /
Other Vegetation / No Vegetation / NA`.

---

### 4. `no-accuracy-assessment` — CONFIRMED, but the fallback numbers are mis-attributed

No `runs/`, `*.pth`, `stratified_cv.csv` or confusion matrix anywhere. `results/` contains
exactly `2012_masked.{npy,png} 2021_masked.{npy,png} cv.png teacher.{npy,png}`.
`results/cv.png` rendered: 7 facets (Dwarf Pine, Dwarf Bamboo, Rowans, Golden Birch,
Montane Alder, Other vegetation, Non Vegetation), x-axis 2015-08-25 … 2015-10-10 plus
"Multidays RNN 1x1" and "Multidays1x1" — 1×1 models, 2015 imagery, no 5×5 entry.
Manuscript grep for accuracy/F1/confusion/kappa returns only HSM/TSS material.

I reproduced the offered fallback exactly (in-mask labelled px, reconstructed mask):

```
2012: n=617568 OA=0.9282 kappa=0.8918
2021: n=617568 OA=0.9315 kappa=0.8960
```

**But these are computed from `results/*_masked.npy`, and the repo's own provenance says
that is not the array behind the published maps** (see §M1). The finding's own retraction
list says so explicitly ("the archived .npy files are not the arrays the maps were made
from") while findings 4 and 6 use them as "the archived maps". The numbers are therefore
not an upper bound on the accuracy of the published classification; they characterise a
different model output.

---

### 5. `pixel-level-cv-leakage` — CONFIRMED

`nnmodel.py:35` `self.idx = list(range(len(self.ds.tensors[1])))` (one entry per pixel);
`nnmodel.py:121-124` `StratifiedKFold(...).split(self.idx, self.ds.tensors[1])`.

Per-polygon pixel counts (each polygon rasterised separately, union):

```
classIndex label        npoly    px    median   min
 1 ササ            25    38370    428    113
 2 その他植生      76   339643   2328    163
 3 無植生          29    40594    818    178
 4 ナナカマド      16    29238   1064    539
 5 ダケカンバ      16    12738    654    160
 6 ミヤマハンノキ  14    48086   2898    611
 7 ハイマツ        40   109253   1180     40
TOTAL 216 polygons, 617571 px; smallest polygon 40 px
pixels covered by >=2 polygons: 351 (only 3 px claimed by two different classes)
```

With k=5 and a 40-px minimum, every polygon is split across folds. Contrast
`matmet.qmd:99/111` (spatial block splitting) — HSM section only. Confirmed.

**Second, independent optimistic bias the finding missed**: `nnmodel.py:104-111` saves
`self.best_metrics = res` at the epoch with the lowest *validation* loss, and
`kfold` (line 134) writes exactly those into `stratified_cv.csv`. The reported per-fold F1
is therefore the maximum over 100 epochs on the same fold used to select it.

---

### 6. `sasa-pine-confusion` — PARTLY CONFIRMED

**Transition half: exact.** From the GeoTIFFs (terra, 0 and NaN excluded):

```
        1      2      3      4      5      6      7      (cols = 2021)
1    6079    932      3    159     85    133   1156
2    2171 432237  32001   3033    380   7187   9385
3      13   5096 342186     49      0     16  10569
4     224   3738     30  17284    871   2682   2104
5     155    624      0   1271   2523    703    424
6     109   3647     45   2023    234  13033   1683
7    1425   5491   6418   1370    209   1189 278651
```

Loss 1156/932/159/133/85/3 and gain 2171/1425/224/155/109/13 — identical to the finding.

Corrections: landscape stability is **1,091,993 / 1,201,030 = 90.92 %** once the mask class
0 is excluded (the finding's 1,097,160 / 1,206,233 = 90.96 % includes 0→0 as a "class").
Distance percentiles: p50 1.4, p75 3.6, **p90 11.2**, **p95 18.4**, **p99 80.2**,
**max 238.4 m** (finding: 11.4 / 18.6 / 80.8 / 242.9); 57.8 % ≤1.5 m and 81.1 % ≤5 m match.

**Confusion half: reproduces exactly but describes a different product.**

```
mask=reproduced  year=2012  n=617568  OA=0.9282 kappa=0.8918
               Sasa   OtherVeg      NoVeg      Rowan ErmanBirch      Alder  DwarfPine  recall
      Sasa      34935       1722          0         19         28          9       1657  0.910
 DwarfPine       5592       1834       2632        272        162        478      98280  0.900
precision       0.729      0.983      0.867      0.954      0.840      0.954      0.874
year=2021: DwarfPine->Sasa 8047, Sasa precision 0.771, OA 0.9315 kappa 0.8960
```

But the *same array's own* transition matrix (image space, in-mask) inverts the finding's
headline:

```
npy image space: Sasa2012=275951 Sasa2021=278799 gain=95692 loss=92844 net=+2848 (+1.0 %)
  gross gain / 2012 Sasa = 34.7 %   unchanged fraction 86.2 %
  Sasa loss destinations: OtherVeg 54919 (59 %) > DwarfPine 25085 (27 %)
  Sasa gain sources     : OtherVeg 52849 (55 %) > DwarfPine 38716 (40 %)
```

Published tiffs: net +19.1 %, gross gain 47.9 %, unchanged 90.9 %, loss dominated by
DwarfPine (46.8 %). The two halves of the finding therefore come from two different
products, and on the `.npy` product the claimed "Dwarf pine is the largest destination of
lost Sasa" is false. The *transition-matrix* statement (from the tiffs) stands; the
juxtaposition with the confusion matrix does not.

---

### 7. `run-rnn-cannot-run` — CONFIRMED

```
$ cd <copy of scripts/vegetation_classification> && venv310/bin/python run_rnn.py
Traceback (most recent call last):
  File "run_rnn.py", line 1, in <module>
    from models.rnn import RNNClassifier
  ... File "utils/utils.py", line 22, in <module>
    plt.rcParams["font.family"] = font_prop.get_name()
FileNotFoundError: [Errno 2] No such file or directory:
  '/usr/share/fonts/truetype/migmix/migmix-1p-regular.ttf'
```

(`FontProperties(fname=...)` constructs fine; `get_name()` is what raises.)
All twelve paths I checked are absent: `../data/{2012,2015,2021}`, `../data/2012_5x5`,
`../data_source/labels`, `../data_source/aligned/{2012,2015,2021}`,
`../data_source/normalized`, `../data_source/aligned_composite`, `../results`, `runs`.
No `data_source/` exists anywhere in the repo. No `requirements.txt`, `environment.yml`,
`pyproject.toml` or `renv.lock` anywhere.

Addendum: `align_photographs.py:174-176` *creates* `data_source/aligned_composite/{2012,2021}`
but never writes into them, while `prepare_data.py:11-13` reads them; and nothing anywhere
produces `data_source/normalized/*` used by `prepare_data.py:7-9`.

---

### 8. `labels-not-year-specific` — CONFIRMED

Labelled pixels by source JSON (last-painted attribution):

```
_mrd_085_eos_vis_20151010_1205.json  414853 (67.2 %)   [2015, image absent]
IMG_8298.json                         92567 (15.0 %)   [2021]
_mrd_085_eos_vis_20150926_1205.json   62249 (10.1 %)   [2015, absent]
_mrd_085_eos_vis_20150920_1205.json   32916 ( 5.3 %)   [2015, absent]
IMG_9304.json                          7808 ( 1.3 %)   [2012]
_mrd_085_eos_vis_20150912_1205.json    7178 ( 1.2 %)   [2015, absent]
```

Sasa polygons: 17 from `IMG_8298` (2021) + 8 from `_mrd_..._20151010` (2015) = 25; none
from a 2012 frame. `IMG_9304.json` (the only 2012 label file) contains only ハイマツ and
ナナカマド. `prepare_data.py:3,5` pass the same `../data_source/labels` for both years.

---

### 9. `alignment-report-wrong-images` — CONFIRMED and strengthened

The CSV's 24 rows all target the single file `data_source/mrd_085_eos_vis_20151010_1205.png`
(the commented-out `align_photographs.py:181`), listing 12 `mrd_085_eos_vis_*` sources per
year. The archived photographs are 7 `IMG_*.JPG` per year. **EXIF timestamps prove they are
a different set:**

```
2012: 08-27 12:00, 09-01 13:00, 09-11 14:00, 09-17 12:00, 09-26 12:00, 10-06 12:00, 10-21 12:00
2021: 08-24 11:00, 08-31 12:00, 09-07 12:00, 09-19 12:46, 09-24 10:57, 10-02 12:00, 10-14 11:59
CSV 2012: 0828_1100 0901_1000 0904_1700 0911_1400 0918_1300 0922_0600 0926_1200 0929_1500
          1002_0700 1006_0700 1010_0900 1018_1400
```

09-17 and 10-21 (2012) appear in no CSV row; 09-01 and 10-06 appear at different hours.

Direct residual measurement (AKAZE, ratio 0.75, RANSAC 5 px, half-res, raw inlier
displacement rescaled ×2) confirms alignment is genuinely good:

```
2012/IMG_9038 vs 2021/IMG_8172  n=3381 median=1.19 p90=3.65 max=13.66 px
2012/IMG_9304 vs 2021/IMG_8298  n= 804 median=1.88 p90=5.64 max=10.56 px
2012/IMG_9038 vs 2012/IMG_9304  n=1390 median=1.34 p90=4.62 max=14.45 px
2021/IMG_8172 vs 2021/IMG_8298  n=9064 median=0.77 p90=3.14 max=11.86 px
2012/IMG_8748 vs 2021/IMG_7763  n=2571 median=1.43 p90=4.44 max=10.31 px
2012/IMG_9514 vs 2021/IMG_8298  n=4929 median=1.30 p90=4.02 max=16.80 px
```

See §M6 for three things about `align_photographs.py` the finding missed.

---

### 10. `mask-provenance` — CONFIRMED

```
masked.png fully-black px            : 9241580
sky_mask_2015.png fully-black px     : 4816892
agreement (masked==0)vs(sky==0)      : 0.7895
reproduced mask (masked.png[:,:,0] then IMG_8748==0) zeros : 9241581
data/images/mask.npy zeros                                 : 9246554
disagreement                                               : 4975  (4974 one-way)
2012 pred!=0 but reproduced mask==0 : 2532     2021: 2439
IMG_8748.png: B==0 51969, fully black 51570 -> 399 px silently masked
```

Addendum: `mask.npy` is *worse*, not better — `pred!=0 & mask.npy==0` is 7,090 (2012) and
6,992 (2021). `masked.png | union-of-all-14-aligned-black` gives 9,243,084 zeros with 3,863
conflicts. No reconstructible mask reproduces the archived zero set, so the mask actually
applied is a fourth object not present in the repo. `mask_sky.py` and `mask.npy` are
referenced by no script.

---

### 11. `manuscript-areas-reconcile` — CONFIRMED

```
s12    cells=  8547 expanse= 8542.3407 ratio=0.99945486   (manuscript 8,542)
s21    cells= 10176 expanse=10170.4538 ratio=0.99945497   (manuscript 10,170)
gain   cells=  4097 expanse= 4094.7684 ratio=0.99945531   (manuscript 4,095)
loss   cells=  2468 expanse= 2466.6553 ratio=0.99945513   (manuscript 2,467)
CRS 6690 JGD2011 / UTM 53N, res 1 m
```

`results.qmd:11` reports 8,542 / 10,170 / net 1,628 / 4,095 / 2,467. Net = 10170.45 −
8542.34 = 1628.11. Nothing to reconcile; retraction correct.

---

### 12. `tiff-encoding-1based` — CONFIRMED

```
v12: 0:5179 1:8547 2:486402 3:357937 4:26934 5:5700 6:20774 7:294760 NaN:1950920
v21: 0:5191 1:10176 2:451770 3:380686 4:25189 5:4302 6:24943 7:303976 NaN:1950920
sasa_inc vs (v12!=1 & v21==1): agreement 1.0, n=1206233, both count 4097
class-0: 41 components, bbox rows 0-1752 cols 0-1544
class-1: 903 components, bbox rows 224-1333 cols 15-1199
```

Correction: the alternative `(v12!=0)&(v21==0)` encoding agrees at **0.99658**, not 0.99869.
Addendum: the class-0 (mask) set is *not* exactly year-invariant — 5,179 vs 5,191 cells,
symmetric difference 36 — because `georectify.R`'s `focal(3, modal, na.policy="only")`
gap-fill can create 0s. Another reason to give 0 a real NoData flag.

---

### 13. `isolated-patches-claim` — CONFIRMED

`discussion.qmd:9` verbatim: "no isolated *Sasa* patches formed by seed dispersal were
observed in the expansion areas between 2012 and 2021".

```
2012 Sasa patches 903 ; 2021 Sasa patches 1082 (8-connectivity)
2021 patches with NO 2012 Sasa cell: 748, 1420 cells = 14.0 % of 2021 Sasa
   of those: median 1 cell, max 40 cells, 51 patches >= 5 cells
   >= 5 cells AND > 10 m from any 2012 Sasa: 19 patches, 139 cells
```

The headline (19 patches / 139 m²) is exact. Minor correction: the finding's
"median 1 cell, max 1242 cells, 248 patches >= 5 cells" describes **all 1,082** 2021 Sasa
patches (I reproduce 1082 / median 1 / max 1242 / 248), not the 748 new ones.

---

### 14. `kernel-reshape-trap` — CONFIRMED (empirically, not just by reading)

Built a synthetic fixture (3 dates, 40×30 px, per-channel constant so channel identity is
visible; 2 rectangular SSE polygons), ran the real `set_patches`:

```
out5/1/imgs.npy (64, 3, 75) uint8      # (N, T, 3*5*5)
5x5 sample[0][t=0] blocks of 25: [80] , [50] , [10]   -> CHANNEL-MAJOR confirmed
after NNClasifier's `x.reshape(x.shape[0], -1, 3)`: torch.Size([64, 75, 3])
row0 first triples: [0.3137,0.3137,0.3137] x5   -> each "RGB triple" is 3 adjacent
                                                   same-channel pixels
out1/1/imgs.npy (64, 3, 3)  -> reshape gives (N, T, 3), correct for 1x1
```

`rnn.py:43-44` passes 8 positional args to `super().__init__`, so `kernel_size` is always
`(1,1)`. The recovered `CRNNClassifier` defaults to `(5,5)` and so skips the reshape.
`utils.py:127` `min(w, u+kh)` uses the kernel *height* for the horizontal bound (harmless
for square kernels). All as claimed.

---

### 15. `dead-and-broken-scripts` — CONFIRMED

```
$ diff utils/utils.py utils/utils_old.py            -> byte identical
$ ls results/  -> 2012_masked.{npy,png} 2021_masked.{npy,png} cv.png teacher.{npy,png}
   (no 2012_5x5.npy, no 2021_5x5.npy, no use_this/, no runs/)
$ grep -n is_others scripts/sdm/*.R  -> only sdm_tdm.R:278-280 (layer == 6 = Montane alder)
$ grep -rn sasa_inc  -> read in analyse_sdm.R:28 and sdm_tbm.R:446; no writeRaster anywhere
image_to_csv.py:15-23 vegetation block entirely commented out
setwd roots: ~/VegetationMapPaper/, ~/Projects/jasms2023f/, ~/doctoral_thesis/chap2/ortho/,
             ~/Projects/jasms2023f//   (4 distinct roots, plus ortho/.Rhistory shows two more)
```

---

### 16. `imsave-autoscale` — CONFIRMED

Decoded `results/2012_masked.png` against `results/2012_masked.npy` (BGR):

```
npy 0 -> [0,0,0] 9239432 (mask) and [50,205,154] 275568 (Sasa)
npy 1 -> [180,130,70] 6611414      npy 2 -> [192,192,192] 1691081
npy 3 -> [60,20,220]  376522       npy 4 -> [0,215,255]   214259
npy 5 -> [19,69,139]  528289       npy 6 -> [0,100,0]    2089739
```

Exactly `run_rnn.py`'s `ListedColormap`, because min=0 and max=6 are both present. Code is
unsafe as claimed.

---

### Retractions — all four/five are correct

The maple, area, `.npy`-vs-tiff and `is_others` retractions are all correct as written, and
so is the "cuda default is not the blocker" one (I reproduced the font traceback).
The `.npy` retraction is however *inconsistent with findings 4 and 6*, which then use
`results/*_masked.npy` as "the archived maps".

---

## Missed findings

### M1. The audit's headline accuracy numbers come from an array that is not the published map

`apply_mask.py:9-11` reads `results/{year}.npy` — the output of `run_rnn.py:44`, the 1×1
`RNNClassifier` branch — and writes `results/{year}_masked.npy`. The published chain is
`results/use_this/{year}_5x5.npy` → `ortho/data/{year}_5x5.csv` → `georectify.R` →
`vege_{year}_5x5.tiff` (`image_to_csv.py:16-17`), and `calculate_diff.py:16,18` and
`interpolate.R:40-43` confirm the 5×5 arrays were separate files
(`results/cnn_lstm5x5_cv_5_ep_200/`). Nothing in the archive links `*_masked.npy` to the
tiffs.

Quantitatively they diverge far beyond resampling noise:

| | archived `.npy` (image space, in-mask) | published tiff (map space) |
|---|---|---|
| net Sasa change | +2,848 px = **+1.0 %** | +1,629 m² = **+19.1 %** |
| gross gain / 2012 Sasa | **34.7 %** | **47.9 %** |
| unchanged fraction | **86.2 %** | **90.9 %** |
| dominant Sasa-loss destination | **Other vegetation 59 %** | **Dwarf pine 46.8 %** |
| Sasa 2021 / Sasa 2012 | 1.010 | 1.191 |

I also attempted a map-space comparison using the projection left in the scratchpad
(`vege_proj.npz`) and **discarded it**: rendering it shows a completely different footprint
(2.17 M valid cells vs the tiff's 1.21 M) and near-zero correlation (r≈0.01) against
`ortho/data/georectified.tiff`. That array is not a faithful reconstruction of the
georectification and any map-space number derived from it is void.

Consequence: findings 4 and 6 must be re-scoped. There is currently **no** accuracy number,
not even a resubstitution one, for the classifier that produced the published maps.

### M2. `st_rasterize` keeps the *last* image pixel per cell — the 1 m class is one arbitrary pixel, not a modal vote

`georectify.R:28` `ras <- st_rasterize(points, dx = res, dy = res)`. `fun = terra::modal` is
passed only to the `focal()` gap-fill on line 40. Demonstrated:

```r
d  <- data.frame(x=c(0.2,0.5,0.8), y=c(0.2,0.5,0.8), data=c(1,1,7))   # all in one 1 m cell
st_rasterize(st_as_sf(d, coords=c("x","y")), dx=1, dy=1)  -> 7
d2 <- ... data=c(7,1,1)                                   -> 1
```

Row order decides the value, and `image_to_csv.py:6-8` writes rows in raster-scan order, so
each 1 m cell takes the class of whichever contributing image pixel comes last in scan
order. There are ~11.8 M in-mask image pixels behind ~1.2 M map cells (≈10:1 on average,
far more in the near field), so ~90 % of the classified pixels are discarded, and at the
Sasa fringes — where 57.8 % of the reported gain sits, within 1.5 m of the 2012 boundary —
the class of a cell is effectively a coin flip among its contributing pixels. This is the
single largest undocumented source of noise in the change statistics, and it is trivially
fixable (`st_rasterize(..., options = ...)` or an explicit modal aggregation).

### M3. `torch.mode` breaks ties toward class 0 = Sasa

`run_rnn.py:42-43` ensembles the 5 fold predictions with `torch.mode`. PyTorch returns the
*smallest* of the tied modal values:

```
votes 6,6,0,0,2 -> 0.0        votes 6,6,3,3,2 -> 3.0        votes 1,2,3,4,0 -> 0.0
```

Class 0 is Sasa. Every 2–2–1 or all-distinct tie is awarded to the class the paper is about.

### M4. Reported CV metrics are best-epoch-on-the-validation-fold

`nnmodel.py:104-111` stores `self.best_metrics` at the epoch with lowest validation loss,
and `kfold` (line 134) writes exactly those to `stratified_cv.csv`. Combined with M5's
pixel-level splitting this makes the (already missing) F1 figures doubly optimistic.

### M5. The 2012 and 2021 image series are not phenologically matched, and two frames fall outside the stated window

From EXIF (`DateTimeOriginal`), matched by position in the sorted series the classifier
consumes:

```
pos      1      2      3      4      5      6      7
2012  Aug27  Sep01  Sep11  Sep17  Sep26  Oct06  Oct21     DOY 240 245 255 261 270 280 295
2021  Aug24  Aug31  Sep07  Sep19  Sep24  Oct02  Oct14     DOY 236 243 250 262 267 275 287
Δ       -4     -2     -5     +1     -3     -5     -8  days
```

The classifier's only features are the RGB values at these seven dates — i.e. the autumn
colouring trajectory. The 2021 series is up to 8 days earlier at the final, most
phenologically discriminating time point, and spans 51 days against 2012's 55. Any
between-year class difference therefore mixes real vegetation change with an acquisition-date
offset. Nothing in the manuscript or the code normalises for this, and `matmet.qmd:34` states
the images were taken in "September–October", whereas `IMG_8748` (2012-08-27),
`IMG_7763` (2021-08-24) are August and `IMG_9514` (2012-10-21) is late October.

### M6. Three things in `align_photographs.py` the alignment finding missed

1. **A second, hand-listed alignment pass (lines 204-240)** re-aligns 7 of the 14 photographs
   — including *both* labelled frames, `IMG_9304` (2012) and `IMG_8298` (2021) — to
   *within-year* references (`aligned/2012/IMG_8819`, `aligned/2012/IMG_9038`,
   `aligned/2021/IMG_8172`), overwriting the outputs of the first loop. So the archived
   stacks are the product of a two-stage, partly manual, chained registration that no
   documentation describes and the CSV does not cover.
2. **The Lowe ratio test is disabled in the function actually used.** `homography_lensdist`
   line 30 sets `ratio = 1`, so `m.distance < 1 * n.distance` accepts essentially every
   knn match and only RANSAC filters. (The unused `homography` uses `ratio = 0.75`.) This
   explains both the 9,785-match and the 19-match rows in the report.
3. **The "rmse" column is not an RMSE.** Lines 89 and 164 compute
   `np.mean(sqrt(dx² + dy²))` — a mean Euclidean residual over RANSAC inliers only.

### M7. Class codes depend on which label files happen to be present

`utils.py:57-60`: `max_class_idx = max(objects["classIndex"]); class_idx[class_idx == 0] =
max_class_idx + 1`. ハイマツ is stored as `classIndex 0` and is remapped to `max+1`. That
maximum is computed over whatever JSONs `glob(label_dir + "/*")` returns. Drop the
ミヤマハンノキ file and ハイマツ silently becomes 6 instead of 7, shifting every downstream
code, colour and GeoTIFF value. There is no fixed class dictionary anywhere in the repo.

### M8. The previous auditor modified the user's repository

`scripts/vegetation_classification/{models/nnmodel,models/rnn,utils/utils}.cpython-310.pyc`
have mtime **2026-08-07 12:35** — written by their own `python run_rnn.py`. The brief for
this audit was investigation-only outside `review/audit`. The practical damage is that the
original 3.10 bytecode, which was the only thing that could have dated the tracked
`nnmodel.py` / `rnn.py` / `utils.py` against the 2023-11 CRNN run, is gone. (The files are
gitignored so `git status` is clean and nothing tracked was touched.)
