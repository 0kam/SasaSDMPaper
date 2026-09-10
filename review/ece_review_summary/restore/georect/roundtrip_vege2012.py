"""
End-to-end check of the recovered georectification.

Chain under test
    results/2012_masked.npy   (image space, 3744 x 5616, class 0..6)
        --[ georectified.csv : (u,v) -> (x,y) ]-->
    ortho/data/vege_2012_5x5.tiff  (map space, 1 m, EPSG:6690)

For every row of the archived georectified.csv we read the class the classifier assigned
to pixel (u,v) and the class the published raster carries at map position (x,y), and
tabulate the agreement.  This tests the transform, the class-code convention and the
`interpolate()` rasterisation in one go.

Usage: python3 roundtrip_vege2012.py [stride]
"""
import sys
import numpy as np
from osgeo import gdal

gdal.UseExceptions()

NPY = "/Users/okamoto/NIES/SasaSDMPaper/results/2012_masked.npy"
CSV = "/Users/okamoto/NIES/SasaSDMPaper/data_from_server/ortho/data/georectified.csv"
TIF = "/Users/okamoto/NIES/SasaSDMPaper/data_from_server/ortho/data/vege_2012_5x5.tiff"

# RASTER codes, from scripts/sdm/plot_vegetation_map.R:31-42.  The npy carries the same
# ordering shifted down by one, and npy 0 additionally absorbs the sky/mask, so npy 0 is
# ambiguous between "Sasa" and "masked" -- see restore_georectification.md 3c.
CLASSES = {0: "(masked)", 1: "Sasa", 2: "Other vegetation", 3: "No vegetation",
           4: "Rowan", 5: "Maple/Birch", 6: "Alnus", 7: "Pinus pumila"}


def main(stride):
    img = np.load(NPY).astype(np.int16)          # (v, u)
    ds = gdal.Open(TIF)
    gt = ds.GetGeoTransform()
    ras = ds.GetRasterBand(1).ReadAsArray()
    H, W = ras.shape
    print(f"image array {img.shape}, raster {ras.shape}, gt={gt}")

    us, vs, xs, ys = [], [], [], []
    with open(CSV) as f:
        f.readline()
        for n, line in enumerate(f):
            if n % stride:
                continue
            p = line.rstrip("\n").split(",")
            if len(p) < 8:
                continue
            try:
                us.append(int(p[0])); vs.append(int(p[1]))
                xs.append(float(p[2])); ys.append(float(p[3]))
            except ValueError:
                continue
    u = np.array(us); v = np.array(vs)
    x = np.array(xs); y = np.array(ys)
    print(f"sampled {len(u)} georectified.csv rows (every {stride}th)")

    col = np.floor((x - gt[0]) / gt[1]).astype(int)
    row = np.floor((y - gt[3]) / gt[5]).astype(int)
    inside = (col >= 0) & (col < W) & (row >= 0) & (row < H)
    print(f"inside published raster extent: {inside.sum()} / {len(u)} "
          f"({100*inside.mean():.2f}%)")

    u, v, col, row = u[inside], v[inside], col[inside], row[inside]
    cls_img = img[v, u]
    cls_map = ras[row, col]
    valid = np.isfinite(cls_map)
    cls_map = cls_map[valid].astype(int)
    cls_img = cls_img[valid]
    print(f"raster cell non-NA: {valid.sum()} ({100*valid.mean():.2f}% of inside)")

    print("\nclass code histograms")
    for name, arr in (("image (npy)", cls_img), ("raster (tiff)", cls_map)):
        vals, cnts = np.unique(arr, return_counts=True)
        print(f"  {name:14s}", {int(a): int(b) for a, b in zip(vals, cnts)})

    for shift in (0, 1, -1):
        agree = (cls_map == cls_img + shift).mean()
        print(f"  agreement with raster == npy {shift:+d} : {100*agree:.4f}%")

    best = max((0, 1, -1), key=lambda s: (cls_map == cls_img + s).mean())
    print(f"\nadopting raster = npy {best:+d}")
    k = 8
    cm = np.zeros((k, k), int)
    np.add.at(cm, (np.clip(cls_img + best, 0, k - 1), np.clip(cls_map, 0, k - 1)), 1)
    print("rows = image class (+shift), cols = raster class")
    print("      " + "".join(f"{j:>9d}" for j in range(k)))
    for i in range(k):
        if cm[i].sum() == 0:
            continue
        print(f"{i:>4d}  " + "".join(f"{cm[i, j]:>9d}" for j in range(k))
              + f"   | acc {100*cm[i, i]/cm[i].sum():6.2f}%  {CLASSES.get(i,'')}")
    print(f"\noverall agreement {100*np.trace(cm)/cm.sum():.4f}%  (n={cm.sum()})")

    # Sasa-specific (raster code 1); note the surviving rows of georectified.csv cover
    # image rows v = 529..1268 only, which contain no Sasa at all -- expect zeros here.
    si, sm = (cls_img + best == 1), (cls_map == 1)
    tp = (si & sm).sum(); fp = (~si & sm).sum(); fn = (si & ~sm).sum()
    print(f"\nSasa (raster code 1): image {si.sum()}, raster {sm.sum()}, both {tp}")
    if sm.sum() == 0:
        print("  -> no Sasa in the surviving band of georectified.csv, as expected; "
              "use validate_map_to_image.py for the near field")


if __name__ == "__main__":
    main(int(sys.argv[1]) if len(sys.argv) > 1 else 20)
