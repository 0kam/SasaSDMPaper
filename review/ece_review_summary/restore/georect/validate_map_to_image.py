"""
Near-field validation of the recovered camera model, and a re-derivation of the
map->image lookup that the truncated georectified.csv can no longer provide.

The archived georectified.csv only survives for image rows v = 529..1268, which contain
essentially no Sasa.  So the round-trip test against it (verify_georectified_csv.py)
only proves the model in the far field.  This script tests the near field instead, by a
route that does not use georectified.csv at all:

    for every 1 m cell of vege_2012_5x5.tiff
        z  <- mrd_dem_1m.tiff
        (u,v) <- recovered camera model
        compare the published raster class at the cell against
        results/2012_masked.npy at (u,v)

If the recovered transform were wrong, the class agreement would collapse to chance.

Also builds a z-buffer over the same projection so occluded cells can be excluded, and
reports agreement with and without that filter.

Usage: python3 validate_map_to_image.py [dem_upsample]
"""
import sys
import time
import numpy as np
from osgeo import gdal
from alproj_camera import load_params, project_render

gdal.UseExceptions()

D = "/Users/okamoto/NIES/SasaSDMPaper/data_from_server/ortho/data/"
VEGE = D + "vege_2012_5x5.tiff"
DEM = D + "mrd_dem_1m.tiff"
NPY = "/Users/okamoto/NIES/SasaSDMPaper/results/2012_masked.npy"

# raster code -> label, from scripts/sdm/plot_vegetation_map.R:31-42
LAB = {0: "(unlabelled)", 1: "Sasa", 2: "Other veg", 3: "No veg",
       4: "Rowan", 5: "Maple/Birch", 6: "Alnus", 7: "Pinus pumila"}


def read(path):
    ds = gdal.Open(path)
    gt = ds.GetGeoTransform()
    a = ds.GetRasterBand(1).ReadAsArray()
    nd = ds.GetRasterBand(1).GetNoDataValue()
    del ds
    return a, gt, nd


def sample_dem(dem, gt, x, y):
    col = (x - gt[0]) / gt[1]
    row = (y - gt[3]) / gt[5]
    c0 = np.floor(col).astype(int); r0 = np.floor(row).astype(int)
    fc = col - c0; fr = row - r0
    H, W = dem.shape
    ok = (c0 >= 0) & (c0 < W - 1) & (r0 >= 0) & (r0 < H - 1)
    c0 = np.clip(c0, 0, W - 2); r0 = np.clip(r0, 0, H - 2)
    z = (dem[r0, c0] * (1 - fc) * (1 - fr) + dem[r0, c0 + 1] * fc * (1 - fr)
         + dem[r0 + 1, c0] * (1 - fc) * fr + dem[r0 + 1, c0 + 1] * fc * fr)
    return z, ok


def main(up=1):
    t0 = time.time()
    P = load_params()
    vege, vgt, _ = read(VEGE)
    dem, dgt, dnd = read(DEM)
    dem = np.where(dem <= -1e30, np.nan, dem)
    img = np.load(NPY).astype(np.int16)
    H, W = vege.shape
    print(f"vege {vege.shape} gt={vgt};  dem {dem.shape};  image {img.shape}")

    # cell centres of the vegetation raster, optionally supersampled
    off = (np.arange(up) + 0.5) / up
    cc = (np.arange(W)[:, None] + off[None, :]).ravel()
    rr = (np.arange(H)[:, None] + off[None, :]).ravel()
    X = vgt[0] + cc * vgt[1]
    Y = vgt[3] + rr * vgt[5]
    XX, YY = np.meshgrid(X, Y)
    xs = XX.ravel(); ys = YY.ravel()
    del XX, YY
    zs, zok = sample_dem(dem, dgt, xs, ys)
    print(f"{len(xs):,} sample points ({up}x supersample), DEM in range {zok.mean()*100:.2f}%")

    u, v, dist = project_render(xs, ys, zs, P)
    good = zok & np.isfinite(zs) & np.isfinite(u) & np.isfinite(v) & np.isfinite(dist)
    iu = np.round(u).astype(np.int64)
    iv = np.round(v).astype(np.int64)
    good &= (iu >= 0) & (iu < P["w"]) & (iv >= 0) & (iv < P["h"])
    print(f"projected inside the frame: {good.sum():,} ({100*good.mean():.2f}%)")

    # z-buffer: nearest surface point wins each pixel
    pix = iv.astype(np.int64) * P["w"] + iu
    order = np.argsort(np.where(good, dist, np.inf), kind="stable")
    winner = np.full(P["w"] * P["h"], -1, np.int64)
    p_sorted = pix[order]
    ok_sorted = good[order]
    # first occurrence of each pixel in distance order == nearest
    first = np.ones(len(p_sorted), bool)
    idx = np.argsort(p_sorted, kind="stable")
    ps = p_sorted[idx]
    first_sorted = np.empty(len(ps), bool)
    first_sorted[0] = True
    first_sorted[1:] = ps[1:] != ps[:-1]
    keep = np.zeros(len(p_sorted), bool)
    keep[idx[first_sorted]] = True
    # NB argsort(stable) preserves the distance order within equal pixel ids,
    # so the kept entry is the nearest one.
    visible = np.zeros(len(pix), bool)
    visible[order[keep & ok_sorted]] = True
    print(f"visible after z-buffer:     {visible.sum():,} "
          f"({100*visible.sum()/max(good.sum(),1):.2f}% of in-frame)")

    cls_map = vege.ravel() if up == 1 else np.repeat(np.repeat(vege, up, 0), up, 1).ravel()
    cls_img = img[np.clip(iv, 0, P["h"] - 1), np.clip(iu, 0, P["w"] - 1)] + 1

    for label, sel in (("all in-frame cells", good & np.isfinite(cls_map)),
                       ("visible cells only", visible & np.isfinite(cls_map))):
        a = cls_map[sel].astype(int)
        b = cls_img[sel].astype(int)
        n = sel.sum()
        print(f"\n=== {label}: n = {n:,} ===")
        print(f"    overall class agreement {100*(a == b).mean():.3f}%")
        print(f"    {'class':>14s} {'n(raster)':>10s} {'recall':>8s} {'precision':>10s}")
        for c in range(8):
            m = a == c
            if m.sum() == 0:
                continue
            rec = (b[m] == c).mean() * 100
            pm = b == c
            pre = (a[pm] == c).mean() * 100 if pm.sum() else float("nan")
            print(f"    {LAB[c]:>14s} {m.sum():>10,} {rec:>7.2f}% {pre:>9.2f}%")
        # where do the Sasa cells land in the image?
        m = a == 1
        if m.sum():
            vv = iv[sel][m]
            print(f"    Sasa cells occupy image rows v = {vv.min()}..{vv.max()} "
                  f"(median {int(np.median(vv))}); "
                  f"{100*np.mean((vv>=529)&(vv<=1268)):.2f}% fall inside the "
                  f"surviving georectified.csv band")
    print(f"\nelapsed {time.time()-t0:.1f} s")


if __name__ == "__main__":
    main(int(sys.argv[1]) if len(sys.argv) > 1 else 1)
