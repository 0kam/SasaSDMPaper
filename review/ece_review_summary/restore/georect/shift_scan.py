"""
Is the recovered map->image transform biased?

Projects every 1 m cell of vege_2012_5x5.tiff into the photograph with the recovered
camera model, then scans a rigid (du, dv) offset applied to the resulting pixel
coordinates and reports where the class agreement against results/2012_masked.npy peaks.
A peak at (0, 0) means the recovered transform is unbiased at the pixel level.

Reported separately for all classes and for the Sasa cells alone, because the Sasa cells
sit in the near field where any residual misregistration would be largest.
"""
import numpy as np
from osgeo import gdal
from alproj_camera import load_params, project_render

gdal.UseExceptions()
D = "/Users/okamoto/NIES/SasaSDMPaper/data_from_server/ortho/data/"


def read(p):
    ds = gdal.Open(p); gt = ds.GetGeoTransform()
    a = ds.GetRasterBand(1).ReadAsArray(); del ds
    return a, gt


def main():
    P = load_params()
    vege, vgt = read(D + "vege_2012_5x5.tiff")
    dem, dgt = read(D + "mrd_dem_1m.tiff")
    dem = np.where(dem <= -1e30, np.nan, dem)
    img = np.load("/Users/okamoto/NIES/SasaSDMPaper/results/2012_masked.npy").astype(np.int16)
    H, W = vege.shape
    cx = vgt[0] + (np.arange(W) + 0.5) * vgt[1]
    cy = vgt[3] + (np.arange(H) + 0.5) * vgt[5]
    XX, YY = np.meshgrid(cx, cy)
    xs, ys = XX.ravel(), YY.ravel()
    col = np.clip(((xs - dgt[0]) / dgt[1]).astype(int), 0, dem.shape[1] - 1)
    row = np.clip(((ys - dgt[3]) / dgt[5]).astype(int), 0, dem.shape[0] - 1)
    zs = dem[row, col]
    u, v, _ = project_render(xs, ys, zs, P)
    cls = vege.ravel()
    keep = np.isfinite(cls) & np.isfinite(u) & np.isfinite(v) & np.isfinite(zs) & (cls > 0)
    u, v, cls = u[keep], v[keep], cls[keep].astype(int)
    sasa = cls == 1
    print(f"{len(cls):,} labelled cells, {sasa.sum():,} of them Sasa")

    best = None
    grid = range(-24, 25, 2)
    tab = {}
    for dv in grid:
        for du in grid:
            iu = np.round(u + du).astype(np.int64)
            iv = np.round(v + dv).astype(np.int64)
            ok = (iu >= 0) & (iu < P["w"]) & (iv >= 0) & (iv < P["h"])
            got = img[iv[ok], iu[ok]] + 1
            agr = (got == cls[ok]).mean()
            sa = sasa[ok]
            sag = (got[sa] == 1).mean() if sa.sum() else np.nan
            tab[(du, dv)] = (agr, sag)
            if best is None or agr > best[0]:
                best = (agr, du, dv, sag)
    print(f"peak overall agreement {100*best[0]:.3f}% at (du, dv) = ({best[1]}, {best[2]}); "
          f"Sasa recall there {100*best[3]:.2f}%")
    a0, s0 = tab[(0, 0)]
    print(f"at (0, 0): overall {100*a0:.3f}%, Sasa recall {100*s0:.2f}%")
    bs = max(tab, key=lambda k: (tab[k][1] if np.isfinite(tab[k][1]) else -1))
    print(f"peak Sasa recall {100*tab[bs][1]:.2f}% at (du, dv) = {bs} "
          f"(overall there {100*tab[bs][0]:.3f}%)")
    print("\noverall agreement (%) on the (du, dv) grid, rows = dv, cols = du")
    print("      " + "".join(f"{du:>7d}" for du in grid))
    for dv in grid:
        print(f"{dv:>5d} " + "".join(f"{100*tab[(du,dv)][0]:>7.2f}" for du in grid))


if __name__ == "__main__":
    main()
