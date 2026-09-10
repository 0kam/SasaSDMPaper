"""
Export the image-space location of every Sasa gain / loss / stable map cell, using the
recovered camera model.  This is what the visual-interpretation deliverable needs and
what the truncated georectified.csv cannot supply (all Sasa cells fall outside the image
rows that survived truncation).

Output CSV columns
    x, y        map coordinate of the 1 m cell centre (EPSG:6690)
    z           DSM elevation
    u, v        pixel in the 5616 x 3744 reference frame (rounded)
    dist        distance from the camera, m
    gsd         approximate ground sample distance at that pixel, m/px
    visible     0/1 from a z-buffer over the same projection
    change      gain | loss | stable
"""
import numpy as np
from osgeo import gdal
from alproj_camera import load_params, project_render

gdal.UseExceptions()
D = "/Users/okamoto/NIES/SasaSDMPaper/data_from_server/ortho/data/"
OUT = "/Users/okamoto/NIES/SasaSDMPaper/.claude/worktrees/review-summary-ece-ddc69c/review/restore/georect/sasa_change_pixels.csv"
SASA = 1


def read(p):
    ds = gdal.Open(p); gt = ds.GetGeoTransform()
    a = ds.GetRasterBand(1).ReadAsArray(); del ds
    return a, gt


def main():
    P = load_params()
    v12, gt = read(D + "vege_2012_5x5.tiff")
    v21, _ = read(D + "vege_2021_5x5.tiff")
    dem, dgt = read(D + "mrd_dem_1m.tiff")
    dem = np.where(dem <= -1e30, np.nan, dem)
    H, W = v12.shape
    s12 = v12 == SASA
    s21 = v21 == SASA
    gain = s21 & ~s12 & np.isfinite(v12)
    loss = s12 & ~s21 & np.isfinite(v21)
    stable = s12 & s21
    print(f"Sasa 2012 {s12.sum():,}  2021 {s21.sum():,}  "
          f"gain {gain.sum():,}  loss {loss.sum():,}  stable {stable.sum():,}")

    rows, cols = np.mgrid[0:H, 0:W]
    xs = gt[0] + (cols + 0.5) * gt[1]
    ys = gt[3] + (rows + 0.5) * gt[5]
    dc = np.clip(((xs - dgt[0]) / dgt[1]).astype(int), 0, dem.shape[1] - 1)
    dr = np.clip(((ys - dgt[3]) / dgt[5]).astype(int), 0, dem.shape[0] - 1)
    zs = dem[dr, dc]
    u, v, dist = project_render(xs.ravel(), ys.ravel(), zs.ravel(), P)
    u = u.reshape(H, W); v = v.reshape(H, W); dist = dist.reshape(H, W)

    # z-buffer over every labelled cell so occluded cells can be flagged
    lab = np.isfinite(v12) & np.isfinite(dist) & np.isfinite(zs)
    iu = np.round(u); iv = np.round(v)
    inb = lab & (iu >= 0) & (iu < P["w"]) & (iv >= 0) & (iv < P["h"])
    flat = np.flatnonzero(inb)
    pix = (iv.ravel()[flat].astype(np.int64) * P["w"] + iu.ravel()[flat].astype(np.int64))
    dd = dist.ravel()[flat]
    order = np.argsort(dd, kind="stable")
    ps = pix[order]
    srt = np.argsort(ps, kind="stable")
    f = np.empty(len(ps), bool); f[0] = True; f[1:] = ps[srt][1:] != ps[srt][:-1]
    keep = np.zeros(len(ps), bool); keep[srt[f]] = True
    visible = np.zeros(H * W, bool)
    visible.ravel()[flat[order[keep]]] = True
    visible = visible.reshape(H, W)

    fx = P["w"] / (2 * np.tan(np.radians(P["fov"]) / 2))
    with open(OUT, "w") as fh:
        fh.write("x,y,z,u,v,dist,gsd,visible,change\n")
        for name, mask in (("gain", gain), ("loss", loss), ("stable", stable)):
            sel = mask & inb
            idx = np.flatnonzero(sel)
            for j in idx:
                r, c = divmod(j, W)
                fh.write(f"{xs[r,c]:.2f},{ys[r,c]:.2f},{zs[r,c]:.2f},"
                         f"{int(round(u[r,c]))},{int(round(v[r,c]))},{dist[r,c]:.1f},"
                         f"{dist[r,c]/fx:.3f},{int(visible[r,c])},{name}\n")
            vv = v[sel]
            print(f"{name:7s} n={sel.sum():6,}  image rows v {int(vv.min())}..{int(vv.max())}"
                  f"  median {int(np.median(vv))}  visible {100*visible[sel].mean():.1f}%"
                  f"  GSD median {np.median(dist[sel])/fx:.3f} m/px")
    print("wrote", OUT)


if __name__ == "__main__":
    main()
