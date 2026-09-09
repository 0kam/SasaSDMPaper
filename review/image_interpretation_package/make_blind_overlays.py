#!/usr/bin/env python3
"""Draw the target 1 m cell footprint onto the blinded native crops.

The interpreter reported that the unannotated crops give no indication of which
pixels are the cell under judgement.  This script re-cuts exactly the same
native crops as `sample_loss_crops.raw_crops` and overlays, on every one of the
four panels, the outline of the 1 m analysis cell projected through the same
camera model.

Blinding is preserved: the overlay encodes position only.  Every sample gets an
identical colour, line width and marker style, and nothing is drawn that varies
with the sample's class or transition.

Reads  : sample_key.csv (blind_id -> sample_id, cell row/col, image u/v)
         _cache/cam.npz (the fitted camera; refitted if absent)
         ortho/data/vege_2012_5x5.tiff, ortho/data/mrd_dem_1m.tiff
         data/images/aligned/{2012,2021}/*.png
Writes : blind_overlay/B###.png          (129 files)
         blind_overlay_footprints.csv    (footprint size in px, for QA)

Does NOT touch blind/, interpretation_sheet.csv or sample_key.csv, and never
re-runs make_interpretation_sheet.py: the blind_id assignment must stay fixed.

Usage: python3 make_blind_overlays.py
"""
import csv
import os
import sys

import numpy as np
from PIL import Image, ImageDraw
from osgeo import gdal

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import sample_loss_crops as S          # noqa: E402  (camera model + constants)

R_NATIVE = 150                          # must match raw_crops(..., r=150)
SIDE = 2 * R_NATIVE + 1                 # 301
GAP = 3                                 # white gutter between tiles
COLOUR = (255, 0, 255)                  # magenta, identical for every sample
HALO = (0, 0, 0)                        # keeps the ticks legible on pale sward
LINE_W = 1                              # footprint outline, px, no fill
TICK_GAP = 18                           # px from footprint centre to tick start
TICK_LEN = 24                           # px
TICK_W = 2                              # px


def cell_footprint(p, x, y, dem, dgt, n_edge=16):
    """Project the outline of the 1 m cell centred on (x, y) into image space.

    Each edge is densified so that lens distortion is followed rather than
    chorded, and each vertex takes its own DEM height: on a steep slope the two
    downhill corners sit well below the two uphill ones, and using a single
    height would draw a footprint that is systematically too small.
    """
    h = 0.5
    corners = [(x - h, y - h), (x + h, y - h), (x + h, y + h), (x - h, y + h)]
    xs, ys = [], []
    for i in range(4):
        x0, y0 = corners[i]
        x1, y1 = corners[(i + 1) % 4]
        for t in np.linspace(0, 1, n_edge, endpoint=False):
            xs.append(x0 + t * (x1 - x0))
            ys.append(y0 + t * (y1 - y0))
    xs = np.array(xs)
    ys = np.array(ys)
    ci = np.clip(((xs - dgt[0]) / dgt[1]).astype(int), 0, dem.shape[1] - 1)
    ri = np.clip(((ys - dgt[3]) / dgt[5]).astype(int), 0, dem.shape[0] - 1)
    zs = dem[ri, ci].astype(float)
    bad = ~np.isfinite(zs) | (zs < -1e30)
    if bad.any():
        good = zs[~bad]
        zs[bad] = good.mean() if good.size else 0.0
    u, v, zc, _ = S._project(p, xs, ys, zs)
    return np.asarray(u, float), np.asarray(v, float)


def main():
    outdir = os.path.join(HERE, "blind_overlay")
    os.makedirs(outdir, exist_ok=True)
    cache = os.path.join(HERE, "_cache")

    p, rmse, med = S.fit_camera(os.path.join(cache, "cam.npz"))
    print("camera: GCP reprojection rmse %.2f px, median %.2f px" % (rmse, med))

    dd = gdal.Open(os.path.join(S.REPO, "ortho/data/mrd_dem_1m.tiff"))
    dem = dd.ReadAsArray()
    dgt = np.array(dd.GetGeoTransform())

    # the four photographs, in the same order raw_crops uses
    eff = S.effective_mask(os.path.join(cache, "eff_mask.npy"))
    _, raw = S.load_photos(eff, exposure_match=True)   # `raw` is never adjusted

    key = list(csv.DictReader(open(os.path.join(HERE, "sample_key.csv"))))
    qa = []
    for k in key:
        uc, vc = float(k["u"]), float(k["v"])
        x, y = float(k["x"]), float(k["y"])
        pu, pv = cell_footprint(p, x, y, dem, dgt)

        # crop origin, exactly as sample_loss_crops.crop() computes it
        u0 = int(round(uc)) - R_NATIVE
        v0 = int(round(vc)) - R_NATIVE
        lx = pu - u0
        ly = pv - v0

        tiles = []
        for idx in (S.IDX_GREEN, S.IDX_AUTUMN):
            tiles.append([S.crop(raw[(yr, idx)][0], uc, vc, R_NATIVE)
                          for yr in ("2012", "2021")])
        out = np.full((2 * SIDE + GAP, 2 * SIDE + GAP, 3), 255, np.uint8)
        for i in range(2):
            for j in range(2):
                out[i * (SIDE + GAP):i * (SIDE + GAP) + SIDE,
                    j * (SIDE + GAP):j * (SIDE + GAP) + SIDE] = tiles[i][j]

        im = Image.fromarray(out)
        dr = ImageDraw.Draw(im)
        cx, cy = float(lx.mean()), float(ly.mean())
        # keep the ticks clear of the footprint itself.  This scales with
        # viewing distance only, which the sheet already discloses, so it
        # leaks nothing about the sample's class.
        gap = max(TICK_GAP,
                  float(np.hypot(lx - cx, ly - cy).max()) + 6.0)
        for i in range(2):
            for j in range(2):
                ox, oy = j * (SIDE + GAP), i * (SIDE + GAP)
                poly = [(ox + a, oy + b) for a, b in zip(lx, ly)]
                # four inward-pointing ticks, so the footprint can be found at
                # low zoom without any ink landing on the cell itself.  Drawn
                # first, dark halo under magenta, so they read on both the pale
                # sward and the dark pine cushions.
                for dx, dy in ((-1, 0), (1, 0), (0, -1), (0, 1)):
                    seg = [(ox + cx + dx * (gap + TICK_LEN),
                            oy + cy + dy * (gap + TICK_LEN)),
                           (ox + cx + dx * gap,
                            oy + cy + dy * gap)]
                    dr.line(seg, fill=HALO, width=TICK_W + 2)
                    dr.line(seg, fill=COLOUR, width=TICK_W)
                # the footprint itself: hairline, no halo, no fill, so that as
                # little of the cell as possible is covered
                dr.line(poly + [poly[0]], fill=COLOUR, width=LINE_W)

        im.save(os.path.join(outdir, k["blind_id"] + ".png"))
        qa.append({"blind_id": k["blind_id"],
                   "footprint_w_px": "%.1f" % (lx.max() - lx.min()),
                   "footprint_h_px": "%.1f" % (ly.max() - ly.min()),
                   "centre_x_in_tile": "%.1f" % cx,
                   "centre_y_in_tile": "%.1f" % cy})

    with open(os.path.join(HERE, "blind_overlay_footprints.csv"), "w",
              newline="") as fh:
        w = csv.DictWriter(fh, fieldnames=list(qa[0].keys()))
        w.writeheader()
        for r in qa:
            w.writerow(r)

    wpx = [float(r["footprint_w_px"]) for r in qa]
    hpx = [float(r["footprint_h_px"]) for r in qa]
    print("wrote %d overlays -> blind_overlay/" % len(qa))
    print("footprint width  px: min %.1f median %.1f max %.1f"
          % (min(wpx), float(np.median(wpx)), max(wpx)))
    print("footprint height px: min %.1f median %.1f max %.1f"
          % (min(hpx), float(np.median(hpx)), max(hpx)))


if __name__ == "__main__":
    main()
