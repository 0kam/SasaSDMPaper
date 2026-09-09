#!/usr/bin/env python3
"""
sample_loss_crops.py -- sample Sasa-loss locations and cut side-by-side
2012 / 2021 photograph crops for visual interpretation.

Motivation (Ecology & Evolution major revision, Reviewer 1)
-----------------------------------------------------------
Reviewer 1 asked whether Sasa persisted beneath shrub canopies and suggested
"field observations or additional image interpretation".  The 2012->2021
transition matrix says 2,468 m2 of 2012 Sasa became something else, 46.8 % of
it dwarf pine (Pinus pumila, class 7) and 37.8 % "other vegetation" (class 2).
This script produces the raw material for a human to judge whether those
transitions are (a) real overgrowth, (b) mixed-pixel reclassification, or
(c) classifier noise.

How it works
------------
1.  Samples 1 m cells from the GEORECTIFIED class rasters
    ortho/data/vege_{2012,2021}_5x5.tiff -- i.e. exactly the cells that make up
    the transition matrix reported in the paper.
2.  Projects each sampled cell into CAMERA IMAGE SPACE with a pinhole + rational
    lens-distortion camera model refitted here from ortho/data/gcp.csv.
    (The original image->ground code and ortho/data/georectified.csv are NOT in
    the repository; only the GCPs and ortho/data/params_optim.json survive, and
    the parameter convention in that JSON could not be reproduced, so the model
    is refitted from the 482 GCPs.  Median GCP reprojection error ~2.3 px.)
3.  Cuts crops at native resolution from data/images/aligned/{2012,2021}/*.png,
    which are all warped into one common image space (all photographs of both
    years were aligned to a single 2015 reference frame), so the same (u, v)
    refers to the same ground point in both years.
4.  Writes one multi-panel figure per sample (two zoom levels, two phenological
    dates per year, plus the classifier's own labels), an index CSV, and one
    contact-sheet montage per stratum.

Usage
-----
    <venv>/bin/python sample_loss_crops.py --outdir OUTDIR [--seed 20260807]

The interpreter needs numpy, scipy, Pillow, matplotlib AND osgeo (GDAL).
On this machine:
    python3 -m venv --system-site-packages venv     # system python has osgeo
    venv/bin/pip install numpy pillow matplotlib scipy

Everything is read-only with respect to the repository; only OUTDIR is written.
"""

import argparse
import csv
import json
import os
import sys

import numpy as np
from scipy.optimize import least_squares
from scipy import ndimage

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.patches import Rectangle
from matplotlib.colors import ListedColormap, BoundaryNorm

from PIL import Image
Image.MAX_IMAGE_PIXELS = None

from osgeo import gdal
gdal.UseExceptions()

# --------------------------------------------------------------------------- #
# Paths and constants
# --------------------------------------------------------------------------- #

REPO = "/Users/okamoto/NIES/SasaSDMPaper"
W, H = 5616, 3744                     # Canon EOS 5D Mark II frame, = npy shape

# Class codes.  scripts/vegetation_classification/run_rnn.py and
# calculate_diff.py use 0-based codes; the GeoTIFFs are +1.  Everything below
# uses the GeoTIFF (1-based) convention.
CLASS_NAME = {
    1: "Sasa",
    2: "Other vegetation",
    3: "No vegetation",
    4: "Rowan (Sorbus)",
    5: "Maple (Acer)",
    6: "Alder (Alnus)",
    7: "Dwarf pine (Pinus pumila)",
}
CLASS_SHORT = {1: "sasa", 2: "otherveg", 3: "noveg", 4: "rowan",
               5: "maple", 6: "alder", 7: "dwarfpine"}
# Colours as in run_rnn.py (index 0..6 there == class 1..7 here)
CLASS_RGB = np.array([[0, 0, 0],
                      [154, 205, 50], [70, 130, 180], [192, 192, 192],
                      [220, 20, 60], [255, 215, 0], [139, 69, 19],
                      [0, 100, 0]], dtype=np.uint8)

# Aligned photographs, sorted by acquisition date (EXIF DateTimeOriginal).
# Alphabetical order == chronological order in both years, and the two series
# pair up almost date-for-date.
PHOTOS = {
    "2012": [("IMG_8748", "2012-08-27"), ("IMG_8819", "2012-09-01"),
             ("IMG_8956", "2012-09-11"), ("IMG_9038", "2012-09-17"),
             ("IMG_9164", "2012-09-26"), ("IMG_9304", "2012-10-06"),
             ("IMG_9514", "2012-10-21")],
    "2021": [("IMG_7763", "2021-08-24"), ("IMG_7827", "2021-08-31"),
             ("IMG_7900", "2021-09-07"), ("IMG_8023", "2021-09-19"),
             ("IMG_8081", "2021-09-24"), ("IMG_8172", "2021-10-02"),
             ("IMG_8298", "2021-10-14")],
}
# The 2021 series is phenologically ~1-2 weeks AHEAD of the 2012 series: green
# chromatic coordinate g/(r+g+b) over all vegetated pixels falls from 0.384 to
# 0.334 during 2012 and from 0.392 to 0.338 during 2021, but reaches any given
# value about ten days earlier in 2021.  The pairs below are matched on that
# statistic rather than on calendar date, so that the two members of a pair show
# the canopy at the same phenological stage:
#   green  : 2012-09-17 (GCC 0.3780)  vs 2021-09-07 (GCC 0.3791)
#   autumn : 2012-10-06 (GCC 0.3422)  vs 2021-10-02 (GCC 0.3424)
PAIRS = [("green season", 3, 2), ("autumn colour", 5, 5)]  # label, i2012, i2021
IDX_GREEN = 0     # index into PAIRS
IDX_AUTUMN = 1

R_CONTEXT = 250   # half-width of the wide panel, px
R_DETAIL = 60     # half-width of the detail panel, px


# --------------------------------------------------------------------------- #
# Camera model
# --------------------------------------------------------------------------- #

def _project(p, X, Y, Z):
    """World (JGD2011 / UTM 53N easting, northing, ellipsoidal-ish height)
    -> image (u, v).  Returns u, v, z_camera (depth), range."""
    cx, cy, fov, pan, tilt, roll = p[0:6]
    camx, camy, camz = p[6:9]
    d = np.stack([X - camx, Y - camy, Z - camz], axis=-1)
    pa, ti, ro = np.radians(pan), np.radians(tilt), np.radians(roll)
    fwd = np.array([np.sin(pa) * np.cos(ti), np.cos(pa) * np.cos(ti), np.sin(ti)])
    rgt = np.array([np.cos(pa), -np.sin(pa), 0.0])
    up = np.cross(rgt, fwd)
    cr, sr = np.cos(ro), np.sin(ro)
    R = cr * rgt + sr * up
    U = -sr * rgt + cr * up
    zc = d @ fwd
    x1 = (d @ R) / zc
    y1 = -(d @ U) / zc
    k1, k2, k3, k4, k5, k6, p1, p2, s1, s2, s3, s4 = p[9:21]
    r2 = x1 ** 2 + y1 ** 2
    r4, r6 = r2 ** 2, r2 ** 3
    rad = (1 + k1 * r2 + k2 * r4 + k3 * r6) / (1 + k4 * r2 + k5 * r4 + k6 * r6)
    xd = x1 * rad + 2 * p1 * x1 * y1 + p2 * (r2 + 2 * x1 ** 2) + s1 * r2 + s2 * r4
    yd = y1 * rad + 2 * p2 * x1 * y1 + p1 * (r2 + 2 * y1 ** 2) + s3 * r2 + s4 * r4
    f = (W / 2) / np.tan(np.radians(fov) / 2)
    return f * xd + cx, f * yd + cy, zc, np.linalg.norm(d, axis=-1)


def fit_camera(cache):
    """Refit the camera from ortho/data/gcp.csv.  Cached to `cache`."""
    if os.path.exists(cache):
        d = np.load(cache)
        return d["p"], float(d["rmse"]), float(d["median"])
    rows = list(csv.DictReader(open(os.path.join(REPO, "ortho/data/gcp.csv"))))
    u = np.array([float(r["u"]) for r in rows])
    v = np.array([float(r["v"]) for r in rows])
    X = np.array([float(r["x"]) for r in rows])
    Y = np.array([float(r["y"]) for r in rows])
    Z = np.array([float(r["z"]) for r in rows])
    prm = json.load(open(os.path.join(REPO, "ortho/data/params_optim.json")))
    q0 = np.array([W / 2, H / 2, prm["fov"], prm["pan"], 1.5, 6.6,
                   prm["x"], prm["y"], prm["z"]])

    def res_nd(q):
        p = np.concatenate([q, np.zeros(12)])
        uu, vv, _, _ = _project(p, X, Y, Z)
        return np.concatenate([uu - u, vv - v])

    r1 = least_squares(res_nd, q0, x_scale=[100, 100, 1, 1, 1, 1, 10, 10, 10])

    def res(p):
        uu, vv, _, _ = _project(p, X, Y, Z)
        return np.concatenate([uu - u, vv - v])

    r2 = least_squares(res, np.concatenate([r1.x, np.zeros(12)]),
                       x_scale=[100, 100, 1, 1, 1, 1, 10, 10, 10] + [0.1] * 12)
    e = r2.fun.reshape(2, -1)
    err = np.hypot(e[0], e[1])
    rmse, med = float(np.sqrt((err ** 2).mean())), float(np.median(err))
    np.savez(cache, p=r2.x, rmse=rmse, median=med)
    return r2.x, rmse, med


# --------------------------------------------------------------------------- #
# Rasters
# --------------------------------------------------------------------------- #

def load_grids(cache_dir):
    """Return V12, V21 (1 m class rasters, 1-based codes, NaN outside),
    the geotransform, and the per-cell projected image coordinates."""
    dv = gdal.Open(os.path.join(REPO, "ortho/data/vege_2012_5x5.tiff"))
    V12 = dv.ReadAsArray()
    gt = np.array(dv.GetGeoTransform())
    V21 = gdal.Open(os.path.join(REPO, "ortho/data/vege_2021_5x5.tiff")).ReadAsArray()
    dd = gdal.Open(os.path.join(REPO, "ortho/data/mrd_dem_1m.tiff"))
    DEM = dd.ReadAsArray()
    dgt = np.array(dd.GetGeoTransform())
    ny, nx = V12.shape
    Xg = gt[0] + (np.arange(nx) + 0.5) * gt[1]
    Yg = gt[3] + (np.arange(ny) + 0.5) * gt[5]
    XX, YY = np.meshgrid(Xg, Yg)
    ci = ((XX - dgt[0]) / dgt[1]).astype(int)
    ri = ((YY - dgt[3]) / dgt[5]).astype(int)
    ok = (ci >= 0) & (ci < DEM.shape[1]) & (ri >= 0) & (ri < DEM.shape[0])
    ZZ = np.full(XX.shape, np.nan)
    ZZ[ok] = DEM[ri[ok], ci[ok]]
    ZZ[ZZ < -1e30] = np.nan
    return V12, V21, gt, XX, YY, ZZ, DEM, dgt


def ground_area_per_pixel(p, V12, gt, DEM, dgt, cache, sub=4):
    """Empirical m^2 of ground subtended by each image pixel, obtained by
    projecting the 1 m analysis grid at `sub` x `sub` sub-cell resolution."""
    if os.path.exists(cache):
        return np.load(cache)
    ny, nx = V12.shape
    area = np.zeros((H, W), np.float32)
    off = (np.arange(sub) + 0.5) / sub
    for oy in off:
        ys = gt[3] + (np.arange(ny) + oy) * gt[5]
        for ox in off:
            xs = gt[0] + (np.arange(nx) + ox) * gt[1]
            XX, YY = np.meshgrid(xs, ys)
            ci = ((XX - dgt[0]) / dgt[1]).astype(int)
            ri = ((YY - dgt[3]) / dgt[5]).astype(int)
            ok = (ci >= 0) & (ci < DEM.shape[1]) & (ri >= 0) & (ri < DEM.shape[0])
            ZZ = np.full(XX.shape, np.nan, np.float32)
            ZZ[ok] = DEM[ri[ok], ci[ok]]
            ZZ[ZZ < -1e30] = np.nan
            good = np.isfinite(ZZ) & np.isfinite(V12)
            uu, vv, zc, _ = _project(p, XX[good], YY[good], ZZ[good])
            m = (zc > 0) & np.isfinite(uu) & (uu >= 0) & (uu < W) \
                & (vv >= 0) & (vv < H)
            np.add.at(area, (vv[m].astype(int), uu[m].astype(int)),
                      (1.0 / sub) ** 2)
    np.save(cache, area)
    return area


def effective_mask(cache):
    """Reproduce the mask applied in scripts/vegetation_classification/apply_mask.py:
    the 2015 sky mask AND the non-black part of aligned/2012/IMG_8748.png."""
    if os.path.exists(cache):
        return np.load(cache)
    sky = np.asarray(Image.open(os.path.join(
        REPO, "data/images/mrd_085_eos_vis_20151010_1205_masked.png")).convert("RGB"))
    ref = np.asarray(Image.open(os.path.join(
        REPO, "data/images/aligned/2012/IMG_8748.png")).convert("RGB"))
    eff = (sky[:, :, 0] != 0) & (ref[:, :, 0] != 0)
    np.save(cache, eff)
    return eff


def image_space_classes(eff, cache):
    """results/{2012,2021}_masked.npy converted to the 1-based GeoTIFF codes,
    with 0 = masked.  NOTE apply_mask.py collapsed masked pixels to 0, which in
    the 0-based coding is also the Sasa class; `eff` disambiguates them."""
    if os.path.exists(cache):
        d = np.load(cache)
        return d["c12"], d["c21"]
    c12 = np.where(eff, np.load(os.path.join(REPO, "results/2012_masked.npy")
                                ).astype(np.int16) + 1, 0)
    c21 = np.where(eff, np.load(os.path.join(REPO, "results/2021_masked.npy")
                                ).astype(np.int16) + 1, 0)
    np.savez(cache, c12=c12, c21=c21)
    return c12, c21


# --------------------------------------------------------------------------- #
# Sampling
# --------------------------------------------------------------------------- #

def sample_stratum(mask, XX, YY, ZZ, p, eff, n, rng, min_sep_m=6.0,
                   margin=R_CONTEXT + 5, max_m_per_px=None):
    """Pick up to n cells satisfying `mask`, at least `min_sep_m` apart on the
    ground, whose projection falls safely inside the unmasked image.

    `max_m_per_px` caps the across-view ground scale (equivalently the viewing
    distance).  Without it the stable-dwarf-pine control pool, which lies high
    on the ridges, is drawn at roughly twice the distance of the loss cells;
    the controls then look categorically blurrier than the cells under test,
    which both weakens them as calibration and lets a blinded interpreter
    recognise them on image quality alone."""
    ridx, cidx = np.nonzero(mask & np.isfinite(ZZ))
    if len(ridx) == 0:
        return []
    order = rng.permutation(len(ridx))
    picked, pts = [], []
    for j in order:
        r, c = ridx[j], cidx[j]
        x, y, z = XX[r, c], YY[r, c], ZZ[r, c]
        if pts:
            a = np.array(pts)
            if np.min((a[:, 0] - x) ** 2 + (a[:, 1] - y) ** 2) < min_sep_m ** 2:
                continue
        u, v, zc, dist = _project(p, np.array([x]), np.array([y]), np.array([z]))
        u, v, zc, dist = float(u[0]), float(v[0]), float(zc[0]), float(dist[0])
        if zc <= 0 or not (margin <= u < W - margin and margin <= v < H - margin):
            continue
        win = eff[int(v) - R_DETAIL:int(v) + R_DETAIL + 1,
                  int(u) - R_DETAIL:int(u) + R_DETAIL + 1]
        if win.mean() < 0.999:
            continue
        if max_m_per_px is not None:
            f_px = (W / 2) / np.tan(np.radians(p[2]) / 2)
            if dist / f_px > max_m_per_px:
                continue
        picked.append(dict(row=int(r), col=int(c), x=x, y=y, z=float(z),
                           u=u, v=v, dist=dist))
        pts.append((x, y))
        if len(picked) >= n:
            break
    return picked


# --------------------------------------------------------------------------- #
# Rendering
# --------------------------------------------------------------------------- #

def load_photos(eff, exposure_match=True):
    """Load the four photographs used in the figures.

    The 2021 frames are systematically darker than the 2012 frames (mean
    brightness over vegetated pixels 109 vs 119 for the green pair) because the
    camera was stopped down from f/11 to f/13 between the two campaigns.  With
    `exposure_match` the 2021 frames are multiplied by a single per-channel gain
    that equalises those means, so that colour can be compared by eye.  The
    files written to crops_native/ are NEVER adjusted."""
    raw, out = {}, {}
    for pi, (label, i12, i21) in enumerate(PAIRS):
        for year, i in (("2012", i12), ("2021", i21)):
            name, date = PHOTOS[year][i]
            path = os.path.join(REPO, "data/images/aligned", year, name + ".png")
            sys.stderr.write("  loading %s (%s)\n" % (path, date))
            raw[(year, pi)] = (np.asarray(Image.open(path).convert("RGB")),
                               date, name)
    for pi in range(len(PAIRS)):
        a12 = raw[("2012", pi)][0]
        a21, date21, name21 = raw[("2021", pi)]
        out[("2012", pi)] = raw[("2012", pi)] + (np.ones(3),)
        if exposure_match:
            g = a12[eff].mean(0) / np.maximum(a21[eff].mean(0), 1e-6)
            adj = np.clip(a21.astype(np.float32) * g, 0, 255).astype(np.uint8)
            out[("2021", pi)] = (adj, date21, name21, g)
        else:
            out[("2021", pi)] = (a21, date21, name21, np.ones(3))
    return out, raw


def crop(arr, u, v, r):
    u, v = int(round(u)), int(round(v))
    return arr[v - r:v + r + 1, u - r:u + r + 1]


def patch_stats(arr, u, v, r=7):
    """Green chromatic coordinate and normalised texture contrast in a small
    window.  Sasa sward is smooth and yellow-green; dwarf pine is dark,
    blue-green and much rougher at this scale."""
    w = crop(arr, u, v, r).astype(np.float32)
    tot = w.sum(2) + 1e-6
    gcc = float((w[:, :, 1] / tot).mean())
    lum = w.mean(2)
    return gcc, float(lum.std() / max(lum.mean(), 1e-6))


def phase_shift(a, b):
    """Residual 2012<->2021 displacement of a crop pair, px (QC metric)."""
    a = a.astype(np.float32).mean(2)
    b = b.astype(np.float32).mean(2)
    a, b = a - a.mean(), b - b.mean()
    win = np.outer(np.hanning(a.shape[0]), np.hanning(a.shape[1]))
    Fa, Fb = np.fft.rfft2(a * win), np.fft.rfft2(b * win)
    R = Fa * np.conj(Fb)
    R /= np.abs(R) + 1e-9
    r = np.fft.irfft2(R, s=a.shape)
    dy, dx = np.unravel_index(np.argmax(r), r.shape)
    if dy > a.shape[0] // 2:
        dy -= a.shape[0]
    if dx > a.shape[1] // 2:
        dx -= a.shape[1]
    return float(dx), float(dy)


def scalebar(ax, mpp, r, label_extra=""):
    """Horizontal scale bar.  `mpp` = ground metres per image pixel measured
    ACROSS the line of sight; the vertical direction is far coarser."""
    span = 2 * r + 1
    for target in (5, 10, 20, 50, 100, 200):
        length_px = target / mpp
        if length_px > 0.18 * span:
            break
    x0 = 0.06 * span
    y0 = span * 0.94
    ax.plot([x0, x0 + length_px], [y0, y0], color="white", lw=3,
            solid_capstyle="butt", zorder=5)
    ax.plot([x0, x0 + length_px], [y0, y0], color="black", lw=1.2,
            solid_capstyle="butt", zorder=6)
    ax.text(x0, y0 - span * 0.02, "%d m (horizontal)%s" % (target, label_extra),
            color="white", fontsize=7, va="bottom",
            path_effects=None, zorder=7,
            bbox=dict(fc="black", ec="none", alpha=0.45, pad=1))


def class_panel(ax, cmap_arr, r):
    ax.imshow(CLASS_RGB[np.clip(cmap_arr, 0, 7)], interpolation="nearest")
    ax.set_xticks([]), ax.set_yticks([])


def georef_overlay(ax, photo_crop, u, v, r, PU, PV, VAL, mpp, target_rc=None):
    """Photo detail with the PAPER'S OWN georectified 1 m class labels drawn on
    top, each 1 m cell as a marker at its projected image position.  Markers are
    deliberately smaller than the true cell footprint so the photograph stays
    visible underneath."""
    ax.imshow(photo_crop, interpolation="nearest")
    sel = (PU >= u - r) & (PU <= u + r) & (PV >= v - r) & (PV <= v + r) \
        & np.isfinite(VAL)
    if sel.any():
        px = PU[sel] - (u - r)
        py = PV[sel] - (v - r)
        cl = VAL[sel].astype(int)
        good = (cl >= 1) & (cl <= 7)
        pts_per_imgpx = (4.4 * 72.0) / (2 * r + 1)   # axis is ~4.4 in wide
        cell_pt = pts_per_imgpx / max(mpp, 1e-6)     # 1 m cell width in points
        size = float(np.clip((0.45 * cell_pt) ** 2, 3.0, 160.0))
        ax.scatter(px[good], py[good], s=size,
                   c=CLASS_RGB[cl[good]] / 255.0, marker="s",
                   linewidths=0.0, alpha=0.9, zorder=3)
    if target_rc is not None:
        tu = PU[target_rc] - (u - r)
        tv = PV[target_rc] - (v - r)
        tc = VAL[target_rc]
        s0 = size if sel.any() else 40.0
        ax.scatter([tu], [tv], s=s0, marker="s", zorder=5, linewidths=0,
                   c=[CLASS_RGB[int(tc)] / 255.0] if 1 <= tc <= 7 else "none")
        ax.scatter([tu], [tv], s=s0 * 3.0, marker="s", zorder=6,
                   facecolors="none", edgecolors="red", linewidths=1.6)
    ax.set_xlim(0, 2 * r)
    ax.set_ylim(2 * r, 0)
    ax.set_xticks([]), ax.set_yticks([])


def make_figure(s, photos, c12, c21, out_png, PU, PV, V12, V21):
    u, v = s["u"], s["v"]
    fig, axes = plt.subplots(5, 2, figsize=(9.6, 23.5))
    fig.subplots_adjust(left=0.02, right=0.98, top=0.955, bottom=0.055,
                        wspace=0.03, hspace=0.09)

    title = ("%s   %s -> %s\n"
             "UTM53N %.1f E  %.1f N   %.0f m a.s.l.   %.0f m from camera\n"
             "image (u,v) = (%.0f, %.0f)   ground scale %.2f m/px across-view, "
             "%.2f m2/px total   2012-2021 residual shift %.0f px"
             % (s["sample_id"], CLASS_NAME[s["cls2012"]], CLASS_NAME[s["cls2021"]],
                s["x"], s["y"], s["z"], s["dist"], u, v,
                s["m_per_px_horiz"], s["m2_per_px"], s["align_shift_px"]))
    fig.suptitle(title, fontsize=9, y=0.985)

    ctx = {}
    for j, year in enumerate(("2012", "2021")):
        arr, date, name = photos[(year, IDX_GREEN)][:3]
        ctx[year] = crop(arr, u, v, R_CONTEXT)
        ax = axes[0, j]
        ax.imshow(ctx[year], interpolation="nearest")
        ax.add_patch(Rectangle((R_CONTEXT - R_DETAIL, R_CONTEXT - R_DETAIL),
                               2 * R_DETAIL, 2 * R_DETAIL, fill=False,
                               ec="yellow", lw=1.2))
        ax.plot([R_CONTEXT], [R_CONTEXT], marker="+", ms=14, mew=1.4, color="red")
        ax.set_title("%s  %s  (context %d px)%s"
                     % (year, date, 2 * R_CONTEXT + 1,
                        "  [exposure-matched]" if year == "2021"
                        and not np.allclose(photos[(year, IDX_GREEN)][3], 1) else ""),
                     fontsize=9)
        ax.set_xticks([]), ax.set_yticks([])
        scalebar(ax, s["m_per_px_horiz"], R_CONTEXT)

    for row, idx in ((1, IDX_GREEN), (2, IDX_AUTUMN)):
        tag = PAIRS[idx][0]
        for j, year in enumerate(("2012", "2021")):
            arr, date, name = photos[(year, idx)][:3]
            ax = axes[row, j]
            ax.imshow(crop(arr, u, v, R_DETAIL), interpolation="nearest")
            ax.plot([R_DETAIL], [R_DETAIL], marker="+", ms=16, mew=1.2, color="red")
            ax.set_title("%s  %s  (%s, detail %d px)%s"
                         % (year, date, tag, 2 * R_DETAIL + 1,
                            "  [exp-matched]" if year == "2021"
                            and not np.allclose(photos[(year, idx)][3], 1) else ""),
                         fontsize=9)
            ax.set_xticks([]), ax.set_yticks([])
            scalebar(ax, s["m_per_px_horiz"], R_DETAIL)

    for j, (year, VV) in enumerate((("2012", V12), ("2021", V21))):
        ax = axes[3, j]
        arr = photos[(year, IDX_GREEN)][0]
        georef_overlay(ax, crop(arr, u, v, R_DETAIL), u, v, R_DETAIL,
                       PU, PV, VV, s["m_per_px_horiz"],
                       target_rc=(s["row"], s["col"]))
        ax.plot([R_DETAIL], [R_DETAIL], marker="+", ms=16, mew=1.4, color="red")
        ax.set_title("%s georectified 1 m labels (the paper's map)" % year,
                     fontsize=9)

    for j, (year, cc) in enumerate((("2012", c12), ("2021", c21))):
        ax = axes[4, j]
        class_panel(ax, crop(cc, u, v, R_DETAIL), R_DETAIL)
        ax.plot([R_DETAIL], [R_DETAIL], marker="+", ms=16, mew=1.2, color="red")
        ax.set_title("%s image-space labels (1x1 model, results/*_masked.npy)"
                     % year, fontsize=9)

    handles = [plt.Line2D([], [], marker="s", ls="", ms=8,
                          mfc=CLASS_RGB[k] / 255.0, mec="k",
                          label="%d %s" % (k, CLASS_NAME[k])) for k in range(1, 8)]
    fig.legend(handles=handles, loc="lower center", ncol=4, fontsize=8,
               frameon=False, bbox_to_anchor=(0.5, 0.004))

    fig.savefig(out_png, dpi=110)
    plt.close(fig)


def raw_crops(s, raw, out_png, r=150):
    """Native-resolution, unrendered 2 x 2 mosaic (rows = date, cols = year).
    No resampling of any kind is applied, so the user can zoom freely."""
    tiles = []
    for idx in (IDX_GREEN, IDX_AUTUMN):
        tiles.append([crop(raw[(y, idx)][0], s["u"], s["v"], r)
                      for y in ("2012", "2021")])
    side = 2 * r + 1
    out = np.zeros((2 * side + 3, 2 * side + 3, 3), np.uint8)
    out[:, :, :] = 255
    for i in range(2):
        for j in range(2):
            out[i * (side + 3):i * (side + 3) + side,
                j * (side + 3):j * (side + 3) + side] = tiles[i][j]
    out[side:side + 3, :] = 255
    out[:, side:side + 3] = 255
    Image.fromarray(out).save(out_png)


def contact_sheet(samples, photos, out_png, cols=6, thumb=R_DETAIL * 2 + 1):
    """One tile per sample: 2012 detail above 2021 detail."""
    if not samples:
        return
    n = len(samples)
    rows = int(np.ceil(n / cols))
    pad, hdr = 6, 16
    tw, th = thumb, thumb * 2 + pad + hdr
    sheet = Image.new("RGB", (cols * (tw + pad) + pad, rows * (th + pad) + pad),
                      (20, 20, 20))
    from PIL import ImageDraw
    dr = ImageDraw.Draw(sheet)
    a12 = photos[("2012", IDX_GREEN)][0]
    a21 = photos[("2021", IDX_GREEN)][0]
    for i, s in enumerate(samples):
        r, c = divmod(i, cols)
        x0 = pad + c * (tw + pad)
        y0 = pad + r * (th + pad)
        dr.text((x0 + 2, y0 + 2), s["sample_id"], fill=(255, 255, 255))
        top = crop(a12, s["u"], s["v"], R_DETAIL)
        bot = crop(a21, s["u"], s["v"], R_DETAIL)
        sheet.paste(Image.fromarray(top), (x0, y0 + hdr))
        sheet.paste(Image.fromarray(bot), (x0, y0 + hdr + thumb + pad))
        for yy in (y0 + hdr + thumb // 2, y0 + hdr + thumb + pad + thumb // 2):
            dr.line([(x0 + thumb // 2 - 5, yy), (x0 + thumb // 2 + 5, yy)],
                    fill=(255, 40, 40))
            dr.line([(x0 + thumb // 2, yy - 5), (x0 + thumb // 2, yy + 5)],
                    fill=(255, 40, 40))
    sheet.save(out_png)


def overview(photo, PU, PV, V12, V21, samples, out_png, shrink=3):
    """Whole-frame diagnostic: where the paper's Sasa change actually is, and
    where the samples were drawn from."""
    img = photo.copy()
    layers = [((V12 == 1) & (V21 == 1), (60, 255, 60)),      # stable Sasa
              ((V12 != 1) & (V21 == 1) & np.isfinite(V12), (255, 230, 0)),  # gain
              ((V12 == 1) & (V21 != 1) & np.isfinite(V21) & (V21 > 0),
               (255, 40, 40))]                                # loss
    for m, col in layers:
        uu = np.round(PU[m]).astype(int)
        vv = np.round(PV[m]).astype(int)
        ok = (uu >= 0) & (uu < W) & (vv >= 0) & (vv < H)
        for du in (-1, 0, 1):
            for dv in (-1, 0, 1):
                img[np.clip(vv[ok] + dv, 0, H - 1),
                    np.clip(uu[ok] + du, 0, W - 1)] = col
    im = Image.fromarray(img).resize((W // shrink, H // shrink), Image.LANCZOS)
    from PIL import ImageDraw
    dr = ImageDraw.Draw(im)
    for s in samples:
        x, y = s["u"] / shrink, s["v"] / shrink
        dr.ellipse([x - 9, y - 9, x + 9, y + 9], outline=(0, 200, 255), width=2)
    dr.text((8, 8), "green = Sasa in both years | yellow = gain to Sasa | "
                    "red = Sasa lost | cyan circles = sampled locations",
            fill=(255, 255, 255))
    im.save(out_png)


# --------------------------------------------------------------------------- #
# Main
# --------------------------------------------------------------------------- #

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--outdir", required=True)
    ap.add_argument("--seed", type=int, default=20260807)
    ap.add_argument("--n-loss", type=int, default=50,
                    help="total loss samples across destination classes")
    ap.add_argument("--n-control", type=int, default=15,
                    help="samples per control stratum")
    ap.add_argument("--min-per-dest", type=int, default=3,
                    help="floor on samples per loss destination class")
    ap.add_argument("--max-m-per-px", type=float, default=0.25,
                    help="cap on across-view ground scale (m/px) of sampled "
                         "cells; keeps controls at the same viewing quality "
                         "as the loss cells. Use 0 to disable.")
    ap.add_argument("--min-pool", type=int, default=20,
                    help="skip destination classes with fewer cells than this")
    ap.add_argument("--no-exposure-match", action="store_true",
                    help="do not equalise 2021 exposure to 2012 in the figures")
    ap.add_argument("--cache", default=None)
    args = ap.parse_args()
    if args.max_m_per_px is not None and args.max_m_per_px <= 0:
        args.max_m_per_px = None

    outdir = os.path.abspath(args.outdir)
    cropdir = os.path.join(outdir, "crops")
    rawdir = os.path.join(outdir, "crops_native")
    os.makedirs(cropdir, exist_ok=True)
    os.makedirs(rawdir, exist_ok=True)
    cache = args.cache or os.path.join(outdir, "_cache")
    os.makedirs(cache, exist_ok=True)
    rng = np.random.default_rng(args.seed)

    print("Fitting camera model from ortho/data/gcp.csv ...")
    p, rmse, med = fit_camera(os.path.join(cache, "cam.npz"))
    print("  GCP reprojection: rmse %.2f px, median %.2f px (n=482)" % (rmse, med))

    print("Loading class rasters and DEM ...")
    V12, V21, gt, XX, YY, ZZ, DEM, dgt = load_grids(cache)
    eff = effective_mask(os.path.join(cache, "eff_mask.npy"))
    c12, c21 = image_space_classes(eff, os.path.join(cache, "imgcls.npz"))
    area = ground_area_per_pixel(p, V12, gt, DEM, dgt,
                                 os.path.join(cache, "area_per_px.npy"))

    # projection of every 1 m analysis cell into image space, for the overlay
    projcache = os.path.join(cache, "cellproj.npz")
    if os.path.exists(projcache):
        d = np.load(projcache)
        PU, PV = d["PU"], d["PV"]
    else:
        PU, PV, zc, _ = _project(p, XX, YY, ZZ)
        bad = ~(np.isfinite(PU) & (zc > 0))
        PU = np.where(bad, -1e6, PU)
        PV = np.where(bad, -1e6, PV)
        np.savez(projcache, PU=PU, PV=PV)

    # ---- strata ---------------------------------------------------------- #
    loss = (V12 == 1) & (V21 != 1) & np.isfinite(V21) & (V21 > 0)
    dest_counts = {k: int(((V12 == 1) & (V21 == k)).sum()) for k in range(2, 8)}
    tot = sum(dest_counts.values())
    print("  loss cells (m2) by 2021 destination:",
          {CLASS_SHORT[k]: v for k, v in dest_counts.items()}, "total", tot)

    strata = []
    # Proportional allocation over every destination class with a non-trivial
    # pool, so that the deciduous-shrub group (rowan + maple + alder, ~15 % of
    # the loss area) is represented in proportion instead of by a fixed 2.
    for k in (7, 2, 4, 5, 6):
        if dest_counts[k] < args.min_pool:
            continue
        n = max(args.min_per_dest,
                int(round(args.n_loss * dest_counts[k] / tot)))
        strata.append(("loss_to_%s" % CLASS_SHORT[k], (V12 == 1) & (V21 == k), n))
    strata.append(("control_stable_sasa", (V12 == 1) & (V21 == 1), args.n_control))
    strata.append(("control_stable_dwarfpine", (V12 == 7) & (V21 == 7), args.n_control))
    strata.append(("gain_dwarfpine_to_sasa", (V12 == 7) & (V21 == 1), 10))

    print("Loading aligned photographs ...")
    photos, raw = load_photos(eff, exposure_match=not args.no_exposure_match)
    print("  green pair  : %s %s  vs  %s %s (2021 gain %s)"
          % (photos[("2012", 0)][2], photos[("2012", 0)][1],
             photos[("2021", 0)][2], photos[("2021", 0)][1],
             np.round(photos[("2021", 0)][3], 3)))
    print("  autumn pair : %s %s  vs  %s %s (2021 gain %s)"
          % (photos[("2012", 1)][2], photos[("2012", 1)][1],
             photos[("2021", 1)][2], photos[("2021", 1)][1],
             np.round(photos[("2021", 1)][3], 3)))

    rows = []
    by_stratum = {}
    for name, mask, n in strata:
        picked = sample_stratum(mask, XX, YY, ZZ, p, eff, n, rng,
                                max_m_per_px=args.max_m_per_px)
        print("  %-26s requested %3d  got %3d  (pool %d cells)"
              % (name, n, len(picked), int((mask & np.isfinite(ZZ)).sum())))
        by_stratum[name] = []
        for i, s in enumerate(picked):
            s["stratum"] = name
            s["sample_id"] = "%s_%02d" % (name, i + 1)
            s["cls2012"] = int(V12[s["row"], s["col"]])
            s["cls2021"] = int(V21[s["row"], s["col"]])
            ui, vi = int(round(s["u"])), int(round(s["v"]))
            f_px = (W / 2) / np.tan(np.radians(p[2]) / 2)
            s["m_per_px_horiz"] = s["dist"] / f_px
            a = area[vi, ui]
            s["m2_per_px"] = float(a) if a > 0 else float("nan")
            # image-space (1x1 model) labels in the detail window
            w12 = crop(c12, s["u"], s["v"], R_DETAIL)
            w21 = crop(c21, s["u"], s["v"], R_DETAIL)
            s["imgcls2012_centre"] = int(c12[vi, ui])
            s["imgcls2021_centre"] = int(c21[vi, ui])
            s["imgfrac_sasa_2012"] = float((w12 == 1).mean())
            s["imgfrac_sasa_2021"] = float((w21 == 1).mean())
            s["imgfrac_pine_2012"] = float((w12 == 7).mean())
            s["imgfrac_pine_2021"] = float((w21 == 7).mean())
            for yr, key in (("2012", "2012"), ("2021", "2021")):
                g, t = patch_stats(photos[(yr, IDX_GREEN)][0], s["u"], s["v"])
                s["gcc_" + key] = g
                s["texture_" + key] = t
            dx, dy = phase_shift(crop(photos[("2012", IDX_GREEN)][0], s["u"], s["v"], 96),
                                 crop(photos[("2021", IDX_GREEN)][0], s["u"], s["v"], 96))
            s["align_dx_px"], s["align_dy_px"] = dx, dy
            s["align_shift_px"] = float(np.hypot(dx, dy))
            png = os.path.join(cropdir, s["sample_id"] + ".png")
            make_figure(s, photos, c12, c21, png, PU, PV, V12, V21)
            s["figure"] = os.path.relpath(png, outdir)
            rawpng = os.path.join(rawdir, s["sample_id"] + "_native.png")
            raw_crops(s, raw, rawpng)
            s["native_crop"] = os.path.relpath(rawpng, outdir)
            rows.append(s)
            by_stratum[name].append(s)
        if by_stratum[name]:
            contact_sheet(by_stratum[name], photos,
                          os.path.join(outdir, "contact_%s.png" % name))

    overview(photos[("2021", IDX_GREEN)][0], PU, PV, V12, V21, rows,
             os.path.join(outdir, "overview_sasa_change.png"))

    fields = ["sample_id", "stratum", "cls2012", "cls2021", "x", "y", "z",
              "dist", "u", "v", "row", "col", "m_per_px_horiz", "m2_per_px",
              "imgcls2012_centre", "imgcls2021_centre",
              "imgfrac_sasa_2012", "imgfrac_sasa_2021",
              "imgfrac_pine_2012", "imgfrac_pine_2021",
              "gcc_2012", "gcc_2021", "texture_2012", "texture_2021",
              "align_dx_px", "align_dy_px", "align_shift_px",
              "figure", "native_crop"]
    with open(os.path.join(outdir, "sample_index.csv"), "w", newline="") as fh:
        w = csv.DictWriter(fh, fieldnames=fields, extrasaction="ignore")
        w.writeheader()
        for r in rows:
            w.writerow(r)
    print("\nWrote %d samples -> %s" % (len(rows), outdir))


if __name__ == "__main__":
    main()
