"""
Reproject the archived GCPs with the archived camera parameters and report RMSE.

Inputs (read-only):
  data_from_server/ortho/data/gcp.csv          482 GCPs: u,v (image px) + x,y,z (JGD2011 / UTM-like, EPSG:6690)
  data_from_server/ortho/data/params_optim.json  camera parameters written by alproj

The camera model is alproj's pinhole model (src/alproj/optimize.py:_pinhole_project,
intrinsic_mat, extrinsic_mat, _distort).  This script re-implements it with numpy only so
that no moderngl / rasterio / OpenGL context is needed.

Several plausible conventions are tested, because the distortion centre and the
principal-point handling changed between alproj versions.
"""
import json
import math
import sys
import numpy as np

GCP = "/Users/okamoto/NIES/SasaSDMPaper/data_from_server/ortho/data/gcp.csv"
PAR = "/Users/okamoto/NIES/SasaSDMPaper/data_from_server/ortho/data/params_optim.json"


def load_gcp(path):
    rows = []
    with open(path) as f:
        hdr = f.readline().strip().split(",")
        idx = {n: i for i, n in enumerate(hdr)}
        for line in f:
            p = line.strip().split(",")
            if len(p) < 6:
                continue
            rows.append([float(p[idx[c]]) for c in ("u", "v", "x", "y", "z")])
    return np.asarray(rows)


def intrinsic_mat(fov_x_deg, w, h, cx, cy):
    fov_x = math.radians(fov_x_deg)
    fov_y = 2 * math.atan(math.tan(fov_x / 2) * h / w)
    fx = w / (2 * math.tan(fov_x / 2))
    fy = h / (2 * math.tan(fov_y / 2))
    return np.array([[fx, 0, cx], [0, fy, cy], [0, 0, 1]], float)


def extrinsic_mat(pan_deg, tilt_deg, roll_deg, tx, ty, tz):
    pan = math.radians(pan_deg)
    tilt = -math.radians(tilt_deg + 90)
    roll = -math.radians(roll_deg)
    rz = np.array([[math.cos(pan), -math.sin(pan), 0],
                   [math.sin(pan), math.cos(pan), 0],
                   [0, 0, 1]])
    rx = np.array([[1, 0, 0],
                   [0, math.cos(tilt), -math.sin(tilt)],
                   [0, math.sin(tilt), math.cos(tilt)]])
    ry = np.array([[math.cos(roll), 0, math.sin(roll)],
                   [0, 1, 0],
                   [-math.sin(roll), 0, math.cos(roll)]])
    rmat = rx @ ry @ rz
    tmat = rmat @ np.array([[-tx], [-ty], [-tz]])
    return np.vstack((np.hstack((rmat, tmat)), np.array([0, 0, 0, 1.0])))


def distort(pts, P, cx, cy):
    x1 = (pts[:, 0] - cx) / cx
    y1 = (pts[:, 1] - cy) / cy
    r2 = x1 ** 2 + y1 ** 2
    r4 = r2 ** 2
    r6 = r2 ** 3
    k1, k2, k3 = P["k1"], P["k2"], P["k3"]
    k4, k5, k6 = P["k4"], P["k5"], P["k6"]
    p1, p2 = P["p1"], P["p2"]
    s1, s2, s3, s4 = P["s1"], P["s2"], P["s3"], P["s4"]
    a1, a2 = P["a1"], P["a2"]
    xd = x1 * (1 + k1 * r2 + k2 * r4 + k3 * r6) / (1 + k4 * r2 + k5 * r4 + k6 * r6) \
        + 2 * p1 * x1 * y1 + p2 * (r2 + 2 * x1 ** 2) + s1 * r2 + s2 * r4
    yd = y1 * (1 + a1 + k1 * r2 + k2 * r4 + k3 * r6) / (1 + a2 + k4 * r2 + k5 * r4 + k6 * r6) \
        + p1 * (r2 + 2 * y1 ** 2) + 2 * p2 * x1 * y1 + s3 * r2 + s4 * r4
    return np.stack([xd * cx + cx, yd * cy + cy], axis=1)


def project(xyz, P, *, flip=True, dist_centre="cxcy", use_cxcy_intrinsic=True):
    w, h = P["w"], P["h"]
    cx = P["cx"] if use_cxcy_intrinsic else w / 2
    cy = P["cy"] if use_cxcy_intrinsic else h / 2
    imat = intrinsic_mat(P["fov"], w, h, cx, cy)
    emat = extrinsic_mat(P["pan"], P["tilt"], P["roll"], P["x"], P["y"], P["z"])
    op = np.vstack((xyz.T, np.ones(len(xyz))))
    cc = emat @ op
    ic = imat @ cc[:3, :]
    u = (w - ic[0] / ic[2]) if flip else (ic[0] / ic[2])
    v = ic[1] / ic[2]
    uv = np.stack([u, v], axis=1)
    if dist_centre == "cxcy":
        dcx, dcy = P["cx"], P["cy"]
    else:                                   # legacy alproj default
        dcx, dcy = (w - 1) / 2, (h - 1) / 2
    return distort(uv, P, dcx, dcy), cc[2]


def stats(pred, obs):
    d = np.hypot(pred[:, 0] - obs[:, 0], pred[:, 1] - obs[:, 1])
    return dict(mean=float(d.mean()),
                rmse=float(np.sqrt((d ** 2).mean())),
                median=float(np.median(d)),
                p90=float(np.percentile(d, 90)),
                max=float(d.max())), d


if __name__ == "__main__":
    P = json.load(open(PAR))
    g = load_gcp(GCP)
    obs = g[:, :2]
    xyz = g[:, 2:5]
    print(f"GCPs: {len(g)}")
    print(f"params_optim.json reported error = {P['error']:.6f}")
    print()
    best = None
    for flip in (True, False):
        for dc in ("cxcy", "legacy"):
            for uc in (True, False):
                pred, depth = project(xyz, P, flip=flip, dist_centre=dc,
                                      use_cxcy_intrinsic=uc)
                s, d = stats(pred, obs)
                tag = f"flip={flip!s:5s} dist_centre={dc:6s} intrinsic_cxcy={uc!s:5s}"
                print(f"{tag}  mean={s['mean']:12.4f} rmse={s['rmse']:12.4f} "
                      f"median={s['median']:12.4f} max={s['max']:12.2f}")
                if best is None or s["mean"] < best[0]["mean"]:
                    best = (s, tag, d, pred, depth)
    print()
    s, tag, d, pred, depth = best
    print("BEST:", tag)
    for k, v in s.items():
        print(f"   {k:8s} {v:.6f}")
    print(f"   n behind camera (depth<=0): {(depth <= 0).sum()}")
    np.save("/private/tmp/claude-501/-Users-okamoto-NIES-SasaSDMPaper--claude-worktrees-review-summary-ece-ddc69c/98ac7ecf-eafb-49c6-aa2d-9aab2998c13e/scratchpad/gcp_resid.npy", d)
