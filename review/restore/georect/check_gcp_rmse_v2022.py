"""
Reproject the archived GCPs with the archived camera parameters, using the
alproj camera model AS IT WAS IN 2022 (alproj commit c8a3e3d, src/alproj/optimize.py),
which is the version contemporary with this analysis.

Differences from alproj >= 1.0 (which is what a naive check would use, and which is why
the earlier audit could not reproduce the convention):

  * vertical focal length:  2022 uses  fov_y = fov_x * h / w
                            >=1.0 uses fov_y = 2*atan(tan(fov_x/2) * h/w)
  * distortion is applied in CAMERA coordinates (x/z, y/z) BEFORE the intrinsic matrix
    in 2022; in >=1.0 it is applied in PIXEL coordinates AFTER the intrinsic matrix,
    normalised by the principal point.
  * the tangential (p1,p2) and thin-prism (s1..s4) terms carry a MINUS sign in 2022 and
    a PLUS sign in >=1.0, and p1/p2 are swapped between the two components.

Usage:  python3 check_gcp_rmse_v2022.py
"""
import json
import math
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


def intrinsic_mat_2022(fov_x_deg, w, h, cx, cy):
    fov_x = math.radians(fov_x_deg)
    fov_y = fov_x * h / w                      # <- 2022 convention
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


def distort_2022(pts, P):
    """pts: 3 x N in camera coordinates."""
    z = pts[2]
    xn = pts[0] / z
    yn = pts[1] / z
    r2 = xn ** 2 + yn ** 2
    r4 = r2 ** 2
    r6 = r2 * r4
    k1, k2, k3 = P["k1"], P["k2"], P["k3"]
    k4, k5, k6 = P["k4"], P["k5"], P["k6"]
    p1, p2 = P["p1"], P["p2"]
    s1, s2, s3, s4 = P["s1"], P["s2"], P["s3"], P["s4"]
    a1, a2 = P["a1"], P["a2"]
    xd = (xn * ((1 + k1 * r2 + k2 * r4 + k3 * r6) / (1 + k4 * r2 + k5 * r4 + k6 * r6))
          - 2 * p1 * xn * yn - p2 * (r2 + 2 * xn ** 2) - s1 * r2 - s2 * r4) * z
    yd = (yn * ((1 + a1 + k1 * r2 + k2 * r4 + k3 * r6) / (1 + a2 + k4 * r2 + k5 * r4 + k6 * r6))
          - 2 * p2 * xn * yn - p1 * (r2 + 2 * yn ** 2) - s3 * r2 - s4 * r4) * z
    return np.vstack([xd, yd, z])


def project_2022(xyz, P):
    op = np.vstack((xyz.T, np.ones(len(xyz))))
    imat = intrinsic_mat_2022(P["fov"], P["w"], P["h"], P["cx"], P["cy"])
    emat = extrinsic_mat(P["pan"], P["tilt"], P["roll"], P["x"], P["y"], P["z"])
    op_cc = emat @ op
    op_cc2 = distort_2022(op_cc[:3], P)
    op_ic = imat @ op_cc2
    u = P["w"] - op_ic[0] / op_ic[2]
    v = op_ic[1] / op_ic[2]
    return np.stack([u, v], axis=1)


def report(pred, obs, label):
    d = np.hypot(pred[:, 0] - obs[:, 0], pred[:, 1] - obs[:, 1])
    print(f"{label}")
    print(f"   n            = {len(d)}")
    print(f"   mean err     = {d.mean():.6f} px   <- alproj's rmse() returns this")
    print(f"   true RMSE    = {np.sqrt((d**2).mean()):.6f} px")
    print(f"   median       = {np.median(d):.6f} px")
    print(f"   90th pct     = {np.percentile(d, 90):.6f} px")
    print(f"   max          = {d.max():.6f} px")
    print(f"   du mean/sd   = {(pred[:,0]-obs[:,0]).mean():.4f} / {(pred[:,0]-obs[:,0]).std():.4f}")
    print(f"   dv mean/sd   = {(pred[:,1]-obs[:,1]).mean():.4f} / {(pred[:,1]-obs[:,1]).std():.4f}")
    return d


if __name__ == "__main__":
    P = json.load(open(PAR))
    g = load_gcp(GCP)
    obs, xyz = g[:, :2], g[:, 2:5]
    print(f"params_optim.json reported 'error' = {P['error']:.6f}\n")
    pred = project_2022(xyz, P)
    d = report(pred, obs, "alproj 2022 (commit c8a3e3d) pinhole model")
    np.save("/private/tmp/claude-501/-Users-okamoto-NIES-SasaSDMPaper--claude-worktrees-review-summary-ece-ddc69c/98ac7ecf-eafb-49c6-aa2d-9aab2998c13e/scratchpad/gcp_resid_2022.npy",
            np.column_stack([obs, pred, d]))
