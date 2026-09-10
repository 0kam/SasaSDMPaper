"""
Verify that the archived data/georectified.csv is the output of alproj's OpenGL
reverse projection under data/params_optim.json.

georectified.csv holds, for every image pixel that hit the terrain surface,
    u, v  : pixel coordinates in the 5616 x 3744 reference photograph
    x,y,z : the map coordinate of the surface point seen through that pixel (EPSG:6690)
    B,G,R : the pixel colour

If params_optim.json really produced it, then pushing (x,y,z) FORWARD through the same
camera model that alproj's vertex shader uses must land back on (u,v) to sub-pixel
accuracy.  This script re-implements that shader arithmetic in numpy (alproj 2022,
commit c8a3e3d, src/alproj/project.py: projection_mat / modelview_mat / distort())
and measures the round-trip error on a random sample of rows.

NOTE this is the RENDER model, which differs from the model used by
alproj.optimize.project() for GCP scoring: the vertex shader applies the tangential
(p1,p2) and thin-prism (s1..s4) terms with a PLUS sign, the optimiser with a MINUS sign.
Both are reproduced here so the size of that internal inconsistency can be measured.

Usage:  python3 verify_georectified_csv.py [stride]
"""
import json
import math
import sys
import numpy as np

CSV = "/Users/okamoto/NIES/SasaSDMPaper/data_from_server/ortho/data/georectified.csv"
PAR = "/Users/okamoto/NIES/SasaSDMPaper/data_from_server/ortho/data/params_optim.json"


def projection_mat(fov_x_deg, w, h, near=-1.0, far=1.0, cx=None, cy=None):
    cx = w / 2 if cx is None else cx
    cy = h / 2 if cy is None else cy
    fov_x = math.radians(fov_x_deg)
    fov_y = fov_x * h / w                       # 2022 convention
    fx = 1 / math.tan(fov_x / 2)
    fy = 1 / math.tan(fov_y / 2)
    return np.array([
        [fx, 0, (w - 2 * cx) / w, 0],
        [0, fy, -(h - 2 * cy) / h, 0],
        [0, 0, -(far + near) / (far - near), -2 * far * near / (far - near)],
        [0, 0, -1, 0],
    ], float)


def modelview_mat(pan_deg, tilt_deg, roll_deg, tx, ty, tz):
    pan = math.radians(360 - pan_deg)
    tilt = math.radians(tilt_deg)
    roll = math.radians(roll_deg)
    rx = np.array([[1, 0, 0, 0],
                   [0, math.cos(tilt), -math.sin(tilt), 0],
                   [0, math.sin(tilt), math.cos(tilt), 0],
                   [0, 0, 0, 1]])
    ry = np.array([[math.cos(pan), 0, math.sin(pan), 0],
                   [0, 1, 0, 0],
                   [-math.sin(pan), 0, math.cos(pan), 0],
                   [0, 0, 0, 1]])
    rz = np.array([[math.cos(roll), -math.sin(roll), 0, 0],
                   [math.sin(roll), math.cos(roll), 0, 0],
                   [0, 0, 1, 0],
                   [0, 0, 0, 1]])
    rmat = rz @ rx @ ry
    tmat = np.array([[1, 0, 0, -tx],
                     [0, 1, 0, -tz],
                     [0, 0, 1, -ty],
                     [0, 0, 0, 1.0]])
    return rmat @ tmat


def shader_distort(view, P, sign=+1.0):
    """view: 4 x N (GL camera space, x right, y up, z towards viewer i.e. negative ahead)"""
    z = view[2]
    x1 = view[0] / z
    y1 = view[1] / z
    x1_2, y1_2 = x1 ** 2, y1 ** 2
    r2 = x1_2 + y1_2
    r4 = r2 * r2
    r6 = r4 * r2
    a1, a2 = P["a1"], P["a2"]
    k1, k2, k3, k4, k5, k6 = (P["k1"], P["k2"], P["k3"], P["k4"], P["k5"], P["k6"])
    p1, p2 = P["p1"], P["p2"]
    s1, s2, s3, s4 = P["s1"], P["s2"], P["s3"], P["s4"]
    rdx = (1 + k1 * r2 + k2 * r4 + k3 * r6) / (1 + k4 * r2 + k5 * r4 + k6 * r6)
    rdy = (1 + a1 + k1 * r2 + k2 * r4 + k3 * r6) / (1 + a2 + k4 * r2 + k5 * r4 + k6 * r6)
    x2 = x1 * rdx + sign * (2 * p1 * x1 * y1 + p2 * (r2 + 2 * x1_2) + s1 * r2 + s2 * r4)
    y2 = y1 * rdy + sign * (2 * p2 * x1 * y1 + p1 * (r2 + 2 * y1_2) + s3 * r2 + s4 * r4)
    out = view.copy()
    skip = (x1_2 > 1.0) | (y1_2 > 1.0)          # the shader's early-out
    out[0] = np.where(skip, view[0], x2 * z)
    out[1] = np.where(skip, view[1], y2 * z)
    return out


def render_project(xyz, P, sign=+1.0):
    """xyz: N x 3 map coordinates -> N x 2 pixel coordinates (top-left origin)."""
    w, h = P["w"], P["h"]
    # alproj feeds vertices as (X, Z, Y)
    local = np.vstack([xyz[:, 0], xyz[:, 2], xyz[:, 1], np.ones(len(xyz))])
    view = modelview_mat(P["pan"], P["tilt"], P["roll"], P["x"], P["y"], P["z"]) @ local
    dist = shader_distort(view, P, sign=sign)
    clip = projection_mat(P["fov"], w, h) @ dist
    ndc_x = clip[0] / clip[3]
    ndc_y = clip[1] / clip[3]
    i = (ndc_x + 1) / 2 * w - 0.5               # GL column
    j = (ndc_y + 1) / 2 * h - 0.5               # GL row
    # Empirically determined against the archived georectified.csv: the stored pixel
    # coordinates are the left-right mirror of the GL column (the same "w - u" flip that
    # alproj.optimize.project applies), and the GL row taken directly.
    return np.stack([(w - 1) - i, j], axis=1)


def sample_rows(path, stride):
    us, vs, xs, ys, zs = [], [], [], [], []
    with open(path) as f:
        f.readline()
        for n, line in enumerate(f):
            if n % stride:
                continue
            p = line.rstrip("\n").split(",")
            if len(p) < 8:
                continue                        # truncated final line
            try:
                us.append(int(p[0])); vs.append(int(p[1]))
                xs.append(float(p[2])); ys.append(float(p[3])); zs.append(float(p[4]))
            except ValueError:
                continue
    return (np.array(us, float), np.array(vs, float),
            np.column_stack([xs, ys, zs]))


if __name__ == "__main__":
    stride = int(sys.argv[1]) if len(sys.argv) > 1 else 200
    P = json.load(open(PAR))
    u, v, xyz = sample_rows(CSV, stride)
    print(f"sampled {len(u)} rows (every {stride}th) from georectified.csv")
    for sign, label in ((+1.0, "render/shader sign (+)"), (-1.0, "optimizer sign (-)")):
        pred = render_project(xyz, P, sign=sign)
        du = pred[:, 0] - u
        dv = pred[:, 1] - v
        d = np.hypot(du, dv)
        print(f"\n{label}")
        print(f"   mean |err| = {d.mean():.4f} px   RMSE = {np.sqrt((d**2).mean()):.4f} px")
        print(f"   median     = {np.median(d):.4f} px   p99 = {np.percentile(d,99):.4f}  max = {d.max():.4f}")
        print(f"   du mean/sd = {du.mean():+.4f} / {du.std():.4f}")
        print(f"   dv mean/sd = {dv.mean():+.4f} / {dv.std():.4f}")
        print(f"   frac within 1 px = {(d<1).mean():.6f}   within 0.5 px = {(d<0.5).mean():.6f}")
