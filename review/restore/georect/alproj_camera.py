"""
Recovered camera model of the Mt. Tateyama / Murodo time-lapse camera.

This is a dependency-light (numpy only) re-implementation of the projection that alproj
(https://github.com/0kam/alproj) performed when `ortho/data/georectified.csv` was made.
It reproduces the archived georectified.csv to a mean 0.41 px / RMSE 0.50 px round trip
(see verify_georectified_csv.py), so it can be used in place of the original OpenGL
renderer for map<->image work.

Which alproj vintage?  The camera model changed between versions.  The one reproduced
here is alproj as of commit c8a3e3d (2022-09-23), the version contemporary with this
analysis.  The two places it differs from alproj >= 1.0 are:

  1. vertical focal length:  fov_y = fov_x * h / w        (>=1.0: 2*atan(tan(fov_x/2)*h/w))
  2. lens distortion is applied to the NORMALISED CAMERA coordinates (x/z, y/z) before
     the projection matrix                                (>=1.0: to pixel coordinates
     normalised by the principal point, after the intrinsic matrix)

and inside 2022 alproj itself the vertex shader (used for rendering / reverse projection,
i.e. for georectified.csv) and optimize.project (used for scoring GCPs) disagree on the
SIGN of the tangential (p1,p2) and thin-prism (s1..s4) terms.  `project_render` below
follows the shader, which is what actually produced the published product;
`project_gcp` follows the optimiser, which is what the reported error refers to.
The disagreement between them is ~29 px, so the distinction matters.

Pixel convention: `u` increases to the right, `v` downwards, origin at the top-left
pixel centre, 5616 x 3744.
"""
import json
import math
import numpy as np

DEFAULT_PARAMS = "/Users/okamoto/NIES/SasaSDMPaper/data_from_server/ortho/data/params_optim.json"


def load_params(path=DEFAULT_PARAMS):
    return json.load(open(path))


# ---------------------------------------------------------------- render model
def _projection_mat(fov_x_deg, w, h, near=-1.0, far=1.0):
    fov_x = math.radians(fov_x_deg)
    fov_y = fov_x * h / w
    fx = 1 / math.tan(fov_x / 2)
    fy = 1 / math.tan(fov_y / 2)
    return np.array([[fx, 0, 0, 0],
                     [0, fy, 0, 0],
                     [0, 0, -(far + near) / (far - near), -2 * far * near / (far - near)],
                     [0, 0, -1, 0]], float)


def _modelview_mat(pan_deg, tilt_deg, roll_deg, tx, ty, tz):
    pan = math.radians(360 - pan_deg)
    tilt = math.radians(tilt_deg)
    roll = math.radians(roll_deg)
    rx = np.array([[1, 0, 0, 0], [0, math.cos(tilt), -math.sin(tilt), 0],
                   [0, math.sin(tilt), math.cos(tilt), 0], [0, 0, 0, 1]])
    ry = np.array([[math.cos(pan), 0, math.sin(pan), 0], [0, 1, 0, 0],
                   [-math.sin(pan), 0, math.cos(pan), 0], [0, 0, 0, 1]])
    rz = np.array([[math.cos(roll), -math.sin(roll), 0, 0],
                   [math.sin(roll), math.cos(roll), 0, 0], [0, 0, 1, 0], [0, 0, 0, 1]])
    tmat = np.array([[1, 0, 0, -tx], [0, 1, 0, -tz], [0, 0, 1, -ty], [0, 0, 0, 1.0]])
    return (rz @ rx @ ry) @ tmat


def _shader_distort(view, P, sign=1.0):
    z = view[2]
    x1 = view[0] / z
    y1 = view[1] / z
    x1_2, y1_2 = x1 * x1, y1 * y1
    r2 = x1_2 + y1_2
    r4 = r2 * r2
    r6 = r4 * r2
    rdx = (1 + P["k1"] * r2 + P["k2"] * r4 + P["k3"] * r6) / \
          (1 + P["k4"] * r2 + P["k5"] * r4 + P["k6"] * r6)
    rdy = (1 + P["a1"] + P["k1"] * r2 + P["k2"] * r4 + P["k3"] * r6) / \
          (1 + P["a2"] + P["k4"] * r2 + P["k5"] * r4 + P["k6"] * r6)
    x2 = x1 * rdx + sign * (2 * P["p1"] * x1 * y1 + P["p2"] * (r2 + 2 * x1_2)
                            + P["s1"] * r2 + P["s2"] * r4)
    y2 = y1 * rdy + sign * (2 * P["p2"] * x1 * y1 + P["p1"] * (r2 + 2 * y1_2)
                            + P["s3"] * r2 + P["s4"] * r4)
    skip = (x1_2 > 1.0) | (y1_2 > 1.0)          # the shader's early-out
    out = view.copy()
    out[0] = np.where(skip, view[0], x2 * z)
    out[1] = np.where(skip, view[1], y2 * z)
    return out


def project_render(x, y, z, P):
    """Map coordinates (EPSG:6690) -> image pixel (u, v) and camera distance.

    Returns u, v, dist.  No occlusion test: see zbuffer() for that.
    """
    x = np.asarray(x, float).ravel()
    y = np.asarray(y, float).ravel()
    z = np.asarray(z, float).ravel()
    w, h = P["w"], P["h"]
    local = np.vstack([x, z, y, np.ones(len(x))])          # alproj feeds (X, Z, Y)
    view = _modelview_mat(P["pan"], P["tilt"], P["roll"], P["x"], P["y"], P["z"]) @ local
    dist = np.sqrt((x - P["x"]) ** 2 + (y - P["y"]) ** 2 + (z - P["z"]) ** 2)
    dist = np.where(view[2] > 0, dist, np.inf)             # inf == behind the camera
    dm = _shader_distort(view, P, sign=1.0)
    clip = _projection_mat(P["fov"], w, h) @ dm
    u = (w - 1) - ((clip[0] / clip[3] + 1) / 2 * w - 0.5)  # GL column, mirrored
    v = (clip[1] / clip[3] + 1) / 2 * h - 0.5
    return u, v, dist


# ------------------------------------------------------------- optimiser model
def _intrinsic_mat(fov_x_deg, w, h, cx, cy):
    fov_x = math.radians(fov_x_deg)
    fov_y = fov_x * h / w
    return np.array([[w / (2 * math.tan(fov_x / 2)), 0, cx],
                     [0, h / (2 * math.tan(fov_y / 2)), cy],
                     [0, 0, 1]], float)


def _extrinsic_mat(pan_deg, tilt_deg, roll_deg, tx, ty, tz):
    pan = math.radians(pan_deg)
    tilt = -math.radians(tilt_deg + 90)
    roll = -math.radians(roll_deg)
    rz = np.array([[math.cos(pan), -math.sin(pan), 0], [math.sin(pan), math.cos(pan), 0], [0, 0, 1]])
    rx = np.array([[1, 0, 0], [0, math.cos(tilt), -math.sin(tilt)], [0, math.sin(tilt), math.cos(tilt)]])
    ry = np.array([[math.cos(roll), 0, math.sin(roll)], [0, 1, 0], [-math.sin(roll), 0, math.cos(roll)]])
    rmat = rx @ ry @ rz
    return np.vstack((np.hstack((rmat, rmat @ np.array([[-tx], [-ty], [-tz]]))),
                      np.array([0, 0, 0, 1.0])))


def project_gcp(x, y, z, P):
    """The projection alproj.optimize.project() used for GCP scoring (2022 vintage)."""
    x = np.asarray(x, float).ravel(); y = np.asarray(y, float).ravel(); z = np.asarray(z, float).ravel()
    op = np.vstack([x, y, z, np.ones(len(x))])
    cc = _extrinsic_mat(P["pan"], P["tilt"], P["roll"], P["x"], P["y"], P["z"]) @ op
    zc = cc[2]
    xn, yn = cc[0] / zc, cc[1] / zc
    r2 = xn ** 2 + yn ** 2
    r4 = r2 ** 2
    r6 = r2 * r4
    xd = (xn * ((1 + P["k1"] * r2 + P["k2"] * r4 + P["k3"] * r6) /
                (1 + P["k4"] * r2 + P["k5"] * r4 + P["k6"] * r6))
          - 2 * P["p1"] * xn * yn - P["p2"] * (r2 + 2 * xn ** 2) - P["s1"] * r2 - P["s2"] * r4) * zc
    yd = (yn * ((1 + P["a1"] + P["k1"] * r2 + P["k2"] * r4 + P["k3"] * r6) /
                (1 + P["a2"] + P["k4"] * r2 + P["k5"] * r4 + P["k6"] * r6))
          - 2 * P["p2"] * xn * yn - P["p1"] * (r2 + 2 * yn ** 2) - P["s3"] * r2 - P["s4"] * r4) * zc
    ic = _intrinsic_mat(P["fov"], P["w"], P["h"], P["cx"], P["cy"]) @ np.vstack([xd, yd, zc])
    return P["w"] - ic[0] / ic[2], ic[1] / ic[2]
