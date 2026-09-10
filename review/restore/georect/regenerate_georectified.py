"""
regenerate_georectified.py
--------------------------
Regenerates ortho/data/georectified.csv (the per-image-pixel lookup table
u,v -> x,y,z) from two inputs only:

    ortho/data/mrd_dem_1m.tiff   1 m DSM of Mt. Tateyama (EPSG:6690)
    ortho/data/params_optim.json optimised camera parameters

It reimplements the geometry of alproj.surface.crop() + alproj.project.persp_proj()
+ alproj.project.reverse_proj() at commit 0kam/alproj@c8a3e3d (2022-09-23) --
the version that reproduces the archived product -- but builds the surface mesh
straight from the DSM instead of from the SQLite point cloud, because
ortho/data/pointcloud.db in the server recovery is truncated (its header
declares 554,768 x 4 kB = 2.27 GB, the recovered file is 109 MB).

Vertex colour is irrelevant to georectification, so the renderer interpolates the
vertex coordinates themselves (exactly what reverse_proj does) and the aerial
orthophoto (tateyama2.tiff, also truncated) is not needed.

Usage:
    python regenerate_georectified.py <out.csv> [--maxdist 3000]
"""
import sys, json, time, argparse
import numpy as np
import rasterio
import moderngl as gl
import math

D = "/Users/okamoto/NIES/SasaSDMPaper/data_from_server/ortho/data/"


def projection_mat(fov_x_deg, w, h, near=-1, far=1, cx=None, cy=None):
    cx = w / 2 if cx is None else cx
    cy = h / 2 if cy is None else cy
    fov_x = fov_x_deg * math.pi / 180
    fov_y = fov_x * h / w          # alproj@c8a3e3d convention (NOT the atan form)
    fx = 1 / math.tan(fov_x / 2)
    fy = 1 / math.tan(fov_y / 2)
    return np.array([fx, 0, (w - 2 * cx) / w, 0,
                     0, fy, -(h - 2 * cy) / h, 0,
                     0, 0, -(far + near) / (far - near), -2 * far * near / (far - near),
                     0, 0, -1, 0])


def modelview_mat(pan_deg, tilt_deg, roll_deg, t_x, t_y, t_z):
    pan = (360 - pan_deg) * math.pi / 180
    tilt = tilt_deg * math.pi / 180
    roll = roll_deg * math.pi / 180
    rx = np.array([[1, 0, 0, 0], [0, math.cos(tilt), -math.sin(tilt), 0],
                   [0, math.sin(tilt), math.cos(tilt), 0], [0, 0, 0, 1]])
    ry = np.array([[math.cos(pan), 0, math.sin(pan), 0], [0, 1, 0, 0],
                   [-math.sin(pan), 0, math.cos(pan), 0], [0, 0, 0, 1]])
    rz = np.array([[math.cos(roll), -math.sin(roll), 0, 0], [math.sin(roll), math.cos(roll), 0, 0],
                   [0, 0, 1, 0], [0, 0, 0, 1]])
    rmat = rz @ rx @ ry
    tmat = np.array([[1, 0, 0, -t_x], [0, 1, 0, -t_z], [0, 0, 1, -t_y], [0, 0, 0, 1]])
    return (rmat @ tmat).transpose().flatten()


VERT_SHADER = '''
#version 330
precision highp float;
in vec3 in_vert;
in vec3 in_color;
out vec3 v_color;
uniform mat4 proj;
uniform mat4 view;
uniform float dist_coeffs[14];
vec4 distort(vec4 view_pos){
  float z = view_pos.z;
  float x1 = view_pos.x / z;
  float y1 = view_pos.y / z;
  float x1_2 = x1*x1; float y1_2 = y1*y1;
  if ((x1_2 > 1.0) || (y1_2 > 1.0)) { return view_pos; }
  float x1_y1 = x1*y1;
  float r2 = x1_2 + y1_2; float r4 = r2*r2; float r6 = r4*r2;
  float r_dist_x = (1.0+dist_coeffs[2]*r2+dist_coeffs[3]*r4+dist_coeffs[4]*r6)
                  /(1.0+dist_coeffs[5]*r2+dist_coeffs[6]*r4+dist_coeffs[7]*r6);
  float r_dist_y = (1.0+dist_coeffs[0]+dist_coeffs[2]*r2+dist_coeffs[3]*r4+dist_coeffs[4]*r6)
                  /(1.0+dist_coeffs[1]+dist_coeffs[5]*r2+dist_coeffs[6]*r4+dist_coeffs[7]*r6);
  float x2 = x1*r_dist_x + 2*dist_coeffs[8]*x1_y1 + dist_coeffs[9]*(r2 + 2*x1_2) + dist_coeffs[10]*r2 + dist_coeffs[11]*r4;
  float y2 = y1*r_dist_y + 2*dist_coeffs[9]*x1_y1 + dist_coeffs[8]*(r2 + 2*y1_2) + dist_coeffs[12]*r2 + dist_coeffs[13]*r4;
  return vec4(x2*z, y2*z, z, view_pos[3]);
}
void main() {
  vec4 view_pos = view * vec4(in_vert, 1.0);
  gl_Position = proj * distort(view_pos);
  v_color = in_color;
}
'''
FRAG_SHADER = '''
#version 330
precision highp float;
in vec3 v_color;
layout(location=0) out vec4 f_color;
void main() { f_color = vec4(v_color, 1.0); }
'''


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("out")
    ap.add_argument("--maxdist", type=float, default=3000.0)
    ap.add_argument("--origin-shift", action="store_true",
                    help="render coordinates relative to the camera (float32-safe) "
                         "and add the offset back; off by default to match the archive")
    a = ap.parse_args()

    P = {k: v for k, v in json.load(open(D + "params_optim.json")).items() if k != "error"}
    w, h = int(P["w"]), int(P["h"])

    t0 = time.time()
    dem = rasterio.open(D + "mrd_dem_1m.tiff")
    Z = dem.read(1).astype("float64")
    T = dem.transform
    ny, nx = Z.shape
    x = T.c + (np.arange(nx) + 0.5) * T.a
    y = T.f + (np.arange(ny) + 0.5) * T.e
    XX, YY = np.meshgrid(x, y)
    nod = dem.nodata
    bad = ~np.isfinite(Z) | (Z <= 0)
    if nod is not None:
        bad |= (Z == nod)
    Z[bad] = 0.0
    far = ((XX - P["x"]) ** 2 + (YY - P["y"]) ** 2) > a.maxdist ** 2
    drop = bad | far
    print(f"DSM {nx} x {ny} = {nx*ny:,} cells; dropped {drop.sum():,} "
          f"(nodata {bad.sum():,}, beyond {a.maxdist:.0f} m {far.sum():,})   [{time.time()-t0:.1f} s]")

    # OpenGL vertex order is (X, Z, Y): z is the near-far axis
    off = np.array([P["x"], P["z"], P["y"]]) if a.origin_shift else np.zeros(3)
    vert = np.stack([XX.ravel() - off[0], Z.ravel() - off[1], YY.ravel() - off[2]], 1).astype("f4")
    # the value channel carries the *geographic* coordinates, as reverse_proj does
    val = np.stack([XX.ravel(), Z.ravel(), YY.ravel()], 1).astype("f4")

    # two triangles per DSM cell, dropping any triangle touching a dropped vertex
    idx = np.arange(ny * nx, dtype=np.int64).reshape(ny, nx)
    a00 = idx[:-1, :-1].ravel(); a01 = idx[:-1, 1:].ravel()
    a10 = idx[1:, :-1].ravel();  a11 = idx[1:, 1:].ravel()
    tri = np.concatenate([np.stack([a00, a10, a11], 1), np.stack([a00, a11, a01], 1)])
    dr = drop.ravel()
    tri = tri[~(dr[tri[:, 0]] | dr[tri[:, 1]] | dr[tri[:, 2]])].astype("i4")
    print(f"triangles kept: {len(tri):,}   [{time.time()-t0:.1f} s]")

    ctx = gl.create_standalone_context()
    ctx.enable(gl.DEPTH_TEST)
    prog = ctx.program(vertex_shader=VERT_SHADER, fragment_shader=FRAG_SHADER)
    prog['proj'].value = tuple(projection_mat(P["fov"], w, h))
    prog['view'].value = tuple(modelview_mat(P["pan"], P["tilt"], P["roll"],
                                             P["x"] - off[0], P["y"] - off[2], P["z"] - off[1]))
    prog['dist_coeffs'].value = [P[k] for k in
                                 ["a1", "a2", "k1", "k2", "k3", "k4", "k5", "k6",
                                  "p1", "p2", "s1", "s2", "s3", "s4"]]
    vbo = ctx.buffer(vert.tobytes()); cbo = ctx.buffer(val.tobytes())
    ibo = ctx.buffer(tri.tobytes())
    vao = ctx.vertex_array(prog, [(vbo, "3f", "in_vert"), (cbo, "3f", "in_color")], ibo)
    rbo = ctx.renderbuffer((w, h), dtype="f4"); drbo = ctx.depth_renderbuffer((w, h))
    fbo = ctx.framebuffer(rbo, drbo); fbo.use(); fbo.clear(0.0, 0.0, 0.0, 1.0)
    t1 = time.time()
    vao.render()
    raw = np.frombuffer(fbo.read(dtype="f4"), dtype="float32").reshape(h, w, 3)
    raw = np.flipud(raw)
    print(f"render: {time.time()-t1:.1f} s   [{time.time()-t0:.1f} s total]")

    coord = raw[:, :, [0, 2, 1]]                 # -> x, y, z
    U, V = np.meshgrid(np.arange(w), np.arange(h))
    keep = coord[:, :, 0] > 0
    import pandas as pd
    df = pd.DataFrame({"u": U[keep].astype("int16"), "v": V[keep].astype("int16"),
                       "x": coord[:, :, 0][keep], "y": coord[:, :, 1][keep],
                       "z": coord[:, :, 2][keep]})
    df.to_csv(a.out, index=False)
    print(f"wrote {a.out}: {len(df):,} rows   [{time.time()-t0:.1f} s total]")


if __name__ == "__main__":
    main()
