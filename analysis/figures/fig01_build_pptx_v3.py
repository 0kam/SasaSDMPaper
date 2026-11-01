#!/usr/bin/env python3
"""Build the single-graph Figure 1 draft without changing the authors' files.

Run from the repository root:
    python3 analysis/figures/fig01_build_pptx_v3.py

Outputs: paper/files/SasaPaper_Figures_v3.pptx, fig01_workflow_draft.pdf,
         fig01_workflow_draft.png (300 dpi, approximately 4000 x 2250 pixels).
Requires Python 3, LibreOffice (soffice), Poppler (pdftoppm, pdffonts), and GDAL
(gdalinfo, gdal_translate, gdaldem). No additional Python packages are needed.

The original OOXML primitives and Arial theme substitution are copied from
fig01_build_pptx.py. Do not import that script: importing it would write v2.
The camera, vegetation and snowmelt images come from the original deck;
probability/suitability thumbnails come from fig01_assets. The DEM thumbnail
is generated from the existing DEM, cropped to the vegetation raster extent.
Image legends are cropped out so all visible text is editable and >=14 pt.

Export (performed automatically in an isolated temporary directory):
    soffice -env:UserInstallation=file:///TEMP/profile --headless \
      --convert-to pdf --outdir TEMP paper/files/SasaPaper_Figures_v3.pptx
    pdftoppm -f 1 -singlefile -r 300 -png TEMP/SasaPaper_Figures_v3.pdf \
      TEMP/fig01_workflow_draft
The PDF and PNG are then copied to their draft paths, never to the originals.
On macOS the exporter supplies a temporary FONTCONFIG_FILE that points to
/System/Library/Fonts/Supplemental so headless LibreOffice can find Arial.
The PDF font check fails if Arial was substituted.
"""
import copy
import hashlib
import json
import shutil
import subprocess
import tempfile
from pathlib import Path
import os
import struct
import zipfile
import xml.etree.ElementTree as ET

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))
SRC = os.path.join(ROOT, "paper/files/SasaPaper_Figures.pptx")
DST = os.path.join(ROOT, "paper/files/SasaPaper_Figures_v3.pptx")
ASSETS = os.path.join(ROOT, "paper/files/fig01_assets")

A = "http://schemas.openxmlformats.org/drawingml/2006/main"
P = "http://schemas.openxmlformats.org/presentationml/2006/main"
R = "http://schemas.openxmlformats.org/officeDocument/2006/relationships"
for pre, uri in (("a", A), ("p", P), ("r", R),
                 ("a16", "http://schemas.microsoft.com/office/drawing/2014/main"),
                 ("p14", "http://schemas.microsoft.com/office/powerpoint/2010/main")):
    ET.register_namespace(pre, uri)
a = lambda t: "{%s}%s" % (A, t)
p = lambda t: "{%s}%s" % (P, t)
r = lambda t: "{%s}%s" % (R, t)

SLIDE_W, SLIDE_H = 12192000, 6858000
SCALE = 190.0 / (SLIDE_W / 914400.0 * 25.4)      # slide EMU -> printed size


def png_aspect(path):
    with open(path, "rb") as f:
        w, h = struct.unpack(">II", f.read(24)[16:24])
    return w / float(h)


AR_THUMB = png_aspect(os.path.join(ASSETS, "thumb_suitability.png"))
AR_SCHEM = png_aspect(os.path.join(ASSETS, "schematic_modelB.png"))

zin = zipfile.ZipFile(SRC)
slide = ET.fromstring(zin.read("ppt/slides/slide1.xml"))
tree = slide.find(p("cSld")).find(p("spTree"))

by_id = {}
for ch in list(tree):
    if ch.tag in (p("sp"), p("pic")):
        by_id[int(ch.find(".//" + p("cNvPr")).get("id"))] = ch

# --------------------------------------------------------------- primitives
_next = [100]


def new_id():
    _next[0] += 1
    return _next[0]


def set_xfrm(sh, x, y, cx, cy, rot=None):
    xf = sh.find(".//" + a("xfrm"))
    xf.find(a("off")).set("x", str(int(round(x))))
    xf.find(a("off")).set("y", str(int(round(y))))
    xf.find(a("ext")).set("cx", str(int(round(cx))))
    xf.find(a("ext")).set("cy", str(int(round(cy))))
    if rot is None:
        xf.attrib.pop("rot", None)
    else:
        xf.set("rot", str(int(rot)))
    return sh


def bbox(sh):
    """Visual bounding box, undoing a quarter-turn rotation."""
    xf = sh.find(".//" + a("xfrm"))
    o, e = xf.find(a("off")), xf.find(a("ext"))
    x, y = int(o.get("x")), int(o.get("y"))
    cx, cy = int(e.get("cx")), int(e.get("cy"))
    if xf.get("rot") in ("5400000", "16200000"):
        ccx, ccy = x + cx / 2.0, y + cy / 2.0
        cx, cy = cy, cx
        x, y = ccx - cx / 2.0, ccy - cy / 2.0
    return int(x), int(y), int(x + cx), int(y + cy)


def _run(txt, sz, bold, sup=False):
    rn = ET.Element(a("r"))
    attrs = {"kumimoji": "1", "lang": "en-US", "altLang": "ja-JP",
             "sz": str(sz), "dirty": "0"}
    if bold:
        attrs["b"] = "1"
    if sup:
        attrs["baseline"] = "30000"
    rp = ET.SubElement(rn, a("rPr"), attrs)
    fill = ET.SubElement(rp, a("solidFill"))
    ET.SubElement(fill, a("sysClr"), {"val": "windowText", "lastClr": "000000"})
    # Text-only template shapes have no fontRef; also make Arial explicit.
    ET.SubElement(rp, a("latin"), {"typeface": "Arial"})
    ET.SubElement(rn, a("t")).text = txt
    return rn


def set_text(sh, lines, align="ctr"):
    """lines: list of paragraphs; each a list of (text, sz, bold[, superscript])."""
    body = sh.find(p("txBody"))
    for el in body.findall(a("p")):
        body.remove(el)
    for line in lines:
        par = ET.SubElement(body, a("p"))
        ET.SubElement(par, a("pPr"), {"algn": align})
        for tup in line:
            par.append(_run(tup[0], tup[1], tup[2],
                            tup[3] if len(tup) > 3 else False))
    return sh


def clone(sh, name, at=None):
    c = copy.deepcopy(sh)
    nv = c.find(".//" + p("cNvPr"))
    nv.set("id", str(new_id()))
    nv.set("name", name)
    nv.attrib.pop("descr", None)
    for ext in list(nv):
        if ext.tag == a("extLst"):
            nv.remove(ext)
    if at is None:
        tree.append(c)
    else:
        tree.insert(at, c)
    return c


def drop(i):
    tree.remove(by_id.pop(i))


box_tpl = by_id[39]
text_tpl = by_id[29]

def textbox(name, x, y, cx, cy, lines, align="ctr"):
    sp = clone(text_tpl, name)
    set_xfrm(sp, x, y, cx, cy)
    return set_text(sp, lines, align)


def modelbox(name, x, y, cx, cy, lines):
    sp = clone(box_tpl, name)
    set_xfrm(sp, x, y, cx, cy)
    return set_text(sp, lines)


def picture(name, rel, x, y, cx, cy):
    pc = copy.deepcopy(by_id[10])
    nv = pc.find(".//" + p("cNvPr"))
    nv.set("id", str(new_id()))
    nv.set("name", name)
    nv.attrib.pop("descr", None)
    for ext in list(nv):
        if ext.tag == a("extLst"):
            nv.remove(ext)
    pc.find(".//" + a("blip")).set(r("embed"), rel)
    set_xfrm(pc, x, y, cx, cy)
    tree.append(pc)
    return pc


# The original slide is only a source of editable shapes and image assets.
for child in list(tree):
    if child.tag not in (p("nvGrpSpPr"), p("grpSpPr")):
        tree.remove(child)

PT = 12700
INK, ENV = "42494C", "3E6B60"
NODES, CONTENT, SEGMENTS, EDGES = {}, [], [], []


def rect_style(sh, fill="FFFFFF", stroke=INK, width=1.0):
    props = sh.find(p("spPr"))
    for el in list(props):
        if el.tag in (a("solidFill"), a("noFill"), a("ln")):
            props.remove(el)
    ET.SubElement(ET.SubElement(props, a("solidFill")), a("srgbClr"),
                  {"val": fill})
    ln = ET.SubElement(props, a("ln"), {"w": str(round(width * PT))})
    ET.SubElement(ET.SubElement(ln, a("solidFill")), a("srgbClr"),
                  {"val": stroke})


def text(name, rect, paragraphs, size=14, bold=False, align="l", owner=None):
    """Point-based placement around the unchanged v2 textbox primitive."""
    sh = textbox(name, *(v * PT for v in rect),
                 [[(line, size * 100, bold)] for line in paragraphs], align)
    body = sh.find(p("txBody")).find(a("bodyPr"))
    body.attrib.update({"lIns": "0", "rIns": "0", "tIns": "0",
                        "bIns": "0", "anchor": "ctr", "wrap": "square"})
    for el in list(body):
        body.remove(el)
    ET.SubElement(body, a("noAutofit"))
    for prop in sh.iter(a("pPr")):
        spacing = ET.SubElement(prop, a("lnSpc"))
        ET.SubElement(spacing, a("spcPts"), {"val": str(round(size * 115))})
        ET.SubElement(ET.SubElement(prop, a("spcAft")), a("spcPts"), {"val": "0"})
    CONTENT.append((name, rect, owner))
    return sh


def node(key, rect, fill="F5F6F5"):
    sh = modelbox(key, *(v * PT for v in rect), [])
    rect_style(sh, fill)
    NODES[key] = rect
    return sh


def thumb(name, rel, rect, owner, aspect=None, crop=None):
    """Fit images without distortion; OOXML crops remove tiny raster legends."""
    x, y, w, h = rect
    if aspect is not None:
        if w / h > aspect:
            x += (w - h * aspect) / 2
            w = h * aspect
        else:
            y += (h - w / aspect) / 2
            h = w / aspect
    sh = picture(name, rel, *(v * PT for v in (x, y, w, h)))
    if crop:
        fill = sh.find(p("blipFill"))
        fill.insert(1, ET.Element(a("srcRect"),
                                 dict(zip(("l", "t", "r", "b"), map(str, crop)))))
    CONTENT.append((name, (x, y, w, h), owner))
    return sh


def route(name, points, color=INK, arrow=True):
    """Native straight connectors keep every segment horizontal or vertical."""
    for i, ((x1, y1), (x2, y2)) in enumerate(zip(points, points[1:])):
        assert x1 == x2 or y1 == y2, name
        sp = ET.SubElement(tree, p("cxnSp"))
        nv = ET.SubElement(sp, p("nvCxnSpPr"))
        ET.SubElement(nv, p("cNvPr"), {"id": str(new_id()),
                      "name": f"{name} segment {i + 1}"})
        ET.SubElement(nv, p("cNvCxnSpPr"))
        ET.SubElement(nv, p("nvPr"))
        props = ET.SubElement(sp, p("spPr"))
        xf = ET.SubElement(props, a("xfrm"))
        if x2 < x1:
            xf.set("flipH", "1")
        if y2 < y1:
            xf.set("flipV", "1")
        ET.SubElement(xf, a("off"), {"x": str(round(min(x1, x2) * PT)),
                                    "y": str(round(min(y1, y2) * PT))})
        ET.SubElement(xf, a("ext"), {"cx": str(round(abs(x2 - x1) * PT)),
                                    "cy": str(round(abs(y2 - y1) * PT))})
        ET.SubElement(ET.SubElement(props, a("prstGeom"), {"prst": "line"}),
                      a("avLst"))
        ln = ET.SubElement(props, a("ln"), {"w": str(round(1.5 * PT))})
        ET.SubElement(ET.SubElement(ln, a("solidFill")), a("srgbClr"),
                      {"val": color})
        if arrow and i == len(points) - 2:
            ET.SubElement(ln, a("tailEnd"), {"type": "triangle", "w": "sm", "len": "sm"})
        SEGMENTS.append((name, (x1, y1), (x2, y2)))


def junction(x, y):
    sh = modelbox("Environmental-predictor junction", (x - 2) * PT,
                  (y - 2) * PT, 4 * PT, 4 * PT, [])
    sh.find(".//" + a("prstGeom")).set("prst", "ellipse")
    rect_style(sh, ENV, ENV, 0)


# Tier 1: the camera feeds two data products; terrain is an independent input.
node("D1", (259, 10, 252, 70))
thumb("Camera photographs", "rId5", (267, 18, 94, 54), "D1",
      aspect=4318000 / 2476500)
text("D1 sentence", (370, 16, 133, 58),
     ["A fixed camera photographed the slope hourly from 2010 to 2021."], owner="D1")

node("D2", (14, 119, 252, 75))
thumb("Vegetation-map thumbnail", "rId4", (22, 128, 69, 57), "D2",
      aspect=(2048 * .29) / (683 * .85), crop=(59000, 2000, 12000, 13000))
text("D2 sentence", (101, 127, 157, 59),
     ["Vegetation maps describe 2012 and 2021 at 1 m cell resolution."], owner="D2")

node("D3", (367, 119, 288, 75))
thumb("Snowmelt thumbnail", "rId3", (375, 128, 72, 57), "D3",
      aspect=(1200 * .68) / (700 * .91), crop=(3000, 4000, 29000, 5000))
text("D3 sentence", (457, 125, 190, 63),
     ["Annual snowmelt dates for 2011–2021 give the mean snowmelt date of every cell."], owner="D3")

node("D4", (720, 119, 224, 75))
thumb("DEM-derived terrain thumbnail", "rId9", (728, 128, 51, 57), "D4",
      aspect=1.0)
text("D4 sentence", (788, 125, 148, 63),
     ["Terrain variables (elevation, slope, TPI, TWI, aspect) are derived from a DEM."], owner="D4")

route("D1 to D2", [(288, 80), (288, 99), (140, 99), (140, 119)])
text("D1 to D2 label", (24, 58, 221, 35),
     ["alignment, classification,", "georectification"])
route("D1 to D3", [(490, 80), (490, 99), (511, 99), (511, 119)])
text("D1 to D3 label", (525, 80, 151, 20), ["snowmelt detection"])
EDGES += [("D1", "D2"), ("D1", "D3")]

# Tier 2: two distinct responses share the same environmental predictors.
node("MA", (14, 270, 295, 64), "EBF1EE")
text("MA title", (24, 278, 275, 20), ["Model A (habitat suitability)"], 16, True, owner="MA")
text("MA question", (24, 300, 275, 28),
     ["Where do environmental conditions permit Sasa?"], owner="MA")
node("MB", (432, 270, 512, 64), "EBF1EE")
text("MB title", (442, 278, 492, 20), ["Model B (establishment)"], 16, True, owner="MB")
text("MB question", (442, 300, 492, 28),
     ["Where did Sasa establish between 2012 and 2021, as a function of distance from the 2012 front and environment?"], owner="MB")

route("D2 to MA", [(70, 194), (70, 270)])
text("D2 to MA label", (81, 218, 140, 20), ["presence in 2021"])
route("D2 to MB", [(202, 194), (202, 203), (468, 203), (468, 270)])
text("D2 to MB label", (223, 207, 236, 47),
     ["2012→2021 establishment and", "distance from the 2012 front", "(distance surface)"])
EDGES += [("D2", "MA"), ("D2", "MB")]

# A shared green trunk visibly merges D3/D4 and forks into BOTH models.
route("D3 environmental input", [(590, 194), (590, 208), (750, 208)], ENV, False)
route("D4 environmental input", [(835, 194), (835, 208), (750, 208)], ENV, False)
route("Environmental predictor trunk", [(750, 208), (750, 258)], ENV, False)
text("Shared environmental-predictor label", (500, 214, 236, 37),
     ["environmental predictors", "(mean snowmelt date + terrain)"])
# A small gap marks the non-junction where the black D2->MB arrow crosses.
route("Environment to MA bus right", [(750, 258), (474, 258)], ENV, False)
route("Environment to MA bus left", [(462, 258), (276, 258), (276, 270)], ENV)
route("Environment to MB", [(750, 258), (900, 258), (900, 270)], ENV)
junction(750, 208)
junction(750, 258)
EDGES += [(source, target) for source in ("D3", "D4") for target in ("MA", "MB")]

# Tier 3: suitability identifies seed exposure; Model B drives front advance.
node("S", (217, 356, 200, 52))
text("S title", (225, 362, 184, 18), ["Snowmelt scenarios"], bold=True, owner="S")
text("S sentence", (225, 383, 184, 19), ["Use s = 0 or −0.71 days/year."], owner="S")

node("CA", (525, 363, 419, 75), "F4F1EB")
text("CA sentence", (535, 370, 399, 61),
     ["A stochastic cellular automaton applies Model B annually from 2022 to 2030. It recomputes the distance surface as simulated fronts advance. Snowmelt scenarios shift the mean snowmelt date by s days/year."], owner="CA")
route("MB to CA", [(735, 334), (735, 363)])
text("MB to CA label", (751, 337, 193, 21), ["establishment probability"])
route("S to CA", [(417, 390), (525, 390)])
text("S to CA label", (425, 351, 92, 34), ["snowmelt-date", "shift"], align="ctr")
EDGES += [("MB", "CA"), ("S", "CA")]

node("R2", (14, 442, 445, 85), "EBF1EE")
thumb("Suitability thumbnail", "rId7", (23, 451, 71, 67), "R2", aspect=AR_THUMB)
text("R2 sentence", (105, 451, 344, 67),
     ["Suitable but unoccupied area identifies the seed-dispersal stratum: communities are exposed if seeds arrive."], owner="R2")
route("MA to R2", [(50, 334), (50, 442)])
text("MA to R2 label", (63, 371, 143, 38), ["habitat suitability", "surface"])

node("R1", (525, 462, 419, 65), "F4F1EB")
thumb("Projection thumbnail", "rId6", (534, 470, 52, 49), "R1", aspect=AR_THUMB)
text("R1 sentence", (596, 469, 338, 51),
     ["Establishment probability by 2030 identifies the rhizome stratum: communities at risk from front advance."], owner="R1")
route("CA to R1", [(735, 438), (735, 462)])
text("CA to R1 label", (751, 439, 193, 20), ["P(established by 2030)"])
EDGES += [("MA", "R2"), ("CA", "R1")]


def overlaps(r1, r2):
    x, y, w, h = r1
    u, v, s, t = r2
    return x < u + s and u < x + w and y < v + t and v < y + h


def verify_layout():
    """Acceptance self-checks; rendering is still needed to check text fit."""
    for key, (x, y, w, h) in NODES.items():
        assert 0 <= x < x + w <= 960 and 0 <= y < y + h <= 540, key
    for i, (key, rect) in enumerate(NODES.items()):
        for other, other_rect in list(NODES.items())[i + 1:]:
            assert not overlaps(rect, other_rect), (key, other)
    for name, (x, y, w, h), owner in CONTENT:
        assert 0 <= x < x + w <= 960 and 0 <= y < y + h <= 540, name
        if owner:
            u, v, s, t = NODES[owner]
            assert u <= x and v <= y and x + w <= u + s and y + h <= v + t, name
        for edge, (x1, y1), (x2, y2) in SEGMENTS:
            assert not (x < x1 < x + w and x1 == x2 and
                        max(min(y1, y2), y) < min(max(y1, y2), y + h)), (edge, name)
            assert not (y < y1 < y + h and y1 == y2 and
                        max(min(x1, x2), x) < min(max(x1, x2), x + w)), (edge, name)
    for i, (name, rect, _) in enumerate(CONTENT):
        for other, other_rect, _ in CONTENT[i + 1:]:
            assert not overlaps(rect, other_rect), (name, other)
    reached = {"D1", "D4", "S"}
    for _ in NODES:
        reached.update(target for source, target in EDGES if source in reached)
    assert reached == set(NODES), reached
    connected = {"D1"}
    for _ in NODES:
        for source, target in EDGES:
            if source in connected or target in connected:
                connected.update((source, target))
    assert connected == set(NODES), connected
    sizes = {int(el.get("sz")) for el in slide.iter(a("rPr"))}
    assert min(sizes) >= 1400, sizes
    assert not list(slide.iter(a("normAutofit")))
    all_text = " ".join(el.text or "" for el in slide.iter(a("t")))
    for forbidden in ("colonized", "hugs", "concentrated", "−2.24"):
        assert forbidden not in all_text, forbidden
    print("Layout: 10 nodes, one connected graph, 12 directed interfaces; all labels >=14 pt.")
    print("No box/content overlaps or connector-label collisions in layout geometry.")


def _fix_theme_fonts(name, data):
    if name.startswith("ppt/theme/") and name.endswith(".xml"):
        return data.replace("游ゴシック Light".encode("utf-8"), b"Arial").replace(
            "游ゴシック".encode("utf-8"), b"Arial")
    return data


def command(args, env=None):
    result = subprocess.run(args, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True, env=env)
    if result.returncode:
        raise RuntimeError(f"{args[0]} failed ({result.returncode}):\n{result.stdout}{result.stderr}")


def terrain_thumbnail(work):
    """Use the real DEM, not a schematic or a snowmelt image relabelled as DEM."""
    vegetation = os.path.join(ROOT, "ortho/data/vege_2021_5x5.tiff")
    meta = json.loads(subprocess.check_output(["gdalinfo", "-json", vegetation]))
    ul, lr = meta["cornerCoordinates"]["upperLeft"], meta["cornerCoordinates"]["lowerRight"]
    dem, shade, png = (str(work / n) for n in ("dem.tif", "hillshade.tif", "terrain.png"))
    command(["gdal_translate", "-q", "-projwin", *map(str, (*ul, *lr)),
             "-outsize", "320", "0", "-r", "bilinear",
             os.path.join(ROOT, "ortho/data/mrd_dem_1m.tiff"), dem])
    command(["gdaldem", "hillshade", "-q", "-compute_edges", dem, shade])
    command(["gdal_translate", "-q", "-of", "PNG", shade, png])
    # Match the actual aspect ratio once the derived asset is available.
    pc = next(el for el in tree if el.find(".//" + p("cNvPr")) is not None and
              el.find(".//" + p("cNvPr")).get("name") == "DEM-derived terrain thumbnail")
    aspect = png_aspect(png)
    w, h = min(51, 57 * aspect), min(57, 51 / aspect)
    set_xfrm(pc, (728 + (51 - w) / 2) * PT, (128 + (57 - h) / 2) * PT, w * PT, h * PT)
    return Path(png).read_bytes()


def build_and_export():
    protected = [SRC, os.path.join(ROOT, "analysis/figures/fig01_build_pptx.py")]
    protected += [os.path.join(ROOT, "paper/files", n) for n in
                  ("SasaPaper_Figures_v2.pptx", "fig01_workflow.png", "fig01_workflow.pdf")]
    before = {path: hashlib.sha256(Path(path).read_bytes()).hexdigest() for path in protected}
    for executable in ("soffice", "pdftoppm", "pdffonts", "gdalinfo", "gdal_translate", "gdaldem"):
        if not shutil.which(executable):
            raise SystemExit(f"Required executable not found: {executable}")
    verify_layout()
    with tempfile.TemporaryDirectory(prefix="fig01-v3-") as temp:
        work = Path(temp)
        terrain = terrain_thumbnail(work)
        rels = zin.read("ppt/slides/_rels/slide1.xml.rels").decode("utf-8")
        extra = {"ppt/media/image5.png": Path(ASSETS, "thumb_suitability.png").read_bytes(),
                 "ppt/media/image7.png": terrain}
        for rid, media in (("rId7", "image5.png"), ("rId9", "image7.png")):
            rels = rels.replace("</Relationships>",
                f'<Relationship Id="{rid}" Type="{R}/image" Target="../media/{media}"/></Relationships>')
        replace = {"ppt/slides/slide1.xml": ET.tostring(slide, encoding="UTF-8", xml_declaration=True),
                   "ppt/slides/_rels/slide1.xml.rels": rels.encode("utf-8"),
                   "ppt/media/image4.png": Path(ASSETS, "thumb_pcol2030.png").read_bytes()}
        candidate = work / Path(DST).name
        with zipfile.ZipFile(candidate, "w", zipfile.ZIP_DEFLATED) as zout:
            for item in zin.infolist():
                zout.writestr(item, _fix_theme_fonts(item.filename,
                              replace.get(item.filename, zin.read(item.filename))))
            for name, data in extra.items():
                zout.writestr(name, data)
        with zipfile.ZipFile(candidate) as z:
            assert z.testzip() is None
            ET.fromstring(z.read("ppt/slides/slide1.xml"))
        export_env = os.environ.copy()
        mac_fonts = Path("/System/Library/Fonts/Supplemental")
        if mac_fonts.is_dir():
            config = ET.Element("fontconfig")
            ET.SubElement(config, "dir").text = str(mac_fonts)
            ET.SubElement(config, "cachedir").text = str(work / "font-cache")
            fontconfig = work / "fonts.conf"
            ET.ElementTree(config).write(fontconfig, encoding="utf-8", xml_declaration=True)
            export_env["FONTCONFIG_FILE"] = str(fontconfig)
        command(["soffice", f"-env:UserInstallation={(work / 'profile').as_uri()}",
                 "--headless", "--convert-to", "pdf", "--outdir", str(work), str(candidate)], env=export_env)
        pdf = candidate.with_suffix(".pdf")
        assert pdf.is_file(), "LibreOffice did not produce a PDF"
        fonts = subprocess.check_output(["pdffonts", str(pdf)], text=True).splitlines()[2:]
        assert fonts and all("Arial" in font for font in fonts), f"Arial was substituted: {fonts}"
        png_base = work / "fig01_workflow_draft"
        command(["pdftoppm", "-f", "1", "-singlefile", "-r", "300", "-png",
                 str(pdf), str(png_base)])
        png = png_base.with_suffix(".png")
        data = png.read_bytes()
        width, height = struct.unpack(">II", data[16:24])
        # LibreOffice's PDF page size can round by a fraction of a point.
        assert abs(width - SLIDE_W / 914400 * 300) <= 1 and height == 2250, (width, height)
        pos = data.index(b"pHYs") + 4
        dx, dy, unit = struct.unpack(">IIB", data[pos:pos + 9])
        assert unit == 1 and abs(dx * .0254 - 300) < .1 and dx == dy
        outputs = [(candidate, Path(DST)), (pdf, Path(ROOT, "paper/files/fig01_workflow_draft.pdf")),
                   (png, Path(ROOT, "paper/files/fig01_workflow_draft.png"))]
        for source, target in outputs:
            shutil.copyfile(source, target)
            print("Wrote", target)
    for path, digest in before.items():
        assert hashlib.sha256(Path(path).read_bytes()).hexdigest() == digest, path
    print(f"PNG: {width} x {height}, 300 dpi. Authors' files, original deck and v2 builder unchanged.")


if __name__ == "__main__":
    try:
        build_and_export()
    finally:
        zin.close()
