#!/usr/bin/env python3
"""Build paper/files/SasaPaper_Figures_v2.pptx (Fig. 1) from the original deck.

paper/files/SasaPaper_Figures.pptx is read-only input; everything is written to
the _v2 copy, so re-running this script reproduces the figure from scratch.

Layout:

  (a) data generation -- one left-to-right chain across the top band:
      time-lapse photo -> "align, classify, geo-correct" -> product column
      (vegetation maps over snowmelt DOY) + terrain from the DEM.
  (b)(c)(d) -- three columns below, each the same vertical flow:
      labelled input -> down arrow -> model box -> down arrow -> output image
      -> caption.  (d) additionally takes the snowmelt-scenario box.

  Every panel sits on a rounded grey plate; every arrow is vertical or
  horizontal; no text is smaller than 14 pt, which is ~7.9 pt once the
  338.7 mm slide is reduced to the 190 mm printed figure (x0.561).

Run:  python3 analysis/figures/fig01_build_pptx.py
"""
import copy
import os
import struct
import zipfile
import xml.etree.ElementTree as ET

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))
SRC = os.path.join(ROOT, "paper/files/SasaPaper_Figures.pptx")
DST = os.path.join(ROOT, "paper/files/SasaPaper_Figures_v2.pptx")
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


# ------------------------------------------------------------------- sizing
TAG_SZ, HEAD_SZ, BODY_SZ, LABEL_SZ, CAP_SZ = 2400, 1800, 1400, 1400, 1400
PANEL_FILL = "F2F2F2"

TAG_W, TAG_H = 950000, 480000  # wide enough that "(a)" never wraps at 24 pt
PAD = 150000            # panel padding
ARROW_W, ARROW_H = 240000, 220000

# Panel plates -------------------------------------------------------------
PA = (100000, 80000, 11992000, 2480000)          # (a) top band
BOT_Y, BOT_H = 2680000, 4120000
PW = (11992000 - 2 * 110000) / 3.0               # three equal columns
PB = (100000, BOT_Y, PW, BOT_H)
PC = (100000 + PW + 110000, BOT_Y, PW, BOT_H)
PD = (100000 + 2 * (PW + 110000), BOT_Y, PW, BOT_H)
PANELS = {"a": PA, "b": PB, "c": PC, "d": PD}


def content(panel):
    x, y, w, h = panel
    return x + PAD, x + w - PAD, w - 2 * PAD, x + w / 2.0


# Shared vertical rhythm of panels (b)(c)(d)
Y_LABEL, H_LABEL = 2720000, 680000
Y_ARROW1 = 3420000
Y_BOX, H_BOX = 3660000, 740000
Y_ARROW2 = 4420000
Y_IMG, H_IMG = 4670000, 1770000
Y_CAP, H_CAP = 6460000, 320000

# --------------------------------------------------------------- templates
box_tpl = by_id[39]        # white rectangle, black outline
arrow_tpl = by_id[43]      # right arrow
text_tpl = by_id[29]       # plain text box


def plate(letter):
    """Rounded grey plate behind one panel (inserted at the bottom of z-order)."""
    x, y, w, h = PANELS[letter]
    sp = clone(box_tpl, "panel %s plate" % letter, at=2)
    set_xfrm(sp, x, y, w, h)
    spPr = sp.find(p("spPr"))
    geom = spPr.find(a("prstGeom"))
    geom.set("prst", "roundRect")
    av = geom.find(a("avLst"))
    for g in list(av):
        av.remove(g)
    ET.SubElement(av, a("gd"), {"name": "adj", "fmla": "val 6000"})
    for el in list(spPr):
        if el.tag in (a("solidFill"), a("noFill"), a("ln")):
            spPr.remove(el)
    fill = ET.SubElement(spPr, a("solidFill"))
    ET.SubElement(fill, a("srgbClr"), {"val": PANEL_FILL})
    ln = ET.SubElement(spPr, a("ln"))
    ET.SubElement(ln, a("noFill"))
    set_text(sp, [[(" ", BODY_SZ, False)]])
    return sp


def tag(letter):
    x, y, _, _ = PANELS[letter]
    sp = clone(box_tpl, "panel tag %s" % letter)
    set_xfrm(sp, x + 70000, y + 40000, TAG_W, TAG_H)
    spPr = sp.find(p("spPr"))
    for el in list(spPr):
        if el.tag in (a("solidFill"), a("noFill"), a("ln")):
            spPr.remove(el)
    ET.SubElement(spPr, a("noFill"))
    ln = ET.SubElement(spPr, a("ln"))
    ET.SubElement(ln, a("noFill"))
    set_text(sp, [[("(%s)" % letter, TAG_SZ, True)]], align="l")
    return sp


def textbox(name, x, y, cx, cy, lines, align="ctr"):
    sp = clone(text_tpl, name)
    set_xfrm(sp, x, y, cx, cy)
    return set_text(sp, lines, align)


def modelbox(name, x, y, cx, cy, lines):
    sp = clone(box_tpl, name)
    set_xfrm(sp, x, y, cx, cy)
    return set_text(sp, lines)


def right_arrow(name, x, y, cx, cy):
    return set_xfrm(clone(arrow_tpl, name), x, y, cx, cy)


def down_arrow(name, x, y, w, h):
    """Visual bbox (x, y, w, h); a right arrow given a quarter turn."""
    sp = clone(arrow_tpl, name)
    return set_xfrm(sp, x + w / 2.0 - h / 2.0, y + h / 2.0 - w / 2.0,
                    h, w, rot=5400000)


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


# =================================================== plates and panel tags
for k in ("a", "b", "c", "d"):
    plate(k)
for k in ("a", "b", "c", "d"):
    tag(k)

# =================================================== (a) data generation
# time-lapse photo
PHOTO_W = 2400000
AR_PHOTO = 4318000 / 2476500.0
textbox("a photo label", 700000, 140000, PHOTO_W, 380000,
        [[("Time-lapse imagery", LABEL_SZ, False)],
         [("(2011–2021)", LABEL_SZ, False)]])
set_xfrm(by_id[28], 700000, 560000, PHOTO_W, PHOTO_W / AR_PHOTO)

# processing arrow, horizontal, centred on the photo
textbox("a process label", 3200000, 540000, 1400000, 560000,
        [[("align,", LABEL_SZ, False)],
         [("classify,", LABEL_SZ, False)],
         [("geo-correct", LABEL_SZ, False)]])
set_xfrm(by_id[43], 3200000, 560000 + PHOTO_W / AR_PHOTO / 2 - 140000,
         1400000, 280000)

# product column: vegetation maps over snowmelt DOY, centred on one axis
AR_VEG = 5560517 / 2074103.0
AR_SNOW = 3934691 / 2295236.0
PROD_H, PROD_CX = 750000, 6000000
textbox("a veg label", PROD_CX - 1300000, 140000, 2600000, 340000,
        [[("Vegetation maps (2012, 2021)", LABEL_SZ, False)]])
set_xfrm(by_id[12], PROD_CX - PROD_H * AR_VEG / 2, 500000,
         PROD_H * AR_VEG, PROD_H)
textbox("a snow label", PROD_CX - 1300000, 1310000, 2600000, 340000,
        [[("Snowmelt DOY (2011–2021)", LABEL_SZ, False)]])
set_xfrm(by_id[10], PROD_CX - PROD_H * AR_SNOW / 2, 1670000,
         PROD_H * AR_SNOW, PROD_H)

# terrain, from the DEM rather than the camera
set_xfrm(by_id[35], 7700000, 1000000, 4100000, 800000)
set_text(by_id[35], [[("Terrain (DEM)", HEAD_SZ, True)],
                     [("elevation, slope, TPI, TWI, aspect", BODY_SZ, False)]])

# =================================================== (b)(c)(d) columns
BX1, BX2, BW, BCX = content(PB)
CX1, CX2, CW, CCX = content(PC)
DX1, DX2, DW, DCX = content(PD)

TAGX_B = PB[0] + 70000 + TAG_W + 60000
TAGX_C = PC[0] + 70000 + TAG_W + 60000
TAGX_D = PD[0] + 70000 + TAG_W + 60000

# --- (b) Model A ----------------------------------------------------------
textbox("b input label", TAGX_B, Y_LABEL, BX2 - TAGX_B, H_LABEL,
        [[("2021 presence + environment", LABEL_SZ, False)]])
down_arrow("b input arrow", BCX - ARROW_W / 2.0, Y_ARROW1, ARROW_W, ARROW_H)
set_xfrm(by_id[39], BX1, Y_BOX, BW, H_BOX)
set_text(by_id[39], [[("Model A: habitat suitability", HEAD_SZ, True)],
                     [("environmental potential (seed layer)", BODY_SZ, False)]])
down_arrow("b output arrow", BCX - ARROW_W / 2.0, Y_ARROW2, ARROW_W, ARROW_H)
picture("b suitability thumbnail", "rId7",
        BCX - H_IMG * AR_THUMB / 2.0, Y_IMG, H_IMG * AR_THUMB, H_IMG)
textbox("b caption", BX1, Y_CAP, BW, H_CAP,
        [[("Habitat suitability surface", CAP_SZ, False)]])

# --- (c) Model B ----------------------------------------------------------
textbox("c input label", TAGX_C, Y_LABEL, CX2 - TAGX_C, H_LABEL,
        [[("2012→2021 transitions + distance to front", LABEL_SZ, False)],
         [("+ environment", LABEL_SZ, False)]])
down_arrow("c input arrow", CCX - ARROW_W / 2.0, Y_ARROW1, ARROW_W, ARROW_H)
modelbox("c Model B box", CX1, Y_BOX, CW, H_BOX,
         [[("Model B: establishment", HEAD_SZ, True)],
          [("front distance + environment (rhizome layer)", BODY_SZ, False)]])
down_arrow("c output arrow", CCX - ARROW_W / 2.0, Y_ARROW2, ARROW_W, ARROW_H)
picture("c Model B schematic", "rId8",
        CCX - H_IMG * AR_SCHEM / 2.0, Y_IMG, H_IMG * AR_SCHEM, H_IMG)
textbox("c caption", CX1, Y_CAP, CW, H_CAP,
        [[("Establishment hugs the 2012 front", CAP_SZ, False)]])

# --- (d) Projection -------------------------------------------------------
D_COL1_W = 850000
D_COL1_X = TAGX_D
D_COL2_X = D_COL1_X + D_COL1_W + 60000
textbox("d input label", D_COL1_X, Y_LABEL, D_COL1_W, H_LABEL,
        [[("Model B applied annually", LABEL_SZ, False)]])
_scen = modelbox("d scenarios box", D_COL2_X, Y_LABEL, DX2 - D_COL2_X, H_LABEL,
         [[("Snowmelt scenarios", 1400, True)],
          [("s = 0, −0.71, −2.24 days/year", BODY_SZ, False)]])
_bp = _scen.find(p("txBody")).find(a("bodyPr"))
_bp.set("lIns", "20000"); _bp.set("rIns", "20000")
_bp.set("tIns", "20000"); _bp.set("bIns", "20000")
down_arrow("d model arrow", D_COL1_X + D_COL1_W / 2.0 - ARROW_W / 2.0,
           Y_ARROW1, ARROW_W, ARROW_H)
down_arrow("d scenario arrow",
           (D_COL2_X + DX2) / 2.0 - ARROW_W / 2.0, Y_ARROW1, ARROW_W, ARROW_H)
modelbox("d projection box", DX1, Y_BOX, DW, H_BOX,
         [[("Projection: cellular automaton", HEAD_SZ, True)],
          [("annual establishment trial, 2022–2030", BODY_SZ, False)]])
down_arrow("d output arrow", DCX - ARROW_W / 2.0, Y_ARROW2, ARROW_W, ARROW_H)
set_xfrm(by_id[48], DCX - H_IMG * AR_THUMB / 2.0, Y_IMG,
         H_IMG * AR_THUMB, H_IMG)
set_xfrm(by_id[49], DX1, Y_CAP, DW, H_CAP)
set_text(by_id[49], [[("P(established by 2030), s = 0", CAP_SZ, False)]])

# --------------------------------------------------------------- deletions
drop(33)                                   # duplicate vegetation-map label
drop(50)                                   # red frame from the old TBM panel
for i in (29, 30, 31, 34, 36, 37, 38, 40, 41, 42, 44, 45, 46):
    drop(i)                                # old labels, boxes and arrows

# ------------------------------------------------------------------- write
slide_xml = ET.tostring(slide, encoding="UTF-8", xml_declaration=True)
rels = zin.read("ppt/slides/_rels/slide1.xml.rels").decode("utf-8")
add = ('<Relationship Id="rId7" Type="http://schemas.openxmlformats.org/'
       'officeDocument/2006/relationships/image" Target="../media/image5.png"/>'
       '<Relationship Id="rId8" Type="http://schemas.openxmlformats.org/'
       'officeDocument/2006/relationships/image" Target="../media/image6.png"/>')
rels = rels.replace("</Relationships>", add + "</Relationships>")

blob = lambda n: open(os.path.join(ASSETS, n), "rb").read()
replace = {"ppt/slides/slide1.xml": slide_xml,
           "ppt/slides/_rels/slide1.xml.rels": rels.encode("utf-8"),
           "ppt/media/image4.png": blob("thumb_pcol2030.png")}
extra = {"ppt/media/image5.png": blob("thumb_suitability.png"),
         "ppt/media/image6.png": blob("schematic_modelB.png")}

# Theme fonts in the source deck are Yu Gothic, which headless LibreOffice cannot
# resolve; use Arial so the exported PNG matches the sans-serif figure fonts.
def _fix_theme_fonts(name, data):
    if name.startswith("ppt/theme/") and name.endswith(".xml"):
        return data.replace("游ゴシック Light".encode("utf-8"), b"Arial").replace(
            "游ゴシック".encode("utf-8"), b"Arial")
    return data

with zipfile.ZipFile(DST, "w", zipfile.ZIP_DEFLATED) as zout:
    for item in zin.infolist():
        zout.writestr(item, _fix_theme_fonts(item.filename,
                      replace.get(item.filename, zin.read(item.filename))))
    for name, data in extra.items():
        zout.writestr(name, data)
zin.close()
print("wrote", DST)

# ------------------------------------------------------------ verification
import defusedxml.minidom as dmd

z = zipfile.ZipFile(DST)
print("zip integrity:", "OK" if z.testzip() is None else "BAD")
dmd.parseString(z.read("ppt/slides/slide1.xml"))
print("slide1.xml well-formed: OK")
print("thumb aspect %.3f, schematic aspect %.3f, slide->print scale %.3f"
      % (AR_THUMB, AR_SCHEM, SCALE))

s = ET.fromstring(z.read("ppt/slides/slide1.xml"))
t2 = s.find(p("cSld")).find(p("spTree"))
rows = []
for ch in t2:
    if ch.tag not in (p("sp"), p("pic")):
        continue
    nv = ch.find(".//" + p("cNvPr"))
    txt = " / ".join("".join(e.text or "" for e in par.iter(a("t")))
                     for par in ch.iter(a("p"))).strip()
    rows.append((bbox(ch), nv.get("name"), txt))
rows.sort(key=lambda v: (v[0][1], v[0][0]))

print("\n%-9s %-9s %-9s %-9s  %-26s %s" % ("x1", "y1", "x2", "y2", "name", "text"))
for (x1, y1, x2, y2), n, txt in rows:
    print("%-9d %-9d %-9d %-9d  %-26s %s" % (x1, y1, x2, y2, n[:26], txt[:52]))

plates = {n: b for b, n, _ in rows if n.endswith("plate")}
print("\ncontainment (every element inside a panel plate):")
outside = [(n, b) for b, n, _ in rows if not n.endswith("plate") and
           not any(q[0] <= b[0] and q[1] <= b[1] and b[2] <= q[2] and b[3] <= q[3]
                   for q in plates.values())]
print("  all inside" if not outside else
      "\n".join("  OUTSIDE %s %s" % (n, b) for n, b in outside))


def hit(b1, b2):
    return not (b1[2] <= b2[0] or b2[2] <= b1[0] or
                b1[3] <= b2[1] or b2[3] <= b1[1])


print("\noverlaps between elements (plates excluded):")
els = [row for row in rows if not row[1].endswith("plate")]
found = 0
for i in range(len(els)):
    for j in range(i + 1, len(els)):
        if hit(els[i][0], els[j][0]):
            found += 1
            print("  %s x %s" % (els[i][1], els[j][1]))
if not found:
    print("  none")

alltxt = " ".join(row[2] for row in rows)
print("\n'invasion' present:", "invasion" in alltxt.lower())
sizes = sorted({int(e.get("sz")) for e in s.iter(a("rPr")) if e.get("sz")})
print("font sizes (1/100 pt):", sizes,
      "-> printed", ["%.1f pt" % (v / 100.0 * SCALE) for v in sizes])
