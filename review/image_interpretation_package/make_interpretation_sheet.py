#!/usr/bin/env python3
"""Build the blinded interpretation sheet from sample_index.csv.

Reads  : sample_index.csv (written by sample_loss_crops.py)
Writes : interpretation_sheet.csv   blinded, randomised, blank verdict columns
         sample_key.csv             blind_id -> sample_id / stratum (open AFTER scoring)
         blind/<blind_id>.png       symlinks to crops_native/<sample_id>_native.png

The interpreter is the author of the paper, so the stratum is hidden to keep
the scoring from being driven by the label under test.  Panel geometry is NOT
randomised: in every native crop the left column is 2012 and the right column
is 2021, top row green season, bottom row autumn.  The verdict vocabulary is
directional, so swapping years per sample would make the sheet unusable.

Usage: python3 make_interpretation_sheet.py [--seed 20260830]
"""
import argparse
import csv
import os
import random

HERE = os.path.dirname(os.path.abspath(__file__))

VERDICTS = "sasa_loss | canopy_overgrowth | boundary_jitter | undecidable"

# columns copied through to the sheet: everything the interpreter may legitimately
# see while scoring (viewing quality), nothing that reveals the mapped transition.
CARRY = ["dist", "m_per_px_horiz", "m2_per_px", "align_shift_px"]


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--seed", type=int, default=20260830)
    args = ap.parse_args()

    rows = list(csv.DictReader(open(os.path.join(HERE, "sample_index.csv"))))
    rng = random.Random(args.seed)
    order = rows[:]
    rng.shuffle(order)

    blinddir = os.path.join(HERE, "blind")
    os.makedirs(blinddir, exist_ok=True)
    for f in os.listdir(blinddir):
        p = os.path.join(blinddir, f)
        if os.path.islink(p):
            os.unlink(p)

    sheet, key = [], []
    for i, r in enumerate(order, start=1):
        bid = "B%03d" % i
        src = os.path.join("..", r["native_crop"])
        os.symlink(src, os.path.join(blinddir, bid + ".png"))

        rec = {"blind_id": bid, "blind_image": "blind/%s.png" % bid}
        for c in CARRY:
            v = r[c]
            try:
                rec[c] = "%.3f" % float(v)
            except (TypeError, ValueError):
                rec[c] = v
        # quality triage flag, computed from viewing geometry only
        try:
            ok = float(r["m_per_px_horiz"]) < 0.25 and float(r["align_shift_px"]) <= 2.0
        except ValueError:
            ok = False
        rec["quality_ok"] = "yes" if ok else "no"
        rec["verdict"] = ""          # one of VERDICTS
        rec["confidence"] = ""       # high / medium / low
        rec["notes"] = ""
        sheet.append(rec)
        key.append({"blind_id": bid, "sample_id": r["sample_id"],
                    "stratum": r["stratum"],
                    "cls2012": r["cls2012"], "cls2021": r["cls2021"],
                    "x": r["x"], "y": r["y"], "z": r["z"],
                    "row": r["row"], "col": r["col"],
                    "u": r["u"], "v": r["v"],
                    "annotated_figure": r["figure"],
                    "native_crop": r["native_crop"]})

    fields = (["blind_id", "blind_image"] + CARRY +
              ["quality_ok", "verdict", "confidence", "notes"])
    with open(os.path.join(HERE, "interpretation_sheet.csv"), "w", newline="") as fh:
        w = csv.DictWriter(fh, fieldnames=fields)
        w.writeheader()   # legend for verdict/confidence lives in README.md
        for r in sheet:
            w.writerow(r)

    with open(os.path.join(HERE, "sample_key.csv"), "w", newline="") as fh:
        w = csv.DictWriter(fh, fieldnames=list(key[0].keys()))
        w.writeheader()
        for r in key:
            w.writerow(r)

    print("wrote interpretation_sheet.csv (%d rows), sample_key.csv, blind/"
          % len(sheet))


if __name__ == "__main__":
    main()
