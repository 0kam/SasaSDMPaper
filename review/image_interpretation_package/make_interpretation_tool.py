#!/usr/bin/env python3
"""Generate a self-contained, offline (file://) HTML tool for the blind visual
interpretation of the Sasa-loss sample crops.

The sample list (blind_id + image path + the metadata columns needed to
reproduce interpretation_sheet.csv on export) is read from
interpretation_sheet.csv and embedded into the HTML as JSON, because most
browsers refuse fetch() on file:// URLs.

Read-only with respect to every existing file; the only file written is
interpretation_tool.html (or --out).

Usage:
    python3 make_interpretation_tool.py
    python3 make_interpretation_tool.py --sheet interpretation_sheet.csv --out interpretation_tool.html
"""

from __future__ import annotations

import argparse
import csv
import json
from pathlib import Path

# Columns the tool fills in; every other column of the sheet is carried through
# untouched so that the exported CSV has the same schema as the input sheet.
INPUT_COLS = ("verdict", "confidence", "notes")

HTML_TEMPLATE = r"""<!DOCTYPE html>
<html lang="ja">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>ササ減少セル 目視判読ツール</title>
<style>
  :root {
    --bg: #14161a;
    --panel: #1e2229;
    --panel-2: #262b34;
    --line: #363c47;
    --fg: #e8eaed;
    --muted: #9aa3af;
    --accent: #4a9eff;
    --ok: #35c07a;
    --warn: #e0a33e;
  }
  * { box-sizing: border-box; }
  html, body {
    margin: 0; height: 100%;
    background: var(--bg); color: var(--fg);
    font-family: -apple-system, BlinkMacSystemFont, "Hiragino Sans",
                 "Yu Gothic UI", "Noto Sans JP", sans-serif;
    font-size: 14px; overflow: hidden;
  }
  #app { display: flex; height: 100%; }

  /* ---- image stage ---- */
  #stage {
    flex: 1 1 auto; position: relative; overflow: hidden;
    background: #0c0d10; cursor: grab; touch-action: none;
  }
  #stage.dragging { cursor: grabbing; }
  #canvas {
    position: absolute; top: 0; left: 0;
    transform-origin: 0 0; will-change: transform;
  }
  #canvas img {
    display: block; user-select: none; -webkit-user-drag: none;
    image-rendering: pixelated;
  }
  #stage-hud {
    position: absolute; left: 12px; top: 12px; z-index: 5;
    background: rgba(20,22,26,.82); border: 1px solid var(--line);
    border-radius: 8px; padding: 6px 10px;
    font-variant-numeric: tabular-nums; pointer-events: none;
  }
  #stage-hud b { font-size: 18px; letter-spacing: .04em; }
  #stage-hud span { color: var(--muted); margin-left: 10px; }
  #zoombar {
    position: absolute; right: 12px; top: 12px; z-index: 5; display: flex; gap: 6px;
  }
  #zoombar button {
    width: 34px; height: 34px; font-size: 16px; padding: 0;
  }

  /* ---- side panel ---- */
  #side {
    flex: 0 0 340px; background: var(--panel); border-left: 1px solid var(--line);
    display: flex; flex-direction: column; overflow-y: auto;
  }
  .sec { padding: 12px 14px; border-bottom: 1px solid var(--line); }
  .sec h2 {
    margin: 0 0 8px; font-size: 11px; font-weight: 600;
    letter-spacing: .12em; color: var(--muted); text-transform: uppercase;
  }
  button {
    font: inherit; color: var(--fg); background: var(--panel-2);
    border: 1px solid var(--line); border-radius: 6px;
    padding: 7px 10px; cursor: pointer;
  }
  button:hover { border-color: #566; }
  button:active { transform: translateY(1px); }
  .choice {
    display: flex; width: 100%; align-items: baseline; gap: 8px;
    margin-bottom: 6px; text-align: left;
  }
  .choice .k {
    flex: 0 0 auto; font-size: 11px; color: var(--muted);
    border: 1px solid var(--line); border-radius: 4px; padding: 1px 5px;
  }
  .choice.on { background: #23405e; border-color: var(--accent); }
  .choice.on .k { color: var(--fg); border-color: var(--accent); }
  .row { display: flex; gap: 6px; }
  .row button { flex: 1 1 0; }
  #notes {
    width: 100%; min-height: 80px; resize: vertical;
    font: inherit; color: var(--fg); background: var(--panel-2);
    border: 1px solid var(--line); border-radius: 6px; padding: 7px 8px;
  }
  #notes:focus, button:focus-visible { outline: 2px solid var(--accent); outline-offset: 1px; }

  /* ---- progress ---- */
  #bar {
    height: 8px; background: var(--panel-2); border-radius: 4px; overflow: hidden;
    margin-bottom: 8px;
  }
  #bar > i { display: block; height: 100%; width: 0; background: var(--ok); }
  table.tally { width: 100%; border-collapse: collapse; font-variant-numeric: tabular-nums; }
  table.tally td { padding: 2px 0; }
  table.tally td:last-child { text-align: right; color: var(--muted); }

  .hint { color: var(--muted); font-size: 12px; line-height: 1.7; }
  .hint kbd {
    background: var(--panel-2); border: 1px solid var(--line); border-bottom-width: 2px;
    border-radius: 4px; padding: 0 5px; font-family: ui-monospace, monospace; font-size: 11px;
  }
  label.toggle { display: flex; align-items: center; gap: 8px; cursor: pointer; }
  #jump { width: 100%; }
  #msg { color: var(--warn); font-size: 12px; min-height: 1.4em; }
  .muted { color: var(--muted); }
</style>
</head>
<body>
<div id="app">
  <div id="stage">
    <div id="canvas"><img id="img" alt=""></div>
    <div id="stage-hud"><b id="hud-id">—</b><span id="hud-pos"></span><span id="hud-zoom"></span></div>
    <div id="zoombar">
      <button id="zoom-out" title="縮小">−</button>
      <button id="zoom-in" title="拡大">＋</button>
      <button id="zoom-fit" title="全体表示にリセット (0)">⤢</button>
    </div>
  </div>

  <div id="side">
    <div class="sec">
      <h2>進捗</h2>
      <div id="bar"><i></i></div>
      <div id="progress-text" class="hint"></div>
      <table class="tally"><tbody id="tally"></tbody></table>
    </div>

    <div class="sec">
      <h2>判定 (verdict)</h2>
      <div id="verdicts"></div>
    </div>

    <div class="sec">
      <h2>確信度 (confidence)</h2>
      <div id="confidences"></div>
    </div>

    <div class="sec">
      <h2>メモ (notes)</h2>
      <textarea id="notes" placeholder="自由記述（任意）"></textarea>
    </div>

    <div class="sec">
      <h2>移動</h2>
      <div class="row" style="margin-bottom:8px">
        <button id="prev">← 前へ</button>
        <button id="next">次へ →</button>
      </div>
      <div class="row" style="margin-bottom:8px">
        <button id="next-unfilled">未入力へ飛ぶ</button>
      </div>
      <select id="jump"></select>
      <div style="margin-top:8px">
        <label class="toggle">
          <input type="checkbox" id="autoadvance" checked>
          <span>判定を入力したら自動で次へ</span>
        </label>
        <label class="toggle" style="margin-top:6px">
          <input type="checkbox" id="overlay" checked>
          <span>対象セルの枠を表示（<kbd>o</kbd>）</span>
        </label>
      </div>
    </div>

    <div class="sec">
      <h2>保存・書き出し</h2>
      <div class="row" style="margin-bottom:8px">
        <button id="export">CSV を保存</button>
        <button id="import-btn">CSV を読み込む</button>
      </div>
      <input type="file" id="import-file" accept=".csv,text/csv" style="display:none">
      <button id="reset-all" style="width:100%">入力をすべて消去</button>
      <div id="msg" style="margin-top:8px"></div>
    </div>

    <div class="sec">
      <h2>キーボード操作</h2>
      <div class="hint" id="shortcuts"></div>
    </div>
  </div>
</div>

<script>
"use strict";

/* ------------------------------------------------------------------ data */
const SAMPLES  = __SAMPLES_JSON__;      // [{blind_id, image, extra:{...}}, ...]
const COLUMNS  = __COLUMNS_JSON__;      // exact column order of the source sheet
const ID_COL   = __ID_COL_JSON__;
const IMG_COL  = __IMG_COL_JSON__;
const STORE_KEY = "sasa_interpretation_v1:" + SAMPLES.length + ":" +
                  (SAMPLES[0] ? SAMPLES[0].blind_id : "") + "-" +
                  (SAMPLES[SAMPLES.length - 1] ? SAMPLES[SAMPLES.length - 1].blind_id : "");

const VERDICTS = [
  { key: "1", value: "sasa_loss",          label: "ササが実際に消えた" },
  { key: "2", value: "canopy_overgrowth",  label: "木本の被覆で見えなくなった" },
  { key: "3", value: "boundary_jitter",    label: "境界のゆらぎ・分類の誤り" },
  { key: "4", value: "undecidable",        label: "判定不能" },
];
const CONFIDENCES = [
  { key: "q", value: "high",   label: "高" },
  { key: "w", value: "medium", label: "中" },
  { key: "e", value: "low",    label: "低" },
];

/* ------------------------------------------------------------------ state */
/* The overlay variant of each crop lives beside the plain one, with the same
   file name, under blind_overlay/.  It marks the projected footprint of the
   1 m cell under judgement; the plain image is the same crop with no ink on
   it.  Toggling between the two is the only way to inspect pixels that the
   footprint outline covers, which on a distant cell can be most of it. */
let overlayOn = true;
function srcFor(s) {
  return overlayOn ? s.image.replace(/^blind\//, "blind_overlay/") : s.image;
}
let idx = 0;
let answers = {};   // blind_id -> {verdict, confidence, notes}

function loadState() {
  try {
    const raw = localStorage.getItem(STORE_KEY);
    if (raw) {
      const o = JSON.parse(raw);
      answers = o.answers || {};
      if (Number.isInteger(o.idx) && o.idx >= 0 && o.idx < SAMPLES.length) idx = o.idx;
      if (typeof o.autoadvance === "boolean") $("autoadvance").checked = o.autoadvance;
      if (typeof o.overlay === "boolean") { overlayOn = o.overlay; $("overlay").checked = o.overlay; }
    }
  } catch (e) { /* corrupt or unavailable storage: start fresh */ }
}
let saveTimer = null;
function saveState() {
  clearTimeout(saveTimer);
  saveTimer = setTimeout(() => {
    try {
      localStorage.setItem(STORE_KEY, JSON.stringify({
        answers, idx, autoadvance: $("autoadvance").checked,
        overlay: overlayOn,
        saved_at: new Date().toISOString(),
      }));
    } catch (e) { msg("自動保存に失敗しました（localStorage 不可）。こまめに CSV を保存してください。"); }
  }, 150);
}
function cur() { return SAMPLES[idx]; }
function ans(id) {
  if (!answers[id]) answers[id] = { verdict: "", confidence: "", notes: "" };
  return answers[id];
}
function $(id) { return document.getElementById(id); }
function msg(t) { $("msg").textContent = t || ""; }

/* ------------------------------------------------------------------ zoom/pan */
const stage = $("stage"), canvas = $("canvas"), img = $("img");
let scale = 1, tx = 0, ty = 0, natW = 0, natH = 0;
const MIN_SCALE = 0.1, MAX_SCALE = 40;

function applyTransform() {
  canvas.style.transform = "translate(" + tx + "px," + ty + "px) scale(" + scale + ")";
  img.style.imageRendering = scale > 1.5 ? "pixelated" : "auto";
  $("hud-zoom").textContent = natW ? "×" + scale.toFixed(2) : "";
}
function fit() {
  if (!natW || !natH) return;
  const r = stage.getBoundingClientRect();
  const pad = 24;
  scale = Math.min((r.width - pad) / natW, (r.height - pad) / natH);
  tx = (r.width  - natW * scale) / 2;
  ty = (r.height - natH * scale) / 2;
  applyTransform();
}
function zoomAt(cx, cy, factor) {
  const next = Math.min(MAX_SCALE, Math.max(MIN_SCALE, scale * factor));
  if (next === scale) return;
  // keep the point under (cx, cy) fixed
  tx = cx - (cx - tx) * (next / scale);
  ty = cy - (cy - ty) * (next / scale);
  scale = next;
  applyTransform();
}
function zoomCenter(factor) {
  const r = stage.getBoundingClientRect();
  zoomAt(r.width / 2, r.height / 2, factor);
}
stage.addEventListener("wheel", (e) => {
  e.preventDefault();
  const r = stage.getBoundingClientRect();
  // trackpad pinch arrives as ctrlKey wheel; both paths zoom
  const step = Math.exp(-e.deltaY * (e.deltaMode === 1 ? 0.05 : 0.0018));
  zoomAt(e.clientX - r.left, e.clientY - r.top, step);
}, { passive: false });

let panning = false, px = 0, py = 0;
stage.addEventListener("pointerdown", (e) => {
  if (e.button !== 0) return;
  panning = true; px = e.clientX; py = e.clientY;
  stage.classList.add("dragging");
  stage.setPointerCapture(e.pointerId);
});
stage.addEventListener("pointermove", (e) => {
  if (!panning) return;
  tx += e.clientX - px; ty += e.clientY - py;
  px = e.clientX; py = e.clientY;
  applyTransform();
});
function endPan(e) {
  if (!panning) return;
  panning = false; stage.classList.remove("dragging");
  try { stage.releasePointerCapture(e.pointerId); } catch (_) {}
}
stage.addEventListener("pointerup", endPan);
stage.addEventListener("pointercancel", endPan);
stage.addEventListener("dblclick", (e) => { e.preventDefault(); fit(); });
window.addEventListener("resize", () => { if (natW) fit(); });
$("zoom-in").onclick  = () => zoomCenter(1.4);
$("zoom-out").onclick = () => zoomCenter(1 / 1.4);
$("zoom-fit").onclick = fit;

/* ------------------------------------------------------------------ rendering */
const preloadCache = [];
function preload(i) {
  const s = SAMPLES[i];
  if (!s) return;
  const p = new Image();
  p.src = srcFor(s);
  preloadCache.push(p);
  if (preloadCache.length > 8) preloadCache.shift();
}

function showSample(keepView) {
  const s = cur();
  $("hud-id").textContent = s.blind_id;
  $("hud-pos").textContent = (idx + 1) + " / " + SAMPLES.length;
  const want = srcFor(s);
  if (img.getAttribute("src") !== want) {
    img.onload = () => {
      natW = img.naturalWidth; natH = img.naturalHeight;
      if (!keepView) fit(); else applyTransform();
    };
    img.onerror = () => {
      if (overlayOn && want !== s.image) {
        // blind_overlay/ missing: fall back to the plain crop rather than
        // leaving the interpreter with a blank stage
        msg("枠つき画像がありません（" + want + "）。枠なしを表示します。");
        setOverlay(false);
      } else {
        msg("画像を読み込めません: " + want);
      }
    };
    img.src = want;
  } else if (!keepView) {
    fit();
  }
  const a = ans(s.blind_id);
  document.querySelectorAll("#verdicts .choice").forEach((b) =>
    b.classList.toggle("on", b.dataset.value === a.verdict));
  document.querySelectorAll("#confidences .choice").forEach((b) =>
    b.classList.toggle("on", b.dataset.value === a.confidence));
  $("notes").value = a.notes || "";
  $("jump").value = String(idx);
  renderProgress();
  msg("");
  preload(idx + 1); preload(idx + 2); preload(idx - 1);
  saveState();
}

function renderProgress() {
  let done = 0;
  const counts = {};
  VERDICTS.forEach((v) => counts[v.value] = 0);
  SAMPLES.forEach((s) => {
    const v = answers[s.blind_id] && answers[s.blind_id].verdict;
    if (v) { done++; if (v in counts) counts[v]++; }
  });
  const pct = SAMPLES.length ? (done / SAMPLES.length * 100) : 0;
  $("bar").firstElementChild.style.width = pct.toFixed(1) + "%";
  $("progress-text").textContent =
    "入力済み " + done + " / " + SAMPLES.length + "（" + pct.toFixed(0) + "%）";
  $("tally").innerHTML = VERDICTS.map((v) =>
    "<tr><td>" + v.value + "</td><td>" + counts[v.value] + "</td></tr>").join("") +
    "<tr><td class='muted'>未入力</td><td>" + (SAMPLES.length - done) + "</td></tr>";
  // reflect completion in the jump list
  const opt = $("jump").options[idx];
  if (opt) opt.textContent = optionLabel(idx);
}

function optionLabel(i) {
  const s = SAMPLES[i];
  const a = answers[s.blind_id];
  const mark = a && a.verdict ? "✓" : "・";
  return mark + " " + s.blind_id + "  " + (a && a.verdict ? a.verdict : "");
}

/* ------------------------------------------------------------------ input */
function setVerdict(value) {
  const a = ans(cur().blind_id);
  a.verdict = (a.verdict === value) ? "" : value;
  document.querySelectorAll("#verdicts .choice").forEach((b) =>
    b.classList.toggle("on", b.dataset.value === a.verdict));
  renderProgress();
  saveState();
  if (a.verdict && $("autoadvance").checked) setTimeout(() => go(1), 120);
}
function setConfidence(value) {
  const a = ans(cur().blind_id);
  a.confidence = (a.confidence === value) ? "" : value;
  document.querySelectorAll("#confidences .choice").forEach((b) =>
    b.classList.toggle("on", b.dataset.value === a.confidence));
  saveState();
}
function go(delta) {
  const n = idx + delta;
  if (n < 0 || n >= SAMPLES.length) return;
  idx = n;
  showSample(false);
}
function gotoIndex(i, keepView) {
  if (i < 0 || i >= SAMPLES.length) return;
  idx = i; showSample(!!keepView);
}
function nextUnfilled() {
  for (let k = 1; k <= SAMPLES.length; k++) {
    const i = (idx + k) % SAMPLES.length;
    const a = answers[SAMPLES[i].blind_id];
    if (!a || !a.verdict) { gotoIndex(i); return; }
  }
  msg("未入力のサンプルはありません。");
}

$("notes").addEventListener("input", () => {
  ans(cur().blind_id).notes = $("notes").value;
  saveState();
});
$("prev").onclick = () => go(-1);
$("next").onclick = () => go(1);
$("next-unfilled").onclick = nextUnfilled;
$("jump").onchange = (e) => gotoIndex(parseInt(e.target.value, 10));
$("autoadvance").onchange = saveState;

function setOverlay(on) {
  overlayOn = !!on;
  $("overlay").checked = overlayOn;
  showSample(true);        // keepView: zoom and pan survive the swap
  saveState();
}
$("overlay").onchange = () => setOverlay($("overlay").checked);

document.addEventListener("keydown", (e) => {
  const typing = e.target === $("notes");
  if (typing) {
    if (e.key === "Escape") { $("notes").blur(); e.preventDefault(); }
    return;
  }
  if (e.metaKey || e.ctrlKey || e.altKey) return;
  const k = e.key.toLowerCase();
  const v = VERDICTS.find((x) => x.key === k);
  if (v) { setVerdict(v.value); e.preventDefault(); return; }
  const c = CONFIDENCES.find((x) => x.key === k);
  if (c) { setConfidence(c.value); e.preventDefault(); return; }
  if (e.key === "ArrowRight" || k === "j" || k === " ") { go(1);  e.preventDefault(); return; }
  if (e.key === "ArrowLeft"  || k === "k")               { go(-1); e.preventDefault(); return; }
  if (k === "o") { setOverlay(!overlayOn); e.preventDefault(); return; }
  if (k === "n") { $("notes").focus(); e.preventDefault(); return; }
  if (k === "u") { nextUnfilled(); e.preventDefault(); return; }
  if (k === "0") { fit(); e.preventDefault(); return; }
  if (k === "+" || k === "=") { zoomCenter(1.4); e.preventDefault(); return; }
  if (k === "-") { zoomCenter(1 / 1.4); e.preventDefault(); return; }
});

/* ------------------------------------------------------------------ CSV */
function csvEscape(s) {
  s = (s === null || s === undefined) ? "" : String(s);
  return /[",\r\n]/.test(s) ? '"' + s.replace(/"/g, '""') + '"' : s;
}
function buildCSV() {
  const lines = [COLUMNS.map(csvEscape).join(",")];
  SAMPLES.forEach((s) => {
    const a = answers[s.blind_id] || {};
    lines.push(COLUMNS.map((c) => {
      if (c === "verdict")    return csvEscape(a.verdict || "");
      if (c === "confidence") return csvEscape(a.confidence || "");
      if (c === "notes")      return csvEscape(a.notes || "");
      return csvEscape(s.extra[c]);
    }).join(","));
  });
  return lines.join("\r\n") + "\r\n";
}
$("export").onclick = () => {
  const blob = new Blob(["﻿" + buildCSV()], { type: "text/csv;charset=utf-8" });
  const url = URL.createObjectURL(blob);
  const a = document.createElement("a");
  const stamp = new Date().toISOString().slice(0, 16).replace(/[-:T]/g, "");
  a.href = url;
  a.download = "interpretation_sheet_filled_" + stamp + ".csv";
  document.body.appendChild(a); a.click(); a.remove();
  setTimeout(() => URL.revokeObjectURL(url), 2000);
  msg("CSV をダウンロードしました。");
};

// RFC4180-ish parser (handles quotes, embedded commas and newlines)
function parseCSV(text) {
  text = text.replace(/^﻿/, "");
  const rows = []; let row = []; let field = ""; let q = false;
  for (let i = 0; i < text.length; i++) {
    const ch = text[i];
    if (q) {
      if (ch === '"') {
        if (text[i + 1] === '"') { field += '"'; i++; } else { q = false; }
      } else field += ch;
    } else if (ch === '"') { q = true; }
    else if (ch === ",") { row.push(field); field = ""; }
    else if (ch === "\n") { row.push(field); field = ""; rows.push(row); row = []; }
    else if (ch === "\r") { /* skip */ }
    else field += ch;
  }
  if (field.length || row.length) { row.push(field); rows.push(row); }
  return rows.filter((r) => r.length > 1 || (r[0] || "").trim() !== "");
}
$("import-btn").onclick = () => $("import-file").click();
$("import-file").onchange = (e) => {
  const f = e.target.files && e.target.files[0];
  if (!f) return;
  const r = new FileReader();
  r.onload = () => {
    try {
      const rows = parseCSV(String(r.result));
      if (!rows.length) { msg("空の CSV です。"); return; }
      const head = rows[0].map((h) => h.trim());
      const ci = {
        id: head.indexOf(ID_COL),
        verdict: head.indexOf("verdict"),
        confidence: head.indexOf("confidence"),
        notes: head.indexOf("notes"),
      };
      if (ci.id < 0) { msg("列 " + ID_COL + " が見つかりません。"); return; }
      const known = new Set(SAMPLES.map((s) => s.blind_id));
      let n = 0, skipped = 0;
      rows.slice(1).forEach((row) => {
        const id = (row[ci.id] || "").trim();
        if (!known.has(id)) { if (id) skipped++; return; }
        const a = ans(id);
        if (ci.verdict    >= 0) a.verdict    = (row[ci.verdict]    || "").trim();
        if (ci.confidence >= 0) a.confidence = (row[ci.confidence] || "").trim();
        if (ci.notes      >= 0) a.notes      = row[ci.notes] || "";
        n++;
      });
      showSample(true);
      msg("読み込み完了: " + n + " 行" + (skipped ? "（未知の ID " + skipped + " 行は無視）" : ""));
    } catch (err) {
      msg("読み込みに失敗しました: " + err);
    }
    e.target.value = "";
  };
  r.readAsText(f, "utf-8");
};
$("reset-all").onclick = () => {
  if (!confirm("入力した判定・確信度・メモをすべて消去します。よろしいですか？")) return;
  answers = {};
  try { localStorage.removeItem(STORE_KEY); } catch (_) {}
  rebuildJumpList();
  showSample(true);
  msg("入力を消去しました。");
};

/* ------------------------------------------------------------------ build UI */
function rebuildJumpList() {
  const sel = $("jump");
  sel.innerHTML = SAMPLES.map((s, i) =>
    '<option value="' + i + '">' + optionLabel(i) + "</option>").join("");
  sel.value = String(idx);
}
function buildButtons() {
  $("verdicts").innerHTML = VERDICTS.map((v) =>
    '<button class="choice" data-value="' + v.value + '">' +
      '<span class="k">' + v.key + "</span>" +
      "<span><b>" + v.label + "</b><br><span class='muted'>" + v.value + "</span></span>" +
    "</button>").join("");
  $("confidences").innerHTML =
    '<div class="row">' + CONFIDENCES.map((c) =>
      '<button class="choice" style="justify-content:center" data-value="' + c.value + '">' +
        '<span class="k">' + c.key + "</span><span>" + c.label + "</span>" +
      "</button>").join("") + "</div>";
  document.querySelectorAll("#verdicts .choice").forEach((b) =>
    b.onclick = () => setVerdict(b.dataset.value));
  document.querySelectorAll("#confidences .choice").forEach((b) =>
    b.onclick = () => setConfidence(b.dataset.value));

  $("shortcuts").innerHTML = [
    "<kbd>1</kbd>–<kbd>4</kbd> 判定（もう一度押すと取り消し）",
    "<kbd>q</kbd>/<kbd>w</kbd>/<kbd>e</kbd> 確信度 高/中/低",
    "<kbd>→</kbd> または <kbd>Space</kbd>/<kbd>j</kbd> 次へ",
    "<kbd>←</kbd> または <kbd>k</kbd> 前へ",
    "<kbd>u</kbd> 次の未入力へ",
    "<kbd>o</kbd> 対象セルの枠を表示 / 非表示",
    "<kbd>n</kbd> メモ欄へ / <kbd>Esc</kbd> メモ欄を抜ける",
    "<kbd>0</kbd> ズームをリセット（ダブルクリックも同じ）",
    "<kbd>+</kbd>/<kbd>-</kbd> 拡大 / 縮小",
    "ホイールで拡大縮小、ドラッグで移動",
  ].join("<br>");
}

buildButtons();
loadState();
rebuildJumpList();
showSample(false);
window.addEventListener("beforeunload", () => { clearTimeout(saveTimer); saveState(); });
</script>
</body>
</html>
"""


def main() -> None:
    here = Path(__file__).resolve().parent
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--sheet", default=str(here / "interpretation_sheet.csv"),
                    help="source sheet defining the sample list and column order")
    ap.add_argument("--out", default=str(here / "interpretation_tool.html"),
                    help="output HTML file")
    ap.add_argument("--id-col", default="blind_id")
    ap.add_argument("--image-col", default="blind_image")
    args = ap.parse_args()

    sheet = Path(args.sheet)
    with sheet.open(newline="", encoding="utf-8") as fh:
        reader = csv.DictReader(fh)
        columns = list(reader.fieldnames or [])
        rows = list(reader)

    for required in (args.id_col, args.image_col):
        if required not in columns:
            raise SystemExit(f"column '{required}' not found in {sheet}")
    for c in INPUT_COLS:
        if c not in columns:
            raise SystemExit(f"column '{c}' not found in {sheet}")

    samples = []
    for r in rows:
        blind_id = (r.get(args.id_col) or "").strip()
        if not blind_id:
            continue
        # Deliberately blind: only the id and the image path reach the UI.
        # Every other column is carried in `extra` solely to reproduce the
        # sheet on export; the HTML never renders it.
        extra = {c: (r.get(c) or "") for c in columns if c not in INPUT_COLS}
        samples.append({
            "blind_id": blind_id,
            "image": (r.get(args.image_col) or "").strip(),
            "extra": extra,
        })

    if not samples:
        raise SystemExit(f"no samples found in {sheet}")

    def emb(obj) -> str:
        # </script> can never appear inside the embedded JSON
        return json.dumps(obj, ensure_ascii=False).replace("</", "<\\/")

    html = (HTML_TEMPLATE
            .replace("__SAMPLES_JSON__", emb(samples))
            .replace("__COLUMNS_JSON__", emb(columns))
            .replace("__ID_COL_JSON__", emb(args.id_col))
            .replace("__IMG_COL_JSON__", emb(args.image_col)))

    out = Path(args.out)
    out.write_text(html, encoding="utf-8")
    print(f"wrote {out}  ({len(samples)} samples, {len(html):,} bytes)")


if __name__ == "__main__":
    main()
