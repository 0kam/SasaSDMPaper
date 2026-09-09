# Patch spec 3: WP4 parallel workers die in full mode

Observed: full run of `analysis/05_ca_projection.R` died at the first projection set:
`mclapply` error 「200 個の並列関数呼び出しで結果が得られませんでした」 (all 200 calls
returned no result) right after "Projection s0: running 200 replicates on 10 worker(s)".
SMOKE (5 replicates) passes. The pilot replicate timed fine (projected main set 0.11 h).

## Diagnose FIRST, then fix

Reproduce cheaply before changing anything: run the projection path with ~20 replicates
and 10 workers on the full grid (a few minutes). Two prime suspects, in order:

1. **Forked children touching parent-created terra/GDAL objects.** SpatRaster is an
   external pointer; using one inside `parallel::mclapply` fork children is unsafe and
   kills workers silently. Audit the worker closure: EVERY object it touches must be a
   plain R vector/matrix/list created before the fork, or created fresh INSIDE the
   child. If `sasa_distance()` (terra-based) runs inside workers, either
   (a) rebuild its input SpatRaster inside the child from plain values + grid constants
   (ext/res/crs as numbers/strings from config, `terra::rast()` called in-child), and
   confirm terra is fork-safe when ALL its objects are child-local; or
   (b) switch the replicate parallelism to a PSOCK cluster (`parallel::makeCluster`)
   with explicit `clusterExport` of plain-data objects only, loading terra in each
   worker. PSOCK is the safer default on macOS — prefer it unless it measurably
   doubles the runtime.
2. **Memory: 10 forks × large per-worker allocations.** Check RSS of a single worker;
   if the total projects past ~20 GB, cap workers accordingly (config constant).

If the reproduction does NOT fail at 20 replicates, scale until it does (50, 100, 200)
before concluding — do not ship a fix you could not watch working.

## Requirements

- Keep the exact-decomposition design and all outputs/schemas unchanged.
- The runtime guard logic stays; with the pilot at ~2 s/replicate the full set should
  stay well under an hour even on PSOCK.
- Worker RNG must remain reproducible: use `parallel::clusterSetRNGStream` (PSOCK) or
  `mc.set.seed` semantics (fork) so the full run is seed-stable; record which in a
  comment and in the run summary CSV.
- After the fix: run (i) SMOKE end-to-end + test_wp4, and (ii) a REDUCED FULL-MODE
  verification — 30 replicates, scenarios {0, −0.71}, full grid, 10 workers — and
  report its wall time and expected-area numbers. Do NOT run the complete 200-replicate
  set; the reviewer does that.

End with: root cause found (with the evidence), what changed, smoke + reduced-run
results.
