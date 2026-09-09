# -----------------------------------------------------------------------------
# Spatial plausibility check for Sasa -> Pinus pumila (dwarf pine) transitions
#
# Question: are 2012-Sasa / 2021-Pine cells adjacent to pre-existing (2012) dwarf
# pine, as required by the manuscript's interpretation (lateral branch growth or
# boundary mixed-pixel classification noise)?  Annual shoot elongation of
# P. pumila is ~4-5 cm, so over 9 years the canopy edge can advance ~0.5 m only.
#
# Inputs : ortho/data/vege_2012_5x5.tiff, ortho/data/vege_2021_5x5.tiff
#          (1 m grid, EPSG:6690; class 1 = Sasa, 7 = Pinus pumila)
# Outputs: review/sasa_to_pine_adjacency/*.csv, *.png
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(terra)
})

REPO   <- "/Users/okamoto/NIES/SasaSDMPaper"
OUTDIR <- file.path(REPO, "review", "sasa_to_pine_adjacency")
dir.create(OUTDIR, showWarnings = FALSE, recursive = TRUE)

CLASS_LABELS <- c(
  "1" = "Sasa", "2" = "Other vegetation", "3" = "Non-vegetated",
  "4" = "Sorbus", "5" = "Acer", "6" = "Alnus", "7" = "Pinus pumila"
)

v12 <- rast(file.path(REPO, "ortho", "data", "vege_2012_5x5.tiff"))
v21 <- rast(file.path(REPO, "ortho", "data", "vege_2021_5x5.tiff"))

cat("--- grid check ---\n")
print(v12); print(v21)
stopifnot(identical(dim(v12)[1:2], dim(v21)[1:2]))
stopifnot(isTRUE(all.equal(as.vector(ext(v12)), as.vector(ext(v21)))))
stopifnot(isTRUE(all.equal(res(v12), res(v21))))
cat("same CRS:", identical(crs(v12), crs(v21)), "\n")

a12 <- as.vector(values(v12))
a21 <- as.vector(values(v21))
# Treat 0 (and NA) as no-data.
a12[a12 == 0] <- NA
a21[a21 == 0] <- NA

cat("\n--- class frequencies ---\n")
print(table(a12, useNA = "ifany"))
print(table(a21, useNA = "ifany"))

# ---- 1. transition masks ----------------------------------------------------
sasa12    <- !is.na(a12) & a12 == 1
pine12    <- !is.na(a12) & a12 == 7
to_pine   <- sasa12 & !is.na(a21) & a21 == 7
stay_sasa <- sasa12 & !is.na(a21) & a21 == 1
to_other  <- sasa12 & !is.na(a21) & !(a21 %in% c(1, 7))

cat("\n--- transition counts (cells = m^2) ---\n")
cat("Sasa 2012            :", sum(sasa12), "\n")
cat("Pine 2012            :", sum(pine12), "\n")
cat("Sasa -> Pine         :", sum(to_pine), "\n")
cat("Sasa -> Sasa         :", sum(stay_sasa), "\n")
cat("Sasa -> other classes:", sum(to_other), "\n")

# ---- 2. distance to nearest 2012 dwarf-pine cell ----------------------------
# Cells with a value keep 0; NA cells get the distance to the nearest value cell.
pine_src <- rast(v12)
values(pine_src) <- ifelse(pine12, 1L, NA_integer_)
dist_pine <- distance(pine_src)
d <- as.vector(values(dist_pine))

writeRaster(dist_pine, file.path(OUTDIR, "dist_to_pine12.tif"),
            overwrite = TRUE, wopt = list(datatype = "FLT4S"))

# ---- 3. distance-bin tabulation ---------------------------------------------
BREAKS <- c(-Inf, 1.5, 3, 5, Inf)
LABELS <- c("adjacent (<=1.5 m)", "2-3 m (1.5-3 m)", "3-5 m", ">5 m (isolated)")

bin_table <- function(mask, group) {
  dd <- d[mask]
  if (length(dd) == 0) return(NULL)
  b  <- cut(dd, breaks = BREAKS, labels = LABELS, right = TRUE)
  tb <- table(b)
  data.frame(
    group      = group,
    n_total    = length(dd),
    bin        = names(tb),
    n_cells    = as.integer(tb),
    pct        = round(100 * as.integer(tb) / length(dd), 3),
    row.names  = NULL
  )
}

groups <- list(
  "Sasa -> Pine"                 = to_pine,
  "Sasa -> other (non-Sasa)"     = to_other,
  "Sasa -> Sasa (persisting)"    = stay_sasa,
  "all Sasa 2012 (baseline)"     = sasa12
)
# Per-target-class breakdown of the Sasa -> other transitions.
for (k in sort(unique(a21[to_other]))) {
  nm <- sprintf("Sasa -> %s (class %d)", CLASS_LABELS[[as.character(k)]], k)
  groups[[nm]] <- to_other & a21 == k
}

res_bins <- do.call(rbind, Map(bin_table, groups, names(groups)))
res_bins$bin <- factor(res_bins$bin, levels = LABELS)
res_bins <- res_bins[order(match(res_bins$group, names(groups)), res_bins$bin), ]
write.csv(res_bins, file.path(OUTDIR, "distance_bins.csv"), row.names = FALSE)
cat("\n--- distance bins ---\n"); print(res_bins, row.names = FALSE)

# Distance summary statistics per group.
res_stats <- do.call(rbind, lapply(names(groups), function(g) {
  dd <- d[groups[[g]]]
  data.frame(group = g, n = length(dd),
             mean = round(mean(dd), 3), sd = round(sd(dd), 3),
             min = round(min(dd), 3),
             q25 = round(quantile(dd, .25), 3),
             median = round(median(dd), 3),
             q75 = round(quantile(dd, .75), 3),
             q95 = round(quantile(dd, .95), 3),
             max = round(max(dd), 3), row.names = NULL)
}))
write.csv(res_stats, file.path(OUTDIR, "distance_summary_stats.csv"), row.names = FALSE)
cat("\n--- distance summary ---\n"); print(res_stats, row.names = FALSE)

# Conversion rate: of the 2012 Sasa cells in each distance bin, what share
# became pine by 2021?  This controls for the fact that pine is abundant
# (294760 m2), so simple adjacency is common even for Sasa cells at large.
bb <- cut(d, breaks = BREAKS, labels = LABELS, right = TRUE)
rate <- data.frame(
  bin = LABELS,
  n_sasa12 = as.integer(table(bb[sasa12])[LABELS]),
  n_to_pine = as.integer(table(bb[to_pine])[LABELS]),
  n_to_other = as.integer(table(bb[to_other])[LABELS])
)
rate$n_to_pine[is.na(rate$n_to_pine)]   <- 0L
rate$n_to_other[is.na(rate$n_to_other)] <- 0L
rate$pct_converted_to_pine  <- round(100 * rate$n_to_pine / rate$n_sasa12, 2)
rate$pct_converted_to_other <- round(100 * rate$n_to_other / rate$n_sasa12, 2)
write.csv(rate, file.path(OUTDIR, "conversion_rate_by_distance.csv"), row.names = FALSE)
cat("\n--- conversion rate of 2012 Sasa cells by distance to 2012 pine ---\n")
print(rate, row.names = FALSE)

# ---- 4. clusters of isolated (>5 m) Sasa -> Pine transitions -----------------
iso <- to_pine & d > 5
cat("\n--- isolated Sasa -> Pine cells (>5 m) :", sum(iso), "---\n")

iso_r <- rast(v12)
values(iso_r) <- ifelse(iso, 1L, NA_integer_)

clu_tbl <- data.frame()
if (sum(iso) > 0) {
  pat <- patches(iso_r, directions = 8, zeroAsNA = TRUE)
  pv  <- as.vector(values(pat))
  ids <- pv[!is.na(pv)]
  sz  <- table(ids)
  xy  <- xyFromCell(pat, which(!is.na(pv)))
  dd  <- d[!is.na(pv)]
  clu_tbl <- do.call(rbind, lapply(names(sz), function(id) {
    sel <- ids == as.integer(id)
    data.frame(cluster_id = as.integer(id),
               n_cells = sum(sel), area_m2 = sum(sel),
               x_centroid = round(mean(xy[sel, 1]), 2),
               y_centroid = round(mean(xy[sel, 2]), 2),
               dist_min = round(min(dd[sel]), 2),
               dist_mean = round(mean(dd[sel]), 2),
               dist_max = round(max(dd[sel]), 2))
  }))
  clu_tbl <- clu_tbl[order(-clu_tbl$n_cells), ]

  # Neighbourhood context: composition of an 11 x 11 (+-5 m) window around each
  # isolated cluster centroid, in 2012 and in 2021.  This separates "sits inside
  # a Sasa patch far from any pine" from "sits in a locally pine-rich mosaic".
  m12 <- matrix(a12, nrow = nrow(v12), byrow = TRUE)
  m21 <- matrix(a21, nrow = nrow(v21), byrow = TRUE)
  ctx <- t(sapply(seq_len(nrow(clu_tbl)), function(i) {
    ce <- cellFromXY(v12, cbind(clu_tbl$x_centroid[i], clu_tbl$y_centroid[i]))
    rw <- rowFromCell(v12, ce); cl <- colFromCell(v12, ce)
    rr <- max(1, rw - 5):min(nrow(v12), rw + 5)
    cc <- max(1, cl - 5):min(ncol(v12), cl + 5)
    w12 <- as.vector(m12[rr, cc]); w21 <- as.vector(m21[rr, cc])
    w12 <- w12[!is.na(w12)];      w21 <- w21[!is.na(w21)]
    c(pct_pine_11x11_2012 = round(100 * mean(w12 == 7), 1),
      pct_sasa_11x11_2012 = round(100 * mean(w12 == 1), 1),
      pct_pine_11x11_2021 = round(100 * mean(w21 == 7), 1),
      pct_sasa_11x11_2021 = round(100 * mean(w21 == 1), 1))
  }))
  clu_tbl <- cbind(clu_tbl, ctx)
  write.csv(clu_tbl, file.path(OUTDIR, "isolated_clusters.csv"), row.names = FALSE)
  cat("clusters:", nrow(clu_tbl), " largest:", max(clu_tbl$n_cells), "m^2\n")
  print(utils::head(clu_tbl, 20), row.names = FALSE)

  clu_sizes <- data.frame(table(size_m2 = clu_tbl$n_cells))
  write.csv(clu_sizes, file.path(OUTDIR, "isolated_cluster_size_distribution.csv"),
            row.names = FALSE)
}

# ---- 5. map ------------------------------------------------------------------
png(file.path(OUTDIR, "isolated_transitions_map.png"),
    width = 2000, height = 2000, res = 200)
op <- par(mar = c(3, 3, 3, 1))
bg <- rast(v21); values(bg) <- a21
cmap <- c("#859900", "#eee8d5", "#c0c0c0", "#dc322f",
          "#b58900", "#6c71c4", "#2aa198")
plot(bg, col = cmap, breaks = 0.5:7.5, legend = FALSE,
     main = "2021 vegetation with Sasa->Pine transitions\n(black = isolated >5 m from 2012 pine)",
     mar = c(3, 3, 4, 6))
tp <- rast(v12); values(tp) <- ifelse(to_pine, 1L, NA_integer_)
plot(tp, col = "#ff00ff", legend = FALSE, add = TRUE)
if (sum(iso) > 0) {
  plot(iso_r, col = "black", legend = FALSE, add = TRUE)
  # 1-4 m^2 clusters are invisible at map scale: ring them.
  points(clu_tbl$x_centroid, clu_tbl$y_centroid, pch = 1, cex = 3, lwd = 2,
         col = "black")
}
legend("topright", inset = c(-0.02, 0), xpd = NA, bty = "n", cex = 0.7,
       legend = c(CLASS_LABELS[c("1","2","3","4","5","6","7")],
                  "Sasa->Pine (1156 cells)",
                  sprintf("isolated >5 m (%d cells, circled)", sum(iso))),
       fill = c(cmap, "#ff00ff", "black"))
par(op); dev.off()

# Zoom on the Sasa-rich core, where nearly all transitions occur.
png(file.path(OUTDIR, "transition_core_zoom.png"),
    width = 2000, height = 1800, res = 200)
e_core <- ext(732900, 733650, 4050900, 4051500)
plot(crop(bg, e_core), col = cmap, breaks = 0.5:7.5, legend = FALSE,
     mar = c(3, 3, 4, 7),
     main = "Sasa-rich core, 2021 vegetation\nmagenta = Sasa->Pine, black = isolated (>5 m)")
plot(crop(tp, e_core), col = "#ff00ff", legend = FALSE, add = TRUE)
if (sum(iso) > 0) {
  plot(crop(iso_r, e_core), col = "black", legend = FALSE, add = TRUE)
  points(clu_tbl$x_centroid, clu_tbl$y_centroid, pch = 1, cex = 2.5, lwd = 2)
}
legend("topright", inset = c(-0.02, 0), xpd = NA, bty = "n", cex = 0.7,
       legend = c(CLASS_LABELS[c("1","2","3","4","5","6","7")],
                  "Sasa->Pine", "isolated (>5 m)"),
       fill = c(cmap, "#ff00ff", "black"))
dev.off()

# Zoomed map on the largest isolated clusters.
if (nrow(clu_tbl) > 0) {
  png(file.path(OUTDIR, "isolated_clusters_zoom.png"),
      width = 2400, height = 1800, res = 200)
  n_show <- min(6, nrow(clu_tbl))
  par(mfrow = c(2, 3), mar = c(2, 2, 3, 1))
  for (i in seq_len(n_show)) {
    cx <- clu_tbl$x_centroid[i]; cy <- clu_tbl$y_centroid[i]
    e  <- ext(cx - 30, cx + 30, cy - 30, cy + 30)
    plot(crop(bg, e), col = cmap, breaks = 0.5:7.5, legend = FALSE,
         main = sprintf("cluster %d: %d m2, %.1f m from 2012 pine",
                        clu_tbl$cluster_id[i], clu_tbl$n_cells[i],
                        clu_tbl$dist_min[i]))
    plot(crop(tp, e), col = "#ff00ff", legend = FALSE, add = TRUE)
    plot(crop(iso_r, e), col = "black", legend = FALSE, add = TRUE)
  }
  dev.off()
}

cat("\nDone. Outputs in", OUTDIR, "\n")
