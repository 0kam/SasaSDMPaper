.libPaths(c("/private/tmp/claude-501/-Users-okamoto-NIES-SasaSDMPaper--claude-worktrees-review-summary-ece-ddc69c/98ac7ecf-eafb-49c6-aa2d-9aab2998c13e/scratchpad/Rlib", .libPaths()))
suppressPackageStartupMessages({library(tidyverse); library(terra); library(tidyterra); library(sf)})
setwd("/Users/okamoto/NIES/SasaSDMPaper/ortho")
OUT <- "/Users/okamoto/NIES/SasaSDMPaper/.claude/worktrees/review-summary-ece-ddc69c/review/restore/out"

ar   <- function(r) { e <- expanse(ifel(r > 0.5, 1, NA), unit = "m"); if (nrow(e)==0) 0 else e[1,2] }
ncell_suit <- function(r) sum(values(r) > 0.5, na.rm = TRUE)
newly <- function(base, fut) { n <- c(fut, base); names(n) <- c("f","b")
  n %>% filter(b < 0.5) %>% filter(f > 0.5) %>% select(f) %>% expanse(unit="m") %>% pull(area) }
lost <- function(base, fut) { n <- c(fut, base); names(n) <- c("f","b")
  n %>% filter(b > 0.5) %>% filter(f < 0.5) %>% select(f) %>% expanse(unit="m") %>% pull(area) }

a21 <- rast("data/sasa_pred_tdm_21.tiff"); names(a21) <- "p"
a30 <- rast("data/sasa_pred_tdm_30.tiff"); names(a30) <- "p"
cat("### archived TDM rasters\n")
cat(sprintf("  2021 suitable area = %.2f m2   (cells>0.5 = %d)   manuscript 12766\n", ar(a21), ncell_suit(a21)))
cat(sprintf("  2030 suitable area = %.2f m2   (cells>0.5 = %d)\n", ar(a30), ncell_suit(a30)))
cat(sprintf("  newly suitable 2030 = %.2f m2   manuscript 4387\n", newly(a21, a30)))

v <- list()
for (f in list.files(OUT, pattern="^repro_tdm_.*tiff$", full.names=TRUE)) {
  nm <- gsub("^repro_tdm_|\\.tiff$", "", basename(f))
  r <- rast(f); names(r) <- "p"; v[[nm]] <- r
}
cat("\n### 2x2 snowmelt vintage x distance layer (TDM stack, polygon>5m2 distances)\n")
for (nm in c("snow21_distpol12","snow21_distpol21","snow30_distpol12","snow30_distpol21",
             "snow21_distall12","snow21_distall21","snow30_distall12","snow30_distall21")) {
  r <- v[[nm]]
  cat(sprintf("  %-18s suitable = %9.2f m2  cells = %6d  newly-vs-arch2021 = %9.2f  lost-vs-arch2021 = %8.2f\n",
              nm, ar(r), ncell_suit(r), newly(a21, r), lost(a21, r)))
}

cat("\n### F-3 decomposition (baseline = archived 2021 = snow21 x distpol12)\n")
base <- v[["snow21_distpol12"]]
full <- v[["snow30_distpol21"]]
snowonly <- v[["snow30_distpol12"]]   # snow 2021->2030, distance held at 2012
distonly <- v[["snow21_distpol21"]]   # distance 2012->2021, snow held at 2021
n_full <- newly(base, full); n_snow <- newly(base, snowonly); n_dist <- newly(base, distonly)
cat(sprintf("  full (snow30 + dist21)      newly = %9.2f  (100%%)   total suitable = %.2f\n", n_full, ar(full)))
cat(sprintf("  snow only (dist held 2012)  newly = %9.2f  (%5.1f%%) total suitable = %.2f\n", n_snow, 100*n_snow/n_full, ar(snowonly)))
cat(sprintf("  dist only (snow held 2021)  newly = %9.2f  (%5.1f%%) total suitable = %.2f\n", n_dist, 100*n_dist/n_full, ar(distonly)))
cat(sprintf("  baseline archived 2021      total suitable = %.2f\n", ar(base)))

cat("\n### F-4 training/prediction distance mismatch\n")
cat(sprintf("  as published (dist from 2012 polygons >5 m2)  2021 suitable = %9.2f m2\n", ar(v[["snow21_distpol12"]])))
cat(sprintf("  training-consistent (dist from ALL 2012 px)   2021 suitable = %9.2f m2  (%+.1f%%)\n",
            ar(v[["snow21_distall12"]]), 100*(ar(v[["snow21_distall12"]])/ar(v[["snow21_distpol12"]])-1)))
cat(sprintf("  observed 2021 Sasa area (manuscript) = 10170 m2\n"))
