.libPaths(c("/private/tmp/claude-501/-Users-okamoto-NIES-SasaSDMPaper--claude-worktrees-review-summary-ece-ddc69c/98ac7ecf-eafb-49c6-aa2d-9aab2998c13e/scratchpad/Rlib", .libPaths()))
suppressPackageStartupMessages({library(tidyverse);library(terra)})
O <- "/Users/okamoto/NIES/SasaSDMPaper/.claude/worktrees/review-summary-ece-ddc69c/review/restore/out"
for (m in c("tdm21","tdm30","tbm21","tbm30")) {
  a <- rast(file.path(O, paste0("reg_pub_", m, ".tiff")))
  b <- rast(file.path(O, paste0("reg_fix_", m, ".tiff")))
  va <- values(a); vb <- values(b); ok <- !is.na(va) & !is.na(vb)
  d <- vb[ok] - va[ok]
  flip <- sum((va[ok] > 0.5) != (vb[ok] > 0.5))
  cat(sprintf("%s  n=%d  mean(d)=%+.5f  sd(d)=%.5f  max|d|=%.4f  cells crossing 0.5 = %d (%.2f%%)  cor=%.6f\n",
      m, sum(ok), mean(d), sd(d), max(abs(d)), flip, 100*flip/sum(ok), cor(va[ok], vb[ok])))
}
# snow predictor delta itself
setwd("/Users/okamoto/NIES/SasaSDMPaper/ortho")
v12 <- rast("data/vege_2012_5x5.tiff")
for (y in c(2012, 2021, 2030)) {
  f <- rast(sprintf("data/snow/fitted_%d.tiff", y))
  p <- resample(f, v12); q <- resample(terra::shift(f, dx=0.5, dy=-0.5), v12)
  vp <- values(p); vq <- values(q); ok <- !is.na(vp) & !is.na(vq); d <- vq[ok]-vp[ok]
  cat(sprintf("snow %d predictor: n=%d mean=%+.4f sd=%.4f max|d|=%.2f  %%|d|>1DOY=%.1f\n",
      y, sum(ok), mean(d), sd(d), max(abs(d)), 100*mean(abs(d)>1)))
}
