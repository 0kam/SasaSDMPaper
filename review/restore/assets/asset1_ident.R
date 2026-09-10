suppressPackageStartupMessages(library(terra))
S <- "/Users/okamoto/NIES/SasaSDMPaper/data_from_server/sasa_increase/"
D <- "/Users/okamoto/NIES/SasaSDMPaper/data_from_server/ortho/data/"
P <- "/Users/okamoto/NIES/SasaSDMPaper/ortho/"
v12 <- rast(paste0(D,"vege_2012_5x5.tiff")); v21 <- rast(paste0(D,"vege_2021_5x5.tiff"))
inc <- rast(paste0(S,"sasa_inc.tiff")); crs(inc) <- crs(v12)
cat("--- sasa_inc.tiff identity test\n")
cand <- (v21==1) & (v12!=1)
iv <- values(inc); cv <- values(cand)
ok <- !is.na(iv) & !is.na(cv)
cat("cells:", sum(ok), " agreement with (2021==1 & 2012!=1):", sprintf("%.6f%%",100*mean(iv[ok]==cv[ok])),
    " archived n1=", sum(iv[ok]==1), " candidate n1=", sum(cv[ok]==1), "\n")
cat(" vege_2012_5x5 copy in sasa_increase identical to ortho/data:",
    isTRUE(all.equal(values(rast(paste0(S,"vege_2012_5x5.tiff"))), values(v12))), "\n")

cat("\n--- sasa_pred_sdm_dist_{21,30} vs ortho/data/sasa_pred_tdm_{21,30}\n")
for (y in c("21","30")) {
  a <- rast(paste0(S,"sasa_pred_sdm_dist_",y,".tiff"))
  for (b_path in c(paste0(D,"sasa_pred_tdm_",y,".tiff"), paste0("/Users/okamoto/NIES/SasaSDMPaper/ortho/data/sasa_pred_tdm_",y,".tiff"))) {
    if (!file.exists(b_path)) { cat("  missing:", b_path, "\n"); next }
    b <- rast(b_path); crs(a) <- crs(b)
    av <- values(a); bv <- values(b)
    ok <- !is.na(av) & !is.na(bv)
    cat(sprintf("  %s vs %s : same dim=%s  overlap=%d  maxabsdiff=%.3g  identicalNA=%s\n",
      basename(sources(a)), b_path, identical(dim(a),dim(b)), sum(ok), max(abs(av[ok]-bv[ok])),
      identical(is.na(av), is.na(bv))))
  }
}
cat("\n--- risky_area.tiff (sasa_increase, band 'risk') identity\n")
rk <- rast(paste0(S,"risky_area.tiff"))
p21 <- rast(paste0(S,"sasa_pred_sdm_dist_21.tiff")); p30 <- rast(paste0(S,"sasa_pred_sdm_dist_30.tiff"))
crs(rk) <- crs(p21)
d <- p30 - p21
rv <- values(rk); dv <- values(d)
ok <- !is.na(rv) & !is.na(dv)
cat("  risk non-NA:", sum(!is.na(rv)), " (p30-p21) non-NA:", sum(!is.na(dv)), " overlap:", sum(ok),
    " maxabsdiff on overlap:", signif(max(abs(rv[ok]-dv[ok])),4), "\n")
# where is risk defined?
msk <- !is.na(rv)
cat("  vege21 classes under risk mask:\n"); print(table(values(v21)[msk], useNA="ifany"))
cat("  p30>0.5 within risk mask:", sum(values(p30)[msk] > 0.5, na.rm=TRUE), "of", sum(msk), "\n")

cat("\n--- published risky_area_* (manuscript)\n")
for (f in c("risky_area_wo_dist.tiff","risky_area_tdm.tiff")) {
  r <- rast(paste0(P,f)); crs(r) <- crs(v12)
  m <- !is.na(values(r))
  v21c <- terra::extract(v21, crds(r, na.rm=FALSE)[m,])
  cat(" ", f, " cells:", sum(m), " vege21 composition:\n"); print(table(v21c[[1]], useNA="ifany"))
}
