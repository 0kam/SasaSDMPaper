suppressMessages(library(terra))
f <- c(
 sc_pub="/Users/okamoto/NIES/SasaSDMPaper/ortho/data/selected_comms.tiff",
 sc_srv="/Users/okamoto/NIES/SasaSDMPaper/data_from_server/ortho/data/selected_comms.tiff",
 sasa_inc="/Users/okamoto/NIES/SasaSDMPaper/data_from_server/sasa_increase/sasa_inc.tiff",
 risky_srv="/Users/okamoto/NIES/SasaSDMPaper/data_from_server/sasa_increase/risky_area.tiff",
 risky_wo="/Users/okamoto/NIES/SasaSDMPaper/ortho/risky_area_wo_dist.tiff",
 risky_tdm="/Users/okamoto/NIES/SasaSDMPaper/ortho/risky_area_tdm.tiff",
 pred21="/Users/okamoto/NIES/SasaSDMPaper/data_from_server/sasa_increase/sasa_pred_sdm_dist_21.tiff",
 pred30="/Users/okamoto/NIES/SasaSDMPaper/data_from_server/sasa_increase/sasa_pred_sdm_dist_30.tiff",
 tdm30bin="/Users/okamoto/NIES/SasaSDMPaper/data_from_server/sasa_increase/sasa_pred_tdm_30_bin.tiff",
 v12="/Users/okamoto/NIES/SasaSDMPaper/ortho/data/vege_2012_5x5.tiff",
 v12s="/Users/okamoto/NIES/SasaSDMPaper/data_from_server/sasa_increase/vege_2012_5x5.tiff",
 georect="/Users/okamoto/NIES/SasaSDMPaper/data_from_server/sasa_increase/georectified.tiff"
)
for (n in names(f)) {
  if(!file.exists(f[n])) { cat(n,"MISSING\n"); next }
  r <- rast(f[n])
  cat("=====", n, "\n")
  cat(" dim:", paste(dim(r),collapse="x"), " names:", paste(names(r),collapse=","), "\n")
  cat(" res:", paste(signif(res(r),8),collapse=","), " ext:", paste(signif(as.vector(ext(r)),12),collapse=","), "\n")
  cat(" crs:", crs(r,describe=TRUE)$code, crs(r,describe=TRUE)$name, " dtype:", paste(datatype(r),collapse=","),"\n")
  s <- summary(r, size=Inf)
  print(s)
  v <- values(r[[1]])
  u <- sort(unique(v[!is.na(v)]))
  cat(" n_unique:", length(u), " head:", paste(head(u,15),collapse=","), "\n")
  cat(" nNA:", sum(is.na(v)), " nCell:", length(v), "\n")
}
