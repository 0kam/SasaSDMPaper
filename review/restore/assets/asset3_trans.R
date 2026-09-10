suppressPackageStartupMessages(library(terra))
D <- "/Users/okamoto/NIES/SasaSDMPaper/data_from_server/ortho/data/"
R <- "/Users/okamoto/NIES/SasaSDMPaper/.claude/worktrees/review-summary-ece-ddc69c/review/restore/assets/out/"
lab <- c("Sasa","OtherVeg","NonVeg","Sorbus","Maple","Alnus","DwarfPine")
tm <- function(a,b,tag){
  av <- values(a); bv <- values(b); ok <- !is.na(av)&!is.na(bv)&av>=1&bv>=1
  t <- table(y2012=av[ok], y2021=bv[ok])
  cat("\n#####",tag," cells:",sum(ok),"\n"); print(t)
  s12 <- sum(av[ok]==1); s21 <- sum(bv[ok]==1)
  gain <- sum(av[ok]!=1 & bv[ok]==1); loss <- sum(av[ok]==1 & bv[ok]!=1)
  cat(sprintf("Sasa 2012=%d 2021=%d  gross gain=%d  gross loss=%d  net=%+d\n", s12,s21,gain,loss,s21-s12))
  lo <- table(bv[ok][av[ok]==1 & bv[ok]!=1]); gi <- table(av[ok][av[ok]!=1 & bv[ok]==1])
  cat("LOSS destination (2012 Sasa -> 2021 class):\n")
  for(k in names(lo)) cat(sprintf("   %-10s %6d  %5.1f%%\n", lab[as.integer(k)], lo[[k]], 100*lo[[k]]/loss))
  cat("GAIN origin (2012 class -> 2021 Sasa):\n")
  for(k in names(gi)) cat(sprintf("   %-10s %6d  %5.1f%%\n", lab[as.integer(k)], gi[[k]], 100*gi[[k]]/gain))
}
tm(rast(paste0(D,"vege_2012_5x5.tiff")), rast(paste0(D,"vege_2021_5x5.tiff")), "PUBLISHED vege_*_5x5.tiff")
tm(rast(paste0(R,"repro_vege_2012_5x5.tiff")), rast(paste0(R,"repro_vege_2021_5x5.tiff")), "REPRODUCED from use_this/*.npy")
