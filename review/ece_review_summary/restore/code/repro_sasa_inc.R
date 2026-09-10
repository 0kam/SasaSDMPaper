# data/sasa_inc.tiff has no producing script anywhere. Test the obvious definition:
# sasa_inc = 1 where 2021 is Sasa and 2012 is not.
suppressPackageStartupMessages(library(terra))
D <- "/Users/okamoto/NIES/SasaSDMPaper/data_from_server/ortho/data/"
a <- rast(paste0(D,"vege_2012_5x5.tiff")); b <- rast(paste0(D,"vege_2021_5x5.tiff"))
inc <- rast(paste0(D,"sasa_inc.tiff"))
cat("archived sasa_inc.tiff: dim", dim(inc), "res", res(inc), "\n")
print(table(values(inc), useNA="ifany"))
cand <- list(
  "2021==1 & 2012!=1" = (b==1) & (a!=1),
  "2021==1 & 2012==1" = (b==1) & (a==1),
  "2021!=1 & 2012==1" = (b!=1) & (a==1)
)
incr <- resample(inc, a, method="near")
iv <- values(incr)
for (nm in names(cand)) {
  cv <- values(cand[[nm]])
  ok <- !is.na(iv) & !is.na(cv)
  cat(sprintf("%-20s cells=%d  agreement with archived=%.4f%%  (archived==1 n=%d, candidate==1 n=%d)\n",
      nm, sum(ok), 100*mean((iv[ok]==1)==(cv[ok]==1)), sum(iv[ok]==1), sum(cv[ok]==1)))
}
