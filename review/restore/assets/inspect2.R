suppressMessages({library(terra)})
sc <- rast("/Users/okamoto/NIES/SasaSDMPaper/ortho/data/selected_comms.tiff")
v <- values(sc); v <- v[!is.na(v)]
cat("selected_comms: n non-NA cells =", length(v), "\n")
tb <- table(v)
cat("n distinct values (=polygons?) :", length(tb), "\n")
print(tb)
cat("check: value == count of cells with that value?\n")
d <- data.frame(val=as.numeric(names(tb)), n=as.integer(tb))
d$diff <- d$n - d$val
print(head(d[order(-abs(d$diff)),],10))
cat("sum of cells:", sum(d$n), " sum of distinct values:", sum(d$val), "\n")
