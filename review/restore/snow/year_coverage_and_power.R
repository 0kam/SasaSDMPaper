# Annual scene means, 2010 assessment, and power of the landscape trend test.
suppressMessages(library(terra))
S <- "/Users/okamoto/NIES/SasaSDMPaper/data_from_server/ortho/data/snow/"
OUTDIR <- "/Users/okamoto/NIES/SasaSDMPaper/.claude/worktrees/review-summary-ece-ddc69c/review/restore/snow"
fs <- sort(list.files(paste0(S,"raw"), "\\.tiff$", full.names=TRUE))
yrs <- as.integer(sub(".*_(20\\d{2})_.*","\\1",basename(fs)))
M <- values(rast(fs)); ok <- rowSums(is.na(M))==0
M[M<1] <- NA
X <- M[ok,]
ann <- data.frame(year=yrs, mean_DOY=colMeans(X,na.rm=TRUE),
                  sd_DOY=apply(X,2,sd,na.rm=TRUE),
                  n_valid_pixels=colSums(!is.na(X)),
                  n_DOY0_dropped=colSums(is.na(X)))
write.csv(ann, file.path(OUTDIR,"annual_scene_means.csv"), row.names=FALSE)
print(ann, row.names=FALSE)

f <- lm(mean_DOY ~ year, ann); rsd <- summary(f)$sigma
pw <- do.call(rbind, lapply(0:8, function(e){
  yy <- c(yrs, if(e>0) 2022:(2021+e)); Sxx <- sum((yy-mean(yy))^2)
  se <- rsd/sqrt(Sxx); df <- length(yy)-2; tc <- qt(.975,df)
  data.frame(n_years=length(yy), last_year=max(yy), slope_SE=se,
             min_detectable_slope=tc*se,
             power_at_true_slope_0.71 = pt(-tc,df,ncp=0.7146/se) + pt(tc,df,ncp=0.7146/se,lower.tail=FALSE))
}))
write.csv(pw, file.path(OUTDIR,"landscape_trend_power.csv"), row.names=FALSE)
print(pw, row.names=FALSE)
