#!/usr/bin/env Rscript
# Descriptive cell comparisons with the separately normalized historical response.
suppressPackageStartupMessages(library(terra))
a=commandArgs(TRUE)
if(length(a)!=2L) stop('Usage: compare_historical_response.R STAGED_RELEASE OUTPUT_ROOT')
stage=a[1]; out=a[2]
target=file.path(out,'tables','historical_raster_comparisons.csv')
if(file.exists(target)) stop('Historical comparisons already exist')
terraOptions(tempdir=file.path(out,'model_objects'),memfrac=.2,progress=0)
h=rast(file.path(stage,'01_manuscript_response/manuscript_suitability.tif'))
paths=c(PAIRED_FULL_684=file.path(stage,'03_paired_sensitivity/full_paired_684_mean_suitability.tif'),PAIRED_ENV_684=file.path(stage,'03_paired_sensitivity/env_only_paired_684_mean_suitability.tif'))
rows=list()
for(label in names(paths)) {
  p=rast(paths[[label]]);stopifnot(compareGeom(h,p,stopOnError=FALSE))
  n=as.numeric(global(is.finite(h)&is.finite(p),'sum',na.rm=TRUE)[1,1]); x=y=numeric(n);k=0
  for(row in seq(1,nrow(h),by=128L)) {
    nr=min(128,nrow(h)-row+1); get=function(r){readStart(r);on.exit(readStop(r));readValues(r,row=row,nrows=nr)}
    hv=get(h);pv=get(p);good=is.finite(hv)&is.finite(pv);j=sum(good)
    if(j){ii=seq.int(k+1,k+j);x[ii]=hv[good];y[ii]=pv[good];k=k+j}
  }
  stopifnot(n==k);d=y-x
  rows[[label]]=data.frame(reference='HISTORICAL_MAIN',comparison=label,finite_cells=n,pearson=cor(x,y),spearman=cor(x,y,method='spearman'),MAE=mean(abs(d)),RMSE=sqrt(mean(d^2)),reference_mean=mean(x),comparison_mean=mean(y),comparison_minus_reference=mean(d),interpretation='Descriptive only: historical normalization/species-set/domain differ; not an isolated predictor-removal effect')
  rm(x,y,d);gc()
}
write.csv(do.call(rbind,rows),target,row.names=FALSE)
cat('Historical raw-scale comparisons saved with explicit aggregation and normalization limitations.\n')
quit(save='no',status=0)
