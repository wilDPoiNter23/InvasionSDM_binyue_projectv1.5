#!/usr/bin/env Rscript
# Compare archived city responses with fresh final-boundary raster extractions.
suppressPackageStartupMessages(library(terra))
a=commandArgs(TRUE)
if(length(a)!=4L) stop('Usage: check_historical_city_concordance.R STAGED_RELEASE CITY_POLYGONS PAIRED_OUTPUT GAM_OUTPUT')
stage=a[1];city_path=a[2];out=a[3];gamout=a[4]
terraOptions(tempdir=file.path(out,'model_objects'),memfrac=.2,progress=0)
arch=read.csv(file.path(stage,'06_pca_gam_input/gamtest_withpca_recheck.csv'))
fresh=read.csv(file.path(out,'tables/three_response_city_extraction.csv'))
key=function(x)trimws(gsub('[^a-z0-9]+',' ',tolower(x)))
stopifnot(!anyDuplicated(key(arch$city)),!anyDuplicated(key(fresh$city)))
idx=match(key(fresh$city),key(arch$city));stopifnot(!anyNA(idx))
cities=vect(city_path);if(any(!is.valid(cities)))cities=makeValid(cities)
gi=match(key(fresh$city),key(cities$shapeName));stopifnot(!anyNA(gi))
r=rast(file.path(stage,'01_manuscript_response/manuscript_suitability.tif'))
ordinary=as.numeric(extract(r,cities[gi,],fun=mean,na.rm=TRUE)[[2]])
contract=read.csv(file.path(gamout,'qc/city_sample_contract.csv'));included=contract$included[match(key(fresh$city),key(contract$city))]
tab=data.frame(city=fresh$city,archived_gam_risk=arch$risk[idx],fresh_exact_fraction_mean=fresh$HISTORICAL_MAIN,fresh_cell_centre_mean=ordinary,in_archived_gam84=included,archived_minus_exact=arch$risk[idx]-fresh$HISTORICAL_MAIN,archived_minus_cell_centre=arch$risk[idx]-ordinary)
target=file.path(out,'qc/historical_city_response_concordance.csv');if(file.exists(target))stop('Concordance QC exists; inspect before rerunning')
write.csv(tab,target,row.names=FALSE)
summary=do.call(rbind,lapply(c('all108','gam84'),function(subset){z=if(subset=='gam84')tab[tab$in_archived_gam84,]else tab;do.call(rbind,lapply(c('fresh_exact_fraction_mean','fresh_cell_centre_mean'),function(method){d=z$archived_gam_risk-z[[method]];ok=is.finite(d);data.frame(subset=subset,extraction=method,n=sum(ok),max_abs_difference=max(abs(d[ok])),mean_abs_difference=mean(abs(d[ok])),correlation=cor(z$archived_gam_risk[ok],z[[method]][ok]),status='WARNING_RESPONSE_LINEAGE_NOT_CONCORDANT')}))}))
write.csv(summary,file.path(out,'qc/historical_city_response_concordance_summary.csv'),row.names=FALSE)
cat('Archived responses preserved; concordance discrepancy quantified without refitting.\n')
quit(save='no',status=0)
