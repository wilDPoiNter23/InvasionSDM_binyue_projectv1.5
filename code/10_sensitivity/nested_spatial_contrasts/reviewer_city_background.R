#!/usr/bin/env Rscript
# Final-boundary city and 50 km surrounding-land contrasts.
# Retains the established equal-area reprojection and buffer definitions.
suppressPackageStartupMessages({library(terra);library(digest);library(ggplot2)})
a <- commandArgs(TRUE)
if(length(a)!=3L) stop('Usage: reviewer_city_background.R STAGED_RELEASE CITY_POLYGONS OUTPUT_ROOT')
stage=a[1]; city_path=a[2]; out=a[3]
for(d in c('tables','qc','logs','figures','model_objects')) dir.create(file.path(out,d),recursive=TRUE,showWarnings=FALSE)
target=file.path(out,'tables','city_background_statistics.csv')
if(file.exists(target)) stop('Completed contrast results exist; inspect before rerunning')
terraOptions(tempdir=file.path(out,'model_objects'),memfrac=.2,progress=0)
cities=vect(city_path); repaired=sum(!is.valid(cities)); if(repaired) cities=makeValid(cities)
stopifnot(nrow(cities)==108,!anyDuplicated(cities$shapeName),all(is.valid(cities)))
cities_m=project(cities,'EPSG:6933'); buffers=buffer(cities_m,width=50000)
# Removing all city interiors matches the original ring construction.
rings=erase(buffers,cities_m)
if(any(!is.valid(rings))) rings=makeValid(rings)
stopifnot(nrow(buffers)==108,nrow(rings)==108,identical(as.character(cities_m$shapeName),as.character(rings$shapeName)))
paths=c(HISTORICAL_MAIN=file.path(stage,'01_manuscript_response/manuscript_suitability.tif'),PAIRED_FULL_684=file.path(stage,'03_paired_sensitivity/full_paired_684_mean_suitability.tif'),PAIRED_ENV_684=file.path(stage,'03_paired_sensitivity/env_only_paired_684_mean_suitability.tif'))
rows=list(); stats=list(); set.seed(20261014); B=1999L
for(label in names(paths)) {
  cat('Projecting and extracting',label,'\n')
  r=rast(paths[[label]])
  filename=file.path(out,'model_objects',paste0(tolower(label),'_epsg6933.tif'))
  if(file.exists(filename)) stop('Partial projected raster exists; inspect before rerunning')
  r=project(r,'EPSG:6933',method='bilinear',filename=filename,overwrite=FALSE,wopt=list(datatype='FLT4S',gdal='COMPRESS=DEFLATE'))
  means=function(v) as.numeric(extract(r,v,fun=mean,na.rm=TRUE)[[2]])
  ct=means(cities_m); bf=means(buffers); bg=means(rings)
  tab=data.frame(response=label,city=cities$shapeName,city_mean=ct,full_buffer_mean=bf,ring_background_mean=bg,city_minus_ring=ct-bg,city_minus_buffer=ct-bf)
  rows[[label]]=tab
  for(baseline in c('ring_background_mean','full_buffer_mean')) {
    x=ct; y=tab[[baseline]]; keep=is.finite(x)&is.finite(y);x=x[keep];y=y[keep];d=x-y;n=length(d)
    tt=t.test(x,y,paired=TRUE); wt=wilcox.test(x,y,paired=TRUE,exact=FALSE)
    boot=replicate(B,{i=sample.int(n,n,replace=TRUE);c(mean_difference=mean(d[i]),percent_difference=100*(mean(x[i])/mean(y[i])-1))})
    ci=apply(boot,1,quantile,c(.025,.975))
    stats[[paste(label,baseline)]]=data.frame(response=label,baseline=baseline,n_cities=n,city_mean=mean(x),background_mean=mean(y),mean_difference=mean(d),median_difference=median(d),percent_difference=100*(mean(x)/mean(y)-1),mean_difference_CI_low=ci[1,1],mean_difference_CI_high=ci[2,1],percent_CI_low=ci[1,2],percent_CI_high=ci[2,2],paired_t=unname(tt$statistic),t_df=unname(tt$parameter),t_p=tt$p.value,wilcoxon_V=unname(wt$statistic),wilcoxon_p=wt$p.value,cohens_dz=mean(d)/sd(d),bootstrap_replicates=B,seed=20261014,scope='Boundary-updated spatial contrast; not exact historical-geometry reproduction')
  }
}
allrows=do.call(rbind,rows); allstats=do.call(rbind,stats)
write.csv(allrows,file.path(out,'tables','three_response_city_background_source.csv'),row.names=FALSE)
write.csv(allstats,target,row.names=FALSE)
write.csv(data.frame(city=cities$shapeName,buffer_km=50,CRS='EPSG:6933',buffer_definition='city polygon buffered by 50 km',ring_definition='buffer minus all city interiors',city_source_sha256=digest(file=city_path,algo='sha256'),city_geometries_repaired=repaired),file.path(out,'qc','spatial_definition.csv'),row.names=FALSE)
write.csv(data.frame(analysis=c('City/background final-boundary sensitivity','Exact historical city/background reproduction','Within-city reclaimed/nonreclaimed reproduction'),status=c('COMPLETED_NUMERICALLY','BLOCKED','BLOCKED'),reason=c('Final108 boundaries; original 50km reprojection/buffer framework','Original city vector currently has107 features; historic version chain unresolved','Five-city patch exists, legacy full-zone geometry lineage unresolved')),file.path(out,'qc','contrast_status.csv'),row.names=FALSE)
p=ggplot(allrows,aes(x=response,y=city_minus_ring))+geom_hline(yintercept=0,colour='grey50',linewidth=.35)+geom_boxplot(width=.55,outlier.shape=NA,fill='#9DC7CA')+geom_jitter(width=.08,height=0,alpha=.5,size=1)+theme_classic(base_size=9,base_family='Arial')+labs(x=NULL,y='City minus surrounding ring mean',subtitle='Final 108 city boundaries; 50 km buffer; each response retains its own scale')
ggsave(file.path(out,'figures','city_background_difference.pdf'),p,width=7.2,height=4,device=cairo_pdf)
ggsave(file.path(out,'figures','city_background_difference.png'),p,width=7.2,height=4,dpi=300)
capture.output(sessionInfo(),file=file.path(out,'logs','sessionInfo.txt'))
cat('Final-boundary city/background contrasts completed. Historical and full-zone provenance blockers retained.\n')
quit(save='no',status=0)
