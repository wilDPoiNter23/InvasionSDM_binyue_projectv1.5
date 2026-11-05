# Known-answer checks for whole-city weighting and distinct ratio estimands.
suppressPackageStartupMessages(library(jsonlite))
args<-commandArgs(TRUE)
out<-if(length(args))args[1]else tempfile('land_overall_')
dir.create(out,recursive=TRUE,showWarnings=FALSE)
zones<-c('OUTSIDE_RING','NON_RECLAIMED_CITY','RECLAIMED_CITY')
rows<-list()
for(city in 1:2)for(g in c('FULL','ENV'))for(j in 1:3){
 values<-if(city==1)c(.1,.4,.1)else c(.2,.6,.2)
 areas<-if(city==1)c(20,9,1)else c(20,1,9)
 rows[[length(rows)+1L]]<-data.frame(city_id=paste0('City',city),zone=zones[j],support_policy='FIXED_261',group=g,
  mean_suitability=values[j]*if(g=='FULL')1 else .5,common_area_km2=areas[j],native_cell_equivalents=10)
}
write.csv(do.call(rbind,rows),file.path(out,'zone_source_means.csv'),row.names=FALSE)
write.csv(data.frame(city_id=paste0('City',1:2),status='EXTRACTED'),file.path(out,'city_geometry_status.csv'),row.names=FALSE)
cfg<-list(output_root=normalizePath(out,winslash='/'),support_policies='FIXED_261',include_city_overall=TRUE,seed=20261015,bootstrap_replicates=1999)
file<-file.path(out,'config.json');write_json(cfg,file,auto_unbox=TRUE)
script<-sub('^--file=','',grep('^--file=',commandArgs(FALSE),value=TRUE))
statistics<-file.path(dirname(script),'common_land_baseline_statistics.R')
rscript<-file.path(R.home('bin'),if(.Platform$OS.type=='windows')'Rscript.exe'else'Rscript')
status<-system2(rscript,c(shQuote(statistics),shQuote(file)))
stopifnot(status==0L)
d<-read.csv(file.path(out,'zone_effects.csv'))
q<-subset(d,group=='FULL'&contrast=='CITY_MINUS_RING'&minimum_cell_equivalents==1)
stopifnot(nrow(q)==1L,abs(q$target_mean-.305)<1e-12,abs(q$reference_mean-.15)<1e-12,
 abs(q$mean_difference-.155)<1e-12,abs(q$CI_low-.04)<1e-12,abs(q$CI_high-.27)<1e-12,
 abs(q$mean_ring_100_index_difference-145)<1e-10,abs(q$ratio_of_means_percent-103.333333333333)<1e-10)
stopifnot(all(table(d$group,d$minimum_cell_equivalents)==4L))
cat('PASS_WHOLE_CITY_WEIGHTING_AND_RATIO_ESTIMANDS\n')
