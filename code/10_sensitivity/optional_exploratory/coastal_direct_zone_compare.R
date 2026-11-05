# Compare reclaimed and other urban land without an outside-ring condition.
suppressPackageStartupMessages({library(jsonlite);library(digest)})
a<-commandArgs(TRUE);stopifnot(length(a)==1L);cfg<-fromJSON(a[1]);out<-cfg$output_root
stopifnot(!dir.exists(out));dir.create(out,recursive=TRUE)
d<-read.csv(cfg$zone_means);d<-d[d$zone%in%c('RECLAIMED_CITY','NON_RECLAIMED_CITY'),]
set.seed(cfg$seed);results<-list();sources<-list()
for(g in unique(d$group)){
 z<-d[d$group==g,];r<-z[z$zone=='RECLAIMED_CITY',];n<-z[z$zone=='NON_RECLAIMED_CITY',]
 q<-merge(r,n,by=c('city_id','group'),suffixes=c('_reclaimed','_other'))
 q<-q[is.finite(q$mean_suitability_reclaimed)&is.finite(q$mean_suitability_other)&q$finite_area_km2_reclaimed>0&q$finite_area_km2_other>0,]
 delta<-q$mean_suitability_reclaimed-q$mean_suitability_other
 boot<-replicate(cfg$bootstrap_replicates,mean(sample(delta,length(delta),TRUE)));ci<-quantile(boot,c(.025,.975))
 results[[g]]<-data.frame(group=g,cities=nrow(q),reclaimed_mean=mean(q$mean_suitability_reclaimed),other_mean=mean(q$mean_suitability_other),difference=mean(delta),lower=ci[1],upper=ci[2],percent=100*(mean(q$mean_suitability_reclaimed)/mean(q$mean_suitability_other)-1))
 sources[[g]]<-q
}
write.csv(do.call(rbind,results),file.path(out,'direct_zone_contrasts.csv'),row.names=FALSE)
write.csv(do.call(rbind,sources),file.path(out,'city_source.csv'),row.names=FALSE)
write_json(list(status='DIRECT_TWO_ZONE_COMPARISON_COMPLETED',source_sha256=digest(file=cfg$zone_means,algo='sha256'),
 rule='Each saved response separately; only finite positive area in the two urban zones; no outside-ring requirement, extra species filter or minimum cell threshold'),file.path(out,'receipt.json'),auto_unbox=TRUE,pretty=TRUE)
