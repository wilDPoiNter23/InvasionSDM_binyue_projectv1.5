# Extract saved per-species suitability independently for each model group.
suppressPackageStartupMessages({library(terra);library(jsonlite);library(digest)})
a<-commandArgs(TRUE);stopifnot(length(a)==4L);cfg<-fromJSON(a[1]);group<-a[2];first<-as.integer(a[3]);count<-as.integer(a[4]);stopifnot(group%in%c('ENV','FULL'))
out<-file.path(cfg$output_root,'species',group);dir.create(out,recursive=TRUE,showWarnings=FALSE)
p<-read.csv(cfg$paired_manifest);s<-readRDS(file.path(cfg$output_root,'native_support.rds'));w<-s$weights;J<-nrow(s$zones)
sumzone<-function(v){r<-rowsum(v,w$zone_id,reorder=FALSE);ans<-rep(0,J);ans[as.integer(rownames(r))]<-r[,1];ans}
for(i in seq(first,min(nrow(p),first+count-1L))){dest<-file.path(out,sprintf('species_%04d.csv',i));if(file.exists(dest))next
 path<-p[[paste0(tolower(group),'_projection')]][i];div<-p[[paste0(tolower(group),'_scale_divisor')]][i]
 r<-rast(path);stopifnot(compareGeom(r,s$grid,stopOnError=FALSE))
 vals<-as.numeric(extract(r,s$cells)[[1]])/div;stopifnot(all(vals[is.finite(vals)]>=0&vals[is.finite(vals)]<=1))
 v<-vals[w$cell_index];ok<-is.finite(v);den<-sumzone(ifelse(ok,w$weight_km2,0));num<-sumzone(ifelse(ok,v*w$weight_km2,0))
 d<-s$zones;d$species<-p$species[i];d$group<-group;d$finite_area_km2<-den;d$mean_suitability<-ifelse(den>0,num/den,NA_real_)
 write.csv(d,paste0(dest,'.part'),row.names=FALSE);stopifnot(file.rename(paste0(dest,'.part'),dest))
 if(i%%10==0){cat(group,i,'species extracted\n');flush.console()}
}
