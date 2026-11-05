# Export small independent raster samples for a cached-input comparison elsewhere.
suppressPackageStartupMessages({library(terra);library(jsonlite)})
a<-commandArgs(TRUE);stopifnot(length(a)==2L)
cfg<-fromJSON(a[1]);out<-a[2];dir.create(out,recursive=TRUE,showWarnings=FALSE)
s<-readRDS(file.path(cfg$cached_comparison,'support.rds'))
x<-read.csv(file.path(cfg$cached_comparison,'species_inputs.csv'))
p<-read.csv(cfg$paired_manifest)
all<-rbind(data.frame(group='FULL_814',x[x$set=='ALL_FULL_CONTINUOUS',c('species','path')]),data.frame(group='PAIRED_FULL',species=p$species,path=p$full_projection),data.frame(group='PAIRED_ENV',species=p$species,path=p$env_projection))
set.seed(cfg$seed+1);rows<-list();k<-0L
for(i in unique(round(seq(1,nrow(all),length.out=9)))){
  ix<-sample.int(length(s$cells),80);r<-rast(all$path[i]);v<-extract(r,s$xy[ix,,drop=FALSE]);k<-k+1L
  rows[[k]]<-data.frame(group=all$group[i],species=all$species[i],cell_index=ix,value=as.numeric(v[[ncol(v)]])/1000,source_path=all$path[i])
}
write.csv(do.call(rbind,rows),file.path(out,'reference_source_values.csv'),row.names=FALSE)
old<-read.csv(cfg$native_projection_manifest);rows<-list();k<-0L
for(i in unique(round(seq(1,nrow(old),length.out=9)))){
  ix<-sample.int(length(s$cells),600)
  r<-rast(old$source_global_projection[i]);m<-rast(old$output_non_native_projection[i])
  x<-extract(r,s$xy[ix,,drop=FALSE]);y<-extract(m,s$xy[ix,,drop=FALSE]);k<-k+1L
  rows[[k]]<-data.frame(species=old$accepted_taxon[i],cell_index=ix,source_value=as.numeric(x[[ncol(x)]]),masked_value=as.numeric(y[[ncol(y)]]))
}
write.csv(do.call(rbind,rows),file.path(out,'reference_mask_values.csv'),row.names=FALSE)
cat('Wrote independent source and mask samples\n')
