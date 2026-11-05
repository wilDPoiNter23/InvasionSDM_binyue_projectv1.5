#!/usr/bin/env Rscript
# Recompute out-of-range native fractions with planar GEOS clipping.
suppressPackageStartupMessages({library(sf);library(jsonlite);library(digest)})
sf_use_s2(FALSE)
a<-commandArgs(TRUE);if(length(a)!=2L)stop("Supply private JSON and fresh support directory")
cfg<-fromJSON(a[1],simplifyVector=FALSE)
if(dir.exists(a[2]))stop("Existing support retained")
source<-file.path(cfg$city_support_directory,"support.rds");s<-readRDS(source)
marker<-fromJSON(file.path(cfg$city_support_directory,"support_receipt.json"))
stopifnot(digest(file=source,algo="sha256")==marker$support_sha256)
bad<-which(!is.finite(s$weights$fraction)|s$weights$fraction<=0|s$weights$fraction>1)
if(any(!is.finite(s$weights$fraction[bad])|s$weights$fraction[bad]<=0|s$weights$fraction[bad]>1.001))stop("Fraction error exceeds precision correction scope")
g<-s$grid;dx<-(g$xmax-g$xmin)/g$ncols;dy<-(g$ymax-g$ymin)/g$nrows
cities<-st_make_valid(st_transform(st_read(cfg$cities,quiet=TRUE),6933))
changes<-list()
for(j in bad){
 w<-s$weights[j,];id<-s$regions$city_id[w$ID];region<-s$regions$region[w$ID]
 i<-match(id,as.character(cities[[cfg$city_id]]));stopifnot(!is.na(i))
 if(region=="CITY")poly<-st_geometry(cities[i,])else{
  z<-readRDS(file.path(cfg$reclamation_checkpoint_root,sprintf("city_%03d.rds",i)))
  stopifnot(all(z$city_id==id));poly<-st_union(st_geometry(st_transform(st_make_valid(z),6933)))
 }
 poly<-st_set_crs(st_make_valid(st_transform(poly,g$crs)),NA)
 row<-floor((w$cell-1)/g$ncols)+1;col<-(w$cell-1)%%g$ncols+1
 xlo<-g$xmin+(col-1)*dx;xhi<-xlo+dx;yhi<-g$ymax-(row-1)*dy;ylo<-yhi-dy
 cell<-st_sfc(st_polygon(list(matrix(c(xlo,ylo,xhi,ylo,xhi,yhi,xlo,yhi,xlo,ylo),ncol=2,byrow=TRUE))))
 clip<-suppressWarnings(st_intersection(poly,cell));fraction<-sum(as.numeric(st_area(clip)))/(dx*dy)
 if(!is.finite(fraction)||fraction<=0||fraction>1+1e-10||abs(fraction-w$fraction)>.001)stop("Independent fraction differs beyond precision scope")
 fraction<-min(1,fraction)
 s$weights$fraction[j]<-fraction;s$weights$area_km2[j]<-w$area_km2*fraction/w$fraction
 changes[[length(changes)+1L]]<-data.frame(weight_row=j,city_id=id,region=region,cell=w$cell,old_fraction=w$fraction,geos_fraction=fraction,old_area_km2=w$area_km2,new_area_km2=s$weights$area_km2[j])
}
s$precision_corrected_weight_rows<-bad
stopifnot(all(s$weights$fraction>0&s$weights$fraction<=1))
dir.create(a[2],recursive=TRUE);path<-file.path(a[2],"support.rds");saveRDS(s,path,compress=FALSE)
write.csv(s$input_checksums,file.path(a[2],"input_checksums.csv"),row.names=FALSE)
write.csv(s$gates,file.path(a[2],"reclamation_scope.csv"),row.names=FALSE)
if(length(changes))write.csv(do.call(rbind,changes),file.path(a[2],"fraction_precision_changes.csv"),row.names=FALSE)
marker$support_sha256<-digest(file=path,algo="sha256");marker$source_support_sha256<-digest(file=source,algo="sha256")
marker$precision_corrected_rows<-length(bad);marker$correction<-"Independent GEOS fractions for out-of-range rows; native cell area retained"
write_json(marker,file.path(a[2],"support_receipt.json"),auto_unbox=TRUE,pretty=TRUE)
cat("Precision support copied:",length(bad),"independently corrected rows\n")
