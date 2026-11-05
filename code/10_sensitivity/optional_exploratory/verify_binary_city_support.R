#!/usr/bin/env Rscript
# Compare cached native weights with independent polygon clipping and ellipsoid areas.
suppressPackageStartupMessages({library(sf);library(jsonlite);library(digest)})
sf_use_s2(FALSE)
cfg<-fromJSON(commandArgs(TRUE)[1],simplifyVector=FALSE)
out<-cfg$city_support_directory;receipt<-file.path(out,"support_check.json")
if(file.exists(receipt))stop("Existing check retained")
path<-file.path(out,"support.rds");s<-readRDS(path)
marker<-fromJSON(file.path(out,"support_receipt.json"))
stopifnot(digest(file=path,algo="sha256")==marker$support_sha256,
 !anyDuplicated(paste(s$weights$ID,s$weights$cell)),!anyDuplicated(s$cells),
 all(s$weights$fraction>0&s$weights$fraction<=1+1e-10),
 all(s$weights$cell==s$cells[s$weights$cell_index]))
for(i in seq_len(nrow(s$input_checksums)))stopifnot(digest(file=s$input_checksums$path[i],algo="sha256")==s$input_checksums$sha256[i])
g<-s$grid;dx<-(g$xmax-g$xmin)/g$ncols;dy<-(g$ymax-g$ymin)/g$nrows
sample<-unique(c(round(seq(1,nrow(s$weights),length.out=32)),s$precision_corrected_weight_rows))
cities<-st_make_valid(st_transform(st_read(cfg$cities,quiet=TRUE),6933))
cache<-new.env(parent=emptyenv());results<-list()
e2<-.0066943799901413165;earth_a<-6378137;ecc<-sqrt(e2)
primitive<-function(u)u/(2*(1-e2*u^2))+atanh(ecc*u)/(2*ecc)
for(j in sample){
 w<-s$weights[j,];id<-s$regions$city_id[w$ID];region<-s$regions$region[w$ID];k<-paste(id,region)
 if(!exists(k,cache,inherits=FALSE)){
  i<-match(id,as.character(cities[[cfg$city_id]]));stopifnot(!is.na(i))
  if(region=="CITY")poly<-st_geometry(cities[i,])else{
   z<-readRDS(file.path(cfg$reclamation_checkpoint_root,sprintf("city_%03d.rds",i)))
   stopifnot(all(z$city_id==id));poly<-st_union(st_geometry(st_transform(st_make_valid(z),6933)))
  }
  poly<-st_make_valid(st_transform(poly,g$crs));poly<-st_set_crs(poly,NA)
  assign(k,poly,cache)
 }
 row<-floor((w$cell-1)/g$ncols)+1;col<-(w$cell-1)%%g$ncols+1
 xlo<-g$xmin+(col-1)*dx;xhi<-xlo+dx;yhi<-g$ymax-(row-1)*dy;ylo<-yhi-dy
 cell<-st_sfc(st_polygon(list(matrix(c(xlo,ylo,xhi,ylo,xhi,yhi,xlo,yhi,xlo,ylo),ncol=2,byrow=TRUE))))
 clip<-suppressWarnings(st_intersection(get(k,cache,inherits=FALSE),cell))
 fraction<-if(length(clip))sum(as.numeric(st_area(clip)))/(dx*dy)else 0
 area<-earth_a^2*(1-e2)*(dx*pi/180)*(primitive(sin(yhi*pi/180))-primitive(sin(ylo*pi/180)))/1e6
 relarea<-abs((w$area_km2/w$fraction)-area)/area
 results[[length(results)+1L]]<-data.frame(city_id=id,region=region,cell=w$cell,cached_fraction=w$fraction,geos_fraction=fraction,fraction_difference=abs(fraction-w$fraction),relative_native_cell_area_difference=relarea)
}
z<-do.call(rbind,results)
# Polygon fractions are planar; latitude-specific native areas retain ellipsoid units.
stopifnot(max(z$fraction_difference)<.001,max(z$relative_native_cell_area_difference)<.0001)
write.csv(z,file.path(out,"independent_fraction_samples.csv"),row.names=FALSE)
write_json(list(status="PASS",samples=nrow(z),native_weight_rows=nrow(s$weights),
 maximum_fraction_difference=max(z$fraction_difference),maximum_relative_native_area_difference=max(z$relative_native_cell_area_difference),
 fraction_tolerance=.001,native_area_tolerance=.0001,geometries="GEOS planar cell clipping",cell_area="Independent WGS84 ellipsoid latitude integral",
 support_sha256=digest(file=path,algo="sha256")),receipt,auto_unbox=TRUE,pretty=TRUE)
cat("Native fractions and ellipsoid cell areas PASS\n")
