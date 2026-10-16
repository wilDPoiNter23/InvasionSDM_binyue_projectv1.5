# Numerical regression checks for missing-value, binary and city-coverage rules.
suppressPackageStartupMessages({library(terra);library(sf);library(jsonlite)})
a<-commandArgs(TRUE);stopifnot(length(a)==1L);script<-normalizePath(a[1],mustWork=TRUE)
root<-tempfile('processing_');dir.create(root)
r<-rast(nrows=2,ncols=6,xmin=0,xmax=6,ymin=0,ymax=2,crs='EPSG:4326')
f1<-c(800,200,NA,700,100,NA,800,200,NA,700,100,NA)
f2<-c(600,300,NA,500,100,NA,600,300,NA,500,100,NA)
e1<-f1;e1[1]<-NA;e2<-f2*.5
put<-function(v,name){z<-r;values(z)<-v;p<-file.path(root,paste0(name,'.tif'));writeRaster(z,p);p}
paths<-c(put(f1,'first'),put(f2,'second'),put(e1,'envfirst'),put(e2,'envsecond'))
bin<-put(ifelse(is.finite(f2),as.numeric(f2>=500),NA),'second_TSSbin')
rectangle<-function(x0,x1)st_polygon(list(rbind(c(x0,0),c(x1,0),c(x1,2),c(x0,2),c(x0,0))))
cities<-st_sf(name=c('One','Two','Three'),geometry=st_sfc(rectangle(0,2),rectangle(2,4),rectangle(4,6),crs=4326))
reclaim<-st_sf(geometry=st_sfc(rectangle(0,1),rectangle(2,3),crs=4326))
citypath<-file.path(root,'cities.gpkg');recpath<-file.path(root,'reclaim.gpkg');st_write(cities,citypath,quiet=TRUE);st_write(reclaim,recpath,quiet=TRUE)
paired<-data.frame(species=c('a','b'),full_projection=paths[1:2],env_projection=paths[3:4]);pm<-file.path(root,'paired.csv');write.csv(paired,pm,row.names=FALSE)
om<-file.path(root,'original.csv');write.csv(data.frame(accepted_taxon=c('a','b'),original_projection_path=c(paths[1],bin)),om,row.names=FALSE)
im<-file.path(root,'inventory.tsv');write.table(data.frame(accepted_taxon=c('a','b'),baseline_global_projection_path=paths[1:2]),im,sep='\t',row.names=FALSE,quote=FALSE)
legacy<-ifelse(is.finite(f1),(f1-100)/700,0)/2+ifelse(is.finite(f2),as.numeric(f2>=500),0)/2
arch<-put(legacy,'archive');cfg<-list(output_root=file.path(root,'results'),cities=citypath,city_id='name',reclamation_sources=list(HISTORICAL=recpath),surfaces=list(ARCHIVE=arch),historical_surface_label='ARCHIVE',paired_manifest=pm,original_manifest=om,full_inventory=im,exclude_city_keys=character(),workers=1,bootstrap_replicates=49,seed=42)
cp<-file.path(root,'config.json');write_json(cfg,cp,auto_unbox=TRUE)
exe<-file.path(R.home('bin'),'Rscript')
for(phase in c('prepare','extract','summarise'))stopifnot(system2(exe,c(shQuote(script),shQuote(cp),phase))==0)
d<-readRDS(file.path(cfg$output_root,'cell_values.rds'));stopifnot(max(abs(d$values$ARCHIVE-d$values$HISTORICAL_SELECTED__MINMAX__ZERO_FIXED))<1e-7)
stopifnot(all(is.finite(d$values$ALL_FULL_CONTINUOUS__SCALE__ZERO_FIXED)),any(is.na(d$values$ALL_FULL_CONTINUOUS__SCALE__AVAILABLE)))
expected<-(f1/1000+f2/1000)/2;got<-d$values$PAIRED_FULL__SCALE__AVAILABLE
stopifnot(identical(is.finite(expected),is.finite(got)),max(abs(expected-got),na.rm=TRUE)<1e-7)
g<-read.csv(file.path(cfg$output_root,'city_geometry_status.csv'));stopifnot(length(unique(g$city_id))==3,!any(g$status=='GEOMETRY_ERROR'),sum(g$status=='ZERO_GEOMETRY')==1L)
cat('PASS: binary selection, global normalization, missing-value denominators, joint membership and all-city geometry\n')
