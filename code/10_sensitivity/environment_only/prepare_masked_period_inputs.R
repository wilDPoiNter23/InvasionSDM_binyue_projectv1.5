# Replace saved category responses while retaining original polygons and weights.
suppressPackageStartupMessages({library(sf);library(terra);library(jsonlite)})
cfg<-fromJSON(commandArgs(TRUE)[1]);out<-cfg$output_root
stopifnot(!dir.exists(out)||!length(list.files(out,all.files=FALSE)))
dir.create(out,recursive=TRUE,showWarnings=FALSE);sf_use_s2(FALSE)
v<-readRDS(cfg$scaled_values);m<-readRDS(cfg$native_membership);a<-readRDS(cfg$city_support)
stopifnot(identical(a$cells,m$cells));pm<-match(v$paired_species,m$species)
products<-list(FULL_814=v$full,PAIRED_FULL_684=v$paired_full,PAIRED_ENV_684=v$paired_env)
for(g in names(products)){
 x<-products[[g]];mask<-if(g=='FULL_814')m$native else m$native[,pm,drop=FALSE]
 x[mask]<-NA_real_;p<-rowMeans(x,na.rm=TRUE);p[!is.finite(p)]<-NA_real_;products[[g]]<-p
}
template<-rast(nrows=4320,ncols=8640,xmin=-180,xmax=180,ymin=-90,ymax=90,crs='EPSG:4326')
values(template)<-0
records<-list();supports<-list();missing<-list()
for(layer in c('coastal_change','coastal_landuse')){
 files<-list.files(file.path(cfg$geometry_root,layer),pattern='[.]rds$',full.names=TRUE)
 for(f in files){
  z<-readRDS(f);if(is.null(z)||!nrow(z))next
  z<-z[!tolower(z$city_id)%in%tolower(cfg$exclude_city_keys),]
  if(!nrow(z))next
  z$area_km2<-as.numeric(st_area(z))/1e6
  e<-terra::extract(template,vect(st_transform(z,crs(template))),exact=TRUE,cells=TRUE)
  e<-e[is.finite(e$fraction)&e$fraction>0,];e$cache_index<-match(e$cell,a$cells)
  if(anyNA(e$cache_index))missing[[length(missing)+1L]]<-e[is.na(e$cache_index),]
  stopifnot(!anyNA(e$cache_index))
  for(i in seq_len(nrow(z))){
   w<-e[e$ID==i,];if(!nrow(w))next
   supports[[length(supports)+1L]]<-list(layer=layer,city_id=z$city_id[i],reported_category=z$reported_category[i],weights=w)
   for(g in names(products)){
    y<-products[[g]][w$cache_index];ok<-is.finite(y);ww<-w$fraction[ok]
    records[[length(records)+1L]]<-data.frame(layer=layer,response=g,city_id=z$city_id[i],reported_category=z$reported_category[i],mapped_area_km2=z$area_km2[i],native_cells=length(unique(w$cell[ok])),covered_cell_equivalents=sum(ww),effective_cells=if(sum(ww^2)>0)sum(ww)^2/sum(ww^2) else 0,mean_suitability=if(sum(ww)>0)sum(ww*y[ok])/sum(ww) else NA_real_)
   }
  }
 }
}
write.csv(do.call(rbind,records),file.path(out,'native_cell_category_suitability.csv'),row.names=FALSE)
saveRDS(supports,file.path(out,'category_weights.rds'))
file.copy(cfg$city_coverage,file.path(out,'city_coverage.csv'))
write_json(list(status='SAVED_GEOMETRY_RESPONSE_UPDATED',rule='Same per-species native exclusion and finite-species cell mean in FULL and ENV; original exact fractional-cell spatial weights retained',missing_cache_cells=0),file.path(out,'receipt.json'),auto_unbox=TRUE,pretty=TRUE)
