# Reuse saved cohort support with consistently scaled, native-excluded predictions.
suppressPackageStartupMessages({library(jsonlite);library(digest)})
cfg<-fromJSON(commandArgs(TRUE)[1]);out<-cfg$output_root
stopifnot(!dir.exists(out));dir.create(out,recursive=TRUE)
v<-readRDS(cfg$scaled_values);m<-readRDS(cfg$native_membership)
a<-readRDS(cfg$city_support);s<-readRDS(cfg$coastal_support)
stopifnot(identical(a$cells,m$cells),all(s$cells%in%a$cells))
ix<-match(s$cells,a$cells);pm<-match(v$paired_species,m$species)
stopifnot(!anyNA(pm));mask<-m$native[ix,pm,drop=FALSE]
groups<-list(ENV=v$paired_env[ix,,drop=FALSE],FULL=v$paired_full[ix,,drop=FALSE])
if(!is.null(cfg$group_names)&&identical(cfg$group_names,'FULL_814')){
 groups<-list(FULL_814=v$full[ix,,drop=FALSE]);mask<-m$native[ix,,drop=FALSE]
 v$paired_species<-m$species
}
saveRDS(s,file.path(out,'native_support.rds'));write.csv(s$zones,file.path(out,'zones.csv'),row.names=FALSE)
J<-nrow(s$zones);w<-s$weights
sumzone<-function(x){r<-rowsum(x,w$zone_id,reorder=FALSE);z<-numeric(J);z[as.integer(rownames(r))]<-r[,1];z}
cell_products<-list();qc<-list()
for(g in names(groups)){
 x<-groups[[g]];stopifnot(all(x[is.finite(x)]>=0&x[is.finite(x)]<=1))
 missing<-!is.finite(x)&!mask;x[mask]<-NA_real_
 dir.create(file.path(out,'species',g),recursive=TRUE)
 for(i in seq_along(v$paired_species)){
  y<-x[w$cell_index,i];ok<-is.finite(y)
  den<-sumzone(ifelse(ok,w$weight_km2,0));num<-sumzone(ifelse(ok,y*w$weight_km2,0))
  d<-s$zones;d$species<-v$paired_species[i];d$group<-g
  d$finite_area_km2<-den;d$mean_suitability<-ifelse(den>0,num/den,NA_real_)
  write.csv(d,file.path(out,'species',g,sprintf('species_%04d.csv',i)),row.names=FALSE)
 }
 z<-rowMeans(x,na.rm=TRUE);z[!is.finite(z)]<-NA_real_;cell_products[[g]]<-z
 qc[[g]]<-data.frame(group=g,species=ncol(x),cells=nrow(x),native_excluded=sum(mask),genuine_missing=sum(missing),remaining_finite=sum(is.finite(x)),masked_values_remaining=sum(is.finite(x[mask])))
}
write.csv(do.call(rbind,qc),file.path(out,'mask_coverage.csv'),row.names=FALSE)
# The aggregate surface averages species within cells before spatial averaging.
joint<-read.csv(cfg$previous_joint);city<-read.csv(cfg$previous_city)
if(identical(names(groups),'FULL_814')){
 joint<-joint[,!grepl('^PAIRED_',names(joint)),drop=FALSE]
 city<-city[,!grepl('^PAIRED_',names(city)),drop=FALSE]
}
summarize_cells<-function(ww,ids){
 result<-data.frame(id=ids)
 for(g in names(cell_products)){
  z<-cell_products[[g]][ww$cell_index];ok<-is.finite(z)
  den<-tapply(ifelse(ok,ww$weight_km2,0),ww$id,sum)
  num<-tapply(ifelse(ok,z*ww$weight_km2,0),ww$id,sum)
  name<-if(g=='FULL_814')g else paste0('PAIRED_',g,'_684')
  result[[name]]<-as.numeric((num/den)[as.character(ids)])
 }
 result
}
ww<-w;ww$id<-ww$zone_id;means<-summarize_cells(ww,s$zones$zone_id)
key<-function(d)paste(d$city_id,d$period_endpoint,d$landuse,sep='|')
j<-match(key(joint),key(s$zones));joint<-joint[!is.na(j),];j<-j[!is.na(j)]
for(n in names(means)[-1])joint[[n]]<-means[[n]][j]
ww$id<-s$zones$city_id[match(w$zone_id,s$zones$zone_id)]
cm<-summarize_cells(ww,unique(ww$id));city<-city[city$city_id%in%cm$id,]
for(n in names(cm)[-1])city[[n]]<-cm[[n]][match(city$city_id,cm$id)]
dir.create(file.path(out,'aggregate_inputs'))
write.csv(joint,file.path(out,'aggregate_inputs','joint_native_summary.csv'),row.names=FALSE)
write.csv(city,file.path(out,'aggregate_inputs','joint_city_summary.csv'),row.names=FALSE)
write_json(list(status='UPDATED_SAVED_SUPPORT',native_exclusion='Same species masks for FULL and ENV; native cells and genuine missing predictions excluded rather than zero-filled',species_pool=length(v$paired_species),cities=length(unique(s$zones$city_id)),zones=J,cells=length(ix),species_summary='Finite non-native area weighted mean per species and zone',aggregate_summary='Finite non-native species mean within cell, then area weighting',source_sha256=vapply(c(cfg$scaled_values,cfg$native_membership,cfg$coastal_support),function(f)digest(file=f,algo='sha256'),character(1))),file.path(out,'aggregate_inputs','extraction_complete.json'),auto_unbox=TRUE,pretty=TRUE)
