# Extract uniform-boundary covariates without modifying manuscript polygons.
# Usage: Rscript extract_ghsl_city_covariates.R configuration.json
suppressPackageStartupMessages({library(sf);library(terra);library(jsonlite);library(digest)})
cfg<-fromJSON(commandArgs(TRUE)[1]);out<-cfg$output_root
if(dir.exists(out))stop('Existing spatial extraction retained; use a fresh output_root.')
dir.create(out,recursive=TRUE,showWarnings=FALSE)
dir.create(cfg$temp_root,recursive=TRUE,showWarnings=FALSE)
terraOptions(memfrac=.5,progress=0,tempdir=cfg$temp_root)
cross<-read.csv(cfg$crosswalk,stringsAsFactors=FALSE,na.strings='')
stopifnot(nrow(cross)==135,!anyDuplicated(cross$source_key))
matched<-cross[!is.na(cross$ghsl_ids),]
split_ids<-function(x)as.integer(strsplit(x,';',fixed=TRUE)[[1]])
ids<-sort(unique(unlist(lapply(matched$ghsl_ids,split_ids))))
g<-st_read(cfg$general_gpkg,layer='GHSL_UCDB_THEME_GENERAL_CHARACTERISTICS_GLOBE_R2024A',quiet=TRUE)
g<-g[match(ids,g$ID_UC_G0),];stopifnot(!anyNA(g$ID_UC_G0))
g<-st_make_valid(g)
stopifnot(all(st_is_valid(g)),all(as.numeric(st_area(g))>0))
st_write(g,file.path(out,'selected_ghsl_centres.gpkg'),layer='centres',quiet=TRUE,delete_layer=TRUE)

# GBIF requires anticlockwise exterior rings and clockwise holes in WGS84.
orient_ring<-function(a,outer){
 signed<-sum(a[-nrow(a),1]*a[-1,2]-a[-1,1]*a[-nrow(a),2])/2
 if((outer && signed<0)||(!outer && signed>0))a<-a[nrow(a):1,,drop=FALSE]
 a
}
orient_polygon<-function(p)lapply(seq_along(p),function(i)orient_ring(p[[i]],i==1))
ll<-st_transform(g,4326)
geom<-lapply(st_geometry(ll),function(x){
 if(inherits(x,'POLYGON'))st_polygon(orient_polygon(x))
 else if(inherits(x,'MULTIPOLYGON'))st_multipolygon(lapply(x,orient_polygon))
 else stop('Unexpected centre geometry type')
})
ll<-st_set_geometry(ll,st_sfc(geom,crs=4326))
wkt<-st_as_text(st_geometry(ll),digits=10)
write.csv(data.frame(ID_UC_G0=g$ID_UC_G0,geometry_wkt=wkt,area_km2=as.numeric(st_area(g))/1e6),
 file.path(out,'gbif_geometry.csv'),row.names=FALSE)

signatures<-sort(unique(matched$ghsl_ids))
unit_geom<-lapply(signatures,function(sig)st_union(st_geometry(g[g$ID_UC_G0 %in% split_ids(sig),]))[[1]])
units<-st_sf(unit_id=signatures,geometry=st_sfc(unit_geom,crs=st_crs(g)))
units<-st_make_valid(units);units$area_km2<-as.numeric(st_area(units))/1e6
center<-st_coordinates(st_transform(st_centroid(st_geometry(units)),4326))
units$longitude<-center[,1];units$latitude<-center[,2]
units$n_source_records<-vapply(signatures,function(x)sum(matched$ghsl_ids==x),integer(1))
units$group<-vapply(signatures,function(x){v<-unique(matched$source_group[matched$ghsl_ids==x]);if(length(v)==1)v else'Mixed'},character(1))
st_write(units,file.path(out,'uniform_city_units.gpkg'),layer='city_units',quiet=TRUE,delete_layer=TRUE)
cat('GEOMETRY_READY',nrow(units),'units',nrow(g),'centres\n');flush.console()

read_provider<-function(path){
 d<-read.csv(path,check.names=FALSE,stringsAsFactors=FALSE,encoding='UTF-8')
 names(d)<-sub('^\xef\xbb\xbf','',names(d),useBytes=TRUE)
 stopifnot('ID_UC_G0' %in% names(d),!anyDuplicated(d$ID_UC_G0),all(ids %in% d$ID_UC_G0))
 d
}
gh<-read_provider(cfg$ghsl_csv);soc<-read_provider(cfg$socioeconomic_csv)
stopifnot(all(c('GH_POP_TOT_2020','GH_BUS_TOT_2020') %in% names(gh)),
 all(c('SC_GDP_AVG_2020','GC_UCA_KM2_2025') %in% names(soc)))
data<-st_drop_geometry(units)
provider<-list()
for(i in seq_along(signatures)){
 use<-split_ids(signatures[i]);a<-gh[match(use,gh$ID_UC_G0),];b<-soc[match(use,soc$ID_UC_G0),]
 num<-function(x){z<-suppressWarnings(as.numeric(x));z[z<0]<-NA_real_;z}
 pop<-num(a$GH_POP_TOT_2020);built<-num(a$GH_BUS_TOT_2020)/1e6
 gd<-num(b$SC_GDP_AVG_2020);area<-num(b$GC_UCA_KM2_2025)
 total<-function(x)if(all(is.finite(x)))sum(x)else NA_real_
 unit_area<-data$area_km2[i]
 provider[[i]]<-data.frame(unit_id=signatures[i],ghsl_population_2020=total(pop),
  ghsl_population_density_2020=total(pop)/unit_area,ghsl_built_surface_km2_2020=total(built),
  ghsl_built_fraction_2020=total(built)/unit_area,
  ghsl_gdp_average_ppp_2020=if(all(is.finite(gd))&&all(is.finite(area)))weighted.mean(gd,area)else NA_real_)
}
data<-merge(data,do.call(rbind,provider),by='unit_id',sort=FALSE)

# Project-grid means use exact cell overlap times geodesic cell area.
r<-rast(cfg$predictor_stack);bands<-c('GDP2020_2.5min','Pop2020_2.5min','BUILT_S_E2025_2.5min','VIIRS_2023_2.5min')
stopifnot(all(bands %in% names(r)));r<-r[[bands]]
short<-c('project_gdp_mean_2020','project_population_mean_2020','project_built_mean_2025','night_light_mean_2023')
summary<-list();coverage<-list()
for(i in seq_len(nrow(units))){
 v<-vect(st_transform(units[i,],crs(r)));rr<-crop(r,ext(v),snap='out')
 ar<-cellSize(rr[[1]],unit='km',mask=FALSE);names(ar)<-'cell_area_km2'
 e<-terra::extract(c(rr,ar),v,exact=TRUE,cells=TRUE)
 stopifnot('fraction' %in% names(e))
 w<-e$fraction*e$cell_area_km2
 total_weight<-sum(w[is.finite(w) & w>0]);stopifnot(total_weight>0)
 values<-setNames(vector('list',length(bands)),short)
 for(j in seq_along(bands)){
  valid<-is.finite(e[[bands[j]]]) & is.finite(w) & w>0
  values[[j]]<-if(any(valid))weighted.mean(e[[bands[j]]][valid],w[valid])else NA_real_
  coverage[[length(coverage)+1L]]<-data.frame(unit_id=units$unit_id[i],variable=short[j],
   valid_cells=sum(valid),valid_area_km2=sum(w[valid]),geometry_area_km2=units$area_km2[i],
   grid_overlap_area_km2=total_weight,valid_area_fraction=sum(w[valid])/total_weight)
 }
 summary[[i]]<-data.frame(unit_id=units$unit_id[i],as.data.frame(values))
 if(i %% 20==0){cat('RASTER',i,'/',nrow(units),'\n');flush.console()}
}
data<-merge(data,do.call(rbind,summary),by='unit_id',sort=FALSE)
write.csv(do.call(rbind,coverage),file.path(out,'raster_coverage.csv'),row.names=FALSE)
write.csv(data,file.path(out,'uniform_covariates_before_reclamation.csv'),row.names=FALSE,na='')

# Measure mapped reclamation within each centre using the independent polygons.
cat('READING_RECLAMATION\n');flush.console()
coastal<-st_read(cfg$coastal_change,quiet=TRUE)
stopifnot(all(coastal$reclass1 %in% c(1990,1995,2000,2005,2010,2015,2020)))
coastal<-suppressWarnings(st_cast(coastal,'POLYGON'))
uv<-st_transform(units,st_crs(coastal));hits<-st_intersects(uv,coastal)
keep<-sort(unique(unlist(hits)));sub<-coastal[keep,];sub<-st_transform(st_make_valid(sub),st_crs(units))
hits<-lapply(hits,function(x)match(x,keep));reclaim<-list()
for(i in seq_len(nrow(units))){
 h<-hits[[i]];ha<-0
 if(length(h)){
  combined<-st_union(st_geometry(sub[h,]));inter<-suppressWarnings(st_intersection(combined,st_geometry(units[i,])))
  ha<-if(length(inter))sum(as.numeric(st_area(inter)))/1e4 else 0
 }
 reclaim[[i]]<-data.frame(unit_id=units$unit_id[i],mapped_reclamation_ha=ha,
  mapped_reclamation_fraction=ha/(100*units$area_km2[i]),n_intersecting_components=length(h))
 if(i %% 20==0){cat('RECLAMATION',i,'/',nrow(units),'\n');flush.console()}
}
data<-merge(data,do.call(rbind,reclaim),by='unit_id',sort=FALSE)
stopifnot(all(data$mapped_reclamation_fraction>=0),all(data$mapped_reclamation_fraction<=1+1e-6),
 all(data$ghsl_built_fraction_2020>=0,na.rm=TRUE),all(data$ghsl_built_fraction_2020<=1+1e-6,na.rm=TRUE))
write.csv(data,file.path(out,'uniform_unit_covariates.csv'),row.names=FALSE,na='')
city<-merge(cross,data,by.x='ghsl_ids',by.y='unit_id',all.x=TRUE,sort=FALSE)
stopifnot(nrow(city)==135);write.csv(city,file.path(out,'uniform_source_city_covariates.csv'),row.names=FALSE,na='')
inputs<-unlist(cfg[c('general_gpkg','crosswalk','ghsl_csv','socioeconomic_csv','predictor_stack','coastal_change')])
write.csv(data.frame(input=names(inputs),path=unname(inputs),sha256=vapply(inputs,function(p)digest(file=p,algo='sha256'),character(1))),file.path(out,'spatial_input_checksums.csv'),row.names=FALSE)
write_json(list(status='SPATIAL_EXTRACTION_COMPLETED',source_records=135,matched_records=nrow(matched),
 unique_units=nrow(units),ghsl_centres=nrow(g),boundary_epoch=2025,release='GHS-UCDB R2024A V1.2',
 original_boundaries_modified=FALSE,sdm_fits=0,source_group_table='106 linked and 29 unlinked source records',
 mapped_reclamation='Independent five-year reclamation polygons, endpoints 1990-2020; zero denotes no mapped intersection within this boundary',
 provider_gdp='Published average GDP PPP grid indicator (SC_GDP_AVG_2020), not municipal GDP accounts or a verified per-capita measure',
 grid_summary='Exact overlap fraction multiplied by geodesic cell area; genuinely missing grid values excluded with coverage reported'),file.path(out,'spatial_receipt.json'),auto_unbox=TRUE,pretty=TRUE)
writeLines(capture.output(sessionInfo()),file.path(out,'spatial_sessionInfo.txt'))
