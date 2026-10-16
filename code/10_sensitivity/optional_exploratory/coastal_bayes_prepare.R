# Freeze native-area support for city-by-cohort-by-use polygons.
suppressPackageStartupMessages({library(sf);library(terra);library(jsonlite);library(digest)})
a<-commandArgs(TRUE);stopifnot(length(a)==1L);cfg<-fromJSON(a[1]);out<-cfg$output_root
stopifnot(!dir.exists(out));dir.create(out,recursive=TRUE);sf_use_s2(FALSE)
manifest<-read.csv(cfg$paired_manifest);stopifnot(nrow(manifest)==684L,!anyDuplicated(manifest$species))
template<-rast(manifest$env_projection[1]);stopifnot(is.lonlat(template),nlyr(template)==1)
stripe<-rast(nrows=nrow(template),ncols=1,xmin=xmin(template),xmax=xmin(template)+res(template)[1],ymin=ymin(template),ymax=ymax(template),crs=crs(template))
rowarea<-as.numeric(values(cellSize(stripe,unit='km')))
cv<-read.csv(file.path(cfg$joint_root,'joint_city_summary.csv'));ids<-read.csv(cfg$city_input)$city
key<-function(x)tolower(trimws(x));exclude<-key(cfg$exclude_city_keys)
rows<-list();weights<-list();sources<-list();k<-0L
files<-list.files(file.path(cfg$joint_root,'joint_checkpoints'),pattern='[.]rds$',full.names=TRUE)
for(f in files){z<-readRDS(f);if(is.null(z$city)||is.null(z$geometry)||!nrow(z$geometry))next
 city<-as.character(z$city$city_id[1]);if(key(city)%in%exclude)next
 geom<-z$geometry;attrs<-st_drop_geometry(geom);stopifnot(all(attrs$area_km2>0))
 e<-terra::extract(template,vect(st_transform(geom,crs(template))),exact=TRUE,cells=TRUE)
 e<-e[is.finite(e$fraction)&e$fraction>0,];e$weight_km2<-e$fraction*rowarea[rowFromCell(template,e$cell)]
 good<-sort(unique(e$ID));newid<-k+seq_along(good);lookup<-setNames(newid,good)
 attrs<-attrs[good,,drop=FALSE];attrs$zone_id<-newid;attrs$city_id<-city
 attrs$age_index_years<-cfg$age_reference_year-attrs$period_endpoint
 rows[[length(rows)+1L]]<-attrs
 weights[[length(weights)+1L]]<-data.frame(zone_id=unname(lookup[as.character(e$ID)]),cell=e$cell,weight_km2=e$weight_km2)
 k<-k+length(good);sources[[length(sources)+1L]]<-data.frame(path=f,sha256=digest(file=f,algo='sha256'))
}
zones<-do.call(rbind,rows);w<-do.call(rbind,weights);cells<-sort(unique(w$cell));w$cell_index<-match(w$cell,cells)
stopifnot(!anyDuplicated(paste(zones$city_id,zones$period_endpoint,zones$landuse)),all(w$weight_km2>0))
saveRDS(list(zones=zones,weights=w,cells=cells,grid=template),file.path(out,'native_support.rds'))
write.csv(zones,file.path(out,'zones.csv'),row.names=FALSE);write.csv(do.call(rbind,sources),file.path(out,'geometry_sources.csv'),row.names=FALSE)
write_json(list(status='NATIVE_SUPPORT_PREPARED',cities=length(unique(zones$city_id)),zones=nrow(zones),cells=length(cells),
 species_manifest_sha256=digest(file=cfg$paired_manifest,algo='sha256'),support_sha256=digest(file=file.path(out,'native_support.rds'),algo='sha256'),
 age_definition='Years before reference minus reported cohort endpoint; contrasts between five-year cohorts',
 support='Each species and response uses its own finite cells; no minimum cell threshold or cross-response intersection'),file.path(out,'support_receipt.json'),auto_unbox=TRUE,pretty=TRUE)
