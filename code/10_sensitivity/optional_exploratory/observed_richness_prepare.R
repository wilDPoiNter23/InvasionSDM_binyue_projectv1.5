#!/usr/bin/env Rscript
# Prepare full-city polygons without selecting reclaimed or mined areas.
suppressPackageStartupMessages({library(sf);library(jsonlite);library(digest)})
cfg <- fromJSON(commandArgs(TRUE)[1],simplifyVector=FALSE)
out <- cfg$output_root
dir.create(out,recursive=TRUE,showWarnings=FALSE)
if(file.exists(file.path(out,"cities.rds")))stop("Existing preparation retained")
sf_use_s2(FALSE)
cities <- st_read(cfg$cities,quiet=TRUE)
id <- as.character(cities[[cfg$city_id_field]])
if(length(id)!=cfg$expected_city_count || anyNA(id) || any(!nzchar(id)) || anyDuplicated(id))stop("City identity contract failed")
original_valid <- st_is_valid(cities)
cities <- st_transform(st_make_valid(cities),6933)
cities$city_id <- id
if(any(st_is_empty(cities)))stop("Empty city geometry")
ll <- st_transform(cities,4326)
b <- t(vapply(st_geometry(ll),function(g)as.numeric(st_bbox(g)),numeric(4)))
if(any(b[,3]-b[,1]>180))stop("Dateline subdivision required")
write.csv(data.frame(city_id=id,xmin=b[,1],ymin=b[,2],xmax=b[,3],ymax=b[,4]),file.path(out,"windows.csv"),row.names=FALSE)
write.csv(data.frame(city_id=id,area_geometry_km2=as.numeric(st_area(cities))/1e6,original_valid=original_valid),file.path(out,"city_geometry.csv"),row.names=FALSE)
saveRDS(cities[,"city_id"],file.path(out,"cities.rds"))
write_json(list(city_count=length(id),configuration_sha256=digest(file=commandArgs(TRUE)[1],algo="sha256"),
 geometry_predicate="Projected full-city polygons; boundaries included and multiple memberships retained",
 shapefile_components=lapply(sort(list.files(dirname(cfg$cities),paste0("^",tools::file_path_sans_ext(basename(cfg$cities)),"[.]"),full.names=TRUE)),function(f)list(file=basename(f),sha256=digest(file=f,algo="sha256")))),file.path(out,"richness_preparation.json"),pretty=TRUE,auto_unbox=TRUE)
