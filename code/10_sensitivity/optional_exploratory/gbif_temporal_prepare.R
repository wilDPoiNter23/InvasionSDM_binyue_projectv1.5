#!/usr/bin/env Rscript
# Prepare bounded polygon windows for a target-species first-record screen.
suppressPackageStartupMessages({library(sf);library(jsonlite);library(digest)})
args <- commandArgs(TRUE)
if(length(args)!=1L)stop("Supply a private JSON configuration")
cfg <- fromJSON(args[1],simplifyVector=FALSE)
out <- cfg$output_root
dir.create(out,recursive=TRUE,showWarnings=FALSE)
if(file.exists(file.path(out,"polygons.rds")))stop("Existing polygon preparation retained")
sf_use_s2(FALSE)
cohorts <- list();qc <- list()
files <- sort(list.files(cfg$coastal_checkpoint_dir,"^city_[0-9]+[.]rds$",full.names=TRUE))
if(!length(files))stop("No coastal geometry checkpoints")
for(f in files){
  q <- readRDS(f)
  if(is.null(q)||!nrow(q))next
  stopifnot(all(c("city_id","reported_category") %in% names(q)),st_crs(q)$epsg==6933)
  if(length(unique(q$city_id))!=1L)stop("Mixed city checkpoint")
  area <- sum(as.numeric(st_area(q)))/1e6
  mapped <- as.numeric(st_area(st_union(q)))/1e6
  excess <- max(0,area-mapped)
  endpoint <- suppressWarnings(as.integer(as.character(q$reported_category)))
  valid <- all(endpoint %in% seq(1990,2020,5))
  eligible <- valid && excess<=max(.001,.001*mapped)
  qc[[length(qc)+1L]] <- data.frame(city_id=q$city_id[1],mapped_area_km2=mapped,
    overlap_excess_km2=excess,valid_year_categories=valid,eligible=eligible,
    geometry_sha256=digest(file=f,algo="sha256"))
  if(!eligible)next
  q$polygon_id <- paste0("coastal:",q$city_id,":",endpoint)
  q$kind <- "coastal_dissolved_city_cohort"
  q$endpoint <- endpoint
  q$start_year <- endpoint-5L
  cohorts[[length(cohorts)+1L]] <- st_sf(st_drop_geometry(q)[,c("polygon_id","kind","city_id","endpoint","start_year")],geometry=st_geometry(q))
}
if(!length(cohorts))stop("No geometry-eligible coastal cohorts")
z <- do.call(rbind,cohorts)
if(!is.null(cfg$mining_polygons)){
  cities <- st_transform(st_make_valid(st_read(cfg$cities,quiet=TRUE)),6933)
  m <- st_read(cfg$mining_polygons,quiet=TRUE)
  stopifnot(cfg$mine_id_field %in% names(m),!anyDuplicated(m[[cfg$mine_id_field]]))
  m <- st_transform(st_make_valid(m),6933)
  selected <- lengths(st_intersects(m,st_union(cities)))>0
  m <- m[selected,]
  if(nrow(m)>cfg$maximum_mine_polygons)stop("Mine polygon bound exceeded")
  if(nrow(m)){
    h <- st_intersects(m,cities)
    m$polygon_id <- paste0("mine:",as.character(m[[cfg$mine_id_field]]))
    m$kind <- "mine_whole_footprint"
    m$city_id <- vapply(h,function(i)paste(as.character(cities[[cfg$city_id_field]][i]),collapse="|"),character(1))
    m$endpoint <- NA_integer_;m$start_year <- NA_integer_
    z <- rbind(z,st_sf(st_drop_geometry(m)[,c("polygon_id","kind","city_id","endpoint","start_year")],geometry=st_geometry(m)))
  }
}
stopifnot(!anyDuplicated(z$polygon_id))
ll <- st_transform(z,4326)
b <- t(vapply(st_geometry(ll),function(g)as.numeric(st_bbox(g)),numeric(4)))
if(any(b[,3]-b[,1]>180))stop("Dateline-spanning windows require explicit subdivision")
write.csv(data.frame(polygon_id=z$polygon_id,xmin=b[,1],ymin=b[,2],xmax=b[,3],ymax=b[,4]),
  file.path(out,"windows.csv"),row.names=FALSE)
write.csv(do.call(rbind,qc),file.path(out,"coastal_geometry_eligibility.csv"),row.names=FALSE)
write.csv(st_drop_geometry(z),file.path(out,"polygon_index.csv"),row.names=FALSE)
saveRDS(z,file.path(out,"polygons.rds"))
write_json(list(scope="Target-species first records; local native status unresolved",
  coastal_definition="Dissolved city by five-year cohort, not individual land parcels",
  mine_definition="Whole mine footprint; no dated reclaimed subpolygon available",
  coastal_checkpoint_files=length(files),eligible_cohorts=sum(z$kind=="coastal_dissolved_city_cohort"),
  retained_mines=sum(z$kind=="mine_whole_footprint"),
  configuration_sha256=digest(file=args[1],algo="sha256")),
  file.path(out,"preparation.json"),pretty=TRUE,auto_unbox=TRUE)
