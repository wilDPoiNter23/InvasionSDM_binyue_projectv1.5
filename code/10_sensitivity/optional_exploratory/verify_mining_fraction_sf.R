#!/usr/bin/env Rscript
# Check independent planar clipping with GEOS and fresh native raster reads.
suppressPackageStartupMessages({library(sf);library(terra);library(jsonlite)})
cfg <- fromJSON(commandArgs(TRUE)[1],simplifyVector=FALSE);out <- cfg$output_root
path <- file.path(out,"independent_sf_fraction_verification.json")
if(file.exists(path))stop("Existing verification retained")
d <- read.csv(file.path(out,"native_polygon_overlaps.csv"),check.names=FALSE)
grid <- fromJSON(file.path(out,"fraction_reconstruction.json"))$grid
indices <- unique(round(seq(1,nrow(d),length.out=min(256,nrow(d)))))
differences <- read.csv(file.path(out,"fraction_method_differences.csv"))
if(nrow(differences)) {
 worst <- differences[order(differences$absolute_difference,decreasing=TRUE)[seq_len(min(10,nrow(differences)))],]
 indices <- unique(c(indices,match(paste(worst$polygon_id,worst$cell),paste(d$polygon_id,d$cell))))
}
s <- d[indices,];v <- st_read(cfg$mining_shapefile,quiet=TRUE)
v <- st_set_crs(v[match(s$polygon_id,v[[cfg$polygon_id_field]]),],NA)
errors <- numeric(nrow(s))
for(i in seq_len(nrow(s))) {
 key <- s$cell[i]-1;col <- key %% grid$ncol;row <- floor(key/grid$ncol)
 x <- grid$xmin+col*grid$xres;y <- grid$ymax-row*grid$yres
 box <- matrix(c(x,y-grid$yres,x+grid$xres,y-grid$yres,x+grid$xres,y,x,y,x,y-grid$yres),ncol=2,byrow=TRUE)
 fraction <- sum(as.numeric(st_area(suppressWarnings(st_intersection(st_geometry(v[i,]),st_sfc(st_polygon(list(box))))))))/(grid$xres*grid$yres)
 errors[i] <- abs(fraction-s$fraction[i])
}
raster_ok <- TRUE
for(n in names(cfg$responses)) {
 r <- rast(cfg$responses[[n]])
 value <- terra::extract(r,s$cell)[,1]
 raster_ok <- raster_ok && isTRUE(all.equal(value,s[[n]],tolerance=1e-10,check.attributes=FALSE))
}
passed <- max(errors)<1e-7 && raster_ok
report <- list(status=if(passed)"PASS_GEOS_FRACTIONS_AND_RASTER_VALUES" else "FAIL",n_sampled_intersections=nrow(s),
 maximum_fraction_absolute_error=max(errors),native_raster_value_check=raster_ok,
 scope="Independent GEOS planar intersections and fresh native raster values; sampled geometry validation")
write_json(report,path,pretty=TRUE,auto_unbox=TRUE,digits=NA);cat(toJSON(report,pretty=TRUE,auto_unbox=TRUE))
if(!passed)quit(status=1)
