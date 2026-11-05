# Polygon area excludes boundary-only contacts and preserves true overlap.
suppressPackageStartupMessages(library(sf))
a<-commandArgs(TRUE);if(length(a)!=1L)stop("Supply geometry helper source")
source(a[1]);sf_use_s2(FALSE)
square<-function(x)st_polygon(list(rbind(c(x,0),c(x+1,0),c(x+1,1),c(x,1),c(x,0))))
p<-st_sfc(square(0),crs=6933)
stopifnot(common_polygon_area(st_sfc(crs=6933))==0,
 common_polygon_area(st_sfc(st_geometrycollection(),crs=6933))==0,
 common_polygon_area(st_intersection(p,st_sfc(square(1),crs=6933)))==0,
 common_polygon_area(st_intersection(p,st_sfc(square(3),crs=6933)))==0,
 abs(common_polygon_area(st_intersection(p,st_sfc(square(.5),crs=6933)))-.5)<1e-12)
mixed<-st_sfc(st_geometrycollection(list(square(0),st_linestring(rbind(c(2,0),c(2,1))),st_point(c(3,3)))),crs=6933)
stopifnot(common_polygon_area(mixed)==1)
cat("PASS polygon, mixed collection, empty, touching, disjoint and real overlap\n")

# Nonlinear coordinate transformations must precede partition differences.
city<-st_sfc(st_polygon(list(rbind(c(-5,50),c(5,50),c(5,65),c(-5,50)))),crs=4326)
city_m<-st_transform(city,6933)
clip<-st_sfc(st_polygon(list(rbind(c(-10,54),c(10,54),c(10,60),c(-10,60),c(-10,54)))),crs=4326)
rec_m<-suppressWarnings(st_intersection(city_m,st_transform(clip,6933)))
w<-function(x)st_set_crs(st_transform(x,4326),NA)
partition<-common_native_partition(w(city_m),w(rec_m),w(st_buffer(city_m,50000)),w(city_m))
for(pair in list(c(1,2),c(1,3),c(2,3)))stopifnot(common_polygon_area(suppressWarnings(st_intersection(partition[pair[1]],partition[pair[2]])))<1e-12)
stopifnot(abs(common_polygon_area(partition[2:3])-common_polygon_area(w(city_m)))<1e-10)
cat("PASS native partition under nonlinear transformation: disjoint and area conserved\n")
