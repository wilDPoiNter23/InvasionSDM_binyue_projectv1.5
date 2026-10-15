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
