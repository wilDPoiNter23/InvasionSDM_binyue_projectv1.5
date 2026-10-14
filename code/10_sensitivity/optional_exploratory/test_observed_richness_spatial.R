#!/usr/bin/env Rscript
# Synthetic full-city memberships and independent richness reconstruction.
suppressPackageStartupMessages({library(sf);library(jsonlite)})
args <- commandArgs(TRUE);out <- args[1];scripts <- args[2]
box <- function(x0,y0,x1,y1)st_polygon(list(matrix(c(x0,y0,x1,y0,x1,y1,x0,y1,x0,y0),ncol=2,byrow=TRUE)))
# Shared edge at longitude 2 tests inclusion and multiple-city support.
p <- st_sf(city_id=c("C1","C2","C3"),geometry=st_sfc(box(0,0,2,2),box(2,0,4,2),box(10,10,11,11),crs=4326))
saveRDS(st_transform(p,6933),file.path(out,"cities.rds"))
cfg <- fromJSON(file.path(out,"config.json"),simplifyVector=FALSE)
cfg$rarefaction_n <- 2L
write_json(cfg,file.path(out,"config.json"),auto_unbox=TRUE)
status <- system2(file.path(R.home("bin"),"Rscript"),c("--vanilla",shQuote(file.path(scripts,"observed_richness_join.R")),shQuote(file.path(out,"config.json"))))
stopifnot(status==0L)
r <- read.csv(file.path(out,"city_observed_richness.csv"),stringsAsFactors=FALSE)
d <- read.csv(file.path(out,"candidate_records.csv"),stringsAsFactors=FALSE)
sf_use_s2(FALSE)
pts <- st_transform(st_as_sf(d,coords=c("longitude","latitude"),crs=4326,remove=FALSE),6933)
hits <- st_covered_by(pts,readRDS(file.path(out,"cities.rds")))
stored <- read.csv(file.path(out,"city_record_membership.csv"),stringsAsFactors=FALSE)
for(i in seq_len(nrow(d)))stopifnot(setequal(p$city_id[hits[[i]]],stored$city_id[stored$record_id==i]))
windows <- read.csv(file.path(out,"date_windows.csv"))
for(i in seq_len(nrow(r))){
 w <- windows[windows$window==r$window[i],]
 ids <- which(vapply(hits,function(h)match(r$city_id[i],p$city_id) %in% h,logical(1)))
 a <- d[ids,,drop=FALSE];a <- a[which(!is.na(a$year)&a$year>=w$start&a$year<=w$end&!(a$longitude==0&a$latitude==0)),,drop=FALSE]
 stopifnot(r$target_record_n[i]==nrow(a))
 if(nrow(a))stopifnot(r$richness[i]==length(unique(a$species)))else stopifnot(is.na(r$richness[i]),r$observation_support[i]=="UNKNOWN_OBSERVATION_SUPPORT")
}
stopifnot(r$richness[r$city_id=="C1"&r$window=="ALL_DATED"]==3L,
 r$target_record_n[r$city_id=="C1"&r$window=="ALL_DATED"]==4L,
 r$richness[r$city_id=="C2"&r$window=="common2010_2024"]==1L,
 all(is.na(r$richness[r$city_id=="C3"])))
rare <- read.csv(file.path(out,"city_rarefaction.csv"))
stopifnot(abs(rare$expected_richness[rare$city_id=="C1"&rare$window=="ALL_DATED"]-11/6)<1e-10)
write_json(list(status="PASS",spatial_predicate="Independent covered-by",checked_rows=nrow(r),boundary_included=TRUE,
 duplicate_collapsed=TRUE,missing_invalid_years_excluded=TRUE,unsupported_richness_missing=TRUE,analytic_rarefaction_checked=TRUE),file.path(out,"synthetic_verification.json"),auto_unbox=TRUE,pretty=TRUE)
cat("PASS_SPATIAL_COUNTS_SUPPORT_RAREFACTION",nrow(r),"\n")
