#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(sf);library(jsonlite)})
cfg <- fromJSON(commandArgs(TRUE)[1],simplifyVector=FALSE)
out <- cfg$association_output
target <- file.path(out,"independent_joint_verification.json")
if(file.exists(target))stop("Existing verification retained")
sf_use_s2(FALSE)
checks <- list()
check <- function(name,value)checks[[length(checks)+1L]] <<- list(name=name,passed=isTRUE(value))
paths <- list.files(file.path(cfg$joint_output,"joint_checkpoints"),pattern="[.]rds$",full.names=TRUE)
means_ok <- overlap_ok <- support_ok <- TRUE
for(p in paths) {
 z <- readRDS(p)
 if(z$status!="EXTRACTED")next
 area <- as.numeric(st_area(z$geometry))/1e6
 union_area <- as.numeric(st_area(st_union(st_geometry(z$geometry))))/1e6
 overlap_ok <- overlap_ok && sum(area)-union_area<=max(.001,.001*union_area)+1e-8
 for(i in seq_len(nrow(z$summary))) {
  e <- z$cells[z$cells$ID==i,]
  w <- e$fraction
  s <- z$summary[i,]
  support_ok <- support_ok && s$native_cells==length(unique(e$cell)) && abs(s$covered_cell_equivalents-sum(w))<1e-8
  for(n in names(cfg$responses)) means_ok <- means_ok && isTRUE(all.equal(s[[n]],sum(w*e[[n]])/sum(w),tolerance=1e-10))
 }
}
check("Checkpoint native means reconstructed",means_ok)
check("Checkpoint support reconstructed",support_ok)
check("Joint category overlap gate",overlap_ok)
d <- read.csv(file.path(cfg$joint_output,"joint_native_summary.csv"),check.names=FALSE)
results <- read.csv(file.path(out,"within_city_use_cohort_association.csv"))
point_ok <- TRUE
for(i in seq_len(nrow(results))) {
 t <- results[i,]; q <- d
 if(t$scope=="native_support")q <- q[q$native_cells>=3 & q$effective_cells>=2,]
 q$x <- (q$period_endpoint-1990)/5
 q$w <- q$area_km2/ave(q$area_km2,q$city_id,FUN=sum)
 q$y <- q[[t$response]]
 xx <- xy <- 0
 for(g in split(q,paste(q$city_id,q$landuse))) {
  x <- g$x-sum(g$w*g$x)/sum(g$w)
  y <- g$y-sum(g$w*g$y)/sum(g$w)
  xx <- xx+sum(g$w*x*x);xy <- xy+sum(g$w*x*y)
 }
 if(is.finite(t$slope_per_later_five_year_cohort))point_ok <- point_ok && abs(xy/xx-t$slope_per_later_five_year_cohort)<1e-10
}
check("Within-city conditional slopes reconstructed from category summaries",point_ok)
passed <- all(vapply(checks,function(x)x$passed,logical(1)))
report <- list(status=if(passed)"PASS_JOINT_NUMERIC_SCOPE" else "FAIL",checks=checks,
 scope="Checkpoint weighted means, category overlap and conditional slope points; not independent raster reads or bootstrap replication",scientific_acceptance=FALSE)
write_json(report,target,pretty=TRUE,auto_unbox=TRUE)
cat(toJSON(report,pretty=TRUE,auto_unbox=TRUE))
if(!passed)quit(status=1)