#!/usr/bin/env Rscript
# Check saved point memberships with a separate covered-by predicate.
suppressPackageStartupMessages({library(sf);library(jsonlite)})
cfg <- fromJSON(commandArgs(TRUE)[1],simplifyVector=FALSE)
out <- cfg$output_root
receipt <- file.path(out,"spatial_membership_verification.json")
if(file.exists(receipt))stop("Existing verification retained")
sf_use_s2(FALSE)
p <- readRDS(file.path(out,"polygons.rds"))
d <- read.csv(file.path(out,"candidate_records.csv"),stringsAsFactors=FALSE)
m <- read.csv(file.path(out,"polygon_record_membership.csv"),stringsAsFactors=FALSE)
ix <- unique(round(seq(1,nrow(d),length.out=min(256,nrow(d)))))
if(!length(ix))stop("No candidate points to check")
points <- st_transform(st_as_sf(d[ix,],coords=c("longitude","latitude"),crs=4326),st_crs(p))
hits <- st_covered_by(points,p)
errors <- list()
for(j in seq_along(ix)) {
 expected <- sort(as.character(p$polygon_id[hits[[j]]]))
 stored <- sort(as.character(m$polygon_id[m$record_id==ix[j]]))
 if(!identical(expected,stored))errors[[length(errors)+1L]] <- list(record_id=ix[j],expected=expected,stored=stored)
}
report <- list(status=if(!length(errors))"PASS_MEMBERSHIP_SPOT_CHECK" else "FAIL",sampled_points=length(ix),errors=errors,
 scope="Separate spatial predicate on saved polygon geometry; source-date and coordinate uncertainty unresolved")
write_json(report,receipt,pretty=TRUE,auto_unbox=TRUE)
cat(toJSON(report,pretty=TRUE,auto_unbox=TRUE))
if(length(errors))quit(status=1)
