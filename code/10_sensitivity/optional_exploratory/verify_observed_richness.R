#!/usr/bin/env Rscript
# Reconstruct reported counts and check spatial membership independently.
suppressPackageStartupMessages({library(sf);library(jsonlite);library(digest)})
cfg <- fromJSON(commandArgs(TRUE)[1],simplifyVector=FALSE);out <- cfg$output_root
receipt <- file.path(out,"richness_verification.json")
if(file.exists(receipt))stop("Existing verification retained")
d <- read.csv(file.path(out,"candidate_records.csv"),stringsAsFactors=FALSE)
m <- read.csv(file.path(out,"city_record_membership.csv"),stringsAsFactors=FALSE)
r <- read.csv(file.path(out,"city_observed_richness.csv"),stringsAsFactors=FALSE)
w <- read.csv(file.path(out,"date_windows.csv"));p <- readRDS(file.path(out,"cities.rds"))
stopifnot(!anyDuplicated(d[,c("species","longitude","latitude","year")]),
 !anyDuplicated(m),all(m$record_id %in% seq_len(nrow(d))),all(m$city_id %in% p$city_id),
 nrow(r)==nrow(p)*nrow(w)*length(unique(r$scope)))
raw <- read.csv(file.path(out,"raw_species_roster.csv"),stringsAsFactors=FALSE)
stopifnot(!anyDuplicated(raw$species),all(d$species %in% raw$species))
extract <- fromJSON(file.path(out,"richness_extraction.json"))
stopifnot(sum(raw$source_rows)==extract$source_rows,nrow(raw)==extract$source_species,
 digest(file=cfg$database,algo="sha256")==extract$database_sha256)
for(scope in unique(r$scope)){
 taxa <- if(scope=="RAW_SELECTED_INVASIVE")raw$species else read.csv(file.path(out,paste0("roster_",scope,".csv")),stringsAsFactors=FALSE)$species
 for(i in which(r$scope==scope)){
  z <- d[m$record_id[m$city_id==r$city_id[i]],,drop=FALSE];a <- w[w$window==r$window[i],]
  good <- !is.na(z$year)&z$year>=a$start&z$year<=a$end&z$year>=1600&z$year<=2024&
   z$species %in% taxa&!(z$longitude==0&z$latitude==0)
  z <- z[which(good),,drop=FALSE]
  stopifnot(r$target_record_n[i]==nrow(z),r$source_rows[i]==sum(z$source_rows),r$sampled_year_n[i]==length(unique(z$year)))
  if(nrow(z))stopifnot(r$richness[i]==length(unique(z$species)),r$richness[i]<=r$roster_n[i],r$richness[i]<=r$target_record_n[i])else
   stopifnot(is.na(r$richness[i]),r$observation_support[i]=="UNKNOWN_OBSERVATION_SUPPORT")
 }
}
sf_use_s2(FALSE)
ix <- if(nrow(d))unique(round(seq(1,nrow(d),length.out=min(512,nrow(d)))))else integer()
if(length(ix)){
 points <- st_transform(st_as_sf(d[ix,],coords=c("longitude","latitude"),crs=4326),6933)
 hits <- st_covered_by(points,p)
 for(j in seq_along(ix))stopifnot(setequal(p$city_id[hits[[j]]],m$city_id[m$record_id==ix[j]]))
}
write_json(list(status="PASS_COUNTS_AND_MEMBERSHIP",summary_rows=nrow(r),membership_sample=length(ix),
 database_checksum_rechecked=TRUE,independent_predicate="covered-by",unknown_support_preserved=TRUE,
 numerical_associations_verified=FALSE,scientific_acceptance=FALSE),receipt,auto_unbox=TRUE,pretty=TRUE)
cat("PASS_COUNTS_AND_MEMBERSHIP",nrow(r),length(ix),"\n")
