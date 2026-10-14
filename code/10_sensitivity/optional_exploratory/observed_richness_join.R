#!/usr/bin/env Rscript
# Count observed richness with explicit date windows and observation support.
suppressPackageStartupMessages({library(sf);library(jsonlite);library(digest)})
cfg <- fromJSON(commandArgs(TRUE)[1],simplifyVector=FALSE);out <- cfg$output_root
if(file.exists(file.path(out,"richness_summary.json")))stop("Existing summary retained")
sf_use_s2(FALSE)
cities <- readRDS(file.path(out,"cities.rds"))
d <- read.csv(file.path(out,"candidate_records.csv"),stringsAsFactors=FALSE)
flag <- function(x)tolower(as.character(x))=="true"
d$valid_year <- flag(d$valid_year);d$zero_zero <- flag(d$zero_zero);d$record_id <- seq_len(nrow(d))
if(nrow(d)){
 pts <- st_transform(st_as_sf(d,coords=c("longitude","latitude"),crs=4326,remove=FALSE),6933)
 hits <- st_intersects(pts,cities)
}else hits <- list()
d$city_memberships <- lengths(hits)
members <- data.frame(city_id=rep(cities$city_id,0),record_id=integer())
if(sum(lengths(hits)))members <- data.frame(city_id=cities$city_id[unlist(hits)],record_id=rep(seq_len(nrow(d)),lengths(hits)))
write.csv(members,file.path(out,"city_record_membership.csv"),row.names=FALSE)
raw <- read.csv(file.path(out,"raw_species_roster.csv"),stringsAsFactors=FALSE)$species
scopes <- list(RAW_SELECTED_INVASIVE=raw)
scope_sources <- list(RAW_SELECTED_INVASIVE=list(source="raw_species_roster.csv",sha256=digest(file=file.path(out,"raw_species_roster.csv"),algo="sha256")))
if(length(cfg$frozen_scopes))for(s in cfg$frozen_scopes){
 r <- read.csv(s$path,stringsAsFactors=FALSE)
 if(!is.null(s$asset_role))r <- r[r$asset_role==s$asset_role & tolower(as.character(r$used_in_paired_sensitivity)) %in% c("true","1","yes"),,drop=FALSE]
 taxa <- trimws(r[[s$species_column]])
 if(length(taxa)!=s$expected_count || anyNA(taxa) || anyDuplicated(taxa) || any(!nzchar(taxa)) || any(!taxa %in% raw))stop("Frozen roster contract failed")
 scopes[[s$name]] <- taxa
 scope_sources[[s$name]] <- list(source_sha256=digest(file=s$path,algo="sha256"),species_column=s$species_column,asset_role=s$asset_role)
 write.csv(data.frame(species=taxa),file.path(out,paste0("roster_",s$name,".csv")),row.names=FALSE)
}
windows <- data.frame(window=c("ALL_DATED","post2000","post2010","common2010_2024"),start=c(1600L,2001L,2011L,2010L),end=2024L)
write.csv(windows,file.path(out,"date_windows.csv"),row.names=FALSE)
result <- list();qa <- list();counts <- list();rare <- list()
for(i in seq_len(nrow(cities))){
 x <- d[members$record_id[members$city_id==cities$city_id[i]],,drop=FALSE]
 qa[[i]] <- data.frame(city_id=cities$city_id[i],candidate_unique_records=nrow(x),source_rows=sum(x$source_rows),
  missing_year_records=sum(is.na(x$year)),invalid_year_records=sum(!is.na(x$year)&!x$valid_year),
  multiple_city_records=sum(x$city_memberships>1),country_conflict_records=sum(x$country_count>1))
 for(scope in names(scopes))for(w in seq_len(nrow(windows))){
  eligible <- x$species %in% scopes[[scope]] & x$valid_year & !x$zero_zero & !is.na(x$year) & x$year>=windows$start[w] & x$year<=windows$end[w]
  a <- x[which(eligible),,drop=FALSE];n <- nrow(a);f <- table(a$species)
  result[[length(result)+1L]] <- data.frame(city_id=cities$city_id[i],scope=scope,roster_n=length(scopes[[scope]]),window=windows$window[w],
   richness=if(n)length(f)else NA_integer_,target_record_n=n,source_rows=sum(a$source_rows),
   sampled_year_n=length(unique(a$year)),first_year=if(n)min(a$year)else NA_integer_,last_year=if(n)max(a$year)else NA_integer_,
   single_city_record_n=sum(a$city_memberships==1),single_city_richness=if(any(a$city_memberships==1))length(unique(a$species[a$city_memberships==1]))else NA_integer_,
   observation_support=if(n)"OBSERVED_TARGET_RECORDS"else"UNKNOWN_OBSERVATION_SUPPORT")
  if(n)counts[[length(counts)+1L]] <- data.frame(city_id=cities$city_id[i],scope=scope,window=windows$window[w],species=names(f),unique_record_n=as.integer(f))
  if(!is.null(cfg$rarefaction_n)){
   m <- as.integer(cfg$rarefaction_n)
   if(m<2L)stop("Rarefaction sample size must exceed one")
   er <- if(n>=m)sum(1-exp(lchoose(n-as.numeric(f),m)-lchoose(n,m)))else NA_real_
   rare[[length(rare)+1L]] <- data.frame(city_id=cities$city_id[i],scope=scope,window=windows$window[w],sample_size=m,record_n=n,expected_richness=er,supported=n>=m)
  }
 }
}
write.csv(do.call(rbind,result),file.path(out,"city_observed_richness.csv"),row.names=FALSE,na="")
write.csv(do.call(rbind,qa),file.path(out,"city_record_qa.csv"),row.names=FALSE)
cc <- if(length(counts))do.call(rbind,counts)else data.frame(city_id=character(),scope=character(),window=character(),species=character(),unique_record_n=integer())
write.csv(cc,file.path(out,"city_species_counts.csv"),row.names=FALSE)
if(length(rare))write.csv(do.call(rbind,rare),file.path(out,"city_rarefaction.csv"),row.names=FALSE,na="")
write_json(list(status="OBSERVED_RICHNESS_SUMMARIZED",cities=nrow(cities),scopes=lapply(scopes,length),scope_sources=scope_sources,
 candidate_sha256=digest(file=file.path(out,"candidate_records.csv"),algo="sha256"),
 boundary_rule="Includes boundaries; cross-city memberships retained, not forced to nearest city",
 unsupported_rule="Richness missing, target-record count zero, true absence not inferred",
 native_status="Source taxon selection is invasive; local native status unresolved",
 intensity="Target-group intensity is endogenous to observed richness and is not independent collecting effort",
 rarefaction="Expected richness from unique species-coordinate-year records; records are not verified independent sampling events"),file.path(out,"richness_summary.json"),pretty=TRUE,auto_unbox=TRUE)
