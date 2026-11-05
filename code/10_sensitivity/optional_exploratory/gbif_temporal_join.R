#!/usr/bin/env Rscript
# Spatially join bounded occurrences and retain first-record ambiguity.
suppressPackageStartupMessages({library(sf);library(jsonlite);library(digest)})
args <- commandArgs(TRUE)
if(length(args)!=1L)stop("Supply a private JSON configuration")
cfg <- fromJSON(args[1],simplifyVector=FALSE)
out <- cfg$output_root
if(file.exists(file.path(out,"temporal_screen.json")))stop("Existing temporal screen retained")
polys <- readRDS(file.path(out,"polygons.rds"))
d <- read.csv(file.path(out,"candidate_records.csv"),stringsAsFactors=FALSE,check.names=FALSE)
flag <- function(x)tolower(as.character(x))=="true"
d$is_target <- flag(d$is_target);d$valid_year <- flag(d$valid_year);d$zero_zero <- flag(d$zero_zero)
d$record_id <- seq_len(nrow(d))
if(nrow(d)){
  pts <- st_transform(st_as_sf(d,coords=c("longitude","latitude"),crs=4326,remove=FALSE),6933)
  hits <- st_intersects(pts,polys)
  coastal <- polys$kind=="coastal_dissolved_city_cohort"
  d$coastal_memberships <- vapply(hits,function(h)sum(coastal[h]),integer(1))
  inverse <- split(rep(seq_along(hits),lengths(hits)),unlist(hits,use.names=FALSE))
  bypolygon <- lapply(seq_len(nrow(polys)),function(j)inverse[[as.character(j)]])
}else{bypolygon <- vector("list",nrow(polys));d$coastal_memberships <- integer(0)}
first <- list();effort <- list();summary <- list()
membership_path <- file.path(out,"polygon_record_membership.csv")
empty <- data.frame(polygon_id=character(),record_id=integer(),species=character(),year=integer(),
  is_target=logical(),valid_year=logical(),source_rows=integer(),zero_zero=logical(),
  coastal_memberships=integer(),country_count=integer())
write.csv(empty,membership_path,row.names=FALSE)
for(j in seq_len(nrow(polys))){
  p <- polys[j,]
  x <- d[bypolygon[[j]],,drop=FALSE]
  valid <- x$valid_year & !x$zero_zero
  target <- valid & x$is_target
  target_years <- x$year[target]
  summary[[j]] <- data.frame(polygon_id=p$polygon_id,kind=p$kind,city_id=p$city_id,
    endpoint=p$endpoint,start_year=p$start_year,unique_records=nrow(x),source_rows=sum(x$source_rows),
    missing_year_records=sum(is.na(x$year)),invalid_year_records=sum(!is.na(x$year)&!x$valid_year),
    zero_zero_records=sum(x$zero_zero),country_conflict_records=sum(x$country_count>1),
    valid_dated_records=sum(valid),valid_target_records=sum(target),
    target_species_observed=length(unique(x$species[target])),
    first_any_target_year=if(length(target_years))min(target_years)else NA_integer_,
    record_state=if(length(target_years))"DATED_TARGET_RECORDED"else"NO_DATED_TARGET_RECORD_IN_SOURCE",
    native_status="UNRESOLVED")
  if(!nrow(x))next
  member <- x[,names(empty)[-1],drop=FALSE]
  member$polygon_id <- p$polygon_id
  member <- member[,names(empty)]
  write.table(member,membership_path,sep=",",row.names=FALSE,col.names=FALSE,append=TRUE,na="",quote=TRUE)
  for(y in sort(unique(x$year[!is.na(x$year)]))){
    a <- x[x$year==y & !is.na(x$year),,drop=FALSE]
    effort[[length(effort)+1L]] <- data.frame(polygon_id=p$polygon_id,year=y,
      valid_year=all(a$valid_year),source_rows=sum(a$source_rows),unique_source_records=nrow(a),
      source_species=length(unique(a$species)),target_source_rows=sum(a$source_rows[a$is_target]),
      unique_target_records=sum(a$is_target),target_species=length(unique(a$species[a$is_target])),
      zero_zero_records=sum(a$zero_zero))
  }
  for(s in sort(unique(x$species[x$is_target]))){
    a <- x[x$is_target & !is.na(x$species) & x$species==s,,drop=FALSE]
    usable <- a$valid_year & !a$zero_zero
    y <- if(any(usable))min(a$year[usable])else NA_integer_
    period <- if(is.na(p$endpoint))"NO_SUBPOLYGON_COMPLETION_DATE"else if(is.na(y))"NO_VALID_YEAR"else
      if(y<p$start_year)"PRE_COHORT"else if(y<=p$endpoint)"WITHIN_COHORT_INTERVAL"else"AFTER_COHORT_ENDPOINT"
    first[[length(first)+1L]] <- data.frame(polygon_id=p$polygon_id,kind=p$kind,city_id=p$city_id,species=s,
      endpoint=p$endpoint,start_year=p$start_year,first_record_year=y,
      lag_lower_years=if(!is.na(y)&&!is.na(p$endpoint))y-p$endpoint else NA_integer_,
      lag_upper_years=if(!is.na(y)&&!is.na(p$start_year))y-p$start_year else NA_integer_,
      temporal_class=period,unique_records=nrow(a),source_rows=sum(a$source_rows),
      valid_dated_records=sum(usable),missing_year_records=sum(is.na(a$year)),
      invalid_year_records=sum(!is.na(a$year)&!a$valid_year),
      first_year_records=if(!is.na(y))sum(usable & a$year==y,na.rm=TRUE)else 0,
      ambiguous_coastal_membership_records=sum(a$coastal_memberships>1),
      native_status="UNRESOLVED",interpretation="TARGET_SPECIES_FIRST_RECORD")
  }
}
write.csv(do.call(rbind,summary),file.path(out,"polygon_record_summary.csv"),row.names=FALSE,na="")
if(length(first))write.csv(do.call(rbind,first),file.path(out,"species_first_records.csv"),row.names=FALSE,na="")
if(length(effort))write.csv(do.call(rbind,effort),file.path(out,"polygon_year_sampling.csv"),row.names=FALSE,na="")
write_json(list(status="TARGET_SPECIES_FIRST_RECORD_SCREEN",native_status="UNRESOLVED",
  biological_invasion_dates_inferred=FALSE,unrecorded_cohorts_absent_or_right_censored=FALSE,
  coordinate_uncertainty_available=FALSE,year_only_dates=TRUE,
  lag_definition="[first year - endpoint, first year - (endpoint - 5)]",
  pre_cohort_and_interval_records_retained=TRUE,
  residual_coastal_memberships_retained_and_flagged=TRUE,
  mine_lags_computed=FALSE,
  interpretation="Observation dates depend on collecting effort; negative lags may indicate date or location incompatibility",
  extraction_sha256=digest(file=file.path(out,"extraction.json"),algo="sha256")),
  file.path(out,"temporal_screen.json"),pretty=TRUE,auto_unbox=TRUE)
