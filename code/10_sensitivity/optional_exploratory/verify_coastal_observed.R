#!/usr/bin/env Rscript
# Independently recount common-window species and compare fixed-effect slopes.
suppressPackageStartupMessages({library(jsonlite);library(digest)})
cfg<-fromJSON(commandArgs(TRUE)[1],simplifyVector=FALSE);out<-cfg$output_root
receipt<-file.path(out,"coastal_observed_check.json")
if(file.exists(receipt))stop("Existing check retained")
d<-read.csv(file.path(out,"cohort_observed_suitability.csv"),stringsAsFactors=FALSE)
m<-read.csv(file.path(cfg$records_root,"polygon_record_membership.csv"),stringsAsFactors=FALSE)
flag<-function(x)tolower(as.character(x))=="true"
checks<-0L
for(i in seq_len(nrow(d))){
 z<-m[m$polygon_id==d$polygon_id[i]&flag(m$is_target)&flag(m$valid_year)&!flag(m$zero_zero)&m$coastal_memberships==1&!is.na(m$year)&m$year>=d$window_start[i]&m$year<=d$window_end[i],]
 stopifnot(d$target_records[i]==nrow(z),d$endpoint[i]<d$window_start[i])
 if(!nrow(z))stopifnot(is.na(d$recorded_species[i]),d$observation_support[i]=="UNKNOWN_UNRECORDED")else stopifnot(d$recorded_species[i]==length(unique(z$species)))
 checks<-checks+1L
}
f<-read.csv(file.path(out,"age_associations.csv"),stringsAsFactors=FALSE)
model_file<-file.path(out,"model_rows.csv");delta<-numeric()
if(file.exists(model_file)){
 rows<-read.csv(model_file,stringsAsFactors=FALSE)
 for(i in which(f$status=="CONDITIONAL_WITHIN_CITY_LOG_RECORDED_RICHNESS_ASSOCIATION")){
  z<-rows[rows$window==f$window[i]&rows$response==f$suitability[i]&rows$adjustment==f$adjustment[i],]
  formula<-if(f$adjustment[i]=="area")y~age_five_years+si+la+factor(city_id)else y~age_five_years+si+la+le+factor(city_id)
  b<-unname(coef(lm(formula,z))["age_five_years"])
  delta<-c(delta,abs(b-f$age_slope_per_five_years[i]));stopifnot(tail(delta,1)<1e-9,f$ci_low[i]<=f$ci_high[i])
 }
}
write_json(list(status="PASS",cohort_window_counts_checked=checks,independent_dummy_city_slopes_checked=length(delta),
 maximum_slope_difference=if(length(delta))max(delta)else NULL,bootstrap_intervals_independently_reproduced=FALSE,
 output_sha256=digest(file=file.path(out,"cohort_observed_suitability.csv"),algo="sha256")),receipt,auto_unbox=TRUE,pretty=TRUE)
cat("Coastal observed counts and slopes PASS\n")
