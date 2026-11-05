#!/usr/bin/env Rscript
# Compare recorded richness and current suitability in common observation windows.
suppressPackageStartupMessages({library(jsonlite);library(digest)})
a <- commandArgs(TRUE)
if(length(a)!=1L)stop("Supply a private JSON configuration")
cfg <- fromJSON(a[1],simplifyVector=FALSE)
stopifnot(cfg$minimum_cities>=20L,cfg$bootstrap_replicates>=999L)
out <- cfg$output_root
dir.create(out,recursive=TRUE,showWarnings=FALSE)
if(file.exists(file.path(out,"coastal_observed_complete.json")))stop("Existing result retained")
read <- function(n)read.csv(file.path(cfg$records_root,n),stringsAsFactors=FALSE)
p <- read("polygon_record_summary.csv")
p <- p[p$kind=="coastal_dissolved_city_cohort",]
m <- read("polygon_record_membership.csv")
flag <- function(x)tolower(as.character(x))=="true"
m <- m[flag(m$is_target)&flag(m$valid_year)&!flag(m$zero_zero)&m$coastal_memberships==1,]
s <- read.csv(cfg$suitability_table,stringsAsFactors=FALSE)
s <- s[s$layer=="coastal_change",]
stopifnot(!anyDuplicated(p$polygon_id),!anyDuplicated(paste(s$city_id,s$reported_category,s$response)))
full <- s[s$response=="PAIRED_FULL_684",]
env <- s[s$response=="PAIRED_ENV_684",]
full$endpoint <- as.integer(full$reported_category)
env$endpoint <- as.integer(env$reported_category)
base <- merge(p,full[,c("city_id","endpoint","mapped_area_km2","native_cells","effective_cells","mean_suitability")],by=c("city_id","endpoint"),all.x=TRUE)
names(base)[names(base)=="mean_suitability"] <- "full_suitability"
base <- merge(base,env[,c("city_id","endpoint","mean_suitability")],by=c("city_id","endpoint"),all.x=TRUE)
names(base)[names(base)=="mean_suitability"] <- "env_suitability"
base$age_five_years <- (2024-base$endpoint)/5
windows <- list(common_2021_2024=c(2021L,2024L,2020L),common_2016_2024=c(2016L,2024L,2015L))
tables <- list();fits <- list();terms <- list()
set.seed(cfg$seed)
for(w in names(windows)){
  v <- windows[[w]]
  z <- base[base$endpoint<=v[3],]
  z$window <- w
  z$window_start <- v[1];z$window_end <- v[2]
  z$target_records <- integer(nrow(z));z$recorded_species <- rep(NA_integer_,nrow(z))
  for(i in seq_len(nrow(z))){
    r <- m[m$polygon_id==z$polygon_id[i]&m$year>=v[1]&m$year<=v[2],]
    z$target_records[i] <- nrow(r)
    if(nrow(r))z$recorded_species[i] <- length(unique(r$species))
  }
  z$observation_support <- ifelse(z$target_records>0,"RECORDED_TARGETS","UNKNOWN_UNRECORDED")
  z$paired_native_support <- is.finite(z$full_suitability)&is.finite(z$env_suitability)&is.finite(z$mapped_area_km2)&z$mapped_area_km2>0&z$native_cells>=3&z$effective_cells>=2
  tables[[w]] <- z
  for(response in c("full_suitability","env_suitability"))for(adjust in c("area","area_target_intensity")){
    q <- z[z$observation_support=="RECORDED_TARGETS"&z$paired_native_support,]
    q <- q[ave(q$endpoint,q$city_id,FUN=function(x)length(unique(x)))>=2,]
    q$y <- log1p(q$recorded_species)
    q$si <- q[[response]];q$la <- log(q$mapped_area_km2);q$le <- log1p(q$target_records)
    predictors <- c("age_five_years","si","la",if(adjust=="area_target_intensity")"le")
    ncity <- length(unique(q$city_id));n <- nrow(q)
    status <- "INSUFFICIENT_SUPPORT";beta<-lo<-hi<-NA_real_;valid<-0L
    if(ncity>=cfg$minimum_cities&&n>=5L*length(predictors)+ncity){
      dm <- function(d){for(k in c("y",predictors))d[[k]]<-d[[k]]-ave(d[[k]],d$city_id,FUN=mean);d}
      fit <- function(d){d<-dm(d);x<-as.matrix(d[,predictors,drop=FALSE]);f<-lm.fit(x,d$y);if(f$rank!=ncol(x))return(NA_real_);unname(f$coefficients[1])}
      beta <- fit(q)
      cities <- unique(q$city_id)
      boots <- replicate(cfg$bootstrap_replicates,{
        chosen <- sample(cities,length(cities),TRUE)
        b <- do.call(rbind,lapply(seq_along(chosen),function(j){x<-q[q$city_id==chosen[j],];x$city_id<-paste0("sample",j);x}))
        fit(b)
      })
      valid <- sum(is.finite(boots))
      if(is.finite(beta)&&valid>=.9*cfg$bootstrap_replicates){
        ci<-quantile(boots[is.finite(boots)],c(.025,.975));lo<-ci[1];hi<-ci[2]
        status<-"CONDITIONAL_WITHIN_CITY_LOG_RECORDED_RICHNESS_ASSOCIATION"
      }else status<-"RANK_OR_BOOTSTRAP_SUPPORT_FAILURE"
      q$window<-w;q$response<-response;q$adjustment<-adjust
      terms[[paste(w,response,adjust)]]<-q[,c("window","response","adjustment","city_id","polygon_id","y","age_five_years","si","la","le")]
    }
    fits[[paste(w,response,adjust)]]<-data.frame(window=w,suitability=response,adjustment=adjust,n_cohorts=n,n_cities=ncity,age_slope_per_five_years=beta,ci_low=lo,ci_high=hi,valid_bootstraps=valid,status=status)
  }
}
write.csv(do.call(rbind,tables),file.path(out,"cohort_observed_suitability.csv"),row.names=FALSE,na="")
write.csv(do.call(rbind,fits),file.path(out,"age_associations.csv"),row.names=FALSE,na="")
if(length(terms))write.csv(do.call(rbind,terms),file.path(out,"model_rows.csv"),row.names=FALSE,na="")
first <- read("species_first_records.csv")
conflict <- first[first$kind=="coastal_dissolved_city_cohort"&first$temporal_class%in%c("PRE_COHORT","WITHIN_COHORT_INTERVAL"),]
write.csv(conflict,file.path(out,"first_record_date_conflicts.csv"),row.names=FALSE,na="")
write_json(list(status="EXPLORATORY_RECORDED_RICHNESS_SCREEN",species_scope="FROZEN_PAIRED_684",source_last_year=2024,
  no_records_are_absence=FALSE,first_record_is_arrival=FALSE,sampling_proxy="ENDOGENOUS_TARGET_RECORD_INTENSITY",
  response="log1p unique recorded selected invasive species",city_effect="within-city centering",
  age_reference_year=2024,land_use_adjusted=FALSE,minimum_cities=cfg$minimum_cities,
  source_sha256=lapply(c(membership=file.path(cfg$records_root,"polygon_record_membership.csv"),suitability=cfg$suitability_table),function(x)digest(file=x,algo="sha256")),
  code_sha256=digest(file=sub("^--file=","",grep("^--file=",commandArgs(),value=TRUE)),algo="sha256")),
  file.path(out,"coastal_observed_complete.json"),auto_unbox=TRUE,pretty=TRUE)
