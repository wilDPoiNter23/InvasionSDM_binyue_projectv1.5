# Audit dated city records for prospective first-record analyses without SDM fitting.
suppressPackageStartupMessages({library(jsonlite);library(digest)})
a<-commandArgs(TRUE);stopifnot(length(a)==1L);cfg<-fromJSON(a[1]);out<-cfg$output_root
stopifnot(!dir.exists(out));dir.create(out,recursive=TRUE)
r<-read.csv(cfg$candidate_records);m<-read.csv(cfg$city_membership,stringsAsFactors=FALSE)
stopifnot(all(m$record_id>=1&m$record_id<=nrow(r)),!anyDuplicated(m))
x<-cbind(m,r[m$record_id,,drop=FALSE]);x<-x[as.logical(x$valid_year)&!as.logical(x$zero_zero)&is.finite(x$year)&x$year<=2024,]
stopifnot(all(x$year==as.integer(x$year)))
year<-aggregate(record_id~city_id+year,x,length);names(year)[3]<-'target_record_count'
first<-aggregate(year~city_id+species,x,min);names(first)[3]<-'first_record_year'
write.csv(year,file.path(out,'city_year_record_support.csv'),row.names=FALSE)
write.csv(first,file.path(out,'city_species_first_year.csv'),row.names=FALSE)
windows<-list(ALL_DATED=c(-Inf,2024),POST_2000=c(2001,2024),FROM_2000=c(2000,2024),RECENT_2010=c(2010,2024))
summary<-do.call(rbind,lapply(names(windows),function(w){lim<-windows[[w]];q<-x[x$year>=lim[1]&x$year<=lim[2],]
 data.frame(window=w,records=nrow(q),cities=length(unique(q$city_id)),species=length(unique(q$species)))}))
panel<-list()
for(end in seq(1995,2025,5)){
 lo<-end-4;hi<-min(end,2024);q<-x[x$year>=lo&x$year<=hi,];fresh<-first[first$first_record_year>=lo&first$first_record_year<=hi,]
 for(city in sort(unique(x$city_id))){z<-q[q$city_id==city,];f<-fresh[fresh$city_id==city,]
  panel[[length(panel)+1L]]<-data.frame(city_id=city,outcome_start=lo,outcome_end=hi,prior_exposure_end=lo-1,
   target_records=nrow(z),recorded_species=length(unique(z$species)),newly_recorded_species=nrow(f),
   historical_records=sum(x$city_id==city&x$year<lo),recording_supported=nrow(z)>0)
 }
}
write.csv(do.call(rbind,panel),file.path(out,'prospective_window_support.csv'),row.names=FALSE)
write.csv(summary,file.path(out,'dated_window_counts.csv'),row.names=FALSE)
pred<-read.csv(cfg$city_predictors,stringsAsFactors=FALSE)
fields<-c('reclamation_area','area_km2','anthro_pc1','climate_pc1','soil_pc1','marine_pc1')
excluded<-data.frame(city=pred$city,missing_fields=vapply(seq_len(nrow(pred)),function(i)paste(fields[!is.finite(as.numeric(unlist(pred[i,fields],use.names=FALSE)))],collapse=';'),character(1)))
write.csv(excluded,file.path(out,'city_predictor_completeness.csv'),row.names=FALSE)
write_json(list(status='TEMPORAL_INPUT_SUPPORT_CHECKED',candidate_sha256=digest(file=cfg$candidate_records,algo='sha256'),
 membership_sha256=digest(file=cfg$city_membership,algo='sha256'),complete_predictor_cities=sum(excluded$missing_fields==''),
 no_sdm_refit=TRUE,no_lag_model_fitted=TRUE,first_record_is_arrival=FALSE,
 limitation='Target-taxon recording support is not independent survey effort; cities without records have unknown detection support'),
 file.path(out,'receipt.json'),pretty=TRUE,auto_unbox=TRUE)
