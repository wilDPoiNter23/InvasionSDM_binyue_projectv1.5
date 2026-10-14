#!/usr/bin/env Rscript
# Check saved model support and threshold contrasts against direct predictions.
suppressPackageStartupMessages({library(jsonlite);library(mgcv)})
cfg<-fromJSON(commandArgs(TRUE)[1],simplifyVector=FALSE);out<-cfg$output_root
path<-file.path(out,"association_check.json");if(file.exists(path))stop("Existing check retained")
rows<-read.csv(file.path(out,"matched_city_data.csv"),stringsAsFactors=FALSE)
g<-read.csv(file.path(out,"model_support.csv"),stringsAsFactors=FALSE)
stopifnot(all(g$n_common_cities>=0),all(rows$frozen_species_n==684),all(rows$status=="COMPLETE_FIXED_684"))
checks<-0L;delta<-numeric()
if(file.exists(file.path(out,"reclamation_contrasts.csv"))){
 d<-read.csv(file.path(out,"reclamation_contrasts.csv"),stringsAsFactors=FALSE)
 for(i in seq_len(nrow(d))){
  r<-d[i,];z<-rows[rows$region==r$region&rows$group==r$group&rows$threshold==r$threshold,]
  file<-gsub("[^A-Za-z0-9_]","_",paste(r$region,r$group,r$threshold,r$response,sep="_"))
  m<-readRDS(file.path(out,paste0(file,".rds")))
  stopifnot(nobs(m)==nrow(z),m$converged)
  low<-high<-z;low$log_recl<-r$reclamation_log_q25;high$log_recl<-r$reclamation_log_q75
  e<-mean(predict(m,high,type="response")-predict(m,low,type="response"))
  delta<-c(delta,abs(e-r$mean_q75_minus_q25));stopifnot(tail(delta,1)<1e-9,r$lower<=r$upper)
  target<-if(r$response=="mean_species_suitable_fraction")z$mean_species_suitable_fraction else log1p(z$suitable_species_area_km2/684)
  stopifnot(max(abs(m$y-target))<1e-12)
  checks<-checks+1L
 }
}
write_json(list(status="PASS",models_checked=checks,maximum_direct_prediction_difference=if(length(delta))max(delta)else NULL,
 independent_model_fit=FALSE,causal_or_sem_validation=FALSE),path,auto_unbox=TRUE,pretty=TRUE)
cat("Binary city response binding and contrasts PASS\n")
