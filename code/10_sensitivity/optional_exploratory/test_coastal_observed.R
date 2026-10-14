#!/usr/bin/env Rscript
# Check common-window filtering, unknown support and conditional slope recovery.
suppressPackageStartupMessages(library(jsonlite))
root <- tempfile("coastal_observed_");dir.create(root)
input <- file.path(root,"input");dir.create(input)
p <- expand.grid(city_id=paste0("city",1:25),endpoint=c(1990,2000,2015,2020),stringsAsFactors=FALSE)
p$polygon_id<-paste(p$city_id,p$endpoint,sep=":");p$kind<-"coastal_dissolved_city_cohort";p$start_year<-p$endpoint-5
write.csv(p,file.path(input,"polygon_record_summary.csv"),row.names=FALSE)
m<-do.call(rbind,lapply(seq_len(nrow(p)-1L),function(i){
 n<-max(1L,round(exp(.6+.12*(2024-p$endpoint[i])/5)))
 data.frame(polygon_id=p$polygon_id[i],species=paste0("species",seq_len(n)),year=2022,is_target=TRUE,valid_year=TRUE,zero_zero=FALSE,coastal_memberships=1)
}))
m<-rbind(m,data.frame(polygon_id=p$polygon_id[1],species="outside_window",year=2010,is_target=TRUE,valid_year=TRUE,zero_zero=FALSE,coastal_memberships=1))
write.csv(m,file.path(input,"polygon_record_membership.csv"),row.names=FALSE)
s<-p[,c("city_id","endpoint")];s$reported_category<-s$endpoint;s$layer<-"coastal_change"
s$mapped_area_km2<-exp(sin(seq_len(nrow(s))));s$native_cells<-5;s$effective_cells<-3
s$mean_suitability<-.4+.1*cos(seq_len(nrow(s)));s$response<-"PAIRED_FULL_684"
t<-s;t$response<-"PAIRED_ENV_684";t$mean_suitability<-t$mean_suitability/2
write.csv(rbind(s,t),file.path(input,"suitability.csv"),row.names=FALSE)
write.csv(data.frame(kind="coastal_dissolved_city_cohort",temporal_class="PRE_COHORT",species="conflict"),file.path(input,"species_first_records.csv"),row.names=FALSE)
cfg<-list(records_root=input,suitability_table=file.path(input,"suitability.csv"),output_root=file.path(root,"output"),minimum_cities=20,bootstrap_replicates=999,seed=218)
path<-file.path(root,"config.json");write_json(cfg,path,auto_unbox=TRUE)
here<-dirname(sub("^--file=","",grep("^--file=",commandArgs(),value=TRUE)))
r<-file.path(R.home("bin"),"Rscript")
if(.Platform$OS.type=="windows")r<-paste0(r,".exe")
status<-system2(r,c(shQuote(file.path(here,"coastal_observed_association.R")),shQuote(path)))
stopifnot(status==0L)
d<-read.csv(file.path(cfg$output_root,"cohort_observed_suitability.csv"))
stopifnot(nrow(d)==175L,all(d$endpoint[d$window=="common_2016_2024"]<=2015),sum(is.na(d$recorded_species))==1L)
stopifnot(d$recorded_species[d$window=="common_2021_2024"&d$polygon_id==p$polygon_id[1]]==nrow(m[m$polygon_id==p$polygon_id[1]&m$year==2022,]))
f<-read.csv(file.path(cfg$output_root,"age_associations.csv"))
stopifnot(all(f$age_slope_per_five_years[f$adjustment=="area"]>0),all(f$n_cities==25L))
stopifnot(nrow(read.csv(file.path(cfg$output_root,"first_record_date_conflicts.csv")))==1L)
status<-system2(r,c(shQuote(file.path(here,"verify_coastal_observed.R")),shQuote(path)))
stopifnot(status==0L)
cat("Common-window, support and slope fixture PASS\n")
