#!/usr/bin/env Rscript
# Check omitted marine chains, response-independent PCs and shared binary support.
suppressPackageStartupMessages(library(jsonlite))
args<-commandArgs(TRUE);source(args[1]);out<-args[2];dir.create(out,recursive=TRUE)
if(file.exists(file.path(out,"config.json")))stop("Existing fixture retained")
set.seed(20261014);n<-108L
p<-data.frame(city=paste0("city",seq_len(n)),reclamation_area=exp(rnorm(n)),area_km2=exp(rnorm(n,5)))
raw<-unique(c(pc_groups(p)$anthro_pc1,pc_groups(p)$soil_pc1,pc_groups(p)$marine_pc1,paste0("wc2.1_2.5m_bio_",1:19)))
for(field in raw)p[[field]]<-rnorm(n)
for(nm in names(pc_groups(p)))p[[nm]]<-rep(0,n)
pc<-fixed_predictor_pcs(p);p<-pc$data
stopifnot(all(fixed_predictor_pcs(p)$qc$status=="VERIFIED_EXISTING_UNSUPERVISED_PC"))
p$risk<-rnorm(n);altered<-p;altered$risk<-rev(p$risk)*100
stopifnot(identical(fixed_predictor_pcs(p)$data$anthro_pc1,fixed_predictor_pcs(altered)$data$anthro_pc1))
# Different missing fields leave exactly 84 jointly complete cities.
p$marine_pc1[85:108]<-NA_real_;p$salinity_mean[85:108]<-NA_real_
r<-data.frame(city_id=p$city,scope="RAW_SELECTED_INVASIVE",window="common2010_2024",richness=rpois(n,12),target_record_n=sample(100:1000,n,replace=TRUE))
binary<-do.call(rbind,lapply(c("FULL","ENV"),function(g)do.call(rbind,lapply(c("T10","MAX_TSS"),function(t)data.frame(city_id=p$city,region="CITY",group=g,threshold=t,
 mean_species_suitable_fraction=runif(n,.05,.95),suitable_species_area_km2=runif(n,100,10000),frozen_species_n=684,status="COMPLETE_FIXED_684")))))
write.csv(p,file.path(out,"predictors.csv"),row.names=FALSE);write.csv(r,file.path(out,"observed.csv"),row.names=FALSE);write.csv(binary,file.path(out,"binary.csv"),row.names=FALSE)
cfg<-list(output_root=file.path(out,"results"),city_predictors=file.path(out,"predictors.csv"),observed_richness=file.path(out,"observed.csv"),binary_city_responses=file.path(out,"binary.csv"),expected_binary_cities=84,minimum_cities=50,bootstrap_replicates=99,seed=20261014)
write_json(cfg,file.path(out,"config.json"),auto_unbox=TRUE)
status<-system2(file.path(R.home("bin"),"Rscript"),c("--vanilla",shQuote(args[1]),shQuote(file.path(out,"config.json"))))
stopifnot(status==0L)
effects<-read.csv(file.path(out,"results","effects.csv"));support<-read.csv(file.path(out,"results","support.csv"))
stopifnot(nrow(support)==10L,all(support$n==84L),length(unique(effects$effect))==8L)
cat("PASS_SYNTHETIC_COMPONENT_SYSTEM",nrow(support),"\n")
