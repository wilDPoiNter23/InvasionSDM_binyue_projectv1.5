#!/usr/bin/env Rscript
# Check signed Gaussian residuals, partial-regression equivalence and group inference.
suppressPackageStartupMessages(library(jsonlite))
args<-commandArgs(TRUE);source(args[1]);source(args[2]);out<-args[3]
dir.create(out,recursive=TRUE);if(file.exists(file.path(out,"config.json")))stop("Existing fixture retained")
set.seed(20261015);n<-108L
saved_rng<-.Random.seed
unit<-as.data.frame(matrix(rnorm(84*8),84,8));names(unit)<-c("y","recl","soc","clim","soil","marine","city_area","intensity")
unit$richness<-sample(1:50,84,replace=TRUE)
for(fwl in c(FALSE,TRUE))for(intensity in c(FALSE,TRUE)){
 cache<-residual_bootstrap_cache(unit,fwl,intensity)
 for(rows in list(c(rep(1L,12),seq_len(72)),rep(seq_len(42),each=2L),sample.int(84,84,replace=TRUE))){
  direct<-residual_component_fit(unit[rows,],fwl,intensity,TRUE)
  fast<-residual_bootstrap_effects(cache,rows)
  stopifnot(max(abs(direct$effects-fast))<1e-10)
  if(fwl)stopifnot(max(direct$equivalence$maximum_difference)<1e-9)
 }
}
.Random.seed<-saved_rng
p<-data.frame(city=paste0("city",seq_len(n)),reclamation_area=exp(rnorm(n)),area_km2=exp(rnorm(n,5)))
raw<-unique(c(pc_groups(p)$anthro_pc1,pc_groups(p)$soil_pc1,pc_groups(p)$marine_pc1,paste0("wc2.1_2.5m_bio_",1:19)))
for(field in raw)p[[field]]<-rnorm(n)
for(nm in names(pc_groups(p)))p[[nm]]<-0
p<-fixed_predictor_pcs(p)$data;p$salinity_mean[85:108]<-NA_real_;p$marine_pc1[85:108]<-NA_real_
data<-data.frame(city_id=p$city,scope="RAW_SELECTED_INVASIVE",window="common2010_2024",richness=rpois(n,12)+1,target_record_n=sample(200:2000,n,replace=TRUE))
residual<-do.call(rbind,lapply(c("M0","M1"),function(model)data.frame(scope="RAW_SELECTED_INVASIVE",window="common2010_2024",model=model,city_id=p$city,observed=data$richness,fitted=data$richness-rnorm(n,0,2))))
residual$residual<-residual$observed-residual$fitted
geo<-data.frame(city=p$city,country=rep(paste0("Country",1:18),each=6),verified_continent=rep(rep(c("A","B","C"),each=6),each=6),
 source_sha256="fixture",coordinate_check="fixture verified coordinates",identity_evidence="Synthetic exact city identities")
write.csv(p,file.path(out,"predictors.csv"),row.names=FALSE);write.csv(data,file.path(out,"observed.csv"),row.names=FALSE)
write.csv(residual,file.path(out,"residuals.csv"),row.names=FALSE);write.csv(geo,file.path(out,"geography.csv"),row.names=FALSE)
cfg<-list(output_root=file.path(out,"results"),component_helpers=normalizePath(args[2],winslash="/"),city_predictors=file.path(out,"predictors.csv"),observed_data=file.path(out,"observed.csv"),count_residuals=file.path(out,"residuals.csv"),geographic_crosswalk=file.path(out,"geography.csv"),minimum_cities=50,expected_sem_cities=84,expected_group_cities=108,bootstrap_replicates=99,seed=20261015,allow_synthetic_geography=TRUE)
write_json(cfg,file.path(out,"config.json"),auto_unbox=TRUE)
status<-system2(file.path(R.home("bin"),"Rscript"),c("--vanilla",shQuote(args[1]),shQuote(file.path(out,"config.json"))))
stopifnot(status==0L)
support<-read.csv(file.path(out,"results","sem_support.csv"));stopifnot(nrow(support)==4,all(support$n==84L))
check<-read.csv(file.path(out,"results","fwl_equivalence.csv"));stopifnot(max(check$maximum_difference)<1e-9)
stored<-read.csv(file.path(out,"results","data_AREA_EFFORT_RESIDUAL_M0.csv"));stopifnot(any(stored$y<0))
stopifnot(!"recl" %in% all.vars(area_effort_formula(FALSE)),!"recl" %in% all.vars(area_effort_formula(TRUE)))
groups<-read.csv(file.path(out,"results","group_support.csv"));stopifnot(all(groups$n==108L),nrow(groups)==2L)
cat("PASS_SIGNED_RESIDUAL_FWL_GROUPS\n")
