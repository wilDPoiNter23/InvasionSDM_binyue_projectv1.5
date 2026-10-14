#!/usr/bin/env Rscript
# Compare threshold-defined paired responses using the same city predictors.
suppressPackageStartupMessages({library(jsonlite);library(digest);library(mgcv)})
cfg<-fromJSON(commandArgs(TRUE)[1],simplifyVector=FALSE);out<-cfg$output_root
K<-if(is.null(cfg$expected_species_count))684L else as.integer(cfg$expected_species_count)
stopifnot(length(K)==1L,is.finite(K),K>0L,K<=684L)
expected_status<-paste0("COMPLETE_FIXED_",K)
if(dir.exists(out))stop("Existing association retained")
dir.create(out,recursive=TRUE)
d<-read.csv(cfg$city_responses,stringsAsFactors=FALSE)
p<-read.csv(cfg$city_predictors,stringsAsFactors=FALSE)
key<-function(x)chartr("ABCDEFGHIJKLMNOPQRSTUVWXYZ","abcdefghijklmnopqrstuvwxyz",trimws(x))
stopifnot(!anyDuplicated(key(p$city)),all(d$frozen_species_n==K),cfg$minimum_cities>=50)
d$key<-key(d$city_id);p$key<-key(p$city)
if(any(!d$key%in%p$key))stop("Unmatched city identity")
d<-merge(d,p,by="key",all.x=TRUE)
d$log_recl<-log1p(d$reclamation_area);d$log_area<-log1p(d$area_km2)
fields<-c("log_recl","log_area","area_km2","anthro_pc1","climate_pc1","soil_pc1","marine_pc1")
summaries<-list();gates<-list();contrasts<-list();modelrows<-list()
for(region in unique(d$region)){
 z<-d[d$region==region&d$status==expected_status&complete.cases(d[,fields]),]
 counts<-table(z$key)
 ids<-names(counts[counts==4L]);z<-z[z$key%in%ids,]
 if(anyDuplicated(paste(z$key,z$group,z$threshold)))stop("Duplicate paired response")
 n<-length(ids)
 for(group in c("FULL","ENV"))for(threshold in c("T10","MAX_TSS")){
  q<-z[z$group==group&z$threshold==threshold,]
  stopifnot(nrow(q)==n)
  for(response in c("mean_species_suitable_fraction","mean_species_suitable_area")){
   q$y<-if(response=="mean_species_suitable_fraction")q$mean_species_suitable_fraction else log1p(q$suitable_species_area_km2/K)
   state<-"INSUFFICIENT_COMMON_CITY_SUPPORT"
   if(n>=cfg$minimum_cities&&all(is.finite(q$y))&&sd(q$y)>0){
    formula<-if(response=="mean_species_suitable_fraction")y~te(log_recl,area_km2,k=c(5,5))+s(anthro_pc1,k=5)+s(climate_pc1,k=5)+s(soil_pc1,k=5)+s(marine_pc1,k=5)else y~s(log_recl,k=5)+s(log_area,k=5)+s(anthro_pc1,k=5)+s(climate_pc1,k=5)+s(soil_pc1,k=5)+s(marine_pc1,k=5)
    m<-tryCatch(gam(formula,data=q,method="REML"),error=function(e)e)
    if(inherits(m,"error"))state<-"FIT_FAILED"else if(!isTRUE(m$converged)||(!is.null(m$outer.info$conv)&&m$outer.info$conv!="full convergence"))state<-"CONVERGENCE_GATE_FAILED"else{
     st<-summary(m);cv<-concurvity(m,full=TRUE)
     summaries[[length(summaries)+1L]]<-data.frame(region=region,group=group,threshold=threshold,response=response,n=n,adj_r2=st$r.sq,deviance_explained=st$dev.expl,focal_edf=st$s.table[1,1],focal_p=st$s.table[1,4],maximum_concurvity=max(cv["estimate",-1]),formula=paste(deparse(formula),collapse=" "))
     # Contrast only uses cities with observed reclamation support.
     nd<-q;lo<-quantile(q$log_recl,.25);hi<-quantile(q$log_recl,.75)
     nd$log_recl<-lo;xl<-predict(m,nd,type="lpmatrix");nd$log_recl<-hi;xh<-predict(m,nd,type="lpmatrix")
     dx<-colMeans(xh-xl);effect<-sum(dx*coef(m));se<-sqrt(drop(dx%*%vcov(m)%*%dx))
     contrasts[[length(contrasts)+1L]]<-data.frame(region=region,group=group,threshold=threshold,response=response,reclamation_log_q25=lo,reclamation_log_q75=hi,mean_q75_minus_q25=effect,se=se,lower=effect-1.96*se,upper=effect+1.96*se,interval="MODEL_BASED_WALD",intervention_or_causal=FALSE)
     file<-paste(region,group,threshold,response,sep="_");file<-gsub("[^A-Za-z0-9_]","_",file)
     saveRDS(m,file.path(out,paste0(file,".rds")))
     state<-"EXPLORATORY_MATCHED_THRESHOLD_ASSOCIATION"
    }
   }
   gates[[length(gates)+1L]]<-data.frame(region=region,group=group,threshold=threshold,response=response,n_common_cities=n,status=state)
  }
 }
 modelrows[[region]]<-z
}
write.csv(do.call(rbind,gates),file.path(out,"model_support.csv"),row.names=FALSE)
if(length(summaries))write.csv(do.call(rbind,summaries),file.path(out,"model_summary.csv"),row.names=FALSE)
if(length(contrasts))write.csv(do.call(rbind,contrasts),file.path(out,"reclamation_contrasts.csv"),row.names=FALSE)
write.csv(do.call(rbind,modelrows),file.path(out,"matched_city_data.csv"),row.names=FALSE)
write_json(list(status="THRESHOLD_SENSITIVITY",species=K,source_species_pool=684,fixed_city_support=TRUE,
 area_response="log1p mean species suitable area; sum across species is not unique habitat area",
 full_response_still_uses_human_predictors=TRUE,causal_or_sem_validation=FALSE,
 response_sha256=digest(file=cfg$city_responses,algo="sha256"),predictor_sha256=digest(file=cfg$city_predictors,algo="sha256")),file.path(out,"association.json"),auto_unbox=TRUE,pretty=TRUE)
