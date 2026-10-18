# Supply updated continuous city responses to the existing component-model functions.
suppressPackageStartupMessages({library(jsonlite);library(digest)})
a<-commandArgs(TRUE);stopifnot(length(a)==1L);cfg<-fromJSON(a[1]);out<-cfg$output_root
script<-sub('^--file=','',grep('^--file=',commandArgs(FALSE),value=TRUE)[1])
source(file.path(dirname(script),'..','optional_exploratory','paired_response_sem.R'))
stopifnot(!dir.exists(out));dir.create(out,recursive=TRUE)
p<-read.csv(cfg$city_predictors,check.names=FALSE);f<-read.csv(cfg$full_city_input);paired<-read.csv(cfg$paired_city_input)
key<-function(x)tolower(trimws(x));p$key<-key(p$city)
pc<-fixed_predictor_pcs(p);p<-pc$data
write.csv(pc$qc,file.path(out,'pc_numerical_provenance.csv'),row.names=FALSE)
write.csv(pc$loadings,file.path(out,'fixed_pc_loadings.csv'),row.names=FALSE)
base<-data.frame(key=p$key,city=p$city,recl=log1p(p$reclamation_area),city_area=log1p(p$area_km2),soc=p$anthro_pc1,clim=p$climate_pc1,soil=p$soil_pc1,marine=p$marine_pc1)
responses<-list(FULL_814=f$risk[match(base$key,key(f$city))],FULL_684=paired$PAIRED_FULL_684[match(base$key,key(paired$city))],ENV_684=paired$PAIRED_ENV_684[match(base$key,key(paired$city))])
set.seed(cfg$seed);effects<-edges<-supports<-draws<-list()
for(group in names(responses)){
 d<-base;d$y<-responses[[group]];vars<-c('y','recl','soc','clim','soil','marine','city_area')
 d<-d[!d$key%in%key(cfg$exclude_city_keys)&complete.cases(d[,vars]),]
 stopifnot(nrow(d)==cfg$expected_complete_cities)
 write.csv(d,file.path(out,paste0('raw_input_',group,'.csv')),row.names=FALSE)
 scale_table<-data.frame(variable=vars,mean=vapply(d[,vars],mean,numeric(1)),sd=vapply(d[,vars],sd,numeric(1)))
 d[,vars]<-scale(d[,vars]);fit<-component_fit(d,FALSE);cache<-component_bootstrap_cache(d,FALSE)
 B<-as.integer(cfg$bootstrap_replicates);boot<-matrix(NA_real_,B,length(fit$effects),dimnames=list(NULL,names(fit$effects)))
 for(i in seq_len(B)){ix<-sample.int(nrow(d),nrow(d),TRUE);v<-tryCatch(component_bootstrap_effects(cache,ix),error=function(e)NULL);if(!is.null(v))boot[i,]<-v}
 ok<-complete.cases(boot);stopifnot(mean(ok)>=.95)
 ci<-apply(boot[ok,,drop=FALSE],2,quantile,c(.025,.975))
 effects[[group]]<-data.frame(group=group,effect=names(fit$effects),estimate=as.numeric(fit$effects),lower=ci[1,],upper=ci[2,],successful_bootstrap=sum(ok))
 fit$edges$group<-group;edges[[group]]<-fit$edges
 supports[[group]]<-data.frame(group=group,cities=nrow(d),formula_source='Unchanged component_formulas(FALSE)')
 draws[[group]]<-data.frame(group=group,replicate=seq_len(B),boot)
 write.csv(d,file.path(out,paste0('standardized_input_',group,'.csv')),row.names=FALSE)
 write.csv(scale_table,file.path(out,paste0('scales_',group,'.csv')),row.names=FALSE)
}
for(item in c('effects','edges','supports','draws'))write.csv(do.call(rbind,get(item)),file.path(out,paste0(item,'.csv')),row.names=FALSE)
write_json(list(status='COMPLETED',model_functions='Existing paired_response_sem.R functions; no equation changes',response='Continuous non-native suitability; unchanged per-case standardization and city bootstrap',bootstrap_replicates=cfg$bootstrap_replicates,seed=cfg$seed),file.path(out,'receipt.json'),auto_unbox=TRUE,pretty=TRUE)
