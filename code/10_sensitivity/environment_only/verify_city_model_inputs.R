# Independently check changed city responses and the unchanged model interfaces.
suppressPackageStartupMessages({library(jsonlite);library(digest)})
a<-commandArgs(TRUE);stopifnot(length(a)==2L);input<-a[1];out<-a[2]
original<-read.csv(file.path(input,'city_predictors.csv'),check.names=FALSE)
full<-read.csv(file.path(input,'full_city_input.csv'),check.names=FALSE)
paired<-read.csv(file.path(input,'paired_city_input.csv'))
stopifnot(identical(names(original),names(full)),identical(original[,names(original)!='risk'],full[,names(full)!='risk']))
key<-function(x)tolower(trimws(x))
root<-dirname(out);s<-readRDS(file.path(root,'analysis_support.rds'));v<-readRDS(file.path(root,'non_native_cell_values.rds'))
w<-s$weights[s$weights$footprint=='HISTORICAL',];maximum_city_error<-0
for(group in names(v$products))for(city in unique(w$city_id)){
 z<-w[w$city_id==city,];values<-v$products[[group]]$values$alien_mean[z$cell_index];ok<-is.finite(values)
 expected<-sum(values[ok]*z$weight_km2[ok])/sum(z$weight_km2[ok])
 actual<-if(group=='FULL_814')full$risk[match(key(city),key(full$city))]else paired[[if(group=='PAIRED_FULL')'PAIRED_FULL_684'else'PAIRED_ENV_684']][match(key(city),key(paired$city))]
 stopifnot(length(actual)==1L,is.finite(actual));maximum_city_error<-max(maximum_city_error,abs(actual-expected))
}
stopifnot(maximum_city_error<1e-12)
gam<-file.path(out,'city_gam');summary<-read.csv(file.path(gam,'model_summary.csv'));used<-read.csv(file.path(gam,'complete_city_input.csv'))
stopifnot(nrow(summary)==12L,all(summary$n==83L))
maximum_gam_input_error<-0
for(i in seq_len(nrow(summary))){
 z<-summary[i,];m<-readRDS(file.path(gam,paste0(z$response,'_',z$model,'.rds')))
 expected<-switch(z$response,ARCHIVED_GAM_RISK=full$risk[match(key(used$city),key(full$city))],PAIRED_FULL=paired$PAIRED_FULL_684[match(key(used$city),key(paired$city))],PAIRED_ENV=paired$PAIRED_ENV_684[match(key(used$city),key(paired$city))])
 maximum_gam_input_error<-max(maximum_gam_input_error,max(abs(m$model$risk-expected)))
}
stopifnot(maximum_gam_input_error<1e-12)
sem<-file.path(out,'city_component_sem');effects<-read.csv(file.path(sem,'effects.csv'));draws<-read.csv(file.path(sem,'draws.csv'))
maximum_sem_error<-0
for(group in unique(effects$group)){
 d<-read.csv(file.path(sem,paste0('standardized_input_',group,'.csv')))
 fit<-lm(y~recl+soc+clim+soil+city_area,data=d)
 direct<-effects$estimate[effects$group==group&effects$effect=='direct']
 maximum_sem_error<-max(maximum_sem_error,abs(coef(fit)['recl']-direct))
 for(effect in effects$effect[effects$group==group]){
  ix<-effects$group==group&effects$effect==effect
  ci<-quantile(draws[[effect]][draws$group==group],c(.025,.975),na.rm=TRUE)
  maximum_sem_error<-max(maximum_sem_error,abs(ci[1]-effects$lower[ix]),abs(ci[2]-effects$upper[ix]))
 }
}
stopifnot(maximum_sem_error<1e-12)
write_json(list(status='PASS',predictors_unchanged=TRUE,maximum_city_response_error=maximum_city_error,maximum_gam_response_error=maximum_gam_input_error,gam_models=nrow(summary),gam_cities=unique(summary$n),sem_response_groups=length(unique(effects$group)),maximum_sem_coefficient_or_interval_error=maximum_sem_error),file.path(out,'city_model_verification.json'),auto_unbox=TRUE,pretty=TRUE)
cat('PASS updated city inputs and model outputs\n')
