# Refit declared GAM parameterisations after an explicit author-defined city exclusion.
suppressPackageStartupMessages({library(mgcv);library(jsonlite);library(digest)})
a<-commandArgs(TRUE);stopifnot(length(a)==1L);cfg<-fromJSON(a[1]);out<-cfg$output_root
stopifnot(!dir.exists(out));dir.create(out,recursive=TRUE)
d<-read.csv(cfg$city_input,stringsAsFactors=FALSE);p<-read.csv(cfg$paired_city_input,stringsAsFactors=FALSE)
key<-function(x)tolower(trimws(x));stopifnot(nrow(d)==108,!anyDuplicated(key(d$city)))
d<-d[!key(d$city)%in%key(cfg$exclude_city_keys),];stopifnot(nrow(d)==cfg$expected_input_cities)
d$log_recl<-log1p(d$reclamation_area);d$log_area<-log1p(d$area_km2);d$true_fraction<-d$reclamation_area/(100*d$area_km2)
fields<-c('risk','log_recl','area_km2','log_area','true_fraction','anthro_pc1','climate_pc1','soil_pc1','marine_pc1')
f<-d[complete.cases(d[,fields]),];stopifnot(nrow(f)==cfg$expected_complete_cities)
ix<-match(key(f$city),key(p$city));stopifnot(!anyNA(ix))
frames<-list(ARCHIVED_GAM_RISK=f,PAIRED_FULL=f,PAIRED_ENV=f)
frames$PAIRED_FULL$risk<-p$PAIRED_FULL_684[ix];frames$PAIRED_ENV$risk<-p$PAIRED_ENV_684[ix]
definitions<-read.csv(cfg$model_definitions,stringsAsFactors=FALSE)
forms<-setNames(lapply(definitions$formula,as.formula),definitions$model)
summaries<-profiles<-terms<-list();max_error<-0
for(response in names(frames))for(model in names(forms)){
 q<-frames[[response]];m<-gam(forms[[model]],data=q,method='REML');stopifnot(m$converged)
 name<-paste(response,model,sep='_');saveRDS(m,file.path(out,paste0(name,'.rds')))
 s<-summary(m);st<-s$s.table;focal<-if(model%in%c('G0','G3'))1L else grep('log_recl|true_fraction',rownames(st))[1]
 summaries[[name]]<-data.frame(response=response,model=model,n=nobs(m),AIC=AIC(m),adjusted_r2=s$r.sq,focal_p=st[focal,4],focal_edf=st[focal,1])
 terms[[name]]<-data.frame(response=response,model=model,term=rownames(st),edf=st[,1],F=st[,3],p=st[,4])
 for(j in seq_len(19))for(mode in c('WHOLE_INPUT_RANGE','LOCAL20_RANGE','COMMON_ABSOLUTE_IQR')){
  area<-as.numeric(quantile(d$area_km2,j*.05));pool<-q[order(abs(log1p(q$area_km2)-log1p(area)))[seq_len(min(20,nrow(q)))],]
  variable<-if(model%in%c('G0','G1'))'log_recl'else'true_fraction'
  if(mode=='COMMON_ABSOLUTE_IQR'){
   R<-as.numeric(quantile(pool$reclamation_area,c(.25,.75)));R[2]<-min(R[2],100*area)
   stopifnot(R[2]>R[1]);rg<-if(variable=='log_recl')log1p(R)else R/(100*area)
  }else{z<-if(mode=='WHOLE_INPUT_RANGE')d else pool;rg<-range(z[[variable]]);R<-if(variable=='log_recl')expm1(rg)else rg*100*area}
  nd<-data.frame(log_recl=median(d$log_recl),true_fraction=median(d$true_fraction),area_km2=area,log_area=log1p(area),
   anthro_pc1=median(d$anthro_pc1,na.rm=TRUE),climate_pc1=median(d$climate_pc1,na.rm=TRUE),soil_pc1=median(d$soil_pc1,na.rm=TRUE),marine_pc1=median(d$marine_pc1,na.rm=TRUE))
  nd<-nd[rep(1,2),];nd[[variable]]<-rg;X<-predict(m,nd,type='lpmatrix');delta<-X[2,]-X[1,]
  effect<-sum(delta*coef(m));se<-sqrt(drop(delta%*%vcov(m)%*%delta));direct<-diff(predict(m,nd))
  error<-abs(effect-direct);stopifnot(error<1e-10);max_error<-max(max_error,error)
  profiles[[length(profiles)+1L]]<-data.frame(response=response,model=model,mode=mode,area_quantile=j*.05,city_area_km2=area,
   reclamation_low_ha=R[1],reclamation_high_ha=R[2],physically_within_city=R[2]<=100*area,
   difference=effect,lower=effect-1.96*se,upper=effect+1.96*se,extrapolation_warning=TRUE)
 }
}
z<-do.call(rbind,profiles);groups<-split(z,paste(z$response,z$model,z$mode));flips<-do.call(rbind,lapply(groups,function(x){i<-which(diff(sign(x$difference))!=0)
 data.frame(response=x$response[1],model=x$model[1],mode=x$mode[1],flips=length(i),lower_km2=if(length(i))x$city_area_km2[i[1]]else NA,upper_km2=if(length(i))x$city_area_km2[i[1]+1]else NA,
  positive_points=sum(x$difference>0),negative_points=sum(x$difference<0),significant_positive=sum(x$lower>0),significant_negative=sum(x$upper<0))}))
write.csv(do.call(rbind,summaries),file.path(out,'model_summary.csv'),row.names=FALSE)
write.csv(do.call(rbind,terms),file.path(out,'smooth_terms.csv'),row.names=FALSE)
write.csv(z,file.path(out,'endpoint_profiles.csv'),row.names=FALSE);write.csv(flips,file.path(out,'sign_changes.csv'),row.names=FALSE)
write.csv(f,file.path(out,'complete_city_input.csv'),row.names=FALSE)
write_json(list(status='PASS_CITY_EXCLUSION_GAM_NUMERICAL_CHECKS',input_n=nrow(d),complete_n=nrow(f),excluded_keys=cfg$exclude_city_keys,
  models=length(summaries),profile_rows=nrow(z),maximum_lpmatrix_prediction_error=max_error,
  source_sha256=list(city_input=digest(file=cfg$city_input,algo='sha256'),paired_input=digest(file=cfg$paired_city_input,algo='sha256')),
  interpretation='Same declared formulas; additive models cannot establish size-dependent sign reversal; tensor joint-term significance is not an ecological breakpoint test',
  limitations='No geometry-preserving null calibration; neighbour-IQR support remains descriptive; historical and paired response construction differ'),
 file.path(out,'verification.json'),auto_unbox=TRUE,pretty=TRUE)
