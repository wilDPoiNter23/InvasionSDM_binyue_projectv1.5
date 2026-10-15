#!/usr/bin/env Rscript
# Alternative residual responses, partial-regression components and verified continent contrasts.
residual_formulas <- function(fwl=FALSE,intensity=FALSE){
 f<-component_formulas(intensity)
 if(fwl)f<-lapply(f,function(q)reformulate(setdiff(all.vars(q)[-1],c("city_area","intensity")),response=all.vars(q)[1]))
 f
}
residual_effects <- function(edges){
 b<-function(response,predictor)edges$estimate[edges$response==response&edges$predictor==predictor]
 z<-c(direct=b("y","recl"),via_soc=b("soc","recl")*b("y","soc"),via_soil=b("soil","recl")*b("y","soil"),
  via_marine_soil=b("marine","recl")*b("soil","marine")*b("y","soil"),via_soc_soil=b("soc","recl")*b("soil","soc")*b("y","soil"),
  via_soc_marine_soil=b("soc","recl")*b("marine","soc")*b("soil","marine")*b("y","soil"))
 c(z,indirect=sum(z[-1]),total=sum(z))
}
area_effort_formula <- function(intensity=FALSE){
 f<-reformulate(c("city_area",if(intensity)"intensity"),response="richness")
 if(any(!all.vars(f)[-1] %in% c("city_area","intensity")))stop("Baseline includes a focal or forbidden predictor")
 f
}
area_effort_residual <- function(raw,intensity=FALSE){
 X<-model.matrix(area_effort_formula(intensity),raw);fit<-lm.fit(X,raw$richness)
 if(fit$rank!=ncol(X)||any(!is.finite(fit$coefficients)))stop("Area-effort baseline rank gate failed")
 list(residual=fit$residuals,fitted=fit$fitted.values,coefficients=fit$coefficients)
}
residual_component_fit <- function(raw,fwl=FALSE,intensity=FALSE,full=FALSE){
 d<-raw;controls<-c("city_area",if(intensity)"intensity");variables<-c("y","recl","soc","clim","soil","marine")
 if(!fwl)d$y<-area_effort_residual(raw,intensity)$residual
 if(fwl){
  C<-cbind(1,as.matrix(d[,controls,drop=FALSE]));projection<-lm.fit(C,as.matrix(d[,variables]))
  if(projection$rank!=ncol(C))stop("Control projection is rank deficient")
  d[,variables]<-projection$residuals
 }
 forms<-residual_formulas(fwl,intensity);edges<-list();equivalence<-list()
 for(response in names(forms)){
  f<-forms[[response]];X<-model.matrix(f,d);fit<-lm.fit(X,d[[response]])
  if(fit$rank!=ncol(X)||any(!is.finite(fit$coefficients)))stop("Component rank or finite coefficient gate failed")
  if(full){
   classical<-summary(lm(f,data=d))$coefficients[-1,,drop=FALSE]
   e<-data.frame(response=response,predictor=rownames(classical),estimate=classical[,1],se=classical[,2],p=classical[,4],row.names=NULL)
  }else e<-data.frame(response=response,predictor=colnames(X)[-1],estimate=unname(fit$coefficients[-1]))
  edges[[response]]<-e
  if(fwl&&full){
   predictors<-setdiff(all.vars(f)[-1],"(Intercept)")
   joint<-lm.fit(cbind(1,as.matrix(raw[,c(predictors,controls),drop=FALSE])),raw[[response]])
   if(joint$rank!=length(joint$coefficients))stop("Joint OLS rank gate failed")
   delta<-max(abs(e$estimate-joint$coefficients[seq_along(predictors)+1L]))
   if(!is.finite(delta)||delta>1e-9)stop("Partial versus joint OLS equivalence failed")
   equivalence[[response]]<-data.frame(response=response,maximum_difference=delta)
  }
 }
 edges<-do.call(rbind,edges)
 list(edges=edges,effects=residual_effects(edges),data=d,equivalence=if(length(equivalence))do.call(rbind,equivalence)else NULL)
}
residual_bootstrap_cache <- function(raw,fwl=FALSE,intensity=FALSE){
 controls<-c("city_area",if(intensity)"intensity")
 forms<-residual_formulas(fwl,intensity)
 list(values=as.matrix(raw[,c("y","recl","soc","clim","soil","marine","city_area","intensity")]),
  C=cbind(1,as.matrix(raw[,controls,drop=FALSE])),richness=raw$richness,fwl=fwl,
  equations=lapply(forms,function(f)list(response=all.vars(f)[1],predictors=all.vars(f)[-1])))
}
residual_bootstrap_effects <- function(cache,rows){
 v<-cache$values[rows,,drop=FALSE]
 if(!cache$fwl){
  C<-cache$C[rows,,drop=FALSE];baseline<-lm.fit(C,cache$richness[rows])
  if(baseline$rank!=ncol(C))stop("Area-effort baseline rank gate failed")
  v[,"y"]<-baseline$residuals
 }
 if(cache$fwl){
  variables<-c("y","recl","soc","clim","soil","marine");C<-cache$C[rows,,drop=FALSE]
  projection<-lm.fit(C,v[,variables,drop=FALSE])
  if(projection$rank!=ncol(C))stop("Control projection is rank deficient")
  v[,variables]<-projection$residuals
 }
 coefficients<-lapply(cache$equations,function(q){
  X<-cbind("(Intercept)"=1,v[,q$predictors,drop=FALSE]);fit<-lm.fit(X,v[,q$response])
  if(fit$rank!=ncol(X)||any(!is.finite(fit$coefficients)))stop("Component rank or finite coefficient gate failed")
  fit$coefficients
 })
 b<-function(response,predictor)unname(coefficients[[response]][predictor])
 z<-c(direct=b("y","recl"),via_soc=b("soc","recl")*b("y","soc"),via_soil=b("soil","recl")*b("y","soil"),
  via_marine_soil=b("marine","recl")*b("soil","marine")*b("y","soil"),via_soc_soil=b("soc","recl")*b("soil","soc")*b("y","soil"),
  via_soc_marine_soil=b("soc","recl")*b("marine","soc")*b("soil","marine")*b("y","soil"))
 c(z,indirect=sum(z[-1]),total=sum(z))
}
verified_group_fit <- function(d,model){
 d$continent<-factor(d$verified_continent);d$country<-factor(d$country)
 counts<-as.data.frame(table(d$continent));names(counts)<-c("continent","n");counts$sparse<-counts$n<5L;counts$model<-model
 repeated<-sum(table(d$country)>=2L);identified<-nlevels(d$country)>=5L&&repeated>=3L
 if(!identified)stop("Country intercept lacks repeated-country support")
 if(!requireNamespace("nlme",quietly=TRUE))stop("nlme required for verified country intercept")
 fit<-nlme::lme(residual~continent,random=~1|country,data=d,method="ML",na.action=na.fail,control=nlme::lmeControl(opt="optim"))
 null<-nlme::lme(residual~1,random=~1|country,data=d,method="ML",na.action=na.fail,control=nlme::lmeControl(opt="optim"))
 country_sd<-as.numeric(nlme::VarCorr(fit)[1,"StdDev"]);singular<-country_sd<=1e-6*fit$sigma
 lrt<-max(0,2*(as.numeric(logLik(fit))-as.numeric(logLik(null))));df_lrt<-length(nlme::fixef(fit))-1L
 omnibus<-data.frame(model=model,test="PRIMARY_CONTINENT_ML_LIKELIHOOD_RATIO",statistic=lrt,df=df_lrt,p=pchisq(lrt,df_lrt,lower.tail=FALSE),country_sd=country_sd,residual_sd=fit$sigma,singular=singular)
 levels<-levels(d$continent);grid<-data.frame(continent=factor(levels,levels=levels));X<-model.matrix(~continent,grid)
 beta<-nlme::fixef(fit);V<-vcov(fit);denominator_df<-min(fit$fixDF$X)
 pairs<-combn(seq_along(levels),2);contrasts<-lapply(seq_len(ncol(pairs)),function(j){
  ix<-pairs[,j];contrast<-X[ix[1],]-X[ix[2],];estimate<-sum(contrast*beta);se<-sqrt(drop(contrast%*%V%*%contrast))
  data.frame(model=model,group1=levels[ix[1]],group2=levels[ix[2]],estimate=estimate,se=se,df=denominator_df,
   lower=estimate-qt(.975,denominator_df)*se,upper=estimate+qt(.975,denominator_df)*se,p=2*pt(-abs(estimate/se),denominator_df),singular=singular)
 });contrasts<-do.call(rbind,contrasts);contrasts$p_holm<-p.adjust(contrasts$p,"holm")
 list(fit=fit,null=null,counts=counts,omnibus=omnibus,contrasts=contrasts,support=data.frame(model=model,n=nrow(d),countries=nlevels(d$country),repeated_countries=repeated,singular=singular))
}
residual_main <- function(){
 suppressPackageStartupMessages({library(jsonlite);library(digest)})
 cfg<-fromJSON(commandArgs(TRUE)[1],simplifyVector=FALSE);out<-cfg$output_root
 source(cfg$component_helpers)
 if(dir.exists(out))stop("Existing residual sensitivity retained")
 dir.create(out,recursive=TRUE)
 key<-function(x)chartr("ABCDEFGHIJKLMNOPQRSTUVWXYZ","abcdefghijklmnopqrstuvwxyz",trimws(as.character(x)))
 p<-read.csv(cfg$city_predictors,check.names=FALSE);p$key<-key(p$city)
 stopifnot(nrow(p)==108L,!anyDuplicated(p$key),cfg$minimum_cities>=50L)
 pc<-fixed_predictor_pcs(p);p<-pc$data
 write.csv(pc$qc,file.path(out,"pc_numerical_provenance.csv"),row.names=FALSE)
 write.csv(pc$loadings,file.path(out,"fixed_pc_loadings.csv"),row.names=FALSE)
 obs<-read.csv(cfg$observed_data,stringsAsFactors=FALSE)
 obs<-obs[obs$scope=="RAW_SELECTED_INVASIVE"&obs$window=="common2010_2024",];obs$key<-key(obs$city_id)
 stopifnot(!anyDuplicated(obs$key),all(obs$key %in% p$key))
 base<-data.frame(key=p$key,city=p$city,recl=log1p(p$reclamation_area),city_area=log1p(p$area_km2),soc=p$anthro_pc1,clim=p$climate_pc1,soil=p$soil_pc1,marine=p$marine_pc1)
 base<-merge(base,obs[,c("key","richness","target_record_n")],by="key",all.x=TRUE)
 base$y<-log1p(base$richness);base$intensity<-log1p(base$target_record_n)
 geo<-read.csv(cfg$geographic_crosswalk,stringsAsFactors=FALSE);geo$key<-key(geo$city)
 stopifnot(nrow(geo)==108L,!anyDuplicated(geo$key),setequal(geo$key,p$key),!anyNA(geo$country),!anyNA(geo$verified_continent),all(nzchar(geo$country)),all(nzchar(geo$verified_continent)))
 if(!isTRUE(cfg$allow_synthetic_geography))stopifnot(all(geo$source_sha256==digest(file=cfg$city_predictors,algo="sha256")),all(nzchar(geo$coordinate_check)),all(nzchar(geo$identity_evidence)))
 write.csv(geo,file.path(out,"verified_group_crosswalk.csv"),row.names=FALSE)
 complete<-complete.cases(base[,c("y","recl","soc","clim","soil","marine","city_area","intensity")])&base$target_record_n>0
 fixed<-base[complete,];stopifnot(nrow(fixed)==cfg$expected_sem_cities,nrow(fixed)>=cfg$minimum_cities)
 write.csv(data.frame(key=fixed$key),file.path(out,"frozen_sem_city_keys.csv"),row.names=FALSE)
 set.seed(cfg$seed);edge_tables<-list();effect_tables<-list();draw_tables<-list();equivalence<-list();sem_support<-list()
 B<-as.integer(cfg$bootstrap_replicates);stopifnot(B>=99L)
 for(case in c("AREA_EFFORT_RESIDUAL_M0","AREA_EFFORT_RESIDUAL_M1","FWL_M0","FWL_M1")){
  fwl<-startsWith(case,"FWL");intensity<-endsWith(case,"M1");raw<-fixed
  a<-residual_component_fit(raw,fwl,intensity,TRUE);a$edges$case<-case;edge_tables[[case]]<-a$edges
  if(fwl){a$equivalence$case<-case;equivalence[[case]]<-a$equivalence}
  boot<-matrix(NA_real_,B,length(a$effects),dimnames=list(NULL,names(a$effects)))
  cache<-residual_bootstrap_cache(raw,fwl,intensity)
  for(b in seq_len(B)){
   rows<-sample.int(nrow(raw),nrow(raw),replace=TRUE)
   q<-tryCatch(residual_bootstrap_effects(cache,rows),error=function(e)NULL)
   if(!is.null(q))boot[b,]<-q
  }
  success<-complete.cases(boot);if(mean(success)<.95)stop("Bootstrap success below 95 percent")
  ci<-apply(boot[success,,drop=FALSE],2,quantile,c(.025,.975))
  effect_tables[[case]]<-data.frame(case=case,effect=names(a$effects),estimate=as.numeric(a$effects),lower=ci[1,],upper=ci[2,],successful_bootstrap=sum(success),requested_bootstrap=B,row.names=NULL)
  draw_tables[[case]]<-data.frame(case=case,replicate=seq_len(B),boot,check.names=FALSE)
  write.csv(raw,file.path(out,paste0("raw_",case,".csv")),row.names=FALSE)
  write.csv(a$data,file.path(out,paste0("data_",case,".csv")),row.names=FALSE)
  sem_support[[case]]<-data.frame(case=case,n=nrow(raw),response=if(fwl)"PARTIAL_LOG1P_RICHNESS"else "REFITTED_AREA_EFFORT_RAW_RICHNESS_RESIDUAL",controls=if(intensity)"AREA_AND_TARGET_INTENSITY"else "AREA",baseline_cities=nrow(raw),baseline_contains_reclamation=FALSE,scale="UNSTANDARDIZED_LINEAR_COMPONENT_COEFFICIENTS")
 }
 files<-c(edge_tables="edges.csv",effect_tables="effects.csv",draw_tables="draws.csv",equivalence="fwl_equivalence.csv",sem_support="sem_support.csv")
 for(item in names(files))write.csv(do.call(rbind,get(item)),file.path(out,files[[item]]),row.names=FALSE)
 group_support<-list();group_omnibus<-list();group_contrasts<-list();group_counts<-list()
 for(m in c("M0","M1")){
  r<-base[!is.na(base$richness)&base$target_record_n>0&is.finite(base$city_area)&is.finite(base$intensity),]
  baseline<-area_effort_residual(r,m=="M1");r$residual<-baseline$residual;r$fitted<-baseline$fitted
  r<-merge(r,geo[,c("key","country","verified_continent")],by="key",all.x=TRUE)
  stopifnot(nrow(r)==cfg$expected_group_cities,!anyNA(r$country),!anyNA(r$verified_continent))
  g<-verified_group_fit(r,m);group_support[[m]]<-g$support;group_omnibus[[m]]<-g$omnibus;group_contrasts[[m]]<-g$contrasts;group_counts[[m]]<-g$counts
  group_support[[m]]$baseline<-paste(deparse(area_effort_formula(m=="M1")),collapse=" ");group_support[[m]]$baseline_contains_reclamation<-FALSE
  write.csv(r,file.path(out,paste0("group_data_",m,".csv")),row.names=FALSE);saveRDS(g$fit,file.path(out,paste0("group_fit_",m,".rds")));saveRDS(g$null,file.path(out,paste0("group_null_",m,".rds")))
 }
 for(item in c("group_support","group_omnibus","group_contrasts","group_counts"))write.csv(do.call(rbind,get(item)),file.path(out,paste0(item,".csv")),row.names=FALSE)
 receipt<-list(status="EXPLORATORY_RESIDUAL_COMPONENT_AND_CONTINENT_SENSITIVITY",sem_cities=nrow(fixed),group_cities=cfg$expected_group_cities,bootstrap=B,
  inputs=lapply(cfg[c("city_predictors","observed_data","count_residuals","geographic_crosswalk","component_helpers")],function(f)digest(file=f,algo="sha256")),
  configuration_sha256=digest(file=commandArgs(TRUE)[1],algo="sha256"),path_pruning=FALSE,causal_or_observation_correction=FALSE,
  unavailable_groups=c("Island status","Development status"),primary_group_test="Continent ML likelihood-ratio test with verified country random intercept",
  pairwise_adjustment="Holm within each M0 or M1 continent family; nominal Wald intervals",
  count_residuals_used_as_response=FALSE,count_residual_diagnostic="Prior count residuals already adjust for focal reclamation; retained as provenance only",
  baselines=lapply(c(FALSE,TRUE),function(x)paste(deparse(area_effort_formula(x)),collapse=" ")),
  limitations=c("Area-effort residual baselines use raw observed richness and no focal reclamation or PC predictors",
   "SEM baselines fit the fixed 84 cities and are refitted within each city bootstrap; group baselines fit all observed cities separately",
   "FWL uses log1p richness and common linear controls, and is distinct from raw-richness area-effort residuals",
   "FWL residualization is repeated inside every city bootstrap; both sides of every component equation are partialled",
   "Target-record intensity is endogenous and does not establish independent collecting effort",
   "Continent likelihood and Wald contrasts condition on estimated first-stage residuals without propagating their baseline uncertainty",
   "Sparse continents and near-zero country variance are flagged; island and development labels are not inferred",
   "Bootstrap and random intercepts do not establish causal paths or resolve spatial dependence"))
 write_json(receipt,file.path(out,"receipt.json"),auto_unbox=TRUE,pretty=TRUE)
 writeLines(capture.output(sessionInfo()),file.path(out,"sessionInfo.txt"))
}
if(sys.nframe()==0L)residual_main()
