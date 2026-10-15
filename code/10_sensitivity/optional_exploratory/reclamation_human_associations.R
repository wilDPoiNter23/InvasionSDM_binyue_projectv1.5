#!/usr/bin/env Rscript
# Predefined city human-activity associations with reclamation amount and fraction.
suppressPackageStartupMessages({library(jsonlite);library(digest)})
cfg<-fromJSON(commandArgs(TRUE)[1],simplifyVector=FALSE);out<-cfg$output_root
if(dir.exists(out))stop("Existing associations retained")
stopifnot(identical(cfg$reclamation_unit,"ha"),identical(cfg$city_area_unit,"km2"),cfg$minimum_cities>=30L,cfg$spearman_bootstrap>=99L)
d<-read.csv(cfg$city_predictors,stringsAsFactors=FALSE,check.names=FALSE)
stopifnot(nrow(d)==cfg$expected_cities,!anyDuplicated(tolower(trimws(d$city))))
source_sha<-digest(file=cfg$city_predictors,algo="sha256")
if(!is.null(cfg$expected_input_sha256)&&source_sha!=cfg$expected_input_sha256)stop("City source checksum mismatch")
fields<-c("BUILT_S_E2025_2.5min","GDP2020_2.5min","Pop2020_2.5min","VIIRS_2023_2.5min")
stopifnot(all(c("city","reclamation_area","area_km2") %in% names(d)),all(d$area_km2[is.finite(d$area_km2)]>0))
# One hectare is 0.01 square kilometres. Previously derived ratio columns are not used.
d$reclamation_fraction<-d$reclamation_area/(100*d$area_km2)
if(any(d$reclamation_fraction>1,na.rm=TRUE))stop("Reclamation fraction exceeds city area; resolve footprint or units")
dir.create(out,recursive=TRUE)
write.csv(d[,c("city","reclamation_area","area_km2","reclamation_fraction",intersect(fields,names(d)))],file.path(out,"city_inputs.csv"),row.names=FALSE,na="")
set.seed(cfg$seed);results<-list();samples<-list();bootstraps<-list();definitions<-list()
for(indicator in fields){
 exists<-indicator %in% names(d);z<-if(exists)d[[indicator]]else rep(NA_real_,nrow(d))
 valid<-is.finite(z)&z>=0&is.finite(d$reclamation_area)&d$reclamation_area>=0&is.finite(d$area_km2)&d$area_km2>0
 a<-d[valid,,drop=FALSE];a$indicator_value<-z[valid];a$log_indicator<-log1p(a$indicator_value);a$log_area<-log1p(a$area_km2)
 definitions[[indicator]]<-data.frame(indicator=indicator,available=exists,source_rows=nrow(d),valid_rows=sum(valid),
  missing_indicator_rows=sum(!is.finite(z)),negative_indicator_rows=sum(is.finite(z)&z<0),
  invalid_reclamation_rows=sum(!is.finite(d$reclamation_area)|d$reclamation_area<0),invalid_area_rows=sum(!is.finite(d$area_km2)|d$area_km2<=0))
 samples[[indicator]]<-data.frame(indicator=rep(indicator,nrow(a)),city=a$city,reclamation_amount_ha=a$reclamation_area,reclamation_fraction=a$reclamation_fraction,
  indicator_value=a$indicator_value,log_indicator=a$log_indicator,log_area=a$log_area)
 for(family in c("SPEARMAN_AMOUNT","AREA_CONTROLLED_AMOUNT","AREA_CONTROLLED_FRACTION")){
  row<-data.frame(indicator=indicator,family=family,n=nrow(a),status="FITTED",estimate=NA_real_,se=NA_real_,lower=NA_real_,upper=NA_real_,p=NA_real_,df=NA_real_,
   interpretation=if(family=="SPEARMAN_AMOUNT")"RAW_SPEARMAN_RHO"else "STANDARDIZED_LOG1P_SLOPE_HC3",successful_bootstrap=NA_integer_)
  if(!exists)row$status<-"INDICATOR_UNAVAILABLE"else if(nrow(a)<cfg$minimum_cities)row$status<-"INSUFFICIENT_CITY_SUPPORT"else
   if(sd(a$indicator_value)==0||sd(a$reclamation_area)==0)row$status<-"CONSTANT_VARIABLE"
  if(row$status=="FITTED"&&family=="SPEARMAN_AMOUNT"){
   test<-cor.test(a$reclamation_area,a$indicator_value,method="spearman",exact=FALSE,continuity=FALSE)
   row$estimate<-unname(test$estimate);row$p<-test$p.value
   boot<-rep(NA_real_,cfg$spearman_bootstrap)
   for(b in seq_along(boot)){ix<-sample.int(nrow(a),nrow(a),replace=TRUE)
    if(sd(a$reclamation_area[ix])>0&&sd(a$indicator_value[ix])>0)boot[b]<-cor(a$reclamation_area[ix],a$indicator_value[ix],method="spearman")}
   finite<-is.finite(boot);row$successful_bootstrap<-sum(finite)
   if(mean(finite)<.95)stop("Spearman bootstrap support below 95 percent")
   ci<-quantile(boot[finite],c(.025,.975));row$lower<-ci[1];row$upper<-ci[2]
   bootstraps[[indicator]]<-data.frame(indicator=indicator,replicate=seq_along(boot),rho=boot)
  }else if(row$status=="FITTED"){
   x<-if(family=="AREA_CONTROLLED_AMOUNT")log1p(a$reclamation_area)else log1p(a$reclamation_fraction)
   if(sd(x)==0||sd(a$log_indicator)==0||sd(a$log_area)==0){row$status<-"CONSTANT_VARIABLE"}else{
    X<-cbind(intercept=1,reclamation=as.numeric(scale(x)),city_area=a$log_area);y<-as.numeric(scale(a$log_indicator))
    fit<-lm.fit(X,y)
    if(fit$rank!=ncol(X)){row$status<-"RANK_DEFICIENT"}else{
     inverse<-solve(crossprod(X));leverage<-rowSums((X%*%inverse)*X)
     if(any(leverage>=1-1e-10))stop("HC3 leverage gate failed")
     covariance<-inverse%*%crossprod(X,X*(fit$residuals/(1-leverage))^2)%*%inverse
     se<-sqrt(covariance[2,2]);df<-nrow(a)-ncol(X);estimate<-fit$coefficients[2]
     row$estimate<-estimate;row$se<-se;row$df<-df;row$lower<-estimate-qt(.975,df)*se;row$upper<-estimate+qt(.975,df)*se;row$p<-2*pt(-abs(estimate/se),df)
    }
   }
  }
  results[[length(results)+1L]]<-row
 }
}
r<-do.call(rbind,results);r$p_bh<-NA_real_
for(family in unique(r$family)){ix<-which(r$family==family);r$p_bh[ix]<-p.adjust(r$p[ix],method="BH",n=length(fields))}
write.csv(r,file.path(out,"associations.csv"),row.names=FALSE,na="")
write.csv(do.call(rbind,definitions),file.path(out,"indicator_support.csv"),row.names=FALSE)
write.csv(do.call(rbind,samples),file.path(out,"analysis_samples.csv"),row.names=FALSE)
if(length(bootstraps))write.csv(do.call(rbind,bootstraps),file.path(out,"spearman_bootstrap.csv"),row.names=FALSE,na="")
write_json(list(status="EXPLORATORY_HUMAN_ACTIVITY_ASSOCIATIONS",city_input_sha256=source_sha,configuration_sha256=digest(file=commandArgs(TRUE)[1],algo="sha256"),
 indicators=fields,families=unique(r$family),focal_units="Reclamation amount in ha; fraction = ha / (100 * city km2)",
 linear_model="standardized log1p indicator ~ standardized log1p amount or fraction + log1p city area",
 intervals="HC3 heteroskedastic covariance with residual-df t intervals; Spearman city-bootstrap percentile intervals",
 multiple_testing="BH separately within each of three four-indicator planned families, including non-estimable planned tests",
 causal_interpretation=FALSE,uses_sdm_response_or_supervised_composites=FALSE,
 limitations=c("Observational associations do not establish causal effects or independence of human pressures",
  "Source indicator units and acquisition definitions require their provider dictionary; filename years differ across indicators",
  "City means on the existing 2.5-arcminute predictor grid cannot quantify fine-scale fragmentation",
  "Fraction shares its denominator with city area; amount and fraction models have different functional forms",
  "HC3 and city bootstrap do not resolve spatial dependence or uncertain source data")),file.path(out,"receipt.json"),auto_unbox=TRUE,pretty=TRUE)
writeLines(capture.output(sessionInfo()),file.path(out,"sessionInfo.txt"))
