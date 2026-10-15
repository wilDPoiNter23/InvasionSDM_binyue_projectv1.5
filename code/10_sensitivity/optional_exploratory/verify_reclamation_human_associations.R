#!/usr/bin/env Rscript
# Independently check unit conversion, rank associations and partial regression with HC3.
suppressPackageStartupMessages({library(jsonlite);library(digest)})
cfg<-fromJSON(commandArgs(TRUE)[1],simplifyVector=FALSE);out<-cfg$output_root
if(file.exists(file.path(out,"verification.json")))stop("Existing verification retained")
d<-read.csv(cfg$city_predictors,check.names=FALSE);saved<-read.csv(file.path(out,"city_inputs.csv"),check.names=FALSE)
r<-read.csv(file.path(out,"associations.csv"),stringsAsFactors=FALSE);s<-read.csv(file.path(out,"analysis_samples.csv"),stringsAsFactors=FALSE)
receipt<-fromJSON(file.path(out,"receipt.json"));stopifnot(receipt$city_input_sha256==digest(file=cfg$city_predictors,algo="sha256"))
stopifnot(identical(d$city,saved$city),max(abs(saved$reclamation_fraction-d$reclamation_area*.01/d$area_km2),na.rm=TRUE)<1e-12)
fields<-c("BUILT_S_E2025_2.5min","GDP2020_2.5min","Pop2020_2.5min","VIIRS_2023_2.5min")
stopifnot(nrow(r)==12L,setequal(r$indicator,fields),!anyDuplicated(paste(r$indicator,r$family)))
boot<-if(file.exists(file.path(out,"spearman_bootstrap.csv")))read.csv(file.path(out,"spearman_bootstrap.csv"))else NULL
checks<-list();set.seed(cfg$seed)
for(i in seq_len(nrow(r))){
 row<-r[i,];a<-s[s$indicator==row$indicator,,drop=FALSE]
 ix<-match(a$city,d$city);stopifnot(nrow(a)==row$n,!anyNA(ix),!anyDuplicated(a$city))
 source_value<-if(row$indicator %in% names(d))d[[row$indicator]]else rep(NA_real_,nrow(d))
 valid<-is.finite(source_value)&source_value>=0&is.finite(d$reclamation_area)&d$reclamation_area>=0&is.finite(d$area_km2)&d$area_km2>0
 stopifnot(setequal(a$city,d$city[valid]))
 if(nrow(a))stopifnot(max(abs(a$indicator_value-d[[row$indicator]][ix]))<1e-10)
 if(row$status!="FITTED")next
 if(row$family=="SPEARMAN_AMOUNT"){
  estimate<-cor(rank(a$reclamation_amount_ha,ties.method="average"),rank(a$indicator_value,ties.method="average"))
  # The asymptotic Spearman test uses the correlation-to-t transformation.
  statistic<-estimate*sqrt((nrow(a)-2)/(1-estimate^2));p<-2*pt(-abs(statistic),nrow(a)-2)
  z<-boot$rho[boot$indicator==row$indicator];ci<-quantile(z[is.finite(z)],c(.025,.975))
  stopifnot(length(z)==cfg$spearman_bootstrap)
  for(b in seq_along(z)){indices<-sample.int(nrow(a),nrow(a),replace=TRUE)
   if(b<=3L){expected<-cor(rank(a$reclamation_amount_ha[indices]),rank(a$indicator_value[indices]));stopifnot(abs(expected-z[b])<1e-10)}}
  stopifnot(abs(estimate-row$estimate)<1e-10,abs(p-row$p)<1e-10,max(abs(ci-c(row$lower,row$upper)))<1e-10)
 }else{
  x<-if(row$family=="AREA_CONTROLLED_AMOUNT")log1p(a$reclamation_amount_ha)else log1p(a$reclamation_fraction)
  xs<-(x-mean(x))/sd(x);y<-(a$log_indicator-mean(a$log_indicator))/sd(a$log_indicator)
  C<-cbind(1,a$log_area);rx<-xs-drop(C%*%qr.solve(C,xs));ry<-y-drop(C%*%qr.solve(C,y));estimate<-sum(rx*ry)/sum(rx^2)
  X<-cbind(C,xs);fitted<-drop(X%*%qr.solve(X,y));Q<-qr.Q(qr(X));leverage<-rowSums(Q^2)
  # The focal HC3 variance follows the partial-regression score weights.
  se<-sqrt(sum(rx^2*((y-fitted)/(1-leverage))^2)/sum(rx^2)^2)
  df<-nrow(a)-3L;ci<-estimate+c(-1,1)*qt(.975,df)*se;p<-2*pt(-abs(estimate/se),df)
  stopifnot(abs(estimate-row$estimate)<1e-9,abs(se-row$se)<1e-9,max(abs(ci-c(row$lower,row$upper)))<1e-9,abs(p-row$p)<1e-10)
 }
 checks[[length(checks)+1L]]<-data.frame(indicator=row$indicator,family=row$family,n=nrow(a),estimate_error=abs(estimate-row$estimate),p_error=abs(p-row$p))
}
for(family in unique(r$family)){ix<-which(r$family==family);stopifnot(max(abs(r$p_bh[ix]-p.adjust(r$p[ix],"BH",n=4)),na.rm=TRUE)<1e-10)}
write.csv(do.call(rbind,checks),file.path(out,"numerical_verification.csv"),row.names=FALSE)
write_json(list(status="PASS_HUMAN_ASSOCIATION_NUMERICAL_QA",planned_tests=12,fitted_tests=length(checks),
 independent_methods=c("Hectare-to-square-kilometre conversion","Rank-Pearson Spearman calculation","FWL partial regression","QR leverage and focal HC3 score variance","Interval and BH reconstruction"),
 scientific_or_causal_acceptance=FALSE),file.path(out,"verification.json"),auto_unbox=TRUE,pretty=TRUE)
cat("PASS_HUMAN_ASSOCIATION_NUMERICAL_QA\n")
