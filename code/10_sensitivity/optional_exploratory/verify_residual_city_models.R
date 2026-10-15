#!/usr/bin/env Rscript
# Independently verify partial regressions, directed paths and mixed continent contrasts.
suppressPackageStartupMessages({library(jsonlite);library(nlme)})
cfg<-fromJSON(commandArgs(TRUE)[1],simplifyVector=FALSE);out<-cfg$output_root
if(file.exists(file.path(out,"verification.json")))stop("Existing verification retained")
edges<-read.csv(file.path(out,"edges.csv"));effects<-read.csv(file.path(out,"effects.csv"));draws<-read.csv(file.path(out,"draws.csv"),check.names=FALSE)
support<-read.csv(file.path(out,"sem_support.csv"));stopifnot(nrow(support)==4L,all(support$n==cfg$expected_sem_cities))
nodes<-c("recl","soc","marine","soil","y");variables<-c("y","recl","soc","clim","soil","marine")
path_values<-function(e){
 b<-function(response,predictor)e$estimate[e$response==response&e$predictor==predictor]
 path<-c(direct=b("y","recl"),via_soc=b("soc","recl")*b("y","soc"),via_soil=b("soil","recl")*b("y","soil"),
  via_marine_soil=b("marine","recl")*b("soil","marine")*b("y","soil"),via_soc_soil=b("soc","recl")*b("soil","soc")*b("y","soil"),
  via_soc_marine_soil=b("soc","recl")*b("marine","soc")*b("soil","marine")*b("y","soil"))
 A<-matrix(0,5,5,dimnames=list(nodes,nodes))
 for(i in seq_len(nrow(e)))if(e$predictor[i] %in% nodes)A[e$response[i],e$predictor[i]]<-e$estimate[i]
 total<-solve(diag(5)-A)["y","recl"]
 stopifnot(abs(total-sum(path))<1e-10)
 c(path,indirect=sum(path[-1]),total=total)
}
partial<-function(raw,controls){
 C<-cbind(1,as.matrix(raw[,controls,drop=FALSE]));Y<-as.matrix(raw[,variables]);raw[,variables]<-Y-C%*%qr.solve(C,Y);raw
}
area_residual<-function(raw,controls){
 C<-cbind(1,as.matrix(raw[,controls,drop=FALSE]));raw$y<-raw$richness-drop(C%*%qr.solve(C,raw$richness));raw
}
refit<-function(d,e){
 for(response in unique(e$response)){
  j<-which(e$response==response);X<-cbind(1,as.matrix(d[,e$predictor[j],drop=FALSE]))
  e$estimate[j]<-drop(qr.solve(X,d[[response]]))[-1]
 }
 e
}
set.seed(cfg$seed);checks<-list();frozen<-NULL
for(case in support$case){
 raw<-read.csv(file.path(out,paste0("raw_",case,".csv")),check.names=FALSE)
 d<-read.csv(file.path(out,paste0("data_",case,".csv")),check.names=FALSE)
 if(is.null(frozen))frozen<-sort(raw$key)
 stopifnot(identical(sort(raw$key),frozen),identical(raw$key,d$key))
 fwl<-startsWith(case,"FWL");controls<-c("city_area",if(endsWith(case,"M1"))"intensity")
 expected<-if(fwl)partial(raw,controls)else area_residual(raw,controls)
 stopifnot(max(abs(as.matrix(d[,variables])-as.matrix(expected[,variables])))<1e-9)
 e<-edges[edges$case==case,];independent<-refit(expected,e)
 coefficient_error<-max(abs(e$estimate-independent$estimate));stopifnot(coefficient_error<1e-9)
 fwl_error<-NA_real_
 if(fwl){
  deltas<-numeric()
  for(response in unique(e$response)){
   q<-e[e$response==response,];X<-cbind(1,as.matrix(raw[,c(q$predictor,controls),drop=FALSE]))
   beta<-drop(qr.solve(X,raw[[response]]))[seq_len(nrow(q))+1L]
   deltas<-c(deltas,abs(beta-q$estimate))
  }
  fwl_error<-max(deltas);stopifnot(fwl_error<1e-9)
 }
 values<-path_values(e);z<-effects[effects$case==case,]
 stopifnot(max(abs(values[z$effect]-z$estimate))<1e-10)
 b<-draws[draws$case==case,];ok<-complete.cases(b)
 for(effect in z$effect){ci<-quantile(b[[effect]][ok],c(.025,.975));j<-which(z$effect==effect);stopifnot(max(abs(ci-c(z$lower[j],z$upper[j])))<1e-10)}
 for(replicate in seq_len(nrow(b))){
  rows<-sample.int(nrow(raw),nrow(raw),replace=TRUE)
  if(replicate<=3L){sampled<-raw[rows,];sampled<-if(fwl)partial(sampled,controls)else area_residual(sampled,controls)
   value<-path_values(refit(sampled,e));stopifnot(max(abs(value-as.numeric(b[replicate,names(value)])))<1e-9)
  }
 }
 checks[[case]]<-data.frame(case=case,coefficient_error=coefficient_error,fwl_joint_error=fwl_error,total=values["total"],row.names=NULL)
}
# Check predictor-only PC scores independently through SVD, without response columns.
p<-read.csv(cfg$city_predictors,check.names=FALSE);key<-function(x)chartr("ABCDEFGHIJKLMNOPQRSTUVWXYZ","abcdefghijklmnopqrstuvwxyz",trimws(as.character(x)))
loadings<-read.csv(file.path(out,"fixed_pc_loadings.csv"));raw<-read.csv(file.path(out,"raw_AREA_EFFORT_RESIDUAL_M0.csv"))
map<-c(anthro_pc1="soc",climate_pc1="clim",soil_pc1="soil",marine_pc1="marine")
for(domain in names(map)){
 fields<-loadings$variable[loadings$domain==domain];stopifnot(!any(fields %in% c("risk","reclamation_area","area_km2","richness")))
 X<-p[,fields,drop=FALSE];cc<-complete.cases(X);s<-svd(scale(X[cc,,drop=FALSE]));score<-rep(NA_real_,nrow(p));score[cc]<-s$u[,1]*s$d[1]
 expected<-score[match(raw$key,key(p$city))];actual<-raw[[map[[domain]]]];sgn<-if(cor(expected,actual)<0)-1 else 1
 stopifnot(max(abs(sgn*expected-actual))<1e-8)
}
omnibus<-read.csv(file.path(out,"group_omnibus.csv"));contrasts<-read.csv(file.path(out,"group_contrasts.csv"));group_support<-read.csv(file.path(out,"group_support.csv"))
geography<-read.csv(cfg$geographic_crosswalk,stringsAsFactors=FALSE);geography$key<-key(geography$city)
for(model in c("M0","M1")){
 d<-read.csv(file.path(out,paste0("group_data_",model,".csv")),stringsAsFactors=FALSE)
 stopifnot(nrow(d)==cfg$expected_group_cities,!anyDuplicated(d$key))
 ix<-match(d$key,geography$key);stopifnot(!anyNA(ix),identical(d$country,geography$country[ix]),identical(d$verified_continent,geography$verified_continent[ix]))
 controls<-c("city_area",if(model=="M1")"intensity");baseline<-area_residual(d,controls)
 stopifnot(max(abs(d$residual-baseline$y))<1e-9,max(abs(d$residual-(d$richness-d$fitted)))<1e-9)
 fit<-readRDS(file.path(out,paste0("group_fit_",model,".rds")));null<-readRDS(file.path(out,paste0("group_null_",model,".rds")))
 q<-omnibus[omnibus$model==model,];statistic<-max(0,2*(as.numeric(logLik(fit))-as.numeric(logLik(null))))
 stopifnot(abs(q$statistic-statistic)<1e-9,abs(q$p-pchisq(statistic,q$df,lower.tail=FALSE))<1e-10)
 co<-contrasts[contrasts$model==model,];stopifnot(max(abs(co$p_holm-p.adjust(co$p,"holm")))<1e-10)
 grid<-data.frame(continent=factor(sort(unique(d$verified_continent)),levels=sort(unique(d$verified_continent))))
 X<-model.matrix(~continent,grid);rownames(X)<-as.character(grid$continent)
 for(i in seq_len(nrow(co))){v<-X[co$group1[i],]-X[co$group2[i],];estimate<-sum(v*fixef(fit));se<-sqrt(drop(v%*%vcov(fit)%*%v))
  stopifnot(abs(co$estimate[i]-estimate)<1e-9,abs(co$se[i]-se)<1e-9,abs(co$p[i]-2*pt(-abs(estimate/se),co$df[i]))<1e-10)
 }
 counts<-read.csv(file.path(out,"group_counts.csv"));counts<-counts[counts$model==model,];stopifnot(sum(counts$n)==nrow(d),all(counts$sparse==(counts$n<5L)))
}
write.csv(do.call(rbind,checks),file.path(out,"numerical_verification.csv"),row.names=FALSE)
write_json(list(status="PASS_RESIDUAL_COMPONENT_AND_GROUP_NUMERICAL_QA",sem_cases=4,group_comparisons=2,sem_cities=cfg$expected_sem_cities,group_cities=cfg$expected_group_cities,
 independently_verified=c("Signed Gaussian responses","Predictor-only SVD PCs","FWL versus joint controlled OLS","DAG matrix inverse and six paths","Resampled control projection and QR fits","Bootstrap percentile intervals","Country/continent identity","Mixed-model likelihood-ratio statistic","Pairwise contrasts and Holm adjustment"),
 scientific_or_causal_acceptance=FALSE),file.path(out,"verification.json"),auto_unbox=TRUE,pretty=TRUE)
cat("PASS_RESIDUAL_COMPONENT_AND_GROUP_NUMERICAL_QA\n")
