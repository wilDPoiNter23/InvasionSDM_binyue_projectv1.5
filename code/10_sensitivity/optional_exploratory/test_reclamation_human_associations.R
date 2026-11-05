#!/usr/bin/env Rscript
# Check predefined indicators, area units, ties, missingness and adjusted associations.
suppressPackageStartupMessages(library(jsonlite))
args<-commandArgs(TRUE);out<-args[2];dir.create(out,recursive=TRUE)
if(file.exists(file.path(out,"config.json")))stop("Existing fixture retained")
set.seed(20261015);n<-108L;area<-exp(runif(n,3,7));amount<-exp(runif(n,0,7))
p<-data.frame(city=paste0("city",seq_len(n)),area_km2=area,reclamation_area=amount,check.names=FALSE)
fields<-c("BUILT_S_E2025_2.5min","GDP2020_2.5min","Pop2020_2.5min","VIIRS_2023_2.5min")
p[[fields[1]]]<-expm1(.4*log1p(amount)+.2*log1p(area)+runif(n,0,.4))
p[[fields[2]]]<-rep(0:8,length.out=n);p[[fields[3]]]<-expm1(.3*log1p(area)+runif(n));p[[fields[4]]]<-rep(1,n)
p[[fields[3]]][1:5]<-NA_real_
write.csv(p,file.path(out,"predictors.csv"),row.names=FALSE,na="")
cfg<-list(output_root=file.path(out,"results"),city_predictors=file.path(out,"predictors.csv"),expected_cities=108,minimum_cities=30,spearman_bootstrap=99,seed=20261015,reclamation_unit="ha",city_area_unit="km2")
write_json(cfg,file.path(out,"config.json"),auto_unbox=TRUE)
status<-system2(file.path(R.home("bin"),"Rscript"),c("--vanilla",shQuote(args[1]),shQuote(file.path(out,"config.json"))))
stopifnot(status==0L)
r<-read.csv(file.path(out,"results","associations.csv"));d<-read.csv(file.path(out,"results","city_inputs.csv"))
stopifnot(nrow(r)==12L,setequal(r$indicator,fields),max(abs(d$reclamation_fraction-p$reclamation_area/(100*p$area_km2)))<1e-12,
 all(r$status[r$indicator==fields[4]]=="CONSTANT_VARIABLE"),all(r$n[r$indicator==fields[3]]==103L),
 all(is.finite(r$p_bh[r$status=="FITTED"])))
cat("PASS_RECLAMATION_HUMAN_SYNTHETIC\n")
