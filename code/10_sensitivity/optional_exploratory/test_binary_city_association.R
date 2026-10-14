#!/usr/bin/env Rscript
# Check paired support gates and direct contrast reproduction.
suppressPackageStartupMessages(library(jsonlite))
set.seed(72);root<-tempfile("binary_city_");dir.create(root)
n<-70L
p<-data.frame(city=paste0("city",seq_len(n)),reclamation_area=exp(runif(n,1,8)),area_km2=exp(runif(n,3,8)),anthro_pc1=rnorm(n),climate_pc1=rnorm(n),soil_pc1=rnorm(n),marine_pc1=rnorm(n))
d<-expand.grid(city_id=p$city,group=c("FULL","ENV"),threshold=c("T10","MAX_TSS"),stringsAsFactors=FALSE)
d$region<-"CITY";d$frozen_species_n<-684L;d$status<-"COMPLETE_FIXED_684"
i<-match(d$city_id,p$city)
d$mean_species_suitable_fraction<-.2+.025*log1p(p$reclamation_area[i])+.004*p$anthro_pc1[i]+ifelse(d$group=="FULL",.03,0)+rnorm(nrow(d),0,.005)
d$suitable_species_area_km2<-d$mean_species_suitable_fraction*p$area_km2[i]*684
small<-d[d$city_id%in%p$city[1:10],];small$region<-"MAPPED_RECLAMATION";d<-rbind(d,small)
write.csv(d,file.path(root,"responses.csv"),row.names=FALSE);write.csv(p,file.path(root,"predictors.csv"),row.names=FALSE)
cfg<-list(city_responses=file.path(root,"responses.csv"),city_predictors=file.path(root,"predictors.csv"),output_root=file.path(root,"association"),minimum_cities=50)
path<-file.path(root,"config.json");write_json(cfg,path,auto_unbox=TRUE)
here<-dirname(sub("^--file=","",grep("^--file=",commandArgs(),value=TRUE)))
r<-file.path(R.home("bin"),"Rscript");if(.Platform$OS.type=="windows")r<-paste0(r,".exe")
for(script in c("binary_city_association.R","verify_binary_city_association.R"))stopifnot(system2(r,c(shQuote(file.path(here,script)),shQuote(path)))==0L)
g<-read.csv(file.path(cfg$output_root,"model_support.csv"));stopifnot(all(g$status[g$region=="MAPPED_RECLAMATION"]=="INSUFFICIENT_COMMON_CITY_SUPPORT"))
z<-read.csv(file.path(cfg$output_root,"reclamation_contrasts.csv"));stopifnot(nrow(z)==8L,all(z$mean_q75_minus_q25[z$response=="mean_species_suitable_fraction"]>0))
cat("Paired city support and contrast fixture PASS\n")
