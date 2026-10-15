#!/usr/bin/env Rscript
# Check period dissolving, size thresholds, unknown support and association controls.
suppressPackageStartupMessages({library(sf);library(jsonlite)})
args<-commandArgs(TRUE);source(args[1]);out<-args[2]
dir.create(out,recursive=TRUE);if(file.exists(file.path(out,"config.json")))stop("Existing fixture retained")
box<-function(x,y,w,h)st_polygon(list(matrix(c(x,y,x+w,y,x+w,y+h,x,y+h,x,y),ncol=2,byrow=TRUE)))
verification_source<-file.path(dirname(args[1]),"verify_reclamation_configuration.R")
expressions<-parse(verification_source)
helper<-vapply(expressions,function(x)is.call(x)&&identical(x[[1]],as.name("<-"))&&identical(x[[2]],as.name("check_component_geometry")),logical(1))
if(!any(helper))stop("Missing component geometry verifier helper")
eval(expressions[[which(helper)]])
large<-st_sf(city_id="Large coordinate fixture",component=1:2,geometry=st_sfc(box(13000000,4000000,1111.1,777.7),box(13002000,4000000,255.5,333.3),crs=6933))
large$area_ha<-as.numeric(st_area(large))/10000
precision<-check_component_geometry(large)
stopifnot(precision$overlap_pairs==0L,precision$threshold_class_changes==0L,precision$local_union_error_ha<1e-8)
overlap<-large;st_geometry(overlap)[2]<-st_geometry(overlap)[1];overlap$area_ha<-as.numeric(st_area(overlap))/10000
stopifnot(inherits(tryCatch(check_component_geometry(overlap),error=function(e)e),"error"))
city<-st_sf(shapeName=c("City A","City B","City C"),geometry=st_sfc(box(0,0,4000,1000),box(5000,0,100,100),box(6000,0,100,100),crs=6933))
st_write(city,file.path(out,"cities.gpkg"),quiet=TRUE)
dir.create(file.path(out,"cohorts"))
# Touching 0.5 ha periods must dissolve into one 1 ha component before thresholding.
z<-st_sf(city_id="City A",reported_category=c(1990,1995,2000,2005),geometry=st_sfc(box(0,0,50,100),box(50,0,50,100),box(200,0,200,500),box(1000,0,1000,1000),crs=6933))
saveRDS(z,file.path(out,"cohorts","city_001.rds"))
saveRDS(st_sf(city_id="City B",reported_category=1990,geometry=st_sfc(box(5000,0,50,100),crs=6933)),file.path(out,"cohorts","city_002.rds"))
saveRDS(st_sf(city_id="City C",reported_category=1990,geometry=st_sfc(box(6000,0,100,100),crs=6933)),file.path(out,"cohorts","city_003.rds"))
coverage<-data.frame(layer="coastal_change",city_id=city$shapeName,mapped_area_km2=c(1.11,.005,.01),overlap_excess_km2=c(0,0,1))
write.csv(coverage,file.path(out,"coverage.csv"),row.names=FALSE)
cfg<-list(output_root=file.path(out,"results"),cities=file.path(out,"cities.gpkg"),city_id="shapeName",expected_cities=3,
 coverage_table=file.path(out,"coverage.csv"),cohort_checkpoint_root=file.path(out,"cohorts"),reported_cohort_endpoints=c(1990,1995,2000,2005,2010,2015,2020),minimum_association_cities=30)
write_json(cfg,file.path(out,"config.json"),auto_unbox=TRUE)
status<-system2(file.path(R.home("bin"),"Rscript"),c("--vanilla",shQuote(args[1]),shQuote(file.path(out,"config.json"))))
stopifnot(status==0L)
r<-read.csv(file.path(out,"results","configuration.csv"));a<-r[r$city_id=="City A",]
stopifnot(identical(a$n_retained,c(3L,2L,1L)),max(abs(a$retained_area_ha-c(111,110,100)))<1e-10,
 abs(a$effective_patch_number[a$cutoff_ha==1]-111^2/(1+100+10000))<1e-10)
b<-r[r$city_id=="City B",];stopifnot(all(b$n_retained==0),all(is.na(b$effective_patch_number)),all(is.na(b$largest_patch_fraction)))
gate<-read.csv(file.path(out,"results","city_geometry_status.csv"));stopifnot(gate$status[gate$city_id=="City C"]=="EXCLUDED_GEOMETRY")
set.seed(20261015);n<-40L
d<-data.frame(city=paste0("Test",seq_len(n)),key=paste0("test",seq_len(n)),city_area=rnorm(n,5),recl=rnorm(n,4),intensity=rnorm(n,6),richness=rpois(n,30))
dir.create(file.path(out,"observed"));dir.create(file.path(out,"association_test"))
for(model in c("M0","M1")){
 C<-cbind(1,d$city_area,if(model=="M1")d$intensity);d$residual<-lm.fit(C,d$richness)$residuals
 write.csv(d,file.path(out,"observed",paste0("group_data_",model,".csv")),row.names=FALSE)
}
synthetic<-do.call(rbind,lapply(seq_len(n),function(i){
 areas<-c(runif(2L+i%%4L,101,1000),runif(2L+i%%3L,11,90),runif(2L+i%%5L,1,9))
 do.call(rbind,lapply(c(1,10,100),function(cutoff){a<-configuration_metrics(areas,cutoff);a$city_id<-d$city[i];a}))
}))
cfg$observed_residual_root<-file.path(out,"observed");cfg$minimum_association_cities<-30
associations<-configuration_associations(synthetic,cfg,file.path(out,"association_test"),d$city)
stopifnot(nrow(associations)==18L,all(associations$status=="FITTED"),all(associations$n==40L),all(is.finite(associations$p_bh)))
samples<-read.csv(file.path(out,"association_test","association_samples.csv"))
for(i in seq_len(nrow(associations))){
 row<-associations[i,];a<-samples[samples$model==row$model&samples$cutoff_ha==row$cutoff_ha&samples$metric==row$metric,]
 x<-(a$x-mean(a$x))/sd(a$x);y<-(a$residual-mean(a$residual))/sd(a$residual)
 C<-cbind(1,a$city_area,a$recl);rx<-x-drop(C%*%qr.solve(C,x));ry<-y-drop(C%*%qr.solve(C,y))
 slope<-sum(rx*ry)/sum(rx^2);stopifnot(abs(slope-row$estimate)<1e-9)
}
# A residual that additionally removed reclamation is explicitly rejected.
wrong<-read.csv(file.path(out,"observed","group_data_M0.csv"));wrong$residual<-lm.fit(cbind(1,wrong$city_area,wrong$recl),wrong$richness)$residuals
write.csv(wrong,file.path(out,"observed","group_data_M0.csv"),row.names=FALSE)
rejected<-tryCatch(configuration_associations(synthetic,cfg,file.path(out,"association_test"),d$city),error=function(e)e)
stopifnot(inherits(rejected,"error"))
cat("PASS_MAPPED_CONFIGURATION_SYNTHETIC\n")
