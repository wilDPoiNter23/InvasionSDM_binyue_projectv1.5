#!/usr/bin/env Rscript
# Reconstruct mapped component metrics, geometry conservation and partial associations.
suppressPackageStartupMessages({library(sf);library(jsonlite);library(digest)})
cfg<-fromJSON(commandArgs(TRUE)[1],simplifyVector=TRUE);out<-cfg$output_root
if(file.exists(file.path(out,"verification.json")))stop("Existing configuration verification retained")
sf_use_s2(FALSE)
input_checks<-read.csv(file.path(out,"input_checksums.csv"),stringsAsFactors=FALSE)
for(i in seq_len(nrow(input_checks)))stopifnot(digest(file=input_checks$file[i],algo="sha256")==input_checks$sha256[i])
r<-read.csv(file.path(out,"configuration.csv"));parts<-read.csv(file.path(out,"component_areas.csv"));gates<-read.csv(file.path(out,"city_geometry_status.csv"))
source<-read.csv(file.path(out,"cohort_source_checksums.csv"),stringsAsFactors=FALSE)
cities<-st_transform(st_make_valid(st_read(cfg$cities,quiet=TRUE)),6933);cities$city_id<-as.character(cities[[cfg$city_id]])
coverage<-read.csv(cfg$coverage_table);coverage<-coverage[coverage$layer=="coastal_change",]
bad<-coverage$city_id[!is.finite(coverage$mapped_area_km2)|!is.finite(coverage$overlap_excess_km2)|coverage$overlap_excess_km2>pmax(.001,.001*coverage$mapped_area_km2)]
stopifnot(nrow(gates)==nrow(cities),!any(gates$status[gates$city_id %in% bad]!="EXCLUDED_GEOMETRY"),
 setequal(r$city_id,gates$city_id[gates$status=="CONFIGURATION_AVAILABLE"]),!anyDuplicated(paste(r$city_id,r$cutoff_ha)))
for(i in seq_len(nrow(r))){
 q<-r[i,];a<-parts$area_ha[parts$city_id==q$city_id];retained<-a[a>=q$cutoff_ha];A<-sum(retained)
 stopifnot(q$n_retained==length(retained),abs(q$retained_area_ha-A)<1e-8,abs(q$total_mapped_area_ha-sum(a))<1e-8)
 stopifnot(abs(q$retained_area_fraction-A/sum(a))<1e-10,
  q$retained_boundary_components==sum(parts$touches_city_boundary[parts$city_id==q$city_id]&a>=q$cutoff_ha))
 if(length(retained))stopifnot(abs(q$largest_patch_fraction-max(retained)/A)<1e-10,
  abs(q$effective_patch_number-1/sum((retained/A)^2))<1e-9,q$effective_patch_number>=1-1e-10,q$effective_patch_number<=q$n_retained+1e-8)else
  stopifnot(is.na(q$largest_patch_fraction),is.na(q$effective_patch_number),q$support=="NO_COMPONENT_ABOVE_ANALYST_CUTOFF")
}
for(city in unique(r$city_id)){
 a<-r[r$city_id==city,];a<-a[order(a$cutoff_ha),];stopifnot(all(diff(a$n_retained)<=0),all(diff(a$retained_area_ha)<=0))
 i<-match(city,cities$city_id);p<-readRDS(file.path(out,"components",sprintf("city_%03d.rds",i)))
 stopifnot(st_crs(p)$epsg==6933L,all(st_is_valid(p)),max(abs(as.numeric(st_area(p))/10000-p$area_ha))<1e-8,
  abs(sum(p$area_ha)-sum(as.numeric(st_area(st_union(p))))/10000)<1e-8)
}
# Rebuild source unions for a deterministic subset, independent of saved component areas.
selected<-if(nrow(source))unique(round(seq(1,nrow(source),length.out=min(16,nrow(source)))))else integer()
for(j in selected){
 s<-source[j,];stopifnot(digest(file=s$file,algo="sha256")==s$sha256)
 z<-readRDS(s$file);i<-match(s$city_id,cities$city_id)
 q<-suppressWarnings(st_intersection(st_union(st_geometry(st_transform(st_make_valid(z),6933))),st_geometry(cities[i,])))
 expected<-sum(as.numeric(st_area(q)))/10000;actual<-sum(parts$area_ha[parts$city_id==s$city_id])
 stopifnot(abs(expected-actual)<=max(1e-8,1e-8*expected))
}
association_n<-0L
if(file.exists(file.path(out,"associations.csv"))){
 a<-read.csv(file.path(out,"associations.csv"));d<-read.csv(file.path(out,"association_samples.csv"));stopifnot(nrow(a)==18L)
 key<-function(x)chartr("ABCDEFGHIJKLMNOPQRSTUVWXYZ","abcdefghijklmnopqrstuvwxyz",trimws(as.character(x)))
 for(i in seq_len(nrow(a))){
  row<-a[i,];s<-d[d$model==row$model&d$cutoff_ha==row$cutoff_ha&d$metric==row$metric,];stopifnot(nrow(s)==row$n)
  observed<-read.csv(file.path(cfg$observed_residual_root,paste0("group_data_",row$model,".csv")),stringsAsFactors=FALSE)
  baseline_C<-cbind(1,observed$city_area,if(row$model=="M1")observed$intensity)
  stopifnot(max(abs(observed$residual-(observed$richness-drop(baseline_C%*%qr.solve(baseline_C,observed$richness)))))<1e-8)
  configuration<-r[r$cutoff_ha==row$cutoff_ha&is.finite(r$effective_patch_number),];configuration$key<-key(configuration$city_id)
  source<-merge(configuration,observed,by="key")
  source$x<-if(row$metric=="largest_patch_fraction")source[[row$metric]]else log1p(source[[row$metric]])
  source<-source[which(is.finite(source$x)&is.finite(source$residual)&is.finite(source$city_area)&is.finite(source$recl)),]
  stopifnot(setequal(s$key,source$key));ix<-match(s$key,source$key)
  if(nrow(s))for(field in c("x","residual","city_area","recl"))stopifnot(max(abs(s[[field]]-source[[field]][ix]))<1e-9)
  if(row$status!="FITTED")next
  y<-(s$residual-mean(s$residual))/sd(s$residual);x<-(s$x-mean(s$x))/sd(s$x)
  C<-cbind(1,s$city_area,s$recl);rx<-x-drop(C%*%qr.solve(C,x));ry<-y-drop(C%*%qr.solve(C,y));estimate<-sum(rx*ry)/sum(rx^2)
  X<-cbind(C,x);beta<-qr.solve(X,y);residual<-y-drop(X%*%beta);h<-rowSums(qr.Q(qr(X))^2)
  se<-sqrt(sum((rx*residual/(1-h))^2))/sum(rx^2);df<-nrow(s)-4L;ci<-estimate+c(-1,1)*qt(.975,df)*se;p<-2*pt(-abs(estimate/se),df)
  stopifnot(abs(estimate-row$estimate)<1e-8,abs(se-row$se)<1e-8,max(abs(ci-c(row$lower,row$upper)))<1e-8,abs(p-row$p)<1e-8)
  association_n<-association_n+1L
 }
 for(model in c("M0","M1")){ix<-which(a$model==model);valid<-is.finite(a$p[ix]);if(any(valid))stopifnot(max(abs(a$p_bh[ix][valid]-p.adjust(a$p[ix],"BH",n=9)[valid]))<1e-10)}
}
write_json(list(status="PASS_MAPPED_CONFIGURATION_NUMERICAL_QA",configuration_rows=nrow(r),eligible_cities=length(unique(r$city_id)),
 source_geometry_checks=length(selected),association_models_checked=association_n,
 verified=c("Whole-city coverage exclusions","Threshold counts and area fractions","Effective number and largest component fraction","Saved geometry validity and union area","Source geometry reconstruction subset","FWL and QR HC3 associations when fitted"),
 habitat_fragmentation_or_causal_acceptance=FALSE),file.path(out,"verification.json"),auto_unbox=TRUE,pretty=TRUE)
cat("PASS_MAPPED_CONFIGURATION_NUMERICAL_QA\n")
