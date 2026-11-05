# Compare the historical response and fixed-community means on shared native cells.
suppressPackageStartupMessages({library(terra);library(jsonlite);library(digest)})
a<-commandArgs(TRUE);if(length(a)!=1L)stop("Supply private configuration")
cfg<-fromJSON(a[1]);out<-cfg$output_root
if(dir.exists(out))stop("Fresh output required")
stopifnot(cfg$bootstrap_replicates==1999L,identical(as.numeric(cfg$minimum_cell_equivalents),c(0,1,5)))
for(p in c(cfg$fixed_verification,file.path(cfg$baseline_root,"verification.json")))if(fromJSON(p)$status!="PASS")stop("Verified source results required")
stopifnot(digest(file=cfg$original_sii,algo="sha256")==cfg$original_sii_sha256)
r<-rast(cfg$original_sii);template<-rast(cfg$grid_template)
stopifnot(nlyr(r)==1L,is.lonlat(r),compareGeom(r,template,stopOnError=FALSE),
 nrow(r)==4320L,ncol(r)==8640L,all(abs(res(r)-1/24)<1e-10))
fixed_receipt<-fromJSON(file.path(cfg$fixed261_root,"extraction_receipt.json"))
stopifnot(fixed_receipt$frozen_species_n==261L,
 digest(file=file.path(cfg$fixed261_root,"core_cell_values.csv"),algo="sha256")==fixed_receipt$cell_values_sha256)
v<-read.csv(file.path(cfg$fixed261_root,"core_cell_values.csv"));stopifnot(!anyDuplicated(v$cell))
v$ORIGINAL_SII<-as.numeric(extract(r,v$cell)[[1]])
v$FIXED261_FULL<-v$FULL;v$FIXED261_ENV<-v$ENV
groups<-c("ORIGINAL_SII","FIXED261_FULL","FIXED261_ENV")
for(g in groups)stopifnot(all(v[[g]][is.finite(v[[g]])]>=0&v[[g]][is.finite(v[[g]])]<=1))
v$joint_eligible<-v$joint_finite_species==261L&apply(is.finite(as.matrix(v[,groups])),1,all)
gates<-read.csv(file.path(cfg$baseline_root,"city_geometry_status.csv"),stringsAsFactors=FALSE)
ids<-gates$city_id[gates$status=="EXTRACTED"]
files<-list.files(file.path(cfg$baseline_root,"cells"),pattern="[.]csv$",full.names=TRUE)
input_files<-unique(c(a[1],cfg$original_sii,cfg$grid_template,cfg$fixed_verification,
 file.path(cfg$fixed261_root,c("extraction_receipt.json","core_cell_values.csv","core_species_roster.csv")),
 file.path(cfg$baseline_root,c("verification.json","city_geometry_status.csv","input_checksums.csv")),files))
dir.create(out,recursive=TRUE)
write.csv(data.frame(path=input_files,sha256=vapply(input_files,function(p)digest(file=p,algo="sha256"),character(1))),file.path(out,"input_checksums.csv"),row.names=FALSE)
write.csv(v[,c("cell","joint_finite_species",groups,"joint_eligible")],file.path(out,"joint_cell_values.csv"),row.names=FALSE)
zones<-c("OUTSIDE_RING","NON_RECLAIMED_CITY","RECLAIMED_CITY")
rows<-list();support_cells<-list();seen<-character()
for(path in files){
 e<-read.csv(path,stringsAsFactors=FALSE);city<-unique(e$city_id)
 if(length(city)!=1L)stop("City cache identity differs")
 if(!city%in%ids)next
 seen<-c(seen,city);ix<-match(e$cell,v$cell);stopifnot(!anyNA(ix),!anyDuplicated(e[,c("zone","cell")]))
 take<-v$joint_eligible[ix]&is.finite(e$area_km2)&e$area_km2>0
 support_cells[[city]]<-data.frame(city_id=city,zone=e$zone,cell=e$cell,area_km2=e$area_km2,
  fraction=e$fraction,joint_eligible=take)
 for(zone in zones){
  j<-which(take&e$zone==zone);w<-e$area_km2[j];den<-sum(w)
  for(g in groups)rows[[length(rows)+1L]]<-data.frame(city_id=city,zone=zone,group=g,
   mean_suitability=if(den>0)sum(w*v[[g]][ix[j]])/den else NA_real_,common_area_km2=den,
   native_cell_equivalents=sum(e$fraction[j]),finite_native_cells=length(unique(e$cell[j])))
 }
}
stopifnot(!anyDuplicated(seen),setequal(seen,ids))
d<-do.call(rbind,rows);write.csv(d,file.path(out,"zone_source_means.csv"),row.names=FALSE)
write.csv(do.call(rbind,support_cells),file.path(out,"joint_cell_membership.csv"),row.names=FALSE)
eligibility<-list();effects<-list();city_source<-list();set.seed(cfg$seed)
for(minimum in cfg$minimum_cell_equivalents){
 keep<-character()
 for(city in sort(ids)){
  q<-d[d$city_id==city,];ok<-nrow(q)==9L&&all(is.finite(q$mean_suitability))&&all(q$common_area_km2>0)&&
   all(q$native_cell_equivalents>=minimum)&&all(q$mean_suitability[q$zone=="OUTSIDE_RING"]>0)
  eligibility[[length(eligibility)+1L]]<-data.frame(city_id=city,minimum_cell_equivalents=minimum,
   scope=if(minimum==0)"EXPLORATORY_ALL_POSITIVE_AREA"else"NATIVE_SUPPORT",eligible=ok)
  if(ok)keep<-c(keep,city)
 }
 n<-length(keep);if(n<2L)next
 draw<-replicate(cfg$bootstrap_replicates,sample.int(n,n,replace=TRUE))
 for(g in groups){
  x<-d[d$group==g,];y<-w<-matrix(NA_real_,n,3,dimnames=list(keep,zones))
  for(zone in zones){q<-x[x$zone==zone,];j<-match(keep,q$city_id);y[,zone]<-q$mean_suitability[j];w[,zone]<-q$common_area_km2[j]}
  y<-cbind(y,CITY_TOTAL=rowSums(y[,2:3,drop=FALSE]*w[,2:3,drop=FALSE])/rowSums(w[,2:3,drop=FALSE]))
  index<-100*y/y[,"OUTSIDE_RING"]
  for(zone in colnames(y))city_source[[length(city_source)+1L]]<-data.frame(city_id=keep,group=g,zone=zone,
   minimum_cell_equivalents=minimum,mean_suitability=y[,zone],ring_100_index=index[,zone],n_matched_cities=n)
  definitions<-list(NON_RECLAIMED_MINUS_RING=c(2,1),RECLAIMED_MINUS_RING=c(3,1),
   RECLAIMED_MINUS_NON_RECLAIMED=c(3,2),CITY_MINUS_RING=c(4,1))
  for(label in names(definitions)){
   ab<-definitions[[label]];delta<-y[,ab[1]]-y[,ab[2]];idelta<-index[,ab[1]]-index[,ab[2]]
   ci<-quantile(colMeans(matrix(delta[draw],nrow=n)),c(.025,.975),names=FALSE)
   ic<-quantile(colMeans(matrix(idelta[draw],nrow=n)),c(.025,.975),names=FALSE)
   effects[[length(effects)+1L]]<-data.frame(group=g,minimum_cell_equivalents=minimum,contrast=label,n_cities=n,
    target_mean=mean(y[,ab[1]]),reference_mean=mean(y[,ab[2]]),mean_difference=mean(delta),CI_low=ci[1],CI_high=ci[2],
    mean_ring_100_index_difference=mean(idelta),ring_index_CI_low=ic[1],ring_index_CI_high=ic[2],
    ratio_of_means_percent=if(mean(y[,ab[2]])>0)100*(mean(y[,ab[1]])/mean(y[,ab[2]])-1)else NA_real_,bootstrap_replicates=cfg$bootstrap_replicates,
    status=if(n>=20L&&minimum>0)"PRIMARY_SUPPORT_THRESHOLD_MET"else"EXPLORATORY_BELOW_20_CITIES_OR_ALL_POSITIVE_AREA")
  }
 }
}
write.csv(do.call(rbind,eligibility),file.path(out,"matched_city_support.csv"),row.names=FALSE)
if(!length(effects))stop("No matched sample with two cities")
write.csv(do.call(rbind,city_source),file.path(out,"common_baseline_city_source.csv"),row.names=FALSE)
write.csv(do.call(rbind,effects),file.path(out,"zone_effects.csv"),row.names=FALSE)
write_json(list(status="RESPONSE_IDENTITY_DIAGNOSTIC_COMPUTED",configuration=cfg,
 original_sii_sha256=cfg$original_sii_sha256,resampling=FALSE,bootstrap_replicates=cfg$bootstrap_replicates,seed=cfg$seed,
 original_response="Historical manuscript SII; original species roster and normalization unresolved",
 geometry="New mapped five-year coastal reclamation cohorts1985-2020 and current108 city boundaries",
 fixed_response="261 fixed species with jointly finite FULL/ENV cells; additionally restricted to finite historical SII",
 same_cell_weights_for_all_responses=TRUE,original39percent_reproduction=FALSE,
 limitation="Within-response spatial contrasts on shared support; scales need not be comparable across differently normalized responses",
 output_sha256=lapply(c("zone_source_means.csv","joint_cell_values.csv","joint_cell_membership.csv","matched_city_support.csv","common_baseline_city_source.csv","zone_effects.csv"),function(p)list(file=p,sha256=digest(file=file.path(out,p),algo="sha256")))),
 file.path(out,"receipt.json"),pretty=TRUE,auto_unbox=TRUE)
writeLines(capture.output(sessionInfo()),file.path(out,"sessionInfo.txt"))
