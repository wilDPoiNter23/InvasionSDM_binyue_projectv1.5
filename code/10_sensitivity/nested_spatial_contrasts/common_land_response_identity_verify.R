# Independently check shared support, original map values and city contrasts.
suppressPackageStartupMessages({library(terra);library(jsonlite);library(digest)})
a<-commandArgs(TRUE);if(length(a)!=2L||file.exists(a[2]))stop("Supply configuration and fresh receipt")
cfg<-fromJSON(a[1]);out<-cfg$output_root;rec<-fromJSON(file.path(out,"receipt.json"))
stopifnot(rec$status=="RESPONSE_IDENTITY_DIAGNOSTIC_COMPUTED",!rec$resampling,!rec$original39percent_reproduction)
for(file in c(cfg$fixed_verification,file.path(cfg$baseline_root,"verification.json")))stopifnot(fromJSON(file)$status=="PASS")
h<-read.csv(file.path(out,"input_checksums.csv"),stringsAsFactors=FALSE)
for(i in seq_len(nrow(h)))stopifnot(digest(file=h$path[i],algo="sha256")==h$sha256[i])
stopifnot(digest(file=cfg$original_sii,algo="sha256")==cfg$original_sii_sha256)
for(i in seq_len(nrow(rec$output_sha256)))stopifnot(digest(file=file.path(out,rec$output_sha256$file[i]),algo="sha256")==rec$output_sha256$sha256[i])
r<-rast(cfg$original_sii);stopifnot(compareGeom(r,rast(cfg$grid_template),stopOnError=FALSE))
raw<-read.csv(file.path(cfg$fixed261_root,"core_cell_values.csv"));v<-read.csv(file.path(out,"joint_cell_values.csv"))
stopifnot(identical(v$cell,raw$cell),!anyDuplicated(v$cell))
original<-as.numeric(extract(r,raw$cell)[[1]])
same<-function(x,y,tol=1e-10){x<-unname(x);y<-unname(y);stopifnot(length(x)==length(y),identical(is.na(x),is.na(y)));keep<-is.finite(x)&is.finite(y);stopifnot(all(abs(x[keep]-y[keep])<tol),identical(is.finite(x),is.finite(y)))}
same(original,v$ORIGINAL_SII);same(raw$FULL,v$FIXED261_FULL);same(raw$ENV,v$FIXED261_ENV)
mask<-raw$joint_finite_species==261L&is.finite(raw$FULL)&is.finite(raw$ENV)&is.finite(original)
stopifnot(identical(mask,v$joint_eligible))
for(field in c("ORIGINAL_SII","FIXED261_FULL","FIXED261_ENV"))stopifnot(all(v[[field]][is.finite(v[[field]])]>=0&v[[field]][is.finite(v[[field]])]<=1))
membership<-read.csv(file.path(out,"joint_cell_membership.csv"),stringsAsFactors=FALSE)
d<-read.csv(file.path(out,"zone_source_means.csv"),stringsAsFactors=FALSE)
gates<-read.csv(file.path(cfg$baseline_root,"city_geometry_status.csv"),stringsAsFactors=FALSE)
ids<-sort(gates$city_id[gates$status=="EXTRACTED"]);groups<-c("ORIGINAL_SII","FIXED261_FULL","FIXED261_ENV")
zones<-c("OUTSIDE_RING","NON_RECLAIMED_CITY","RECLAIMED_CITY")
stopifnot(nrow(d)==9L*length(ids),setequal(d$city_id,ids),!anyDuplicated(d[,c("city_id","zone","group")]))
checks<-0L;found<-character()
for(path in list.files(file.path(cfg$baseline_root,"cells"),pattern="[.]csv$",full.names=TRUE)){
 e<-read.csv(path,stringsAsFactors=FALSE);city<-unique(e$city_id);if(!city%in%ids)next
 found<-c(found,city);q<-membership[membership$city_id==city,];stopifnot(nrow(q)==nrow(e),!anyDuplicated(q[,c("zone","cell")]))
 j<-match(paste(e$zone,e$cell),paste(q$zone,q$cell));stopifnot(!anyNA(j));q<-q[j,]
 stopifnot(identical(q$zone,e$zone),identical(q$cell,e$cell));same(q$area_km2,e$area_km2);same(q$fraction,e$fraction)
 ix<-match(e$cell,raw$cell);stopifnot(!anyNA(ix));take<-mask[ix]&is.finite(e$area_km2)&e$area_km2>0
 stopifnot(identical(q$joint_eligible,take))
 for(zone in zones){
  rows<-which(take&e$zone==zone);den<-sum(e$area_km2[rows]);w<-if(den>0)e$area_km2[rows]/den else numeric()
  for(group in groups){
   observed<-d[d$city_id==city&d$zone==zone&d$group==group,];stopifnot(nrow(observed)==1L)
   value<-switch(group,ORIGINAL_SII=original,FIXED261_FULL=raw$FULL,FIXED261_ENV=raw$ENV)
   expected<-if(den>0)crossprod(w,value[ix[rows]])[1]else NA_real_
   same(expected,observed$mean_suitability);same(den,observed$common_area_km2)
   same(sum(e$fraction[rows]),observed$native_cell_equivalents)
   stopifnot(observed$finite_native_cells==length(unique(e$cell[rows])));checks<-checks+1L
  }
 }
}
stopifnot(setequal(found,ids),!anyDuplicated(found))
support<-read.csv(file.path(out,"matched_city_support.csv"),stringsAsFactors=FALSE)
source<-read.csv(file.path(out,"common_baseline_city_source.csv"),stringsAsFactors=FALSE)
effects<-read.csv(file.path(out,"zone_effects.csv"),stringsAsFactors=FALSE)
stopifnot(nrow(support)==3L*length(ids),!anyDuplicated(support[,c("city_id","minimum_cell_equivalents")]),
 !anyDuplicated(source[,c("city_id","group","zone","minimum_cell_equivalents")]),
 !anyDuplicated(effects[,c("group","contrast","minimum_cell_equivalents")]))
set.seed(cfg$seed);expected_sources<-expected_effects<-0L
for(minimum in c(0,1,5)){
 keep<-ids[vapply(ids,function(city){x<-d[d$city_id==city,];all(is.finite(x$mean_suitability))&&all(x$common_area_km2>0)&&all(x$native_cell_equivalents>=minimum)&&all(x$mean_suitability[x$zone=="OUTSIDE_RING"]>0)},logical(1))]
 gate<-support[support$minimum_cell_equivalents==minimum,];stopifnot(setequal(gate$city_id,ids),setequal(gate$city_id[gate$eligible],keep))
 stopifnot(all(gate$scope==if(minimum==0)"EXPLORATORY_ALL_POSITIVE_AREA"else"NATIVE_SUPPORT"))
 n<-length(keep);if(n<2L)next
 draws<-matrix(unlist(lapply(seq_len(cfg$bootstrap_replicates),function(j)sample.int(n,n,TRUE))),nrow=n)
 for(group in groups){
  x<-d[d$group==group,];value<-area<-matrix(NA_real_,n,3,dimnames=list(keep,zones))
  for(zone in zones){q<-x[x$zone==zone,];j<-match(keep,q$city_id);value[,zone]<-q$mean_suitability[j];area[,zone]<-q$common_area_km2[j]}
  city<-apply(cbind(value[,2:3],area[,2:3]),1,function(z)sum(z[1:2]*z[3:4])/sum(z[3:4]))
  value<-cbind(value,CITY_TOTAL=city);ring<-value[,1]
  for(zone in colnames(value)){
   q<-source[source$minimum_cell_equivalents==minimum&source$group==group&source$zone==zone,];stopifnot(nrow(q)==n,setequal(q$city_id,keep),all(q$n_matched_cities==n));q<-q[match(keep,q$city_id),]
   same(q$mean_suitability,value[,zone]);same(q$ring_100_index,100*value[,zone]/ring,1e-8);expected_sources<-expected_sources+n
  }
  definitions<-list(NON_RECLAIMED_MINUS_RING=c(2,1),RECLAIMED_MINUS_RING=c(3,1),RECLAIMED_MINUS_NON_RECLAIMED=c(3,2),CITY_MINUS_RING=c(4,1))
  for(label in names(definitions)){
   ab<-definitions[[label]];delta<-value[,ab[1]]-value[,ab[2]];idelta<-100*delta/ring
   boot<-vapply(seq_len(ncol(draws)),function(j)mean(delta[draws[,j]]),numeric(1));ci<-quantile(boot,c(.025,.975),names=FALSE)
   iboot<-vapply(seq_len(ncol(draws)),function(j)mean(idelta[draws[,j]]),numeric(1));ic<-quantile(iboot,c(.025,.975),names=FALSE)
   q<-effects[effects$minimum_cell_equivalents==minimum&effects$group==group&effects$contrast==label,];stopifnot(nrow(q)==1L,q$n_cities==n,q$bootstrap_replicates==1999L,
    q$status==if(n>=20L&&minimum>0)"PRIMARY_SUPPORT_THRESHOLD_MET"else"EXPLORATORY_BELOW_20_CITIES_OR_ALL_POSITIVE_AREA")
   same(c(q$target_mean,q$reference_mean,q$mean_difference,q$CI_low,q$CI_high),c(mean(value[,ab[1]]),mean(value[,ab[2]]),mean(delta),ci))
   same(c(q$mean_ring_100_index_difference,q$ring_index_CI_low,q$ring_index_CI_high),c(mean(idelta),ic),1e-8)
   ratio<-if(mean(value[,ab[2]])>0)100*(mean(value[,ab[1]])/mean(value[,ab[2]])-1)else NA_real_
   same(q$ratio_of_means_percent,ratio,1e-8);expected_effects<-expected_effects+1L
  }
 }
}
stopifnot(nrow(source)==expected_sources,nrow(effects)==expected_effects)
write_json(list(status="PASS",original_map_cells_reextracted=nrow(raw),zone_mean_checks=checks,matched_city_source_rows=expected_sources,contrast_bootstrap_rows=expected_effects,
 original_sii_sha256=digest(file=cfg$original_sii,algo="sha256"),result_receipt_sha256=digest(file=file.path(out,"receipt.json"),algo="sha256"),
 interpretation="Arithmetic and shared support verified; historical39percent geometry and original response normalization remain distinct"),a[2],auto_unbox=TRUE,pretty=TRUE)
