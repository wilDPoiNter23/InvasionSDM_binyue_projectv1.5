# Matched-city three-zone effects with a common outside-ring reference.
suppressPackageStartupMessages(library(jsonlite))
a<-commandArgs(TRUE);if(length(a)!=1L)stop("Supply private configuration")
cfg<-fromJSON(a[1],simplifyVector=TRUE);out<-cfg$output_root
if(file.exists(file.path(out,"zone_effects.csv")))stop("Statistics already exist")
d<-read.csv(file.path(out,"zone_source_means.csv"),stringsAsFactors=FALSE)
gates<-read.csv(file.path(out,"city_geometry_status.csv"),stringsAsFactors=FALSE)
d<-d[d$city_id %in% gates$city_id[gates$status=="EXTRACTED"],]
if(anyDuplicated(paste(d$city_id,d$zone,d$support_policy,d$group)))stop("Duplicate zone source")
zones<-c("OUTSIDE_RING","NON_RECLAIMED_CITY","RECLAIMED_CITY")
sources<-list();effects<-list();support<-list();group_differences<-list()
set.seed(cfg$seed);B<-as.integer(cfg$bootstrap_replicates)
policies<-if(is.null(cfg$support_policies))c("STRICT_684","PAIRED_AVAILABLE")else cfg$support_policies
stopifnot(length(policies)>0L,all(policies%in%c("STRICT_684","PAIRED_AVAILABLE","FIXED_261")))
for(policy in policies)for(minimum in c(1,5)) {
  x<-d[d$support_policy==policy,];ids<-sort(unique(x$city_id));keep<-character()
  for(city in ids) {
    z<-x[x$city_id==city,]
    valid<-nrow(z)==6L && all(is.finite(z$mean_suitability)) && all(z$common_area_km2>0) && all(z$native_cell_equivalents>=minimum)
    if(valid)for(zone in zones) {
      q<-z[z$zone==zone,];valid<-valid && nrow(q)==2L && max(abs(diff(q$common_area_km2)))<1e-8 && max(abs(diff(q$native_cell_equivalents)))<1e-10
    }
    if(valid)valid<-all(z$mean_suitability[z$zone=="OUTSIDE_RING"]>0)
    if(valid)keep<-c(keep,city)
    support[[length(support)+1L]]<-data.frame(city_id=city,support_policy=policy,minimum_cell_equivalents=minimum,in_common_sample=valid,
      reason=if(valid)"ELIGIBLE_ALL_THREE_ZONES_BOTH_GROUPS"else"INSUFFICIENT_COMMON_ZONE_SUPPORT_OR_ZERO_RING")
  }
  n<-length(keep);if(n<2L)next
  sample_indices<-replicate(B,sample.int(n,n,replace=TRUE))
  delta_by_group<-list()
  for(group in c("FULL","ENV")) {
    values<-matrix(NA_real_,nrow=n,ncol=length(zones))
    for(j in seq_along(zones)){q<-x[x$zone==zones[j]&x$group==group,];values[,j]<-q$mean_suitability[match(keep,q$city_id)]}
    colnames(values)<-zones
    if(isTRUE(cfg$include_city_overall)) {
      urban_area<-sapply(zones[2:3],function(zone){q<-x[x$zone==zone&x$group==group,];q$common_area_km2[match(keep,q$city_id)]})
      values<-cbind(values,CITY_TOTAL=rowSums(values[,2:3,drop=FALSE]*urban_area)/rowSums(urban_area))
    }
    indices<-100*values/values[,"OUTSIDE_RING"]
    for(j in seq_along(zones))sources[[length(sources)+1L]]<-data.frame(city_id=keep,group=group,zone=zones[j],support_policy=policy,
      minimum_cell_equivalents=minimum,mean_suitability=values[,j],ring_100_index=indices[,j],n_matched_cities=n)
    contrast<-list(NON_RECLAIMED_MINUS_RING=c(2,1),RECLAIMED_MINUS_RING=c(3,1),RECLAIMED_MINUS_NON_RECLAIMED=c(3,2))
    if(isTRUE(cfg$include_city_overall))contrast$CITY_MINUS_RING<-c(4,1)
    delta_by_group[[group]]<-list()
    for(name in names(contrast)) {
      ab<-contrast[[name]];difference<-values[,ab[1]]-values[,ab[2]];index_difference<-indices[,ab[1]]-indices[,ab[2]]
      delta_by_group[[group]][[name]]<-difference
      boot_difference<-colMeans(matrix(difference[sample_indices],nrow=n))
      boot_index<-colMeans(matrix(index_difference[sample_indices],nrow=n))
      ci<-quantile(boot_difference,c(.025,.975),names=FALSE);ic<-quantile(boot_index,c(.025,.975),names=FALSE)
      effects[[length(effects)+1L]]<-data.frame(group=group,support_policy=policy,minimum_cell_equivalents=minimum,contrast=name,n_cities=n,
        target_mean=mean(values[,ab[1]]),reference_mean=mean(values[,ab[2]]),mean_difference=mean(difference),CI_low=ci[1],CI_high=ci[2],
        mean_ring_100_index_difference=mean(index_difference),ring_index_CI_low=ic[1],ring_index_CI_high=ic[2],
        ratio_of_means_percent=if(mean(values[,ab[2]])>0)100*(mean(values[,ab[1]])/mean(values[,ab[2]])-1)else NA_real_,bootstrap_replicates=B)
    }
  }
  for(name in names(delta_by_group$FULL)) {
    delta<-delta_by_group$FULL[[name]]-delta_by_group$ENV[[name]]
    boot<-colMeans(matrix(delta[sample_indices],nrow=n));ci<-quantile(boot,c(.025,.975),names=FALSE)
    group_differences[[length(group_differences)+1L]]<-data.frame(support_policy=policy,minimum_cell_equivalents=minimum,
      contrast=name,n_cities=n,FULL_minus_ENV_contrast=mean(delta),CI_low=ci[1],CI_high=ci[2])
  }
}
write.csv(do.call(rbind,support),file.path(out,"matched_city_support.csv"),row.names=FALSE)
if(!length(effects))stop("No matched sample with at least two cities")
write.csv(do.call(rbind,sources),file.path(out,"common_baseline_city_source.csv"),row.names=FALSE)
write.csv(do.call(rbind,effects),file.path(out,"zone_effects.csv"),row.names=FALSE)
write.csv(do.call(rbind,group_differences),file.path(out,"FULL_ENV_contrast_differences.csv"),row.names=FALSE)
write_json(list(status="MATCHED_CITY_BOOTSTRAP_COMPUTED",bootstrap_replicates=B,seed=cfg$seed,
  support_sensitivity="At least1 or5 native cell equivalents in each of three zones; no outcome-sign-based filtering",
  baseline="Each matched city's outside ring equals100; absolute means and distinct ratios retained",
  sample="Exact same cities for all three zones and FULL/ENV within each support policy and support threshold",
  interpretation="Spatial suitability contrasts, not causal reclamation effects; mapped cohort geometry differs from historical39percent zones"),file.path(out,"statistics_receipt.json"),auto_unbox=TRUE,pretty=TRUE)
