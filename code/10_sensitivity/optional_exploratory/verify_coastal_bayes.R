# Independently reconstruct sampled source summaries and posterior contrasts.
suppressPackageStartupMessages({library(terra);library(jsonlite);library(digest)})
a<-commandArgs(TRUE);stopifnot(length(a)==1L);cfg<-fromJSON(a[1]);out<-cfg$output_root
s<-readRDS(file.path(out,'native_support.rds'));p<-read.csv(cfg$paired_manifest)
stopifnot(!any(tolower(s$zones$city_id)%in%tolower(cfg$exclude_city_keys)))
stopifnot(digest(file=file.path(out,'native_support.rds'),algo='sha256')==fromJSON(file.path(out,'support_receipt.json'))$support_sha256)
set.seed(1661);checks<-list();models<-list()
for(group in c('ENV','FULL')){
 files<-list.files(file.path(out,'species',group),pattern='[.]csv$',full.names=TRUE);stopifnot(length(files)==684)
 for(index in c(1L,171L,343L,514L,684L)){
  z<-read.csv(files[index]);path<-p[[paste0(tolower(group),'_projection')]][index]
  stopifnot(all(z$species==p$species[index]),all(z$group==group))
  for(zone in sample(s$zones$zone_id,5)){
   w<-s$weights[s$weights$zone_id==zone,];r<-rast(path);tt<-terra::extract(r,xyFromCell(r,w$cell));v<-as.numeric(tt[[ncol(tt)]])/p[[paste0(tolower(group),'_scale_divisor')]][index]
   valid<-is.finite(v);den<-sum(w$weight_km2[valid]);mean<-if(den>0)sum(w$weight_km2[valid]*v[valid])/den else NA_real_
   row<-z[z$zone_id==zone,];stopifnot(nrow(row)==1,abs(row$finite_area_km2-den)<1e-10,is.finite(mean)==is.finite(row$mean_suitability))
   if(is.finite(mean))stopifnot(abs(mean-row$mean_suitability)<1e-12)
   checks[[length(checks)+1L]]<-data.frame(group=group,species=p$species[index],zone_id=zone,finite_area_km2=den,source_mean=mean,stored_mean=row$mean_suitability)
  }
 }
 model_directory<-if(is.null(cfg$model_directory))'models'else cfg$model_directory
 root<-file.path(out,model_directory,group);complete<-fromJSON(file.path(root,'completion.json'));stopifnot(complete$status=='BAYESIAN_FITS_COMPLETED')
 for(case in c('primary','prior_sensitivity','transform_sensitivity')){
  dest<-file.path(root,case);f<-readRDS(file.path(dest,'posterior_chains.rds'));B<-do.call(rbind,lapply(f,`[[`,'beta'));q<-read.csv(file.path(dest,'posterior_summary.csv'))
  for(term in colnames(B)){v<-B[,term];row<-q[q$term==term,];stopifnot(abs(row$mean-mean(v))<1e-10,abs(row$lower-quantile(v,.025))<1e-10,abs(row$upper-quantile(v,.975))<1e-10)}
  receipt<-fromJSON(file.path(dest,'receipt.json'));models[[paste(group,case)]]<-receipt$status
 }
}
write.csv(do.call(rbind,checks),file.path(out,'independent_source_checks.csv'),row.names=FALSE)
write_json(list(status='PASS_SOURCE_AND_POSTERIOR_RECONSTRUCTION',source_checks=length(checks),models=models,
 scope='Independent point-coordinate raster reads and posterior coefficient summaries; chain status reported separately; no SDM refitting'),file.path(out,'independent_verification.json'),auto_unbox=TRUE,pretty=TRUE)
