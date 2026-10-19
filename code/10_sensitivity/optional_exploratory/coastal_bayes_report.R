# Consolidate complete posterior tables and interpretable point comparisons.
suppressPackageStartupMessages({library(jsonlite);library(digest)})
a<-commandArgs(TRUE);stopifnot(length(a)==1L);cfg<-fromJSON(a[1]);root<-cfg$output_root
out<-file.path(root,'report_tables');stopifnot(!dir.exists(out));dir.create(out)
contrasts<-list();terms<-list();support<-list();receipts<-list()
groups<-if(is.null(cfg$model_groups))c('ENV','FULL')else cfg$model_groups
for(group in groups){
 mr<-file.path(root,cfg$model_directory,group);completion<-fromJSON(file.path(mr,'completion.json'));stopifnot(completion$status=='BAYESIAN_FITS_COMPLETED')
 d<-readRDS(file.path(mr,'model_input.rds'));z<-unique(d[,c('city_id','zone_id','landuse','age_index_years')])
 for(use in unique(as.character(z$landuse))){x<-z[z$landuse==use,];support[[paste(group,use)]]<-data.frame(group=group,landuse=use,cities=length(unique(x$city_id)),zones=nrow(x),minimum_age_index=min(x$age_index_years),maximum_age_index=max(x$age_index_years))}
 for(case in names(completion$cases)){
  cr<-file.path(mr,case);receipt<-fromJSON(file.path(cr,'receipt.json'));pred<-read.csv(file.path(cr,'age_use_predictions.csv'));c<-read.csv(file.path(cr,'response_scale_contrasts.csv'));s<-read.csv(file.path(cr,'posterior_summary.csv'))
  c$relative_percent_point<-NA_real_;c$reference_predicted_mean<-NA_real_
  for(i in seq_len(nrow(c))){r<-c[i,]
   if(r$type=='USE_VS_REFERENCE'){
    denominator<-pred$mean[pred$landuse==r$from&pred$age_index_years==r$age_index_years]
    numerator<-pred$mean[pred$landuse==r$to&pred$age_index_years==r$age_index_years]
   }else{
    denominator<-pred$mean[pred$landuse==receipt$reference_use&pred$age_index_years==as.numeric(r$from)]
    numerator<-pred$mean[pred$landuse==receipt$reference_use&pred$age_index_years==as.numeric(r$to)]
   }
   stopifnot(length(denominator)==1,length(numerator)==1,denominator>0,abs(numerator-denominator-r$difference)<1e-12)
   c$reference_predicted_mean[i]<-denominator;c$relative_percent_point[i]<-100*(numerator/denominator-1)
  }
  c$group<-group;c$case<-case;c$chain_status<-receipt$status;c$supported_direction<-ifelse(c$lower>0,'POSITIVE',ifelse(c$upper<0,'NEGATIVE','UNRESOLVED'))
  s$group<-group;s$case<-case;contrasts[[paste(group,case)]]<-c;terms[[paste(group,case)]]<-s
  receipts[[paste(group,case)]]<-data.frame(group=group,case=case,chain_status=receipt$status,cities=receipt$cities,zones=receipt$zones,species=receipt$species,rows=receipt$rows,
   maximum_rhat=max(s$rank_split_rhat),minimum_ess=min(s$ess_initial_positive),epsilon=receipt$epsilon)
 }
}
write.csv(do.call(rbind,contrasts),file.path(out,'all_response_contrasts.csv'),row.names=FALSE)
write.csv(do.call(rbind,terms),file.path(out,'all_coefficients.csv'),row.names=FALSE)
write.csv(do.call(rbind,support),file.path(out,'landuse_support.csv'),row.names=FALSE)
write.csv(do.call(rbind,receipts),file.path(out,'model_status.csv'),row.names=FALSE)
write_json(list(status='COMPLETE_RESULT_TABLES_WRITTEN',relative_percent='Ratio of posterior predictive means; interval columns remain score differences',
 denominator='Standardised reference-use prediction or younger-cohort prediction',configuration_sha256=digest(file=a[1],algo='sha256')),
 file.path(out,'receipt.json'),pretty=TRUE,auto_unbox=TRUE)
