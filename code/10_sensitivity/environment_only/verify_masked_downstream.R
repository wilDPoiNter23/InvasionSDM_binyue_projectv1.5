# Reconstruct native-excluded zone responses, posterior tables and binary inputs.
suppressPackageStartupMessages({library(jsonlite);library(Matrix)})
cfg<-fromJSON(commandArgs(TRUE)[1]);v<-readRDS(cfg$scaled_values)
m<-readRDS(cfg$native_membership);a<-readRDS(cfg$city_support)
stopifnot(identical(a$cells,m$cells));errors<-numeric();checks<-0L
check<-function(x,y){stopifnot(identical(unname(is.finite(x)),unname(is.finite(y))));ok<-is.finite(x);err<-if(any(ok))max(abs(x[ok]-y[ok]))else 0;stopifnot(err<1e-10);errors<<-c(errors,err);checks<<-checks+length(x)}
pm<-match(v$paired_species,m$species)
if(!is.null(cfg$coastal_roots))for(root in cfg$coastal_roots){
 s<-readRDS(file.path(root,'native_support.rds'));w<-s$weights
 W<-sparseMatrix(i=w$zone_id,j=match(w$cell,a$cells),x=w$weight_km2,dims=c(nrow(s$zones),length(a$cells)))
 model_directory<-if(is.null(cfg$model_directory))'models_multilevel'else cfg$model_directory
 groups<-list.files(file.path(root,model_directory))
 if(!is.null(cfg$model_groups))groups<-intersect(groups,cfg$model_groups)
 for(g in groups){
  mr<-file.path(root,model_directory,g);stopifnot(file.exists(file.path(mr,'completion.json')))
  completion<-fromJSON(file.path(mr,'completion.json'))
  x<-switch(g,ENV=v$paired_env,FULL=v$paired_full,FULL_814=v$full)
  sp<-if(g=='FULL_814')m$species else v$paired_species
  mask<-if(g=='FULL_814')m$native else m$native[,pm,drop=FALSE]
  good<-is.finite(x)&!mask;x[!good]<-0
  numerator<-as.matrix(W%*%x);denominator<-as.matrix(W%*%good)
  expected<-numerator/denominator;d<-readRDS(file.path(mr,'model_input.rds'))
  ind<-cbind(match(d$zone_id,s$zones$zone_id),match(as.character(d$species),sp));stopifnot(!anyNA(ind))
  check(expected[ind],d$mean_suitability);check(denominator[ind],d$finite_area_km2)
  for(case in names(completion$cases)){
   cr<-file.path(mr,case);fits<-readRDS(file.path(cr,'posterior_chains.rds'))
   samples<-if(is.null(cfg$expected_samples))4500L else cfg$expected_samples
   stopifnot(length(fits)==4L,all(vapply(fits,function(f)nrow(f$beta)==samples,logical(1))))
   B<-do.call(rbind,lapply(fits,`[[`,'beta'));q<-read.csv(file.path(cr,'posterior_summary.csv'))
   for(term in colnames(B)){
    row<-q[q$term==term,];stopifnot(nrow(row)==1L)
    check(c(mean(B[,term]),quantile(B[,term],c(.025,.975))),c(row$mean,row$lower,row$upper))
   }
   receipt<-fromJSON(file.path(cr,'receipt.json'))
   actual<-if(all(q$rank_split_rhat<1.01)&all(q$ess_initial_positive>=400))'PASS_CHAIN_DIAGNOSTICS'else'REQUIRES_CHAIN_REVIEW'
   stopifnot(receipt$status==actual)
   p<-read.csv(file.path(cr,'age_use_predictions.csv'));ct<-read.csv(file.path(cr,'response_scale_contrasts.csv'))
   for(i in seq_len(nrow(ct))){
    r<-ct[i,]
    if(r$type=='TEN_YEAR_COHORT_DIFFERENCE'){
     left<-p$mean[p$landuse==receipt$reference_use&p$age_index_years==as.numeric(r$from)]
     right<-p$mean[p$landuse==receipt$reference_use&p$age_index_years==as.numeric(r$to)]
    }else{
     left<-p$mean[p$landuse==r$from&p$age_index_years==r$age_index_years]
     right<-p$mean[p$landuse==r$to&p$age_index_years==r$age_index_years]
    }
    stopifnot(length(left)==1L,length(right)==1L);check(right-left,r$difference)
   }
  }
 }
}
if(!is.null(cfg$binary_root)){
 root<-cfg$binary_root;tt<-read.csv(cfg$paired_thresholds);main<-read.csv(cfg$main_thresholds)
 d<-rbind(read.csv(file.path(root,'paired_city_responses.csv')),read.csv(file.path(root,'full814_city_responses.csv')))
 saved<-readRDS(file.path(root,'binary_cell_means.rds'));stopifnot(identical(saved$cells,a$cells))
 w<-a$weights;w<-w[w$footprint=='HISTORICAL'&!tolower(w$city_id)%in%tolower(cfg$exclude_city_keys),]
 cc<-w;cc$region<-'CITY';rr<-w[w$zone=='RECLAIMED',];rr$region<-'MAPPED_RECLAMATION';w<-rbind(cc,rr)
 for(g in unique(d$source_group)){
  x<-switch(g,ENV=v$paired_env,FULL=v$paired_full,FULL_814=v$full);sp<-if(g=='FULL_814')m$species else v$paired_species
  mask<-if(g=='FULL_814')m$native else m$native[,pm,drop=FALSE];t<-if(g=='FULL_814')main else tt[tt$group==g,];t<-t[match(sp,t$species),]
  good<-is.finite(x)&!mask
  for(method in c('t10','max_tss')){
   b<-x>=matrix(t[[paste0(method,'_01')]],nrow(x),ncol(x),byrow=TRUE)
   b[!good]<-NA;score<-apply(b,1,function(z)if(any(is.finite(z)))mean(z[is.finite(z)])else NA_real_)
   check(score,saved$products[[paste(g,method,sep='_')]]$mean)
   z<-d[d$source_group==g&d$threshold==(if(method=='t10')'T10'else'MAX_TSS'),]
   for(i in seq_len(nrow(z))){
    ww<-w[w$city_id==z$city_id[i]&w$region==z$region[i],];y<-score[ww$cell_index];ok<-is.finite(y)
    area<-sum(ww$weight_km2[ok]);value<-if(area>0)sum(y[ok]*ww$weight_km2[ok])else NA_real_
    check(c(area,value,if(area>0)value/area else NA_real_),c(z$finite_area_km2[i],z$mean_species_suitable_area_km2[i],z$mean_species_suitable_fraction[i]))
   }
  }
 }
}
write_json(list(status='PASS_NUMERICAL_RECONSTRUCTION',numeric_values_checked=checks,maximum_error=max(errors),scope='Saved raw scaled predictions and masks independently reconstruct zone/model inputs and binary cell and regional means; saved posterior coefficient intervals and completion diagnostics checked'),cfg$output,auto_unbox=TRUE,pretty=TRUE)
