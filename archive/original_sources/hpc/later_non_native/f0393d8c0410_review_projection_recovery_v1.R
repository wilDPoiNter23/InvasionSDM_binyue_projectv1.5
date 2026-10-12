# Original source: review_projection_recovery_v1.R
# Original source location: ${NON_NATIVE_ROOT}/audit/environment_only_sensitivity/review_triage/review_projection_recovery_v1.R
# Original source SHA-256: f0393d8c0410d5c52428a256acde38e1e00ef8d43bfd53fbcae1a8910c08d129
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_ENVIRONMENT_ONLY_SENSITIVITY)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# Controller-reserved downstream-only recovery. Never fits, evaluates, selects, or reselects.
suppressPackageStartupMessages({library(biomod2);library(terra);library(jsonlite)})
atomic_rds<-function(x,p){t<-paste0(p,'.staging');saveRDS(x,t);if(!file.rename(t,p))stop('atomic write failed: ',p)}
sha256<-function(p){x<-system2('sha256sum',shQuote(p),stdout=TRUE);strsplit(x,' +')[[1]][1]}
rollback<-function(root,backup,files){for(n in files){src<-file.path(backup,n);dst<-file.path(root,n);if(file.exists(src))file.copy(src,dst,overwrite=TRUE)}}
run_one<-function(mp){
 z<-fromJSON(mp,simplifyVector=FALSE);root<-normalizePath(z$root,mustWork=TRUE);setwd(root)
 if(as.character(packageVersion('biomod2'))!='4.2.6.1')stop('BIOMOD2_VERSION_MISMATCH')
 if(dir.exists('ACTIVE_OWNER')||!dir.exists('REVIEW_RECOVERY_OWNER'))stop('RECOVERY_OWNER_GUARD')
 if(file.exists('VALIDATED.rds'))stop('ALREADY_VALIDATED')
 req<-c('FIT_VALIDATED.rds','fit_contract.rds','models.rds','evaluation.csv','selection.csv','projection_contract.rds')
 miss<-req[!file.exists(req)];if(length(miss))stop('RECOVERY_INPUT_MISSING:',paste(miss,collapse=','))
 protected<-c('FIT_VALIDATED.rds','fit_contract.rds','models.rds','evaluation.csv','selection.csv')
 before<-vapply(protected,sha256,character(1));m<-readRDS('models.rds');fc<-readRDS('fit_contract.rds');fit<-readRDS('FIT_VALIDATED.rds')
 if(!identical(m@expl.var.names,fc$predictors))stop('FROZEN_PREDICTOR_ORDER_MISMATCH')
 selected<-as.character(read.csv('selection.csv',stringsAsFactors=FALSE)$model);if(!length(selected)||!all(selected%in%m@models.computed))stop('SAVED_SELECTION_INVALID')
 oldpc<-readRDS('projection_contract.rds');if(!identical(oldpc$selected,selected)||!identical(oldpc$fit,fc))stop('SAVED_PROJECTION_CONTRACT_MISMATCH')
 ens<-if(file.exists('ensemble.rds'))readRDS('ensemble.rds') else NULL
 if(!is.null(ens)&&!file.exists('ensemble_contract.rds'))stop('ENSEMBLE_CONTRACT_MISSING')
 chosen<-z$chosen_ensemble
 if(file.exists('writer_contract.rds'))chosen<-readRDS('writer_contract.rds')$chosen_ensemble
 else if(!is.null(ens)&&file.exists('ensemble_evaluation.csv')){
   ee<-read.csv('ensemble_evaluation.csv',stringsAsFactors=FALSE)
   if(grepl('V4_MULTIFOLD',z$baseline_source_route)){bm<-get_built_models(ens);chosen<-bm[grepl('EMmean',bm)]}
   else {score<-rowMeans(ee[,c('sensitivity','specificity','calibration')],na.rm=TRUE);chosen<-ee$full.name[which.max(score)]}
 }
 if(!is.null(ens)&&(length(chosen)!=1L||is.na(chosen)||!nzchar(chosen)))stop('CHOSEN_ENSEMBLE_NOT_FROZEN')
 if(is.null(ens)&&length(selected)>1L)stop('SAVED_ENSEMBLE_MISSING_FOR_MULTI_MODEL_SELECTION')
 stack<-rast(oldpc$stack)[[m@expl.var.names]];ref<-rast(oldpc$domain)
 stopifnot(identical(names(stack),m@expl.var.names),nlyr(ref)==1L,compareGeom(stack[[1]],ref,stopOnError=FALSE))
 terraOptions(tempdir=file.path(root,'tmp'),memfrac=.5,memmax=40,todisk=TRUE,threads=1,progress=0)
 tag<-gsub('[^A-Za-z0-9]','',z$key);projname<-paste0('sensitivity814_review_v1_',substr(tag,max(1,nchar(tag)-15),nchar(tag)))
 pc<-list(fit=fc,selected=selected,stack=oldpc$stack,domain=oldpc$domain,proj_name=projname)
 proj<-BIOMOD_Projection(bm.mod=m,proj.name=projname,new.env=stack,models.chosen=selected,metric.binary='TSS',build.clamping.mask=FALSE,do.stack=FALSE,keep.in.memory=FALSE,nb.cpu=1,overwrite=FALSE)
 idir<-file.path(root,m@sp.name,paste0('proj_',projname),'individual_projections');ifs<-list.files(idir,pattern='\\.tif$',full.names=TRUE)
 ip<-vapply(selected,function(n){q<-ifs[basename(ifs)==paste0('proj_',projname,'_',n,'.tif')];if(length(q)!=1L)stop('SELECTED_PROJECTION_MISSING:',n);q},character(1))
 ensforecast<-NULL
 if(!is.null(ens)){ensforecast<-BIOMOD_EnsembleForecasting(bm.em=ens,bm.proj=proj,models.chosen=chosen,metric.binary='TSS');ff<-list.files(file.path(root,m@sp.name,paste0('proj_',projname)),pattern='\\.tif$',full.names=TRUE,recursive=TRUE);cc<-ff[grepl(chosen,basename(ff),fixed=TRUE)&!grepl('TSSbin|TSSfilt',basename(ff))];if(length(cc)!=1L)stop('SELECTED_ENSEMBLE_FORECAST_MISSING');out<-rast(cc)}else out<-if(length(ip)==1L)rast(ip) else mean(rast(ip),na.rm=TRUE)
 if(!compareGeom(out,stack[[1]],stopOnError=FALSE)||!compareGeom(out,ref,stopOnError=FALSE))stop('OUTPUT_GEOMETRY_MISMATCH')
 out<-mask(out,ref);recdir<-file.path(root,'review_projection_recovery_v1');dir.create(recdir,showWarnings=FALSE)
 staged<-file.path(recdir,'ensemble_suitability.staging.tif');writeRaster(out,staged,overwrite=TRUE,datatype='FLT4S',gdal=c('COMPRESS=LZW','TILED=YES','BIGTIFF=YES'))
 backup<-file.path(recdir,paste0('before_job_',Sys.getenv('SLURM_JOB_ID')));dir.create(backup,showWarnings=FALSE)
 publish<-c('ensemble_suitability.tif','projection_contract.rds','projection_object.rds','writer_contract.rds','ensemble_forecast.rds')
 for(n in publish)if(file.exists(n))file.copy(n,file.path(backup,n),overwrite=FALSE)
 target<-file.path(root,'ensemble_suitability.tif')
 if(file.exists(target)){old<-paste0(target,'.pre_projection_recovery_v1_job_',Sys.getenv('SLURM_JOB_ID'));if(file.exists(old))stop('BACKUP_EXISTS');if(!file.rename(target,old))stop('CANNOT_ARCHIVE_OLD_RASTER')}
 if(!file.rename(staged,target)){rollback(root,backup,publish);stop('CANNOT_PROMOTE_RECOVERED_RASTER')}
 atomic_rds(pc,'projection_contract.rds');atomic_rds(proj,'projection_object.rds')
 wc<-list(projection=pc,chosen_ensemble=chosen,fixed_domain=oldpc$domain);atomic_rds(wc,'writer_contract.rds')
 if(!is.null(ensforecast))atomic_rds(ensforecast,'ensemble_forecast.rds')
 qcs<-if(is.null(ens))file.path(z$audit_root,'review_triage/qc_only_recover_singlemodel_v1.R') else if(grepl('EMmedian',chosen,fixed=TRUE))file.path(z$audit_root,'review_triage/qc_only_recover_emmedian_v2.R') else if(grepl('EMmean',chosen,fixed=TRUE))file.path(z$audit_root,'review_triage/qc_only_recover_emmean_v1.R') else stop('UNSUPPORTED_ENSEMBLE_QC_METHOD')
 log<-system2('<HPC_HOME>/R-4.4.1/bin/Rscript',c('--vanilla',qcs,root,z$species,'ENV'),stdout=TRUE,stderr=TRUE)
 writeLines(log,file.path(recdir,'qc_child.log'));status<-attr(log,'status');if(is.null(status))status<-0L
 if(status!=0L||!file.exists('VALIDATED.rds')){rollback(root,backup,publish);if(file.exists(target)&&file.exists(paste0(target,'.pre_projection_recovery_v1_job_',Sys.getenv('SLURM_JOB_ID'))))file.rename(target,staged);stop('INDEPENDENT_QC_FAILED; previous projection restored')}
 after<-vapply(protected,sha256,character(1));if(!identical(before,after)){rollback(root,backup,publish);stop('PROTECTED_FIT_EVALUATION_SELECTION_CHANGED')}
 receipt<-list(key=z$key,job=Sys.getenv('SLURM_JOB_ID'),projection_name=projname,chosen_ensemble=chosen,no_refit=TRUE,projection_recomputed=TRUE,fit_evaluation_selection_hashes_before=before,fit_evaluation_selection_hashes_after=after,qc_child_status=status,qc_script_sha256=sha256(qcs),BIOMOD2_version='4.2.6.1',verified_utc=as.character(Sys.time()))
 write_json(receipt,'PROJECTION_RECOVERY_PASS_V1.json',auto_unbox=TRUE,pretty=TRUE)
 cat('PROJECTION_RECOVERY_PASS key=',z$key,' no_refit=TRUE no_reprojection=FALSE\n',sep='')
}
a<-commandArgs(TRUE);if(length(a)!=1L)stop('Usage: review_projection_recovery_v1.R manifest.json')
tryCatch(run_one(a[1]),error=function(e){cat('Error: ',conditionMessage(e),'\n',file=stderr(),sep='');quit(status=1)})
