# Original source: qc_only_recover_singlemodel_v1.R
# Original source location: ${NON_NATIVE_ROOT}/audit/environment_only_sensitivity/review_triage/qc_only_recover_singlemodel_v1.R
# Original source SHA-256: 2beab35907ccad85538be2b6da407184800c59877f00c219604a4bd72536345b
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_ENVIRONMENT_ONLY_SENSITIVITY)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# QC-only validation for a saved single selected model and its existing projection raster.
# Never refits, reselects, changes predictors or rewrites a projection.
suppressPackageStartupMessages({library(biomod2); library(terra); library(jsonlite)})
atomic_write_rds <- function(x,path) { tmp<-paste0(path,'.staging'); saveRDS(x,tmp); if(!file.rename(tmp,path)) stop('atomic RDS publish failed') }
atomic_write_json <- function(x,path) { tmp<-paste0(path,'.staging'); write_json(x,tmp,auto_unbox=TRUE,pretty=TRUE,na='null'); if(!file.rename(tmp,path)) stop('atomic JSON publish failed') }
run_single_qc <- function(root,taxon,variant='ENV',job_id=Sys.getenv('SLURM_JOB_ID')) {
 root<-normalizePath(root,mustWork=TRUE)
 if(dir.exists(file.path(root,'ACTIVE_OWNER'))) stop('Active production owner present')
 if(!dir.exists(file.path(root,'REVIEW_RECOVERY_OWNER'))) stop('Controller recovery reservation missing')
 if(file.exists(file.path(root,'VALIDATED.rds'))) stop('Already validated; do not rerun')
 required<-c('FIT_VALIDATED.rds','models.rds','evaluation.csv','selection.csv','projection_object.rds','projection_contract.rds','ensemble_suitability.tif','writer_contract.rds')
 missing<-required[!file.exists(file.path(root,required))]
 if(length(missing)) stop('Missing protected single-model QC artifacts: ',paste(missing,collapse=', '))
 if(as.character(packageVersion('biomod2'))!='4.2.6.1') stop('Unexpected BIOMOD2 version')
 setwd(root); started<-as.numeric(Sys.time())
 fit<-readRDS('FIT_VALIDATED.rds'); model_out<-readRDS('models.rds')
 projection_contract<-readRDS('projection_contract.rds'); writer<-readRDS('writer_contract.rds')
 selected<-unique(read.csv('selection.csv',stringsAsFactors=FALSE)$model)
 saved_selected<-projection_contract$selected; writer_selected<-writer$projection$selected
 if(length(selected)!=1L || !identical(as.character(selected),as.character(saved_selected)) || !identical(as.character(selected),as.character(writer_selected))) stop('Saved selection/projection/writer model identity mismatch')
 predictors<-slot(model_out,'expl.var.names')
 if(!length(predictors) || anyDuplicated(predictors)) stop('Invalid frozen predictor order in model object')
 stack<-rast(projection_contract$stack)[[predictors]]
 out<-rast('ensemble_suitability.tif'); reference<-rast(writer$fixed_domain)
 if(nlyr(out)!=1L) stop('Expected exactly one saved selected-model projection layer')
 if(!identical(names(stack),predictors) || !isTRUE(compareGeom(out,stack[[1L]],stopOnError=FALSE)) || !isTRUE(compareGeom(out,reference,stopOnError=FALSE))) stop('Raster geometry or predictor order mismatch')
 range<-global(out,c('min','max'),na.rm=TRUE); non_na<-global(!is.na(out),'sum',na.rm=TRUE)[1L,1L]
 if(!is.finite(range[1L,1L]) || !is.finite(range[1L,2L]) || range[1L,1L]<0 || range[1L,2L]>1000.01 || range[1L,2L]<=range[1L,1L] || non_na<=1000) stop('Saved production raster failed finite/range guard')
 missing_cells<-global(ifel(!is.na(reference)&is.na(out),1,0),'sum',na.rm=TRUE)[1L,1L]
 extra_cells<-global(ifel(is.na(reference)&!is.na(out),1,0),'sum',na.rm=TRUE)[1L,1L]
 infinite_cells<-global(is.infinite(out),'sum',na.rm=TRUE)[1L,1L]
 if(missing_cells!=0 || extra_cells!=0 || infinite_cells!=0) stop('Saved raster NA/domain support guard failed; retain as projection failure')
 loaded<-BIOMOD_LoadModels(model_out,full.name=selected)
 if(length(loaded)!=1L) stop('Expected one frozen selected model')
 model<-get(loaded[[1L]])
 set.seed(42L); sample_n<-min(10000L,as.integer(non_na))
 sites<-as.data.frame(spatSample(out,sample_n,method='random',na.rm=TRUE,xy=TRUE,values=FALSE))
 if(!all(c('x','y')%in%names(sites))) stop('Could not sample valid projection cells')
 newdata<-as.data.frame(terra::extract(stack,as.matrix(sites[,c('x','y')]))); if('ID'%in%names(newdata)) newdata$ID<-NULL
 newdata<-newdata[,predictors,drop=FALSE]; ok<-complete.cases(newdata); newdata<-newdata[ok,,drop=FALSE]; sites<-sites[ok,,drop=FALSE]
 if(nrow(newdata)<10L) stop(paste0('INSUFFICIENT_COMPLETE_PREDICTOR_SAMPLE:n=',nrow(newdata)))
 take<-unique(round(seq(1,nrow(newdata),length.out=min(200L,nrow(newdata)))))
 newdata<-newdata[take,,drop=FALSE]; sites<-sites[take,,drop=FALSE]
 independent_grid<-rast(nrows=1,ncols=nrow(newdata),nlyrs=ncol(newdata),xmin=0,xmax=nrow(newdata),ymin=0,ymax=1,crs='')
 names(independent_grid)<-predictors; values(independent_grid)<-as.matrix(newdata)
 predicted<-predict(model,newdata=independent_grid,on_0_1000=TRUE,seedval=42L,mod.name='independent_single_model_QC_v1')
 expected<-as.numeric(values(predicted,mat=FALSE)); observed<-terra::extract(out,as.matrix(sites[,c('x','y')]))[,1L]
 if(length(expected)!=nrow(newdata) || any(!is.finite(expected)) || any(!is.finite(observed))) stop('Non-finite or mis-sized independent predictions')
 delta<-max(abs(expected-observed)); if(!is.finite(delta) || delta>1.5) stop(paste0('QC_DELTA_OVER_1_5:',delta))
 check<-data.frame(x=sites$x,y=sites$y,expected=expected,observed=observed,absolute_error=abs(expected-observed))
 tmpcsv<-'independent_value_check_single_v1.staging.csv'; write.csv(check,tmpcsv,row.names=FALSE)
 if(!file.rename(tmpcsv,'independent_value_check_single_v1.csv')) stop('Could not atomically publish sample table')
 completed<-as.numeric(Sys.time()); stage<-fromJSON('stage_times.json',simplifyVector=FALSE)
 stage$qc_only_single_model_v1_started<-started; stage$qc_only_single_model_v1_completed<-completed
 atomic_write_json(stage,'stage_times.json')
 writeLines('TECHNICALLY_VALIDATED_QC_ONLY_SINGLE_MODEL_V1','STATUS.single_qc.staging.txt')
 if(!file.rename('STATUS.single_qc.staging.txt','STATUS.txt')) stop('Could not publish QC status')
 qc<-list(taxon=taxon,variant=variant,selected_model=selected,selection_receipt='selection.csv exact single selected model',predictors=predictors,PA_CV_identical=TRUE,BIOMOD2_version='4.2.6.1',projection_raster='ensemble_suitability.tif',scale='BIOMOD_0_1000',rounding='BIOMOD2 saved prediction semantics; no raster rewriting',NA_support='exact domain support; missing=0 extra=0',range=range,nonNA=non_na,max_independent_delta=delta,sample_count=nrow(newdata),independent_value_check_path='independent_value_check_single_v1.csv',qc_version='qc_only_single_model_v1',no_refit=TRUE,no_reprojection=TRUE,job_id=job_id,verified_at=as.character(Sys.time()))
 atomic_write_rds(qc,'VALIDATED.rds'); invisible(qc)
}
args<-commandArgs(trailingOnly=TRUE)
if(length(args)) { if(length(args)<2L) stop('Usage: qc_only_recover_singlemodel_v1.R <root> <taxon> [variant]'); v<-if(length(args)>=3L)args[[3L]] else 'ENV'; result<-tryCatch(run_single_qc(args[[1L]],args[[2L]],v),error=function(e){cat('Error: ',conditionMessage(e),'\n',file=stderr(),sep='');quit(status=1)}); cat('QC_ONLY_SINGLE_MODEL_PASS',args[[2L]],v,'delta=',result$max_independent_delta,'n=',result$sample_count,'\n') }