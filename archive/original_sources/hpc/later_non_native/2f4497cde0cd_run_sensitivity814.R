# Original source: run_sensitivity814.R
# Original source location: ${NON_NATIVE_ROOT}/audit/environment_only_sensitivity/run_sensitivity814.R
# Original source SHA-256: 2f4497cde0cd9090eb4beb91c1b12c013111aba5fe45f6a849763b15444a5634
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_ENVIRONMENT_ONLY_SENSITIVITY)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/usr/bin/env Rscript
# All generated products are private, versioned; baseline design remains read-only.
.libPaths(c(Sys.getenv('R_LIBS_USER'),.libPaths()))
suppressPackageStartupMessages({library(biomod2);library(terra)})
a='<NON_NATIVE_ROOT>/audit/environment_only_sensitivity'
source(file.path(a,'sensitivity_guards.R'))
args=commandArgs(TRUE);stopifnot(length(args)==2);taxon=args[1];variant=args[2]
x=read.delim(file.path(a,'sensitivity_inventory.tsv'),quote='',check.names=FALSE,stringsAsFactors=FALSE);r=x[x$accepted_taxon==taxon,,drop=FALSE];stopifnot(nrow(r)==1,r$scope=='814_FINAL_ANALYSIS')
pre=read.csv(file.path(a,'static_preflight_814.csv'));stopifnot(nrow(pre)==814,all(pre$static_pass),setequal(pre$accepted_taxon,x$accepted_taxon))
verified=system2('python3',c(file.path(a,'static_contract.py'),'verify',shQuote(taxon)),stdout=TRUE,stderr=TRUE);stopifnot(is.null(attr(verified,'status')))
policies=jsonlite::fromJSON(file.path(a,'frozen_route_policies.json'),simplifyVector=FALSE);policy=policies[[r$authoritative_baseline_source]];stopifnot(!is.null(policy))
root=if(variant=='CLEAN_FULL')r$clean_full_output_root else r$output_root
stopifnot(nzchar(root),startsWith(root,paste0(a,'/production_v2/')),!grepl('\\.(rds|tif)$',root))
if(file.exists(file.path(root,'VALIDATED.rds')))stop('Already validated: reuse; no overwrite')
dir.create(root,recursive=TRUE,showWarnings=FALSE)
lock=file.path(root,'ACTIVE_OWNER');if(!dir.create(lock,showWarnings=FALSE))stop('Duplicate owner: reconcile lock against Slurm before retry')
writeLines(c(taxon,variant,Sys.getenv('SLURM_JOB_ID'),as.character(Sys.time())),file.path(lock,'owner.txt'))

setwd(root);dir.create('tmp',showWarnings=FALSE);terraOptions(tempdir=file.path(root,'tmp'),memfrac=.5,memmax=40,todisk=TRUE,threads=1,progress=0)
stage_times=if(file.exists('stage_times.json'))jsonlite::fromJSON('stage_times.json') else list(e2e_started=as.numeric(Sys.time()))
mark_stage=function(stage){if(is.null(stage_times[[stage]])){stage_times[[stage]]<<-as.numeric(Sys.time());jsonlite::write_json(stage_times,'stage_times.json',auto_unbox=TRUE,pretty=TRUE)}}
o=load_saved(r$baseline_model_path);f=load_saved(r$formatted_input_path);cv=load_saved(r$calibration_path)
opt=if(r$model_options_path=='IN_MODEL_OBJECT')o@models.options@val else load_saved(r$model_options_path)
p=o@expl.var.names;h=strsplit(r$human_exclusion_set,';',fixed=TRUE)[[1]];e=predictor_subset(p,h,variant);excluded=setdiff(p,e)
before=list(taxon=taxon,variant=variant,predictors=e,coord=f@coord,response=f@data.species,PA=if('PA.table'%in%slotNames(f))f@PA.table else NULL,cv=cv,baseline=r$baseline_model_path)
if(file.exists('design_before.rds'))stopifnot(identical(before,readRDS('design_before.rds')))else saveRDS(before,'design_before.rds')
f@data.env.var=f@data.env.var[,e,drop=FALSE]
if(ncol(f@eval.data.env.var))f@eval.data.env.var=f@eval.data.env.var[,e,drop=FALSE]
f@dir.name=root
for(k in seq_along(opt@options))for(n in names(opt@options[[k]]@args.values)){
 v=opt@options[[k]]@args.values[[n]]
 if('formula'%in%names(v)&&inherits(v$formula,'formula'))v$formula=prune_formula(v$formula,excluded)
 opt@options[[k]]@args.values[[n]]=v
}
if(file.exists('options_used.rds'))stopifnot(isTRUE(all.equal(opt,readRDS('options_used.rds'),ignore.environment=TRUE)))else saveRDS(opt,'options_used.rds')
if(!file.exists('formatted_used.rds'))saveRDS(f,'formatted_used.rds')
if(!file.exists('calibration_used.rds'))saveRDS(cv,'calibration_used.rds')
# Test actual BIOMOD CV adapter before fitting, without random generation.
actual_cv=bm_CrossValidation(f,strategy='user.defined',user.table=cv,do.full.models=FALSE)
stopifnot(identical(as.matrix(cv),as.matrix(actual_cv)))
alg=strsplit(r$algorithm_route,';',fixed=TRUE)[[1]]
ev0=if(o@models.evaluation@inMemory)o@models.evaluation@val else load_saved(r$baseline_evaluation_artifact_path)
metrics=unique(ev0$metric.eval);id=paste0(r$task_key,'_',variant)
iv_path=file.path(dirname(r$formatted_input_path),'variables.importance');var_import=0L
if(nzchar(o@variables.importance@link)||o@variables.importance@inMemory){iv=if(o@variables.importance@inMemory)o@variables.importance@val else load_saved(iv_path);if(nrow(iv))var_import=as.integer(max(iv$rand))}
planned=list(source_digest=jsonlite::fromJSON(file.path(a,'static_preflight_contract.json'),simplifyVector=FALSE)$contracts[[taxon]]$digest,policy=policy,algorithms=alg,metrics=metrics,var_import=var_import,scale_models=o@scale.models,predictors=e)
if(file.exists('fit_contract.rds'))stopifnot(identical(planned,readRDS('fit_contract.rds')))else saveRDS(planned,'fit_contract.rds')
if(file.exists(file.path(root,'models.rds'))){m=readRDS(file.path(root,'models.rds'));stopifnot(identical(m@expl.var.names,e));cat('RESUME_SAVED_FIT_WITH_DESIGN_VALIDATION\n')}else{
mark_stage('fit_started')
m=BIOMOD_Modeling(bm.format=f,modeling.id=id,models=alg,OPT.strategy='user.defined',OPT.user=opt,CV.strategy='user.defined',CV.user.table=cv,CV.do.full.models=FALSE,metric.eval=metrics,var.import=var_import,scale.models=o@scale.models,nb.cpu=1,seed.val=42L,do.progress=FALSE)
mark_stage('fit_finished')
}
saveRDS(m,file.path(root,'models.rds'));ev=get_evaluations(m);write.csv(ev,file.path(root,'evaluation.csv'),row.names=FALSE)
post_cv=get_calib_lines(m);post_f=get_formal_data(m)
stopifnot(identical(as.matrix(post_cv),as.matrix(cv)),identical(post_f@coord,f@coord),identical(post_f@data.species,f@data.species),identical(post_f@data.env.var,f@data.env.var),identical(m@expl.var.names,e))
if('PA.table'%in%slotNames(f))stopifnot(identical(post_f@PA.table,f@PA.table))
stopifnot(identical(m@scale.models,o@scale.models),all(sub('.*_','',m@models.computed)%in%alg),options_equal(get_options(m),opt))
saveRDS(list(fit_contract=planned,PA_CV_identical=TRUE,options_verified=TRUE),'FIT_VALIDATED.rds')
summary=do.call(rbind,lapply(unique(ev$full.name),function(n){z=ev[ev$full.name==n,];data.frame(model=n,TSS=max(z$validation[z$metric.eval=='TSS'],na.rm=TRUE),ROC=max(z$validation[z$metric.eval=='ROC'],na.rm=TRUE))}))
valid=summary[is.finite(summary$TSS)&is.finite(summary$ROC),];primary=valid[valid$TSS>.8&valid$ROC>.8,];fallback=valid[valid$TSS>.7&valid$ROC>.7,]
addon=isTRUE(policy$best_TSS_single_fallback)
if(nrow(primary)>=policy$minimum_qualified_models){selected=primary$model;tier='PRIMARY'}else if(nrow(fallback)>=policy$minimum_qualified_models){selected=fallback$model;tier='FALLBACK'}else if(addon&&nrow(valid)){selected=valid$model[which.max(valid$TSS)];tier='FALLBACK_SINGLE'}else stop('SCIENTIFIC_REVIEW_REQUIRED: no model qualifies under saved selection policy')
write.csv(data.frame(model=selected,tier=tier),file.path(root,'selection.csv'),row.names=FALSE)
b='<WORK_ROOT>'
src=r$authoritative_baseline_source
stack_path=if(grepl('V4_MULTIFOLD|DIRECT34',src))file.path(b,'data/predictors/full_stack_terrestrial_primary_v1.tif')else if(grepl('GAP13|VIIRS35|SPARTINA|SOLANUM',src))file.path(b,'data/predictors/full_stack_terrestrial_plus_VIIRS_v1.tif')else '<HPC_INPUT_ROOT>/climate/full_stack.tif'
stack=rast(stack_path)[[e]];stopifnot(identical(names(stack),e),all(abs(res(stack)-1/24)<1e-10))
manual=policy$kind=='MANUAL_EMMEAN'
ens=NULL;chosen_ensemble=NULL
if(!manual&&length(selected)>=2){
  ensemble_contract=list(fit=planned,selected=selected,policy=policy)
 if(file.exists('ensemble.rds')){stopifnot(identical(ensemble_contract,readRDS('ensemble_contract.rds')));ens=readRDS('ensemble.rds')}else{
 saveRDS(ensemble_contract,'ensemble_contract.rds')
 ens=BIOMOD_EnsembleModeling(bm.mod=m,models.chosen=selected,em.by='all',em.algo=c('EMmean','EMmedian'),metric.select='TSS',metric.select.thresh=if(grepl('V4_MULTIFOLD',src))0 else if(tier=='PRIMARY').8 else .7,metric.eval=c('TSS','ROC','KAPPA'),var.import=3)
 }
 saveRDS(ens,file.path(root,'ensemble.rds'));ee=get_evaluations(ens);write.csv(ee,file.path(root,'ensemble_evaluation.csv'),row.names=FALSE)
 if(grepl('V4_MULTIFOLD',src)){chosen_ensemble=get_built_models(ens);chosen_ensemble=chosen_ensemble[grepl('EMmean',chosen_ensemble)]}else{score=rowMeans(ee[,c('sensitivity','specificity','calibration')],na.rm=TRUE);chosen_ensemble=ee$full.name[which.max(score)]}
 stopifnot(length(chosen_ensemble)==1)
}
if(file.exists('projection_object.rds')){proj_name=readRDS('projection_contract.rds')$proj_name}else{
 attempt_file='projection_attempt_index.txt';attempt=if(file.exists(attempt_file))as.integer(readLines(attempt_file))+1L else 1L;writeLines(as.character(attempt),attempt_file);proj_name=paste0('sensitivity814_attempt',attempt)
}
projection_contract=list(fit=planned,selected=selected,stack=stack_path,domain=r$baseline_global_projection_path,proj_name=proj_name)
if(file.exists('projection_object.rds')){stopifnot(identical(projection_contract,readRDS('projection_contract.rds')));proj=readRDS('projection_object.rds')}else{
 mark_stage('projection_started')
 saveRDS(projection_contract,'projection_contract.rds')
 proj=BIOMOD_Projection(bm.mod=m,proj.name=proj_name,new.env=stack,models.chosen=selected,metric.binary='TSS',build.clamping.mask=FALSE,do.stack=FALSE,keep.in.memory=FALSE,nb.cpu=1,overwrite=FALSE)
}
saveRDS(proj,file.path(root,'projection_object.rds'))
mark_stage('projection_finished')
files=list.files(file.path(root,m@sp.name,paste0('proj_',proj_name),'individual_projections'),pattern='\\.tif$',full.names=TRUE)
paths=vapply(selected,function(n){z=files[basename(files)==paste0('proj_',proj_name,'_',n,'.tif')];stopifnot(length(z)==1);z},character(1))
if(!is.null(ens)){
 ef=BIOMOD_EnsembleForecasting(bm.em=ens,bm.proj=proj,models.chosen=chosen_ensemble,metric.binary='TSS');saveRDS(ef,file.path(root,'ensemble_forecast.rds'))
 ff=list.files(file.path(root,m@sp.name,paste0('proj_',proj_name)),pattern='\\.tif$',full.names=TRUE,recursive=TRUE);cc=ff[grepl(chosen_ensemble,basename(ff),fixed=TRUE)&!grepl('TSSbin|TSSfilt',basename(ff))];stopifnot(length(cc)==1);out=rast(cc)
}else{out=if(length(paths)==1)rast(paths)else mean(rast(paths),na.rm=TRUE)}
reference=rast(r$baseline_global_projection_path);stopifnot(nlyr(reference)==1,compareGeom(out,reference,stopOnError=FALSE))
out=mask(out,reference)
writer_contract=list(projection=projection_contract,chosen_ensemble=chosen_ensemble,fixed_domain=r$baseline_global_projection_path)
if(file.exists('ensemble_suitability.tif')){
 stopifnot(file.exists('writer_contract.rds'),identical(writer_contract,readRDS('writer_contract.rds')))
}else{
 saveRDS(writer_contract,'writer_contract.rds')
 writeRaster(out,'ensemble_suitability.staging.tif',overwrite=TRUE,datatype='FLT4S',gdal=c('COMPRESS=LZW','TILED=YES','BIGTIFF=YES'))
 stopifnot(file.rename('ensemble_suitability.staging.tif','ensemble_suitability.tif'))
}
# No success claim is written before independent writer and value checks.
mark_stage('qc_started')
out=rast(file.path(root,'ensemble_suitability.tif'));rg=global(out,c('min','max'),na.rm=TRUE);nna=global(!is.na(out),'sum',na.rm=TRUE)[1,1]
missing_cells=global(ifel(!is.na(reference)&is.na(out),1,0),'sum',na.rm=TRUE)[1,1];extra_cells=global(ifel(is.na(reference)&!is.na(out),1,0),'sum',na.rm=TRUE)[1,1];infinite_cells=global(is.infinite(out),'sum',na.rm=TRUE)[1,1]
stopifnot(missing_cells==0,extra_cells==0,infinite_cells==0)
stopifnot(compareGeom(out,stack[[1]],stopOnError=FALSE),nna>1000,all(is.finite(as.numeric(unlist(rg)))),rg[1,1]>=0,rg[1,2]<=1000.01,rg[1,2]>rg[1,1])
sites=as.data.frame(spatSample(out,10000,method='regular',na.rm=TRUE,xy=TRUE,values=TRUE));sites=sites[is.finite(sites[[names(out)[1]]]),c('x','y'),drop=FALSE];stopifnot(nrow(sites)>0)
newdata=terra::extract(stack,as.matrix(sites[,c('x','y')]));ok=complete.cases(newdata);newdata=newdata[ok,,drop=FALSE];sites=sites[ok,,drop=FALSE];stopifnot(nrow(newdata)>=10)
take=unique(round(seq(1,nrow(newdata),length.out=min(200,nrow(newdata)))));newdata=newdata[take,,drop=FALSE];sites=sites[take,,drop=FALSE]
independent_grid=rast(nrows=1,ncols=nrow(newdata),nlyrs=ncol(newdata),xmin=0,xmax=nrow(newdata),ymin=0,ymax=1,crs='');names(independent_grid)=names(newdata);values(independent_grid)=as.matrix(newdata)
# Use the same raster dispatch as projection. Installed MAXNET dataframe dispatch
# clamps by default, whereas its raster dispatch explicitly uses clamp=FALSE.
independent_predict=function(object){v=predict(object,newdata=independent_grid,on_0_1000=TRUE,seedval=42L,mod.name='independent_QC');as.numeric(values(v,mat=FALSE))}
if(!is.null(ens)){
 # This BIOMOD2 build can pass the EMmean component predictions as a list
 # to its SpatRaster predict2 method. Predict the exact saved ensemble members
 # individually through the same raster dispatch, then reproduce EMmean.
 ensemble_object=BIOMOD_LoadModels(ens,full.name=chosen_ensemble);ensemble_model=get(ensemble_object[1]);ensemble_members=slot(ensemble_model,'model')
 objects=BIOMOD_LoadModels(m,full.name=ensemble_members);pred=sapply(objects,function(n)independent_predict(get(n)));expected=if(is.matrix(pred))rowMeans(round(pred),na.rm=TRUE)else round(pred)
}else{
 objects=BIOMOD_LoadModels(m,full.name=selected);pred=sapply(objects,function(n)independent_predict(get(n)));expected=if(is.matrix(pred))rowMeans(round(pred),na.rm=TRUE)else round(pred)
}
observed=terra::extract(out,as.matrix(sites[,c('x','y')]))[,1];stopifnot(length(expected)==nrow(newdata),all(is.finite(expected)),all(is.finite(observed)));delta=max(abs(expected-observed));stopifnot(is.finite(delta),delta<=1.5)
write.csv(data.frame(x=sites$x,y=sites$y,expected=expected,observed=observed,absolute_error=abs(expected-observed)),file.path(root,'independent_value_check.csv'),row.names=FALSE)
qc=list(taxon=taxon,variant=variant,source=r$baseline_model_path,stack=stack_path,predictors=e,PA_CV_identical=TRUE,selected_models=selected,ensemble=chosen_ensemble,range=rg,scale='BIOMOD_0_1000',nonNA=nna,max_independent_delta=delta,job_id=Sys.getenv('SLURM_JOB_ID'),verified_at=as.character(Sys.time()))
saveRDS(qc,file.path(root,'VALIDATED.rds'));writeLines('TECHNICALLY_VALIDATED',file.path(root,'STATUS.txt'))
mark_stage('complete')
unlink(lock,recursive=TRUE);cat('SENSITIVITY_ROUTE_PASS',taxon,variant,'\n')
