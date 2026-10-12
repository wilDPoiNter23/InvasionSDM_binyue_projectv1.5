# Original source: static_preflight_814.R
# Original source location: ${NON_NATIVE_ROOT}/audit/environment_only_sensitivity/static_preflight_814.R
# Original source SHA-256: 026ae866757909649a6600db33bdae400bd481449a78102025261d88193f51fd
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_ENVIRONMENT_ONLY_SENSITIVITY)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
.libPaths(c('<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu',.libPaths()))
suppressPackageStartupMessages({library(biomod2);library(terra)})
a='<NON_NATIVE_ROOT>/audit/environment_only_sensitivity'
x=read.delim(file.path(a,'sensitivity_inventory.tsv'),quote='',check.names=FALSE,stringsAsFactors=FALSE)
load_obj=function(p){if(grepl('rds$',p))return(readRDS(p));e=new.env();nn=load(p,e);get(nn[1],e)}
H=c('reclamation_raster','VIIRS_2023_2.5min','GDP2020_2.5min','BUILT_S_E2025_2.5min','Pop2020_2.5min')
stack=rast('<HPC_INPUT_ROOT>/climate/full_stack.tif');plus=rast('<WORK_ROOT>/data/predictors/full_stack_terrestrial_plus_VIIRS_v1.tif');available=union(names(stack),names(plus));ans=list()
for(i in seq_len(nrow(x))){r=x[i,];err=character();details='';tryCatch({
o=load_obj(r$baseline_model_path);f=load_obj(r$formatted_input_path);cv=load_obj(r$calibration_path);op=if(r$model_options_path=='IN_MODEL_OBJECT')o@models.options@val else load_obj(r$model_options_path)
p=strsplit(r$baseline_predictor_vector,';',fixed=TRUE)[[1]];e=strsplit(r$environmental_predictor_vector,';',fixed=TRUE)[[1]]
stopifnot(identical(o@expl.var.names,p),all(p %in% names(f@data.env.var)),identical(e,p[!p %in% c(H,'ID')]),all(e %in% available),nrow(cv)==length(f@data.species),!is.null(op),all(colnames(cv) %in% unique(unlist(lapply(op@options,function(z)names(z@args.values))))))
if('ID'%in%p)stopifnot(identical(strsplit(r$clean_full_predictor_vector,';',fixed=TRUE)[[1]],p[p!='ID']))
stopifnot(file.exists(r$baseline_global_projection_path),file.exists(r$baseline_analysis_projection_path),!file.exists(r$output_model_path))
pa=if('PA.table'%in%slotNames(f))paste(colSums(f@PA.table),collapse=';')else 'OBSERVED_ABSENCES'
argsnames=unique(unlist(lapply(op@options,function(z)unlist(lapply(z@args.values,names)))))
details=paste('nrows',length(f@data.species),'pa',pa,'cv',ncol(cv),'options',paste(argsnames,collapse=';'))
},error=function(z){err=conditionMessage(z)})
ans[[i]]=data.frame(accepted_taxon=r$accepted_taxon,static_pass=!length(err),error=paste(err,collapse=';'),details=details)
if(i%%50==0){write.csv(do.call(rbind,ans),file.path(a,'static_preflight_814.csv'),row.names=FALSE);cat(i,'\n');gc(FALSE)}
}
write.csv(do.call(rbind,ans),file.path(a,'static_preflight_814.csv'),row.names=FALSE)
print(table(do.call(rbind,ans)$static_pass));print(subset(do.call(rbind,ans),!static_pass)[,1:3])
