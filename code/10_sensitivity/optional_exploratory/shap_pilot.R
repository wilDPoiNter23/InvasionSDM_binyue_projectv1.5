# Bounded attribution of existing continuous products on frozen current city cells.
Sys.setenv(OMP_NUM_THREADS="1",OPENBLAS_NUM_THREADS="1",MKL_NUM_THREADS="1",GDAL_CACHEMAX="64")
suppressPackageStartupMessages({library(biomod2);library(terra);library(jsonlite);library(digest)})
args<-commandArgs(TRUE);if(length(args)!=1L)stop("Supply a private pilot configuration")
cfg<-fromJSON(args[1],simplifyVector=TRUE)
dir.create(cfg$tmp_directory,recursive=TRUE,showWarnings=FALSE)
Sys.setenv(TMPDIR=cfg$tmp_directory,TMP=cfg$tmp_directory,TEMP=cfg$tmp_directory)
terraOptions(tempdir=cfg$tmp_directory,memfrac=.1,progress=0)
source(file.path(cfg$module_directory,"shap_saved_predict.R"));source(file.path(cfg$module_directory,"shap_permutation.R"))
shap_synthetic_check()
if(dir.exists(cfg$output_directory))stop("Fresh output directory required")
dir.create(cfg$output_directory,recursive=TRUE)
manifest<-read.csv(cfg$manifest,stringsAsFactors=FALSE)
support<-readRDS(cfg$support_path);city_ids<-which(support$regions$region=="CITY")
cells<-sort(unique(support$weights$cell[support$weights$ID %in% city_ids]))
stack<-rast(cfg$predictor_stack);H<-c("reclamation_raster","VIIRS_2023_2.5min","GDP2020_2.5min","BUILT_S_E2025_2.5min","Pop2020_2.5min")
status<-list();all_groups<-list()
for(i in seq_along(cfg$species)) {
  species<-cfg$species[i];out<-file.path(cfg$output_directory,gsub("[^A-Za-z0-9]","_",species));dir.create(out)
  setwd(out)
  result<-tryCatch({
    row<-manifest[manifest$species==species & manifest$group=="FULL",,drop=FALSE];if(nrow(row)!=1L)stop("Exact MAIN product unresolved")
    model<-shap_load_predictor(row,cfg$model_directory);if(!all(model$variables %in% names(stack)))stop("Current stack lacks actual declared predictors")
    r<-rast(row$projection_path);if(!compareGeom(r,stack,stopOnError=FALSE))stop("Predictor/product grid differs")
    input<-extract(stack[[model$variables]],cells);observed<-extract(r,cells)[[1]]
    good<-complete.cases(input)&is.finite(observed)
    eligible<-which(good);if(length(eligible)<cfg$n_explain+cfg$n_background)stop("Insufficient finite city cells")
    set.seed(cfg$seed+i);chosen<-sample(eligible,cfg$n_explain+cfg$n_background,replace=FALSE)
    x<-input[head(chosen,cfg$n_explain),,drop=FALSE];background<-input[tail(chosen,cfg$n_background),,drop=FALSE]
    freeze<-list(species=species,scope="CITY_CURRENT_GRID_PILOT",seed=cfg$seed+i,
      cell_ids=cells[chosen],x=x,background=background,selection="Uniform without replacement from finite current CITY cells; no outcome direction selection",
      model_path=row$model_path,projection_path=row$projection_path,selected_members=model$selected,chosen_product=model$chosen,
      variables=model$variables,model_sha256=digest(file=row$model_path,algo="sha256"),
      ensemble_path=model$ensemble_path,ensemble_sha256=if(!is.na(model$ensemble_path))digest(file=model$ensemble_path,algo="sha256")else NA_character_,
      member_paths=model$member_paths,member_file_sha256=vapply(model$member_paths,function(p)if(!is.na(p))digest(file=p,algo="sha256")else NA_character_,character(1)),
      loaded_object_sha256=model$loaded_object_sha256,
      source_code_sha256=vapply(file.path(cfg$module_directory,c("shap_saved_predict.R","shap_permutation.R","shap_pilot.R")),function(p)digest(file=p,algo="sha256"),character(1)),
      manifest_sha256=digest(file=cfg$manifest,algo="sha256"),support_sha256=digest(file=cfg$support_path,algo="sha256"))
    saveRDS(freeze,file.path(out,"frozen_design.rds"))
    qa_predict<-model$predict(input[chosen,,drop=FALSE]);qa_error<-abs(qa_predict-observed[chosen]/1000)
    write.csv(data.frame(cell=cells[chosen],expected=qa_predict,observed=observed[chosen]/1000,absolute_error=qa_error),file.path(out,"model_raster_check.csv"),row.names=FALSE)
    if(max(qa_error)>cfg$model_raster_tolerance)stop("Exact saved product reconstruction failed raster tolerance")
    z<-shap_permutation(model$predict,x,background,cfg$pairs,cfg$seed+i)
    if(max(abs(z$additivity_residual))>1e-8)stop("SHAP additivity failed")
    saveRDS(z,file.path(out,"shap_values.rds"))
    long<-data.frame(point=rep(seq_len(nrow(x)),each=ncol(x)),variable=rep(names(x),times=nrow(x)),
      value=as.vector(t(as.matrix(x))),shap=as.vector(t(z$phi)),mc_standard_error=as.vector(t(z$se)))
    long$predictor_group<-ifelse(long$variable %in% H,"HUMAN","NATURAL")
    write.csv(long,file.path(out,"signed_variable_attributions.csv"),row.names=FALSE)
    summary<-do.call(rbind,lapply(names(x),function(v){ix<-long$variable==v;data.frame(variable=v,predictor_group=if(v %in% H)"HUMAN"else"NATURAL",
      mean_signed_shap=mean(long$shap[ix]),mean_absolute_shap=mean(abs(long$shap[ix])),
      mean_mc_standard_error=mean(long$mc_standard_error[ix]),positive_points=sum(long$shap[ix]>0),negative_points=sum(long$shap[ix]<0))}))
    write.csv(summary,file.path(out,"variable_summary.csv"),row.names=FALSE)
    group<-do.call(rbind,lapply(c("HUMAN","NATURAL"),function(g){v<-which(if(g=="HUMAN")names(x)%in%H else !names(x)%in%H);effect<-rowSums(z$phi[,v,drop=FALSE]);data.frame(species=species,predictor_group=g,mean_signed_shap=mean(effect),mean_absolute_group_shap=mean(abs(effect)),sum_variable_mean_absolute_shap=sum(colMeans(abs(z$phi[,v,drop=FALSE]))),positive_points=sum(effect>0),negative_points=sum(effect<0),background_baseline=z$baseline)}))
    write.csv(group,file.path(out,"group_summary.csv"),row.names=FALSE);all_groups[[species]]<-group
    half<-z$checkpoints[[as.character(cfg$pairs%/%2L)]]
    record<-list(species=species,status="ATTRIBUTION_COMPUTED",scope="CITY_CURRENT_GRID_PILOT",route=model$route,
      selected_members=model$selected,declared_predictors=model$variables,n_explain=nrow(x),n_background=nrow(background),
      permutations=z$permutations,maximum_model_raster_error=max(qa_error),maximum_additivity_error=max(abs(z$additivity_residual)),
      prediction_warnings=model$get_prediction_warnings(),
      legacy_rank_deficiency_warning=any(grepl("rank-deficient|non-estim",model$get_prediction_warnings())),
      maximum_half_full_change=max(abs(z$phi-half)),mean_half_full_change=mean(abs(z$phi-half)),
      background_reference="Uniform finite current city cells, not presence-conditioned",causal_interpretation=FALSE,
      limitation="Interventional background mixing may create correlated-predictor combinations outside observed support; pilot does not establish a community-wide, old-new reclamation or causal effect.")
    write_json(record,file.path(out,"receipt.json"),auto_unbox=TRUE,pretty=TRUE);record
  },error=function(e){record<-list(species=species,status="BLOCKED",reason=conditionMessage(e));write_json(record,file.path(out,"receipt.json"),auto_unbox=TRUE,pretty=TRUE);record})
  status[[species]]<-result
}
if(length(all_groups))write.csv(do.call(rbind,all_groups),file.path(cfg$output_directory,"group_summary.csv"),row.names=FALSE)
write_json(status,file.path(cfg$output_directory,"pilot_receipt.json"),auto_unbox=TRUE,pretty=TRUE)
cat(toJSON(status,auto_unbox=TRUE,pretty=TRUE),"\n")
