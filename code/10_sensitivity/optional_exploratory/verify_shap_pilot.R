# Independently bind stored permutation contributions to exact saved-model predictions.
suppressPackageStartupMessages({library(biomod2);library(terra);library(jsonlite);library(digest)})
args<-commandArgs(TRUE);if(length(args)!=2L)stop("Supply private configuration and fresh verification receipt")
if(file.exists(args[2]))stop("Verification receipt exists")
cfg<-fromJSON(args[1],simplifyVector=TRUE)
dir.create(cfg$tmp_directory,recursive=TRUE,showWarnings=FALSE);terraOptions(tempdir=cfg$tmp_directory,memfrac=.1,progress=0)
setwd(cfg$tmp_directory)
source(file.path(cfg$module_directory,"shap_saved_predict.R"))
pilot<-fromJSON(file.path(cfg$output_directory,"pilot_receipt.json"),simplifyVector=FALSE)
checks<-list()
for(species in cfg$species) {
  if(is.null(pilot[[species]]) || pilot[[species]]$status!="ATTRIBUTION_COMPUTED")stop("A frozen pilot species was not attributed: ",species)
  out<-file.path(cfg$output_directory,gsub("[^A-Za-z0-9]","_",species))
  design<-readRDS(file.path(out,"frozen_design.rds"));z<-readRDS(file.path(out,"shap_values.rds"))
  x<-as.matrix(design$x);b<-as.matrix(design$background);ne<-nrow(x);p<-ncol(x)
  stopifnot(identical(colnames(x),design$variables),identical(colnames(b),design$variables),
    ne==cfg$n_explain,nrow(b)==cfg$n_background,identical(dim(z$phi),c(ne,p)),
    identical(dim(z$pair_values),c(ne,p,as.integer(cfg$pairs))),all(is.finite(x)),all(is.finite(b)))
  calculated<-matrix(0,ne,p)
  for(k in seq_len(cfg$pairs))calculated<-calculated+z$pair_values[,,k]/cfg$pairs
  residuals<-sweep(z$pair_values,c(1,2),calculated,"-")
  standard_error<-sqrt(apply(residuals^2,c(1,2),sum)/(cfg$pairs-1)/cfg$pairs)
  half<-matrix(0,ne,p);nh<-cfg$pairs%/%2L
  for(k in seq_len(nh))half<-half+z$pair_values[,,k]/nh
  stopifnot(all(is.finite(calculated)),max(abs(calculated-z$phi))<1e-12,
    max(abs(standard_error-z$se))<1e-12,max(abs(half-z$checkpoints[[as.character(nh)]]))<1e-12)
  if(digest(file=design$model_path,algo="sha256")!=design$model_sha256)stop("Top-level model changed")
  if(!is.na(design$ensemble_path) && digest(file=design$ensemble_path,algo="sha256")!=design$ensemble_sha256)stop("Exact ensemble object changed")
  for(j in seq_along(design$member_paths))if(!is.na(design$member_paths[j]))stopifnot(digest(file=design$member_paths[j],algo="sha256")==design$member_file_sha256[j])
  for(path in names(design$source_code_sha256))stopifnot(digest(file=path,algo="sha256")==design$source_code_sha256[[path]])
  manifest<-read.csv(cfg$manifest,stringsAsFactors=FALSE);row<-manifest[manifest$species==species&manifest$group=="FULL",,drop=FALSE]
  stopifnot(nrow(row)==1L,row$projection_path==design$projection_path,row$model_path==design$model_path)
  model<-shap_load_predictor(row,cfg$model_directory)
  stopifnot(identical(model$variables,design$variables),identical(model$selected,design$selected_members))
  independently_predicted<-model$predict(as.data.frame(x));baseline<-mean(model$predict(as.data.frame(b)))
  stopifnot(max(abs(independently_predicted-z$prediction))<1e-12,abs(baseline-z$baseline)<1e-12)
  addition<-independently_predicted-baseline-rowSums(calculated)
  stopifnot(max(abs(addition))<1e-8,max(abs(addition-z$additivity_residual))<1e-12)
  raster<-rast(design$projection_path);observed<-extract(raster,design$cell_ids)[[1]]/1000
  predicted_all<-model$predict(rbind(as.data.frame(x),as.data.frame(b)))
  qa<-read.csv(file.path(out,"model_raster_check.csv"))
  stopifnot(length(observed)==ne+nrow(b),all(is.finite(observed)),
    max(abs(predicted_all-observed))<=cfg$model_raster_tolerance,
    max(abs(qa$observed-observed))<1e-12,max(abs(qa$expected-predicted_all))<1e-12)
  long<-read.csv(file.path(out,"signed_variable_attributions.csv"),stringsAsFactors=FALSE)
  stopifnot(nrow(long)==ne*p,identical(long$variable,rep(colnames(x),times=ne)),
    max(abs(long$value-as.vector(t(x))))<1e-8,max(abs(long$shap-as.vector(t(calculated))))<1e-12,
    max(abs(long$mc_standard_error-as.vector(t(standard_error))))<1e-12)
  H<-c("reclamation_raster","VIIRS_2023_2.5min","GDP2020_2.5min","BUILT_S_E2025_2.5min","Pop2020_2.5min")
  group<-read.csv(file.path(out,"group_summary.csv"),stringsAsFactors=FALSE)
  for(g in c("HUMAN","NATURAL")) {
    columns<-which(if(g=="HUMAN")colnames(x)%in%H else !colnames(x)%in%H)
    effect<-rowSums(calculated[,columns,drop=FALSE]);q<-group[group$predictor_group==g,]
    stopifnot(nrow(q)==1L,abs(q$mean_signed_shap-mean(effect))<1e-12,
      abs(q$mean_absolute_group_shap-mean(abs(effect)))<1e-12,
      abs(q$sum_variable_mean_absolute_shap-sum(colMeans(abs(calculated[,columns,drop=FALSE]))))<1e-12,
      q$positive_points==sum(effect>0),q$negative_points==sum(effect<0))
  }
  checks[[species]]<-list(status="PASS",maximum_additivity_error=max(abs(addition)),
    maximum_model_raster_error=max(abs(predicted_all-observed)),
    maximum_half_full_change=max(abs(calculated-half)),mean_half_full_change=mean(abs(calculated-half)),
    frozen_design_sha256=digest(file=file.path(out,"frozen_design.rds"),algo="sha256"))
}
if(length(checks)!=length(cfg$species))stop("Incomplete frozen species verification")
write_json(list(status="PASS",species_checked=length(checks),checks=checks,
  causal_validation=FALSE,scope="Bounded saved-product city-grid attribution only"),args[2],auto_unbox=TRUE,pretty=TRUE)
cat("Independent frozen SHAP reconstruction verification passed\n")
