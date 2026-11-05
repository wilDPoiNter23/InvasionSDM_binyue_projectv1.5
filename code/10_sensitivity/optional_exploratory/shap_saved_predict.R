# Predict the exact stored single model or selected EMmean on small matrices.
shap_load_object <- function(path) {
  if(grepl("[.]rds$",path))return(readRDS(path))
  e<-new.env(parent=globalenv());n<-load(path,e)
  if(length(n)!=1L)stop("One saved object required")
  get(n[1],e)
}
shap_load_predictor <- function(row,model_directory) {
  model_env<-environment()
  previous_directory<-getwd();on.exit(setwd(previous_directory),add=TRUE)
  setwd(model_directory)
  m<-shap_load_object(row$model_path);vars<-m@expl.var.names
  if(grepl("EMmean",basename(row$projection_path),fixed=TRUE)) {
    ens_path<-sub("[.]models[.]out$",".ensemble.models.out",row$model_path)
    ens<-shap_load_object(ens_path);chosen<-sub("[.](grd|tif)$","",basename(row$projection_path))
    loaded<-BIOMOD_LoadModels(ens,full.name=chosen);obj<-get(loaded[1],envir=model_env,inherits=TRUE)
    if(!is(obj,"EMmean_biomod2_model"))stop("Only the exact stored EMmean route is supported")
    selected<-slot(obj,"model");route<-"STORED_EMMEAN"
  } else {
    selected<-m@models.computed[vapply(m@models.computed,function(n)grepl(n,basename(row$projection_path),fixed=TRUE),logical(1))]
    if(length(selected)!=1L)stop("Single product identity unresolved")
    ens_path<-NA_character_;chosen<-selected;route<-"STORED_SINGLE"
  }
  loaded<-BIOMOD_LoadModels(m,full.name=selected)
  objects<-lapply(loaded,function(n)get(n,envir=model_env,inherits=TRUE))
  member_paths<-file.path(m@dir.name,m@sp.name,"models",m@modeling.id,selected)
  member_paths<-vapply(member_paths,function(p)if(file.exists(p))normalizePath(p)else NA_character_,character(1))
  prediction_warnings<-character()
  predict_matrix<-function(x) {
    x<-as.data.frame(x);if(!identical(names(x),vars)||any(!is.finite(as.matrix(x))))stop("Predictor matrix contract failed")
    grid<-terra::rast(nrows=1,ncols=nrow(x),nlyrs=ncol(x),xmin=0,xmax=nrow(x),ymin=0,ymax=1,crs="")
    names(grid)<-vars;terra::values(grid)<-as.matrix(x)
    # Raster dispatch preserves installed MAXNET clamp=FALSE behavior.
    p<-vapply(objects,function(o){
      value<-withCallingHandlers(predict(o,newdata=grid,on_0_1000=TRUE,seedval=42L,mod.name="matrix_prediction"),
        warning=function(w){prediction_warnings<<-unique(c(prediction_warnings,conditionMessage(w)))})
      as.numeric(terra::values(value))
    },numeric(nrow(x)))
    if(any(!is.finite(p)))stop("Nonfinite exact member prediction")
    p<-matrix(p,nrow=nrow(x),ncol=length(objects))
    raw<-rowMeans(round(p))
    # Persisted signed-integer products quantize nonnegative ensemble means.
    floor(raw+.5)/1000
  }
  list(predict=predict_matrix,variables=vars,selected=selected,chosen=chosen,route=route,ensemble_path=ens_path,
    member_paths=member_paths,loaded_object_sha256=vapply(objects,function(o)digest::digest(o,algo="sha256"),character(1)),
    get_prediction_warnings=function()prediction_warnings)
}
