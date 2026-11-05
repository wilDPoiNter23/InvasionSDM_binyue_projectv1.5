# Prepare sampling designs without fitting or projecting distribution models.
pa_hash_file<-function(path){
 stopifnot(length(path)==1L,!is.na(path),file.exists(path),!file.info(path)$isdir)
 digest::digest(file=path,algo='sha256',serialize=FALSE)
}
pa_artifact_hashes<-function(root,files){
 stopifnot(length(files)>0L,!anyDuplicated(files),all(nzchar(files)),
           !any(grepl('(^[/\\\\]|^[A-Za-z]:|(^|[/\\\\])[.][.]([/\\\\]|$))',files)))
 setNames(lapply(file.path(root,files),pa_hash_file),files)
}
verify_pa_artifacts<-function(root,hashes,required=character()){
 expected<-unlist(hashes,use.names=TRUE)
 stopifnot(length(expected)>0L,length(names(expected))==length(expected),
           !anyDuplicated(names(expected)),all(required%in%names(expected)))
 actual<-unlist(pa_artifact_hashes(root,names(expected)),use.names=TRUE)
 if(!identical(actual,expected))stop('Saved preparation artifacts differ from their receipt')
 invisible(TRUE)
}
apply_predictor_aliases<-function(r,aliases){
 stopifnot(all(c('source_name','target_name','evidence')%in%names(aliases)),
           !anyDuplicated(aliases$source_name),!anyDuplicated(aliases$target_name),
           all(nzchar(aliases$evidence)),all(aliases$source_name%in%names(r)))
 updated<-names(r);updated[match(aliases$source_name,updated)]<-aliases$target_name
 if(anyDuplicated(updated))stop('Predictor aliases would create duplicate layer names')
 names(r)<-updated;r
}
format_equal_pa<-function(resp_xy,predictors,species,
 output_directory=getwd(),seed=42L){
 legacy_method<-Sys.getenv('INVASIONSDM_PA_METHOD','sre_equal')
 if(legacy_method!='sre_equal')stop('Only SRE equal-count sampling is supported; remove the obsolete method setting')
 stopifnot(terra::is.lonlat(predictors),!anyDuplicated(names(predictors)))
 xy<-as.matrix(resp_xy);stopifnot(ncol(xy)==2,all(is.finite(xy)))
 cells<-unique(na.omit(terra::cellFromXY(predictors,xy)))
 xy<-terra::xyFromCell(predictors,cells)
 complete<-rowSums(!is.finite(as.matrix(terra::extract(predictors,xy))))==0
 xy<-xy[complete,,drop=FALSE];n<-nrow(xy)
 if(n<2)stop('Too few complete unique presence cells')
 parameters<-list(resp.name=species,resp.var=rep(1,n),resp.xy=xy,
  expl.var=predictors,dir.name=output_directory,PA.nb.rep=3L,PA.nb.absences=n,
  PA.strategy='sre',PA.sre.quant=.025,na.rm=TRUE,
  filter.raster=TRUE,seed.val=seed)
 formatted<-do.call(biomod2::BIOMOD_FormatingData,parameters)
 presence<-!is.na(formatted@data.species)&formatted@data.species==1
 counts<-colSums(formatted@PA.table[!presence,,drop=FALSE])
 if(sum(presence)!=n||length(counts)!=3||any(counts!=n))
  stop('Requested equal-count sampling was not achieved; no model should be fitted')
 attr(formatted,'sampling_spec')<-list(method='sre_equal',n_presence=n,n_pa=n,
  replicates=3L,sre_quantile=.025,
  seed=seed,predictors=names(predictors),effort_weighted=FALSE)
 formatted
}
extract_cells_blockwise<-function(r,cells,rows_per_block=64L,progress=FALSE){
 stopifnot(all(is.finite(cells)),all(cells>=1&cells<=terra::ncell(r)))
 unique_cells<-sort(unique(cells));result<-matrix(NA_real_,length(unique_cells),terra::nlyr(r),
                                               dimnames=list(NULL,names(r)))
 block<-floor((unique_cells-1)/(terra::ncol(r)*rows_per_block))
 terra::readStart(r);on.exit(terra::readStop(r))
 for(k in unique(block)){
  first<-k*rows_per_block+1L;nrows<-min(rows_per_block,terra::nrow(r)-first+1L)
  values<-terra::readValues(r,row=first,nrows=nrows,mat=TRUE)
  ix<-which(block==k);local<-unique_cells[ix]-(first-1)*terra::ncol(r)
  result[ix,]<-values[local,,drop=FALSE]
  if(progress){cat('Raster block',k+1,'of',ceiling(terra::nrow(r)/rows_per_block),'\n');flush.console()}
 }
 result[match(cells,unique_cells),,drop=FALSE]
}
sre_bounds<-function(presence_values,quantile=0.025){
 x<-as.matrix(presence_values)
 stopifnot(nrow(x)>1,ncol(x)>0,all(is.finite(x)),quantile>=0,quantile<.5)
 rbind(lower=apply(x,2,stats::quantile,probs=quantile,names=FALSE,type=7),
       upper=apply(x,2,stats::quantile,probs=1-quantile,names=FALSE,type=7))
}
outside_sre<-function(candidate_values,bounds){
 x<-as.matrix(candidate_values);stopifnot(ncol(x)==ncol(bounds))
 complete<-rowSums(!is.finite(x))==0
 outside<-rowSums(sweep(x,2,bounds[1,],'<')|sweep(x,2,bounds[2,],'>'),na.rm=TRUE)>0
 complete&outside
}
draw_pa_replicates<-function(candidate_cells,presence_cells,n,replicates=3,seed=42){
 stopifnot(!anyDuplicated(candidate_cells),all(is.finite(candidate_cells)),
           length(n)==1,n>=1,n==as.integer(n),replicates>=1)
 cells<-candidate_cells[!candidate_cells%in%presence_cells]
 if(length(cells)<n)stop('Insufficient eligible cells; requested count was not reduced')
 set.seed(seed)
 lapply(seq_len(replicates),function(i)cells[sample.int(length(cells),n,replace=FALSE)])
}
make_pa_table<-function(presence_cells,replicate_cells){
 stopifnot(!anyDuplicated(presence_cells),length(replicate_cells)>0,
           !any(unlist(replicate_cells)%in%presence_cells))
 cells<-c(presence_cells,sort(unique(unlist(replicate_cells))))
 tab<-vapply(replicate_cells,function(pa)cells%in%c(presence_cells,pa),logical(length(cells)))
 colnames(tab)<-paste0('PA',seq_along(replicate_cells))
 list(cells=cells,response=c(rep(1,length(presence_cells)),rep(NA_real_,length(cells)-length(presence_cells))),table=as.data.frame(tab))
}
format_pa_for_biomod<-function(design,predictors,species,output_directory){
 # The supported public interface preserves distinct pseudo-absence replicates.
 xy<-terra::xyFromCell(predictors,design$cells)
 values<-terra::extract(predictors,xy)
 stopifnot(all(is.finite(as.matrix(values))))
 biomod2::BIOMOD_FormatingData(resp.name=species,resp.var=design$response,resp.xy=xy,
   expl.var=values,dir.name=output_directory,PA.strategy='user.defined',
   PA.nb.rep=ncol(design$table),PA.user.table=design$table,filter.raster=FALSE)
}
