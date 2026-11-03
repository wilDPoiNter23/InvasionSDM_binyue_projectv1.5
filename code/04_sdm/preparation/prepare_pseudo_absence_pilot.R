# Prepare a fixed species sampling pilot; no SDMs are fitted or evaluated.
# Usage: Rscript prepare_pseudo_absence_pilot.R configuration.json
suppressPackageStartupMessages({library(terra);library(jsonlite);library(digest)})
args<-getOption('invasionSDM.pa.config',commandArgs(TRUE));stopifnot(length(args)==1)
cfg<-fromJSON(args[1]);stopifnot(identical(cfg$allow_model_fitting,FALSE),
 length(cfg$replicates)==1L,cfg$replicates==3L,
 length(cfg$sre_quantile)==1L,cfg$sre_quantile==.025)
for(field in c('seed','pilot_species','candidate_cells','minimum_presence_cells')){
 value<-cfg[[field]]
 stopifnot(length(value)==1L,is.finite(value),value==as.integer(value))
}
stopifnot(cfg$seed>=0L,cfg$pilot_species>=1L,cfg$pilot_species<=814L,
 cfg$candidate_cells>=1L,cfg$minimum_presence_cells>=2L)
 if(!is.null(cfg$effort_raster)&&nzchar(cfg$effort_raster))
  stop('External weighting is not part of the SRE-only preparation')
helper_path<-file.path(cfg$project_root,'code/04_sdm/preparation/pseudo_absence_design.R')
source(helper_path)
hash_file<-pa_hash_file
out<-cfg$output_root;receipt<-file.path(out,'preparation_receipt.json')
inventory<-read.delim(cfg$inventory,check.names=FALSE)
inventory<-inventory[inventory$scope=='814_FINAL_ANALYSIS',]
stopifnot(nrow(inventory)==814,!anyDuplicated(inventory$accepted_taxon))
inventory<-inventory[order(inventory$accepted_taxon),]
safe<-gsub('[^A-Za-z0-9_]','_',inventory$accepted_taxon)
stopifnot(!anyDuplicated(safe))
inventory$occurrence_file<-file.path(cfg$occurrence_root,safe,paste0(safe,'_occurrences.csv'))
inventory$occurrence_file_available<-file.exists(inventory$occurrence_file)
set.seed(cfg$seed);index<-sample.int(nrow(inventory),cfg$pilot_species,replace=FALSE)
pilot<-inventory[index,];pilot$pilot_order<-seq_len(nrow(pilot))
r<-rast(cfg$predictor_stack)
raster_info<-fromJSON(paste(describe(cfg$predictor_stack,options='json'),collapse='\n'))
if(!identical(raster_info$driverShortName,'GTiff')||length(raster_info$files)!=1L||
   !identical(normalizePath(raster_info$files),normalizePath(cfg$predictor_stack)))
 stop('Preparation requires one self-contained GeoTIFF without external sidecars or backing rasters')
if(!is.null(cfg$predictor_aliases))r<-apply_predictor_aliases(r,read.csv(cfg$predictor_aliases))
stopifnot(is.lonlat(r),all(abs(res(r)-1/24)<1e-10))
needed<-sort(unique(unlist(strsplit(pilot$environmental_predictor_vector,';',fixed=TRUE))))
available<-union(intersect(needed,names(r)),'wc2.1_2.5m_bio_1');stopifnot(all(available%in%names(r)))
presence_inputs<-lapply(seq_len(nrow(pilot)),function(i){
 row<-pilot[i,]
 if(!row$occurrence_file_available)return(NULL)
 raw<-read.csv(row$occurrence_file)
 stopifnot(all(raw$species==row$accepted_taxon))
 xy<-as.matrix(raw[,c('decimalLongitude','decimalLatitude')])
 valid<-rowSums(!is.finite(xy))==0&xy[,1]>=-180&xy[,1]<=180&xy[,2]>=-90&xy[,2]<=90
 list(raw_records=nrow(raw),valid_coordinate_records=sum(valid),
      cells=sort(unique(na.omit(cellFromXY(r,xy[valid,,drop=FALSE])))),sha256=hash_file(row$occurrence_file))
})
source_identity<-list(inventory_sha256=hash_file(cfg$inventory),
 predictor_stack_sha256=hash_file(cfg$predictor_stack),
 predictor_aliases_sha256=if(!is.null(cfg$predictor_aliases))hash_file(cfg$predictor_aliases)else NULL,
 occurrence_sha256=lapply(presence_inputs,function(x)x$sha256),
 helper_sha256=hash_file(helper_path),
 runner_sha256=hash_file(file.path(cfg$project_root,'code/04_sdm/preparation/prepare_pseudo_absence_pilot.R')),
 runtime=list(R_version=R.version.string,terra_version=as.character(packageVersion('terra'))),
 settings=cfg[c('seed','pilot_species','candidate_cells','minimum_presence_cells','replicates','sre_quantile')])
input_fingerprint<-digest(source_identity)
fingerprint<-digest(list(configuration=hash_file(args[1]),design='SRE_EQUAL_ONLY_V2',input=input_fingerprint))
required_artifacts<-c('tables/roster_readiness.csv','tables/pilot_species.csv',
 'tables/sampling_diagnostics.csv','tables/presence_preparation.csv','cache/background_candidates.rds',
 'preparation_state.json',
 file.path('designs',paste0(gsub('[^A-Za-z0-9_]','_',pilot$accepted_taxon),'.rds')))
if(file.exists(receipt)){
 old<-fromJSON(receipt)
 if(!identical(old$preparation_fingerprint,fingerprint))
  stop('Changed or unverified preparation inputs require a separate output_root')
 verify_pa_artifacts(out,old$artifact_sha256,required_artifacts)
 cat('Existing completed pilot and all recorded artifacts verified; no work repeated.\n');quit(save='no',status=0)
}
state_path<-file.path(out,'preparation_state.json')
if(file.exists(state_path)){
 state<-fromJSON(state_path)
 if(!identical(state$preparation_fingerprint,fingerprint))
  stop('Changed partial preparation inputs require a separate output_root')
}else if(dir.exists(out)&&length(list.files(out,recursive=TRUE,all.files=TRUE,no..=TRUE))>0L)
 stop('Existing unverified files require a separate output_root')
if(!is.null(cfg$reuse_completed_from)){
 old_receipt<-fromJSON(file.path(cfg$reuse_completed_from,'preparation_receipt.json'))
 if(!identical(old_receipt$input_fingerprint,input_fingerprint))
  stop('Reuse requires matching source-content identities; historical receipts without these identities are not reusable')
 verify_pa_artifacts(cfg$reuse_completed_from,old_receipt$artifact_sha256,required_artifacts)
}
dir.create(out,recursive=TRUE,showWarnings=FALSE)
for(x in c('tables','designs','cache','logs'))dir.create(file.path(out,x),showWarnings=FALSE)
if(!file.exists(state_path))write_json(list(preparation_fingerprint=fingerprint),state_path,auto_unbox=TRUE)
terraOptions(tempdir=file.path(out,'cache'),memfrac=.25,progress=0)
write.csv(inventory[,c('accepted_taxon','occurrence_file_available','authoritative_baseline_source','algorithm_route')],
          file.path(out,'tables','roster_readiness.csv'),row.names=FALSE)
write.csv(pilot[,c('pilot_order','accepted_taxon','occurrence_file_available','environmental_predictor_vector')],
          file.path(out,'tables','pilot_species.csv'),row.names=FALSE)
cachepath<-file.path(out,'cache','background_candidates.rds')
stack_info<-file.info(cfg$predictor_stack,extra_cols=FALSE)
cachekey<-digest(list(stack=normalizePath(cfg$predictor_stack),size=stack_info$size,
                     mtime=stack_info$mtime,available=available,n=cfg$candidate_cells,seed=cfg$seed,
                     input_fingerprint=input_fingerprint))
if(file.exists(cachepath)){
 cached<-readRDS(cachepath);stopifnot(identical(cached$key,cachekey));bg<-cached$data
 presence_environment<-cached$presence_environment
}else if(!is.null(cfg$reuse_completed_from)){
 # Preserve the original candidate draw and already extracted environmental values.
 cached<-readRDS(file.path(cfg$reuse_completed_from,'cache','background_candidates.rds'))
 old_cache<-cached
 bg<-cached$data;presence_environment<-cached$presence_environment
 missing<-setdiff(available,colnames(presence_environment$values))
 ids<-sort(unique(c(bg$cell,presence_environment$cells)))
 if(length(missing)){
  extra<-extract_cells_blockwise(r[[missing]],ids,progress=TRUE)
  for(v in missing)bg[[v]]<-extra[match(bg$cell,ids),v]
  presence_environment$values<-cbind(presence_environment$values,
    extra[match(presence_environment$cells,ids),,drop=FALSE])
 }
 saveRDS(list(key=cachekey,data=bg,presence_environment=presence_environment),cachepath)
}else{
 set.seed(cfg$seed)
 candidate_ids<-sample.int(ncell(r),min(ncell(r),cfg$candidate_cells*6),replace=FALSE)
 presence_ids<-sort(unique(unlist(lapply(presence_inputs,function(x)x$cells))))
 all_ids<-sort(unique(c(candidate_ids,presence_ids)))
 env<-extract_cells_blockwise(r[[available]],all_ids,progress=TRUE)
 presence_environment<-list(cells=presence_ids,values=env[match(presence_ids,all_ids),,drop=FALSE])
 bio1<-env[match(candidate_ids,all_ids),'wc2.1_2.5m_bio_1']
 land_ids<-candidate_ids[is.finite(bio1)]
 if(length(land_ids)<cfg$candidate_cells)stop('Candidate draw contains too few land cells; increase the candidate draw explicitly')
 cells<-land_ids[seq_len(cfg$candidate_cells)]
 xy<-xyFromCell(r,cells)
 bg<-data.frame(cell=cells,x=xy[,1],y=xy[,2],env[match(cells,all_ids),,drop=FALSE],check.names=FALSE)
 stopifnot(!anyDuplicated(bg$cell));saveRDS(list(key=cachekey,data=bg,presence_environment=presence_environment),cachepath)
}
rows<-list();qc<-list()
for(i in seq_len(nrow(pilot))){
 row<-pilot[i,];species<-row$accepted_taxon;cat(i,'/',nrow(pilot),species,'\n');flush.console()
 target<-file.path(out,'designs',paste0(gsub('[^A-Za-z0-9_]','_',species),'.rds'))
 key<-digest(list(fingerprint,presence_inputs[[i]]$sha256,cachekey))
 if(file.exists(target)){
  z<-readRDS(target)
  if(identical(z$key,key)){rows[[i]]<-z$summary;qc[[i]]<-z$qc;next}
  stop('Changed or unverified saved designs require a separate output_root')
 }
 if(!is.null(cfg$reuse_completed_from)){
  old_path<-file.path(cfg$reuse_completed_from,'designs',basename(target))
  previous<-readRDS(old_path)
  if(identical(previous$qc$status,'PREPARED')&&!is.null(previous$designs$sre_equal)){
   # Reuse only when occurrence-file identity and all stored predictor values agree.
   if(!exists('old_cache',inherits=FALSE))old_cache<-readRDS(file.path(cfg$reuse_completed_from,'cache','background_candidates.rds'))
   old_receipt<-fromJSON(file.path(cfg$reuse_completed_from,'preparation_receipt.json'))
   old_fingerprint<-if(is.null(old_receipt$preparation_fingerprint))old_receipt$config_sha256 else old_receipt$preparation_fingerprint
   old_key<-digest(list(old_fingerprint,presence_inputs[[i]]$sha256,old_cache$key))
   stopifnot(identical(previous$key,old_key))
   vars<-strsplit(row$environmental_predictor_vector,';',fixed=TRUE)[[1]]
   stopifnot(identical(bg[,c('cell',vars)],old_cache$data[,c('cell',vars)]))
   previous$designs<-previous$designs['sre_equal']
   previous$summary<-previous$summary[previous$summary$scheme=='sre_equal',]
   previous$summary$mean_effort<-NULL
   previous$key<-key;previous$reused_from<-old_path
   saveRDS(previous,target);rows[[i]]<-previous$summary;qc[[i]]<-previous$qc
   cat('Retained completed sampling design.\n');next
  }
 }
 z<-tryCatch({
  if(!row$occurrence_file_available)stop('Processed occurrence file unavailable')
  vars<-strsplit(row$environmental_predictor_vector,';',fixed=TRUE)[[1]]
  if(!all(vars%in%names(r)))stop(paste('Missing exact predictor names:',paste(setdiff(vars,names(r)),collapse=';')))
  input<-presence_inputs[[i]];cells<-input$cells
  values<-presence_environment$values[match(cells,presence_environment$cells),vars,drop=FALSE]
  finite<-rowSums(!is.finite(values))==0;cells<-cells[finite];values<-values[finite,,drop=FALSE]
  if(length(cells)<cfg$minimum_presence_cells)stop('Too few complete presence cells')
  bounds<-sre_bounds(values,cfg$sre_quantile)
  bv<-as.matrix(bg[,vars,drop=FALSE]);eligible<-rowSums(!is.finite(bv))==0&!bg$cell%in%cells
  outside<-outside_sre(bv,bounds)&eligible
  scheme<-'sre_equal';n<-length(cells);use<-outside
  draws<-tryCatch(draw_pa_replicates(bg$cell[use],cells,n,cfg$replicates,cfg$seed+i*100+6L),error=function(e)e)
  designs<-list()
  if(!inherits(draws,'error')){
   design<-make_pa_table(cells,draws);design$predictors<-vars
   design$source<-'processed_occurrences_cell_deduplicated';designs[[scheme]]<-design
   summaries<-lapply(seq_along(draws),function(k){
    ix<-match(draws[[k]],bg$cell)
    data.frame(species=species,scheme=scheme,replicate=k,n_presence=n,n_requested=n,
     n_selected=length(ix),eligible_candidates=sum(use),pa_presence_ratio=length(ix)/n,
     fraction_outside_sre=mean(outside[ix]),mean_latitude=mean(bg$y[ix]),status='PREPARED')
   })
  }else summaries<-list(data.frame(species=species,scheme=scheme,replicate=NA_integer_,
   n_presence=n,n_requested=n,n_selected=0L,eligible_candidates=sum(use),pa_presence_ratio=NA_real_,
   fraction_outside_sre=NA_real_,mean_latitude=NA_real_,status=conditionMessage(draws)))
  list(key=key,summary=do.call(rbind,summaries),designs=designs,sre_bounds=bounds,
    qc=data.frame(species=species,raw_records=input$raw_records,valid_coordinate_records=input$valid_coordinate_records,
                  complete_unique_presence_cells=length(cells),sre_candidates=sum(outside),
                  status=if(inherits(draws,'error'))conditionMessage(draws)else 'PREPARED'))
 },error=function(e)list(key=key,summary=NULL,designs=NULL,
     qc=data.frame(species=species,raw_records=NA_integer_,valid_coordinate_records=NA_integer_,
                   complete_unique_presence_cells=NA_integer_,sre_candidates=NA_integer_,status=conditionMessage(e))))
 saveRDS(z,target);rows[[i]]<-z$summary;qc[[i]]<-z$qc
}
write.csv(do.call(rbind,rows),file.path(out,'tables','sampling_diagnostics.csv'),row.names=FALSE)
write.csv(do.call(rbind,qc),file.path(out,'tables','presence_preparation.csv'),row.names=FALSE)
write_json(list(status='PREPARATION_COMPLETE',config_sha256=hash_file(args[1]),
   preparation_fingerprint=fingerprint,
    input_fingerprint=input_fingerprint,source_identity=source_identity,
    artifact_sha256=pa_artifact_hashes(out,required_artifacts),
   predictor_aliases_sha256=if(!is.null(cfg$predictor_aliases))hash_file(cfg$predictor_aliases)else NULL,
   sdms_fitted=0,
   pilot_species=nrow(pilot),prepared_species=sum(do.call(rbind,qc)$status=='PREPARED'),sampling_strategy='sre_equal',effort_weighted=FALSE,
   occurrence_scope='Existing processed invasive-species files; not all vascular plants and not frozen fitted presence rows',
   candidate_scope='Fixed random sample of native-resolution land cells, not an exhaustive global PA pool',
   source_doi='10.1038/s41559-026-03040-2',
   interpretation='Sampling geometry and feasibility only; no TSS, Boyce or optimal-strategy inference',
   R_version=R.version.string,terra_version=as.character(packageVersion('terra'))),receipt,pretty=TRUE,auto_unbox=TRUE)
cat('PREPARATION_COMPLETE: no distribution models fitted.\n')
