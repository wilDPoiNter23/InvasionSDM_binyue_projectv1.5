# Independently check saved sampling cells and summarize preparation diagnostics.
suppressPackageStartupMessages(library(jsonlite))
source(file.path(Sys.getenv('INVASIONSDM_PROJECT_ROOT','.'),'code/04_sdm/preparation/pseudo_absence_design.R'))
args<-commandArgs(TRUE);stopifnot(length(args)==1)
out<-args[1];receipt<-fromJSON(file.path(out,'preparation_receipt.json'))
stopifnot(receipt$sdms_fitted==0,receipt$status=='PREPARATION_COMPLETE')
diagnostics<-read.csv(file.path(out,'tables','sampling_diagnostics.csv'))
presences<-read.csv(file.path(out,'tables','presence_preparation.csv'))
pilot<-read.csv(file.path(out,'tables','pilot_species.csv'))
required<-c('tables/roster_readiness.csv','tables/pilot_species.csv',
 'tables/sampling_diagnostics.csv','tables/presence_preparation.csv','cache/background_candidates.rds',
 'preparation_state.json',file.path('designs',paste0(gsub('[^A-Za-z0-9_]','_',pilot$accepted_taxon),'.rds')))
verify_pa_artifacts(out,receipt$artifact_sha256,required)
cache<-readRDS(file.path(out,'cache','background_candidates.rds'));bg<-cache$data
checks<-list()
for(species in pilot$accepted_taxon){
 path<-file.path(out,'designs',paste0(gsub('[^A-Za-z0-9_]','_',species),'.rds'))
 z<-readRDS(path)
  if(!identical(z$qc$status,'PREPARED')){
   stopifnot(length(z$designs)==0L,!any(z$summary$status=='PREPARED'))
   next
  }
  stopifnot(identical(names(z$designs),'sre_equal'))
 for(scheme in names(z$designs)){
  d<-z$designs[[scheme]];is_presence<-!is.na(d$response)
  stopifnot(!anyDuplicated(d$cells),all(d$response[is_presence]==1),all(d$table[is_presence,]),
            nrow(d$table)==length(d$cells),ncol(d$table)==3,!anyNA(d$table))
  presence_values<-cache$presence_environment$values[
   match(d$cells[is_presence],cache$presence_environment$cells),d$predictors,drop=FALSE]
  stopifnot(all(is.finite(presence_values)))
  bounds<-sre_bounds(presence_values,receipt$source_identity$settings$sre_quantile)
  stopifnot(isTRUE(all.equal(bounds,z$sre_bounds,tolerance=1e-12)))
  counts<-integer(3);outside_fraction<-numeric(3)
  for(k in 1:3){
   cells<-d$cells[!is_presence&d$table[,k]];counts[k]<-length(cells)
   stopifnot(!anyDuplicated(cells),!any(cells%in%d$cells[is_presence]),all(cells%in%bg$cell))
   values<-as.matrix(bg[match(cells,bg$cell),d$predictors,drop=FALSE])
   stopifnot(all(is.finite(values)))
   outside<-rep(FALSE,nrow(values))
   for(j in seq_len(ncol(values)))outside<-outside|values[,j]<z$sre_bounds[1,j]|values[,j]>z$sre_bounds[2,j]
   outside_fraction[k]<-mean(outside)
   if(startsWith(scheme,'sre'))stopifnot(all(outside))
   s<-diagnostics[diagnostics$species==species&diagnostics$scheme==scheme&diagnostics$replicate==k&!is.na(diagnostics$replicate),]
   stopifnot(nrow(s)==1,length(cells)==s$n_requested,length(cells)==s$n_selected,
             sum(is_presence)==s$n_presence,abs(outside_fraction[k]-s$fraction_outside_sre)<1e-12)
  }
  checks[[length(checks)+1]]<-data.frame(species=species,scheme=scheme,replicates=3,
      n_presence=sum(is_presence),n_pa=counts[1],all_checks_passed=TRUE)
 }
}
checked<-if(length(checks))do.call(rbind,checks)else data.frame(
 species=character(),scheme=character(),replicates=integer(),n_presence=integer(),n_pa=integer(),all_checks_passed=logical())
stopifnot(nrow(pilot)==receipt$pilot_species,nrow(presences)==nrow(pilot),
          !anyDuplicated(pilot$accepted_taxon),!anyDuplicated(presences$species),
          setequal(pilot$accepted_taxon,presences$species),
          nrow(diagnostics[diagnostics$status=='PREPARED',])==nrow(checked)*3)
write.csv(checked,file.path(out,'tables','verified_designs.csv'),row.names=FALSE)
good<-diagnostics[diagnostics$status=='PREPARED',]
summary<-if(nrow(good))do.call(rbind,lapply(split(good,good$scheme),function(d)data.frame(
  scheme=d$scheme[1],n_species=length(unique(d$species)),n_replicates=nrow(d),
  median_presence_cells=median(d$n_presence),minimum_pa_presence_ratio=min(d$pa_presence_ratio),
  median_pa_presence_ratio=median(d$pa_presence_ratio),maximum_pa_presence_ratio=max(d$pa_presence_ratio),
   median_fraction_outside_sre=median(d$fraction_outside_sre))))else data.frame()
write.csv(summary,file.path(out,'tables','scheme_summary.csv'),row.names=FALSE)
complete<-setequal(checked$species,pilot$accepted_taxon)&&all(presences$status=='PREPARED')
stopifnot(receipt$prepared_species==length(unique(checked$species)))
write_json(list(status=if(complete)'PASS'else 'INCOMPLETE',pilot_species=nrow(pilot),prepared_species=length(unique(checked$species)),
  verified_species_scheme_pairs=nrow(checked),verified_replicates=nrow(good),models_fitted=0,
  checks=c('Unique cells','Presence exclusion','Exact PA counts','Separate replicate membership',
           'Complete predictor values','SRE exterior membership','Stored diagnostic agreement'),
  schemes_verified=unique(checked$scheme)),
  file.path(out,'verification.json'),pretty=TRUE,auto_unbox=TRUE)
print(summary)
if(!complete)stop('INCOMPLETE: not every pilot species has a prepared and verified design')
cat('PASS: independent saved-design verification; no model fitting.\n')
