# Original source: validate_pair814.R
# Original source location: ${NON_NATIVE_ROOT}/audit/environment_only_sensitivity/validate_pair814.R
# Original source SHA-256: c6d947cc4c8481f125257833a292e47dc4ae55bead563dc7e0b6fa1561ea1b53
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_ENVIRONMENT_ONLY_SENSITIVITY)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/usr/bin/env Rscript
.libPaths(c(Sys.getenv('R_LIBS_USER'),.libPaths()));suppressPackageStartupMessages(library(terra))
a='<NON_NATIVE_ROOT>/audit/environment_only_sensitivity'
x=read.delim(file.path(a,'sensitivity_inventory.tsv'),quote='',stringsAsFactors=FALSE);sp=commandArgs(TRUE)[1];r=x[x$accepted_taxon==sp,,drop=FALSE];stopifnot(nrow(r)==1,nzchar(r$clean_full_output_root))
f=r$clean_full_output_root;e=r$clean_env_output_root
stopifnot(file.exists(file.path(f,'VALIDATED.rds')),file.exists(file.path(e,'VALIDATED.rds')))
bf=readRDS(file.path(f,'design_before.rds'));be=readRDS(file.path(e,'design_before.rds'))
for(k in c('coord','response','PA','cv','baseline'))stopifnot(identical(bf[[k]],be[[k]]))
stopifnot(!'ID'%in%bf$predictors,!'ID'%in%be$predictors,identical(be$predictors,bf$predictors[!bf$predictors%in%strsplit(r$human_exclusion_set,';',fixed=TRUE)[[1]]]))
rf=rast(file.path(f,'ensemble_suitability.tif'));re=rast(file.path(e,'ensemble_suitability.tif'));old=rast(r$baseline_global_projection_path)
stopifnot(compareGeom(rf,re,old,stopOnError=FALSE));terraOptions(tempdir=file.path(e,'tmp'),todisk=TRUE,memmax=40)
qf=readRDS(file.path(f,'VALIDATED.rds'));qe=readRDS(file.path(e,'VALIDATED.rds'));stopifnot(qf$scale=='BIOMOD_0_1000',qe$scale=='BIOMOD_0_1000')
# GAP13 accepted FULL is the manual EMmean of BIOMOD's 0-1000 individual rasters.
# Source provenance: project_gap13_repaired.r, explicit manual mean; no scale transformation.
policy=jsonlite::fromJSON(file.path(a,'frozen_route_policies.json'),simplifyVector=FALSE)[[r$authoritative_baseline_source]]
stopifnot(grepl('GAP13',r$authoritative_baseline_source),policy$kind=='MANUAL_EMMEAN')
for(z in list(rf,re,old)){mm=global(z,c('min','max'),na.rm=TRUE);stopifnot(mm[1,1]>=0,mm[1,2]<=1000.01)}
rf=rf/1000;re=re/1000;old=old/1000
write.csv(data.frame(contrast=c('CLEAN_FULL_vs_CLEAN_ENV','contaminated_FULL_vs_CLEAN_FULL'),role=c('PRIMARY_ANTHROPOGENIC_SENSITIVITY','TECHNICAL_CONTAMINATION_DIAGNOSTIC_ONLY'),mean_absolute_difference=c(global(abs(rf-re),'mean',na.rm=TRUE)[1,1],global(abs(old-rf),'mean',na.rm=TRUE)[1,1])),file.path(dirname(f),'paired_comparison.csv'),row.names=FALSE)
saveRDS(list(species=sp,PA_CV_identical=TRUE,ID_absent=TRUE,both_routes_validated=TRUE,completed_at=as.character(Sys.time())),file.path(dirname(f),'PAIRED_VALIDATED.rds'));cat('PAIRED_ROUTE_PASS\n')
