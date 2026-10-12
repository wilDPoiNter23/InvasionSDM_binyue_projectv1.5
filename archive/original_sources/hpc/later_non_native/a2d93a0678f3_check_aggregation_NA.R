# Original source: check_aggregation_NA.R
# Original source location: ${NON_NATIVE_ROOT}/audit/environment_only_sensitivity/check_aggregation_NA.R
# Original source SHA-256: a2d93a0678f3148643aa6d149c515a42614aa5dbda26784533ca256d2de1d8b2
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_ENVIRONMENT_ONLY_SENSITIVITY)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
.libPaths(c('<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu',.libPaths()));suppressPackageStartupMessages(library(terra))
d='<NON_NATIVE_ROOT>/02_derived_non_native_potential';v=file.path(d,'04_aggregate/rasters/aggregation_variants_20260917');tax=read.csv(file.path(d,'06_tables/tssbin_manifest_audit_813.csv'))$accepted_taxon;safe=function(x)gsub('[ .]+','_',trimws(x));paths=file.path(v,'inputs/cont_masked',paste0(safe(tax[1:10]),'__ensemble_suitability_non_native.tif'));z=rast(paths);points=spatSample(z[[1]],10000,method='regular',xy=TRUE,na.rm=FALSE);values=extract(z,points[,c('x','y')],ID=FALSE);idx=which(is.na(values[,1])&rowSums(!is.na(values[,-1,drop=FALSE]))>0);cat('FIRST_TAXON',tax[1],'candidate_cells',length(idx),'\n');stopifnot(length(idx)>0);idx=idx[1:min(10,length(idx))];old=rast(file.path(v,'aggregates/ensemble_mean_suitability_non_native_814.tif'));out=data.frame(x=points$x[idx],y=points$y[idx],first_taxon_value=values[idx,1],later_nonNA_among_9=rowSums(!is.na(values[idx,-1,drop=FALSE])),old_814_mean=extract(old,points[idx,c('x','y')],ID=FALSE)[,1]);print(out);write.csv(out,'<NON_NATIVE_ROOT>/audit/environment_only_sensitivity/aggregation_first_taxon_NA_diagnostic.csv',row.names=FALSE)
