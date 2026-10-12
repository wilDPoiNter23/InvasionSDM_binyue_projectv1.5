# Original source: probe_saved_QC814.R
# Original source location: ${NON_NATIVE_ROOT}/audit/environment_only_sensitivity/probe_saved_QC814.R
# Original source SHA-256: c9adf663ff2d7dc82140ada4468be6ef80cf7682f1c18381a16ffc21a92a8f32
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_ENVIRONMENT_ONLY_SENSITIVITY)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
.libPaths(c('<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu',.libPaths()))
suppressPackageStartupMessages({library(biomod2);library(terra)})
a='<NON_NATIVE_ROOT>/audit/environment_only_sensitivity'
x=read.delim(file.path(a,'sensitivity_inventory.tsv'),quote='',stringsAsFactors=FALSE);args=commandArgs(TRUE);taxon=if(length(args))args[1] else 'Abutilon grandifolium';r=x[x$accepted_taxon==taxon,];root=if(taxon=='Acer platanoides')r$clean_full_output_root else r$output_root;setwd(root)
m=readRDS('models.rds');out=rast('ensemble_suitability.tif');stack=rast(readRDS('projection_contract.rds')$stack)[[m@expl.var.names]]
selected=read.csv('selection.csv')$model;ens=if(file.exists('ensemble.rds'))readRDS('ensemble.rds')else NULL;chosen_ensemble=readRDS('writer_contract.rds')$chosen_ensemble
source_lines=readLines(file.path(a,'run_sensitivity814.R'));start=grep('sites=as.data.frame(spatSample',source_lines,fixed=TRUE);end=grep('write.csv(data.frame(x=sites',source_lines,fixed=TRUE)
eval(parse(text=source_lines[start:end]));cat('SAVED_FULL_QC_API_PROBE_PASS delta=',delta,'n=',length(expected),'\n')
runner_sha256=strsplit(system2('sha256sum',file.path(a,'run_sensitivity814.R'),stdout=TRUE),' ')[[1]][1]
jsonlite::write_json(list(taxon=r$accepted_taxon,runner_sha256=runner_sha256,max_independent_delta=delta,sample_count=length(expected),passed=TRUE),file.path(a,'QC_API_PROBE_PASS.json'),auto_unbox=TRUE,pretty=TRUE)
