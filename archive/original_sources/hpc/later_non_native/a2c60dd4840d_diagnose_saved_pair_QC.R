# Original source: diagnose_saved_pair_QC.R
# Original source location: ${NON_NATIVE_ROOT}/audit/environment_only_sensitivity/diagnose_saved_pair_QC.R
# Original source SHA-256: a2c60dd4840dd5bebf0dbce11986f64f0e417094e1da56343261e3ed0d84f0ba
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_ENVIRONMENT_ONLY_SENSITIVITY)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
.libPaths(c('<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu',.libPaths()))
suppressPackageStartupMessages({library(biomod2);library(terra)})
a='<NON_NATIVE_ROOT>/audit/environment_only_sensitivity'
x=read.delim(file.path(a,'sensitivity_inventory.tsv'),quote='',stringsAsFactors=FALSE);r=x[x$accepted_taxon=='Acer platanoides',];root=r$clean_full_output_root;setwd(root)
m=readRDS('models.rds');out=rast('ensemble_suitability.tif');pc=readRDS('projection_contract.rds');stack=rast(pc$stack)[[m@expl.var.names]];selected=read.csv('selection.csv')$model;ens=NULL
lines=readLines(file.path(a,'run_sensitivity814.R'));start=grep('sites=as.data.frame(spatSample',lines,fixed=TRUE);end=grep('write.csv(data.frame(x=sites',lines,fixed=TRUE)
code=lines[start:end];code=gsub('stopifnot(is.finite(delta),delta<=1.5)','print(delta)',code,fixed=TRUE);eval(parse(text=code));print(head(data.frame(expected,observed),12));print(selected)
files=list.files(file.path(root,m@sp.name,paste0('proj_',pc$proj_name),'individual_projections'),pattern='\\.tif$',full.names=TRUE)
for(n in objects){direct=as.numeric(predict(get(n),newdata=newdata,on_0_1000=TRUE));f=files[basename(files)==paste0('proj_',pc$proj_name,'_',n,'.tif')];v=terra::extract(rast(f),as.matrix(sites))[,1];print(data.frame(model=n,delta=max(abs(round(direct)-v)),direct_min=min(direct),direct_max=max(direct),raster_min=min(v),raster_max=max(v)))}
print(args(biomod2:::predict2));showMethods('predict2')
