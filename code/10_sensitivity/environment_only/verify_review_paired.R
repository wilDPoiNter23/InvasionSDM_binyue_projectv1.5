#!/usr/bin/env Rscript
# Independent numerical checks of saved review products against frozen sources.
suppressPackageStartupMessages({library(terra);library(digest)})
a=commandArgs(TRUE)
if(length(a)!=3L) stop('Usage: verify_review_paired.R STAGED_RELEASE PAIRED_OUTPUT CONTRAST_OUTPUT')
stage=a[1];out=a[2];contrast=a[3]
terraOptions(tempdir=file.path(out,'model_objects'),memfrac=.2,progress=0)
checks=read.csv(file.path(out,'qc','verified_source_checksums.csv'))
stopifnot(all(checks$match),all(vapply(checks$staged_path,function(p)digest(file=p,algo='sha256'),character(1))==checks$sha256))
new=read.csv(file.path(out,'tables','raster_comparisons.csv'));old=read.csv(file.path(stage,'03_paired_sensitivity/full_vs_env_only_paired_684_summary.csv'))
errs=c(pearson=abs(new$pearson-old$pearson_correlation),MAE=abs(new$MAE-old$MAE),RMSE=abs(new$RMSE-old$RMSE),full_mean=abs(new$reference_mean-old$full_mean),env_mean=abs(new$comparison_mean-old$env_mean))
stopifnot(all(errs<1e-10),new$finite_cells==old$paired_finite_cells)
density=read.csv(file.path(out,'tables','paired_density_source.csv'))
stopifnot(sum(density$count)==new$finite_cells,all(density$count>=0))
city=read.csv(file.path(out,'tables','three_response_city_extraction.csv'));cityqc=read.csv(file.path(out,'qc','city_extraction_reproduction.csv'))
stopifnot(nrow(city)==108,!anyDuplicated(city$city),max(cityqc$full_abs_error,cityqc$env_abs_error)<1e-9)
full=rast(file.path(stage,'03_paired_sensitivity/full_paired_684_mean_suitability.tif'));env=rast(file.path(stage,'03_paired_sensitivity/env_only_paired_684_mean_suitability.tif'));delta=rast(file.path(out,'model_objects','paired_env_minus_full.tif'))
stopifnot(compareGeom(full,delta,stopOnError=FALSE))
set.seed(20261014); cells=sample.int(ncell(full),1000L);f=extract(full,cells)[,1];e=extract(env,cells)[,1];d=extract(delta,cells)[,1]
ok=is.finite(f)&is.finite(e);stopifnot(all(is.finite(d)==ok),max(abs(d[ok]-(e-f)[ok]))<1e-7)
tab=read.csv(file.path(contrast,'tables','three_response_city_background_source.csv'));stats=read.csv(file.path(contrast,'tables','city_background_statistics.csv'))
for(i in seq_len(nrow(stats))){z=tab[tab$response==stats$response[i],];x=z$city_mean;y=z[[stats$baseline[i]]];good=is.finite(x)&is.finite(y);x=x[good];y=y[good];stopifnot(length(x)==stats$n_cities[i],abs(mean(x-y)-stats$mean_difference[i])<1e-12,abs(100*(mean(x)/mean(y)-1)-stats$percent_difference[i])<1e-10)}
result=data.frame(check=c(names(errs),'density_cell_total','paired_city_mean_reproduction','difference_raster_sample_max_error','city_background_source_recalculation'),value=c(errs,sum(density$count),max(cityqc$full_abs_error,cityqc$env_abs_error),max(abs(d[ok]-(e-f)[ok])),nrow(stats)),status='PASS')
write.csv(result,file.path(out,'qc','independent_saved_product_checks.csv'),row.names=FALSE)
cat('Saved product verification PASS; independent source-summary, density, city and difference-raster checks.\n')
quit(save='no',status=0)
