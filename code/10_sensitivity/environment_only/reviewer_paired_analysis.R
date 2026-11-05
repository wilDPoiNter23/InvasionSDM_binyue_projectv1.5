#!/usr/bin/env Rscript
# Review comparisons of frozen paired products and the historical response.
# Input: staged release, final city polygons and an article-specific output root.
# Output: exact cell statistics, city extractions, source tables and diagnostic plots.
suppressPackageStartupMessages({library(terra); library(digest); library(ggplot2)})
a <- commandArgs(TRUE)
if(length(a)!=3L) stop('Usage: reviewer_paired_analysis.R STAGED_RELEASE CITY_POLYGONS OUTPUT_ROOT')
stage <- a[1]; city_path <- a[2]; out <- a[3]
dirs <- setNames(file.path(out,c('tables','figures','qc','logs','model_objects')),c('tables','figures','qc','logs','model_objects'))
for(d in dirs) dir.create(d,recursive=TRUE,showWarnings=FALSE)
if(file.exists(file.path(dirs[['tables']],'raster_comparisons.csv'))) stop('Completed results exist; inspect before rerunning')
terraOptions(tempdir=dirs[['model_objects']],memfrac=0.25,progress=0)
write_table <- function(x,name,folder='tables') write.csv(x,file.path(dirs[[folder]],name),row.names=FALSE,na='')
manifest <- read.csv(file.path(stage,'stage_manifest.csv'),stringsAsFactors=FALSE)
needed <- manifest$asset_group %in% c('03_paired_sensitivity','01_manuscript_response','04_city_geography','13_model_performance')
checks <- manifest[needed,c('asset_group','staged_path','sha256')]
checks$actual_sha256 <- vapply(checks$staged_path,function(p) digest(file=p,algo='sha256'),character(1))
checks$match <- checks$sha256==checks$actual_sha256
write_table(checks,'verified_source_checksums.csv','qc')
if(!all(checks$match)) stop('Input checksum mismatch')
sens <- file.path(stage,'03_paired_sensitivity')
species <- read.csv(file.path(sens,'paired_species_manifest.csv'))
details <- read.csv(file.path(sens,'paired_vs_excluded_species_details.csv'))
stopifnot(nrow(species)==684L,!anyDuplicated(species$species),sum(details$paired_status=='PAIRED')==684L,sum(details$paired_status=='EXCLUDED')==130L)
paths <- c(HISTORICAL_MAIN=file.path(stage,'01_manuscript_response/manuscript_suitability.tif'),
           PAIRED_FULL_684=file.path(sens,'full_paired_684_mean_suitability.tif'),
           PAIRED_ENV_684=file.path(sens,'env_only_paired_684_mean_suitability.tif'))
rs <- lapply(paths,rast); count <- rast(file.path(sens,'paired_684_valid_species_count.tif'))
stopifnot(all(vapply(c(rs,list(count)),function(x) compareGeom(rs[[1]],x,stopOnError=FALSE),logical(1))))
metadata <- data.frame(response=c(names(rs),'PAIRED_VALID_COUNT'),path=c(paths,sources(count)),rows=vapply(c(rs,list(count)),nrow,numeric(1)),columns=vapply(c(rs,list(count)),ncol,numeric(1)),cellsize_degrees=res(count)[1],CRS=crs(count,proj=TRUE))
write_table(metadata,'raster_metadata.csv','qc')
finite_n <- sum(as.numeric(global(is.finite(rs[[2]]) & is.finite(rs[[3]]),'sum',na.rm=TRUE)[1,1]))
full <- env <- numeric(finite_n); k <- 0L
for(row in seq(1L,nrow(count),by=128L)) {
  nr <- min(128L,nrow(count)-row+1L)
  get_values <- function(r){readStart(r); on.exit(readStop(r)); readValues(r,row=row,nrows=nr)}
  f <- get_values(rs[[2]]); e <- get_values(rs[[3]]); c <- get_values(count)
  good <- is.finite(f)&is.finite(e)
  stopifnot(all(good==(c>0)),all(is.finite(c)),all(c>=0 & c<=684 & c==floor(c)))
  n <- sum(good); if(n){ii <- seq.int(k+1,k+n); full[ii] <- f[good]; env[ii] <- e[good]; k <- k+n}
}
stopifnot(k==finite_n,all(full>=0 & full<=1),all(env>=0 & env<=1))
delta <- env-full
summary <- data.frame(reference='PAIRED_FULL_684',comparison='PAIRED_ENV_684',finite_cells=k,pearson=cor(full,env),spearman=cor(full,env,method='spearman'),MAE=mean(abs(delta)),RMSE=sqrt(mean(delta^2)),reference_mean=mean(full),comparison_mean=mean(env),comparison_minus_reference=mean(delta),scale_note='Both products use BIOMOD values divided by 1000 and a shared species-cell denominator')
write_table(summary,'raster_comparisons.csv')
write_table(data.frame(probability=c(0,.01,.025,.05,.25,.5,.75,.95,.975,.99,1),ENV_minus_FULL=as.numeric(quantile(delta,c(0,.01,.025,.05,.25,.5,.75,.95,.975,.99,1)))), 'paired_difference_quantiles.csv')
# Plot source is the complete binned cell density; all finite cells contribute.
bins <- 250L; fi <- pmin(bins,floor(full*bins)+1L); ei <- pmin(bins,floor(env*bins)+1L)
tab <- tabulate((ei-1L)*bins+fi,nbins=bins*bins)
density <- expand.grid(full_bin=seq_len(bins),env_bin=seq_len(bins)); density$count <- tab
density$FULL <- (density$full_bin-.5)/bins; density$ENV <- (density$env_bin-.5)/bins
write_table(density,'paired_density_source.csv')
writeRaster(rs[[3]]-rs[[2]],file.path(dirs[['model_objects']],'paired_env_minus_full.tif'),overwrite=FALSE,wopt=list(datatype='FLT4S',gdal=c('COMPRESS=DEFLATE')))
map_rs <- c(rs[[2]],rs[[3]],rs[[3]]-rs[[2]])
names(map_rs) <- c('PAIRED_FULL_684','PAIRED_ENV_684','ENV_MINUS_FULL')
map <- as.data.frame(aggregate(map_rs,10,fun=mean,na.rm=TRUE),xy=TRUE,na.rm=FALSE)
write_table(map,'paired_map_source_25arcmin.csv')
theme_set(theme_classic(base_size=9,base_family='Arial'))
save_plot <- function(p,name,width=7.2,height=4.2){ggsave(file.path(dirs[['figures']],paste0(name,'.pdf')),p,width=width,height=height,device=cairo_pdf);ggsave(file.path(dirs[['figures']],paste0(name,'.png')),p,width=width,height=height,dpi=300)}
p <- ggplot(density[density$count>0,],aes(FULL,ENV,fill=log10(count)))+geom_tile(width=1/bins,height=1/bins)+geom_abline(slope=1,intercept=0,colour='grey40',linewidth=.35)+scale_fill_viridis_c(name='log10 cells')+coord_equal(xlim=c(0,1),ylim=c(0,1))+labs(x='Paired FULL suitability',y='Paired ENV suitability',title='All finite paired cells',subtitle=paste(format(k,big.mark=','),'cells; bins = 0.004'))
save_plot(p,'paired_cell_density',5.2,4.4)
long <- rbind(data.frame(x=map$x,y=map$y,response='PAIRED_FULL_684',value=map$PAIRED_FULL_684),data.frame(x=map$x,y=map$y,response='PAIRED_ENV_684',value=map$PAIRED_ENV_684))
p <- ggplot(long,aes(x,y,fill=value))+geom_raster()+facet_wrap(~response,ncol=1)+scale_fill_viridis_c(limits=c(0,1),name='Suitability',na.value='white')+coord_equal()+labs(x='Longitude',y='Latitude',subtitle='Display means over 25-arcmin cells; analyses use native 2.5-arcmin cells')
save_plot(p,'paired_common_scale_maps',7.2,6)
p <- ggplot(map,aes(x,y,fill=ENV_MINUS_FULL))+geom_raster()+scale_fill_gradient2(low='#2166AC',mid='white',high='#B2182B',midpoint=0,name='ENV - FULL',na.value='white')+coord_equal()+labs(x='Longitude',y='Latitude',subtitle='Display means over 25-arcmin cells')
save_plot(p,'paired_difference_map',7.2,3.6)
rm(full,env,delta,fi,ei); gc()
cities <- vect(city_path); invalid <- sum(!is.valid(cities)); if(invalid) cities <- makeValid(cities)
stopifnot(nrow(cities)==108L,!anyDuplicated(cities$shapeName),all(is.valid(cities)))
if(!same.crs(cities,count)) cities <- project(cities,crs(count))
city <- data.frame(city=cities$shapeName,region=if('shapeGroup' %in% names(cities)) cities$shapeGroup else NA_character_)
extract_one <- function(r){vapply(seq_len(nrow(cities)),function(i) as.numeric(extract(r,cities[i,],exact=TRUE,fun=mean,na.rm=TRUE,ID=FALSE)[1,1]),numeric(1))}
for(i in names(rs)) city[[i]] <- extract_one(rs[[i]])
city$env_minus_full <- city$PAIRED_ENV_684-city$PAIRED_FULL_684
city$valid_species_count <- extract_one(count)
city$pixel_count <- vapply(seq_len(nrow(cities)),function(i){z=extract(c(rs[[2]],rs[[3]]),cities[i,],exact=TRUE,ID=FALSE);sum(is.finite(z[[1]])&is.finite(z[[2]]))},numeric(1))
write_table(city,'three_response_city_extraction.csv')
legacy <- read.csv(file.path(sens,'full_vs_env_only_paired_684_city_comparison.csv'))
idx <- match(city$city,legacy$city_id); stopifnot(!anyNA(idx))
cityqc <- data.frame(city=city$city,full_abs_error=abs(city$PAIRED_FULL_684-legacy$full_mean[idx]),env_abs_error=abs(city$PAIRED_ENV_684-legacy$env_mean[idx]))
write_table(cityqc,'city_extraction_reproduction.csv','qc')
stopifnot(max(cityqc$full_abs_error)<1e-9,max(cityqc$env_abs_error)<1e-9)
write_table(data.frame(check=c('frozen_species','excluded_species','paired_finite_cells','city_geometries_repaired','city_count','maximum_city_reproduction_error'),value=c(684,130,k,invalid,nrow(city),max(cityqc$full_abs_error,cityqc$env_abs_error))), 'paired_acceptance_checks.csv','qc')
# Attrition is descriptive; historical evaluation values are not matched refit evaluations.
metrics <- c('occurrence_record_count','full_best_TSS','full_best_ROC','baseline_predictor_count','matched_full_predictor_count')
attrition <- do.call(rbind,lapply(metrics,function(metric){x=as.numeric(details[[metric]][details$paired_status=='PAIRED']);y=as.numeric(details[[metric]][details$paired_status=='EXCLUDED']); x=x[is.finite(x)];y=y[is.finite(y)];pooled=sqrt((var(x)+var(y))/2);data.frame(metric=metric,paired_available=length(x),excluded_available=length(y),paired_mean=mean(x),excluded_mean=mean(y),paired_median=median(x),excluded_median=median(y),standardized_mean_difference=(mean(x)-mean(y))/pooled,scope='Historical FULL metadata and inventory vectors; attrition diagnostic only')}))
write_table(attrition,'paired_excluded_attrition.csv'); write_table(details,'paired_excluded_source.csv')
write_table(data.frame(stage=c('P0.1-P0.2','P0.3','P0.4','P0.5','P0.7'),status=c('NUMERICALLY_VERIFIED','BLOCKED','DESCRIPTIVE_COMPLETED','NUMERICALLY_REPRODUCED','BLOCKED'),reason=c('Exact correlations and differences; unchanged frozen products','Accepted ENV and CLEAN_FULL selected-route evaluation records are not staged locally','No new traits; no inferential elimination-of-bias claim','Exact coverage fractions reproduce accepted paired city means','Only five updated-city zone geometries are version-bound; legacy 108/103 zone vectors need provenance reconciliation')), 'analysis_scope_status.csv','qc')
capture.output(sessionInfo(),file=file.path(dirs[['logs']],'sessionInfo.txt'))
cat('Verified paired raster comparisons and final-city extractions. Performance and full-zone lineage remain explicit blockers.\n')
quit(save='no',status=0)
