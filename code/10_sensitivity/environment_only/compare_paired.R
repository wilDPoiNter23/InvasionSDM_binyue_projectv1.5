#!/usr/bin/env Rscript
# Cell-weighted comparison and exact polygon means of paired suitability outputs.
suppressPackageStartupMessages(library(terra))
if (!requireNamespace('digest',quietly=TRUE)) stop('Install the digest package for SHA256 checksums')
args <- commandArgs(trailingOnly=TRUE)
usage <- 'Usage: Rscript compare_paired.R --manifest FILE --output-dir DIR --cities FILE [--city-id FIELD] [--chunk-rows 128]'
if(length(args)==0L || '--help' %in% args) {cat(usage,'\n'); quit(status=0L)}
if(length(args) %% 2L != 0L || anyDuplicated(args[seq(1,length(args),2)])) stop(usage)
opt <- setNames(as.list(args[seq(2,length(args),2)]),args[seq(1,length(args),2)])
if(!all(c('--manifest','--output-dir','--cities') %in% names(opt)) || any(!names(opt) %in% c('--manifest','--output-dir','--cities','--city-id','--chunk-rows'))) stop(usage)
chunk <- if(is.null(opt[['--chunk-rows']])) 128L else as.integer(opt[['--chunk-rows']])
if(is.na(chunk) || chunk<1L) stop('chunk-rows must be positive')
m <- read.csv(opt[['--manifest']],stringsAsFactors=FALSE)
if(!all(c('species','full_projection','env_projection') %in% names(m)) || nrow(m)==0L || anyNA(m$species) || any(!nzchar(trimws(m$species))) || anyDuplicated(m$species)) stop('Manifest must contain unique paired species')
N <- nrow(m); outdir <- opt[['--output-dir']]
paths <- file.path(outdir,c(sprintf('full_paired_%d_mean_suitability.tif',N),sprintf('env_only_paired_%d_mean_suitability.tif',N),sprintf('paired_%d_valid_species_count.tif',N)))
rs <- lapply(paths,rast)
if(any(vapply(rs,nlyr,numeric(1))!=1L)) stop('Outputs must have one layer')
aligned <- all(vapply(rs,function(r) compareGeom(rs[[1L]],r,stopOnError=FALSE),logical(1)))
if(!aligned) stop('Output grids differ')
finite <- missing <- nonfinite <- totals <- numeric(3L); lows <- rep(Inf,3L); highs <- rep(-Inf,3L)
n <- 0; mf <- me <- vf <- ve <- covar <- absdiff <- sqdiff <- 0
consistent <- TRUE
for(row in seq.int(1L,nrow(rs[[1L]]),by=chunk)) {
  nr <- min(chunk,nrow(rs[[1L]])-row+1L)
  vals <- lapply(rs,function(r) {readStart(r); on.exit(readStop(r)); readValues(r,row=row,nrows=nr,mat=FALSE)})
  for(i in 1:3) {
    x <- vals[[i]]; good <- is.finite(x)
    finite[i] <- finite[i]+sum(good); missing[i] <- missing[i]+sum(is.na(x)); nonfinite[i] <- nonfinite[i]+sum(!is.finite(x) & !is.na(x))
    if(any(good)) {lows[i] <- min(lows[i],x[good]); highs[i] <- max(highs[i],x[good]); totals[i] <- totals[i]+sum(x[good])}
  }
  f <- vals[[1L]]; e <- vals[[2L]]; count <- vals[[3L]]
  if(any(!is.finite(count)) || any(count<0 | count>N | count!=floor(count))) consistent <- FALSE
  positive <- is.finite(count) & count>0
  if(any(is.finite(f)!=positive) || any(is.finite(e)!=positive) || any(!positive & (!is.na(f) | !is.na(e)))) consistent <- FALSE
  good <- is.finite(f) & is.finite(e); f <- f[good]; e <- e[good]; k <- length(f)
  if(k>0L) {
    bf <- mean(f); be <- mean(e); df <- bf-mf; de <- be-me; combined <- n+k
    vf <- vf+sum((f-bf)^2)+df^2*n*k/combined
    ve <- ve+sum((e-be)^2)+de^2*n*k/combined
    covar <- covar+sum((f-bf)*(e-be))+df*de*n*k/combined
    mf <- mf+df*k/combined; me <- me+de*k/combined; n <- combined
    absdiff <- absdiff+sum(abs(e-f)); sqdiff <- sqdiff+sum((e-f)^2)
  }
}
if(!consistent || any(nonfinite>0L) || any(lows[1:2]<0 | highs[1:2]>1)) stop('Output values or paired count consistency failed')
qc <- data.frame(file=basename(paths),nrow=vapply(rs,nrow,numeric(1)),ncol=vapply(rs,ncol,numeric(1)),CRS=vapply(rs,crs,character(1)),resolution_x=vapply(rs,function(r) res(r)[1L],numeric(1)),resolution_y=vapply(rs,function(r) res(r)[2L],numeric(1)),finite_cells=finite,NA_cells=missing,nonfinite_cells=nonfinite,min=ifelse(finite>0,lows,NA),max=ifelse(finite>0,highs,NA),mean=ifelse(finite>0,totals/pmax(1,finite),NA),sha256=vapply(paths,function(p) digest::digest(file=p,algo='sha256'),character(1)),alignment_pass=aligned,count_consistency_pass=consistent)
cities <- vect(opt[['--cities']])
if(!grepl('polygons',geomtype(cities),ignore.case=TRUE) || nrow(cities)==0L) stop('Cities must be nonempty polygons')
if(!nzchar(crs(cities))) stop('City polygons require a CRS')
if(!same.crs(cities,rs[[1L]])) cities <- project(cities,crs(rs[[1L]]))
invalid_before <- sum(!is.valid(cities))
if(invalid_before>0L) cities <- makeValid(cities)
if(any(!is.valid(cities))) stop('City polygons remain invalid after geometric repair')
idfield <- opt[['--city-id']]
if(!is.null(idfield) && !idfield %in% names(cities)) stop('city-id field not found')
ids <- if(is.null(idfield)) seq_len(nrow(cities)) else values(cities)[[idfield]]
# Exact fractions are normalized over finite covered cells separately per polygon.
city_mean <- function(r) {
  vapply(seq_len(nrow(cities)),function(i) {
    z <- extract(r,cities[i,],exact=TRUE,fun=mean,na.rm=TRUE,ID=FALSE)
    x <- z[[1L]][1L]
    if(length(x)==0L || !is.finite(x)) NA_real_ else x
  },numeric(1))
}
cf <- city_mean(rs[[1L]]); ce <- city_mean(rs[[2L]]); ok <- is.finite(cf) & is.finite(ce)
citycor <- if(sum(ok)>1L && sd(cf[ok])>0 && sd(ce[ok])>0) cor(cf[ok],ce[ok]) else NA_real_
citytable <- data.frame(city_id=ids,full_mean=cf,env_mean=ce,mean_difference=ce-cf)
summary <- data.frame(species_count=N,paired_finite_cells=n,full_mean=if(n>0) mf else NA_real_,env_mean=if(n>0) me else NA_real_,mean_difference=if(n>0) me-mf else NA_real_,pearson_correlation=if(n>1 && vf>0 && ve>0) covar/sqrt(vf*ve) else NA_real_,MAE=if(n>0) absdiff/n else NA_real_,RMSE=if(n>0) sqrt(sqdiff/n) else NA_real_,finite_city_pairs=sum(ok),city_pearson_correlation=citycor,city_geometries_repaired=invalid_before)
write.csv(summary,file.path(outdir,sprintf('full_vs_env_only_paired_%d_summary.csv',N)),row.names=FALSE)
write.csv(citytable,file.path(outdir,sprintf('full_vs_env_only_paired_%d_city_comparison.csv',N)),row.names=FALSE)
write.csv(qc,file.path(outdir,sprintf('environment_only_paired_%d_QC.csv',N)),row.names=FALSE)
cat(sprintf('Compared %d paired finite cells and %d city pairs. Mean differences are ENV minus FULL.\n',n,sum(ok)))
