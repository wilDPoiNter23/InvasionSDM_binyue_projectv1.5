#!/usr/bin/env Rscript
# Paired continuous suitability means; BIOMOD values are divided by 1000.
suppressPackageStartupMessages(library(terra))
args <- commandArgs(trailingOnly = TRUE)
usage <- 'Usage: Rscript aggregate_paired.R --manifest FILE --output-dir DIR [--chunk-rows 128] [--workers 1]'
if (length(args) == 0L || '--help' %in% args) { cat(usage, '\n'); quit(status = 0L) }
if (length(args) %% 2L != 0L || anyDuplicated(args[seq(1,length(args),2)])) stop(usage)
opt <- setNames(as.list(args[seq(2,length(args),2)]), args[seq(1,length(args),2)])
if (!all(c('--manifest','--output-dir') %in% names(opt)) || any(!names(opt) %in% c('--manifest','--output-dir','--chunk-rows','--workers'))) stop(usage)
chunk <- if (is.null(opt[['--chunk-rows']])) 128L else as.integer(opt[['--chunk-rows']])
if (is.na(chunk) || chunk < 1L) stop('chunk-rows must be positive')
workers <- if (is.null(opt[['--workers']])) 1L else suppressWarnings(as.integer(opt[['--workers']]))
if (is.na(workers) || workers < 1L) stop('workers must be a positive integer')
cpu_limit <- Sys.getenv('SLURM_CPUS_PER_TASK',unset='')
if (nzchar(cpu_limit)) {
  cpu_limit <- suppressWarnings(as.integer(cpu_limit))
  if (is.na(cpu_limit) || cpu_limit < 1L || workers > cpu_limit) stop('workers must not exceed SLURM_CPUS_PER_TASK')
}
manifest_path <- normalizePath(opt[['--manifest']], mustWork=TRUE)
m <- read.csv(manifest_path, stringsAsFactors=FALSE, check.names=FALSE)
if (!all(c('species','full_projection','env_projection') %in% names(m)) || nrow(m)==0L) stop('Manifest requires species, full_projection and env_projection')
if (anyNA(m$species) || any(!nzchar(trimws(m$species))) || anyDuplicated(m$species)) stop('Species must be unique and nonempty')
resolve <- function(x) {
  if (anyNA(x) || any(!nzchar(trimws(x)))) stop('Projection paths must be nonempty')
  absolute <- grepl('^(/|[A-Za-z]:[/\\\\]|\\\\\\\\)',x)
  x[!absolute] <- file.path(dirname(manifest_path),x[!absolute])
  normalizePath(x,mustWork=TRUE)
}
paths <- c(resolve(m$full_projection),resolve(m$env_projection))
if (any(grepl('TSSbin|TSSfilt|binary|(^|[_.-])bin([_.-]|$)',basename(paths),ignore.case=TRUE))) stop('Binary projection filenames are not accepted')
inputs <- lapply(paths,rast)
if (any(vapply(inputs,nlyr,numeric(1)) != 1L)) stop('Each input must have one layer')
reference <- inputs[[1L]]
for (r in inputs) if (!compareGeom(reference,r,stopOnError=FALSE)) stop('Projection grids differ')
N <- nrow(m); workers <- min(workers,N); outdir <- opt[['--output-dir']]
dir.create(outdir,recursive=TRUE,showWarnings=FALSE)
filenames <- file.path(outdir,c(sprintf('full_paired_%d_mean_suitability.tif',N),sprintf('env_only_paired_%d_mean_suitability.tif',N),sprintf('paired_%d_valid_species_count.tif',N)))
if (any(file.exists(filenames))) stop('Output files already exist; choose a new output directory')
outputs <- lapply(seq_len(3L),function(i) rast(reference))
started <- logical(3L); completed <- FALSE; cluster <- NULL
# Each process opens only its species group; raster objects remain process-local.
initialize_group <- function(group,full_paths,env_paths) {
  suppressPackageStartupMessages(library(terra))
  local_inputs <- list(full=lapply(full_paths[group],terra::rast),env=lapply(env_paths[group],terra::rast),keep_open=length(group)<=128L)
  # Larger groups cache metadata and open each pair only while reading.
  if (local_inputs$keep_open) for (r in c(local_inputs$full,local_inputs$env)) terra::readStart(r)
  assign('.paired_inputs',local_inputs,envir=.GlobalEnv)
  invisible(NULL)
}
close_group <- function() {
  if (exists('.paired_inputs',envir=.GlobalEnv,inherits=FALSE)) {
    cache <- get('.paired_inputs',envir=.GlobalEnv)
    if (cache$keep_open) for (r in c(cache$full,cache$env)) try(terra::readStop(r),silent=TRUE)
    rm('.paired_inputs',envir=.GlobalEnv)
  }
  invisible(NULL)
}
sum_group <- function(row,nr,nc) {
  cache <- get('.paired_inputs',envir=.GlobalEnv)
  sf <- se <- numeric(nr*nc); count <- integer(nr*nc)
  read_chunk <- function(r) {
    if (!cache$keep_open) { terra::readStart(r); on.exit(terra::readStop(r)) }
    terra::readValues(r,row=row,nrows=nr,mat=FALSE)
  }
  for (i in seq_along(cache$full)) {
    f <- read_chunk(cache$full[[i]])
    e <- read_chunk(cache$env[[i]])
    if (any(is.finite(f) & (f<0 | f>1000)) || any(is.finite(e) & (e<0 | e>1000))) stop('Finite input values must be in BIOMOD range 0..1000')
    ok <- is.finite(f) & is.finite(e)
    sf[ok] <- sf[ok]+f[ok]/1000; se[ok] <- se[ok]+e[ok]/1000; count[ok] <- count[ok]+1L
  }
  list(full=sf,env=se,count=count)
}

cleanup <- function() {
  if (!is.null(cluster)) {
    try(parallel::clusterCall(cluster,close_group),silent=TRUE)
    try(parallel::stopCluster(cluster),silent=TRUE)
  } else close_group()
  for (i in which(started)) try(writeStop(outputs[[i]]),silent=TRUE)
  if (!completed) unlink(filenames)
}
tryCatch({
  if (workers>1L) {
    cluster <- parallel::makePSOCKcluster(workers)
    groups <- split(seq_len(N),rep(seq_len(workers),length.out=N))
    parallel::clusterMap(cluster,initialize_group,groups,
                         MoreArgs=list(full_paths=paths[seq_len(N)],env_paths=paths[N+seq_len(N)]))
  } else initialize_group(seq_len(N),paths[seq_len(N)],paths[N+seq_len(N)])
  rm(inputs)
  cat(sprintf('Aggregating %d species with %d worker processes.\n',N,workers)); flush.console()
  for (i in seq_len(3L)) {
    writeStart(outputs[[i]],filename=filenames[i],overwrite=FALSE,wopt=list(datatype=if(i==3L) 'INT4S' else 'FLT8S',gdal=c('COMPRESS=DEFLATE','BIGTIFF=IF_SAFER')))
    started[i] <- TRUE
  }
  for (row in seq.int(1L,nrow(reference),by=chunk)) {
    nr <- min(chunk,nrow(reference)-row+1L); len <- nr*ncol(reference)
    partial <- if (workers>1L) parallel::clusterCall(cluster,sum_group,row,nr,ncol(reference)) else list(sum_group(row,nr,ncol(reference)))
    sf <- Reduce(`+`,lapply(partial,`[[`,'full'))
    se <- Reduce(`+`,lapply(partial,`[[`,'env'))
    count <- Reduce(`+`,lapply(partial,`[[`,'count'))
    rm(partial)
    ok <- count>0L; sf[ok] <- sf[ok]/count[ok]; se[ok] <- se[ok]/count[ok]
    sf[!ok] <- NA_real_; se[!ok] <- NA_real_
    writeValues(outputs[[1L]],sf,row,nr); writeValues(outputs[[2L]],se,row,nr); writeValues(outputs[[3L]],count,row,nr)
    cat(sprintf("Completed rows %d-%d of %d.\n",row,row+nr-1L,nrow(reference))); flush.console()
  }
  for (i in seq_len(3L)) { writeStop(outputs[[i]]); started[i] <- FALSE }
  completed <- TRUE
},finally=cleanup())
cat(sprintf('Wrote paired means and valid-species counts for %d species.\n',N))
