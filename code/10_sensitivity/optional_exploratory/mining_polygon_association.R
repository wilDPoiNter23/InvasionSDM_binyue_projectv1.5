#!/usr/bin/env Rscript
# Fit checked native-cell tables without repeating polygon extraction.
suppressPackageStartupMessages({library(terra);library(jsonlite);library(digest)})
args <- commandArgs(TRUE)
stopifnot(length(args)==1L)
cfg <- fromJSON(args[1],simplifyVector=FALSE);out <- cfg$output_root
if(file.exists(file.path(out,"polygon_complete.json")))stop("Completed result retained")
r <- rast(cfg$responses$FULL)
g <- read.csv(file.path(out,"native_cells.csv"),check.names=FALSE)
p <- read.csv(file.path(out,"polygon_support.csv"),check.names=FALSE)
d <- read.csv(cfg$mining_csv,check.names=FALSE)
excluded <- read.csv(file.path(out,"geometry_exclusions.csv"))
correction_file <- file.path(out,"fraction_precision_corrections.csv")
corrections <- if(file.exists(correction_file))read.csv(correction_file) else NULL
base <- sub("[.]shp$","",cfg$mining_shapefile,ignore.case=TRUE)
shape_files <- paste0(base,c(".shp",".shx",".dbf",".prj",".cpg"))
extraction_configuration <- if(is.null(cfg$extraction_configuration))args[1] else cfg$extraction_configuration
inputs <- c(extraction_configuration,cfg$mining_csv,shape_files[file.exists(shape_files)],unlist(cfg$responses))
receipt <- list(inputs=lapply(unname(inputs),function(f)list(path=f,sha256=digest(file=f,algo="sha256"))),script_sha256="08e2e6c3f0680a8d3bafb6f70a1aad5d5ef36d6f06113aacce3aa85c7a65c5e1")
signature <- digest(receipt,algo="sha256")
legacy_receipt <- receipt;legacy_receipt$script_sha256 <- "d13842f714996f474c7a52f8eb55e1f952c4a2187db894938151401870108fdd"
legacy_signature <- digest(legacy_receipt,algo="sha256")
checkpoint_directory <- if(is.null(cfg$extraction_checkpoint_directory))file.path(out,"checkpoints") else cfg$extraction_checkpoint_directory
paths <- list.files(checkpoint_directory,pattern="^chunk_[0-9]+[.]rds$",full.names=TRUE)
stopifnot(length(paths)==ceiling(nrow(d)/cfg$chunk_size))
for(i in seq_along(paths)) {
 q <- readRDS(paths[i]);stopifnot(q$signature %in% c(signature,legacy_signature),
   identical(q$indices,seq.int((i-1L)*cfg$chunk_size+1L,min(i*cfg$chunk_size,nrow(d)))))
}
receipt$table_sha256 <- setNames(lapply(c("native_cells.csv","polygon_support.csv","native_polygon_overlaps.csv"),
 function(n)digest(file=file.path(out,n),algo="sha256")),c("native_cells","polygon_support","overlaps"))
estimates <- list(); omissions <- list()
for(scope in unique(g$scope)) for(model in c("reclamation_amount","reclamation_timing")) {
  q <- g[g$scope==scope & g$n_countries==1 & is.finite(g$FULL)&is.finite(g$ENV),]
  if(model=="reclamation_timing") q <- q[q$reclamation_km2>0,]
  nc <- table(q$country); q <- q[q$country %in% names(nc)[nc>=cfg$minimum_cells_per_country],]
  if(nrow(q)<100) {
    omissions[[paste(scope,model)]] <- data.frame(scope=scope,model=model,reason="FEWER_THAN_100_SUPPORTED_CELLS")
    next
  }
  lat <- q$cell_latitude*pi/180; lon <- q$cell_longitude*pi/180
  X <- cbind(log1p_reclamation_km2=log1p(q$reclamation_km2),
    log1p_damage_km2=log1p(q$damage_km2),log1p_footprint_km2=log1p(q$footprint_km2),
    latitude_scaled=lat/(pi/2),latitude_squared=(lat/(pi/2))^2,
    sphere_x=cos(lat)*cos(lon),sphere_y=cos(lat)*sin(lon))
  if(model=="reclamation_timing") X <- cbind(X,endpoint_per_decade=(q$endpoint_area_sum/q$reclamation_km2-2000)/10)
  bycountry <- split(seq_len(nrow(q)),q$country); xd <- X
  for(ix in bycountry) xd[ix,] <- sweep(X[ix,,drop=FALSE],2,colMeans(X[ix,,drop=FALSE]))
  stopifnot(qr(xd)$rank==ncol(xd)); bread <- solve(crossprod(xd))
  for(response in c("FULL","ENV")) {
    y <- q[[response]]; yd <- y
    for(ix in bycountry) yd[ix] <- y[ix]-mean(y[ix])
    beta <- as.vector(qr.solve(xd,yd)); residual <- as.vector(yd-xd%*%beta)
    for(blocksize in unlist(cfg$spatial_block_degrees)) {
      block <- paste(floor((q$cell_longitude+180)/blocksize),floor((q$cell_latitude+90)/blocksize),sep="_")
      score <- rowsum(xd*residual,block,reorder=FALSE)
      G <- nrow(score); N <- nrow(q); K <- ncol(xd)+length(bycountry)
      stopifnot(G>=20,N>K)
      vc <- bread%*%crossprod(score)%*%bread * G/(G-1)*(N-1)/(N-K)
      se <- sqrt(pmax(0,diag(vc))); critical <- qt(.975,G-1)
      estimates[[paste(scope,model,response,blocksize)]] <- data.frame(scope=scope,model=model,response=response,
        term_index=seq_along(beta),term=colnames(X),estimate=beta,cluster_se=se,
        ci_low=beta-critical*se,ci_high=beta+critical*se,n_cells=N,n_countries=length(bycountry),
        spatial_block_degrees=blocksize,n_spatial_blocks=G,status="EXPLORATORY_STATIC_SUITABILITY_ASSOCIATION")
    }
  }
}
stopifnot(length(estimates)>0)
write.csv(do.call(rbind,estimates),file.path(out,"model_estimates.csv"),row.names=FALSE)
if(length(omissions)) write.csv(do.call(rbind,omissions),file.path(out,"model_omissions.csv"),row.names=FALSE)
writeLines(capture.output(sessionInfo()),file.path(out,"sessionInfo.txt"))
grid <- list(xmin=xmin(r),xmax=xmax(r),ymin=ymin(r),ymax=ymax(r),xres=res(r)[1],yres=res(r)[2],ncol=ncol(r),nrow=nrow(r),crs=crs(r))
write_json(list(status="POLYGON_SUPPORT_CALCULATED_PENDING_INDEPENDENT_CHECK",grid=grid,receipt=receipt,
  checkpoint_signature=signature,compatible_previous_signature=legacy_signature,
  precision_corrections=if(is.null(corrections))0L else nrow(corrections),n_polygons=nrow(d),n_supported_polygons=nrow(p),n_excluded_geometry=nrow(excluded),association_only_recovery=TRUE,
  association_writer_sha256=digest(file=sub("^--file=","",grep("^--file=",commandArgs(FALSE),value=TRUE)),algo="sha256"),
  exact_native_fraction=TRUE,native_grid_changed=FALSE,shared_cell_duplicates_collapsed=TRUE,
  area_weight_method="Exact longitude-latitude cell fraction multiplied by spherical native-cell area",
  caveat="Mine-period totals allocated uniformly across mine footprint as landscape proxies; repeated transitions possible; no dated patch locations or observed invasion",
  overlapping_footprints="Summed mine support may exceed cell area and is not a union footprint",
  polygon_means="Descriptive only; inferential models use one row per unique native cell"),
  file.path(out,"polygon_complete.json"),pretty=TRUE,auto_unbox=TRUE,digits=NA)
cat("Polygon support calculated; independent checks required.\n")
