#!/usr/bin/env Rscript
# Exact native-grid support with footprint-distributed landscape activity.
suppressPackageStartupMessages({library(terra); library(sf); library(jsonlite); library(digest)})
args <- commandArgs(TRUE)
if(length(args)<1L || length(args)>2L) stop("Supply configuration and optional new-chunk limit")
cfg <- fromJSON(args[1],simplifyVector=FALSE)
limit <- if(length(args)==2L) as.integer(args[2]) else Inf
stopifnot(is.finite(cfg$chunk_size),cfg$chunk_size>0,limit>0)
out <- cfg$output_root
dir.create(out,recursive=TRUE,showWarnings=FALSE)
if(file.exists(file.path(out,"polygon_complete.json"))) stop("Completed polygon outputs retained")
terraOptions(tempdir=out,memfrac=.4)
r <- rast(cfg$responses$FULL); e <- rast(cfg$responses$ENV)
stopifnot(is.lonlat(r),compareGeom(r,e,stopOnError=TRUE),nlyr(r)==1L,nlyr(e)==1L,
          abs(res(r)[1]-2.5/60)<1e-10,abs(res(r)[2]-2.5/60)<1e-10)
responses <- c(r,e); names(responses) <- c("FULL","ENV")
v <- vect(cfg$mining_shapefile)
stopifnot(is.lonlat(v),crs(v,proj=TRUE)==crs(r,proj=TRUE))
d <- read.csv(cfg$mining_csv,check.names=FALSE)
stopifnot(cfg$polygon_id_field %in% names(v),!anyDuplicated(d$polygon_id))
ids <- as.character(values(v)[[cfg$polygon_id_field]])
stopifnot(!anyDuplicated(ids),length(ids)==nrow(d),setequal(ids,as.character(d$polygon_id)))
d <- d[match(ids,as.character(d$polygon_id)),]
rc <- grep("^reclamation_area_km2_",names(d),value=TRUE)
dc <- grep("^damage_area_km2_",names(d),value=TRUE)
stopifnot(length(rc)==25L,length(dc)==25L,
          all(is.finite(as.matrix(d[,c(rc,dc)]))),all(as.matrix(d[,c(rc,dc)])>=0),
          all(is.finite(d$area_km2_source)),all(d$area_km2_source>0))
endpoint <- as.numeric(sub(".*_","",rc))
d$reclamation_km2 <- rowSums(d[,rc]); d$damage_km2 <- rowSums(d[,dc])
d$endpoint_area_sum <- as.vector(as.matrix(d[,rc])%*%endpoint)
base <- sub("[.]shp$","",cfg$mining_shapefile,ignore.case=TRUE)
shape_files <- paste0(base,c(".shp",".shx",".dbf",".prj",".cpg"))
stopifnot(all(file.exists(shape_files[1:4])))
inputs <- c(args[1],cfg$mining_csv,shape_files[file.exists(shape_files)],unlist(cfg$responses))
script <- sub("^--file=","",grep("^--file=",commandArgs(FALSE),value=TRUE))
receipt <- list(inputs=lapply(unname(inputs),function(f)list(path=f,sha256=digest(file=f,algo="sha256"))),
                script_sha256=digest(file=script,algo="sha256"))
signature <- digest(receipt,algo="sha256")
# Earlier checkpoints passed the same geometry gates and contain no excess fraction.
legacy_receipt <- receipt
legacy_receipt$script_sha256 <- "d13842f714996f474c7a52f8eb55e1f952c4a2187db894938151401870108fdd"
legacy_signature <- digest(legacy_receipt,algo="sha256")
chunks <- split(seq_len(nrow(d)),ceiling(seq_len(nrow(d))/cfg$chunk_size))
checkpoint_dir <- file.path(out,"checkpoints")
dir.create(checkpoint_dir,showWarnings=FALSE)
new_chunks <- 0L
for(j in seq_along(chunks)) {
  target <- file.path(checkpoint_dir,sprintf("chunk_%05d.rds",j))
  if(file.exists(target)) {
    saved <- readRDS(target)
    stopifnot(saved$signature %in% c(signature,legacy_signature),identical(saved$indices,chunks[[j]]))
    next
  }
  index <- chunks[[j]]; geom <- v[index,]
  valid <- is.valid(geom)
  stopifnot(length(valid)==length(index))
  good <- which(!is.na(valid)&valid)
  skipped <- data.frame(polygon_id=ids[index[setdiff(seq_along(index),good)]],
                         reason=rep("INVALID_OR_UNKNOWN_GEOMETRY",length(index)-length(good)))
  overlap <- data.frame(); corrections <- data.frame()
  if(length(good)) {
    z <- extract(responses,geom[good,],exact=TRUE,cells=TRUE,ID=TRUE)
    stopifnot(all(c("ID","cell","fraction","FULL","ENV") %in% names(z)))
    z <- z[is.finite(z$cell)&is.finite(z$fraction)&z$fraction>0,]
    if(nrow(z)) {
      z$polygon_index <- index[good[z$ID]]
      excessive <- which(z$fraction>1+1e-8)
      if(length(excessive))for(k in excessive) {
        stopifnot(z$fraction[k]<=1+1e-4)
        xy0 <- xyFromCell(r,z$cell[k]); half <- res(r)/2
        box <- matrix(c(xy0[1]-half[1],xy0[2]-half[2],
          xy0[1]+half[1],xy0[2]-half[2],xy0[1]+half[1],xy0[2]+half[2],
          xy0[1]-half[1],xy0[2]+half[2],xy0[1]-half[1],xy0[2]-half[2]),ncol=2,byrow=TRUE)
        polygon <- st_set_crs(st_geometry(st_as_sf(geom[good[z$ID[k]],])),NA)
        cell_polygon <- st_sfc(st_polygon(list(box)))
        corrected <- sum(as.numeric(st_area(suppressWarnings(st_intersection(polygon,cell_polygon)))))/prod(res(r))
        stopifnot(is.finite(corrected),corrected>0,corrected<=1+1e-8,
          abs(corrected-z$fraction[k])<1e-4)
        corrections <- rbind(corrections,data.frame(polygon_id=ids[z$polygon_index[k]],cell=z$cell[k],
          terra_fraction=z$fraction[k],planar_intersection_fraction=corrected))
        z$fraction[k] <- corrected
      }
      stopifnot(!anyDuplicated(paste(z$polygon_index,z$cell)),all(z$fraction<=1+1e-8))
      xy <- xyFromCell(r,z$cell); rad <- pi/180; dy <- res(r)[2]/2
      area <- 6371.0088^2*res(r)[1]*rad*(sin((xy[,2]+dy)*rad)-sin((xy[,2]-dy)*rad))
      overlap <- data.frame(polygon_id=ids[z$polygon_index],polygon_index=z$polygon_index,
        cell=z$cell,fraction=z$fraction,overlap_km2=z$fraction*area,FULL=z$FULL,ENV=z$ENV)
    }
  }
  partial <- paste0(target,".partial")
  saveRDS(list(signature=signature,indices=index,overlap=overlap,excluded=skipped,corrections=corrections),partial)
  stopifnot(file.rename(partial,target))
  new_chunks <- new_chunks+1L
  cat("Checkpoint",j,"of",length(chunks),"; intersections",nrow(overlap),"\n")
  if(new_chunks>=limit && j<length(chunks)) quit(status=0)
}
saved <- lapply(seq_along(chunks),function(j)readRDS(file.path(checkpoint_dir,sprintf("chunk_%05d.rds",j))))
z <- do.call(rbind,lapply(saved,`[[`,"overlap")); excluded <- do.call(rbind,lapply(saved,`[[`,"excluded"))
stopifnot(nrow(z)>0,!anyDuplicated(paste(z$polygon_id,z$cell)))
corrections <- do.call(rbind,lapply(saved,`[[`,"corrections"))
if(!is.null(corrections)&&nrow(corrections))write.csv(corrections,file.path(out,"fraction_precision_corrections.csv"),row.names=FALSE)
write.csv(data.frame(chunk=seq_along(saved),signature=vapply(saved,`[[`,character(1),"signature")),
  file.path(out,"checkpoint_source_signatures.csv"),row.names=FALSE)
write.csv(excluded,file.path(out,"geometry_exclusions.csv"),row.names=FALSE)
unsupported <- data.frame(polygon_id=setdiff(ids,z$polygon_id))
write.csv(unsupported,file.path(out,"unsupported_polygons.csv"),row.names=FALSE)
groups <- split(seq_len(nrow(z)),z$polygon_id)
p <- do.call(rbind,lapply(groups,function(ix) {
  a <- z[ix,]; i <- a$polygon_index[1]; w <- a$overlap_km2/sum(a$overlap_km2)
  joint <- is.finite(a$FULL)&is.finite(a$ENV)
  data.frame(polygon_id=ids[i],country=d$country_name[i],n_cells=nrow(a),
    effective_cells=1/sum(w^2),overlap_area_km2=sum(a$overlap_km2),
    source_footprint_km2=d$area_km2_source[i],joint_finite_area_fraction=sum(w[joint]),
    FULL_mean=if(any(joint))sum(w[joint]*a$FULL[joint])/sum(w[joint]) else NA_real_,
    ENV_mean=if(any(joint))sum(w[joint]*a$ENV[joint])/sum(w[joint]) else NA_real_)
}))
rownames(p) <- NULL
write.csv(p,file.path(out,"polygon_support.csv"),row.names=FALSE)
pi <- match(z$polygon_id,p$polygon_id)
z$allocation_weight <- z$overlap_km2/p$overlap_area_km2[pi]
z$large_supported <- p$n_cells[pi]>=cfg$large_minimum_cells & p$effective_cells[pi]>=cfg$large_minimum_effective_cells
z$country <- d$country_name[z$polygon_index]
for(k in c("footprint_km2","damage_km2","reclamation_km2","endpoint_area_sum")) {
  values <- if(k=="footprint_km2")d$area_km2_source else d[[k]]
  z[[k]] <- values[z$polygon_index]*z$allocation_weight
}
write.csv(z,file.path(out,"native_polygon_overlaps.csv"),row.names=FALSE)
cellsets <- list()
for(scope in c("all_polygon_support","large_polygon_support")) {
  q <- if(scope=="all_polygon_support")z else z[z$large_supported,]
  if(!nrow(q)) next
  q <- do.call(rbind,lapply(split(q,q$cell),function(a) {
    countries <- unique(a$country)
    stopifnot(length(unique(a$FULL[is.finite(a$FULL)]))<=1,
              length(unique(a$ENV[is.finite(a$ENV)]))<=1)
    data.frame(scope=scope,cell=a$cell[1],n_mines=nrow(a),
      country=if(length(countries)==1)countries else "MULTIPLE",n_countries=length(countries),
      footprint_km2=sum(a$footprint_km2),damage_km2=sum(a$damage_km2),
      reclamation_km2=sum(a$reclamation_km2),endpoint_area_sum=sum(a$endpoint_area_sum),
      footprint_support_sum_km2=sum(a$overlap_km2),fraction_sum=sum(a$fraction),
      FULL=a$FULL[1],ENV=a$ENV[1])
  }))
  xy <- xyFromCell(r,q$cell); q$cell_longitude <- xy[,1]; q$cell_latitude <- xy[,2]
  cellsets[[scope]] <- q
}
g <- do.call(rbind,cellsets); rownames(g)<-NULL
write.csv(g,file.path(out,"native_cells.csv"),row.names=FALSE)
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
  precision_corrections=if(is.null(corrections))0L else nrow(corrections),n_polygons=nrow(d),n_supported_polygons=nrow(p),n_excluded_geometry=nrow(excluded),
  exact_native_fraction=TRUE,native_grid_changed=FALSE,shared_cell_duplicates_collapsed=TRUE,
  area_weight_method="Exact longitude-latitude cell fraction multiplied by spherical native-cell area",
  caveat="Mine-period totals allocated uniformly across mine footprint as landscape proxies; repeated transitions possible; no dated patch locations or observed invasion",
  overlapping_footprints="Summed mine support may exceed cell area and is not a union footprint",
  polygon_means="Descriptive only; inferential models use one row per unique native cell"),
  file.path(out,"polygon_complete.json"),pretty=TRUE,auto_unbox=TRUE,digits=NA)
cat("Polygon support calculated; independent checks required.\n")
