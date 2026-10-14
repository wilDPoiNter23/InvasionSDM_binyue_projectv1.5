#!/usr/bin/env Rscript
# Screen mining reclamation at the native suitability-grid scale.
suppressPackageStartupMessages({library(terra); library(jsonlite); library(digest)})
args <- commandArgs(TRUE)
if(length(args)!=1L) stop("Supply a private JSON configuration")
cfg <- fromJSON(args[1],simplifyVector=FALSE)
out <- cfg$output_root
dir.create(out,recursive=TRUE,showWarnings=FALSE)
if(file.exists(file.path(out,"screen_complete.json"))) stop("Completed output retained")
terraOptions(tempdir=out,memfrac=.4)
d <- read.csv(cfg$mining_csv,check.names=FALSE)
stopifnot(!anyDuplicated(d$polygon_id),all(is.finite(d$area_km2_source)),
          all(d$area_km2_source>0),all(is.finite(d$centroid_latitude_deg)),
          all(is.finite(d$centroid_longitude_deg)))
rc <- grep("^reclamation_area_km2_",names(d),value=TRUE)
dc <- grep("^damage_area_km2_",names(d),value=TRUE)
stopifnot(length(rc)==25L,length(dc)==25L,all(as.matrix(d[,c(rc,dc)])>=0))
endpoint <- as.numeric(sub(".*_","",rc))
r <- rast(cfg$responses$FULL); e <- rast(cfg$responses$ENV)
stopifnot(is.lonlat(r),compareGeom(r,e,stopOnError=TRUE),nlyr(r)==1,nlyr(e)==1)
xy <- cbind(d$centroid_longitude_deg,d$centroid_latitude_deg)
cell <- cellFromXY(r,xy)
centre <- xyFromCell(r,cell)
vfull <- extract(r,xy)[,1]; venv <- extract(e,xy)[,1]
stopifnot(all(vfull[is.finite(vfull)]>=0 & vfull[is.finite(vfull)]<=1),
          all(venv[is.finite(venv)]>=0 & venv[is.finite(venv)]<=1))
rad <- pi/180; dy <- res(r)[2]/2
# Spherical cell area is a screening approximation, not polygon intersection.
cellarea <- 6371.0088^2*res(r)[1]*rad*(sin((centre[,2]+dy)*rad)-sin((centre[,2]-dy)*rad))
p <- data.frame(polygon_id=d$polygon_id,country=d$country_name,
  longitude=xy[,1],latitude=xy[,2],cell=cell,
  footprint_km2=d$area_km2_source,damage_km2=rowSums(d[,dc]),
  reclamation_km2=rowSums(d[,rc]),
  endpoint_area_sum=as.vector(as.matrix(d[,rc])%*%endpoint),
  cell_area_approx_km2=cellarea,
  compact=d$area_km2_source<=cfg$maximum_mine_cell_area_ratio*cellarea,
  FULL=vfull,ENV=venv)
write.csv(p,file.path(out,"mine_points.csv"),row.names=FALSE)
sets <- list(all_centroids=p[!is.na(p$cell),],
  compact_centroids=p[!is.na(p$cell)&!is.na(p$compact)&p$compact,])
cellsets <- lapply(names(sets),function(scope) {
  z <- sets[[scope]]
  ans <- lapply(split(z,z$cell),function(a) {
    countries <- unique(a$country)
    data.frame(scope=scope,cell=a$cell[1],n_mines=nrow(a),
      country=if(length(countries)==1)countries else "MULTIPLE",
      n_countries=length(countries),footprint_km2=sum(a$footprint_km2),
      damage_km2=sum(a$damage_km2),reclamation_km2=sum(a$reclamation_km2),
      endpoint_area_sum=sum(a$endpoint_area_sum),FULL=a$FULL[1],ENV=a$ENV[1])
  })
  ans <- do.call(rbind,ans)
  cxy <- xyFromCell(r,ans$cell)
  ans$cell_longitude <- cxy[,1]; ans$cell_latitude <- cxy[,2]
  ans
})
g <- do.call(rbind,cellsets); rownames(g)<-NULL
write.csv(g,file.path(out,"native_cells.csv"),row.names=FALSE)
estimates <- list(); descriptions <- list()
for(scope in unique(g$scope)) for(model in c("reclamation_amount","reclamation_timing")) {
  q <- g[g$scope==scope & g$n_countries==1 & is.finite(g$FULL)&is.finite(g$ENV),]
  if(model=="reclamation_timing") q <- q[q$reclamation_km2>0,]
  nc <- table(q$country)
  q <- q[q$country %in% names(nc)[nc>=cfg$minimum_cells_per_country],]
  if(nrow(q)<100) stop("Insufficient supported cells for declared models")
  lat <- q$cell_latitude*pi/180; lon <- q$cell_longitude*pi/180
  X <- cbind(log1p_reclamation_km2=log1p(q$reclamation_km2),
    log1p_damage_km2=log1p(q$damage_km2),log1p_footprint_km2=log1p(q$footprint_km2),
    latitude_scaled=lat/(pi/2),latitude_squared=(lat/(pi/2))^2,
    sphere_x=cos(lat)*cos(lon),sphere_y=cos(lat)*sin(lon))
  if(model=="reclamation_timing") X <- cbind(X,
    endpoint_per_decade=(q$endpoint_area_sum/q$reclamation_km2-2000)/10)
  bycountry <- split(seq_len(nrow(q)),q$country)
  xd <- X
  for(ix in bycountry) xd[ix,] <- sweep(X[ix,,drop=FALSE],2,colMeans(X[ix,,drop=FALSE]))
  stopifnot(qr(xd)$rank==ncol(xd))
  bread <- solve(crossprod(xd))
  for(response in c("FULL","ENV")) {
    y <- q[[response]]; yd <- y
    for(ix in bycountry) yd[ix] <- y[ix]-mean(y[ix])
    beta <- as.vector(qr.solve(xd,yd)); residual <- as.vector(yd-xd%*%beta)
    descriptions[[paste(scope,model,response)]] <- data.frame(scope=scope,model=model,response=response,
      n_cells=nrow(q),n_countries=length(bycountry),
      mean_suitability=mean(y),
      unadjusted_spearman_amount=cor(log1p(q$reclamation_km2),y,method="spearman"))
    for(blocksize in unlist(cfg$spatial_block_degrees)) {
      block <- paste(floor((q$cell_longitude+180)/blocksize),floor((q$cell_latitude+90)/blocksize),sep="_")
      score <- rowsum(xd*residual,block,reorder=FALSE)
      G <- nrow(score); N <- nrow(q); K <- ncol(xd)+length(bycountry)
      stopifnot(G>=20,N>K)
      vc <- bread%*%crossprod(score)%*%bread * G/(G-1)*(N-1)/(N-K)
      se <- sqrt(pmax(0,diag(vc))); critical <- qt(.975,G-1)
      estimates[[paste(scope,model,response,blocksize)]] <- data.frame(scope=scope,model=model,
        response=response,term_index=seq_along(beta),term=colnames(X),estimate=beta,
        cluster_se=se,ci_low=beta-critical*se,ci_high=beta+critical*se,
        n_cells=N,n_countries=length(bycountry),spatial_block_degrees=blocksize,
        n_spatial_blocks=G,status="EXPLORATORY_STATIC_SUITABILITY_ASSOCIATION")
    }
  }
}
write.csv(do.call(rbind,estimates),file.path(out,"model_estimates.csv"),row.names=FALSE)
write.csv(do.call(rbind,descriptions),file.path(out,"descriptive_associations.csv"),row.names=FALSE)
coverage <- lapply(split(g,g$scope),function(z)data.frame(scope=z$scope[1],n_cells=nrow(z),
  n_joint_finite=sum(is.finite(z$FULL)&is.finite(z$ENV)),
  mixed_country_cells=sum(z$n_countries>1),n_mines=sum(z$n_mines)))
write.csv(do.call(rbind,coverage),file.path(out,"coverage.csv"),row.names=FALSE)
writeLines(capture.output(sessionInfo()),file.path(out,"sessionInfo.txt"))
grid <- list(xmin=xmin(r),xmax=xmax(r),ymin=ymin(r),ymax=ymax(r),
  xres=res(r)[1],yres=res(r)[2],ncol=ncol(r),nrow=nrow(r),crs=crs(r))
inputs <- c(cfg$mining_csv,unlist(cfg$responses))
write_json(list(status="SCREEN_CALCULATED_PENDING_INDEPENDENT_CHECK",grid=grid,
  configuration_sha256=digest(file=args[1],algo="sha256"),
  inputs=lapply(inputs,function(f)list(path=f,sha256=digest(file=f,algo="sha256"))),
  native_grid_changed=FALSE,exact_polygon_extraction=FALSE,
  shared_cell_duplicates_collapsed=TRUE,observed_invasion_response=FALSE,
  temporal_scope="Cumulative reclamation activity through 2022 versus static suitability",
  caveat="Centroid-based landscape screen; reported period totals may include repeated transitions"),
  file.path(out,"screen_complete.json"),pretty=TRUE,auto_unbox=TRUE)
cat("Mining native-cell screen calculated; independent checks still required.\n")
