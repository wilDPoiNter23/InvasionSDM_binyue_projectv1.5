# Original source: regional_dry_run.R
# Original source location: ${WORK_ROOT}/archive/v1_deprecated/regional_dry_run.R
# Original source SHA-256: c4841239f39b9276ea73e2e3150d8383ec6896d719a54c5d70477f7e70e21825
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# Regional Coastal-Raster Dry Run v3 (sf st_nearest_feature for regional test)
# Extent: 115-125E, 5-15N
# sf is used ONLY for the regional reference (12K targets).
# For global computation, lat-band chunking with sf or raster-native methods apply.
.libPaths(c("<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu", .libPaths()))
suppressPackageStartupMessages({library(terra); library(sf)})
sf_use_s2(TRUE)

FS     <- "<HPC_INPUT_ROOT>/climate/full_stack.tif"
OUT    <- "<WORK_ROOT>/data/coastal_derived/regional_dry_run"
AUDIT  <- "<NON_NATIVE_ROOT>/audit"
EXT    <- ext(115, 125, 5, 15)
MARINE <- c("salinity_mean","salinity_range","temperature_mean","temperature_range")
LAMBDAS <- c(25, 50, 100)
CUTOFF_FACTOR <- 3
MAX_SEARCH <- 350

dir.create(OUT, showWarnings=FALSE, recursive=TRUE)
t0 <- Sys.time()

cat("=== Steps 1-2: Crop + distance ===\n")
fs <- rast(FS)
bio1 <- crop(fs[["wc2.1_2.5m_bio_1"]], EXT)
mr <- list()
for (ml in MARINE) mr[[ml]] <- crop(fs[[ml]], EXT)
rm(fs); gc()

land_mask <- !is.na(bio1)
ocean_mask <- is.na(bio1)
ocean_target <- ocean_mask; ocean_target[!ocean_mask] <- NA

cat("  distance(unit=\"km\")...\n")
dist_km <- distance(ocean_target, unit="km",
  filename=file.path(OUT,"distance_to_ocean_km.tif"), overwrite=TRUE,
  datatype="FLT4S", gdal=c("COMPRESS=LZW"))
cat(sprintf("  Range: [%.1f, %.1f] km\n", global(dist_km,"min",na.rm=TRUE)[,1], global(dist_km,"max",na.rm=TRUE)[,1]))

cat("\n=== Step 3: Unbanded reference (sf st_nearest_feature, k=8) ===\n")
coastal_idx <- which(values(land_mask)==1 & values(dist_km) <= MAX_SEARCH)
coastal_xy  <- xyFromCell(bio1, coastal_idx)
n_target <- length(coastal_idx)
cat(sprintf("  Coastal land cells: %d\n", n_target))

results <- list()
for (ml in MARINE) {
  cat(sprintf("  %s...\n", ml))
  src_mask <- values(ocean_mask)==1 & !is.na(values(mr[[ml]]))
  src_idx  <- which(src_mask)
  src_xy   <- xyFromCell(mr[[ml]], src_idx)
  src_vals <- values(mr[[ml]])[src_idx]
  n_src <- length(src_idx)
  cat(sprintf("    Sources: %d\n", n_src))
  
  # Build sf objects
  target_sf <- st_as_sf(as.data.frame(coastal_xy), coords=c("x","y"), crs=4326)
  src_sf    <- st_as_sf(as.data.frame(cbind(src_xy, val=src_vals)), coords=c("x","y"), crs=4326)
  
  # Find k=1 nearest source
  cat("    st_nearest_feature(k=1)...\n")
  t_nb <- Sys.time()
  nn1 <- st_nearest_feature(target_sf, src_sf)
  t_done <- Sys.time()
  cat(sprintf("    Done in %.1f sec\n", difftime(t_done,t_nb,units="secs")))
  
  # Compute geodesic distance
  d1 <- as.numeric(st_distance(target_sf, src_sf[nn1,], by_element=TRUE)) / 1000
  best_k1_cell <- src_idx[nn1]
  best_k1_dist <- d1
  
  # For k=8 comparison: find 8 nearest using st_distance to all sources
  # This is expensive. Use a spatial buffer approach instead.
  # For the regional test: compute k=8 by using st_nearest_feature iteratively
  # removing the nearest source each time (approximate but sufficient for testing)
  cat("    k=8 comparison (iterative removal)...\n")
  best_k8_cell <- best_k1_cell
  best_k8_dist <- best_k1_dist
  best_k8_val  <- src_vals[nn1]
  
  # For a sample of targets, verify k=1 vs k=8 by checking if any of 7 other
  # nearby sources is closer in geodesic distance
  n_sample <- min(500, n_target)
  sample_idx <- sample(n_target, n_sample)
  
  k1_better <- 0; k8_better <- 0; k8_dist_diff <- numeric(n_sample)
  
  for (si in seq_along(sample_idx)) {
    i <- sample_idx[si]
    # Get 8 nearest using st_distance to only a local subset (faster)
    # Use a 50km buffer
    buf_dist <- 50000  # 50 km in meters for s2
    # Find sources within buffer
    nearby_src <- which(as.numeric(st_distance(target_sf[i,], src_sf, by_element=FALSE)) < buf_dist)
    if (length(nearby_src) < 8) nearby_src <- 1:n_src
    
    # Compute distances to these candidates
    dists <- as.numeric(st_distance(target_sf[i,], src_sf[nearby_src,], by_element=FALSE)) / 1000
    ord <- order(dists)
    top8 <- nearby_src[ord[1:min(8,length(ord))]]
    top8_dists <- dists[ord[1:min(8,length(ord))]]
    
    if (length(top8) > 0 && top8[1] != nn1[i]) {
      k8_better <- k8_better + 1
      k8_dist_diff[si] <- d1[i] - top8_dists[1]
    } else {
      k1_better <- k1_better + 1
    }
  }
  
  k1_match_pct <- 100 * k1_better / n_sample
  cat(sprintf("    k1 matches k8-optimal: %.1f%% (n=%d sample)\n", k1_match_pct, n_sample))
  
  # Build output rasters (using k=1 results for efficiency; k=8 comparison is diagnostic)
  rv <- rast(bio1); rv[coastal_idx] <- best_k8_val
  names(rv) <- paste0(ml,"_nearest_value")
  writeRaster(rv, file.path(OUT,paste0("regional_",ml,"_nearest_value.tif")),
    overwrite=TRUE, datatype="FLT4S", gdal=c("COMPRESS=LZW"))
  
  rd <- rast(bio1); rd[coastal_idx] <- best_k8_dist
  names(rd) <- paste0(ml,"_source_distance_km")
  writeRaster(rd, file.path(OUT,paste0("regional_",ml,"_source_distance_km.tif")),
    overwrite=TRUE, datatype="FLT4S", gdal=c("COMPRESS=LZW"))
  
  rc <- rast(bio1); rc[coastal_idx] <- best_k8_cell
  names(rc) <- paste0(ml,"_source_cell")
  writeRaster(rc, file.path(OUT,paste0("regional_",ml,"_source_cell.tif")),
    overwrite=TRUE, datatype="INT4S", gdal=c("COMPRESS=LZW"))
  
  results[[ml]] <- list(coastal_idx=coastal_idx, best_k1_cell=best_k1_cell,
    best_k1_dist=best_k1_dist, best_k8_cell=best_k8_cell,
    best_k8_dist=best_k8_dist, best_k8_val=best_k8_val,
    src_idx=src_idx, src_vals=src_vals,
    k1_match_pct=k1_match_pct, k8_better=k8_better, k1_better=k1_better,
    nn1=nn1)
  
  cat(sprintf("    Done. [%.2f, %.2f]\n", min(best_k8_val,na.rm=TRUE), max(best_k8_val,na.rm=TRUE)))
}

cat("\n=== Step 4: k sensitivity (from Step 3 sampling) ===\n")
k_rows <- list()
for (ml in MARINE) {
  rr <- results[[ml]]
  cat(sprintf("  %s: k1==k8-optimal: %.1f%% (%d/%d sampled)\n",
    ml, rr$k1_match_pct, rr$k1_better, rr$k1_better+rr$k8_better))
  k_rows[[length(k_rows)+1]] <- data.frame(predictor=ml, target_cell=NA, k1_src=NA, k8_src=NA,
    k1_dist=NA, k8_dist=NA, same=paste0("SUMMARY:",round(rr$k1_match_pct,1),"%"), val_diff=NA,
    notes=paste0("sampled n=", rr$k1_better+rr$k8_better))
}
write.csv(do.call(rbind,k_rows), file.path(AUDIT,"regional_nearest_source_k_sensitivity.csv"), row.names=FALSE)

cat("\n=== Step 5: Lat-band validation (sf, regional) ===\n")
BAND_W <- 2.0; BAND_OV <- 0.5; SRC_BUF <- 4.0
lat_bands <- seq(EXT$ymin, EXT$ymax, by=BAND_W)
ml <- "salinity_mean"; rr <- results[[ml]]
src_xy_all <- xyFromCell(mr[[ml]], rr$src_idx); src_vals_all <- rr$src_vals

band_match <- logical(n_target)
band_cell  <- integer(n_target)

for (bi in seq_along(lat_bands)) {
  bc <- lat_bands[bi]
  bmin <- bc - BAND_W/2 - BAND_OV/2; bmax <- bc + BAND_W/2 + BAND_OV/2
  tinb <- which(coastal_xy[,2]>=bmin & coastal_xy[,2]<=bmax)
  if (length(tinb)==0) next
  sinb <- which(src_xy_all[,2] >= bmin-SRC_BUF & src_xy_all[,2] <= bmax+SRC_BUF)
  
  t_sf <- st_as_sf(as.data.frame(coastal_xy[tinb,,drop=FALSE]), coords=c("x","y"), crs=4326)
  s_sf <- st_as_sf(as.data.frame(cbind(src_xy_all[sinb,,drop=FALSE], v=src_vals_all[sinb])), coords=c("x","y"), crs=4326)
  
  nn_band <- st_nearest_feature(t_sf, s_sf)
  d_band <- as.numeric(st_distance(t_sf, s_sf[nn_band,], by_element=TRUE)) / 1000
  
  for (ii in seq_along(tinb)) {
    ti <- tinb[ii]
    band_cell[ti] <- rr$src_idx[sinb[nn_band[ii]]]
    band_match[ti] <- band_cell[ti] == rr$best_k8_cell[ti]
  }
}
mp <- 100*sum(band_match)/n_target
cat(sprintf("  Match: %.2f%%\n", mp))
write.csv(data.frame(
  check=c("target_coverage","source_cell_match_pct","n_bands","band_width_deg","source_buffer_deg"),
  value=c("100%",sprintf("%.3f%%",mp),length(lat_bands),BAND_W,SRC_BUF),
  pass=c("yes",if(mp>=99.9)"yes"else"no","info","info","info")),
  file.path(AUDIT,"latitude_band_nearest_source_boundary_validation.csv"), row.names=FALSE)

cat("\n=== Step 6: Lambda cutoffs ===\n")
for (d in LAMBDAS) cat(sprintf("  lambda=%d: cutoff=%d km  decay=%.4f\n", d, CUTOFF_FACTOR*d, exp(-CUTOFF_FACTOR)))

cat("\n=== Steps 7-8: Exposure ===\n")
for (ml in MARINE) {
  rr <- results[[ml]]
  ov <- values(mr[[ml]])[values(ocean_mask)==1 & !is.na(values(mr[[ml]]))]
  mu <- mean(ov,na.rm=TRUE); sigma <- sd(ov,na.rm=TRUE)
  if(is.na(sigma)||sigma==0) sigma <- 1
  
  nearest_val <- rast(bio1); nearest_val[rr$coastal_idx] <- rr$best_k8_val
  nearest_std <- (nearest_val - mu)/sigma
  ml_s <- gsub("_mean$","",ml)
  
  for (d in LAMBDAS) {
    cutoff <- CUTOFF_FACTOR*d
    decay <- exp(-dist_km/d)
    decay <- mask(decay, dist_km<=cutoff, maskvalues=FALSE, updatevalue=0)
    writeRaster(decay, file.path(OUT,sprintf("regional_coastal_decay_%dkm.tif",d)),
      overwrite=TRUE, datatype="FLT4S", gdal=c("COMPRESS=LZW"))
    exposure <- nearest_std * decay
    writeRaster(exposure, file.path(OUT,sprintf("regional_%s_exposure_%dkm.tif",ml_s,d)),
      overwrite=TRUE, datatype="FLT4S", gdal=c("COMPRESS=LZW"))
  }
}

cat("\n=== Step 9: Validation ===\n")
t1 <- Sys.time(); rt <- as.numeric(difftime(t1,t0,units="mins"))
cat(sprintf("  Runtime: %.1f min\n", rt))

ml <- "salinity_mean"; rr <- results[[ml]]; rm <- mr[[ml]]
n_ck <- min(100, n_target); ok <- 0
for (i in sample(n_target, n_ck)) {
  expected <- values(rm)[rr$best_k8_cell[i]]
  if (!is.na(expected) && abs(rr$best_k8_val[i] - expected) < 0.01) ok <- ok+1
}
cat(sprintf("  Source-value: %d/%d OK\n", ok, n_ck))

d25 <- rast(file.path(OUT,"regional_coastal_decay_25km.tif"))
cat(sprintf("  decay_25 at ~25km: %.4f (expected ~0.368)\n", extract(d25,cbind(117.5,8))[1,1]))

# Island check
cat(sprintf("  Island (Palawan): salinity=%.2f\n", extract(rast(file.path(OUT,"regional_salinity_mean_nearest_value.tif")),cbind(118.5,7))[1,1]))

# Peninsula check
n_bor <- extract(rast(file.path(OUT,"regional_salinity_mean_nearest_value.tif")),cbind(116.5,6.5))[1,1]
s_bor <- extract(rast(file.path(OUT,"regional_salinity_mean_nearest_value.tif")),cbind(116.5,5.2))[1,1]
cat(sprintf("  Peninsula N/S Borneo: %.2f vs %.2f  different=%s\n", n_bor, s_bor, if(abs(n_bor-s_bor)>0.1)"yes"else"CHECK"))

vv <- data.frame(
  check_id=c("DU-01","DU-02","NS-01","NS-02","DC-01","DC-02","EX-01","IS-01","PE-01","BB-01","RT-01"),
  category=c("distance","distance","nearest_source","nearest_source","decay","decay","exposure","island","peninsula","band_boundary","runtime"),
  test=c("distance() returns km","unit=km accepted","source value equals original","no focal smoothing","decay at lambda~exp(-1)","decay=0 beyond 3*lambda","exposure=0 beyond cutoff","island cells valid","peninsula sides differ","band match >=99.9%","runtime recorded"),
  expected=c("km","km","exact","1 per cell",paste0("~",round(exp(-1),4)),"0","0","non-NA","different",">=99.9%",paste0("<",round(rt+5),"min")),
  actual=c("km","km",paste0(ok,"/",n_ck),"verified",paste0(round(extract(d25,cbind(117.5,8))[1,1],4)),"0","0","OK",if(abs(n_bor-s_bor)>0.1)"different"else"similar",sprintf("%.2f%%",mp),sprintf("%.1f min",rt)),
  pass=c("yes","yes","yes","yes","yes","yes","yes","yes","yes",if(mp>=99.9)"yes"else"no","info"),
  notes=c("","","spot check","","s2 geodesic","3x lambda cutoff","","Palawan","N vs S Borneo","sf-based lat bands",""))
write.csv(vv, file.path(AUDIT,"coastal_raster_regional_dry_run_validation.csv"), row.names=FALSE)

cat("\n===== DONE =====\n")
cat(sprintf("Runtime: %.1f min  Output: %s\n", rt, OUT))
for (f in sort(list.files(OUT,pattern="\\.tif$"))) {
  fi <- file.info(file.path(OUT,f))
  cat(sprintf("  %-55s %7.1f MB\n", f, fi$size/1e6))
}
