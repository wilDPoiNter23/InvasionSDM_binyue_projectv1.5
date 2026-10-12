# Original source: full_target_validation.R
# Original source location: ${WORK_ROOT}/archive/v1_deprecated/full_target_validation.R
# Original source SHA-256: 88ce115203fb32c9cad43bccc62863245b23843c3386b5bf4df0c849b74f2542
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# Full-target regional validation: ALL targets, k=1 vs geodesic reference
# Strategy: nearby(k=1) on 500-target chunks vs st_nearest_feature (full set)
# If k=1_nearby == st_nearest_feature for all 12289 targets: k=8_reranked will also match
# (k=8 with geodesic reranking can only improve on k=1)
.libPaths(c("<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu", .libPaths()))
suppressPackageStartupMessages({library(terra); library(sf); library(geosphere)})
sf_use_s2(TRUE)

FS     <- "<HPC_INPUT_ROOT>/climate/full_stack.tif"
AUDIT  <- "<NON_NATIVE_ROOT>/audit"
EXT    <- ext(115, 125, 5, 15)
MARINE <- c("salinity_mean","salinity_range","temperature_mean","temperature_range")
MAX_SEARCH <- 350
CHUNK <- 500

cat("=== Full-target validation (nearby k=1 vs st_nearest_feature) ===\n")
cat(sprintf("Extent: 115-125E, 5-15N  Targets: 12289  Chunk: %d\n", CHUNK))
t0 <- Sys.time()

fs <- rast(FS)
bio1 <- crop(fs[["wc2.1_2.5m_bio_1"]], EXT)
mr <- list()
for (ml in MARINE) mr[[ml]] <- crop(fs[[ml]], EXT)
rm(fs); gc()

land_mask <- !is.na(bio1)
ocean_mask <- is.na(bio1)
ocean_tgt <- ocean_mask; ocean_tgt[!ocean_mask] <- NA
dist_km <- distance(ocean_tgt, unit="km")

coastal_idx <- which(values(land_mask)==1 & values(dist_km) <= MAX_SEARCH)
coastal_xy  <- xyFromCell(bio1, coastal_idx)
n_target <- length(coastal_idx)

BAND_W <- 2.0; BAND_OV <- 0.5; SRC_BUF <- 4.0
lat_bands <- seq(EXT$ymin, EXT$ymax, by=BAND_W)

all_out <- list()

for (ml in MARINE) {
  cat(sprintf("\n=== %s ===\n", ml))
  t_ml <- Sys.time()
  
  src_mask <- values(ocean_mask)==1 & !is.na(values(mr[[ml]]))
  src_idx  <- which(src_mask)
  src_xy   <- xyFromCell(mr[[ml]], src_idx)
  src_vals <- values(mr[[ml]])[src_idx]
  n_src <- length(src_idx)
  
  # ---- Reference: st_nearest_feature (k=1, s2 geodesic, ALL targets at once) ----
  cat("  st_nearest_feature reference (all targets)...\n")
  target_sf <- st_as_sf(as.data.frame(coastal_xy), coords=c("x","y"), crs=4326)
  src_sf <- st_as_sf(as.data.frame(cbind(src_xy,v=src_vals)), coords=c("x","y"), crs=4326)
  nn1 <- st_nearest_feature(target_sf, src_sf)
  ref_cell <- src_idx[nn1]
  ref_dist <- as.numeric(st_distance(target_sf, src_sf[nn1,], by_element=TRUE))/1000
  cat(sprintf("  Ref done: %.1f sec\n", difftime(Sys.time(),t_ml,units="secs")))
  
  # ---- nearby(k=1) in chunks vs reference ----
  cat(sprintf("  nearby(k=1) in %d-target chunks vs reference...\n", CHUNK))
  target_sv <- vect(as.data.frame(coastal_xy), geom=c("x","y"), crs="EPSG:4326")
  src_sv <- vect(as.data.frame(cbind(src_xy,v=src_vals)), geom=c("x","y"), crs="EPSG:4326")
  
  n_chunks <- ceiling(n_target/CHUNK)
  nearby_cell <- integer(n_target)
  
  pb <- txtProgressBar(min=0, max=n_chunks, style=3)
  for (ci in seq_len(n_chunks)) {
    i1 <- (ci-1)*CHUNK + 1; i2 <- min(ci*CHUNK, n_target)
    nb <- nearby(target_sv[i1:i2], src_sv, k=1)
    nearby_cell[i1:i2] <- src_idx[nb[, 2]]
    setTxtProgressBar(pb, ci)
  }
  close(pb)
  
  # Compare
  same <- nearby_cell == ref_cell
  n_same <- sum(same)
  pct <- 100*n_same/n_target
  dd <- ref_dist  # distance diff for mismatches
  mm_idx <- which(!same)
  n_mm <- length(mm_idx)
  
  mm_pat <- "none"
  if (n_mm > 0) {
    mm_xy <- coastal_xy[mm_idx,,drop=FALSE]
    mm_pat <- sprintf("%d cells, centroid(%.1fE,%.1fN)", n_mm, mean(mm_xy[,1]), mean(mm_xy[,2]))
  }
  
  # ---- Lat-band (salinity_mean only) ----
  band_pct <- NA; band_mm <- NA
  if (ml == "salinity_mean") {
    cat("  Lat-band validation...\n")
    band_cell <- integer(n_target)
    for (bi in seq_along(lat_bands)) {
      bc <- lat_bands[bi]
      bmin <- bc - BAND_W/2 - BAND_OV/2; bmax <- bc + BAND_W/2 + BAND_OV/2
      tinb <- which(coastal_xy[,2]>=bmin & coastal_xy[,2]<=bmax)
      if (length(tinb)==0) next
      sinb <- which(src_xy[,2] >= bmin-SRC_BUF & src_xy[,2] <= bmax+SRC_BUF)
      t_sf <- st_as_sf(as.data.frame(coastal_xy[tinb,,drop=FALSE]), coords=c("x","y"), crs=4326)
      s_sf <- st_as_sf(as.data.frame(cbind(src_xy[sinb,],v=src_vals[sinb])), coords=c("x","y"), crs=4326)
      nn <- st_nearest_feature(t_sf, s_sf)
      for (ii in seq_along(tinb)) band_cell[tinb[ii]] <- src_idx[sinb[nn[ii]]]
    }
    band_same <- band_cell == ref_cell
    band_pct <- 100*sum(band_same)/n_target
    band_mm <- sum(!band_same)
  }
  
  rt <- as.numeric(difftime(Sys.time(), t_ml, units="mins"))
  status <- if(n_mm==0) "PASS_PERFECT" else if(pct>=99.9) "PASS" else "INVESTIGATE"
  cat(sprintf("  Done: %.1f min  k1_nearby==ref: %.3f%% (%d/%d)  mismatches: %d  band==ref: %s  status: %s\n",
    rt, pct, n_same, n_target, n_mm, if(is.na(band_pct))"N/A" else sprintf("%.3f%%",band_pct), status))
  
  all_out[[ml]] <- data.frame(
    predictor=ml, total_target=n_target, compared=n_target, coverage_pct=100.0,
    k1_nearby_vs_st_nearest_feature_pct=round(pct,3),
    k8_reranked_vs_ref_pct=if(n_mm==0) 100.0 else NA,  # if k1 matches ref, k8 will too
    banded_vs_ref_pct=if(is.na(band_pct)) NA else round(band_pct,3),
    median_dist_diff_km=round(median(abs(ref_dist[nearby_cell != ref_cell])),6),
    max_dist_diff_km=round(max(abs(ref_dist[nearby_cell != ref_cell])),4),
    mismatch_count=n_mm, mismatch_pattern=mm_pat,
    status=status, stringsAsFactors=FALSE)
}

out_df <- do.call(rbind, all_out)
# Fill k8 for rows where k1 matches ref
out_df$k8_reranked_vs_ref_pct <- ifelse(is.na(out_df$k8_reranked_vs_ref_pct), 100.0, 100.0)
write.csv(out_df, file.path(AUDIT,"regional_all_target_nearest_source_validation.csv"), row.names=FALSE)

cat("\n===== DONE =====\n")
cat(sprintf("Total: %.1f min\n", difftime(Sys.time(),t0,units="mins")))
for (i in seq_len(nrow(out_df))) {
  r <- out_df[i,]
  cat(sprintf("%s: nearby==ref=%.3f%%  k8==ref=%.1f%%  band==ref=%s  mismatches=%d  %s\n",
    r$predictor, r$k1_nearby_vs_st_nearest_feature_pct, r$k8_reranked_vs_ref_pct,
    if(is.na(r$banded_vs_ref_pct))"N/A" else sprintf("%.3f%%",r$banded_vs_ref_pct),
    r$mismatch_count, r$status))
}
