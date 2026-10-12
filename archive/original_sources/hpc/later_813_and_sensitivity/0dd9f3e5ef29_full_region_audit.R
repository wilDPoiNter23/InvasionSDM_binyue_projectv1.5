# Original source: full_region_audit.R
# Original source location: ${WORK_ROOT}/archive/v1_deprecated/full_region_audit.R
# Original source SHA-256: 0dd9f3e5ef29ec8f64649911a4801e950e04192afcb3ded3488c863b7776af66
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# Full-region nearest-source comparison (all 12,289 targets, k=1 vs k=8)
.libPaths(c("<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu", .libPaths()))
suppressPackageStartupMessages({library(terra); library(sf); library(geosphere)})
sf_use_s2(TRUE)

FS     <- "<HPC_INPUT_ROOT>/climate/full_stack.tif"
AUDIT  <- "<NON_NATIVE_ROOT>/audit"
EXT    <- ext(115, 125, 5, 15)
MARINE <- c("salinity_mean","salinity_range","temperature_mean","temperature_range")
MAX_SEARCH <- 350
CHUNK_SIZE <- 2000  # targets per nearby() chunk

cat("=== Full-region k=1 vs k=8 comparison ===\n")
cat("Extent: 115-125E, 5-15N\n")

# Load data
fs <- rast(FS)
bio1 <- crop(fs[["wc2.1_2.5m_bio_1"]], EXT)
mr <- list()
for (ml in MARINE) mr[[ml]] <- crop(fs[[ml]], EXT)
rm(fs); gc()

land_mask <- !is.na(bio1)
ocean_mask <- is.na(bio1)
ocean_target <- ocean_mask; ocean_target[!ocean_mask] <- NA
dist_km <- distance(ocean_target, unit="km")

coastal_idx <- which(values(land_mask)==1 & values(dist_km) <= MAX_SEARCH)
coastal_xy  <- xyFromCell(bio1, coastal_idx)
n_target <- length(coastal_idx)
cat(sprintf("Coastal targets: %d\n", n_target))

# Results storage
all_results <- list()

for (ml in MARINE) {
  cat(sprintf("\n%s...\n", ml))
  t_ml <- Sys.time()
  
  src_mask <- values(ocean_mask)==1 & !is.na(values(mr[[ml]]))
  src_idx  <- which(src_mask)
  src_xy   <- xyFromCell(mr[[ml]], src_idx)
  src_vals <- values(mr[[ml]])[src_idx]
  n_src <- length(src_idx)
  cat(sprintf("  Sources: %d\n", n_src))
  
  # ---- k=1 via sf (fast, s2 geodesic) ----
  cat("  k=1 via st_nearest_feature...\n")
  target_sf <- st_as_sf(as.data.frame(coastal_xy), coords=c("x","y"), crs=4326)
  src_sf <- st_as_sf(as.data.frame(cbind(src_xy,v=src_vals)), coords=c("x","y"), crs=4326)
  nn1 <- st_nearest_feature(target_sf, src_sf)
  k1_cell <- src_idx[nn1]
  k1_dist <- as.numeric(st_distance(target_sf, src_sf[nn1,], by_element=TRUE))/1000
  
  # ---- k=8 via terra::nearby() on chunks ----
  cat(sprintf("  k=8 via nearby() in chunks of %d...\n", CHUNK_SIZE))
  target_sv <- vect(as.data.frame(coastal_xy), geom=c("x","y"), crs="EPSG:4326")
  src_sv <- vect(as.data.frame(cbind(src_xy,v=src_vals)), geom=c("x","y"), crs="EPSG:4326")
  
  n_chunks <- ceiling(n_target / CHUNK_SIZE)
  k8_cell <- integer(n_target)
  k8_dist <- numeric(n_target)
  
  pb <- txtProgressBar(min=0, max=n_chunks, style=3)
  for (ci in seq_len(n_chunks)) {
    i1 <- (ci-1)*CHUNK_SIZE + 1
    i2 <- min(ci*CHUNK_SIZE, n_target)
    chunk_targets <- target_sv[i1:i2]
    
    # nearby(k=8) on this chunk
    nb <- nearby(chunk_targets, src_sv, k=8)  # [n_chunk × 9] matrix
    
    # For each target in chunk: compute geodesic distance to all 8 candidates
    n_chunk <- nrow(nb)
    for (i in seq_len(n_chunk)) {
      ti <- i1 + i - 1
      target_xy_i <- coastal_xy[ti, , drop=FALSE]
      best_d <- Inf; best_c <- NA
      for (j in 2:9) {  # nb columns 2:9 are k1..k8
        src_vec_idx <- nb[i, j]
        src_xy_j <- src_xy[src_vec_idx, , drop=FALSE]
        d <- distGeo(target_xy_i, src_xy_j) / 1000
        if (d < best_d) { best_d <- d; best_c <- src_idx[src_vec_idx] }
      }
      k8_cell[ti] <- best_c
      k8_dist[ti] <- best_d
    }
    setTxtProgressBar(pb, ci)
  }
  close(pb)
  
  # ---- Compare k=1 vs k=8 ----
  same  <- k1_cell == k8_cell
  n_same <- sum(same)
  pct <- 100 * n_same / n_target
  dd <- k1_dist - k8_dist
  
  # Spatial pattern of mismatches
  mismatch_idx <- which(!same)
  n_mismatch <- length(mismatch_idx)
  mismatch_pattern <- "none"
  if (n_mismatch > 0) {
    mm_xy <- coastal_xy[mismatch_idx, , drop=FALSE]
    mm_lat <- mean(mm_xy[,2]); mm_lon <- mean(mm_xy[,1])
    mm_dd <- mean(dd[mismatch_idx])
    mismatch_pattern <- sprintf("%d cells, centroid (%.1fE,%.1fN), mean dd=%.4f km",
      n_mismatch, mm_lon, mm_lat, mm_dd)
  }
  
  t_done <- Sys.time()
  cat(sprintf("\n  %s: k1==k8: %.2f%% (%d/%d)  med_dd=%.4f km  max_dd=%.4f km  mismatches=%d  time=%.1f min\n",
    ml, pct, n_same, n_target, median(dd), max(dd), n_mismatch,
    as.numeric(difftime(t_done,t_ml,units="mins"))))
  
  all_results[[ml]] <- data.frame(
    predictor = ml,
    total_target_cells = n_target,
    cells_compared = n_target,
    target_coverage_pct = 100.0,
    k1_k8_source_match_pct = round(pct, 3),
    banded_unbanded_source_match_pct = NA,  # populated below
    median_distance_difference_km = round(median(dd), 6),
    maximum_distance_difference_km = round(max(dd), 4),
    mismatched_cell_count = n_mismatch,
    mismatch_spatial_pattern = mismatch_pattern,
    final_status = if(n_mismatch==0) "PASS_PERFECT_MATCH" else if(pct>=99.9) "PASS" else "INVESTIGATE",
    stringsAsFactors = FALSE
  )
}

# ---- Lat-band comparison (salinity_mean only, for banded vs unbanded) ----
cat("\n=== Lat-band validation (all targets) ===\n")
ml <- "salinity_mean"
BAND_W <- 2.0; BAND_OV <- 0.5; SRC_BUF <- 4.0
lat_bands <- seq(EXT$ymin, EXT$ymax, by=BAND_W)
src_xy_all <- xyFromCell(mr[[ml]], all_results[[ml]]$src_idx <- src_idx)
# Use k8 values from above as reference
k8_ref_cell <- k8_cell
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

band_same <- band_cell == k8_ref_cell
band_pct <- 100*sum(band_same)/n_target
cat(sprintf("  Banded vs unbanded: %.2f%% match\n", band_pct))
all_results[["salinity_mean"]]$banded_unbanded_source_match_pct <- round(band_pct, 3)

# ---- Write CSV ----
out_df <- do.call(rbind, all_results)
# Fix the src_idx assignment issue
for (i in seq_len(nrow(out_df))) {
  if (is.na(out_df$banded_unbanded_source_match_pct[i])) 
    out_df$banded_unbanded_source_match_pct[i] <- ""
}
write.csv(out_df, file.path(AUDIT,"regional_all_target_nearest_source_validation.csv"), row.names=FALSE)

cat("\n===== Full-region comparison complete =====\n")
cat(sprintf("Output: %s\n", file.path(AUDIT,"regional_all_target_nearest_source_validation.csv")))
for (i in seq_len(nrow(out_df))) {
  cat(sprintf("  %s: k1==k8=%.3f%%  band==unband=%s  status=%s\n",
    out_df$predictor[i], out_df$k1_k8_source_match_pct[i],
    out_df$banded_unbanded_source_match_pct[i], out_df$final_status[i]))
}
