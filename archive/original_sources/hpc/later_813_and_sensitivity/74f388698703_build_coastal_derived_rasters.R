# Original source: build_coastal_derived_rasters.R
# Original source location: ${WORK_ROOT}/archive/v1_deprecated/build_coastal_derived_rasters.R
# Original source SHA-256: 74f3886987032df5d6ef21a057ca84298483087867bddaa48bfc66cf450e3d3f
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# Steps 2-4 (CORRECTED): Coastal distance, marine-to-land transfer, decayed exposure
# ============================================================================
# Distance method:    s2 spherical-geometry geodesic (sf::st_distance with s2)
# Marine transfer:    nearest-valid-cell lookup (sf::st_nearest_feature)
# Exposure:           exp(-dist/lambda) * standardised_nearest_marine_value
# Max transfer:       110 km
# ============================================================================

.libPaths(c("<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu", .libPaths()))
suppressPackageStartupMessages({
  library(terra)
  library(sf)
})

sf_use_s2(TRUE)  # spherical geometry for true geodesic distances

FS         <- "<HPC_INPUT_ROOT>/climate/full_stack.tif"
OUT        <- "<WORK_ROOT>/data/coastal_derived"
MARINE     <- c("salinity_mean","salinity_range","temperature_mean","temperature_range")
LAMBDAS    <- c(25, 50, 100)
MAX_COASTAL <- max(LAMBDAS) + 10   # 110 km — furthest we compute nearest-marine
BLOCK_SIZE  <- 20000               # land cells per s2 query block

dir.create(OUT, showWarnings=FALSE, recursive=TRUE)

# ═══════════════════════════════════════════════════════════════════════
# STEP 2: Coastal distance (s2 geodesic)
# ═══════════════════════════════════════════════════════════════════════

cat("=== Step 2: Coastal distance (s2 geodesic) ===\n")

# Land mask from bio_1
cat("  Loading land mask...\n")
fs   <- rast(FS)
bio1 <- fs[["wc2.1_2.5m_bio_1"]]
land_mask <- !is.na(bio1)
rm(bio1); gc()

n_land <- global(land_mask, "sum", na.rm=TRUE)[,1]
cat(sprintf("  Land cells: %d (%.1f%%)\n", n_land, 100*n_land/ncell(land_mask)))

# Coastline = land cells adjacent to ocean
cat("  Computing coastline boundary...\n")
coast_r  <- boundaries(land_mask, classes=FALSE, directions=8)
coast_xy <- xyFromCell(coast_r, which(values(coast_r) == 1))
rm(coast_r); gc()
cat(sprintf("  Coastline cells: %d\n", nrow(coast_xy)))

# Build coastline sf object
coast_sf <- st_as_sf(as.data.frame(coast_xy), coords=c("x","y"), crs=4326)
cat(sprintf("  Coastline sf: %d points\n", nrow(coast_sf)))

# Land cell indices and coordinates
land_idx <- which(values(land_mask) == 1)
land_xy  <- xyFromCell(land_mask, land_idx)

# Compute geodesic distance to nearest coast for all land cells (blocked)
cat(sprintf("  Computing geodesic distance for %d land cells (block_size=%d)...\n",
  length(land_idx), BLOCK_SIZE))

dist_km <- numeric(length(land_idx))
n_blocks <- ceiling(length(land_idx) / BLOCK_SIZE)
pb <- txtProgressBar(min=0, max=n_blocks, style=3)

for (b in seq_len(n_blocks)) {
  i_start <- (b-1) * BLOCK_SIZE + 1
  i_end   <- min(b * BLOCK_SIZE, length(land_idx))
  block_xy <- land_xy[i_start:i_end, , drop=FALSE]
  
  block_sf <- st_as_sf(as.data.frame(block_xy), coords=c("x","y"), crs=4326)
  nearest  <- st_nearest_feature(block_sf, coast_sf)
  d_m      <- st_distance(block_sf, coast_sf[nearest, ], by_element=TRUE)
  dist_km[i_start:i_end] <- as.numeric(d_m) / 1000
  
  setTxtProgressBar(pb, b)
}
close(pb)

# Build distance raster
cat("  Building distance raster...\n")
dist_r <- rast(land_mask)
dist_r[land_idx] <- dist_km
names(dist_r) <- "distance_to_coast_km"

# Validate
cat(sprintf("  distance_to_coast_km: [%.1f, %.1f] km, NA-frac=%.4f\n",
  global(dist_r, "min", na.rm=TRUE)[,1],
  global(dist_r, "max", na.rm=TRUE)[,1],
  global(is.na(dist_r), "mean", na.rm=TRUE)[,1]))

# Spot checks
spot_checks <- data.frame(
  loc   = c("London","Sahara","Kashgar","Hawaii","Nairobi","Urumqi"),
  lon   = c(-0.1, 10.0, 76.0, -155.5, 36.8, 87.6),
  lat   = c(51.5, 25.0, 39.5, 19.7, -1.3, 43.8),
  expected_min = c(0, 500, 1500, 0, 300, 1500)
)
cat("  Spot checks:\n")
for (i in seq_len(nrow(spot_checks))) {
  sc <- spot_checks[i,]
  e <- extract(dist_r, cbind(sc$lon, sc$lat))[1,1]
  if (is.na(e)) e <- -1
  cat(sprintf("    %-12s (%.1f, %.1f): %.0f km (expected >%.0f km) %s\n",
    sc$loc, sc$lon, sc$lat, e, sc$expected_min,
    if(e >= sc$expected_min) "OK" else "CHECK"))
}

# Save
cat("  Saving distance_to_coast_km.tif...\n")
writeRaster(dist_r, file.path(OUT, "distance_to_coast_km.tif"),
  overwrite=TRUE, datatype="FLT4S", gdal=c("COMPRESS=LZW","TILED=YES"))

# Coastal-zone masks
for (d in LAMBDAS) {
  cat(sprintf("  Saving coastal_zone_%dkm.tif...\n", d))
  zone <- dist_r <= d
  writeRaster(zone, file.path(OUT, sprintf("coastal_zone_%dkm.tif", d)),
    overwrite=TRUE, datatype="INT1U", gdal=c("COMPRESS=LZW","TILED=YES"))
}

# ═══════════════════════════════════════════════════════════════════════
# STEP 3: Nearest-marine-value transfer (s2 nearest-cell lookup)
# ═══════════════════════════════════════════════════════════════════════

cat("\n=== Step 3: Nearest-marine-value transfer ===\n")

# Identify coastal land cells (within MAX_COASTAL km)
coastal_land_idx <- land_idx[dist_km <= MAX_COASTAL]
coastal_land_xy  <- land_xy[dist_km <= MAX_COASTAL, , drop=FALSE]
cat(sprintf("  Coastal land cells (<=%d km): %d\n", MAX_COASTAL, length(coastal_land_idx)))

# Build ocean mask for value extraction
ocean_mask <- is.na(fs[["wc2.1_2.5m_bio_1"]])

for (ml in MARINE) {
  cat(sprintf("  Processing %s...\n", ml))
  r_marine <- fs[[ml]]
  
  # Identify valid ocean cells with marine data
  ocean_valid <- which(values(ocean_mask) == 1 & !is.na(values(r_marine)))
  ocean_xy    <- xyFromCell(r_marine, ocean_valid)
  ocean_vals  <- values(r_marine)[ocean_valid]
  
  cat(sprintf("    Valid ocean cells: %d\n", length(ocean_valid)))
  
  # Build ocean sf
  ocean_df <- data.frame(x=ocean_xy[,1], y=ocean_xy[,2], val=ocean_vals)
  ocean_sf <- st_as_sf(ocean_df, coords=c("x","y"), crs=4326)
  
  # For each coastal land cell: find nearest ocean cell, get value + distance
  nearest_val <- numeric(length(coastal_land_idx))
  source_dist  <- numeric(length(coastal_land_idx))
  
  n_blocks <- ceiling(length(coastal_land_idx) / BLOCK_SIZE)
  pb <- txtProgressBar(min=0, max=n_blocks, style=3)
  
  for (b in seq_len(n_blocks)) {
    i_start <- (b-1) * BLOCK_SIZE + 1
    i_end   <- min(b * BLOCK_SIZE, length(coastal_land_idx))
    
    block_xy <- coastal_land_xy[i_start:i_end, , drop=FALSE]
    block_sf <- st_as_sf(as.data.frame(block_xy), coords=c("x","y"), crs=4326)
    
    nearest <- st_nearest_feature(block_sf, ocean_sf)
    nearest_val[i_start:i_end] <- ocean_sf$val[nearest]
    
    d_m <- st_distance(block_sf, ocean_sf[nearest, ], by_element=TRUE)
    source_dist[i_start:i_end] <- as.numeric(d_m) / 1000
    
    setTxtProgressBar(pb, b)
  }
  close(pb)
  
  # Build nearest-marine raster
  nm_r <- rast(land_mask)
  nm_r[coastal_land_idx] <- nearest_val
  nm_name <- paste0("nearest_marine_", ml)
  names(nm_r) <- nm_name
  
  cat(sprintf("    Saving %s...\n", nm_name))
  writeRaster(nm_r, file.path(OUT, paste0(nm_name, ".tif")),
    overwrite=TRUE, datatype="FLT4S", gdal=c("COMPRESS=LZW","TILED=YES"))
  
  cat(sprintf("    Range: [%.2f, %.2f], NA-frac=%.4f\n",
    global(nm_r, "min", na.rm=TRUE)[,1],
    global(nm_r, "max", na.rm=TRUE)[,1],
    global(is.na(nm_r), "mean", na.rm=TRUE)[,1]))
  
  rm(r_marine, ocean_df, ocean_sf, nm_r); gc()
}

# Save marine source distance (separate from distance-to-coast for clarity)
cat("  Saving marine_source_distance_km.tif...\n")
sd_r <- rast(land_mask)
sd_r[coastal_land_idx] <- source_dist
names(sd_r) <- "marine_source_distance_km"
writeRaster(sd_r, file.path(OUT, "marine_source_distance_km.tif"),
  overwrite=TRUE, datatype="FLT4S", gdal=c("COMPRESS=LZW","TILED=YES"))

# ═══════════════════════════════════════════════════════════════════════
# STEP 4: Distance-decayed exposure
# ═══════════════════════════════════════════════════════════════════════

cat("\n=== Step 4: Distance-decayed exposure ===\n")

# 4a: Coastal decay (distance-only)
for (d in LAMBDAS) {
  cat(sprintf("  coastal_decay_%dkm...\n", d))
  decay <- exp(-dist_r / d)
  decay <- mask(decay, dist_r <= d, maskvalues=FALSE, updatevalue=0)
  nm <- sprintf("coastal_decay_%dkm", d)
  names(decay) <- nm
  writeRaster(decay, file.path(OUT, paste0(nm, ".tif")),
    overwrite=TRUE, datatype="FLT4S", gdal=c("COMPRESS=LZW","TILED=YES"))
}

# 4b: Marine exposure (standardised nearest-marine * decay)
for (ml in MARINE) {
  ml_short <- gsub("_mean$", "", ml)
  nm_file  <- file.path(OUT, paste0("nearest_marine_", ml, ".tif"))
  nearest  <- rast(nm_file)
  
  # Standardise over all ocean cells (global marine reference frame)
  ocean_vals_all <- values(fs[[ml]])[values(ocean_mask) == 1]
  ocean_vals_all <- ocean_vals_all[!is.na(ocean_vals_all)]
  mu    <- mean(ocean_vals_all)
  sigma <- sd(ocean_vals_all)
  if (is.na(sigma) || sigma == 0) sigma <- 1
  
  cat(sprintf("  %s: mu=%.3f sigma=%.3f (N_ocean=%d)\n",
    ml_short, mu, sigma, length(ocean_vals_all)))
  
  nearest_std <- (nearest - mu) / sigma
  
  for (d in LAMBDAS) {
    cat(sprintf("  %s_exposure_%dkm...\n", ml_short, d))
    decay <- rast(file.path(OUT, sprintf("coastal_decay_%dkm.tif", d)))
    marine_exp <- nearest_std * decay
    marine_exp <- mask(marine_exp, dist_r <= d, maskvalues=FALSE, updatevalue=0)
    nm <- sprintf("%s_exposure_%dkm", ml_short, d)
    names(marine_exp) <- nm
    writeRaster(marine_exp, file.path(OUT, paste0(nm, ".tif")),
      overwrite=TRUE, datatype="FLT4S", gdal=c("COMPRESS=LZW","TILED=YES"))
  }
  rm(nearest, nearest_std); gc()
}

# ═══════════════════════════════════════════════════════════════════════
# VALIDATION
# ═══════════════════════════════════════════════════════════════════════

cat("\n=== Validation ===\n")
cat(sprintf("  distance_to_coast_km: [%.1f, %.1f] km\n",
  global(dist_r, "min", na.rm=TRUE)[,1],
  global(dist_r, "max", na.rm=TRUE)[,1]))

for (d in LAMBDAS) {
  zone <- rast(file.path(OUT, sprintf("coastal_zone_%dkm.tif", d)))
  nz <- global(zone, "sum", na.rm=TRUE)[,1]
  cat(sprintf("  coastal_zone_%dkm: %d cells (%.1f%% of land)\n", d, nz, 100*nz/n_land))
}

for (ml in MARINE) {
  ml_short <- gsub("_mean$", "", ml)
  for (d in LAMBDAS) {
    fn <- file.path(OUT, sprintf("%s_exposure_%dkm.tif", ml_short, d))
    if (!file.exists(fn)) next
    r <- rast(fn)
    v <- global(r, c("min","max","mean"), na.rm=TRUE)
    na_f <- global(is.na(r), "mean", na.rm=TRUE)[,1]
    cat(sprintf("  %s_exposure_%dkm: [%.3f, %.3f] mu=%.4f NA-frac=%.3f\n",
      ml_short, d, v[1,1], v[2,1], v[3,1], na_f))
  }
}

cat("\n--- Output inventory ---\n")
for (f in sort(list.files(OUT, pattern="\\.tif$"))) {
  fi <- file.info(file.path(OUT, f))
  cat(sprintf("  %-50s %8.1f MB\n", f, fi$size/1e6))
}

cat("\n===== Steps 2-4 complete (corrected method) =====\n")
