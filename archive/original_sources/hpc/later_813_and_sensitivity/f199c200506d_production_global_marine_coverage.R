# Original source: production_global_marine_coverage.R
# Original source location: ${WORK_ROOT}/archive/v1_deprecated/production_global_marine_coverage.R
# Original source SHA-256: f199c200506d488c0d634942b6703107254f91dfb91f7c9be07892b70b71d13d
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/usr/bin/env Rscript
# production_global_marine_coverage.R
# Terrestrial marine-coverage for decoded HYCOM salinity_mean + temperature_mean
# ============================================================================
# Usage:
#   Test mode:  Rscript production_global_marine_coverage.R --mode test
#   Global mode: Rscript production_global_marine_coverage.R --mode global [--resume]
# ============================================================================

.libPaths(c("<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu", .libPaths()))
suppressPackageStartupMessages({library(terra); library(sf); library(geosphere)})
sf_use_s2(TRUE)

# ── Parse CLI arguments ────────────────────────────────────────────────
args <- commandArgs(trailingOnly=TRUE)
mode     <- "test"
resume   <- FALSE
out_dir  <- NULL

i <- 1
while (i <= length(args)) {
  if (args[i] == "--mode" && i < length(args)) { mode <- args[i+1]; i <- i+2 }
  else if (args[i] == "--resume") { resume <- TRUE; i <- i+1 }
  else if (args[i] == "--out-dir" && i < length(args)) { out_dir <- args[i+1]; i <- i+2 }
  else { i <- i+1 }
}

# ── Resolve mode ───────────────────────────────────────────────────────
FS        <- "<HPC_INPUT_ROOT>/climate/full_stack.tif"
HYCOM_DIR <- "<WORK_ROOT>/data/coastal_derived/decoded_hycom"
MARINE    <- c("hycom_salinity_mean_decoded", "hycom_temperature_mean_decoded")
LAMBDAS   <- c(25, 50, 100)
MAX_SEARCH_KM <- 550
BAND_W_DEG    <- 4.0
SRC_EXP_DEG   <- 7.0
SRC_EXP_LON_FACTOR <- 2.0

if (mode == "test") {
  EXTENT  <- ext(110, 130, -5, 15)
  if (is.null(out_dir)) out_dir <- "<WORK_ROOT>/data/coastal_derived/test_20deg_endtoend_v2"
} else if (mode == "global") {
  EXTENT  <- ext(-180, 180, -90, 90)
  if (is.null(out_dir)) out_dir <- "<WORK_ROOT>/data/coastal_derived/global_v1"
} else {
  stop("Unknown mode. Use --mode test or --mode global")
}

CHECKPOINT_DIR <- file.path(out_dir, "checkpoints")

cat(sprintf("=== Marine Coverage Production ===\n"))
cat(sprintf("Mode:    %s\n", mode))
cat(sprintf("Extent:  %.0f-%.0fE, %.0f-%.0fN\n", EXTENT$xmin, EXTENT$xmax, EXTENT$ymin, EXTENT$ymax))
cat(sprintf("Output:  %s\n", out_dir))
cat(sprintf("Resume:  %s\n", resume))

# ── Pre-flight checks ──────────────────────────────────────────────────
if (!resume && dir.exists(out_dir) && length(list.files(out_dir, pattern="\\.tif$")) > 0) {
  stop(sprintf("Output directory %s exists with .tif files. Use --resume to continue, or remove/rename the directory.", out_dir))
}

# Disk space check
df_output <- system(paste0("df --output=avail ", shQuote(out_dir), " 2>/dev/null | tail -1"), intern=TRUE)
if (length(df_output) > 0 && as.numeric(df_output) < 100*1024*1024) {
  cat(sprintf("WARNING: <100 GB available on output filesystem (%s KB)\n", df_output))
}

dir.create(out_dir, showWarnings=FALSE, recursive=TRUE)
dir.create(CHECKPOINT_DIR, showWarnings=FALSE, recursive=TRUE)

t0 <- Sys.time()

# ═══════════════════════════════════════════════════════════════════════
# 1. LOAD DATA
# ═══════════════════════════════════════════════════════════════════════
cat("\n[1/6] Loading data...\n")
fs <- rast(FS)
bio1 <- crop(fs[["wc2.1_2.5m_bio_1"]], EXTENT)
land_mask <- !is.na(bio1)
ocean_mask <- is.na(bio1)
n_land <- global(land_mask, "sum", na.rm=TRUE)[,1]
cat(sprintf("  Land cells: %d\n", n_land))

# Distance to ocean
dist_file <- file.path(out_dir, "distance_to_nearest_ocean_cell_km.tif")
if (!file.exists(dist_file)) {
  ocean_tgt <- ocean_mask; ocean_tgt[!ocean_mask] <- NA
  cat("  Computing distance to ocean...\n")
  dist_km <- distance(ocean_tgt, unit="km", filename=dist_file,
    overwrite=TRUE, datatype="FLT4S", gdal=c("COMPRESS=LZW","TILED=YES"))
} else {
  dist_km <- rast(dist_file)
}
cat(sprintf("  Distance range: [%.1f, %.1f] km\n",
  global(dist_km,"min",na.rm=TRUE)[,1], global(dist_km,"max",na.rm=TRUE)[,1]))

# Coastal targets (land within search radius)
coastal_idx <- which(values(land_mask)==1 & values(dist_km) <= MAX_SEARCH_KM)
coastal_xy  <- xyFromCell(bio1, coastal_idx)
n_coastal <- length(coastal_idx)
cat(sprintf("  Coastal targets (<=%d km): %d\n", MAX_SEARCH_KM, n_coastal))

# ═══════════════════════════════════════════════════════════════════════
# 2. LATITUDE BANDS (non-overlapping ownership)
# ═══════════════════════════════════════════════════════════════════════
cat("\n[2/6] Defining lat bands...\n")
lat_seq <- seq(EXTENT$ymin, EXTENT$ymax, by=BAND_W_DEG)
n_bands <- length(lat_seq)
bands <- data.frame(
  band_id = seq_len(n_bands),
  lat_min = lat_seq,
  lat_max = c(lat_seq[-1], EXTENT$ymax + 0.001))
target_band <- findInterval(coastal_xy[,2], bands$lat_min)
cat(sprintf("  %d bands, targets: %s\n", n_bands, paste(table(target_band), collapse=", ")))

# ═══════════════════════════════════════════════════════════════════════
# 3. MARINE SOURCE INDICES
# ═══════════════════════════════════════════════════════════════════════
cat("\n[3/6] Building source indices...\n")
src_data <- list()
for (ml in MARINE) {
  r <- crop(rast(file.path(HYCOM_DIR, paste0(ml, ".tif"))), EXTENT)
  src_mask <- values(ocean_mask)==1 & !is.na(values(r))
  src_data[[ml]] <- list(
    idx  = which(src_mask),
    xy   = xyFromCell(r, which(src_mask)),
    vals = values(r)[which(src_mask)],
    raster = r
  )
  cat(sprintf("  %s: %d sources\n", ml, length(src_data[[ml]]$idx)))
}

# ═══════════════════════════════════════════════════════════════════════
# 4. PROCESS BANDS
# ═══════════════════════════════════════════════════════════════════════
cat("\n[4/6] Processing bands...\n")

for (ml in MARINE) {
  cat(sprintf("\n--- %s ---\n", ml))
  sd <- src_data[[ml]]
  
  nearest_val <- rast(bio1); values(nearest_val) <- NA_real_
  source_cell <- rast(bio1); values(source_cell) <- NA_integer_
  source_dist <- rast(bio1); values(source_dist) <- NA_real_
  
  for (bi in seq_len(n_bands)) {
    b <- bands[bi,]
    tinb <- which(target_band == bi)
    if (length(tinb) == 0) next
    t_xy <- coastal_xy[tinb, , drop=FALSE]; n_t <- length(tinb)
    
    # Checkpoint
    cp_file <- file.path(CHECKPOINT_DIR, sprintf("%s_band_%03d.RDS", ml, bi))
    if (file.exists(cp_file)) {
      cp <- tryCatch(readRDS(cp_file), error=function(e) NULL)
      if (!is.null(cp) && is.list(cp) && identical(cp$n_target, n_t) &&
          !is.null(cp$checksum_vals)) {
        cat(sprintf("  Band %d/%d: checkpoint OK (%d targets)\n", bi, n_bands, n_t))
        nearest_val[coastal_idx[tinb]] <- cp$nearest_vals
        source_cell[coastal_idx[tinb]] <- cp$source_cells
        source_dist[coastal_idx[tinb]] <- cp$source_dists
        next
      }
    }
    
    # Source expansion
    mean_lat <- mean(abs(t_xy[,2])) * pi/180
    lon_exp  <- max(SRC_EXP_DEG, SRC_EXP_LON_FACTOR / max(cos(mean_lat), 0.1))
    src_lat_ok <- sd$xy[,2] >= b$lat_min - SRC_EXP_DEG & sd$xy[,2] <= b$lat_max + SRC_EXP_DEG
    
    # Date-line aware longitude wrap (0-360)
    t_lon_w <- ifelse(t_xy[,1] < 0, t_xy[,1] + 360, t_xy[,1])
    s_lon_w <- ifelse(sd$xy[,1] < 0, sd$xy[,1] + 360, sd$xy[,1])
    t_lon_min <- min(t_lon_w); t_lon_max <- max(t_lon_w)
    
    if (t_lon_max - t_lon_min > 180) {
      src_lon_ok <- s_lon_w >= t_lon_min - lon_exp & s_lon_w <= t_lon_max + lon_exp
    } else {
      src_lon_ok <- s_lon_w >= t_lon_min - lon_exp & s_lon_w <= t_lon_max + lon_exp
    }
    sinb <- which(src_lat_ok & src_lon_ok)
    
    if (length(sinb) == 0) {
      cat(sprintf("  Band %d/%d: NO SOURCES in expanded range — writing error checkpoint\n", bi, n_bands))
      saveRDS(list(error="no_sources", n_target=n_t), cp_file); next
    }
    
    # sf nearest-feature
    t_sf <- st_as_sf(data.frame(lon=t_lon_w, lat=t_xy[,2]), coords=c("lon","lat"), crs=4326)
    s_sf <- st_as_sf(data.frame(lon=s_lon_w[sinb], lat=sd$xy[sinb,2]), coords=c("lon","lat"), crs=4326)
    nn <- st_nearest_feature(t_sf, s_sf)
    
    # Map to raster cells (CRITICAL: sinb[nn] maps vector index → original src_idx)
    src_cells_mapped <- sd$idx[sinb[nn]]
    nearest_vals_mapped <- sd$vals[sinb[nn]]
    src_dists_mapped <- distGeo(t_xy, sd$xy[sinb[nn], , drop=FALSE]) / 1000
    
    # Filter beyond search radius
    too_far <- src_dists_mapped > MAX_SEARCH_KM
    if (any(too_far)) {
      nearest_vals_mapped[too_far] <- NA_real_
      src_cells_mapped[too_far] <- NA_integer_
      src_dists_mapped[too_far] <- NA_real_
    }
    
    nearest_val[coastal_idx[tinb]] <- nearest_vals_mapped
    source_cell[coastal_idx[tinb]] <- src_cells_mapped
    source_dist[coastal_idx[tinb]] <- src_dists_mapped
    
    # Checkpoint (atomically via temp file)
    cp <- list(n_target=n_t, nearest_vals=nearest_vals_mapped,
      source_cells=src_cells_mapped, source_dists=src_dists_mapped,
      checksum_vals=digest::digest(nearest_vals_mapped), timestamp=Sys.time())
    saveRDS(cp, cp_file)
    cat(sprintf("  Band %d/%d: %d targets saved\n", bi, n_bands, n_t))
  }
  
  # Write rasters
  nm_val  <- file.path(out_dir, paste0("nearest_", ml, ".tif"))
  nm_cell <- file.path(out_dir, paste0(ml, "_source_cell.tif"))
  nm_dist <- file.path(out_dir, paste0(ml, "_source_distance_km.tif"))
  writeRaster(nearest_val, nm_val, overwrite=TRUE, datatype="FLT4S", gdal=c("COMPRESS=LZW","TILED=YES"))
  writeRaster(source_cell, nm_cell, overwrite=TRUE, datatype="INT4S", gdal=c("COMPRESS=LZW","TILED=YES"))
  writeRaster(source_dist, nm_dist, overwrite=TRUE, datatype="FLT4S", gdal=c("COMPRESS=LZW","TILED=YES"))
  cat(sprintf("  %s complete.\n", ml))
}

# ═══════════════════════════════════════════════════════════════════════
# 5. COASTAL EXPOSURE (non-NA on ALL land cells)
# ═══════════════════════════════════════════════════════════════════════
cat("\n[5/6] Coastal exposure (guaranteeing non-NA on all land)...\n")

for (ml in MARINE) {
  cat(sprintf("  %s...\n", ml))
  nv <- rast(file.path(out_dir, paste0("nearest_", ml, ".tif")))
  sd <- rast(file.path(out_dir, paste0(ml, "_source_distance_km.tif")))
  
  # Standardise using all ocean source values
  ocean_vals <- values(src_data[[ml]]$raster)[values(ocean_mask)==1 & !is.na(values(src_data[[ml]]$raster))]
  mu <- mean(ocean_vals, na.rm=TRUE); sigma <- sd(ocean_vals, na.rm=TRUE)
  if(is.na(sigma)||sigma==0) sigma <- 1
  cat(sprintf("    mu=%.3f sigma=%.3f\n", mu, sigma))
  
  nv_z <- (nv - mu) / sigma
  ml_short <- gsub("hycom_|_decoded", "", ml)
  
  for (d in LAMBDAS) {
    c3 <- 3*d; c5 <- 5*d
    
    # Weight: exp(-dist/lambda) with cosine taper
    w <- exp(-sd / d)
    taper_mask <- (!is.na(values(sd)) & values(sd) > c3 & values(sd) < c5)
    w[taper_mask] <- w[taper_mask] * 0.5 * (1 + cos(pi * (values(sd)[taper_mask] - c3) / (2*d)))
    # Beyond 5*lambda → 0 (NOT NA)
    w[!is.na(values(sd)) & values(sd) >= c5] <- 0
    # Land cells beyond max search with no source: set weight=0
    inland_no_source <- is.na(values(sd)) & values(land_mask)==1 & values(dist_km) > MAX_SEARCH_KM
    w[inland_no_source] <- 0
    
    exposure <- nv_z * w
    # Land cells with NA exposure but on valid land: set to 0
    exposure[is.na(values(exposure)) & values(land_mask)==1] <- 0
    
    writeRaster(w, file.path(out_dir, sprintf("coastal_decay_%dkm.tif", d)),
      overwrite=TRUE, datatype="FLT4S", gdal=c("COMPRESS=LZW","TILED=YES"))
    writeRaster(exposure, file.path(out_dir, sprintf("%s_exposure_%dkm.tif", ml_short, d)),
      overwrite=TRUE, datatype="FLT4S", gdal=c("COMPRESS=LZW","TILED=YES"))
    
    # Validate ALL land cells
    land_w <- values(w)[values(land_mask)==1]
    land_exp <- values(exposure)[values(land_mask)==1]
    n_na_w <- sum(is.na(land_w)); n_na_exp <- sum(is.na(land_exp))
    n_zero_w <- sum(!is.na(land_w) & land_w==0)
    cat(sprintf("    lambda=%d: decay NA=%d zero=%d exp NA=%d\n", d, n_na_w, n_zero_w, n_na_exp))
  }
}

# ═══════════════════════════════════════════════════════════════════════
# 6. VALIDATION
# ═══════════════════════════════════════════════════════════════════════
cat("\n[6/6] Validation...\n")

# All-land coverage for modelling layers
cat("\n--- All-land coverage validation ---\n")
cat("layer,total_land,non_NA_land,NA_land,zero_land,coverage_pct,unexpected_NA,pass\n")

check_layers <- c("distance_to_nearest_ocean_cell_km.tif",
  paste0("coastal_decay_", LAMBDAS, "km.tif"),
  paste0("salinity_mean_exposure_", rep(LAMBDAS, each=1), "km.tif"),
  paste0("temperature_mean_exposure_", rep(LAMBDAS, each=1), "km.tif"))

for (lyr in check_layers) {
  fp <- file.path(out_dir, lyr)
  if (!file.exists(fp)) next
  r <- rast(fp)
  land_vals <- values(r)[values(land_mask)==1]
  n_na <- sum(is.na(land_vals))
  n_zero <- sum(!is.na(land_vals) & land_vals==0)
  n_non_na <- sum(!is.na(land_vals))
  cov_pct <- round(100*n_non_na/n_land, 2)
  pass <- n_na == 0
  cat(sprintf("%s,%d,%d,%d,%d,%.2f,%d,%s\n",
    lyr, n_land, n_non_na, n_na, n_zero, cov_pct, n_na, if(pass)"PASS"else"FAIL"))
}

# Inland spot check (>500 km from coast)
cat("\n--- Inland spot check (>500 km from coast) ---\n")
inland_idx <- which(values(land_mask)==1 & values(dist_km) > 500)
if (length(inland_idx) > 0) {
  check_idx <- sample(inland_idx, min(10, length(inland_idx)))
  for (lyr in c("coastal_decay_100km.tif", "salinity_mean_exposure_100km.tif")) {
    r <- rast(file.path(out_dir, lyr))
    vals <- values(r)[check_idx]
    cat(sprintf("  %s at %d inland points: all_zero=%s all_non_NA=%s\n",
      lyr, length(check_idx),
      if(all(!is.na(vals)&vals==0))"yes"else"NO",
      if(all(!is.na(vals)))"yes"else"NO"))
  }
}

t1 <- Sys.time()
cat(sprintf("\nTotal runtime: %.1f min\n", difftime(t1,t0,units="mins")))
cat(sprintf("Output: %s\n", out_dir))
cat("\n===== DONE =====\n")
