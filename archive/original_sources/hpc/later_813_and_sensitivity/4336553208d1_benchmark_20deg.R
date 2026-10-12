# Original source: benchmark_20deg.R
# Original source location: ${WORK_ROOT}/archive/v1_deprecated/benchmark_20deg.R
# Original source SHA-256: 4336553208d18bfdd7dd72859b6829ca28fdc1937d2eabcaa931cf0239e26055
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# 20-degree benchmark: sf::st_nearest_feature production method
# Tile: 110-130E, -5-15N (20°×20°, complex coastlines)
.libPaths(c("<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu", .libPaths()))
suppressPackageStartupMessages({library(terra); library(sf)})
sf_use_s2(TRUE)

FS    <- "<HPC_INPUT_ROOT>/climate/full_stack.tif"
EXT   <- ext(110, 130, -5, 15)
MARINE <- c("salinity_mean","salinity_range","temperature_mean","temperature_range")
MAX_SEARCH <- 500  # km (for lambda=100, 5*lambda=500)
BAND_W <- 3.0; BAND_OV <- 1.0; SRC_EXP <- 6.0  # degrees (> 500 km at equator)

t_total <- Sys.time()
cat("=== 20-degree benchmark (sf production method) ===\n")
cat(sprintf("Extent: %.0f-%.0fE, %.0f-%.0fN\n", EXT$xmin, EXT$xmax, EXT$ymin, EXT$ymax))

# Load + crop
fs <- rast(FS)
bio1 <- crop(fs[["wc2.1_2.5m_bio_1"]], EXT)
mr <- list()
for (ml in MARINE) mr[[ml]] <- crop(fs[[ml]], EXT)
rm(fs); gc()

land_mask <- !is.na(bio1); ocean_mask <- is.na(bio1)
ocean_tgt <- ocean_mask; ocean_tgt[!ocean_mask] <- NA
n_cells <- ncell(bio1)
n_land <- global(land_mask,"sum",na.rm=TRUE)[,1]

t_dist <- Sys.time()
dist_km <- distance(ocean_tgt, unit="km")
t_dist_d <- Sys.time()
cat(sprintf("  distance(): %.1f sec\n", difftime(t_dist_d,t_dist,units="secs")))

coastal_idx <- which(values(land_mask)==1 & values(dist_km) <= MAX_SEARCH)
coastal_xy  <- xyFromCell(bio1, coastal_idx)
n_target <- length(coastal_idx)
cat(sprintf("  Cells: %d total, %d land, %d coastal targets (<=%d km)\n",
  n_cells, n_land, n_target, MAX_SEARCH))

lat_bands <- seq(EXT$ymin, EXT$ymax, by=BAND_W)
n_bands <- length(lat_bands)
cat(sprintf("  Latitude bands: %d (%.1f deg each, %.1f deg overlap, %.1f deg source expansion)\n",
  n_bands, BAND_W, BAND_OV, SRC_EXP))

# Process each marine layer
results <- list()
for (ml in MARINE) {
  cat(sprintf("\n  %s...\n", ml))
  t_ml <- Sys.time()
  
  src_mask <- values(ocean_mask)==1 & !is.na(values(mr[[ml]]))
  src_idx  <- which(src_mask)
  src_xy   <- xyFromCell(mr[[ml]], src_idx)
  src_vals <- values(mr[[ml]])[src_idx]
  cat(sprintf("    Sources: %d\n", length(src_idx)))
  
  # Pre-build full target SF once
  target_sf <- st_as_sf(as.data.frame(coastal_xy), coords=c("x","y"), crs=4326)
  
  t_nn <- Sys.time()
  ref_cell <- integer(n_target)
  ref_dist <- numeric(n_target)
  
  for (bi in seq_along(lat_bands)) {
    bc <- lat_bands[bi]
    bmin <- bc - BAND_W/2 - BAND_OV/2; bmax <- bc + BAND_W/2 + BAND_OV/2
    tinb <- which(coastal_xy[,2] >= bmin & coastal_xy[,2] <= bmax)
    if (length(tinb)==0) next
    sinb <- which(src_xy[,2] >= bmin-SRC_EXP & src_xy[,2] <= bmax+SRC_EXP)
    
    # Handle date-line: this tile (110-130E) does not cross date line; skip wrapping
    
    t_sf <- target_sf[tinb,]
    s_sf <- st_as_sf(as.data.frame(cbind(src_xy[sinb,],v=src_vals[sinb])), coords=c("x","y"), crs=4326)
    nn <- st_nearest_feature(t_sf, s_sf)
    d_m <- as.numeric(st_distance(t_sf, s_sf[nn,], by_element=TRUE))
    
    ref_cell[tinb] <- src_idx[sinb[nn]]
    ref_dist[tinb] <- d_m / 1000
  }
  t_nn_d <- Sys.time()
  cat(sprintf("    st_nearest_feature (all bands): %.1f sec\n", difftime(t_nn_d,t_nn,units="secs")))
  
  # Write nearest-value raster to disk
  t_wr <- Sys.time()
  rv <- rast(bio1); rv[coastal_idx] <- src_vals[match(ref_cell, src_idx)]
  out_f <- file.path("<WORK_ROOT>/data/coastal_derived/benchmark_20deg",
    paste0("bm_", ml, "_nearest_value.tif"))
  dir.create(dirname(out_f), showWarnings=FALSE, recursive=TRUE)
  writeRaster(rv, out_f, overwrite=TRUE, datatype="FLT4S", gdal=c("COMPRESS=LZW","TILED=YES"))
  t_wr_d <- Sys.time()
  cat(sprintf("    writeRaster: %.1f sec\n", difftime(t_wr_d,t_wr,units="secs")))
  
  rt <- as.numeric(difftime(Sys.time(), t_ml, units="mins"))
  cat(sprintf("    Done: %.1f min\n", rt))
  
  results[[ml]] <- list(n_src=length(src_idx), n_target_banded=n_target, rt_min=rt)
}

t_total_d <- Sys.time()
rt_total <- as.numeric(difftime(t_total_d, t_total, units="mins"))

cat(sprintf("\n===== BENCHMARK COMPLETE: %.1f min =====\n", rt_total))

# Estimates for global
cat("\n--- Global scaling estimates ---\n")
n_cells_global <- 37324800
n_land_global <- 12532612
ratio_cells <- n_cells_global / n_cells
ratio_land <- n_land_global / n_land
cat(sprintf("  Cell ratio (global/20deg): %.0fx\n", ratio_cells))
cat(sprintf("  Land cell ratio: %.0fx\n", ratio_land))
cat(sprintf("  Estimated global distance(): %.0f min\n", 3.9*ratio_cells/60))
cat(sprintf("  Estimated global st_nearest_feature (36 lat bands): %.0f min\n",
  rt_total*36/n_bands))
cat(sprintf("  Estimated global disk output: ~%.0f GB\n", 0.05*ratio_land/1024*n_cells_global/n_cells*28))
