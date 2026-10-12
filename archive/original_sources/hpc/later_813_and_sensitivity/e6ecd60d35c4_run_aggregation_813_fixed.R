# Original source: run_aggregation_813_fixed.R
# Original source location: ${WORK_ROOT}/archive/v1_deprecated/run_aggregation_813_fixed.R
# Original source SHA-256: e6ecd60d35c44916a4ed30fd71f190e83892b8a85e69da931b383b5b88f865d6
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/usr/bin/env Rscript
# Fixed aggregation for 813 species
# Fix: robust raster finding that works for both ensemble and fallback projections

renv_lib <- "<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu"
.libPaths(c(renv_lib, .libPaths()))

library(terra)
library(biomod2)

cat("Starting fixed 813-species aggregation\n")
cat("Time:", format(Sys.time()), "\n")

base_dir <- "<WORK_ROOT>"
proj_dir <- file.path(base_dir, "results", "projections")
model_dir <- file.path(base_dir, "models")
summary_csv <- file.path(base_dir, "results/evaluations/projection_summary.csv")
output_dir <- file.path(base_dir, "results", "aggregation_813_final_20260729")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

output_mean <- file.path(output_dir, "hotspot_mean_suitability_normalized.tif")
log_file <- file.path(output_dir, "aggregation_log.csv")

summary_df <- read.csv(summary_csv)
summary_df$species_dot <- gsub(" ", ".", summary_df$species)

cat("CSV rows:", nrow(summary_df), "\n")
cat("Output dir:", output_dir, "\n")

# Log
agg_log <- data.frame(species=character(), type=character(), status=character(), 
                       reason=character(), stringsAsFactors=FALSE)

normalize_raster <- function(r) {
  mm <- terra::minmax(r)
  rmin <- mm[1]; rmax <- mm[2]
  if (is.na(rmin) || is.na(rmax) || rmin == rmax) return(r * 0)
  return((r - rmin) / (rmax - rmin))
}

r_sum <- NULL
count <- 0

for (i in seq_len(nrow(summary_df))) {
  sp <- summary_df$species[i]
  sp_dot <- summary_df$species_dot[i]
  proj_type <- summary_df$type[i]
  
  if (i %% 50 == 0) cat(sprintf("[%03d/%03d] %d successful, %d failed\n", i, nrow(summary_df), count, nrow(agg_log) - count))
  
  search_dir <- file.path(model_dir, sp_dot)
  if (!dir.exists(search_dir)) {
    agg_log <- rbind(agg_log, data.frame(species=sp, type=proj_type, status="FAILED", reason="Model dir not found"))
    next
  }
  
  # ROBUST raster search: find ALL .tif/.grd files, exclude aux/binary/clamping, pick the suitability raster
  all_rasters <- list.files(search_dir, pattern="\\.(tif|grd)$", recursive=TRUE, full.names=TRUE, ignore.case=TRUE)
  # Exclude auxiliary, binary, clamping mask files
  suit_rasters <- all_rasters[!grepl("(TSSbin|TSSfilt|ClampingMask|\\.aux\\.xml|\\.gri$)", all_rasters)]
  
  if (length(suit_rasters) == 0) {
    agg_log <- rbind(agg_log, data.frame(species=sp, type=proj_type, status="FAILED", reason="No suitable raster found"))
    next
  }
  
  # Prefer .tif over .grd, prefer files with "ensemble" or "fallback" in path
  tif_rasters <- suit_rasters[grepl("\\.tif$", suit_rasters)]
  if (length(tif_rasters) > 0) {
    # Take the one in the main projection folder (not Tmp)
    main_rasters <- tif_rasters[grepl(paste0("proj_", proj_type), tif_rasters)]
    if (length(main_rasters) > 0) {
      raster_file <- main_rasters[1]
    } else {
      raster_file <- tif_rasters[1]
    }
  } else {
    raster_file <- suit_rasters[1]
  }
  
  r <- tryCatch(terra::rast(raster_file), error=function(e) NULL)
  if (is.null(r)) {
    agg_log <- rbind(agg_log, data.frame(species=sp, type=proj_type, status="FAILED", reason="Raster read error"))
    next
  }
  
  # Normalize and add
  r <- normalize_raster(r)
  r <- terra::ifel(is.na(r), 0, r)
  
  if (is.null(r_sum)) {
    r_sum <- r
  } else {
    tryCatch({
      r_sum <- terra::cover(r_sum + r, r_sum)
    }, error=function(e) {
      agg_log <<- rbind(agg_log, data.frame(species=sp, type=proj_type, status="FAILED", reason=paste("Dimension mismatch:", conditionMessage(e))))
    })
    if (nrow(agg_log) > 0 && agg_log$species[nrow(agg_log)] == sp) next
  }
  
  count <- count + 1
  agg_log <- rbind(agg_log, data.frame(species=sp, type=proj_type, status="SUCCESS", reason=""))
}

# Write output
cat(sprintf("\n=== Final: %d successful, %d failed ===\n", count, nrow(agg_log) - count))

if (!is.null(r_sum) && count > 0) {
  r_mean <- r_sum / count
  terra::writeRaster(r_mean, output_mean, overwrite=TRUE)
  cat("Mean suitability saved to:", output_mean, "\n")
}

write.csv(agg_log, log_file, row.names=FALSE)
cat("Log saved to:", log_file, "\n")
cat("Done at:", format(Sys.time()), "\n")
