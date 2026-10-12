# Original source: run_aggregation_final.R
# Original source location: ${WORK_ROOT}/archive/v1_deprecated/run_aggregation_final.R
# Original source SHA-256: ebf8d07413aaa1e00a8f91a5563bd57804d70fe460db1cacd93e5ff51a2d56ec
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/usr/bin/env Rscript
# FIXED 0405Allstack.r — only change: robust raster pattern matching
renv_lib <- "<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu"
.libPaths(c(renv_lib, .libPaths()))

library(terra)
library(openxlsx)

base_dir <- "<WORK_ROOT>"
proj_dir <- file.path(base_dir, "results", "projections")
model_dir <- file.path(base_dir, "models")
summary_csv <- file.path(base_dir, "results/evaluations/projection_summary.csv")
output_dir <- file.path(base_dir, "results", "aggregation_813_final_20260729")
dir.create(output_dir, showWarnings=FALSE, recursive=TRUE)
output_mean <- file.path(output_dir, "hotspot_mean_suitability_normalized.tif")
log_file <- file.path(output_dir, "stack_failed_species_log_normalized.xlsx")

summary_df <- read.csv(summary_csv)
summary_df$species_dot <- gsub(" ", ".", summary_df$species)

cat("Starting fixed aggregation: ", nrow(summary_df), " species\n", sep="")
cat("Output: ", output_mean, "\n", sep="")

fail_log <- data.frame(species = character(), reason = character(), stringsAsFactors=FALSE)
r_sum <- NULL
count <- 0

normalize_raster <- function(r) {
  mm <- terra::minmax(r)
  rmin <- mm[1]; rmax <- mm[2]
  if (is.na(rmin) || is.na(rmax) || rmin == rmax) return(r * 0)
  return((r - rmin) / (rmax - rmin))
}

for (i in seq_len(nrow(summary_df))) {
  sp <- summary_df$species[i]
  sp_dot <- summary_df$species_dot[i]
  proj_type <- summary_df$type[i]
  
  if (i %% 20 == 0) cat(sprintf("[%03d/%03d] %d ok, %d fail\n", i, nrow(summary_df), count, nrow(fail_log)))

  search_dir <- file.path(model_dir, sp_dot)
  
  # FIXED: search for ALL .tif/.grd files (not pattern-matched to model_name)
  # Filter out TSSbin, TSSfilt, ClampingMask, aux files
  all_rasters <- list.files(search_dir, pattern="\\.(tif|grd)$", recursive=TRUE, full.names=TRUE, ignore.case=TRUE)
  suit_rasters <- all_rasters[!grepl("(TSSbin|TSSfilt|ClampingMask|\\.aux\\.xml|\\.gri$)", all_rasters, ignore.case=TRUE)]
  
  # Prefer raster in proj_{type}_projection folder
  type_rasters <- suit_rasters[grepl(paste0("proj_", proj_type), suit_rasters)]
  if (length(type_rasters) > 0) {
    tif_file <- type_rasters[1]
  } else if (length(suit_rasters) > 0) {
    tif_file <- suit_rasters[1]
  } else if (length(all_rasters) > 0) {
    tif_file <- all_rasters[1]  # fallback: take any raster
  } else {
    cat(sprintf("❌ Failed: %s (Prediction raster not found)\n", sp))
    fail_log <- rbind(fail_log, data.frame(species=sp, reason="Prediction raster not found"))
    next
  }

  r <- tryCatch(terra::rast(tif_file), error = function(e) NULL)
  if (is.null(r)) {
    cat(sprintf("❌ Failed: %s (Raster read error)\n", sp))
    fail_log <- rbind(fail_log, data.frame(species=sp, reason="Raster read error"))
    next
  }

  r <- normalize_raster(r)
  r <- terra::ifel(is.na(r), 0, r)

  if (is.null(r_sum)) {
    r_sum <- r
  } else {
    tryCatch({
      r_sum <- terra::cover(r_sum + r, r_sum)
    }, error = function(e) {
      cat(sprintf("❌ Failed: %s (Dimension mismatch: %s)\n", sp, conditionMessage(e)))
      fail_log <<- rbind(fail_log, data.frame(species=sp, reason=paste("Dimension mismatch:", conditionMessage(e))))
    })
    if (nrow(fail_log) > 0 && tail(fail_log$species, 1) == sp) next
  }

  count <- count + 1
  cat(sprintf("✅ Success: %s\n", sp))
}

# Write output
cat(sprintf("\n=== FINAL: %d successful, %d failed ===\n", count, nrow(fail_log)))

if (!is.null(r_sum) && count > 0) {
  r_mean <- r_sum / count
  terra::writeRaster(r_mean, output_mean, overwrite=TRUE)
  cat("Mean suitability saved to:", output_mean, "\n")
  
  # Validation
  cat("Dimensions:", paste(dim(r_mean), collapse=" x "), "\n")
  cat("CRS:", terra::crs(r_mean), "\n")
  mm <- terra::minmax(r_mean)
  gm <- terra::global(r_mean, "mean", na.rm=TRUE)
  cat(sprintf("Min: %.2f, Max: %.2f, Mean: %.4f\n", mm[1], mm[2], gm[1,1]))
} else {
  cat("ERROR: No valid rasters to average\n")
}

if (nrow(fail_log) > 0) {
  openxlsx::write.xlsx(fail_log, file=log_file, overwrite=TRUE)
  cat("Failures logged to:", log_file, "\n")
}

cat("Done at:", format(Sys.time()), "\n")
