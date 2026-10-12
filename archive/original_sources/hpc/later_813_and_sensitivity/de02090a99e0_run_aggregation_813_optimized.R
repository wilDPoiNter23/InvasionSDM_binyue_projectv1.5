# Original source: run_aggregation_813_optimized.R
# Original source location: ${WORK_ROOT}/archive/v1_deprecated/run_aggregation_813_optimized.R
# Original source SHA-256: de02090a99e0fa450dd66c908d1a313bd18c32f5dd7590129716579146b584ec
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/usr/bin/env Rscript
# OPTIMIZED aggregation for 813 species - uses direct path, no recursive walk
renv_lib <- "<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu"
.libPaths(c(renv_lib, .libPaths()))
library(terra)

cat("Starting optimized 813-species aggregation\n")
cat("Time:", format(Sys.time()), "\n")

base_dir <- "<WORK_ROOT>"
proj_dir <- file.path(base_dir, "results", "projections")
model_dir <- file.path(base_dir, "models")
summary_csv <- file.path(base_dir, "results/evaluations/projection_summary.csv")
output_dir <- file.path(base_dir, "results", "aggregation_813_final_20260729")
dir.create(output_dir, showWarnings=FALSE, recursive=TRUE)

summary_df <- read.csv(summary_csv)
summary_df$species_dot <- gsub(" ", ".", summary_df$species)
cat("Species:", nrow(summary_df), "\n")

# Log to CSV
log_con <- file(file.path(output_dir, "aggregation_log.csv"), "w")
cat("species,type,status,reason\n", file=log_con)

normalize_raster <- function(r) {
  mm <- terra::minmax(r)
  if (is.na(mm[1]) || is.na(mm[2]) || mm[1] == mm[2]) return(r * 0)
  (r - mm[1]) / (mm[2] - mm[1])
}

# Find raster file for a species - use DIRECT path, no recursive walk
find_raster <- function(sp_dot, proj_type) {
  proj_subdir <- file.path(model_dir, sp_dot, paste0("proj_", proj_type, "_projection"))
  if (!dir.exists(proj_subdir)) return(character(0))
  
  files <- list.files(proj_subdir, pattern="\\.(tif|grd)$", recursive=TRUE, full.names=TRUE, ignore.case=TRUE)
  
  # Exclude TSSbin, TSSfilt, ClampingMask, aux.xml
  suit <- files[!grepl("(TSSbin|TSSfilt|ClampingMask|\\.aux\\.xml)", files, ignore.case=TRUE)]
  if (length(suit) > 0) return(suit[1])
  
  # If all excluded, take any
  if (length(files) > 0) return(files[1])
  
  return(character(0))
}

r_sum <- NULL
count <- 0
fail_count <- 0
t0 <- Sys.time()

for (i in seq_len(nrow(summary_df))) {
  sp <- summary_df$species[i]
  sp_dot <- summary_df$species_dot[i]
  proj_type <- summary_df$type[i]
  
  raster_file <- find_raster(sp_dot, proj_type)
  
  if (length(raster_file) == 0) {
    fail_count <- fail_count + 1
    cat(sprintf("[%03d/%03d] FAILED %s (%s) - no raster\n", i, nrow(summary_df), sp, proj_type), file=log_con)
    next
  }
  
  r <- tryCatch(terra::rast(raster_file), error=function(e) NULL)
  if (is.null(r)) {
    fail_count <- fail_count + 1
    cat(sprintf("[%03d/%03d] FAILED %s (%s) - read error\n", i, nrow(summary_df), sp, proj_type), file=log_con)
    next
  }
  
  r <- normalize_raster(r)
  r <- terra::ifel(is.na(r), 0, r)
  
  if (is.null(r_sum)) {
    r_sum <- r
  } else {
    res <- tryCatch({
      r_sum <<- terra::cover(r_sum + r, r_sum)
      TRUE
    }, error=function(e) {
      cat(sprintf("[%03d/%03d] FAILED %s (%s) - %s\n", i, nrow(summary_df), sp, proj_type, conditionMessage(e)), file=log_con)
      FALSE
    })
    if (!res) { fail_count <- fail_count + 1; next }
  }
  
  count <- count + 1
  
  # Flush log every 50 species
  if (i %% 50 == 0 || i == nrow(summary_df)) {
    elapsed <- difftime(Sys.time(), t0, units="mins")
    cat(sprintf("[%03d/%03d] %d ok, %d fail, %.1f min elapsed\n", i, nrow(summary_df), count, fail_count, elapsed))
    flush(log_con)
  }
}

close(log_con)

# Write output
cat(sprintf("\n=== FINAL: %d successful, %d failed ===\n", count, fail_count))
if (!is.null(r_sum) && count > 0) {
  r_mean <- r_sum / count
  output_file <- file.path(output_dir, "hotspot_mean_suitability_normalized.tif")
  terra::writeRaster(r_mean, output_file, overwrite=TRUE)
  cat("Output:", output_file, "\n")
  
  # Quick validation
  cat("Raster dimensions:", paste(dim(r_mean), collapse=" x "), "\n")
  cat("CRS:", crs(r_mean), "\n")
  mm <- minmax(r_mean)
  gm <- global(r_mean, "mean", na.rm=TRUE)
  cat(sprintf("Min: %.2f, Max: %.2f, Mean: %.4f\n", mm[1], mm[2], gm[1,1]))
} else {
  cat("ERROR: No valid rasters to average\n")
}

cat("Done at:", format(Sys.time()), "\n")
