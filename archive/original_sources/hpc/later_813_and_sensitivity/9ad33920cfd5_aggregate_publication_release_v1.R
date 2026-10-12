# Original source: aggregate_publication_release_v1.R
# Original source location: ${WORK_ROOT}/publication_release_v1/scripts/aggregate_publication_release_v1.R
# Original source SHA-256: 9ad33920cfd594aa68ac25d25bb171aebb2faa08cd47302bf24e0616c6d0cb70
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/usr/bin/env Rscript
# aggregate_publication_release_v1.R
# Publication aggregation — replacement-only, frozen protocol from 0405Allstack.r
suppressMessages(library(terra))

BASE <- "<WORK_ROOT>"
MANIFEST <- file.path(BASE, "publication_release_v1/manifests/final_aggregation_input_manifest_v1.csv")
OUT_DIR <- file.path(BASE, "results/aggregation_publication_v1_20260807")
OUT_MEAN <- file.path(OUT_DIR, "hotspot_mean_suitability_normalized.tif")
LOG_FILE <- file.path(OUT_DIR, "failed_species_log.csv")

dir.create(OUT_DIR, recursive=TRUE, showWarnings=FALSE)
cat("Output directory:", OUT_DIR, "\n")

# Read authoritative manifest only
mani <- read.csv(MANIFEST, stringsAsFactors=FALSE)
mani <- mani[mani$include_in_aggregation %in% TRUE, ]
cat("Input species:", nrow(mani), "\n")

# ── Frozen normalization function from 0405Allstack.r ──
normalize_raster <- function(r) {
  mm <- terra::minmax(r)
  rmin <- mm[1]
  rmax <- mm[2]
  if (is.na(rmin) || is.na(rmax) || rmin == rmax) {
    return(r * 0)
  }
  return((r - rmin) / (rmax - rmin))
}

# ── Main loop ──
fail_log <- data.frame(species=character(), reason=character(), stringsAsFactors=FALSE)
r_sum <- NULL
count <- 0

for (i in seq_len(nrow(mani))) {
  sp <- mani$original_species_name[i]
  tif_path <- mani$final_projection_path[i]
  src <- mani$source_generation[i]
  
  cat(sprintf("[%03d/%03d] %s (%s)\n", i, nrow(mani), sp, src))
  
  if (is.na(tif_path) || !file.exists(tif_path)) {
    cat("  FAIL: raster not found\n")
    fail_log <- rbind(fail_log, data.frame(species=sp, reason="Raster not found", stringsAsFactors=FALSE))
    next
  }
  
  r <- tryCatch(terra::rast(tif_path), error=function(e) NULL)
  if (is.null(r)) {
    cat("  FAIL: raster read error\n")
    fail_log <- rbind(fail_log, data.frame(species=sp, reason="Raster read error", stringsAsFactors=FALSE))
    next
  }
  
  # Min-Max normalization (per original protocol)
  r <- normalize_raster(r)
  
  # NA → 0 (per original protocol)
  r <- terra::ifel(is.na(r), 0, r)
  
  # NA-safe accumulation (per original protocol)
  if (is.null(r_sum)) {
    r_sum <- r
  } else {
    tryCatch({
      r_sum <- terra::cover(r_sum + r, r_sum)
    }, error=function(e) {
      cat("  FAIL: dimension mismatch\n")
      fail_log <<- rbind(fail_log, data.frame(species=sp, reason="Dimension mismatch", stringsAsFactors=FALSE))
    })
  }
  
  count <- count + 1
  cat("  OK\n")
}

# ── Output ──
if (!is.null(r_sum) && count > 0) {
  r_mean <- r_sum / count
  terra::writeRaster(r_mean, OUT_MEAN, overwrite=TRUE)
  cat("\nMean suitability saved:", OUT_MEAN, "\n")
  cat("Denominator (species count):", count, "\n")
} else {
  cat("\nNo valid rasters to average.\n")
}

if (nrow(fail_log) > 0) {
  write.csv(fail_log, LOG_FILE, row.names=FALSE)
  cat("Failures logged:", LOG_FILE, "\n")
}

cat("Done.\n")
