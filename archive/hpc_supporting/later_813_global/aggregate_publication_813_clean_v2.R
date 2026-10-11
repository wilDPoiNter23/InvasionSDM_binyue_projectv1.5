# SUPPORTING LATER PRODUCT: not established as submitted 2025 workflow.
# Original SHA256: 3c48c1cf1dfae518bf458e906360cf2b460d0ccc23ab3a54f4b8a2cd3d8fdb94
#!/usr/bin/env Rscript
# aggregate_publication_fullcoverage_v1.R — Final 812-taxon aggregation
# Protocol from 0405Allstack.r: per-species min-max norm → NA→0 → sum/count

suppressMessages(library(terra))

BASE <- "${WORK_ROOT}"
MANIFEST <- file.path(BASE, "publication_release_v1/manifests/final_aggregation_input_813_v2.csv")
OUT_DIR  <- file.path(BASE, "results/aggregation_publication_813_clean_v1_20260813")
AUDIT_DIR <- file.path(BASE, "publication_release_v1/audits")

dir.create(OUT_DIR, recursive=TRUE, showWarnings=FALSE)
dir.create(AUDIT_DIR, recursive=TRUE, showWarnings=FALSE)

cat("=== Full-coverage aggregation ===\n")
cat("Manifest:", MANIFEST, "\n")
cat("Output:", OUT_DIR, "\n")

# Read manifest
mani <- read.csv(MANIFEST, stringsAsFactors=FALSE)
cat(sprintf("Input taxa: %d\n", nrow(mani)))

# Normalization function
normalize_raster <- function(r) {
  mm <- terra::minmax(r)
  rmin <- mm[1]
  rmax <- mm[2]
  if (is.na(rmin) || is.na(rmax) || rmin == rmax) {
    return(r * 0)
  }
  return((r - rmin) / (rmax - rmin))
}

# Accumulators
r_sum <- NULL
r_count <- 0L
fail_log <- data.frame(species=character(0), reason=character(0), stringsAsFactors=FALSE)

for (i in seq_len(nrow(mani))) {
  sp <- mani$accepted_taxon[i]
  raster_path <- mani$final_projection_path[i]
  
  if (i %% 50 == 0) cat(sprintf("  [%d/%d] %s\n", i, nrow(mani), sp))
  
  if (!file.exists(raster_path)) {
    fail_log <- rbind(fail_log, data.frame(species=sp, reason="Raster file not found", stringsAsFactors=FALSE))
    next
  }
  
  r <- tryCatch(rast(raster_path), error=function(e) NULL)
  if (is.null(r)) {
    fail_log <- rbind(fail_log, data.frame(species=sp, reason="Raster read error", stringsAsFactors=FALSE))
    next
  }
  
  # Min-max normalize
  r <- normalize_raster(r)
  # NA → 0
  r <- ifel(is.na(r), 0, r)
  
  if (is.null(r_sum)) {
    r_sum <- r
  } else {
    r_sum <- tryCatch(r_sum + r, error=function(e) {
      # Try cover() fallback
      tryCatch(cover(r_sum, r) + cover(r, r_sum), error=function(e2) r_sum)
    })
  }
  r_count <- r_count + 1L
}

cat(sprintf("\nProcessed: %d taxa, %d failed\n", r_count, nrow(fail_log)))
if (nrow(fail_log) > 0) {
  cat("Failures:\n")
  print(fail_log)
}

# Mean
if (r_count > 0) {
  r_mean <- r_sum / r_count
  out_file <- file.path(OUT_DIR, "hotspot_mean_suitability_normalized.tif")
  writeRaster(r_mean, out_file, overwrite=TRUE, gdal=c("COMPRESS=LZW","PREDICTOR=2","TILED=YES","BIGTIFF=YES"))
  cat(sprintf("\nOutput: %s\n", out_file))
  
  # QC
  vals <- values(r_mean, na.rm=TRUE)
  sha <- system(sprintf("sha256sum %s | cut -d' ' -f1", out_file), intern=TRUE)
  qc <- data.frame(
    input_taxa = r_count,
    output_path = out_file,
    SHA256 = sha,
    CRS = crs(r_mean, describe=TRUE)$code,
    n_non_NA = length(vals),
    min = min(vals, na.rm=TRUE),
    median = median(vals, na.rm=TRUE),
    mean = mean(vals, na.rm=TRUE),
    max = max(vals, na.rm=TRUE),
    sd = sd(vals, na.rm=TRUE),
    all_NA = length(vals)==0,
    all_zero = all(vals==0),
    Inf_count = sum(is.infinite(vals)),
    NaN_count = sum(is.nan(vals)),
    row.names = NULL
  )
  write.csv(qc, file.path(AUDIT_DIR, "aggregation_publication_fullcoverage_v1_QC.csv"), row.names=FALSE)
  
  cat(sprintf("QC: non-NA=%d min=%.4f median=%.4f mean=%.4f max=%.4f sd=%.4f\n",
    qc$n_non_NA, qc$min, qc$median, qc$mean, qc$max, qc$sd))
  cat(sprintf("SHA256: %s\n", sha))
} else {
  cat("ERROR: No valid rasters processed\n")
}
