# Original source: 34_aggregate_variants.R
# Original source location: ${NON_NATIVE_ROOT}/02_derived_non_native_potential/07_scripts/34_aggregate_variants.R
# Original source SHA-256: 2304c55789e99860272e47a10c57c56616e5dc571f5489586d3f393880a68688
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_NON_NATIVE)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# 34_aggregate_variants.R — the two requested aggregation variants (no min-max;
# native domains stay NA, per-cell mean over non-NA taxa; NA only where all taxa NA)
#   A) tss_binary_suitable_taxon_share_non_native_813.tif
#      = mean of TSS-binary layers = share of taxa (0-1) predicted suitable
#   B) ensemble_mean_suitability_non_native_813.tif
#      = mean of 0-1 ensemble suitability (0-1000 divided by 1000)
ca <- commandArgs(trailingOnly = FALSE)
source(file.path(dirname(sub("^--file=", "", ca[grep("^--file=", ca)])), "00_config.R"))
suppressMessages(library(terra))

VARDIR <- file.path(DIR_AGG_RASTERS, "aggregation_variants_20260917")
BINDIR <- file.path(VARDIR, "inputs", "bin_masked")
CONTDIR <- file.path(VARDIR, "inputs", "cont_masked")
OUTDIR <- file.path(VARDIR, "aggregates")
dir.create(OUTDIR, recursive = TRUE, showWarnings = FALSE)

aud <- read.csv(file.path(DIR_TABLES, "tssbin_manifest_audit_813.csv"), stringsAsFactors = FALSE)
tsafe <- function(x) gsub("[ .]+", "_", trimws(x))

acc <- function(paths, scale_div = 1) {
  s <- NULL; c0 <- NULL
  for (i in seq_along(paths)) {
    r <- rast(paths[i])
    if (scale_div != 1) r <- r / scale_div
    r[is.na(r)] <- NA
    if (is.null(s)) {
      s <- r; c0 <- ifel(is.na(r), 0, 1)
    } else {
      s <- s + ifel(is.na(r), 0, r)
      c0 <- c0 + ifel(is.na(r), 0, 1)
      s <- s * 1  # keep lazy
    }
    if (i %% 100 == 0) cat(sprintf("  [%d/%d]\n", i, length(paths)))
  }
  out <- s / c0
  out[c0 == 0] <- NA
  list(mean_r = out, count = c0)
}

# ---- Variant A: all binary ----
pathsA <- file.path(BINDIR, paste0(tsafe(aud$accepted_taxon), "__tss_binary_non_native.tif"))
stopifnot(all(file.exists(pathsA)))
cat("=== Variant A: all-binary (TSS) mean ===\n")
A <- acc(pathsA)
pA <- file.path(OUTDIR, "tss_binary_suitable_taxon_share_non_native_813.tif")
writeRaster(A$mean_r, pA, overwrite = TRUE, datatype = "FLT4S", NAflag = -3.4e+38,
            gdal = c("COMPRESS=LZW", "PREDICTOR=2", "TILED=YES", "BIGTIFF=YES"))
qcA <- data.frame(product = "tss_binary_suitable_taxon_share_non_native_813",
  definition = "per-cell mean of TSS-binary suitability across 813 taxa; native domains and ocean NA (not 0); no min-max; denominator = taxa with non-NA value at that cell",
  input_layers = length(pathsA), input_type = "TSS binary (0/1), native -> NA",
  min = minmax(A$mean_r)[1], max = minmax(A$mean_r)[2],
  mean = global(A$mean_r, "mean", na.rm = TRUE)[1, 1],
  non_NA_cells = global(ifel(is.na(A$mean_r), 0, 1), "sum")[1, 1],
  sha256 = sha256_of(pA), stringsAsFactors = FALSE)
write.csv(qcA, file.path(OUTDIR, "tss_binary_suitable_taxon_share_non_native_813_QC.csv"), row.names = FALSE)
print(qcA)

# ---- Variant B: all continuous /1000 ----
pathsB <- character(nrow(aud))
for (i in seq_len(nrow(aud))) {
  sp <- aud$accepted_taxon[i]
  fc <- file.path(CONTDIR, paste0(tsafe(sp), "__ensemble_suitability_non_native.tif"))
  pathsB[i] <- fc
}
stopifnot(all(file.exists(pathsB)))
cat("=== Variant B: continuous (0-1) mean ===\n")
B <- acc(pathsB, scale_div = 1)   # already 0-1 after step 33
pB <- file.path(OUTDIR, "ensemble_mean_suitability_non_native_813.tif")
writeRaster(B$mean_r, pB, overwrite = TRUE, datatype = "FLT4S", NAflag = -3.4e+38,
            gdal = c("COMPRESS=LZW", "PREDICTOR=2", "TILED=YES", "BIGTIFF=YES"))
qcB <- data.frame(product = "ensemble_mean_suitability_non_native_813",
  definition = "per-cell mean of continuous ensemble suitability (0-1) across 813 taxa; native domains and ocean NA (not 0); NO min-max; denominator = taxa with non-NA value at that cell",
  input_layers = length(pathsB), input_type = "continuous ensemble suitability (0-1), native -> NA",
  min = minmax(B$mean_r)[1], max = minmax(B$mean_r)[2],
  mean = global(B$mean_r, "mean", na.rm = TRUE)[1, 1],
  non_NA_cells = global(ifel(is.na(B$mean_r), 0, 1), "sum")[1, 1],
  sha256 = sha256_of(pB), stringsAsFactors = FALSE)
write.csv(qcB, file.path(OUTDIR, "ensemble_mean_suitability_non_native_813_QC.csv"), row.names = FALSE)
print(qcB)
cat("DONE\n")
