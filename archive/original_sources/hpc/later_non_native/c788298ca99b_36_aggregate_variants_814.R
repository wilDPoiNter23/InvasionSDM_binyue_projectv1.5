# Original source: 36_aggregate_variants_814.R
# Original source location: ${NON_NATIVE_ROOT}/02_derived_non_native_potential/07_scripts/36_aggregate_variants_814.R
# Original source SHA-256: c788298ca99b7f30ba6014a27d2b43d1766786f6f5086ae7da02662351e6eabf
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_NON_NATIVE)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# 36_aggregate_variants_814.R — the two aggregation variants over 814 taxa
# (813 frozen + Solanum rostratum). No min-max. Native domains stay NA (never 0);
# per-cell mean over non-NA taxa; NA only where no taxon has a value.
#   A) tss_binary_suitable_taxon_share_non_native_814.tif
#   B) ensemble_mean_suitability_non_native_814.tif
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
taxa <- c(aud$accepted_taxon, "Solanum rostratum")
stopifnot(length(taxa) == 814)

acc <- function(paths) {
  s <- NULL; c0 <- NULL
  for (i in seq_along(paths)) {
    r <- rast(paths[i])
    if (is.null(s)) { s <- r; c0 <- ifel(is.na(r), 0, 1) }
    else { s <- s + ifel(is.na(r), 0, r); c0 <- c0 + ifel(is.na(r), 0, 1) }
    if (i %% 100 == 0) cat(sprintf("  [%d/%d]\n", i, length(paths)))
  }
  out <- s / c0; out[c0 == 0] <- NA
  list(mean_r = out, count = c0)
}

pathsA <- file.path(BINDIR, paste0(tsafe(taxa), "__tss_binary_non_native.tif"))
pathsB <- file.path(CONTDIR, paste0(tsafe(taxa), "__ensemble_suitability_non_native.tif"))
stopifnot(all(file.exists(pathsA)), all(file.exists(pathsB)))

cat("=== Variant A: all-binary (TSS), 814 taxa ===\n")
A <- acc(pathsA)
pA <- file.path(OUTDIR, "tss_binary_suitable_taxon_share_non_native_814.tif")
writeRaster(A$mean_r, pA, overwrite = TRUE, datatype = "FLT4S", NAflag = -3.4e+38,
            gdal = c("COMPRESS=LZW", "PREDICTOR=2", "TILED=YES", "BIGTIFF=YES"))
qcA <- data.frame(product = "tss_binary_suitable_taxon_share_non_native_814",
  definition = "per-cell mean of TSS-binary suitability across 814 taxa (0-1 share of taxa predicted suitable); native domains NA (not 0); no min-max",
  input_layers = 814, input_type = "TSS binary (0/1), native -> NA",
  min = minmax(A$mean_r)[1], max = minmax(A$mean_r)[2],
  mean = global(A$mean_r, "mean", na.rm = TRUE)[1, 1],
  non_NA_cells = global(ifel(is.na(A$mean_r), 0, 1), "sum")[1, 1],
  sha256 = sha256_of(pA), stringsAsFactors = FALSE)
write.csv(qcA, file.path(OUTDIR, "tss_binary_suitable_taxon_share_non_native_814_QC.csv"), row.names = FALSE)
print(qcA)

cat("=== Variant B: continuous (0-1) mean, 814 taxa ===\n")
B <- acc(pathsB)
pB <- file.path(OUTDIR, "ensemble_mean_suitability_non_native_814.tif")
writeRaster(B$mean_r, pB, overwrite = TRUE, datatype = "FLT4S", NAflag = -3.4e+38,
            gdal = c("COMPRESS=LZW", "PREDICTOR=2", "TILED=YES", "BIGTIFF=YES"))
qcB <- data.frame(product = "ensemble_mean_suitability_non_native_814",
  definition = "per-cell mean of continuous ensemble suitability (0-1) across 814 taxa; native domains NA (not 0); scale-restored (no min-max)",
  input_layers = 814, input_type = "continuous ensemble suitability (0-1), native -> NA",
  min = minmax(B$mean_r)[1], max = minmax(B$mean_r)[2],
  mean = global(B$mean_r, "mean", na.rm = TRUE)[1, 1],
  non_NA_cells = global(ifel(is.na(B$mean_r), 0, 1), "sum")[1, 1],
  sha256 = sha256_of(pB), stringsAsFactors = FALSE)
write.csv(qcB, file.path(OUTDIR, "ensemble_mean_suitability_non_native_814_QC.csv"), row.names = FALSE)
print(qcB)
cat("DONE\n")
