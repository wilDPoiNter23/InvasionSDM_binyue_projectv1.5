# Original source: 35_build_rostratum_variant_inputs.R
# Original source location: ${NON_NATIVE_ROOT}/02_derived_non_native_potential/07_scripts/35_build_rostratum_variant_inputs.R
# Original source SHA-256: 43da58d1b142e41edcc937f63607a34d11bf256dbf28b158e76750589ec24cb1
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_NON_NATIVE)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# 35_build_rostratum_variant_inputs.R — Solanum rostratum (814th taxon) input
# layers for the two aggregation variants.
#   bin_masked/: EMmean thresholded at the TSS-weighted mean cutoff of the two
#                selected models (MAXNET cutoff 309 w=0.801, RF cutoff 491 w=0.808
#                -> weighted cutoff ~400 on the 0-1000 scale), native -> NA
#   cont_masked/: existing masked EMmean / 1000 (0-1), native -> NA
ca <- commandArgs(trailingOnly = FALSE)
source(file.path(dirname(sub("^--file=", "", ca[grep("^--file=", ca)])), "00_config.R"))
suppressMessages({ library(terra); library(sf) })
sf_use_s2(FALSE)

VARDIR <- file.path(DIR_AGG_RASTERS, "aggregation_variants_20260917")
BINDIR <- file.path(VARDIR, "inputs", "bin_masked")
CONTDIR <- file.path(VARDIR, "inputs", "cont_masked")
dir.create(BINDIR, recursive = TRUE, showWarnings = FALSE)
dir.create(CONTDIR, recursive = TRUE, showWarnings = FALSE)

EM <- "<WORK_ROOT>/results/Solanum_rostratum_occpatch_v1/projection/Solanum_rostratum_occpatch_EMmean.tif"
MASKED <- file.path(DIR_SPP_RASTERS, "Solanum_rostratum__non_native_suitability.tif")
ADDPOLY <- file.path(DIR_MASK_VECTORS, "native_range_addon_Solanum_rostratum.gpkg")
stopifnot(file.exists(EM), file.exists(MASKED), file.exists(ADDPOLY))

cutoff <- (309 * 0.801 + 491 * 0.808) / (0.801 + 0.808)
cat(sprintf("TSS-weighted cutoff: %.1f / 1000\n", cutoff))

# continuous input: existing masked product (0-815) / 1000
rc <- rast(MASKED) / 1000
fc <- file.path(CONTDIR, "Solanum_rostratum__ensemble_suitability_non_native.tif")
writeRaster(rc, fc, overwrite = TRUE, datatype = "FLT4S", NAflag = -3.4e+38,
            gdal = c("COMPRESS=LZW", "PREDICTOR=2", "TILED=YES", "BIGTIFF=YES"))
cat("wrote", basename(fc), " min/max:", paste(minmax(rc), collapse="/"), "\n")

# binary input: EMmean >= cutoff, then native -> NA
r <- rast(EM)
rb <- ifel(r >= cutoff, 1, 0)
poly <- vect(ADDPOLY)
psf <- st_as_sf(poly)
psf <- st_wrap_dateline(psf, options = c("WRAPDATELINE=YES"))
pv <- project(vect(psf), "EPSG:4326")
rb <- mask(rb, pv, inverse = TRUE)
fb <- file.path(BINDIR, "Solanum_rostratum__tss_binary_non_native.tif")
writeRaster(rb, fb, overwrite = TRUE, datatype = "FLT4S", NAflag = -3.4e+38,
            gdal = c("COMPRESS=LZW", "PREDICTOR=2", "TILED=YES", "BIGTIFF=YES"))
v <- values(rb, mat = FALSE, na.rm = TRUE)
cat("wrote", basename(fb), " fraction suitable among non-NA:", round(mean(v), 4), "\n")
qc <- data.frame(taxon = "Solanum rostratum", cutoff_weighted = round(cutoff, 1),
  cutoff_maxnet = 309, cutoff_rf = 491,
  bin_file = fb, bin_fraction_suitable = mean(v),
  cont_file = fc, cont_min = minmax(rc)[1], cont_max = minmax(rc)[2],
  sha_bin = sha256_of(fb), sha_cont = sha256_of(fc), stringsAsFactors = FALSE)
write.csv(qc, file.path(VARDIR, "inputs", "rostratum_variant_inputs_QC.csv"), row.names = FALSE)
print(qc)
cat("DONE\n")
