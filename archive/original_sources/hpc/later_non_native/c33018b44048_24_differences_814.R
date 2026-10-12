# Original source: 24_differences_814.R
# Original source location: ${NON_NATIVE_ROOT}/02_derived_non_native_potential/07_scripts/24_differences_814.R
# Original source SHA-256: c33018b44048c726fe0daa2d2a2d5095ba1d43403842008e0fce81763ab6d98a
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_NON_NATIVE)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# 24_differences_814.R — TASK C: difference rasters only (reads finished rasters)
# Outputs: 04_aggregate/supplementary_814/differences/
#   diff_814_minus_813.tif            (Solanum rostratum marginal contribution)
#   diff_813_non_native_minus_global.tif (native-domain removal spatial effect)
ca <- commandArgs(trailingOnly = FALSE)
source(file.path(dirname(sub("^--file=", "", ca[grep("^--file=", ca)])), "00_config.R"))
suppressMessages(library(terra))

SUPP <- file.path(DIR_AGG_RASTERS, "supplementary_814")
DIFF <- file.path(SUPP, "differences")
dir.create(DIFF, recursive = TRUE, showWarnings = FALSE)

nn814 <- rast(file.path(SUPP, "multispecies_non_native_invasion_potential_814.tif"))
nn813 <- rast(AGG_OUT_RASTER)
glob  <- rast(FROZEN_AGG_RASTER)

d1 <- nn814 - nn813
d2 <- nn813 - glob
p1 <- file.path(DIFF, "diff_814_minus_813.tif")
p2 <- file.path(DIFF, "diff_813_non_native_minus_global.tif")
writeRaster(d1, p1, overwrite = TRUE, datatype = "FLT4S", NAflag = -3.4e+38,
            gdal = c("COMPRESS=LZW", "PREDICTOR=2", "TILED=YES", "BIGTIFF=YES"))
writeRaster(d2, p2, overwrite = TRUE, datatype = "FLT4S", NAflag = -3.4e+38,
            gdal = c("COMPRESS=LZW", "PREDICTOR=2", "TILED=YES", "BIGTIFF=YES"))

qc <- data.frame(
  difference = c("814_minus_813", "813_non_native_minus_global"),
  interpretation = c("Solanum rostratum marginal contribution",
                     "native-domain removal spatial effect"),
  path = c(p1, p2),
  min = c(minmax(d1)[1], minmax(d2)[1]),
  max = c(minmax(d1)[2], minmax(d2)[2]),
  mean = c(global(d1, "mean", na.rm = TRUE)[1, 1], global(d2, "mean", na.rm = TRUE)[1, 1]),
  SHA256 = c(sha256_of(p1), sha256_of(p2)))
write.csv(qc, file.path(DIFF, "differences_QC.csv"), row.names = FALSE)
print(qc)
cat("DONE\n")
