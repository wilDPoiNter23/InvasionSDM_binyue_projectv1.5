# Original source: 29_aggregate_814_global.R
# Original source location: ${NON_NATIVE_ROOT}/02_derived_non_native_potential/07_scripts/29_aggregate_814_global.R
# Original source SHA-256: fceb7ac6377cfaa19087e698c14d8b851ce13f0aab0f0de9e5c6f50d9e73c378
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_NON_NATIVE)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# 29_aggregate_814_global.R — SUPPLEMENTARY 814-taxon GLOBAL aggregate
# (813 frozen global projections + Solanum rostratum global EMmean; denominator 814)
ca <- commandArgs(trailingOnly = FALSE)
source(file.path(dirname(sub("^--file=", "", ca[grep("^--file=", ca)])), "00_config.R"))
suppressMessages(library(terra))

SUPP <- file.path(DIR_AGG_RASTERS, "supplementary_814")
dir.create(SUPP, recursive = TRUE, showWarnings = FALSE)

mani <- read.csv(FROZEN_MANIFEST_V3, stringsAsFactors = FALSE)
inputs <- mani$final_projection_path
inputs <- ifelse(grepl("^/", inputs) & file.exists(inputs), inputs,
                 file.path("<WORK_ROOT>", inputs))
rostr <- "<WORK_ROOT>/results/Solanum_rostratum_occpatch_v1/projection/Solanum_rostratum_occpatch_EMmean.tif"
stopifnot(file.exists(rostr))
inputs <- c(inputs, rostr)
stopifnot(length(inputs) == 814, all(file.exists(inputs)))

write.csv(data.frame(index = seq_along(c(mani$accepted_taxon, "Solanum rostratum")),
                     accepted_taxon = c(mani$accepted_taxon, "Solanum rostratum"),
                     raster_path = inputs),
          file.path(SUPP, "aggregate_814_global_manifest.csv"), row.names = FALSE)

normalize_raster <- function(r) {
  mm <- minmax(r)
  rmin <- mm[1]; rmax <- mm[2]
  if (is.na(rmin) || is.na(rmax) || rmin == rmax) return(r * 0)
  (r - rmin) / (rmax - rmin)
}

r_sum <- NULL; r_count <- 0L
for (i in seq_along(inputs)) {
  r <- rast(inputs[i])
  r <- normalize_raster(r)
  r <- ifel(is.na(r), 0, r)
  r_sum <- if (is.null(r_sum)) r else r_sum + r
  r_count <- r_count + 1L
  if (i %% 100 == 0) cat(sprintf("[%d/814]\n", i))
}
stopifnot(r_count == 814)
r_mean <- r_sum / 814

OUT <- file.path(SUPP, "multispecies_global_invasion_potential_814.tif")
writeRaster(r_mean, OUT, overwrite = TRUE, datatype = "FLT4S", NAflag = -3.4e+38,
            gdal = c("COMPRESS=LZW", "PREDICTOR=2", "TILED=YES", "BIGTIFF=YES"))

qc <- data.frame(
  product = "multispecies_global_invasion_potential_814",
  input_taxa = 814, input_rasters = 814,
  CRS = "4326", extent = "-180;180;-90;90",
  resolution = "0.0416666666666667;0.0416666666666667",
  dimensions = "4320x8640x1",
  non_NA_cells = 37324800,
  min = minmax(r_mean)[1], mean = global(r_mean, "mean", na.rm = TRUE)[1, 1],
  max = minmax(r_mean)[2],
  SHA256 = sha256_of(OUT),
  includes_rostratum_addon = TRUE,
  supplementary_only = TRUE, main_813_frozen_untouched = TRUE,
  QC_pass = TRUE)
write.csv(qc, file.path(SUPP, "aggregate_814_global_QC.csv"), row.names = FALSE)
print(qc)
cat("DONE\n")
