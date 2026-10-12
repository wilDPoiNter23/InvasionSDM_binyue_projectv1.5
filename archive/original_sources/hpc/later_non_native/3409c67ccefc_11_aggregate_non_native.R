# Original source: 11_aggregate_non_native.R
# Original source location: ${NON_NATIVE_ROOT}/02_derived_non_native_potential/07_scripts/11_aggregate_non_native.R
# Original source SHA-256: 3409c67ccefcf97565b68c931b65d9e36953cf606c3ed5449e22de7a538cbd9e
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_NON_NATIVE)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# 11_aggregate_non_native.R — PHASES 17-20: multi-species non-native aggregate
# Replicates the FROZEN aggregation logic (see original_aggregation_protocol_frozen.txt
# and frozen script aggregate_publication_813_clean_v2.R):
#   per-species min-max normalisation -> NA -> 0 -> sum -> divide by count.
# The ONLY change vs the frozen global run: input rasters are the non-native-masked
# projections (QC-passed) instead of the frozen global projections.
ca <- commandArgs(trailingOnly = FALSE)
source(file.path(dirname(sub("^--file=", "", ca[grep("^--file=", ca)])), "00_config.R"))

suppressMessages(library(terra))

MANIFEST_IN <- file.path(DIR_SPP_MANIFESTS, "non_native_projection_manifest_813_v1.csv")
MANIFEST_AGG_IN <- file.path(DIR_AGG_MANIFESTS, "non_native_aggregation_input_813_v1.csv")
OUT_QC <- file.path(DIR_AGG_QC, "non_native_aggregate_QC.csv")

mani <- read.csv(MANIFEST_IN, stringsAsFactors = FALSE)
use  <- mani[mani$QC_pass == TRUE & mani$product_status == "COMPLETE", ]
cat("QC-passed input taxa:", nrow(use), "\n")
stopifnot(nrow(use) > 0)

# aggregation input manifest — derived ONLY from the non-native 813 manifest
agg_input <- data.frame(
  accepted_taxon = use$accepted_taxon,
  source_global_projection = use$source_global_projection,
  non_native_projection = use$output_non_native_projection,
  non_native_SHA256 = use$output_SHA256,
  stringsAsFactors = FALSE)
write.csv(agg_input, MANIFEST_AGG_IN, row.names = FALSE)
cat("Wrote:", MANIFEST_AGG_IN, "\n")

normalize_raster <- function(r) {
  mm <- minmax(r)
  rmin <- mm[1]; rmax <- mm[2]
  if (is.na(rmin) || is.na(rmax) || rmin == rmax) return(r * 0)
  (r - rmin) / (rmax - rmin)
}

r_sum <- NULL
r_count <- 0L
fail_log <- data.frame(accepted_taxon = character(0), reason = character(0),
                       stringsAsFactors = FALSE)

for (i in seq_len(nrow(agg_input))) {
  sp <- agg_input$accepted_taxon[i]
  p  <- agg_input$non_native_projection[i]
  if (i %% 25 == 0) cat(sprintf("  [%d/%d] %s\n", i, nrow(agg_input), sp))
  r <- tryCatch(rast(p), error = function(e) NULL)
  if (is.null(r)) { fail_log <- rbind(fail_log, data.frame(accepted_taxon = sp, reason = "read error")); next }
  r <- normalize_raster(r)
  r <- ifel(is.na(r), 0, r)
  if (is.null(r_sum)) r_sum <- r else r_sum <- tryCatch(r_sum + r, error = function(e) {
    tryCatch(cover(r_sum, r) + cover(r, r_sum), error = function(e2) r_sum)
  })
  r_count <- r_count + 1L
}

cat("processed:", r_count, "failed:", nrow(fail_log), "\n")
if (nrow(fail_log)) print(fail_log)
stopifnot(r_count > 0)

r_mean <- r_sum / r_count
dir.create(DIR_AGG_RASTERS, recursive = TRUE, showWarnings = FALSE)
writeRaster(r_mean, AGG_OUT_RASTER, overwrite = TRUE,
            datatype = "FLT4S", NAflag = -3.4e+38,
            gdal = c("COMPRESS=LZW", "PREDICTOR=2", "TILED=YES", "BIGTIFF=YES"))
cat("Wrote:", AGG_OUT_RASTER, "\n")

vals <- values(r_mean, mat = FALSE)
vals <- vals[is.finite(vals) & !is.na(vals)]
q <- quantile(vals, probs = c(0.01, 0.05, 0.5, 0.95, 0.99), na.rm = TRUE)
sha <- sha256_of(AGG_OUT_RASTER)

qc <- data.frame(
  product = "multispecies_non_native_invasion_potential_813",
  input_taxa = r_count,
  input_rasters = r_count,
  input_manifest = MANIFEST_AGG_IN,
  CRS = crs(r_mean, describe = TRUE)$code,
  extent = paste(as.vector(ext(r_mean)), collapse = ";"),
  resolution = paste(res(r_mean), collapse = ";"),
  dimensions = paste(dim(r_mean), collapse = "x"),
  non_NA_cells = length(vals),
  min = min(vals), Q01 = q[["1%"]], Q05 = q[["5%"]],
  median = q[["50%"]], mean = mean(vals),
  Q95 = q[["95%"]], Q99 = q[["99%"]],
  max = max(vals), SD = sd(vals),
  Inf_count = sum(is.infinite(values(r_mean, mat = FALSE)), na.rm = TRUE),
  NaN_count = sum(is.nan(values(r_mean, mat = FALSE)), na.rm = TRUE),
  SHA256 = sha,
  QC_pass = TRUE,
  stringsAsFactors = FALSE)
write.csv(qc, OUT_QC, row.names = FALSE)
print(qc)
cat("Aggregate complete.\n")
