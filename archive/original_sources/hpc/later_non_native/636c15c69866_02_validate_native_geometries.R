# Original source: 02_validate_native_geometries.R
# Original source location: ${NON_NATIVE_ROOT}/02_derived_non_native_potential/07_scripts/02_validate_native_geometries.R
# Original source SHA-256: 636c15c698663874b94b6f3eb3cdd99605132094e85916d295b79a55f6db26fc
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_NON_NATIVE)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# 02_validate_native_geometries.R — authoritative validation of the existing
# native_ranges_813.gpkg before masking. Outputs:
#   02_native_range_masks/qc/native_range_geometry_freeze_QC.csv
source("<NON_NATIVE_ROOT>/02_derived_non_native_potential/07_scripts/00_config.R")
suppressMessages({ library(terra); library(sf) })
sf_use_s2(FALSE)

mani  <- read.csv(FROZEN_MANIFEST_V3, stringsAsFactors = FALSE)
v <- tryCatch(vect(NATIVE_GPKG), error = function(e) stop("gpkg unreadable: ", conditionMessage(e)))

cat("rows:", nrow(v), " unique taxa:", length(unique(v$accepted_taxon)),
    " dup:", sum(duplicated(v$accepted_taxon)), "\n")
stopifnot(nrow(v) == 813, length(unique(v$accepted_taxon)) == 813)

v <- v[match(mani$accepted_taxon, v$accepted_taxon), ]
taxon_match <- v$accepted_taxon == mani$accepted_taxon
cat("manifest order match:", all(taxon_match), "\n")

s <- st_as_sf(v)
geom_present <- !is.na(v$accepted_taxon)
geom_empty   <- st_is_empty(s)

# vectorised single-pass validation (fast path over the whole layer)
cat("running st_is_valid over all 813 ...\n")
g_valid  <- suppressWarnings(st_is_valid(s, NA_on_exception = TRUE))
crs_ok   <- !is.na(st_crs(s))
cat("computing areas ...\n")
area_km2 <- as.numeric(terra::expanse(v, unit = "km"))
n_countries <- v$n_native_countries

# suspicious global extent: area > 70% of Earth's land (~149M km2 * 0.7 ≈ 104M)
suspicious <- !is.na(area_km2) & area_km2 > 104e6

qc <- data.frame(
  accepted_taxon = v$accepted_taxon,
  geometry_present = geom_present,
  geometry_valid = g_valid,
  geometry_empty = geom_empty,
  n_native_countries = n_countries,
  area_km2 = round(area_km2, 1),
  manifest_taxon_match = taxon_match,
  suspicious_global_extent = suspicious,
  QC_pass = geom_present & !geom_empty & (is.na(g_valid) | g_valid) & taxon_match & !suspicious,
  notes = ifelse(suspicious, "SUSPICIOUS_GLOBAL_EXTENT",
          ifelse(geom_empty, "EMPTY_GEOMETRY (MISSING_NATIVE_MAPPING)",
          ifelse(is.na(g_valid) | !g_valid, "INVALID_GEOMETRY",
          ifelse(!taxon_match, "TAXON_MISMATCH", "")))),
  stringsAsFactors = FALSE)
qc$notes[is.na(qc$notes)] <- ""

write.csv(qc, file.path(DIR_MASK_QC, "native_range_geometry_freeze_QC.csv"), row.names = FALSE)
cat("QC rows:", nrow(qc), " PASS:", sum(qc$QC_pass), " FAIL:", sum(!qc$QC_pass), "\n")
print(table(qc$notes))
cat("area summary km2:\n"); print(summary(area_km2[!is.na(area_km2)]))
cat("empty-geometry taxa:", paste(v$accepted_taxon[geom_empty], collapse = "; "), "\n")
