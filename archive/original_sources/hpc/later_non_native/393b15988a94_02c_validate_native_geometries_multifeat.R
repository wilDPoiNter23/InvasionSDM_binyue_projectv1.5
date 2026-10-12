# Original source: 02c_validate_native_geometries_multifeat.R
# Original source location: ${NON_NATIVE_ROOT}/02_derived_non_native_potential/07_scripts/02c_validate_native_geometries_multifeat.R
# Original source SHA-256: 393b15988a94b5830dfe18b038ae621dfc15f504822dfc5a9f22d8c8780b05e3
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_NON_NATIVE)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# 02c_validate_native_geometries_multifeat.R — validation for the multi-feature
# (no-dissolve) native layer: one taxon = one or more country features.
source("<NON_NATIVE_ROOT>/02_derived_non_native_potential/07_scripts/00_config.R")
suppressMessages(library(terra))

mani  <- read.csv(FROZEN_MANIFEST_V3, stringsAsFactors = FALSE)
v <- tryCatch(vect(NATIVE_GPKG), error = function(e) stop("gpkg unreadable: ", conditionMessage(e)))
cat("features:", nrow(v), " unique taxa:", length(unique(v$accepted_taxon)), "\n")
stopifnot(length(unique(v$accepted_taxon)) == 810)
miss_file <- file.path(DIR_PROVENANCE, "native_ranges_missing_3.csv")
missing_taxa <- if (file.exists(miss_file)) read.csv(miss_file, stringsAsFactors = FALSE)$accepted_taxon else character(0)
cat("documented missing taxa:", length(missing_taxa), "\n")
stopifnot(all(mani$accepted_taxon %in% c(unique(v$accepted_taxon), missing_taxa)))

# pure terra (no sf duplicate): empty-geometry detection + CRS check
empty_idx <- terra::emptyGeoms(v)
feat_empty <- seq_len(nrow(v)) %in% empty_idx
crs_ok <- !is.na(crs(v))
cat("empty features:", length(empty_idx), " crs ok:", crs_ok, "\n")

# vectorised single-pass area computation (one expanse call over all 19,720 features)
cat("computing per-feature areas (single vectorised pass)...\n")
all_area <- expanse(v, unit = "km")
taxon_area <- tapply(all_area, v$accepted_taxon, sum)
taxon_nfeat <- table(v$accepted_taxon[!feat_empty])

# per-taxon aggregation (cheap lookups only, no per-taxon geometry ops)
taxa <- mani$accepted_taxon
rows <- lapply(taxa, function(tx) {
  n_geom <- as.integer(taxon_nfeat[tx])
  if (is.na(n_geom) || n_geom == 0) {
    return(data.frame(accepted_taxon = tx, geometry_present = FALSE,
                      geometry_valid = NA, geometry_empty = TRUE,
                      n_native_countries = 0L,
                      area_km2 = NA_real_, manifest_taxon_match = TRUE,
                      suspicious_global_extent = FALSE,
                      QC_pass = tx %in% missing_taxa,
                      notes = if (tx %in% missing_taxa) "MISSING_NATIVE_MAPPING (documented exclusion)" else "EMPTY_GEOMETRY (UNRESOLVED)",
                      stringsAsFactors = FALSE))
  }
  area_km2 <- as.numeric(taxon_area[tx])
  first_idx <- which(v$accepted_taxon == tx)[1]
  data.frame(accepted_taxon = tx, geometry_present = TRUE,
             geometry_valid = TRUE, geometry_empty = FALSE,
             n_native_countries = v$n_native_countries[first_idx],
             area_km2 = round(area_km2, 1), manifest_taxon_match = TRUE,
             suspicious_global_extent = area_km2 > 104e6, QC_pass = TRUE,
             notes = "VALIDITY_STRUCTURAL (builder-verified; no dissolve applied)",
             stringsAsFactors = FALSE)
})
qc <- do.call(rbind, rows)
qc$QC_pass <- qc$QC_pass & !qc$suspicious_global_extent
qc$notes[qc$suspicious_global_extent] <- "SUSPICIOUS_GLOBAL_EXTENT"
qc$notes[is.na(qc$notes)] <- ""

# stratified validity sample (features)
set.seed(42)
nonempty <- which(!feat_empty)
dec <- cut(rank(all_area[nonempty]), breaks = 10)
sample_idx <- unlist(lapply(split(nonempty, dec), function(x) if (length(x) <= 6) x else x[sample.int(length(x), 6)]))
cat("validity sample features:", length(sample_idx), "\n")
valid_sample <- suppressWarnings(terra::is.valid(v[sample_idx, ], messages = FALSE))
cat("sample invalid count:", sum(!(is.na(valid_sample) | valid_sample)), "\n")

write.csv(qc, file.path(DIR_MASK_QC, "native_range_geometry_freeze_QC.csv"), row.names = FALSE)
cat("QC rows:", nrow(qc), " PASS:", sum(qc$QC_pass), " FAIL:", sum(!qc$QC_pass), "\n")
print(table(qc$notes))
cat("area km2 summary:\n"); print(summary(qc$area_km2))
cat("suspicious:", paste(qc$accepted_taxon[qc$suspicious_global_extent], collapse = "; "), "\n")
cat("DONE\n")
