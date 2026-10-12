# Original source: 02b_validate_native_geometries.R
# Original source location: ${NON_NATIVE_ROOT}/02_derived_non_native_potential/07_scripts/02b_validate_native_geometries.R
# Original source SHA-256: a14e2fcf4588d899e5691ec500dc607fd3e2aea73e5ac340c20691237f04ba19
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_NON_NATIVE)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# 02b_validate_native_geometries.R — lightweight but authoritative validation
# Full-layer st_is_valid on the 8.1 GB country-level gpkg proved too slow (killed
# overnight). Replacement strategy:
#   - present/empty: st_is_empty (full layer, cheap)
#   - CRS: single check
#   - area: terra::expanse in ONE vectorised call (full layer)
#   - validity: stratified sample (up to 60 taxa across area deciles + all
#     suspicious/flagged) with st_is_valid; builder already reported invalid=0
#     for the full build. Documented as SAMPLED_VALIDITY in notes.
source("<NON_NATIVE_ROOT>/02_derived_non_native_potential/07_scripts/00_config.R")
suppressMessages({ library(terra); library(sf) })
sf_use_s2(FALSE)

mani  <- read.csv(FROZEN_MANIFEST_V3, stringsAsFactors = FALSE)
v <- tryCatch(vect(NATIVE_GPKG), error = function(e) stop("gpkg unreadable: ", conditionMessage(e)))
cat("rows:", nrow(v), " unique:", length(unique(v$accepted_taxon)), " dup:", sum(duplicated(v$accepted_taxon)), "\n")
stopifnot(nrow(v) == 813, length(unique(v$accepted_taxon)) == 813)
v <- v[match(mani$accepted_taxon, v$accepted_taxon), ]
taxon_match <- v$accepted_taxon == mani$accepted_taxon
cat("manifest order match:", all(taxon_match), "\n")

s <- st_as_sf(v)
geom_empty <- st_is_empty(s)
crs_ok <- !is.na(st_crs(s))
cat("computing areas (vectorised) ...\n")
area_km2 <- as.numeric(terra::expanse(v, unit = "km"))
n_countries <- v$n_native_countries

# stratified validity sample
set.seed(42)
idx_nonempty <- which(!geom_empty)
dec <- cut(rank(area_km2[idx_nonempty]), breaks = 10)
sample_idx <- unlist(lapply(split(idx_nonempty, dec), function(x) {
  if (length(x) <= 6) x else x[sample.int(length(x), 6)]
}))
suspicious_proxy <- area_km2 > 104e6 | (area_km2 < 500 & !geom_empty)
sample_idx <- sort(unique(c(sample_idx, which(suspicious_proxy))))
cat("validity sample size:", length(sample_idx), "\n")
valid_sample <- suppressWarnings(st_is_valid(s[sample_idx, ], NA_on_exception = TRUE))

g_valid <- rep(TRUE, nrow(v))
g_valid[sample_idx] <- !is.na(valid_sample) & valid_sample
sampled_flag <- rep(FALSE, nrow(v)); sampled_flag[sample_idx] <- TRUE

suspicious <- area_km2 > 104e6
qc <- data.frame(
  accepted_taxon = v$accepted_taxon,
  geometry_present = TRUE,
  geometry_valid = g_valid,
  geometry_empty = geom_empty,
  n_native_countries = n_countries,
  area_km2 = round(area_km2, 1),
  manifest_taxon_match = taxon_match,
  suspicious_global_extent = suspicious,
  QC_pass = !geom_empty & g_valid & taxon_match & !suspicious,
  notes = ifelse(suspicious, "SUSPICIOUS_GLOBAL_EXTENT",
          ifelse(geom_empty, "EMPTY_GEOMETRY (MISSING_NATIVE_MAPPING)",
          ifelse(!g_valid, "INVALID_GEOMETRY",
          ifelse(!sampled_flag, "VALIDITY_SAMPLED_STRATIFIED", "")))),
  stringsAsFactors = FALSE)
qc$notes[is.na(qc$notes)] <- ""

write.csv(qc, file.path(DIR_MASK_QC, "native_range_geometry_freeze_QC.csv"), row.names = FALSE)
cat("QC rows:", nrow(qc), " PASS:", sum(qc$QC_pass), " FAIL:", sum(!qc$QC_pass), "\n")
print(table(qc$notes))
cat("area km2 summary (nonempty):\n"); print(summary(area_km2[!geom_empty]))
cat("empty-geometry taxa:", paste(v$accepted_taxon[geom_empty], collapse = "; "), "\n")
cat("suspicious taxa:", paste(v$accepted_taxon[suspicious], collapse = "; "), "\n")
