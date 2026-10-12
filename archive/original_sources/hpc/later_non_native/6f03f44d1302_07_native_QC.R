# Original source: 07_native_QC.R
# Original source location: ${NON_NATIVE_ROOT}/02_derived_non_native_potential/07_scripts/07_native_QC.R
# Original source SHA-256: 6f03f44d1302264874c2b78cfc4f2648eac133a1030813cc16aa73787c3ef184
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_NON_NATIVE)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# 07_native_QC.R — PHASES 7 & 13: native-range mapping QC + special-geography QC
ca <- commandArgs(trailingOnly = FALSE)
source(file.path(dirname(sub("^--file=", "", ca[grep("^--file=", ca)])), "00_config.R"))

suppressMessages({ library(sf); library(terra) })
sf_use_s2(FALSE)

clean  <- read.csv(file.path(DIR_INPUTS, "species_native_mapping_clean.csv"),
                   stringsAsFactors = FALSE)
cw     <- read.csv(file.path(DIR_INPUTS, "native_country_boundary_crosswalk.csv"),
                   stringsAsFactors = FALSE)
native <- st_read(NATIVE_GPKG, quiet = TRUE)

# per-taxon reconciliation flags from crosswalk
recon_used <- c("NORMALISED_NAME", "MANUAL_CONFIRMED")
tax_recon  <- tapply(cw$match_type %in% recon_used, cw$native_country_original, any)
manual_req <- tapply(cw$manual_review_required, cw$native_country_original, any)

rows <- lapply(seq_len(nrow(clean)), function(i) {
  tx   <- clean$accepted_taxon[i]
  st   <- clean$mapping_status[i]
  toks <- trimws(unlist(strsplit(clean$native_country_standardised[i], ";")))
  toks <- toks[!is.na(toks) & toks != ""]
  geom_row <- native[match(tx, native$accepted_taxon), ]
  g_created <- nrow(geom_row) == 1L
  g_nonempty <- g_created && !st_is_empty(geom_row)
  g_valid    <- g_nonempty && st_is_valid(geom_row)

  any_recon <- any(tax_recon[toks], na.rm = TRUE)
  any_man   <- any(manual_req[toks], na.rm = TRUE)
  n_country <- if (g_nonempty) geom_row$n_native_countries else 0L

  qc <- if (st == "MISSING_NATIVE_MAPPING") {
    "MISSING_MAPPING"
  } else if (st == "AMBIGUOUS_MAPPING") {
    "AMBIGUOUS"
  } else if (st == "UNMATCHED_COUNTRY") {
    "UNMATCHED_COUNTRY"
  } else if (!g_nonempty || !g_valid) {
    "AMBIGUOUS"
  } else if (any_recon) {
    "PASS_WITH_DOCUMENTED_NAME_RECONCILIATION"
  } else {
    "PASS"
  }

  data.frame(
    accepted_taxon = tx,
    mapping_found = st %in% c("EXACT_MATCH", "MULTIPLE_NATIVE_COUNTRIES", "SYNONYM_RECONCILED"),
    native_country_count = n_country,
    all_countries_matched = st != "UNMATCHED_COUNTRY",
    geometry_created = g_created,
    geometry_valid = if (g_nonempty) g_valid else NA,
    geometry_nonempty = g_nonempty,
    manual_review_required = any_man || any_recon || !g_valid || !g_nonempty ||
      (st %in% c("MISSING_NATIVE_MAPPING", "AMBIGUOUS_MAPPING", "UNMATCHED_COUNTRY")),
    QC_status = qc,
    notes = if (g_created) geom_row$notes[1] else clean$notes[i],
    stringsAsFactors = FALSE)
})
map_qc <- do.call(rbind, rows)

write.csv(map_qc, file.path(DIR_MASK_QC, "native_range_mapping_QC.csv"), row.names = FALSE)
cat("QC status counts:\n"); print(table(map_qc$QC_status))
cat("manual review required:", sum(map_qc$manual_review_required), "\n")

# ---- Phase 13: special-geography QC on the polygon layer ----
grows <- native[!st_is_empty(native), ]
g <- st_geometry(grows)

# per-taxon coarse-grid overlap estimate (1-degree grid) to flag zero/very low overlap
ref <- rast(ext(-180, 180, -90, 90), nrows = 180, ncols = 360, crs = "EPSG:4326")
native_cell_count_1deg <- integer(nrow(grows))
for (i in seq_len(nrow(grows))) {
  cov_i <- tryCatch(rasterize(vect(grows[i, ]), ref, field = 1, background = 0),
                    error = function(e) NULL)
  if (is.null(cov_i)) { native_cell_count_1deg[i] <- NA_integer_; next }
  cc <- values(cov_i, mat = FALSE)
  cc[is.na(cc)] <- 0
  native_cell_count_1deg[i] <- as.integer(sum(cc))
  if (i %% 100 == 0) cat("overlap", i, "/", nrow(grows), "\n")
}

# per-taxon special flags
geom_flags <- data.frame(
  accepted_taxon = grows$accepted_taxon,
  n_native_countries = grows$n_native_countries,
  n_geometry_parts = vapply(g, function(x) {
    if (inherits(x, "MULTIPOLYGON")) length(x) else if (inherits(x, "POLYGON")) 1L else 0L
  }, integer(1)),
  dateline_crossing = vapply(g, function(x) {
    cc <- tryCatch(st_coordinates(x), error = function(e) NULL)
    if (is.null(cc) || nrow(cc) == 0) return(FALSE)
    any(cc[, 1] > 170, na.rm = TRUE) && any(cc[, 1] < -170, na.rm = TRUE)
  }, logical(1)),
  area_km2 = as.numeric(terra::expanse(terra::vect(g), unit = "km")),
  native_cell_count_1deg = native_cell_count_1deg,
  zero_cell_overlap = !is.na(native_cell_count_1deg) & native_cell_count_1deg == 0,
  extremely_low_overlap = !is.na(native_cell_count_1deg) & native_cell_count_1deg > 0 & native_cell_count_1deg <= 5,
  unexpected_global_scale = !is.na(native_cell_count_1deg) & native_cell_count_1deg > 0.5 * 360 * 180,
  flag_notes = "",
  stringsAsFactors = FALSE)

for (i in seq_len(nrow(geom_flags))) {
  x <- geom_flags[i, ]
  notes <- c()
  if (isTRUE(x$dateline_crossing)) notes <- c(notes, "DATELINE_CROSSING")
  if (!is.na(x$zero_cell_overlap) && isTRUE(x$zero_cell_overlap)) notes <- c(notes, "ZERO_CELL_OVERLAP")
  if (isTRUE(x$extremely_low_overlap)) notes <- c(notes, "EXTREMELY_LOW_RASTER_OVERLAP")
  if (isTRUE(x$unexpected_global_scale)) notes <- c(notes, "UNEXPECTED_GLOBAL_SCALE_MASK")
  if (!is.na(x$area_km2) && isTRUE(x$n_native_countries == 1 && x$area_km2 < 500)) notes <- c(notes, "VERY_SMALL_TERRITORY")
  geom_flags$flag_notes[i] <- paste(notes, collapse = ";")
}
geom_flags$needs_review <- nzchar(geom_flags$flag_notes)

write.csv(geom_flags, file.path(DIR_MASK_QC, "native_range_geometry_QC.csv"), row.names = FALSE)
cat("dateline crossings:", sum(geom_flags$dateline_crossing), "\n")
cat("zero overlap:", sum(geom_flags$zero_cell_overlap),
    "extremely low:", sum(geom_flags$extremely_low_overlap),
    "global scale:", sum(geom_flags$unexpected_global_scale), "\n")
cat("flagged taxa:\n")
print(geom_flags[geom_flags$needs_review,
                 c("accepted_taxon", "n_native_countries", "flag_notes")], row.names = FALSE)
cat("\nWrote QC files.\n")
