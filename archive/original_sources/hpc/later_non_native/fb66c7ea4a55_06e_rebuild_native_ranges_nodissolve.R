# Original source: 06e_rebuild_native_ranges_nodissolve.R
# Original source location: ${NON_NATIVE_ROOT}/02_derived_non_native_potential/07_scripts/06e_rebuild_native_ranges_nodissolve.R
# Original source SHA-256: fb66c7ea4a5537ad9703de392357c837c5adf89fbf46b6bdcc08e3bc85f9990e
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_NON_NATIVE)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# 06e_rebuild_native_ranges_nodissolve.R — fast compact rebuild (no dissolve)
# One taxon may be stored as MULTIPLE country features (no artificial dissolve).
# Masking semantics are identical: 09 uses all features of the taxon.
# Fields per feature: accepted_taxon, n_native_countries (taxon-level, repeated),
# native_country_codes (taxon-level), mapping_source, country_name (feature-level).
source("<NON_NATIVE_ROOT>/02_derived_non_native_potential/07_scripts/00_config.R")
suppressMessages({ library(terra); library(sf) })
sf_use_s2(FALSE)

clean <- read.csv(file.path(DIR_INPUTS, "species_native_mapping_clean.csv"), stringsAsFactors = FALSE)
countries <- vect(COUNTRY_GPKG)
w3 <- vect(WGSRPD_L3_SHP)
w3$l3_name <- trimws(w3$LEVEL3_NAM)
country_names <- countries$country_name

out <- list(); empty_rows <- list()
for (i in seq_len(nrow(clean))) {
  tx    <- clean$accepted_taxon[i]
  code  <- clean$native_country_code[i]
  stmap <- clean$mapping_status[i]
  if (is.na(code) || trimws(code) == "") {
    empty_rows[[length(empty_rows) + 1]] <- tx
    next
  }
  codes <- trimws(unlist(strsplit(code, ";")))
  sub <- countries[match(codes, countries$iso3), ]
  sub <- sub[!is.na(sub), ]
  feats <- list()
  if (nrow(sub) >= 1) {
    sub$accepted_taxon <- tx
    sub$n_native_countries <- length(codes)
    sub$native_country_codes <- paste(sort(codes), collapse = ";")
    sub$mapping_source <- stmap
    sub$notes <- clean$notes[i]
    sub$country_name <- sub$country_name
    feats[[length(feats) + 1]] <- sub
  }
  # L3 fallback for codes absent from the country layer
  miss <- setdiff(codes, countries$iso3)
  if (length(miss)) {
    l3u <- trimws(unlist(strsplit(clean$native_country_standardised[i], ";")))
    idx <- which(w3$l3_name %in% l3u)
    if (length(idx)) {
      fb <- w3[idx, ]
      fb$accepted_taxon <- tx
      fb$n_native_countries <- length(codes)
      fb$native_country_codes <- paste(sort(codes), collapse = ";")
      fb$mapping_source <- stmap
      fb$notes <- paste(clean$notes[i], "L3_FALLBACK")
      fb$country_name <- paste0(fb$l3_name, "[L3]")
      feats[[length(feats) + 1]] <- fb
    }
  }
  if (length(feats)) out[[length(out) + 1]] <- do.call(rbind, feats)
  if (i %% 100 == 0) cat("built", i, "/", nrow(clean), "\n")
}
main <- do.call(rbind, out)
keep <- c("accepted_taxon","n_native_countries","native_country_codes",
          "mapping_source","notes","country_name")
main <- main[, keep]
cat("geometric features:", nrow(main), " taxa with geometry:",
    length(unique(main$accepted_taxon)), " empty taxa:", length(empty_rows), "\n")

writeVector(main, NATIVE_GPKG, overwrite = TRUE)

# append empty-geometry rows for missing-mapping taxa
if (length(empty_rows)) {
  s <- st_read(NATIVE_GPKG, quiet = TRUE)
  empt <- do.call(rbind, lapply(empty_rows, function(tx) {
    st_sf(data.frame(accepted_taxon = tx, n_native_countries = 0L,
                     native_country_codes = "", mapping_source = "MISSING_NATIVE_MAPPING",
                     notes = "", country_name = NA_character_, stringsAsFactors = FALSE),
          geometry = st_sfc(st_multipolygon(), crs = st_crs(s)))
  }))
  for (col in setdiff(names(s), names(empt))) empt[[col]] <- NA
  empt <- empt[, names(s)]
  s2 <- rbind(s, empt)
  st_write(s2, NATIVE_GPKG, delete_dsn = TRUE, quiet = TRUE)
  cat("appended", length(empty_rows), "empty rows\n")
}

chk <- vect(NATIVE_GPKG)
cat("verify: features:", nrow(chk), " unique taxa:", length(unique(chk$accepted_taxon)),
    " cols:", paste(names(chk), collapse = ","), "\n")
cat("DONE\n")
