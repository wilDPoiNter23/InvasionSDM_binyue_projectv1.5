# Original source: 06f_rebuild_native_ranges_vectorised.R
# Original source location: ${NON_NATIVE_ROOT}/02_derived_non_native_potential/07_scripts/06f_rebuild_native_ranges_vectorised.R
# Original source SHA-256: 95b4ffafa215cee000edd8f3e44891859ebedc67e7727e77dfbae5d7da9a7872
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_NON_NATIVE)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# 06f_rebuild_native_ranges_vectorised.R — fully vectorised, no per-taxon loop
# One taxon may be stored as MULTIPLE features (no dissolve; mask semantics identical).
# Vectorised: all country features subset once, all L3 fallback features subset once.
source("<NON_NATIVE_ROOT>/02_derived_non_native_potential/07_scripts/00_config.R")
suppressMessages({ library(terra); library(sf) })
sf_use_s2(FALSE)

clean <- read.csv(file.path(DIR_INPUTS, "species_native_mapping_clean.csv"), stringsAsFactors = FALSE)
countries <- vect(COUNTRY_GPKG)
w3 <- vect(WGSRPD_L3_SHP)
w3$l3_name <- trimws(w3$LEVEL3_NAM)

# ---- expand taxon -> codes (base R) ----
taxa <- clean$accepted_taxon
codes_list <- strsplit(ifelse(is.na(clean$native_country_code), "", clean$native_country_code), ";")
codes_list <- lapply(codes_list, function(x) trimws(x[x != ""]))
n_per <- lengths(codes_list)
taxon_rep <- rep(taxa, n_per)
code_rep  <- unlist(codes_list, use.names = FALSE)
n_countries_per_taxon <- n_per[match(taxon_rep, taxa)]
stmap_rep <- clean$mapping_status[match(taxon_rep, taxa)]
notes_rep <- clean$notes[match(taxon_rep, taxa)]
collapsed_codes <- vapply(codes_list, function(x) paste(sort(unique(x)), collapse = ";"), character(1))
cat("expanded code rows:", length(code_rep), "\n")

iso_in <- code_rep %in% countries$iso3
iso_idx <- match(code_rep, countries$iso3)

# ---- country features (one big subset) ----
cfeat <- countries[iso_idx[iso_in], ]
cfeat$accepted_taxon <- taxon_rep[iso_in]
cfeat$n_native_countries <- n_countries_per_taxon[iso_in]
cfeat$native_country_codes <- collapsed_codes[taxon_rep[iso_in]]
cfeat$mapping_source <- stmap_rep[iso_in]
cfeat$notes <- notes_rep[iso_in]
cat("country features:", nrow(cfeat), "\n")

# ---- L3 fallback features (codes absent from country layer) ----
miss <- !iso_in
l3_feats <- NULL
if (any(miss)) {
  # token -> L3 names from crosswalk
  cw <- read.csv(file.path(DIR_INPUTS, "native_country_boundary_crosswalk.csv"), stringsAsFactors = FALSE)
  tok_l3 <- tapply(cw$native_country_standardised[!is.na(cw$native_country_standardised)],
                   cw$native_country_original[!is.na(cw$native_country_standardised)],
                   function(x) unique(x))
  tok_l3 <- unlist(lapply(tok_l3, `[`, 1))
  l3_name_for <- unname(tok_l3[code_rep[miss]])
  l3_idx <- match(l3_name_for, w3$l3_name)
  ok3 <- !is.na(l3_idx)
  if (any(ok3)) {
    f <- w3[l3_idx[ok3], ]
    f$accepted_taxon <- taxon_rep[miss][ok3]
    f$n_native_countries <- n_countries_per_taxon[miss][ok3]
    f$native_country_codes <- collapsed_codes[taxon_rep[miss][ok3]]
    f$mapping_source <- stmap_rep[miss][ok3]
    f$notes <- paste(notes_rep[miss][ok3], "L3_FALLBACK")
    l3_feats <- f
    cat("L3 fallback features:", nrow(f), "\n")
  } else {
    cat("L3 fallback features: 0\n")
  }
}

# ---- combine and write ----
keep <- c("accepted_taxon","n_native_countries","native_country_codes",
          "mapping_source","notes")
main <- if (!is.null(l3_feats)) rbind(cfeat[, keep], l3_feats[, keep]) else cfeat[, keep]
cat("total features:", nrow(main), " unique taxa:", length(unique(main$accepted_taxon)), "\n")
writeVector(main, NATIVE_GPKG, overwrite = TRUE)
cat("written.\n")

# ---- append empty rows for missing-mapping taxa ----
missing_taxa <- taxa[n_per == 0]
if (length(missing_taxa)) {
  s <- st_read(NATIVE_GPKG, quiet = TRUE)
  empt <- do.call(rbind, lapply(missing_taxa, function(tx) {
    st_sf(data.frame(accepted_taxon = tx, n_native_countries = 0L,
                     native_country_codes = "", mapping_source = "MISSING_NATIVE_MAPPING",
                     notes = "introduced-only distribution in source", stringsAsFactors = FALSE),
          geometry = st_sfc(st_multipolygon(), crs = st_crs(s)))
  }))
  for (col in setdiff(names(s), names(empt))) empt[[col]] <- NA
  empt <- empt[, names(s)]
  s2 <- rbind(s, empt)
  st_write(s2, NATIVE_GPKG, delete_dsn = TRUE, quiet = TRUE)
  cat("appended", length(missing_taxa), "empty rows\n")
}

chk <- vect(NATIVE_GPKG)
cat("verify features:", nrow(chk), " unique taxa:", length(unique(chk$accepted_taxon)),
    " cols:", paste(names(chk), collapse = ","), "\n")
cat("DONE\n")
