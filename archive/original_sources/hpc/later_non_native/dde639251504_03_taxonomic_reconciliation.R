# Original source: 03_taxonomic_reconciliation.R
# Original source location: ${NON_NATIVE_ROOT}/02_derived_non_native_potential/07_scripts/03_taxonomic_reconciliation.R
# Original source SHA-256: dde639251504c04e75383edcf5325a1abbf899cc741b597fb9d1420fc2283e91
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_NON_NATIVE)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# 03_taxonomic_reconciliation.R — PHASE 3: join 813 manifest taxa to native mapping
ca <- commandArgs(trailingOnly = FALSE)
source(file.path(dirname(sub("^--file=", "", ca[grep("^--file=", ca)])), "00_config.R"))

mani <- read.csv(FROZEN_MANIFEST_V3, stringsAsFactors = FALSE)
mp   <- read.csv(MAPPING_CSV, stringsAsFactors = FALSE)

# Authoritative taxonomic list = manifest v3 accepted_taxon (813).
stopifnot(length(unique(mani$accepted_taxon)) == nrow(mani))

# Documented synonym reconciliations (from Phase 2 audit):
#   Spartina alterniflora -> Sporobolus alterniflorus (POWO-accepted synonym)
#   Diodia teres row is superseded by exact-match Hexasepalum teres row.
synonym_map <- c("Spartina alterniflora" = "Sporobolus alterniflorus")

clean <- data.frame(
  accepted_taxon          = mani$accepted_taxon,
  original_name_in_mapping = NA_character_,
  taxonomic_match_type    = NA_character_,
  native_country_original = NA_character_,
  native_country_standardised = NA_character_,
  native_country_code     = NA_character_,   # filled by script 05 after crosswalk
  mapping_status          = NA_character_,
  notes                   = "",
  stringsAsFactors        = FALSE)

row_by_name <- match(clean$accepted_taxon, mp$species_name)

for (i in seq_len(nrow(clean))) {
  tx  <- clean$accepted_taxon[i]
  if (!is.na(row_by_name[i])) {
    mr <- mp[row_by_name[i], ]
    clean$original_name_in_mapping[i] <- mr$species_name
    clean$native_country_original[i]  <- mr$area
    toks <- trimws(unlist(strsplit(mr$area, ";")))
    toks <- toks[toks != ""]
    clean$native_country_standardised[i] <- paste(sort(unique(toks)), collapse = "; ")
    if (mr$introduced == 1) {
      clean$taxonomic_match_type[i] <- "EXACT_NAME"
      clean$mapping_status[i]        <- "MISSING_NATIVE_MAPPING"
      clean$notes[i] <- paste0("Mapping row exists but lists introduced-only distribution (introduced=1, '",
                               mr$note, "'); no native-range list available in source. Excluded from native masking.")
    } else {
      clean$taxonomic_match_type[i] <- "EXACT_NAME"
      clean$mapping_status[i] <-
        if (length(toks) > 1) "MULTIPLE_NATIVE_COUNTRIES" else "EXACT_MATCH"
    }
  } else if (tx %in% names(synonym_map)) {
    src <- synonym_map[[tx]]
    mr  <- mp[mp$species_name == src, ]
    stopifnot(nrow(mr) == 1)
    clean$original_name_in_mapping[i] <- src
    clean$native_country_original[i]  <- mr$area
    toks <- trimws(unlist(strsplit(mr$area, ";")))
    toks <- toks[toks != ""]
    clean$native_country_standardised[i] <- paste(sort(unique(toks)), collapse = "; ")
    clean$taxonomic_match_type[i] <- "SYNONYM"
    clean$mapping_status[i]        <- "SYNONYM_RECONCILED"
    clean$notes[i] <- paste0("Accepted taxon '", tx, "' reconciled to mapping row '", src,
                             "' (POWO synonym).")
  } else {
    clean$taxonomic_match_type[i] <- "NONE"
    clean$mapping_status[i]        <- "AMBIGUOUS_MAPPING"
    clean$notes[i] <- "No mapping row and no documented synonym found."
  }
}

# every taxon must have an explicit status
stopifnot(!any(is.na(clean$mapping_status)))

cat("Status counts:\n"); print(table(clean$mapping_status))
cat("Match-type counts:\n"); print(table(clean$taxonomic_match_type))

write.csv(clean, file.path(DIR_INPUTS, "species_native_mapping_clean.csv"),
          row.names = FALSE)
cat("Wrote:", file.path(DIR_INPUTS, "species_native_mapping_clean.csv"), "\n")
