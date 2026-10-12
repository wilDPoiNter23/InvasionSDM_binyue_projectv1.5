# Original source: 05_patch_clean_codes.R
# Original source location: ${NON_NATIVE_ROOT}/02_derived_non_native_potential/07_scripts/05_patch_clean_codes.R
# Original source SHA-256: b510c94a14372822d7571be1a24638af1b1c7056fb982fe37818029d14ec09ae
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_NON_NATIVE)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# 05_patch_clean_codes.R — re-apply ISO3 codes to species_native_mapping_clean.csv
# from MY completed crosswalk (prefers the private snapshot crosswalk_internal.rds;
# rewrites the canonical crosswalk CSV from it as well, so downstream steps are
# robust to concurrent edits by a parallel session).
ca <- commandArgs(trailingOnly = FALSE)
source(file.path(dirname(sub("^--file=", "", ca[grep("^--file=", ca)])), "00_config.R"))

mp   <- read.csv(MAPPING_CSV, stringsAsFactors = FALSE)
rds  <- file.path(DIR_PROVENANCE, "crosswalk_internal.rds")
cw   <- if (file.exists(rds)) readRDS(rds)$crosswalk else
  read.csv(file.path(DIR_INPUTS, "native_country_boundary_crosswalk.csv"),
           stringsAsFactors = FALSE)

# restore canonical crosswalk CSV from my snapshot (country-level geometric attribution)
write.csv(cw, file.path(DIR_INPUTS, "native_country_boundary_crosswalk.csv"),
          row.names = FALSE)

tok_iso <- split(cw$native_country_code[cw$match_type != "UNMATCHED"],
                 cw$native_country_original[cw$match_type != "UNMATCHED"])
tok_iso <- lapply(tok_iso, function(x) sort(unique(x[!is.na(x) & x != ""])))
rev_map <- setNames(cw$native_country_original, cw$native_country_standardised)
rev_map <- rev_map[!duplicated(rev_map) & !is.na(rev_map)]

mani <- read.csv(FROZEN_MANIFEST_V3, stringsAsFactors = FALSE)
synonym_map <- c("Spartina alterniflora" = "Sporobolus alterniflorus")

clean <- data.frame(
  accepted_taxon          = mani$accepted_taxon,
  original_name_in_mapping = NA_character_,
  taxonomic_match_type    = NA_character_,
  native_country_original = NA_character_,
  native_country_standardised = NA_character_,
  native_country_code     = NA_character_,
  mapping_status          = NA_character_,
  notes                   = "",
  stringsAsFactors        = FALSE)

row_by_name <- match(clean$accepted_taxon, mp$species_name)
for (i in seq_len(nrow(clean))) {
  tx <- clean$accepted_taxon[i]
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
      next
    }
    clean$taxonomic_match_type[i] <- "EXACT_NAME"
    codes <- unique(unlist(tok_iso[toks]))
    bad   <- toks[!(toks %in% names(tok_iso))]
    if (length(bad)) {
      codes <- sort(unique(c(codes, unique(unlist(tok_iso[unname(rev_map[bad])])))))
      still <- setdiff(bad, names(rev_map))
      if (length(still)) {
        clean$notes[i] <- paste0("UNRESOLVED area tokens: ", paste(still, collapse = ", "))
        clean$mapping_status[i] <- "UNMATCHED_COUNTRY"
      } else {
        clean$mapping_status[i] <-
          if (length(toks) > 1) "MULTIPLE_NATIVE_COUNTRIES" else "EXACT_MATCH"
      }
    } else {
      clean$mapping_status[i] <-
        if (length(toks) > 1) "MULTIPLE_NATIVE_COUNTRIES" else "EXACT_MATCH"
    }
    clean$native_country_code[i] <- paste(sort(unique(codes)), collapse = ";")
  } else if (tx %in% names(synonym_map)) {
    src <- synonym_map[[tx]]
    mr  <- mp[mp$species_name == src, ]
    stopifnot(nrow(mr) == 1)
    clean$original_name_in_mapping[i] <- src
    clean$native_country_original[i]  <- mr$area
    toks <- trimws(unlist(strsplit(mr$area, ";")))
    toks <- toks[toks != ""]
    clean$native_country_standardised[i] <- paste(sort(unique(toks)), collapse = "; ")
    codes <- unique(unlist(tok_iso[toks]))
    bad   <- toks[!(toks %in% names(tok_iso))]
    if (length(bad)) {
      codes <- sort(unique(c(codes, unique(unlist(tok_iso[unname(rev_map[bad])])))))
      still <- setdiff(bad, names(rev_map))
      if (length(still)) {
        clean$notes[i] <- paste0("UNRESOLVED area tokens: ", paste(still, collapse = ", "))
        clean$mapping_status[i] <- "UNMATCHED_COUNTRY"
      } else {
        clean$mapping_status[i] <- "SYNONYM_RECONCILED"
      }
    } else {
      clean$mapping_status[i] <- "SYNONYM_RECONCILED"
    }
    clean$native_country_code[i] <- paste(sort(unique(codes)), collapse = ";")
    clean$taxonomic_match_type[i] <- "SYNONYM"
    clean$notes[i] <- paste0("Accepted taxon '", tx, "' reconciled to mapping row '", src,
                             "' (POWO synonym).")
  } else {
    clean$taxonomic_match_type[i] <- "NONE"
    clean$mapping_status[i]        <- "AMBIGUOUS_MAPPING"
    clean$notes[i] <- "No mapping row and no documented synonym found."
  }
}
stopifnot(!any(is.na(clean$mapping_status)))
write.csv(clean, file.path(DIR_INPUTS, "species_native_mapping_clean.csv"),
          row.names = FALSE)
cat("status counts:\n"); print(table(clean$mapping_status))
cat("taxa with codes:", sum(!is.na(clean$native_country_code) & clean$native_country_code != ""), "\n")
