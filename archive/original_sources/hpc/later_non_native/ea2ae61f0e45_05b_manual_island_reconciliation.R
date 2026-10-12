# Original source: 05b_manual_island_reconciliation.R
# Original source location: ${NON_NATIVE_ROOT}/02_derived_non_native_potential/07_scripts/05b_manual_island_reconciliation.R
# Original source SHA-256: ea2ae61f0e4512afe0bbd36923afb3d2ca54ae19e384fbcbdb64c91edaf929a0
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_NON_NATIVE)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# 05b_manual_island_reconciliation.R — resolve the 31 island/territory tokens
# Every decision here is MANUAL_CONFIRMED and documented in the notes column.
# Geometry source for these tokens: their WGSRPD level-3 polygon (same name).
source("<NON_NATIVE_ROOT>/02_derived_non_native_potential/07_scripts/00_config.R")

manual <- data.frame(
  native_country_original = c(
    "Antipodean Is.","Aruba","Ascension","Bermuda","Cayman Is.","Chagos Archipelago",
    "Cook Is.","Falkland Is.","Føroyar","Howland-Baker Is.","Kermadec Is.","Marcus I.",
    "Marquesas","Mozambique Channel Is.","Netherlands Antilles","New Caledonia","Niue",
    "Pitcairn Is.","Réunion","Selvagens","Society Is.","South China Sea","St.Helena",
    "Svalbard","Tokelau-Manihiki","Tristan da Cunha","Tuamotu","Tubuai Is.",
    "Turks-Caicos Is.","Wake I.","Wallis-Futuna Is."),
  native_country_code = c(
    "NZL","ABW","SHN","BMU","CYM","IOT",
    "COK","FLK","FRO","UMI","NZL","JPN",
    "PYF","FRA","CUW;SXM;BES","NCL","NIU",
    "PCN","REU","PRT","PYF","CHN","SHN",
    "SJM","TKL;COK","SHN","PYF","PYF",
    "TCA","UMI","WLF"),
  stringsAsFactors = FALSE)
manual$note_text <- c(
  "NZ subantarctic islands; NZL attribution (documented)",
  "Caribbean island territory; ABW",
  "UK overseas territory; SHN (Saint Helena, Ascension and Tristan da Cunha)",
  "UK overseas territory; BMU",
  "UK overseas territory; CYM",
  "British Indian Ocean Territory; IOT (disputed, documented)",
  "COK (also includes NZ-associated islands per WGSRPD; COK retained)",
  "FLK (disputed sovereignty; documented)",
  "Faroe Islands; FRO",
  "US minor outlying islands; UMI",
  "NZ islands; NZL",
  "Minami-Tori-shima; JPN (documented)",
  "French Polynesia; PYF",
  "French Scattered Islands in the Mozambique Channel; FRA (documented)",
  "Dissolved 2010 entity; CUW;SXM;BES recorded; geometry from WGSRPD L3 unit",
  "NCL",
  "NIU",
  "PCN",
  "REU (overseas department of France)",
  "Selvagens belong to Portugal (Madeira region); PRT",
  "French Polynesia; PYF",
  "TDWG unit for South China Sea islands; attributed to CHN as documented approximation",
  "UK overseas territory; SHN",
  "Norway; SJM",
  "TKL plus Manihiki (Cook Is.); TKL;COK",
  "UK overseas territory; SHN",
  "French Polynesia; PYF",
  "French Polynesia; PYF",
  "UK overseas territory; TCA",
  "US minor outlying islands; UMI",
  "WLF")

cw <- read.csv(file.path(DIR_INPUTS, "native_country_boundary_crosswalk.csv"), stringsAsFactors = FALSE)
cw_keep <- cw[cw$match_type != "UNMATCHED", ]

new_rows <- do.call(rbind, lapply(seq_len(nrow(manual)), function(i) {
  tk <- manual$native_country_original[i]
  codes <- trimws(unlist(strsplit(manual$native_country_code[i], ";")))
  do.call(rbind, lapply(codes, function(cc) {
    data.frame(
      native_country_original = tk,
      native_country_standardised = tk,   # WGSRPD L3 unit has the same name
      native_country_code = cc,
      boundary_country_name = cc,
      boundary_country_code = cc,
      match_type = "MANUAL_CONFIRMED",
      match_confidence = 1.0,
      overlap_fraction = 1.0,
      manual_review_required = TRUE,
      notes = paste0("Island/territory without ADM1 country coverage. ISO attribution: ",
                     manual$note_text[i], ". Geometry = WGSRPD level-3 polygon."),
      stringsAsFactors = FALSE)
  }))
}))
cw2 <- rbind(cw_keep, new_rows)
write.csv(cw2, file.path(DIR_INPUTS, "native_country_boundary_crosswalk.csv"), row.names = FALSE)
cat("crosswalk rows:", nrow(cw2), " match types:\n"); print(table(cw2$match_type))

# ---- rebuild species_native_mapping_clean.csv (spec columns) ----
mp    <- read.csv(MAPPING_CSV, stringsAsFactors = FALSE)
mani  <- read.csv(FROZEN_MANIFEST_V3, stringsAsFactors = FALSE)

# token -> codes and token -> L3 units from the UPDATED crosswalk
matched_cw <- cw2[!is.na(cw2$native_country_code) & cw2$native_country_code != "", ]
tok_codes <- split(matched_cw$native_country_code, matched_cw$native_country_original)
tok_codes <- lapply(tok_codes, function(x) sort(unique(trimws(unlist(strsplit(paste(x, collapse=";"), ";"))))))
tok_l3    <- split(matched_cw$native_country_standardised, matched_cw$native_country_original)
tok_l3    <- lapply(tok_l3, unique)

mp_lookup <- mp$species_name
# documented synonym: Spartina alterniflora = Sporobolus alterniflorus (POWO)
SYN <- c("Spartina alterniflora" = "Sporobolus alterniflorus")

rows <- lapply(mani$accepted_taxon, function(tx) {
  src_name <- tx
  tt <- "EXACT_MATCH"
  if (tx %in% names(SYN)) { src_name <- SYN[[tx]]; tt <- "SYNONYM_RECONCILED" }
  hit <- which(mp_lookup == src_name)
  if (length(hit) == 0) {
    return(data.frame(accepted_taxon=tx, original_taxon_name=src_name,
      taxonomic_match_type="MISSING_MAPPING", native_range_original=NA,
      native_country_original=NA, native_country_standardised=NA,
      native_country_code=NA, mapping_status="MISSING_NATIVE_MAPPING",
      notes="No row in Supplementary_Data_1_IAPs_list.csv.", stringsAsFactors=FALSE))
  }
  mrow <- mp[hit[1], ]
  if (mrow$introduced == 1) {
    return(data.frame(accepted_taxon=tx, original_taxon_name=src_name,
      taxonomic_match_type="MISSING_MAPPING", native_range_original=mrow$area,
      native_country_original=mrow$area, native_country_standardised=NA,
      native_country_code=NA, mapping_status="MISSING_NATIVE_MAPPING",
      notes="Mapping row lists introduced-only distribution (introduced=1); no native-range list in source. Excluded from native masking (documented).",
      stringsAsFactors=FALSE))
  }
  toks <- trimws(unlist(strsplit(trimws(mrow$area), ";")))
  toks <- toks[toks != ""]
  codes <- unique(unlist(tok_codes[toks], use.names = FALSE))
  l3s   <- unique(unlist(tok_l3[toks], use.names = FALSE))
  missing <- setdiff(toks, names(tok_codes))
  st <- if (length(missing)) "UNMATCHED_COUNTRY" else
        if (length(toks) == 1 && length(codes) == 1) "EXACT_MATCH" else "MULTIPLE_NATIVE_COUNTRIES"
  if (tt == "SYNONYM_RECONCILED") st <- "SYNONYM_RECONCILED"
  notes <- if (length(missing)) paste0("UNRESOLVED area tokens: ", paste(missing, collapse=", ")) else ""
  data.frame(accepted_taxon=tx, original_taxon_name=src_name,
    taxonomic_match_type=tt, native_range_original=mrow$area,
    native_country_original=paste(toks, collapse="; "),
    native_country_standardised=paste(sort(l3s), collapse="; "),
    native_country_code=paste(sort(codes), collapse=";"),
    mapping_status=st, notes=notes, stringsAsFactors=FALSE)
})
clean2 <- do.call(rbind, rows)
write.csv(clean2, file.path(DIR_INPUTS, "species_native_mapping_clean.csv"), row.names = FALSE)
cat("clean mapping rows:", nrow(clean2), "\n")
print(table(clean2$mapping_status))
cat("taxa with UNRESOLVED notes:", sum(grepl("UNRESOLVED", clean2$notes)), "\n")
cat("taxa with codes:", sum(!is.na(clean2$native_country_code) & clean2$native_country_code != ""), "\n")
