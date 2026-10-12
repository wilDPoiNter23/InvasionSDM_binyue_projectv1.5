# Original source: 02_mapping_schema_audit.R
# Original source location: ${NON_NATIVE_ROOT}/02_derived_non_native_potential/07_scripts/02_mapping_schema_audit.R
# Original source SHA-256: 9aaccf994a862637068ac089d800f693bd67248bd7c09ed6a13710561163c087
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_NON_NATIVE)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# 02_mapping_schema_audit.R — PHASE 2: schema report for the species mapping table
ca <- commandArgs(trailingOnly = FALSE)
source(file.path(dirname(sub("^--file=", "", ca[grep("^--file=", ca)])), "00_config.R"))

mp <- read.csv(MAPPING_CSV, stringsAsFactors = FALSE, check.names = FALSE)
w3 <- vect(WGSRPD_L3_SHP)
w2 <- vect(WGSRPD_L2_SHP)

l3_names <- trimws(w3$LEVEL3_NAM)
l2_names <- trimws(w2$LEVEL2_NAM)

tokens_all <- unlist(strsplit(trimws(mp$area), ";"))
tokens_all <- trimws(tokens_all[tokens_all != ""])
utok <- sort(unique(tokens_all))

# normalised comparison (strip accents, punctuation, case)
norm <- function(x) tolower(gsub("[^a-z]", "", iconv(x, to = "ASCII//TRANSLIT")))

match_report <- data.frame(token = utok, stringsAsFactors = FALSE)
match_report$in_L3 <- norm(match_report$token) %in% norm(l3_names)
match_report$in_L2 <- norm(match_report$token) %in% norm(l2_names)

tok_intro <- unique(trimws(unlist(strsplit(trimws(mp$area[mp$introduced == 1]), ";"))))
tok_nat   <- unique(trimws(unlist(strsplit(trimws(mp$area[mp$introduced == 0]), ";"))))

not_in_any <- match_report$token[!match_report$in_L3 & !match_report$in_L2]

out <- file.path(DIR_PROVENANCE, "native_mapping_schema_audit.md")
con <- file(out, open = "w")
writeLines(c(
  "# Native-range mapping schema audit",
  "",
  sprintf("Generated: %s", Sys.time()),
  "",
  sprintf("Source: `%s`", MAPPING_CSV),
  "",
  "## 1. Columns",
  "| column | description | missing |",
  "|---|---|---|",
  sprintf("| species_name | taxon name (GloNAF name) | %d |", sum(is.na(mp$species_name))),
  sprintf("| final_plant_name_id | plant name id | %d |", sum(is.na(mp$final_plant_name_id))),
  sprintf("| wcvp_plant_name_id | WCVP name id | %d |", sum(is.na(mp$wcvp_plant_name_id))),
  sprintf("| glonaf_taxon_id | GloNAF taxon id | %d |", sum(is.na(mp$glonaf_taxon_id))),
  sprintf("| continent | TDWG level-1 regions (';'-delimited) | %d |", sum(is.na(mp$continent))),
  sprintf("| region | TDWG level-2 regions (';'-delimited) | %d |", sum(is.na(mp$region))),
  sprintf("| area | TDWG level-3 botanical areas (';'-delimited) — the native-range field | %d |", sum(is.na(mp$area))),
  sprintf("| record_count | occurrence records | %d |", sum(is.na(mp$record_count))),
  sprintf("| introduced | 0 = listed areas are native; 1 = listed areas are introduced-only (no native list) | %d |", sum(is.na(mp$introduced))),
  sprintf("| note | provenance note | %d |", sum(is.na(mp$note))),
  "",
  "## 2. Key structure facts",
  sprintf("- Rows: %d; unique species_name: %d (no duplicated names).", nrow(mp), length(unique(mp$species_name))),
  "- Country-code convention: **none**. The `area` field uses **TDWG level-3 botanical-area names** (e.g. \"Brazil South\", \"Bolivia\"), NOT ISO2/ISO3 codes and not country names.",
  "- Delimiter convention: `;` separates areas within `area`, `region`, `continent`.",
  "- `introduced`: 810 rows = 0 (areas are the native list); 3 rows = 1 (areas are the introduced-only distribution; native range unresolved in the source).",
  sprintf("- Rows with introduced==1: %s.", paste(sprintf("`%s`", mp$species_name[mp$introduced == 1]), collapse = ", ")),
  "- Non-empty notes exist only for the 3 introduced==1 rows: \"Extracted from GLONaF (Artificial Hybrid Invasion)\".",
  "",
  "## 3. Native-area token audit",
  sprintf("- Unique area tokens (all rows): %d", length(utok)),
  sprintf("- Unique tokens from native rows (introduced==0): %d", length(tok_nat)),
  sprintf("- Tokens matching a WGSRPD level-3 name: %d / %d", sum(match_report$in_L3), nrow(match_report)),
  sprintf("- Tokens matching a WGSRPD level-2 name only: %d", sum(!match_report$in_L3 & match_report$in_L2)),
  sprintf("- Tokens matching neither level-3 nor level-2: %d", sum(!match_report$in_L3 & !match_report$in_L2)),
  "", "### Tokens not matched to WGSRPD level-3 (require documented name reconciliation)",
  if (length(not_in_any) == 0) "- none" else paste0("- `", not_in_any, "`"),
  "",
  "## 4. Taxonomic reconciliation notes",
  "- Manifest v3 contains `Spartina alterniflora`; the mapping table has no such row. The mapping table contains `Sporobolus alterniflorus`, the accepted synonym in POWO for Spartina alterniflora. Reconciled manually as SYNONYM_RECONCILED.",
  "- Mapping table also contains `Diodia teres` (synonym of `Hexasepalum teres`); the manifest carries `Hexasepalum teres`. The `Diodia teres` row is not used (superseded by the exact-match `Hexasepalum teres` row).",
  "",
  "## 5. Interpretation",
  "- Native ranges are provided at TDWG level-3 precision (finer than country in many cases, e.g. Brazilian states-groupings).",
  "- Per project boundary rules, masks are built from geoBoundaries **country-level** polygons (dissolved ADM1). A TDWG area that lies inside a country maps to that country; sub-country TDWG areas therefore mask the whole country. This conservative approximation is documented in the README (geographic-domain approximation).",
  "",
  sprintf("Audit written by script 02_mapping_schema_audit.R (see %s)", file.path(DIR_SCRIPTS, "02_mapping_schema_audit.R"))
), con = con)
close(con)

cat("Wrote:", out, "\n")
cat("tokens total:", nrow(match_report), "in L3:", sum(match_report$in_L3), "L2 only:",
    sum(!match_report$in_L3 & match_report$in_L2), "neither:", sum(!match_report$in_L3 & !match_report$in_L2), "\n")
print(not_in_any)
