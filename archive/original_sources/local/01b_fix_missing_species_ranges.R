# Original source: 01b_fix_missing_species_ranges.R
# Original source location: <LOCAL_CODE_ROOT>/01b_fix_missing_species_ranges.R
# Original source SHA-256: ca04cddf78c4d325c75a7f03856a60b40253811d24fcbdec9b71284f56b69707
# Public-safe original snapshot: archive/original_sources/local/01b_fix_missing_species_ranges.R
# Classification: SUPPORTING
# Publication modifications:
# - machine-specific absolute path literals replaced by configured publication_path aliases
# - local encoding decoded to UTF-8; source bytes hashed above
# - comments and historical private machine paths sanitized
# - working-directory calls retained with configured paths; no analyses executed
# Scientific modification: NONE
# This is full historical source, not a certified unattended/reproducible run.
# Snapshot for inspection; path placeholders are not executable locations.
# ==============================================================================
# Script: 01b_fix_missing_species_ranges.R
# Purpose: Consolidate all the data fixes we applied to species_native_ranges.csv
#          and species_invasive_ranges.csv. This includes handling artificial
#          hybrids, filling WCVP/GLONaF IDs, mapping GLONaF string regions to
#          TDWG standard Continents/Regions, injecting Crataegus mexicana,
#          and properly aggregating the native ranges.
# Note: This is a historical compilation of the patches applied to produce
#       the clean data ready for masking.
# ==============================================================================

library(dplyr)
library(duckdb)
library(readr)

native_csv <- "<LOCAL_ANALYSIS_ROOT>/data/native_invasion_region/species_native_ranges.csv"
inv_csv <- "<LOCAL_ANALYSIS_ROOT>/data/native_invasion_region/species_invasive_ranges.csv"
db_path <- "<LOCAL_ANALYSIS_ROOT>/data/data/glonaf_wcvp.duckdb"

con <- dbConnect(duckdb(), dbdir = db_path)

# ------------------------------------------------------------------------------
# 1. Missing Geographical Mapping for GLONaF Regions
# ------------------------------------------------------------------------------
# Many GLONaF areas do not directly correspond to WCVP TDWG Level 3 standards.
# We build an explicit map to assign their accurate Continent and Region.
geo_map <- tibble::tribble(
    ~area, ~new_continent, ~new_region,
    "Korea, Republic of", "ASIA-TEMPERATE", "Eastern Asia",
    "Korea, Democratic People's Republic of", "ASIA-TEMPERATE", "Eastern Asia",
    "Para\u00EDba", "SOUTHERN AMERICA", "Brazil",
    "Paran\u00E1", "SOUTHERN AMERICA", "Brazil",
    "Rio Grande do Sul", "SOUTHERN AMERICA", "Brazil",
    "Santa Catarina", "SOUTHERN AMERICA", "Brazil",
    "S\u00E3o Paulo", "SOUTHERN AMERICA", "Brazil",
    "Brazil", "SOUTHERN AMERICA", "Brazil",
    "China incl. Taiwan and Hong Kong", "ASIA-TEMPERATE", "China",
    "China", "ASIA-TEMPERATE", "China",
    "South Africa", "AFRICA", "Southern Africa",
    "Spain (incl. islands)", "EUROPE", "Southwestern Europe",
    "Shanghai", "ASIA-TEMPERATE", "China",
    "Biskaia", "EUROPE", "Southwestern Europe",
    "Argentina", "SOUTHERN AMERICA", "Southern South America",
    "Micronesia (Federated States of)", "PACIFIC", "Southwestern Pacific",
    "New Zealand", "AUSTRALASIA", "New Zealand",
    "United States of America, contiguous (without Alaska and Hawaii)", "NORTHERN AMERICA", "Northern America",
    "Abruzzo Region, Italy", "EUROPE", "Southeastern Europe",
    "Campania Region, Italy", "EUROPE", "Southeastern Europe",
    "Vojvodina Province, Serbia", "EUROPE", "Southeastern Europe",
    "Anhui", "ASIA-TEMPERATE", "China",
    "Beijing", "ASIA-TEMPERATE", "China",
    "Chongqing", "ASIA-TEMPERATE", "China",
    "Fujian", "ASIA-TEMPERATE", "China",
    "Guizhou", "ASIA-TEMPERATE", "China",
    "Heilongjiang", "ASIA-TEMPERATE", "China",
    "Nei Mongol", "ASIA-TEMPERATE", "China",
    "Jiangsu", "ASIA-TEMPERATE", "China",
    "Jilin", "ASIA-TEMPERATE", "China",
    "Liaoning", "ASIA-TEMPERATE", "China",
    "Shaanxi", "ASIA-TEMPERATE", "China",
    "Shandong", "ASIA-TEMPERATE", "China",
    "Shanxi", "ASIA-TEMPERATE", "China",
    "Sichuan", "ASIA-TEMPERATE", "China",
    "Tianjin", "ASIA-TEMPERATE", "China",
    "Yunnan", "ASIA-TEMPERATE", "China",
    "Zhejiang", "ASIA-TEMPERATE", "China",
    "Montenegro", "EUROPE", "Southeastern Europe",
    "Ticino, Switzerland", "EUROPE", "Middle Europe",
    "Guangdong", "ASIA-TEMPERATE", "China",
    "Guangxi", "ASIA-TEMPERATE", "China",
    "Hainan", "ASIA-TEMPERATE", "China",
    "Hebei", "ASIA-TEMPERATE", "China",
    "Henan", "ASIA-TEMPERATE", "China",
    "Hubei", "ASIA-TEMPERATE", "China",
    "Hunan", "ASIA-TEMPERATE", "China",
    "Jiangxi", "ASIA-TEMPERATE", "China",
    "Taiwan", "ASIA-TEMPERATE", "Eastern Asia"
)

# ------------------------------------------------------------------------------
# 2. Fix Artificial Hybrids & Missing WCVP IDs
# ------------------------------------------------------------------------------
# Citrus aurantium, Citrus limon, and Oenothera glazioviana had no native
# ranges in WCVP because they are artificial hybrids. We extracted their GLONaF
# invasive records and appended them to both invasive & native CSVs with introduced=1.

id_map <- data.frame(
    species_name = c("Citrus aurantium", "Citrus limon", "Oenothera glazioviana"),
    correct_id = c("2723957", "2724178", "2398876"),
    stringsAsFactors = FALSE
)

# Fix Urochloa distachya GLONaF ID
uro_id <- "NA"
uro_taxon <- dbGetQuery(con, "SELECT id FROM taxon WHERE taxa_accepted LIKE '%Urochloa distachya%' LIMIT 1")
if (nrow(uro_taxon) > 0) uro_id <- uro_taxon$id[1]


# ------------------------------------------------------------------------------
# 3. Add Crataegus mexicana
# ------------------------------------------------------------------------------
# Missing entirely from WCVP native distributions (unrecorded in Kew).
# Fallback applied to GLONaF invasive data from South Africa.
crat_areas <- c("Eastern Cape", "KwaZulu-Natal", "Limpopo", "Mpumalanga", "North West", "South Africa")
crat_row_template <- data.frame(
    species_name = "Crataegus mexicana",
    final_plant_name_id = "2946149", # 'Moc. & Sess\u00E9 ex DC.'
    wcvp_plant_name_id = "2946149",
    glonaf_taxon_id = "216001",
    continent = "AFRICA",
    region = "Southern Africa",
    area = "Eastern Cape; KwaZulu-Natal; Limpopo; Mpumalanga; North West; South Africa",
    note = "Extracted from GLONaF (Invasive/Introduced)",
    introduced = "1",
    extinct = "0",
    stringsAsFactors = FALSE
)


# ==============================================================================
# Execution Logic (Commented out to prevent accidental overwrites,
# as the final DB is already correctly mapped and generated).
# ==============================================================================
#
# for (f in c(native_csv, inv_csv)) {
#   df <- read.csv(f, colClasses = "character")
#
#   # Map Urochloa glonaf_taxon_id
#   idx_uro <- which(df$species_name == "Urochloa distachya" & (is.na(df$glonaf_taxon_id) | df$glonaf_taxon_id == ""))
#   if (length(idx_uro) > 0 & uro_id != "NA") df$glonaf_taxon_id[idx_uro] <- uro_id
#
#   # Map WCVP ID for artificial hybrids
#   for (i in 1:nrow(id_map)) {
#     idx <- which(grepl("GLONaF", df$note) & df$species_name == id_map$species_name[i])
#     if (length(idx) > 0) {
#       if ("final_plant_name_id" %in% names(df)) df$final_plant_name_id[idx] <- id_map$correct_id[i]
#       if ("wcvp_plant_name_id" %in% names(df)) df$wcvp_plant_name_id[idx] <- id_map$correct_id[i]
#     }
#   }
#
#   # Map Continents & Regions
#   df <- df %>%
#     left_join(geo_map, by = "area") %>%
#     mutate(
#       continent = ifelse(is.na(continent) | continent == "", new_continent, continent),
#       region = ifelse(is.na(region) | region == "", new_region, region)
#     ) %>%
#     select(-new_continent, -new_region)
#
#   write.csv(df, f, row.names = FALSE)
# }
#
# # Aggregate Native Ranges strictly by species (fixing the 11,000+ row duplicate bug)
# df_nat <- read.csv(native_csv, colClasses = "character")
# df_nat_agg <- df_nat %>%
#   group_by(species_name) %>%
#   summarise(
#     final_plant_name_id = first(na.omit(final_plant_name_id)),
#     wcvp_plant_name_id = first(na.omit(wcvp_plant_name_id)),
#     glonaf_taxon_id = first(na.omit(glonaf_taxon_id)),
#     continent = paste(unique(na.omit(continent[continent != ""])), collapse = "; "),
#     region = paste(unique(na.omit(region[region != ""])), collapse = "; "),
#     area = paste(unique(na.omit(area[area != ""])), collapse = "; "),
#     note = paste(unique(na.omit(note[note != ""])), collapse = "; "),
#     introduced = paste(unique(na.omit(introduced[introduced != ""])), collapse = "; "),
#     extinct = paste(unique(na.omit(extinct[extinct != ""])), collapse = "; "),
#     .groups = "drop"
#   )
# write.csv(df_nat_agg, native_csv, row.names = FALSE)

dbDisconnect(con)
cat("Compilation script loaded for documentation.\n")
