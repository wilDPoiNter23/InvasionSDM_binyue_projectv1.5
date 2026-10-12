# Original source: 07_addon_rostratum_native_mapping.R
# Original source location: ${NON_NATIVE_ROOT}/02_derived_non_native_potential/07_scripts/07_addon_rostratum_native_mapping.R
# Original source SHA-256: d3a7fd83c478e53b3a155fde29fc431ec0be0d502647095226702f05c9545c5a
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_NON_NATIVE)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# 07_addon_rostratum_native_mapping.R — add Solanum rostratum native mapping
# (add-on to the 813 non-native pipeline; does NOT modify frozen 813 products)
# Native range (user-specified, 2026-08-17): Mexico + United States of America
# (contiguous; Great Plains & southwestern states).
# Steps: 1) crosswalk rows  2) species mapping row  3) add-on native vector gpkg
source("<NON_NATIVE_ROOT>/02_derived_non_native_potential/07_scripts/00_config.R")
suppressMessages(library(terra))

XW_FILE <- file.path(DIR_INPUTS, "native_country_boundary_crosswalk.csv")
MP_FILE <- file.path(DIR_INPUTS, "species_native_mapping_clean.csv")
ADDON_GPKG <- file.path(DIR_MASK_VECTORS, "native_range_addon_Solanum_rostratum.gpkg")
COUNTRY_GPKG_SIMPLIFIED <- file.path(DIR_MASK_VECTORS, "country_boundaries_simplified.gpkg")
NOTE <- "ADDON_2026-08-17: Solanum rostratum native range (user-specified: native to Mexico and USA — Great Plains & southwestern states, per POWO/FNA). Separate add-on native vector; not part of native_ranges_813.gpkg."

# ---- 1. crosswalk additions (skip if token already present) ----
xw <- read.csv(XW_FILE, stringsAsFactors = FALSE, check.names = FALSE)
new_tokens <- c("Mexico", "United States of America, contiguous (without Alaska and Hawaii)")
add <- data.frame(
  native_country_original    = new_tokens,
  native_country_standardised = new_tokens,
  native_country_code        = c("MEX", "USA"),
  boundary_country_name      = c("Mexico", "United States of America"),
  boundary_country_code      = c("MEX", "USA"),
  match_type                 = "MANUAL_CONFIRMED",
  match_confidence           = 1, overlap_fraction = 1,
  manual_review_required     = FALSE, notes = NOTE,
  stringsAsFactors = FALSE)
missing_tok <- new_tokens[!new_tokens %in% xw$native_country_standardised]
if (length(missing_tok)) {
  xw <- rbind(xw, add[add$native_country_standardised %in% missing_tok, ])
  write.csv(xw, XW_FILE, row.names = FALSE)
  cat("crosswalk rows added:", length(missing_tok), "\n")
} else cat("crosswalk tokens already present\n")

# ---- 2. species mapping row ----
mp <- read.csv(MP_FILE, stringsAsFactors = FALSE, check.names = FALSE)
if ("Solanum rostratum" %in% mp$accepted_taxon) {
  cat("mapping row already present; skipping\n")
} else {
  row <- data.frame(
    accepted_taxon             = "Solanum rostratum",
    original_name_in_mapping   = "Solanum rostratum",
    taxonomic_match_type       = "MANUAL_CONFIRMED",
    native_country_original    = "Mexico; United States of America",
    native_country_standardised = "Mexico; United States of America, contiguous (without Alaska and Hawaii)",
    native_country_code        = "MEX;USA",
    mapping_status             = "MANUAL_CONFIRMED",
    notes                      = paste0(NOTE, " De novo SDM built 2026-08-16 (occ_patch protocol). Not in frozen 813 manifest; add-on product."),
    stringsAsFactors = FALSE)
  mp <- rbind(mp, row)
  write.csv(mp, MP_FILE, row.names = FALSE)
  cat("mapping row added; rows now:", nrow(mp), "\n")
}

# ---- 3. add-on native vector (country-level, simplified boundaries) ----
cb <- vect(COUNTRY_GPKG_SIMPLIFIED)
stopifnot(all(c("MEX", "USA") %in% cb$iso3))
v <- cb[cb$iso3 %in% c("MEX", "USA"), ]
v$accepted_taxon        <- "Solanum rostratum"
v$n_native_countries    <- 2L
v$native_country_codes  <- "MEX;USA"
v$mapping_source        <- "MANUAL_CONFIRMED"
v$notes                 <- NOTE
v <- v[, c("accepted_taxon", "n_native_countries", "native_country_codes",
           "mapping_source", "notes")]
writeVector(v, ADDON_GPKG, overwrite = TRUE)
cat("add-on native vector written:", ADDON_GPKG, " features:", nrow(v),
    " area_km2:", round(sum(expanse(v, unit = "km")), 1), "\n")
cat("DONE\n")
