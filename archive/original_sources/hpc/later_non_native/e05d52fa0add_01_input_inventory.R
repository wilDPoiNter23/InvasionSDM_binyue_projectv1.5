# Original source: 01_input_inventory.R
# Original source location: ${NON_NATIVE_ROOT}/02_derived_non_native_potential/07_scripts/01_input_inventory.R
# Original source SHA-256: e05d52fa0adddff273a1251812cf9473be69498e6f923e06bc1a4ce6e642cf66
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_NON_NATIVE)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# 01_input_inventory.R — PHASE 1: inventory of authoritative + auxiliary inputs
ca <- commandArgs(trailingOnly = FALSE)
source(file.path(dirname(sub("^--file=", "", ca[grep("^--file=", ca)])), "00_config.R"))

rows <- list()

add <- function(source, path, type, count, crs, key_fields, role, status, notes) {
  rows[[length(rows) + 1]] <<- data.frame(
    source = source, path = path, file_type = type,
    row_or_feature_count = count, CRS = crs, key_fields = key_fields,
    role_in_workflow = role, authoritative_status = status, notes = notes,
    stringsAsFactors = FALSE)
}

# 1. Frozen manifest v3
m <- read.csv(FROZEN_MANIFEST_V3, stringsAsFactors = FALSE)
add("frozen_manifest_v3", FROZEN_MANIFEST_V3, "CSV", paste0(nrow(m), " taxa"),
    NA, paste(names(m), collapse = "; "),
    "Authoritative list of 813 accepted taxa and their frozen global projection paths/SHA256",
    "AUTHORITATIVE_FROZEN",
    "READ-ONLY. Only final_projection_path / final_projection_SHA256 / accepted_taxon consumed by this workflow.")

# 2. Species native mapping table
mp <- read.csv(MAPPING_CSV, stringsAsFactors = FALSE)
add("Supplementary_Data_1_IAPs_list", MAPPING_CSV, "CSV", paste0(nrow(mp), " rows"),
    NA, "species_name; final_plant_name_id; wcvp_plant_name_id; glonaf_taxon_id; continent; region; area; record_count; introduced; note",
    "Native-range source table (GloNAF-derived). area = TDWG level-3 botanical areas, ';'-delimited.",
    "AUTHORITATIVE",
    "READ-ONLY. 813 rows; 3 rows have introduced=1 (listed distribution is introduced-only).")

# 3. geoBoundaries ADM1
v <- vect(BOUNDARY_SHP)
add("geoBoundariesCGAZ_ADM1", BOUNDARY_SHP, "ESRI Shapefile", paste0(nrow(v), " ADM1 features"),
    "EPSG:4326", "shapeName (ADM1 name); shapeGroup (ISO3); shapeID; shapeType",
    "Country/ADM1 boundary source; dissolved by shapeGroup (ISO3) to country level for native masks.",
    "AUTHORITATIVE",
    paste0("READ-ONLY. ", length(unique(v$shapeGroup)), " unique ISO3 groups detected."))

# 4. WGSRPD level 3 (auxiliary mapping aid)
v3 <- vect(WGSRPD_L3_SHP)
flds <- names(v3)
add("WGSRPD level3 (tdwg/wgsrpd)", WGSRPD_L3_SHP, "ESRI Shapefile",
    paste0(nrow(v3), " botanical countries"),
    crs(v3, describe = TRUE)$code,
    paste(flds, collapse = "; "),
    "Auxiliary mapping aid: TDWG level-3 name -> geometry, intersected with geoBoundaries to build the TDWG->ISO3 crosswalk. NOT used as mask geometry.",
    "AUXILIARY",
    "Downloaded 2026-08-14 from github.com/tdwg/wgsrpd. Used read-only.")

# 5. Frozen aggregate (reference for comparison + frozen aggregation logic)
agg <- rast(FROZEN_AGG_RASTER)
add("frozen_global_aggregate", FROZEN_AGG_RASTER, "GeoTIFF",
    paste0(nrow(agg), "x", ncol(agg)),
    crs(agg, describe = TRUE)$code,
    "hotspot_mean_suitability_normalized (813-taxon frozen aggregate)",
    "Reference for Phase 21 descriptive comparison. Also the protocol source (per-species min-max norm, NA->0, sum/count).",
    "AUTHORITATIVE_FROZEN", "READ-ONLY. Never modified.")

inv <- do.call(rbind, rows)
write.csv(inv, file.path(DIR_INPUTS, "source_inventory.csv"), row.names = FALSE)
cat("Wrote:", file.path(DIR_INPUTS, "source_inventory.csv"), "\n")
print(inv[, c("source", "row_or_feature_count", "CRS")])
