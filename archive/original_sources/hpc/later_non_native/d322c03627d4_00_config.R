# Original source: 00_config.R
# Original source location: ${NON_NATIVE_ROOT}/02_derived_non_native_potential/07_scripts/00_config.R
# Original source SHA-256: d322c03627d4d145bd0ed42cd67e9ed1e992624ad5a79bf1a96635edfeff9b47
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_NON_NATIVE)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# 00_config.R — shared configuration for the non-native potential workflow
# Project: Scientific Data — invasion pressure
# This file must NOT be modified after products are generated without
# updating provenance. All persistent outputs live under PROJECT_DERIVED_ROOT.

PROJECT_ROOT        <- "<NON_NATIVE_ROOT>"
DERIVED_ROOT        <- file.path(PROJECT_ROOT, "02_derived_non_native_potential")
DATA_PACKAGE        <- file.path(PROJECT_ROOT, "01_data_package")

# ---- Authoritative inputs (READ-ONLY — never modify) ----
MAPPING_CSV         <- file.path(DATA_PACKAGE, "Supplementary_Data_1_IAPs_list.csv")
BOUNDARY_SHP        <- file.path(DATA_PACKAGE, "geoBoundariesCGAZ_ADM1", "geoBoundariesCGAZ_ADM1.shp")
FROZEN_MANIFEST_V3  <- "<WORK_ROOT>/publication_release_v1/manifests/final_accepted_projection_manifest_813_v3.csv"
FROZEN_AGG_INPUT    <- "<WORK_ROOT>/publication_release_v1/manifests/final_aggregation_input_813_v2.csv"
FROZEN_AGG_RASTER   <- "<WORK_ROOT>/results/aggregation_publication_813_clean_v1_20260813/hotspot_mean_suitability_normalized.tif"
FROZEN_AGG_PROTOCOL <- "<WORK_ROOT>/publication_release_v1/audits/original_aggregation_protocol_frozen.txt"
FROZEN_AGG_SCRIPT   <- "<WORK_ROOT>/publication_release_v1/scripts/aggregate_publication_813_clean_v2.R"

# ---- Auxiliary mapping aid (WGSRPD botanical-country shapes, tdwg/wgsrpd) ----
WGSRPD_DIR          <- file.path(DERIVED_ROOT, "01_inputs", "aux_wgsrpd")
WGSRPD_L3_SHP       <- file.path(WGSRPD_DIR, "level3.shp")
WGSRPD_L2_SHP       <- file.path(WGSRPD_DIR, "level2.shp")
WGSRPD_L1_SHP       <- file.path(WGSRPD_DIR, "level1.shp")

# ---- Derived outputs ----
DIR_INPUTS          <- file.path(DERIVED_ROOT, "01_inputs")
DIR_MASK_VECTORS    <- file.path(DERIVED_ROOT, "02_native_range_masks", "vectors")
DIR_MASK_QC         <- file.path(DERIVED_ROOT, "02_native_range_masks", "qc")
DIR_SPP_RASTERS     <- file.path(DERIVED_ROOT, "03_species_non_native_projections", "rasters")
DIR_SPP_MANIFESTS   <- file.path(DERIVED_ROOT, "03_species_non_native_projections", "manifests")
DIR_SPP_QC          <- file.path(DERIVED_ROOT, "03_species_non_native_projections", "qc")
DIR_AGG_RASTERS     <- file.path(DERIVED_ROOT, "04_aggregate", "rasters")
DIR_AGG_MANIFESTS   <- file.path(DERIVED_ROOT, "04_aggregate", "manifests")
DIR_AGG_QC          <- file.path(DERIVED_ROOT, "04_aggregate", "qc")
DIR_FIGURES         <- file.path(DERIVED_ROOT, "05_figures")
DIR_TABLES          <- file.path(DERIVED_ROOT, "06_tables")
DIR_SCRIPTS         <- file.path(DERIVED_ROOT, "07_scripts")
DIR_LOGS            <- file.path(DERIVED_ROOT, "08_logs")
DIR_PROVENANCE      <- file.path(DERIVED_ROOT, "09_provenance")
DIR_PILOT           <- file.path(DIR_PROVENANCE, "pilot_10_taxa")

COUNTRY_GPKG        <- file.path(DIR_MASK_VECTORS, "country_boundaries_clean.gpkg")
NATIVE_GPKG         <- file.path(DIR_MASK_VECTORS, "native_ranges_813.gpkg")
PER_TAXON_QC_DIR    <- file.path(DIR_SPP_QC, "per_taxon")

AGG_OUT_RASTER      <- file.path(DIR_AGG_RASTERS, "multispecies_non_native_invasion_potential_813.tif")

for (d in c(DIR_INPUTS, DIR_MASK_VECTORS, DIR_MASK_QC, DIR_SPP_RASTERS,
            DIR_SPP_MANIFESTS, DIR_SPP_QC, PER_TAXON_QC_DIR, DIR_AGG_RASTERS,
            DIR_AGG_MANIFESTS, DIR_AGG_QC, DIR_FIGURES, DIR_TABLES,
            DIR_SCRIPTS, DIR_LOGS, DIR_PROVENANCE, DIR_PILOT)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

suppressMessages(library(terra))

# Taxon-safe filename: replace spaces/dots with underscores
taxon_safe <- function(x) gsub("[ .]+", "_", trimws(x))

# Expected authoritative output path for a taxon
non_native_output_path <- function(accepted_taxon) {
  file.path(DIR_SPP_RASTERS, paste0(taxon_safe(accepted_taxon), "__non_native_suitability.tif"))
}

sha256_of <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  out <- system2("sha256sum", shQuote(path), stdout = TRUE)
  unlist(strsplit(out[1], "[ \t]+"))[1]
}
