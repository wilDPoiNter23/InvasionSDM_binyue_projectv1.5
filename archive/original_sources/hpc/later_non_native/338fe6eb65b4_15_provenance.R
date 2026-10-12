# Original source: 15_provenance.R
# Original source location: ${NON_NATIVE_ROOT}/02_derived_non_native_potential/07_scripts/15_provenance.R
# Original source SHA-256: 338fe6eb65b4a70d779f2c88b58182c83b9a3e167fa3df47fc5e229aa7dcfcb7
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_NON_NATIVE)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# 15_provenance.R — PHASE 25: provenance document (auto-generated facts)
ca <- commandArgs(trailingOnly = FALSE)
source(file.path(dirname(sub("^--file=", "", ca[grep("^--file=", ca)])), "00_config.R"))

mani     <- read.csv(file.path(DIR_SPP_MANIFESTS, "non_native_projection_manifest_813_v1.csv"),
                     stringsAsFactors = FALSE)
agg_qc   <- read.csv(file.path(DIR_AGG_QC, "non_native_aggregate_QC.csv"),
                     stringsAsFactors = FALSE)
src_inv  <- read.csv(file.path(DIR_INPUTS, "source_inventory.csv"), stringsAsFactors = FALSE)

script_files <- list.files(DIR_SCRIPTS, pattern = "\\.(R|sh)$", full.names = TRUE)
script_sha <- vapply(script_files, sha256_of, character(1))
script_lines <- c(
  "# Non-native invasion / introduction potential — PROVENANCE",
  "",
  sprintf("Generated: %s", Sys.time()),
  "",
  "## Frozen assets (READ-ONLY — never modified)",
  sprintf("- Frozen projection manifest: `%s` (v3, 813 taxa)", FROZEN_MANIFEST_V3),
  sprintf("- Frozen aggregation protocol: `%s`", FROZEN_AGG_PROTOCOL),
  sprintf("- Frozen aggregation script: `%s`", FROZEN_AGG_SCRIPT),
  sprintf("- Frozen global aggregate (reference): `%s`", FROZEN_AGG_RASTER),
  "",
  "## Inputs",
  sprintf("- Native-range mapping: `%s` (GloNAF-derived; area = TDWG level-3, ';'-delimited)", MAPPING_CSV),
  sprintf("- Boundaries: `%s` + sibling *_packed.shp (see 04_boundary_prep.R header for the documented corruption workaround)", BOUNDARY_SHP),
  sprintf("- WGSRPD level-1/2/3 shapes (auxiliary mapping aid, tdwg/wgsrpd GitHub): `%s`", WGSRPD_DIR),
  "",
  "## Software",
  sprintf("- R %s, terra %s, sf %s", getRversion(),
          as.character(packageVersion("terra")), as.character(packageVersion("sf"))),
  sprintf("- GDAL (via terra/sf): %s", gsub("[\"']", "", sf::sf_extSoftVersion()[["GDAL"]])),
  sprintf("- Processing host at run time: %s", Sys.info()[["nodename"]]),
  "",
  "## Scripts (SHA256)",
  paste0("- `", basename(script_files), "`  ", script_sha),
  "",
  "## Outputs",
  sprintf("- Native-range GeoPackage: `%s`", NATIVE_GPKG),
  sprintf("- Species non-native manifest: `02_derived_non_native_potential/03_species_non_native_projections/manifests/non_native_projection_manifest_813_v1.csv` SHA256: %s",
          sha256_of(file.path(DIR_SPP_MANIFESTS, "non_native_projection_manifest_813_v1.csv"))),
  sprintf("- Species QC: `02_derived_non_native_potential/03_species_non_native_projections/qc/non_native_projection_QC_813.csv`"),
  sprintf("- Aggregate raster: `%s`", AGG_OUT_RASTER),
  sprintf("- Aggregate SHA256: %s", agg_qc$SHA256[1]),
  "",
  "## Completeness",
  sprintf("- Species products: %d of 813 taxa processed", sum(mani$product_status == "COMPLETE")),
  sprintf("- No-native-origin global-equal taxa: %s",
          paste(mani$accepted_taxon[mani$non_native_treatment == "NO_NATIVE_ORIGIN_GLOBAL_EQUAL"], collapse = "; ")),
  sprintf("- Aggregate built from %d QC-passed non-native rasters", agg_qc$input_taxa[1]),
  "",
  "## Reproducibility",
  "- Re-run order: 01 -> 02 -> 03 -> 04 -> 05 -> 06 -> 07 -> (08 pilot) -> SLURM array (09) -> 10 -> 11 -> 12 -> 13 -> 14 -> 15.",
  "- The frozen global release was NOT modified at any step; frozen rasters are read-only inputs.",
  "")
writeLines(script_lines, file.path(DIR_PROVENANCE, "non_native_product_provenance.md"))
cat("Provenance written.\n")
