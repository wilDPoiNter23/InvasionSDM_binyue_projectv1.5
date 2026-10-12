# Data package

The deposit accompanies the code in this repository. Large suitability rasters and numeric source-data tables are staged separately; no upload or deposit DOI has been assigned.

## Manuscript data

The historical response is manuscript_suitability.tif, retaining the normalized2.0 raster used in the manuscript. Include the revised 108-city table, reclaimed/non-reclaimed comparison tables, separate PCA loading and GAM-score inputs, GAM summaries and conditional-curve data, archived SEM composite input and reported coefficient table. Retain model-performance summaries, predictor definitions and available figure/table source data.

The taxonomic roster, projection contributor list and sensitivity species set describe different populations. They should remain labeled separately. The historical response is not replaced by the paired sensitivity raster.

## Paired environment-only sensitivity

The final paired set contains 684 taxa from 814 candidates; 130 are excluded. The package contains:

- full_paired_684_mean_suitability.tif
- env_only_paired_684_mean_suitability.tif
- paired_684_valid_species_count.tif
- full_vs_env_only_paired_684_summary.csv
- paired_species_manifest.csv
- paired_vs_excluded_species_summary.csv

Supporting numeric files include city extractions, raster QC and the attrition detail table. The optional city GAM outputs retain the manuscript model definition and archived covariates. Continuous predictions are divided by 1000; means share the species with finite values in both groups within each cell. No new native-range mask or per-species minâ€“max normalization is applied. No binary aggregates are required.

The package manifest uses root aliases to avoid machine-specific paths. FULL_SDM_ROOT identifies accepted baseline model/projection inputs; ENV_SOURCE_ROOT identifies the saved environment-only inputs; ATTRITION_INPUTS identifies the source roster, evaluation table and candidate inventory. Relocate these aliases before re-running with the separately retained model/projection collections. Keep model members and calibration/formatted inputs together.

## External data and permissions

Obtain GBIF downloads, WorldClim, SoilGrids, VIIRS, GDP/population, marine and reclamation provider inputs from their original sources. See ../docs/DATA.md for expected variables and citations. City-boundary redistribution and deposit licences require confirmation before upload. Staging does not grant redistribution rights.

## Inventory

zenodo_manifest.csv records the current location alias, package filename, byte size and SHA256 where available. YES identifies a planned deposit item, OPTIONAL supporting material, NO_EXTERNAL_SOURCE provider data, and TO_DECIDE a version or rights decision. Directory collections may lack a whole-collection checksum. Staged file checksums describe the exact package bytes, including any documented path-alias substitution in small manifests.

Unique fitted models, calibration inputs and accepted continuous projections remain protected until an external cold archive is copied and checked against recorded hashes. Code inclusion or local staging alone does not authorize deletion of these scientific objects.
