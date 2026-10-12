# Data package for the coastal reclamation analyses

The planned Zenodo deposit will accompany the code and include the manuscript suitability raster, city analysis tables, PCA/GAM inputs, SEM composite and coefficient tables, model summaries, conditional-curve data and numeric source data for figures. These files are listed in zenodo_manifest.csv. No deposit DOI has been assigned in this repository.

## Manuscript data

The manuscript response is the normalized2.0 suitability raster. Local and server copies contain identical bytes; only one copy is needed. Include the 108-city analysis table, land-zone comparison tables, loading-table input, GAM score table and the selected SEM composite input. The coefficient CSV contains the fourteen reported paths. Retain the species-projection contributor list with the raster so readers can identify its species set.

The 813-name taxonomic roster, original 737-row contributor manifest and 814-taxon sensitivity inventory are different lists. The roster is not a statement that every species contributed to the manuscript raster. Later aggregate alternatives are candidates until the deposited response version is selected.

## Environment-only sensitivity

The environment-only analysis is in progress. Its final eight FULL/ENV aggregate rasters and final comparison tables are not yet available. The required sensitivity inventory and route policies are included as data-package candidates. Before deposit, replace private file paths in these tables with paths within the public package. Include the final model/projection manifest, retained taxon set, valid-species counts and threshold/mask definitions with the finished products.

## External data

Obtain GBIF downloads, WorldClim, SoilGrids and other standard environmental products from their providers and cite the source releases. Do not duplicate them in this deposit. Geography and mixed-source processed inputs need redistribution review. Provider links and expected layer names are in ../docs/DATA.md.

## Reading the manifest

YES marks a planned deposit item, not an already uploaded file. OPTIONAL marks supporting material. NO_EXTERNAL_SOURCE marks provider data obtained elsewhere. TO_DECIDE marks items whose version, scope or redistribution terms need confirmation. Empty size/checksum fields denote pending outputs or directory collections whose transfer inventory is not yet complete.

The current_path column uses root aliases, not computer-specific locations: LOCAL_ANALYSIS_ROOT for local analysis files, MAIN_SDM_ROOT for species-model products, ENV_ONLY_ROOT for sensitivity files, PROJECT_RESULTS_ROOT and SUPPORTING_RESULTS_ROOT for derived products, and ZENODO_STAGING for small files prepared for deposit. Non-ASCII source filenames are URL-encoded. The proposed public filenames are the package destinations.

Retain original rasters, fitted models, projections, calibration/option files and execution metadata until the selected files are deposited or placed in cold storage and their checksums are checked. The present inventory does not authorize deletion.
