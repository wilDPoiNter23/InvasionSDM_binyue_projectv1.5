# Data and variable definitions

The Zenodo package is being prepared. Until a deposit identifier is available, zenodo/zenodo_manifest.csv records the proposed assets and filenames. Standard source datasets should be obtained from their providers rather than duplicated in the package.

## External inputs

| Data | Provider / release information | Expected input | Identifier / citation |
|---|---|---|---|
| Species occurrences | GBIF download selected for the analysis | Species occurrence CSVs and download metadata | Retain the dataset-specific GBIF download DOI and licences; a download DOI is not yet listed here |
| Taxonomy and regional flora | GloNAF and the accepted-name list | GloNAF tables and species roster | Supply the exact flora/taxonomy release with the dataset metadata |
| Bioclimatic variables | WorldClim 2.1, 1970-2000, 2.5 arc minutes | wc2.1_2.5m_bio_1 through bio_19 | [Provider and citation](https://www.worldclim.org/data/worldclim21.html): Fick and Hijmans (2017), International Journal of Climatology 37, 4302-4315 |
| Soil properties | ISRIC SoilGrids; depth/aggregation details belong with the aligned layers | bdod, cfvo, nitrogen, ocd, phh2o, sand, silt, soc and any retained clay layer | [Provider](https://www.isric.org/explore/soilgrids); retain the exact download release and depth metadata |
| Marine variables | HYCOM-labelled inputs; precise source release to be supplied | Salinity and temperature means/ranges | Retain the source release and temporal aggregation information |
| Elevation and terrain | Source elevation and derived slope/aspect | wc2.1_2.5m_elev, slope, aspect | [WorldClim elevation](https://www.worldclim.org/data/worldclim21.html) is SRTM-derived; identify the source used for each terrain layer |
| Population | Population product used for the 2020 layer | Pop2020_2.5min | Supply the exact product edition and download DOI |
| GDP | Gridded GDP product used for the 2020 layer | GDP2020_2.5min | Supply the provider, edition and DOI |
| Night lights | VIIRS-labelled 2023 layer | VIIRS_2023_2.5min | Supply the product edition and processing description |
| Built-up land | Built-up product labelled E2025 | BUILT_S_E2025_2.5min | Supply the product edition and download citation |
| Reclamation and city geography | Author-derived and mixed-source vectors | Reclamation polygons and original/revised 108-city boundaries | Confirm source rights and include all shapefile companions or a portable vector export |

Filename years identify the files used; they do not establish the acquisition period of every source product. Download metadata and redistribution terms must accompany the selected release. SoilGrids' provider page identifies its public maps as CC-BY 4.0; this does not replace the metadata for the particular layers/transformations used here. External processed products with unspecified rights remain undecided in the deposit manifest.

## Supplied analysis data

- The manuscript suitability raster is hotspot_mean_suitability_normalized2.0.tif, proposed for deposit as manuscript_suitability.tif.
- The revised city means and city_gam_input table contain 108 cities. Model samples depend on the fields required by each formula; the reported tensor-product GAM uses 84 complete records.
- The PCA reference/loading input and the revised GAM score table are separate assets because their PCA definitions differ.
- The archived SEM composite input contains Climate, socioeconomic, Soil, reclamation and Water. Its risk response has undergone the supplied preprocessing; do not assume it is on the same scale as the untransformed city suitability mean.
- City-effect tables, curve/surface data, coefficient summaries, performance tables and spatial diagnostics support the figure components.
- Species rosters and projection contributor manifests describe different sets. The metadata/species_list.csv file is a taxonomic roster of 813 names, not the contributor denominator of the manuscript raster. The sensitivity candidate roster has 814 taxa, with 684 final paired taxa and 130 excluded; the original contributor candidate manifest has 737 rows.

See ../metadata/predictor_list.csv and ../metadata/city_variables.csv. The aligned predictor stack generally uses the 2.5-arc-minute grid. Preserve predictor order, units, missing values and per-species selections. ID is an identifier rather than an environmental predictor.

## Units and configuration

City area is in km2. The GAM reclamation input is in hectares; conditional-curve exports convert it to km2 by dividing by 100. The additive model retains the supplied raw area-ratio definition. Do not reinterpret that ratio as a dimensionless land fraction without checking the input conversion. PC scores and SEM composites are different covariates and should not be interchanged.

File-valued config entries can point directly to the downloaded package files. Directory-valued entries retain supporting subtrees used by the scripts: occurrence files, per-species model caches, predictor layers, geometry and figure source tables. The config example documents these roots; readers can adapt them to a local data layout without changing model formulas. Non-English input identifiers are retained as Unicode escapes in source where required for matching.

## Package completion

Record a licence, creators, version, related manuscript/code identifiers and checksums for the selected deposit. The paired sensitivity uses continuous predictions divided by 1000, with a common finite-pair species count and no new native-range mask. It remains separate from the normalized historical response. The deposit licence, some source versions and final figure-to-table selections require confirmation before uploading. No DOI or licence is assigned here.
