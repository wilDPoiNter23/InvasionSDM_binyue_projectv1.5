# Analysis workflow

Run from the repository root using an R environment compatible with the supplied model objects. Copy config/config.example.R to config/config.local.R first. Input paths remain external; output paths are configured separately. Do not place credentials in tracked files.

## Main analysis

1. Prepare accepted species and city inventories with code/01_data_preparation. The taxonomic roster is distinct from the contributor list for any one suitability raster.
2. Download and clean occurrences with code/02_occurrence_processing. Retain GBIF download identifiers and record licences. Existing cleaned tables can be used directly. Configure GBIF_DOWNLOADS and OCCURRENCE_OUTPUT; GBIF credentials belong in a private .Renviron file. GeoNames lookup requires GEONAMES_USERNAME.
3. Align marine layers and build the predictor stack with code/03_predictors. The modelling grid is 2.5 arc minutes. Variable selection is part of the species batch script.
4. Fit, evaluate and project species models with code/04_sdm/fit_sdm_batch.R. This implementation uses three-fold spatial partitions. The separate fit_sdm_five_fold.R supplies an alternative five-fold formulation. Their pseudo-absence and fitting settings differ; select the configuration corresponding to the models being reproduced. For matching individual models, retain their saved data, partitions and options.
5. Calculate multispecies suitability with code/05_invasion_suitability/aggregate_suitability.R. The supplied manuscript raster can instead be used directly. The aggregation script normalizes each species raster and uses its successful-contributor count; the deposited raster must include its contributor manifest.
6. Use code/06_city_analysis/prepare_city_table.R for the revised city means and GAM scores. compare_reclaimed_land.R calculates revised land-zone contrasts; plot_city_zone_contrasts.R provides the original comparison/regression components. compare_city_buffers.R supplies city-background and buffer-sensitivity comparisons. Select the city/table version accompanying the figures being reproduced.
7. Calculate the loading table with code/07_pca_sem/pca_loadings.R. Fit the SEM from the archived composite table using fit_sem.R. prepare_sem_composites.R provides the composite-construction equations; recalculating scores can change the fitted coefficients. calculate_city_effects.R uses the reported coefficients for city-level contributions.
8. Run code/08_gam/scale_dependence.R for model summaries, surfaces, conditional curves and diagnostics. Input is the revised city PCA/GAM table. The tensor-product model uses log1p(reclamation_area) and raw city area, with anthropogenic, climate, soil and marine PC1 covariates. Its area sign-change scan brackets a change between sampled area quantiles; 5,621.5 km2 is the lower endpoint, not a fitted exact change point.
9. Use code/09_figures for component plots and code/10_sensitivity for additional model formulations and residual checks. Some tables and manual panel layouts require the supplied numeric source data.

For example:

```sh
Rscript code/04_sdm/fit_sdm_batch.R 1
Rscript code/07_pca_sem/fit_sem.R
Rscript code/08_gam/scale_dependence.R
```

These commands run analyses; they are not data-download or syntax-check commands. Set all required paths and install the script dependencies before execution. The batch index selects species in the input roster. Do not combine output directories from different model configurations.

## Environment-only sensitivity

See ../code/10_sensitivity/environment_only/README.md. The paired analysis includes 684 accepted taxa and excludes 130 of the 814 candidates. It reuses stored designs, model options and accepted continuous FULL projections; human predictors are removed for the ENV fits. Ten candidate taxa required paired clean fits because ID was present. Continuous values are divided by 1000, with no per-species min–max normalization or new native-range mask. A species contributes to either mean only where both predictions are finite, giving identical per-cell denominators. The historical manuscript response remains separate.

The production scripts generate two continuous means, a shared count raster, global/city comparisons and QC tables. The optional city association uses the existing GAM formula and archived covariates on common matched complete cases. No binary aggregate is required.

## R packages

Species modelling uses biomod2, terra, sf, usdm, spThin, dplyr and supporting packages loaded in each script. City analyses also use mgcv, gratia, piecewiseSEM, lme4, ggplot2, patchwork, openxlsx, rstatix and plotting helpers. Install packages separately. BIOMOD2 objects should be read with a compatible package version. The plotbiomes package is required only for the biome-context panel.

See [ENVIRONMENT](ENVIRONMENT.md) for a recorded paired-release R session and the observed 2026-10-13 HPC package descriptions. This evidence does not lock all historical or local analysis environments.

## Practical limits

Use the archived score/composite tables for the manuscript's downstream results. Loading-table PCA includes reclamation and aspect where the GAM-score PCA does not. The precise SEM preprocessing selection is not fully specified, so fresh composite construction is not guaranteed to reproduce the reported coefficients. Regional plots and figure layouts may require the supplied source-data tables. The historical main response and the paired sensitivity use different species sets and aggregation scales; retain their labels and contributor manifests.
