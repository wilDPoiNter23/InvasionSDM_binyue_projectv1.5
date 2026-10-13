# Paired environment-only sensitivity

This analysis compares accepted FULL and environment-only continuous predictions for the same species. The final paired set contains 684 taxa from the 814 candidate taxa; 130 are excluded. The historical manuscript response raster is retained separately and is not replaced by this sensitivity result.

Stored occurrence data, pseudo-absence assignments, cross-validation splits and model options are retained. Human predictors are removed for ENV fits. Taxa with a fitted ID predictor use CLEAN_FULL and CLEAN_ENV fits with ID removed from both. The original candidate workflow required 824 fitting variants; the final paired denominator is determined by the frozen species manifest. Selection thresholds were not lowered and missing ENV predictions were not replaced by FULL predictions.

## Production scripts

- prepare_paired_inputs.py freezes the paired continuous inputs from the accepted baseline inventory and stored completion records. Binary FULL references are resolved to matching existing continuous products. Existing manifests are preserved.
- validate_paired_inputs.R reads completion records, checks paired designs for clean fits and verifies raster geometry. Acceptance of ordinary FULL models comes from the accepted baseline inventory.
- aggregate_paired.R creates two suitability means and one shared valid-species count.
- compare_paired.R calculates global and city comparisons and raster QC.
- check_paired_samples.R compares the finished means and counts with direct species values at sampled grid cells.
- compare_attrition.py summarizes occurrence counts, available baseline FULL performance and predictor counts for paired and excluded taxa. Missing source values remain missing; this is an attrition diagnostic.
- fit_city_association.R applies the existing reclamation GAM formula to historical, paired FULL and paired ENV city responses using a common complete-case sample. Covariates and PCA scores remain unchanged.
- submit_paired.sh is a Slurm template for input checking, aggregation and comparison.
- fit_species.R, model_helpers.R and submit_species.sh describe the species-fitting workflow. Existing accepted fits should be reused.

## Continuous inputs and aggregation

Use paired_species_manifest.csv with species, full_projection and env_projection columns. The frozen working manifest also records model types, scales and completion-record fingerprints. Set input paths to the available continuous projections and their model metadata. GRD inputs require their GRI companions. TSS binary and threshold-filtered rasters are not continuous inputs.

Both groups use the same species and raster grid. Raw BIOMOD continuous values are divided by 1000 to obtain 0-1 suitability; no per-species min-max normalization is used. No native-range mask is introduced. Within each cell, a species contributes only when both its FULL and ENV values are finite. Both means divide by this shared count. A zero count gives missing means and a count of zero.

For N species, the products are:

- full_paired_N_mean_suitability.tif
- env_only_paired_N_mean_suitability.tif
- paired_N_valid_species_count.tif
- full_vs_env_only_paired_N_summary.csv

Additional numeric tables record city means, geometry/value/checksum QC, input checks and attrition diagnostics. No binary aggregates are required.

## Execution

Install compatible terra, digest and mgcv versions. Run Python with its standard library. Species fitting additionally needs biomod2 and its saved model dependencies. Supply private file paths as command arguments; do not put credentials in tracked files.

```sh
python code/10_sensitivity/environment_only/prepare_paired_inputs.py --inventory species_inventory.tsv --output paired_species_manifest.csv
Rscript code/10_sensitivity/environment_only/validate_paired_inputs.R paired_species_manifest.csv paired_input_checks.csv
Rscript code/10_sensitivity/environment_only/aggregate_paired.R --manifest paired_species_manifest.csv --output-dir sensitivity
Rscript code/10_sensitivity/environment_only/compare_paired.R --manifest paired_species_manifest.csv --output-dir sensitivity --cities city_boundaries.shp --city-id shapeName
Rscript code/10_sensitivity/environment_only/check_paired_samples.R paired_species_manifest.csv sensitivity
Rscript code/10_sensitivity/environment_only/fit_city_association.R city_gam_input.csv sensitivity/full_vs_env_only_paired_684_city_comparison.csv sensitivity
```

The frozen inventory and completion records are trusted inputs; file presence alone does not establish scientific acceptance. Numerical input ranges and alignment are checked before and during aggregation. Outputs are written to a new directory. The aggregator accepts --workers; set it to the Slurm CPU allocation (the supplied template requests eight). Workers read disjoint species groups and preserve the same contribution rule.

## Comparison and interpretation

Global correlation, MAE, RMSE and means use finite paired raster cells with equal cell weights. They are not geographic-area-weighted statistics. City means use exact cell-overlap fractions normalized over finite coverage within the final city polygons. Invalid input geometries are repaired in memory with terra::makeValid for both groups; source polygon files remain unchanged and the summary records the repair count. Mean difference is ENV minus FULL. The city-level GAM retains the existing tensor interaction, smooth covariates, basis sizes and REML fitting. Its area scan compares fitted endpoint differences at the original city-area quantiles; it does not estimate a new exact scale threshold. Outputs are descriptive sensitivity results, not causal estimates.
