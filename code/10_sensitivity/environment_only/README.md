# Environment-only sensitivity analysis

This planned manuscript analysis refits the species distribution models after removing the human predictors. The stored occurrence data, pseudo-absence assignments, cross-validation splits, algorithm routes and model options are retained. Terms involving excluded predictors are removed from model formulas. Species with a fitted `ID` predictor use paired `CLEAN_FULL` and `CLEAN_ENV` fits with `ID` removed from both sides.

The analysis covers 814 taxa and 824 fitting tasks: 804 environment-only fits and ten pairs of clean full and environment-only fits. The final aggregate raster and comparison tables are unavailable as of 12 October 2026. This directory contains the current species modelling workflow; it does not yet provide a completed 814-taxon sensitivity result.

## Scripts

- `fit_species.R` fits and projects one taxon, preserves its stored design, selects models under the saved route policy and checks raster coverage and numerical predictions.
- `model_helpers.R` subsets predictors, prunes formulas and compares model options.
- `submit_species.sh` is a Slurm template for one taxon and variant. Set the account, partition and R environment for the destination cluster.

The cluster scheduling controllers, retry tools and diagnostic experiments are outside this publication workflow. Existing valid models should be reused. The script refuses to overwrite an output carrying its completion record.

## Inputs and execution

Copy `config/config.example.R` to `config/config.local.R` and configure `PROJECT_ROOT`, `ENV_ONLY_ROOT`, `CLIMATE_STACK`, `ENV_PRIMARY_STACK` and `ENV_VIIRS_STACK`. Install `biomod2`, `terra`, `jsonlite` and `digest`; use an R and BIOMOD2 environment compatible with the saved model objects. Run from the repository root, or export `INVASIONSDM_PROJECT_ROOT`.

The external working directory must contain:

- `sensitivity_inventory.tsv`: one row per taxon, with valid local paths to the stored model, formatted data, calibration table, options, evaluations and baseline projection; model paths embedded inside BIOMOD2 objects must also resolve.
- `static_preflight_814.csv`: the matching 814-taxon design checks.
- `frozen_route_policies.json`: the stored route-specific model-selection policies.

Relocate file paths in the input inventory before use. Keep each output root below `ENV_ONLY_ROOT/production_v2/`. Inputs are fingerprinted in the saved-fit contract; do not change them during a run.

```sh
Rscript code/10_sensitivity/environment_only/fit_species.R "Accepted taxon" ENV
sbatch code/10_sensitivity/environment_only/submit_species.sh "Accepted taxon" ENV
```

The ordinary route removes `reclamation_raster`, `VIIRS_2023_2.5min`, `GDP2020_2.5min`, `BUILT_S_E2025_2.5min` and `Pop2020_2.5min` when present. Predictor order and other variables are retained. The saved selection policy uses strict TSS and ROC thresholds of 0.8, then 0.7, with route-specific minimum model counts and single-model fallback rules. Projection uses the corresponding 2.5-arc-minute predictor stack and the baseline global raster domain. Saved model designs and numerical checks are retained with each species output.

## Aggregation and comparison still to be completed

The author-defined continuous aggregate is the per-cell mean of finite, non-missing species values, using the number of valid species as the denominator. Cells with no valid species remain missing. Apply the same implementation to the paired full and environment-only products, after the accepted taxon set and non-native masks have been fixed.

The intended aggregate products are the continuous mean and valid-species count, plus the TSS binary suitable-taxon share and its valid-species count, for both model groups. The final aggregation script, threshold/mask manifest and comparison script will be added with the completed 814-taxon products. The exact normalization and comparison summaries must follow that final implementation before comparison with the manuscript response raster. Earlier 813-taxon aggregation and exploratory comparisons do not supply these missing 814-taxon outputs.
