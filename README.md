# Coastal reclamation and invasive-plant suitability

This repository contains the code used to model the global distribution of invasive vascular plants and to examine how predicted invasion suitability varies across coastal reclamation and urban environmental gradients.

The workflow links species occurrences and environmental predictors to species distribution models, multispecies suitability, city-level comparisons, PCA, SEM and scale-dependent GAMs. The environment-only sensitivity compares 684 taxa with accepted paired FULL and ENV predictions, from 814 candidate taxa.

## Contents

| Directory | Contents |
|---|---|
| code/01_data_preparation | Species, regional and reclamation inventories |
| code/02_occurrence_processing | Occurrence download and cleaning |
| code/03_predictors | Aligned predictor stacks and correlations |
| code/04_sdm | Species model fitting, evaluation and projection |
| code/05_invasion_suitability | Multispecies suitability aggregation |
| code/06_city_analysis | City tables, reclaimed-land contrasts and buffer comparisons |
| code/07_pca_sem | PCA loadings, SEM composites and city effects |
| code/08_gam | Scale-dependent GAMs and conditional curves |
| code/09_figures | Statistical figure components |
| code/10_sensitivity | Spatial, raw-proxy and environment-only analyses |
| config | External data and output paths |
| metadata | Species roster, predictors and city-variable definitions |
| docs | Workflow, data requirements and manuscript correspondence |
| zenodo | Data-package plan and asset manifest |

## Start

Read [WORKFLOW](docs/WORKFLOW.md), obtain the files described in [DATA](docs/DATA.md), and copy config/config.example.R to config/config.local.R. Set the external data and output paths. Run scripts from the repository root. Model fitting is computationally expensive; downstream analyses can start from the supplied city tables and suitability raster.

[METHODS_CODE_MAP](docs/METHODS_CODE_MAP.md) identifies the scripts for the manuscript analyses and figure components. The [data-package plan](zenodo/README.md) lists files intended for Zenodo; the deposit has not yet been published. Large inputs, fitted models and rasters are not stored in Git.

## Interpretation and reproduction

The response is predicted invasion suitability, rather than observed establishment or propagule pressure. Scripts retain the original variable names, including risk in city tables. Use the supplied inputs and model-specific settings to reproduce the corresponding analysis. The loading-table PCA and GAM-score PCA use different variable groups; the archived SEM composite table is needed for the reported coefficient analysis. Conditional GAM sign changes are reported as area intervals rather than exact breakpoints.

Manual arrangement of figure panels is separate from the component plots. This repository provides the statistical calculations and component-generation scripts.

See [REPRODUCIBILITY](docs/REPRODUCIBILITY.md) for source preservation, remaining historical lineage and SEM preprocessing requirements, and a static validation command that runs no analyses.

## Citation

Use CITATION.cff to cite the code and cite each external dataset separately. The data-package DOI and release metadata will be added when the deposit is available.
