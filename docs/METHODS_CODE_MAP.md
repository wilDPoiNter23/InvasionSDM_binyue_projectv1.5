# Manuscript analyses and code

The table identifies statistical calculations and component plots. Final multi-panel layouts can be assembled from these outputs and the deposited source data.

| Manuscript item | Script(s) | Main input |
|---|---|---|
| Main Fig. 1 | `code/01_data_preparation/prepare_species_regions.R`; `code/01_data_preparation/prepare_reclamation_cities.R`; `code/09_figures/biome_context.R` | Occurrence/city inventory and city climate table |
| Main Fig. 2 | `code/05_invasion_suitability/aggregate_suitability.R`; `code/06_city_analysis/compare_city_buffers.R` | Manuscript raster, city geography and city-background tables |
| Main Fig. 3; Extended Data Table 2 | `code/07_pca_sem/fit_sem.R`; `code/07_pca_sem/calculate_city_effects.R` | Archived SEM composites and reported coefficient table |
| Main Fig. 4; Extended Data Fig. 4; Supplementary Fig. 7 | `code/08_gam/scale_dependence.R` | Revised city PCA/GAM table |
| Main Fig. 5; Extended Data Fig. 5 | `code/09_figures/city_effects.R`; `code/09_figures/regional_effects.R` | Coefficient-based city table |
| Extended Data Fig. 1; Extended Data Fig. 3 | `code/06_city_analysis/compare_reclaimed_land.R`; `code/06_city_analysis/plot_city_zone_contrasts.R` | City/land-zone summaries |
| Extended Data Fig. 2; Supplementary Fig. 6 | `code/09_figures/model_performance.R`; `code/09_figures/predictor_importance.R` | Model performance and importance summaries |
| Extended Data Table 1; Supplementary Fig. 2 | `code/06_city_analysis/compare_city_buffers.R` | City-background comparison and buffer tables |
| Supplementary Table 3 | `code/08_gam/scale_dependence.R`; `code/10_sensitivity/raw_proxy_gam.R` | GAM input and the coefficient/term tables accompanying the selected figure version |
| Extended Data Table 4 | `metadata/predictor_list.csv`; `docs/DATA.md` | Dataset source and layer metadata |
| Supplementary Fig. 1 | `docs/WORKFLOW.md` | Workflow diagram arranged separately |
| Supplementary Fig. 3 | `code/03_predictors/calculate_correlations.R`; `code/09_figures/predictor_correlations.R` | Predictor stack and correlation table |
| Supplementary Fig. 4 | `code/09_figures/predictor_importance.R` | Importance/selection-frequency table |
| Supplementary Fig. 5 | `code/09_figures/calculate_response_curves.R`; `code/09_figures/plot_response_curves.R` | Fitted models and numeric response curves |
| Supplementary Fig. 8; Supplementary Table 4 | `code/10_sensitivity/spatial_diagnostics.R` | City PCA/GAM table and residual coordinates |
| Supplementary Table 1 | `code/01_data_preparation/prepare_reclamation_cities.R` | 108-city metadata |
| Supplementary Table 2 | `code/07_pca_sem/pca_loadings.R` | PCA loading-table input |
| Supplementary Table 5 | `code/09_figures/regional_effects.R` | Coefficient-based city table |
| Supplementary Table 6 | `Deposited quartile cross-tab source data` | Pressure/leverage group counts and city names; table layout is supplied separately |
| Environment-only sensitivity | `code/10_sensitivity/environment_only/` | Saved species model designs, predictor stacks and the paired inventory |

## Scientific details

Reviewer-informed downstream sensitivities are documented in
[REVIEWER_SENSITIVITY](REVIEWER_SENSITIVITY.md). Their stage-specific scripts under
code/10_sensitivity/ provide new paired-product comparisons, explicit GAM
parameterisations, geographic influence and residual diagnostics. These outputs
are separate from the submitted figure/table versions listed above. Revised
residual diagnostics use the common final GAM sample; archived diagnostics may
use a different sample and neighbourhood definition.

The original land-zone figures and revised boundary calculations are separate script inputs. Use the version accompanying the figure/source-data file. The supplied GAM coefficient summaries should match the selected figure version: the older Extended Data smooth summary and alternative-model table are not interchangeable with the revised tensor-product output.

Loading-table PCA uses mean imputation and includes reclamation/aspect in the relevant groups. GAM-score PCA uses complete cases and excludes those variables. The SEM fits the archived composite table, while the city-effect calculation uses the reported coefficients. These definitions are explicit in the scripts.

The threshold scan identifies an interval between area quantiles; it does not estimate a unique breakpoint. Spatial and raw-proxy sensitivity scripts contain their own additional terms and may use different complete-case samples. The environment-only directory contains input freezing, aggregation, comparison, attrition and city-association scripts for the final 684-species paired set.

Extended Data Table 3 is supplied as manuscript source data; its smooth-term summary differs from the main tensor-product output and is not reconstructed by assigning it to a different model. The GAM comparison script writes summaries calculated from the fitted objects; precompiled table values are supplied separately.
