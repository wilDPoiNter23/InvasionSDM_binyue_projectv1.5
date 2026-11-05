# Manuscript analyses and code

This index links the manuscript figures and tables to their analysis scripts
and inputs. Configure external data paths as described in [DATA](DATA.md).
[WORKFLOW](WORKFLOW.md) gives the execution order. Final maps, satellite-image
panels and conceptual diagrams also use spatial or artwork assets for assembly.

## Main figures

| Item | Analysis or component scripts | Inputs |
| --- | --- | --- |
| Fig. 1: study context | `code/01_data_preparation/prepare_species_regions.R`; `code/01_data_preparation/prepare_reclamation_cities.R`; `code/09_figures/biome_context.R` | Occurrence and city/region inventories, reclamation areas and climate fields in `GAM_INPUT`; spatial layers and satellite scenes for panel assembly. |
| Fig. 2: suitability and spatial contrasts | `code/05_invasion_suitability/aggregate_suitability.R`; `code/06_city_analysis/compare_city_buffers.R`; `code/09_figures/render_original_surface_boxplot.R` | `MAIN_RESPONSE_RASTER`, city/reclamation geometry, city-background tables, `all_108_city_comparisons.csv` and `all_108_city_statistics.csv`. |
| Fig. 3: SEM paths and effects | `code/07_pca_sem/prepare_sem_composites.R`; `code/07_pca_sem/fit_sem.R` | `SEM_INPUT` composites and `sem_coefficients.csv`. |
| Fig. 4a-c: scale dependence | `code/08_gam/scale_dependence.R` | `GAM_INPUT`, `TableS1_scale_effect_model_summary.csv`, `step1B_area_slope_scan.csv`, `step1B_area_conditional_peaks.csv` and curve/surface exports. |
| Fig. 4d-e: city/reclaimed habitat | `code/09_figures/render_city_reclaimed_habitat_comparison.R` | Saved whole-city and mapped-reclamation fraction/area GAMs, `matched_city_data.csv` and `reclamation_contrasts.csv`. Arguments: model directory and figure directory. |
| Fig. 5: city contributions and typology | `code/07_pca_sem/calculate_city_effects.R`; `code/09_figures/city_effects.R`; `code/09_figures/regional_effects.R` | `SEM_INPUT`, `CITY_EFFECT_TABLE` and the supporting workbook under `LOCAL_ANALYSIS_ROOT`. Dependency quadrants use `regional_effects.R`. |

## Supplementary figures

| Item | Analysis or component scripts | Inputs |
| --- | --- | --- |
| Fig. S1: workflow | `docs/WORKFLOW.md` | Workflow stages and diagram layout. |
| Fig. S2: SDM performance | `code/09_figures/model_performance.R`; `code/09_figures/render_candidate_model_validation.R`; `code/09_figures/render_foldmatched_ensemble_validation.R` | `MODEL_EVALUATIONS` / `SELECTED_MODEL_EVALUATIONS`; candidate scores and species summary; or the ensemble manifest and per-species fold tables. |
| Fig. S3: reclaimed/non-reclaimed contrasts | `code/06_city_analysis/compare_reclaimed_land.R`; `code/06_city_analysis/plot_city_zone_contrasts.R` | Response raster, city/reclamation boundaries and land-zone summaries. |
| Fig. S4: reclamation cohorts | `code/10_sensitivity/optional_exploratory/coastal_period_association.R`; `code/09_figures/render_coastal_history_figures.R` | Saved cohort estimates and `source_manifest.csv`. Renderer selection: `formation_cohorts`. |
| Fig. S5: reclamation land use | `code/10_sensitivity/optional_exploratory/coastal_bayes_report.R`; `code/09_figures/render_species_richness_panels.R` | Saved FULL/ENV contrasts, prediction grids, zone tables and model-input data frames. Renderer selection: `land_use`. |
| Fig. S6: buffer sensitivity | `code/06_city_analysis/compare_city_buffers.R` | Response raster, city boundaries and comparisons at each buffer width. |
| Fig. S7: sensitivity pathways | `code/10_sensitivity/environment_only/run_city_component_inputs.R`; `code/09_figures/render_pathway_scale_figures.R` | Saved component effects, edges, support and intervals. Renderer selection: `S14` (the script's output identifier). |
| Fig. S8: urban activity | `code/10_sensitivity/optional_exploratory/reclamation_human_associations.R`; `code/09_figures/render_revision_synthesis.R` | Saved human-activity association tables and JSON configuration. Renderer selection: `human_activity`. |
| Fig. S9: recorded richness | `code/10_sensitivity/optional_exploratory/observed_richness_association.R`; `code/09_figures/render_species_richness_panels.R` | Common-window observations, count-model coefficients, dispersion, fitted values and continental residuals/tests. |
| Fig. S10: suitable-area pathways | `code/10_sensitivity/optional_exploratory/paired_response_sem.R`; `code/09_figures/render_suitable_area_pathways.R` | Saved `effects.csv`, `edges.csv`, `draws.csv`, `support.csv` and `verification.json`. Arguments: model directory and figure directory. |
| Fig. S11: univariate associations | `code/06_city_analysis/plot_city_zone_contrasts.R`; `code/06_city_analysis/compare_reclaimed_land.R` | City/land-zone table, city area, reclamation extent/intensity and suitability summaries. |
| Fig. S12: partial/conditional effects | `code/08_gam/scale_dependence.R` | City PCA/GAM table and selected partial-effect/conditional-curve exports. |
| Fig. S13: FULL/ENV habitat comparisons | `code/09_figures/render_city_reclaimed_full_env_sensitivity.R` | Saved FULL/ENV mean, fraction and area models, common profiles from the Fig. 4d-e renderer, and `reclaimed_land_response_contrasts.csv`. |
| Fig. S14: mechanistic typologies | `code/09_figures/regional_effects.R`; `code/09_figures/city_effects.R` | City table with region, suitability, direct/indirect/total contributions and dependency fields. |
| Fig. S15: city selection | `code/10_sensitivity/sampling_selection/compare_ghsl_selection.R`; `code/09_figures/render_ghsl_selection.R` | GHSL crosswalk, unit covariates, coverage, occurrence counts and saved comparison tables; see [city selection](ghsl_sample_selection.md). |
| Fig. S16: predictor correlations | `code/03_predictors/calculate_correlations.R`; `code/09_figures/predictor_correlations.R` | Predictor stack and `PREDICTOR_CORRELATIONS`. |
| Fig. S17: weights and importance | `code/09_figures/model_performance.R`; `code/09_figures/predictor_importance.R` | Evaluation summaries, `PREDICTOR_IMPORTANCE` and predictor rasters. |
| Fig. S18: threshold sensitivity | `code/09_figures/render_reclaimed_land_responses.R` | `curves.csv`, `effect.csv` and `continuous_city_inputs.csv`, with response-group and threshold labels. Uses JSON configuration. |
| Fig. S19: reclamation maps | `code/09_figures/render_study_area_reclamation.R` | City boundaries/locations, reclamation polygons, land-use attributes and mapped year classes. Uses JSON configuration. |
| Fig. S20: GAM diagnostics | `code/08_gam/scale_dependence.R` | Selected GAM's model frame and diagnostic outputs. |
| Fig. S21: spatial correlogram | `code/10_sensitivity/spatial_diagnostics.R` | City PCA/GAM table, coordinates, residuals and distance-bin/permutation settings. |

## Supplementary tables

| Item | Analysis or component scripts | Inputs |
| --- | --- | --- |
| Table S1: predictors | `metadata/predictor_list.csv`; `docs/DATA.md` | Dataset metadata and the model predictor-layer manifest. |
| Table S2: 108 cities | `code/01_data_preparation/prepare_reclamation_cities.R` | City inventory, coordinates, regions, reclamation areas and occurrence/richness fields. |
| Table S3: hierarchical contrasts | `code/10_sensitivity/optional_exploratory/coastal_bayes_report.R`; `code/09_figures/render_species_richness_panels.R` | Saved FULL/ENV Bayesian contrast tables. |
| Table S4: city/background statistics | `code/06_city_analysis/compare_city_buffers.R` | City/background values and group summaries. |
| Table S5: SEM coefficients | `code/07_pca_sem/fit_sem.R` | Archived composites and `sem_coefficients.csv`. |
| Table S6: ENV suitability pathways | `code/10_sensitivity/environment_only/run_city_component_inputs.R` | Saved continuous component effects/support and `continuous_component_sem.csv`. |
| Table S7: observed-richness pathways | `code/10_sensitivity/optional_exploratory/paired_response_sem.R`; `code/09_figures/render_species_richness_panels.R` | Observed-component effects/support and `OBSERVED_M0` rows of `T12_observed_component_sem.csv`. |
| Table S8: suitable-area pathways | `code/10_sensitivity/optional_exploratory/paired_response_sem.R`; `code/09_figures/render_suitable_area_pathways.R` | Saved threshold-specific component exports and `suitable_area_pathway_sensitivity_statistics.csv`. |
| Table S9: GAM alternatives | `code/08_gam/scale_dependence.R` | City PCA/GAM table and model-comparison exports. |
| Table S10: GAM smooth terms | `code/08_gam/scale_dependence.R` | Selected GAM and its smooth-term summary. |
| Table S11: reclaimed responses | `code/10_sensitivity/environment_only/fit_reclaimed_mean_relationships.R`; `code/09_figures/render_reclaimed_land_responses.R` | Saved response models, curves and `reclaimed_land_response_contrasts.csv`. |
| Table S12: continental contributions | `code/09_figures/regional_effects.R` | City-effect table, continental summaries and pairwise comparisons. |
| Table S13: joint quartiles | `code/07_pca_sem/calculate_city_effects.R` | City suitability, absolute net contributions, quartile definitions, city memberships and the cross-tab source table. |
| Table S14: PCA loadings | `code/07_pca_sem/pca_loadings.R` | `PCA_LOADING_INPUT` and archived loading output; see [model definitions](REPRODUCIBILITY.md#model-definitions). |
| Table S15: spatial-bin sensitivity | `code/10_sensitivity/spatial_diagnostics.R` | GAM model frame, coordinates and `m_scale_B_bin_sensitivity.csv`. |

## Running the components

Use the saved city tables, model summaries and prediction exports for downstream
figures. Each renderer lists its arguments at the start of the script.
The Fig. 4d-e renderer creates common prediction profiles used by the FULL/ENV
companion.

See [REPRODUCIBILITY](REPRODUCIBILITY.md#model-definitions) for model definitions,
[response comparisons](reclaimed_land_response_figures.md) for habitat panels,
[hierarchical/richness figures](species_richness_figures.md) for contrast and
count-model inputs, and [ensemble validation](ensemble_validation.md) for
evaluation tables.

Additional analyses are described in
[REVIEWER_SENSITIVITY](REVIEWER_SENSITIVITY.md) and the
[environment-only workflow](../code/10_sensitivity/environment_only/README.md).
