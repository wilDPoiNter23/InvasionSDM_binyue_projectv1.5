# Manuscript analyses and code

This index follows the current manuscript: five main figures, 21
supplementary figures and 15 supplementary tables. It identifies analysis and
plotting components with their inputs. Several items can share one script.
Final panel arrangement, conceptual artwork and satellite-image layouts are
prepared separately.

An entry identifies code by analytical role; it does not certify reproduction
of every printed value or the final image. Missing components and material
method differences are described below. Input tables, saved models, rasters
and image assets remain external to Git. See [DATA](DATA.md) and the
[data-package plan](../zenodo/README.md) for availability; the deposit is not
yet published. Legacy output filenames retain earlier figure numbers; use this
index to identify the current manuscript item.

## Main figures

| Item | Analysis or component scripts | Inputs and scope |
| --- | --- | --- |
| Fig. 1: study context | `code/01_data_preparation/prepare_species_regions.R`; `code/01_data_preparation/prepare_reclamation_cities.R`; `code/09_figures/biome_context.R` | Occurrences, city/region inventories, reclamation areas and climate fields in GAM_INPUT. These cover inventory and climate-space components; the occurrence-grid map, Landsat panels and continental bar-chart layout still need their original spatial/image sources. |
| Fig. 2: suitability and spatial contrasts | `code/05_invasion_suitability/aggregate_suitability.R`; `code/06_city_analysis/compare_city_buffers.R`; `code/09_figures/render_original_surface_boxplot.R` | Manuscript MAIN_RESPONSE_RASTER, city/reclamation geometry and archived city-background / all_108_city_comparisons.csv / all_108_city_statistics.csv tables. The global map and class-area panel need separate mapping/classification sources. The aggregate script uses a distinct scaling/denominator; use the manuscript raster for historical downstream replication. |
| Fig. 3: SEM paths and effects | `code/07_pca_sem/prepare_sem_composites.R`; `code/07_pca_sem/fit_sem.R` | Archived SEM_INPUT composites and sem_coefficients.csv. The available fit is a piecewise system on composite columns; see the method note below. |
| Fig. 4a-c: scale dependence | `code/08_gam/scale_dependence.R` | Historical GAM_INPUT, TableS1_scale_effect_model_summary.csv, step1B_area_slope_scan.csv, step1B_area_conditional_peaks.csv and curve/surface exports. The archived main result uses 84 complete cities; its lower sign-change scan endpoint is 5,621.5 km2. |
| Fig. 4d-e: city/reclaimed habitat | `code/09_figures/render_city_reclaimed_habitat_comparison.R` | Saved whole-city and mapped-reclamation fraction/area GAMs, matched_city_data.csv and reclamation_contrasts.csv. Takes model and figure directories as two arguments; exports curves, contrasts, formulas and prediction profiles from saved fits. |
| Fig. 5: city contributions and typology | `code/07_pca_sem/calculate_city_effects.R`; `code/09_figures/city_effects.R`; `code/09_figures/regional_effects.R` | Archived SEM_INPUT, CITY_EFFECT_TABLE and the supporting city workbook under LOCAL_ANALYSIS_ROOT. Use regional_effects.R for the manuscript's absolute-contribution dependency definition. Conceptual artwork is arranged separately. |

## Supplementary figures

| Item | Analysis or component scripts | Inputs and scope |
| --- | --- | --- |
| Fig. S1: workflow | `docs/WORKFLOW.md` | Stage descriptions and the separately arranged workflow diagram. |
| Fig. S2: SDM performance | `code/09_figures/model_performance.R`; `code/09_figures/render_candidate_model_validation.R`; `code/09_figures/render_foldmatched_ensemble_validation.R` | MODEL_EVALUATIONS / SELECTED_MODEL_EVALUATIONS; candidate scores and species summary; or ensemble manifest and per-species fold tables. Candidate and fold-matched ensemble evaluations are separate routes; see ensemble_validation.md. |
| Fig. S3: reclaimed/non-reclaimed contrasts | `code/06_city_analysis/compare_reclaimed_land.R`; `code/06_city_analysis/plot_city_zone_contrasts.R` | Matching response raster, city/reclamation boundaries and saved land-zone summaries. |
| Fig. S4: reclamation cohorts | `code/10_sensitivity/optional_exploratory/coastal_period_association.R`; `code/09_figures/render_coastal_history_figures.R` | Saved cohort estimates and the existing figure directory's source_manifest.csv; renderer selection formation_cohorts. |
| Fig. S5: reclamation land use | `code/10_sensitivity/optional_exploratory/coastal_bayes_report.R`; `code/09_figures/render_species_richness_panels.R` | Saved FULL/ENV Bayesian contrasts, prediction grids, zone tables and model-input data frames; renderer selection land_use. See species_richness_figures.md. |
| Fig. S6: buffer sensitivity | `code/06_city_analysis/compare_city_buffers.R` | Manuscript response raster, matching boundaries and city-background comparisons at each buffer width. |
| Fig. S7: sensitivity pathways | `code/10_sensitivity/environment_only/run_city_component_inputs.R`; `code/09_figures/render_pathway_scale_figures.R` | Saved continuous component effects, edges, support and intervals. The renderer's legacy S14 selection refers to this pathway component. |
| Fig. S8: urban activity | `code/10_sensitivity/optional_exploratory/reclamation_human_associations.R`; `code/09_figures/render_revision_synthesis.R` | Saved human-activity association tables supplied through JSON configuration; renderer selection human_activity. |
| Fig. S9: recorded richness | `code/10_sensitivity/optional_exploratory/observed_richness_association.R`; `code/09_figures/render_species_richness_panels.R` | Common-window observations, saved count-model coefficients, dispersion, fitted values and continental residuals/tests. |
| Fig. S10: suitable-area pathways | `code/10_sensitivity/optional_exploratory/paired_response_sem.R`; `code/09_figures/render_suitable_area_pathways.R` | FULL and matched component exports: effects.csv, edges.csv, draws.csv, support.csv and verification.json. Renderer takes existing model and figure directories. |
| Fig. S11: univariate associations | `code/06_city_analysis/plot_city_zone_contrasts.R`; `code/06_city_analysis/compare_reclaimed_land.R` | Matching city/land-zone table, city area, reclamation extent/intensity and suitability summaries. |
| Fig. S12: partial/conditional effects | `code/08_gam/scale_dependence.R` | Historical PCA/GAM table, selected model and its partial-effect/conditional-curve exports. |
| Fig. S13: FULL/ENV habitat comparisons | `code/09_figures/render_city_reclaimed_full_env_sensitivity.R` | Saved matched FULL/ENV mean, fraction and area models, common profiles from the Fig. 4d-e renderer, and reclaimed_land_response_contrasts.csv. |
| Fig. S14: mechanistic typologies | `code/09_figures/regional_effects.R`; `code/09_figures/city_effects.R` | Archived city table with region, suitability, direct/indirect/total contributions and dependency fields. |
| Fig. S15: city selection | `code/10_sensitivity/sampling_selection/compare_ghsl_selection.R`; `code/09_figures/render_ghsl_selection.R` | GHSL crosswalk, unit covariates, coverage and cached occurrence counts; renderer reads saved comparisons. This route has a different population from the caption's 108/27 split; see ghsl_sample_selection.md. |
| Fig. S16: predictor correlations | `code/03_predictors/calculate_correlations.R`; `code/09_figures/predictor_correlations.R` | Predictor stack and PREDICTOR_CORRELATIONS. |
| Fig. S17: weights and importance | `code/09_figures/model_performance.R`; `code/09_figures/predictor_importance.R` | Evaluation summaries, PREDICTOR_IMPORTANCE and predictor rasters. Legacy algorithm weights use calibration TSS; CV weights require their corresponding score source. |
| Fig. S18: threshold sensitivity | `code/09_figures/render_reclaimed_land_responses.R` | Saved curves.csv, effect.csv and continuous_city_inputs.csv, with FULL/paired groups and T10/MAX_TSS labels. Uses JSON configuration. |
| Fig. S19: reclamation maps | `code/09_figures/render_study_area_reclamation.R` | JSON-configured city boundaries/locations, reclamation polygons, land-use attributes and mapped year classes. |
| Fig. S20: GAM diagnostics | `code/08_gam/scale_dependence.R` | Selected model's city table/model frame and diagnostic outputs. |
| Fig. S21: spatial correlogram | `code/10_sensitivity/spatial_diagnostics.R` | Matching GAM_INPUT, coordinates and residuals, with retained distance-bin and permutation settings. |

## Supplementary tables

| Item | Analysis or component scripts | Inputs and scope |
| --- | --- | --- |
| Table S1: predictors | `metadata/predictor_list.csv`; `docs/DATA.md` | Provider/release metadata and the layer manifest for the selected model stack. |
| Table S2: 108 cities | `code/01_data_preparation/prepare_reclamation_cities.R` | Archived city inventory, coordinates, region assignments, reclamation areas and occurrence/richness fields. |
| Table S3: hierarchical contrasts | `code/10_sensitivity/optional_exploratory/coastal_bayes_report.R`; `code/09_figures/render_species_richness_panels.R` | Saved FULL814 and ENV684 Bayesian contrasts; the manuscript displays a subset of the available contrasts. |
| Table S4: city/background statistics | `code/06_city_analysis/compare_city_buffers.R` | Archived city/background values and group summaries, retaining their weighting and paired-city definitions. |
| Table S5: SEM coefficients | `code/07_pca_sem/fit_sem.R` | Archived composites and sem_coefficients.csv; see the SEM method note below. |
| Table S6: ENV suitability pathways | `code/10_sensitivity/environment_only/run_city_component_inputs.R` | Saved continuous component effects/support; ENV684 rows of continuous_component_sem.csv. |
| Table S7: observed-richness pathways | `code/10_sensitivity/optional_exploratory/paired_response_sem.R`; `code/09_figures/render_species_richness_panels.R` | Observed-component effects/support; OBSERVED_M0 rows of T12_observed_component_sem.csv. The record-count-adjusted M1 companion is separate. |
| Table S8: suitable-area pathways | `code/10_sensitivity/optional_exploratory/paired_response_sem.R`; `code/09_figures/render_suitable_area_pathways.R` | Saved threshold-specific component exports and suitable_area_pathway_sensitivity_statistics.csv. |
| Table S9: GAM alternatives | `code/08_gam/scale_dependence.R` | Matching historical city table and model-specific comparison exports; preserve each row's response, sample and covariate definitions. |
| Table S10: GAM smooth terms | `code/08_gam/scale_dependence.R` | Selected historical GAM and its term summary. The manuscript summary differs from the archived Fig. 4a-c summary. |
| Table S11: reclaimed responses | `code/10_sensitivity/environment_only/fit_reclaimed_mean_relationships.R`; `code/09_figures/render_reclaimed_land_responses.R` | Saved response models, curves and reclaimed_land_response_contrasts.csv; retains 70-city support and response transformations. |
| Table S12: continental contributions | `code/09_figures/regional_effects.R` | Archived city-effect table and continental summaries. Code uses Dunn Bonferroni; the manuscript's FDR/letter display needs its comparison source. |
| Table S13: joint quartiles | `code/07_pca_sem/calculate_city_effects.R` | City suitability and absolute net contributions, quartile definitions and city memberships. The script supplies contribution/bivariate components; the exact 16-cell cross-tab is supplied separately. |
| Table S14: PCA loadings | `code/07_pca_sem/pca_loadings.R` | PCA_LOADING_INPUT and retained loading output. This legacy script includes a session-dependent coordinate reference; use the archived table for historical values. Loading-table and GAM-score PCA definitions differ. |
| Table S15: spatial-bin sensitivity | `code/10_sensitivity/spatial_diagnostics.R` | Matching model frame, coordinates and m_scale_B_bin_sensitivity.csv: 6/8/10 bins and distance quantiles 0.5/0.6/0.7. |

## Inputs, execution and version notes

Start from saved city tables, model summaries and prediction exports for
downstream figures. [WORKFLOW](WORKFLOW.md) gives the stage order;
[REPRODUCIBILITY](REPRODUCIBILITY.md) describes checks. Renderers read saved
inputs; analysis scripts such as `scale_dependence.R`, `fit_sem.R` and
sensitivity fitting modules execute models. Read each script's usage and
configure its external inputs before running it.

- **Source assets and assembly:** complete global map/classification components
  of Figs. 1-2, satellite-image sources and conceptual artwork remain to be
  supplied. Inventory and aggregation code covers upstream calculations rather
  than those finished panels. Table S13 also needs its retained cross-tab.
- **SEM:** `fit_sem.R` implements five linear equations using piecewiseSEM on
  archived composites. The manuscript describes latent-variable maximum
  likelihood, robust errors and global fit indices. The executed source for
  that description is still needed. City contributions use reported
  unstandardized coefficients separately from refitting the SEM.
- **GAM versions:** the archived 84-city Fig. 4a-c summary supports edf 7.37,
  F 3.49 and P 0.0017. Tables S9-S10 retain a different summary/PC specification;
  keep these versions identifiable pending reconciliation. The 5,621.5 km2
  value is the lower endpoint of a sign-change interval sampled over city-area
  quantiles. Fig. 4d-e uses separate saved fraction/area fits. Its legacy Table
  18 filename is not a current table number; Table S11 has reclaimed-land rows
  and omits the whole-city contrasts.
- **PCA definitions:** loading-table PCA uses mean imputation and includes
  reclamation/aspect in the relevant groups. GAM-score PCA in
  `code/06_city_analysis/prepare_city_table.R` uses complete cases and different
  groups. Preserve the definition belonging to each table.
- **Inference and samples:** regional FDR versus Bonferroni, calibration versus
  validation weights and the city-selection populations remain distinct in the
  available sources. The study frame has 108 cities; the archived scale fit has
  84, later component fits 83, observed-component fit 84, observed-count fit 107
  and reclaimed-response comparisons 70. Use saved support tables for the
  fitting and prediction samples.

Related figure-input documentation:
[response comparisons](reclaimed_land_response_figures.md),
[hierarchical/richness figures](species_richness_figures.md),
[ensemble validation](ensemble_validation.md) and
[city selection](ghsl_sample_selection.md).

Additional formulations are documented in
[REVIEWER_SENSITIVITY](REVIEWER_SENSITIVITY.md). The
[environment-only workflow](../code/10_sensitivity/environment_only/README.md)
retains 814 candidate and 684 paired species separately from the historical
response. Revised boundary calculations, raw-proxy models and spatial
diagnostics retain their own response, sample and configuration labels.
