# Manuscript Methods to code mapping

Mappings refer to the current manuscript subsection titles inspected privately. No manuscript/reviewer material is distributed. “Reference” means available implementation, not proof of submitted execution.

| Methods subsection | Publication script | Main input | Main output / evidence gap |
|---|---|---|---|
| Study area | code/01_data_preparation/prepare_species_and_regions.R | Reclamation inventory, GloNAF regions, city boundaries | Regional/city species lists; manual GEE/Landsat vector delineation is external |
| Invasion Pressure Index: taxa/occurrences | code/02_occurrence_processing/download_occurrences.R; clean_occurrence_coordinates_reference.R | Accepted taxa, GBIF downloads | Cleaned records; taxonomy versions, 839→813 ledger and exact cleaning writer unresolved |
| Invasion Pressure Index: environmental predictors | code/03_environmental_predictors/build_environmental_stack.R | External aligned raster layers | full_stack.tif; original resampling scripts archived; exact marine/GDP/VIIRS releases TO VERIFY |
| Invasion Pressure Index: selection, thinning, PA, modeling and evaluation | code/04_sdm/hpc/fit_and_project_batch.R | Occurrences, full_stack.tif, cached formatted/model objects | Selected predictors; models/CV/evaluations/projections; heterogeneous saved objects supersede uniform header claims |
| Invasion Pressure Index: projection and aggregation | code/05_projection_and_index/build_multispecies_index.R | Projection summary and associated rasters/models | normalized.tif reference; exact normalized2.0.tif writer/contributors/native mask unresolved |
| Invasion Pressure Index: city summaries | code/06_city_extraction/compare_reclaimed_and_other_land.R | Response raster, 108 polygons, reclaimed/other zones and GIS tables | City and zone summaries; extraction/GIS table provenance requires confirmation |
| Structural equation modeling | code/07_pca_sem/sem_108_cities.R; pca_and_loading_tables.R | Final city table and workspace | Fits, paths, loadings; manuscript SEM description and code's mixed approaches require reconciliation |
| Nonlinear relationships: generalized additive mixed models | code/08_gam/gam_reclamation_analysis.R | City/PCA table and saved GAM objects | Multiple candidate fits; author must identify final selected model |
| Nonlinear relationships: scale dependence, conditional slopes/peaks, spatial diagnostics | code/08_gam/SCALE_DEPENDENT_SOURCE_REQUIRED.md | Final city table / complete 060503 source | BLOCKER: outputs found, full source not found; no formula reconstructed from plots |
| City-specific net effects / reclamation dependence | code/09_figures/city_effect_tables.R; visualize_city_effects.R | Unstandardized SEM coefficients and city effects | Historical coefficient calculations/figures; final coefficient provenance unresolved |

## Conflicts preserved for author resolution

- Manuscript k=5 versus old public header k=3; historical objects have three/four/five RUN columns. RUN count is not automatically a proof of the intended spatial fold design.
- Manuscript minimum 15 records versus the public reference's filtering and former 100-record header. Do not change the threshold to reconcile text.
- Manuscript 10,000 random PAs versus saved random/disk settings and commonly 1,000 derived PA-only rows. Counts that include presences must not be misreported as PA-only counts.
- Ensemble threshold descriptions are not proof that every accepted/fallback validation score exceeds those thresholds. The Abutilon fitted GLM example has lower held-out TSS/ROC.
- Reclamation and socioeconomic variables are observed fitted inputs for many species. No environment-only reanalysis has been substituted.
- Manuscript native masking versus unresolved 2025 mask provenance. Later masking has a separate product scope.
- SEM methods wording mixes piecewise/latent and global fit-index concepts; the source includes multiple exploratory analyses. Final model correspondence is unproven.
- Effect calculations contain fixed coefficients and overwrite/read dependencies. Their formulas are preserved; author-confirmed final coefficient sources are required.

These are scientific BLOCKERs or WARNINGs, not portability defects. No numerical setting or scientific formula was changed to make the manuscript and code appear consistent.
