# Analysis workflow and source selection

“Canonical” denotes the single publication-facing implementation/reference selected for a stage. It does not certify that this exact script generated submitted outputs or that every stage runs from a fresh R session. No alternative was selected merely because it had a newer timestamp.

| Order | Implementation | Inputs → outputs | Execution/evidence boundary |
|---|---|---|---|
| 1 | [Species/regions](../code/01_data_preparation/prepare_species_and_regions.R) | GloNAF/taxonomy, reclamation inventory, boundaries → lists/region joins | Private service username now comes from environment; taxonomic releases/joins require confirmation |
| 2 | [Download](../code/02_occurrence_processing/download_occurrences.R), [cleaning reference](../code/02_occurrence_processing/clean_occurrence_coordinates_reference.R) | Accepted taxa/GBIF downloads → coordinate-cleaned tables | Cleaning reference contains database/workspace and wrong-output-object defects; blocked before repair |
| 3 | [Predictor assembly](../code/03_environmental_predictors/build_environmental_stack.R) | Climate/soil/marine/topographic/human files → full_stack.tif | Explicit naming/order assumptions retained; raster preparation variants remain archived |
| 4 | [Batch SDM](../code/04_sdm/hpc/fit_and_project_batch.R) | Species CSVs/stack → thinning, species-specific selection, PA, fitted objects, CV, selection/fallback and projections | HPC; 29 species per batch in this reference, not a universal historical configuration |
| 5 | [Index reference](../code/05_projection_and_index/build_multispecies_index.R) | Projection summary + model/projection objects → normalized multispecies raster | First-match raster selection and success denominator retained; exact submitted writer unresolved |
| 6 | [City/zone comparison](../code/06_city_extraction/compare_reclaimed_and_other_land.R) | Submitted response + 108 boundaries + reclaimed/other polygons + tables → summaries/plots | External final geometry and GIS-produced tables required; area/extraction methods retained |
| 7 | [PCA](../code/07_pca_sem/pca_and_loading_tables.R), [SEM](../code/07_pca_sem/sem_108_cities.R) | City tables + verified workspace → loadings, SEM fits and diagrams | Workspace-generating steps and final SEM selection unresolved |
| 8 | [GAM reference](../code/08_gam/gam_reclamation_analysis.R) | City/PCA table + saved models → candidate fits/plots/diagnostics | Final formula selection and complete 060503 scale-dependent source missing |
| 9 | [Effect tables](../code/09_figures/city_effect_tables.R), [city figures](../code/09_figures/visualize_city_effects.R) | SEM/effect tables and plotting workspace → effect tables/maps/plots | Fixed historical coefficients preserved; final figure mapping not certified |

Preparation/assembly and fitting are coupled in the original batch implementation. Splitting predictor selection, thinning and PA into invented standalone functions would obscure the actual cache-dependent behavior, so the batch implementation is retained as a unit. Existing projection/formatted-data caches can skip computation; file presence alone is not independent QC.

The current public source header formerly asserted uniform k=3 and minimum 100 records. The manuscript describes k=5 and at least 15 unique records. Saved-object evidence is heterogeneous. We removed the misleading descriptive header from the publication copy, preserved its executable expressions, and retained the entire public-safe old source in archive.

Later global/non-native workflows are [supporting snapshots](../archive/hpc_supporting/); they require their own accepted manifests and country/native mapping intermediates. They do not replace the submitted response. Archive snapshots use symbolic strings and are intentionally non-executable.

The [inventory](../metadata/code_integration_inventory.csv) records all enumerated candidates before promotion; [archive manifest](../metadata/archive_manifest.csv) records sources actually copied. Searches stayed within the authorized project roots, skipped fitted-model/data/cache/log trees for code enumeration, and rejected external symlink targets. The local `flow` and literature trees were excluded as unrelated. No model refit or raster aggregation was run.
