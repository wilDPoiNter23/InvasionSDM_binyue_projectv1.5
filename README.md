# Coastal reclamation and invasive-plant suitability

This publication branch contains full recovered historical source for reviewer inspection, configured public copies, original-source snapshots and provenance. **Source coverage is substantially complete; full result reproduction is not yet established.**

[Code completeness audit](docs/CODE_COMPLETENESS_AUDIT.md) · [Manuscript coverage matrix](metadata/manuscript_code_coverage.csv) · [Original source hashes](metadata/source_recovery_manifest.csv) · [Scientific expression validation](metadata/original_scientific_comparison.csv)

## Inspect the real implementations

| Analysis | Complete real source | Version boundary |
|---|---|---|
| SDM, variable selection and projection | [0401 batch](code/04_sdm/historical/real_sources/0401batch_modeling_config.r), [040101](code/04_sdm/historical/real_sources/040101batch2.0.r), [040102](code/04_sdm/historical/real_sources/040102restbatch.r), [0402](code/04_sdm/historical/real_sources/0402simple_batch.r) | Full historical variants; exact accepted execution version heterogeneous/unresolved |
| Multispecies index | [0405Allstack](code/05_projection_and_index/real_sources/0405Allstack.r), [research index](code/05_projection_and_index/real_sources/040501REsearch.r) | Exact submitted normalized2.0 writer remains unresolved |
| City/zone contrasts | [0603](code/06_city_extraction/real_sources/0603reclaim_analysis.r), [buffers](code/06_city_extraction/real_sources/0407buffer_city_compare.r) | Original and boundary-recheck variants kept separately |
| Manuscript PCA loading table | [060501](code/07_pca_sem/real_sources/060501final_pc_table.r) | Includes reclamation/aspect; mean imputation |
| Final recheck PCA scores and GAM dispatch | [010303](code/08_gam/real_sources/010303_rerun_gam_after_boundary_update.R) | Excludes reclamation/aspect from those PCA groups; complete cases |
| Reported scale-dependent GAM | [060503 full source](code/08_gam/real_sources/060503scale_dependentEffect.r) | Actual raw-area tensor includes marine; historical header conflicts with expressions |
| Matching SEM structure | [Piecewise original](code/07_pca_sem/real_sources/piecewise_sem_historical.R), [preprocessing original](archive/original_sources/local/sem_preprocessing_damaged.R.txt) | Original fitting input interactive; preprocessing source is damaged |
| SEM coefficients/city effects | [060502](code/09_figures/real_sources/060502indirect_re_caculated.r), [city plots](code/09_figures/real_sources/0604plot_cities_detial.r), [regional tables](code/09_figures/real_sources/060401cities_details_analysis.r) | Actual hard-coded coefficients preserved; fit object unresolved |
| Evaluation, correlation and response figures | [Full source catalog](metadata/workflow_stages.csv) | Identified statistical/component code present; some manual assemblies unresolved |

## Configuration and inspection

Copy `config/config.example.R` to ignored `config/config.local.R` and configure external paths. Preserve historical relative subtree layouts. `RECOVERED_CODE_SOURCE_ROOT` points to the recovered GAM dispatcher dependencies; `RECOVERED_CODE_ROOT` points to an external historical working/data directory. These are intentionally separate. Historical working-directory calls use configured paths; interactive selections and workspace dependencies remain visible rather than replaced with invented steps.

Run `Rscript scripts/verify_source_recovery.R .` for parse/expression checks only. `Rscript code/run_stage.R --list` lists actual sources; `--check` checks syntax/configuration. Whole-file execution is gated until historical inputs, workspace state and conflicting versions are resolved. No setup or validation command fits a model, submits jobs or downloads data.

Each source records original SHA-256 and publication modifications. [Full original snapshots](archive/original_sources/README.md) retain diagnostics/calculations. Seven damaged originals are full `.R.txt` inspection snapshots; they are not passing entrypoints. [Previous integration references](archive/prior_integration_references/README.md) are outside the main workflow. The missing scale-dependent source note is superseded.

## Limits before a reproducible final release

PCA loading and GAM-score definitions differ. SEM original fit input is unknown. Main Fig. 4 embeds an older image while statistics use recheck. ED Table 3 has no matching identified model. Supplementary Table 3 mixes versions; spatial outputs retain old n=83 results. **5,621.5 km² is a rounded lower endpoint of a P85–P90 sign-change bracket, not a fitted exact breakpoint.** See the coverage matrix for all 28 outputs and 15 stages.

Large rasters, raw occurrences, fitted models and manuscript documents are external. Later 813-taxon, non-native and environment-only sensitivity implementations are supporting/historical archives and are not substituted for submitted analyses. No HPC files were deleted.

## Citation and reuse

Cite an author-approved commit/release when available. See [CITATION.cff](CITATION.cff) and [DATA_SOURCES.md](DATA_SOURCES.md). Licensing/dataset-release metadata still require author confirmation; no archival DOI is asserted.
