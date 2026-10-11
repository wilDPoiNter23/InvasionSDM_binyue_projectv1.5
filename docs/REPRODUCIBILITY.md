# Reproducibility scope

This branch supplies a structured implementation/reference workflow with small provenance metadata. It does not promise exact 2025 model bytes or a clean end-to-end replay of all final manuscript figures.

## Path configuration and inputs

Start at the repository root and copy config/config.example.R to ignored config/config.local.R. Set PROJECT_ROOT and DATA_ROOT, plus aliases for work, shared input, author analysis, database, GloNAF and occurrence preparation. The original subdirectory relationships remain inside each alias to preserve cache/output behavior. MODEL_ROOT and RESULT_ROOT describe work/models and work/results; they are not independent overrides of the historical batch's base_dir. The [unmapped path alias](../metadata/path_aliases.csv) must be configured separately. No directories or packages are created/installed by bootstrap.

Relative input filenames and source setwd calls remain where their original meaning depends on a source tree. Inspect [runtime dependencies](../metadata/runtime_dependencies.csv), which lists string-literal input/output calls and packages. Dynamic filenames and GIS-created tables require manual confirmation. Running --check verifies syntax/config only; it is not input QC. Running a stage may write/overwrite outputs and must use a separate external work directory.

Private GBIF credentials remain in the user's environment. GEONAMES_USERNAME is read from environment. Neither credentials nor a .Renviron file belong in this repository. Package installation, machine library selection and a pasted PowerShell launcher were removed only in publication copies. [Cleanup ledger](../metadata/technical_cleanup_log.csv) identifies these changes. There is no invented dependency lockfile: historical R/biomod2/other package versions must be recovered from author session/provenance records.

## BLOCKERs

1. **Historical response lineage.** `hotspot_mean_suitability_normalized2.0.tif` is MOST LIKELY the submitted response. Audit SHA256: `e4f9011ac720471135e7f73f245558cc62522291ca664700180ebf4310e31680`. Matching bytes do not prove the immutable contributor list, 2025 aggregation writer, per-species raster representation, failure denominator or native mask. The public index reference writes `normalized.tif`, not `normalized2.0.tif`.
2. **Final statistical source.** Local 060503 output files and fragmented analysis notes do not constitute the complete final scale-dependent script. GAM contains multiple candidate formulas and reads saved objects; author must identify final fit and prediction/uncertainty settings.
3. **Workspace dependencies.** PCA first uses `city_df_res`; SEM first requires `cities`, `city_vect`, `full_stack`, `failed_cities`. Guards give an explicit failure instead of silently borrowing arbitrary interactive objects. Later sections also reuse objects; a complete author-confirmed preprocessing/workspace manifest is still needed. A named-list WORKSPACE_RDS can supply verified objects but does not itself establish their scientific provenance.
4. **Occurrence writer.** The historical cleaning reference disconnects an unexplained `base`, writes/appends the same table, later writes `species_geodatapoint` instead of the computed `species_geodatapoint_cleaned`, and queries a disconnected connection. Result-changing repairs were not made. The exact submitted cleaning/export implementation is required.
5. **City/figure provenance.** Final polygons/GIS tables, fixed SEM coefficients and final figure-source correspondences require confirmation. Effect script writes undefined `df_analysis`; figure source calls unqualified `tidy` without explicitly loading its provider. These issues were documented without changing results/formulas.
6. **Licensing and data releases.** A licence file/author confirmation, GBIF download DOI, taxonomic releases and input release/licence documentation are required for a citation-ready release.

## Preserved evidence

The species metadata has 814 unique keys: 737 historical candidates plus 77 later-only entries. It is not the submitted response denominator. Current 813 membership intersects 736 historical keys; historical Diodia teres is absent. Membership tags also have known mismatches. These distinctions remain in product_scope and submitted_membership_status.

Among 737 historical candidate rows, the audit records reclamation in 298, GDP in 724, population in 690, night lights in 168 and built-up in 735 predictor sets. Evidence includes saved parent-input metadata and a direct fitted GLM example; it does not certify every ensemble component's terms. Saved calibration/PA objects are heterogeneous, and some later repaired objects contain an ID predictor anomaly. No values, formulas, thresholds or models were repaired/refitted.

The old public layout, actual local draft sources and later HPC references are in archive with original source hashes. Archived sources use symbolic placeholders and may retain parse/setup defects; they are evidence, not execution entry points. Audit path substitutions retain output hashes and evidence categories. Existing Git history still contains personal paths and a service username; ignoring files does not erase them. No history rewrite was performed.

## Cheap checks versus scientific acceptance

`Rscript examples/validate_structure.R` parses canonical/config R and checks stage entries. `Rscript code/run_stage.R --list` requires no scientific data. --check deliberately stops at missing configuration. Shell syntax can be checked with `bash -n code/04_sdm/slurm/batch.example.sh`. These checks never fit, project or aggregate a model. They do not validate raster alignment, contributor membership, final SEM/GAM selection or scientific conclusions.
