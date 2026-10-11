# Coastal reclamation and invasive-plant suitability

This repository organizes the available code for global species distribution models and the downstream analysis of 108 coastal cities. It contains R implementations, a Slurm template, input documentation and public-safe provenance metadata. Large datasets, fitted models and manuscript files are external.

**Status:** publication integration candidate. The code structure and syntax can be checked; exact historical 2025 model bytes and all final statistical outputs cannot yet be reproduced. See [validation](docs/PUBLICATION_REPOSITORY_VALIDATION.md) and [remaining blockers](docs/REPRODUCIBILITY.md).

01 occurrence preparation → 02 predictors → 03 SDM → 04 projection/index → 05 city extraction → 06 PCA/SEM → 07 GAM → 08 figures

The expensive fitting/projection stages belong on HPC. The downstream stages use author-supplied rasters, polygons and city tables and ordinarily run locally. The numbered `code/` folders follow the underlying data flow; [workflow](docs/ANALYSIS_WORKFLOW.md), [Methods mapping](docs/METHODS_MAPPING.md) and [stage catalog](metadata/workflow_stages.csv) identify the implementation for each stage.

## Start here

1. Read [DATA_SOURCES.md](DATA_SOURCES.md) and obtain the external inputs. Releases/licences marked TO VERIFY are not resolved by this repository.
2. Copy [config/config.example.R](config/config.example.R) to `config/config.local.R`; set the external path aliases. Do not put credentials or data in Git.
3. From the repository root run `Rscript code/run_stage.R --list`. Use `--check <catalogued-script>` for a parse/configuration check. This does not verify external inputs or reproduce results.
4. Review each script's input/output header and its [runtime dependency ledger](metadata/runtime_dependencies.csv). Unresolved statistical stages are implementation references, not unattended entry points.
5. For an author-confirmed SDM rerun, follow [HPC instructions](code/04_sdm/hpc/README.md). No jobs are submitted by setup or validation.

Configuration contains paths only. Numerical thresholds, PA/CV choices and formulas remain in the source implementations; saved historical objects show heterogeneous realized settings. Reclamation, GDP, population, built-up and night lights occur in species-specific models. These facts must not be replaced with a uniform environmental-only model description.

The submitted downstream response is MOST LIKELY `hotspot_mean_suitability_normalized2.0.tif`. The index implementation retained here writes a differently named file and is not proven to be its exact writer. Later 813-taxon products and non-native masks are kept separately in [archive](archive/README.md).

[Predictor dictionary](metadata/sdm_predictor_dictionary.csv), [species model summary](metadata/species_model_summary.csv), [source inventory](metadata/code_integration_inventory.csv), and [audit](docs/audits/hpc_accepted_run_audit.md) preserve the evidence boundaries. The 814 metadata rows are a union of historical candidates and later-only species, not an asserted submitted denominator.

## Citation and reuse

The authoritative repository is [InvasionSDM_binyue_projectv1.5](https://github.com/wilDPoiNter23/InvasionSDM_binyue_projectv1.5). Cite an author-approved release/commit when available; no archived release DOI is supplied. [CITATION.cff](CITATION.cff) records software authorship. The previous MIT declaration has no accompanying licence file: author confirmation of licensing is required before a public release is called ready for reuse.
