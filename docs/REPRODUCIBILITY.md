# Reproducibility status

Reproducing an analysis requires its source version, external inputs, saved model
metadata and compatible software. This page describes the available checks and
the inputs needed for historical replication.

## Static validation

Current revision code is on `reviewer-sensitivity`. The earlier integration
version remains on `publication-integration`; use commit IDs to identify exact
source versions.

Run from the repository root with Python 3, Rscript and Bash available:

```sh
python scripts/check_repository_layout.py
python tools/validate_publication.py
```

Supply `--rscript /path/to/Rscript` and `--bash /path/to/bash` when these
executables are outside PATH. The validator checks R, Python and Bash syntax,
local Markdown links, `publication_path` aliases and eleven archived source
checksums. It runs without scientific inputs or model execution and fails if a
required checking executable is unavailable. Check JavaScript modules separately
with `node --check`.

## Implementation tests

Small synthetic fixtures check binary thresholds and coverage denominators,
incomplete products and resume protection, observation dates and duplicates,
native exclusions versus genuinely missing predictions, geometric partitions,
whole-city area weights and ratio definitions. The ensemble evaluator provides
`--self-test` for its ranking, training-cutoff and training-only member-selection
helpers. These tests use synthetic records and temporary files. Python
observation fixtures require `duckdb`;
the geospatial R fixtures require the packages declared by their scripts.

The optional workbook exporter additionally needs the separately supplied
`@oai/artifact-tool` JavaScript runtime. Its package and dependency lock are not
distributed here. Saved CSV tables provide a portable source-data route.

The fitted-model entrypoints retain their legacy pseudo-absence settings. Their
explicit `--prepare-pa` mode exits before fitting and is documented in
[pseudo_absence_preparation](pseudo_absence_preparation.md). The original
period/lag/residual figure entry remains the default; saved absolute-area model
figures use its explicit `--absolute-area` mode. New data-dependent verifiers
require the matching saved models, source tables and external configuration.
Use the matching real inputs to check manuscript values after the implementation
tests pass.

Two narrower verification scopes remain: the local-slope verifier checks saved
grid and band consistency without independently rebuilding all support and
simultaneous-band calculations; the response-step verifier does not establish
the archived historical baseline's confidence-interval identity. Keep these
requirements distinct from the saved-model interval reconstruction performed
by `verify_absolute_area_models.R`.

## Source coverage

Eleven paired-sensitivity production files retain the bytes preserved in the
source archive, as checked on 2026-10-13. Their SHA-256 values are recorded in
[archived_source_checksums.csv](../metadata/archived_source_checksums.csv).
They cover input freezing and validation, species fitting, shared-denominator
aggregation, sampled-cell checks, raster/city comparisons, attrition, city
associations and submission templates.

The archive also contains older source variants. Select the version matching
the response definition and species set for the analysis being reproduced.

The previous public layout is pinned at commit
[`141bca2d60ee9bdc6f9572131d07a76debb0858b`](https://github.com/wilDPoiNter23/InvasionSDM_binyue_projectv1.5/tree/141bca2d60ee9bdc6f9572131d07a76debb0858b).
The current layout adds analyses and corrects duplicate occurrence insertion
and cleaned-table writing. These corrections can change regenerated inputs.
Historical replication therefore uses the frozen inputs and their matching
writer scripts from that baseline or the source archive.

[METHODS_CODE_MAP](METHODS_CODE_MAP.md) lists the main analyses and figure components. Coverage has these practical limits:

| Component | Available implementation or supplied input | Remaining requirement |
|---|---|---|
| Occurrence processing | Download and cleaning implementations | The cleaner corrects legacy duplicate database insertion, connection lifetime and cleaned-table writing defects; use archived cleaned inputs for historical replication |
| Species SDM and projections | Three-fold batch and separate five-fold implementation | Choose the saved design/options and package versions for the corresponding fitted objects |
| Historical manuscript aggregate | Aggregation implementation and staged normalized raster | Bind the exact historical writer, successful contributor list and projection versions to the deposited raster |
| PCA, SEM and city effects | Loading/composite equations, SEM fit and coefficient-based effects | Use archived composite/coefficient tables; the precise upstream SEM transformations and standardization are not fully specified |
| GAM and sensitivity components | Model, surface, curve, raw-proxy and spatial scripts | Supply the matching score table and figure-specific summaries |
| Maps and multi-panel figures | Statistical components and listed source data | Final cartography, manual panel arrangement and figure/table version selection remain separate |
| Tables S10 and S13 | GAM smooth summaries and the quartile cross-tab | Use the selected GAM exports and retained cross-tab source table |

## Model definitions

`fit_sem.R` fits five linear equations with piecewiseSEM on archived composites.
The latent-variable maximum-likelihood model described in the manuscript
requires its corresponding executed source. City-effect calculations use the
reported unstandardized coefficients.

The archived Fig. 4a-c GAM summary and Tables S9-S10 use different model
summaries/PC specifications; their selected fitted versions need reconciliation.
Regional comparisons in `regional_effects.R` use Dunn Bonferroni, while the
manuscript's FDR letters require their corresponding comparison table.
The algorithm-weight block in `model_performance.R` uses calibration TSS;
validation plots use the relevant held-out score exports.

Loading-table PCA uses mean imputation and includes reclamation/aspect.
GAM-score PCA uses complete cases and different variable groups. The loading
script also expects an existing coordinate object in its session.

## Open scientific and release requirements

The historical candidate manifest contains 737 rows; `metadata/species_list.csv`
lists 813 taxa from the later preservation pool. The exact contributors to the
historical raster still require a binding to its writer and projection files.
Retain its original denominator. The paired sensitivity has a separate accepted
set of 684 taxa drawn from 814 candidates.

The SEM composite table includes upstream response and covariate transformations
whose full order and settings remain unresolved. Use the archived table to
reproduce the downstream coefficients; rebuilding it requires the original
preprocessing specification.

Release requirements include dataset editions, GBIF download identifiers,
redistribution permissions, a deposit identifier and a compatible R/package
environment. See [DATA](DATA.md) and the [deposit plan](../zenodo/README.md).
The code licence and locked environment are still pending. Static checks cover
source structure and syntax; release preparation also requires real-input
numerical verification and resolution of the listed data and rights requirements.

[ENVIRONMENT](ENVIRONMENT.md) records the paired-release session, observed HPC
packages and local runtime tests. Historical fits and local analyses require
their corresponding environment records.
