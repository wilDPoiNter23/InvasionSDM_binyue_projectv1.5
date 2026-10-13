# Reviewer-informed downstream sensitivities

These analyses reuse accepted suitability products and existing city covariates.
They do not fit species distribution models or alter the historical response.
Store all results, models, figures, logs and written findings in one external,
article-specific evidence directory. Keep results outside version control.

## Modules

| Module | Entry point | Inputs and scope |
| --- | --- | --- |
| Raster and city comparisons | `code/10_sensitivity/environment_only/reviewer_paired_analysis.R` | Staged products, final city polygons and external module output directory |
| Historical raster comparison | `code/10_sensitivity/environment_only/compare_historical_response.R` | Historical and paired products; retains distinct response scales |
| Saved-product verification | `code/10_sensitivity/environment_only/verify_review_paired.R` | Existing results and frozen inputs; no model fitting |
| City/background contrasts | `code/10_sensitivity/nested_spatial_contrasts/reviewer_city_background.R` | Final city boundaries, 50 km buffers and surrounding rings |
| GAM formulations | `code/10_sensitivity/gam_robustness/run_city_robustness.R` | Canonical city covariates, matched paired city extractions and explicit bootstrap settings |
| Geographic influence | `code/10_sensitivity/sampling_selection/geographic_influence.R` | Called by the GAM entry point; preserves a common complete-city sample |
| Spatial residual diagnostics | `code/10_sensitivity/spatial_diagnostics/city_residual_moran.R` | Called with saved model residuals and coordinates |
| Optional source checks | `code/10_sensitivity/optional_exploratory/review_source_provenance.R` | Private configuration declaring existing composite and age candidates |
| Run control | `code/10_sensitivity/run_revision_sensitivity_analyses.R` | Private JSON configuration; records input/output checksums and code revision |

Use each entry point's argument contract. For example, from the repository root:

```sh
Rscript --vanilla code/10_sensitivity/environment_only/reviewer_paired_analysis.R "<STAGED_RELEASE>" "<CITY_POLYGONS>" "<EVIDENCE_ROOT>/01_environment_only"
Rscript --vanilla code/10_sensitivity/gam_robustness/run_city_robustness.R --input "<GAM_INPUT>" --paired "<CITY_RESPONSE_TABLE>" --output-root "<EVIDENCE_ROOT>" --bootstrap 30
Rscript --vanilla code/10_sensitivity/run_revision_sensitivity_analyses.R "<PRIVATE_CONFIGURATION>" --record-only
python scripts/check_repository_layout.py
```

The run-control configuration declares repository, branch, output_root, input
IDs and roles, source paths, packages, seeds, unresolved blockers and stages.
Each stage declares name, script, arguments, output_directory, input_ids and
completed_marker. Explicit ordered artifact_owners specify each output family's
pattern, generating script and input IDs, so saved fits are not attributed to a
later plotting or verification command. It can execute stages in separate clean R sessions or record
already completed stages. Completed numerical outputs must be inspected and
preserved before any further execution.
Retained intermediate exports and superseded geographic summaries are explicitly
marked in the output index; they must not be used in place of current verified tables.

## Scientific definitions

HISTORICAL_MAIN, PAIRED_FULL_684 and PAIRED_ENV_684 remain distinct. FULL versus
ENV uses the same accepted taxa and finite paired-cell denominator. Neither
missing predictions nor failed species become zero. The historical raster uses
different aggregation and species membership, so historical-versus-paired
differences cannot be attributed solely to predictor removal.

G0 preserves the reported tensor-product formula and REML fitting method. G1
uses separate smooths of log1p(reclaimed area) and log1p(city area). G2 uses a
reclamation fraction and log1p(city area). G3 uses their tensor product subject
to explicit support checks. All retain the anthropogenic, climate, soil and
marine PC1 covariates. Reclaimed area is supplied in hectares; the new fraction
converts hectares to km2 before division by city area. This conversion is explicit
and does not silently redefine the reported G0 model or its archived inputs.

The sign scan compares predicted values at the high and low ends of the
reclamation range. It is an endpoint contrast, not a local derivative or a fitted
ecological breakpoint. Whole-range scans may extrapolate; local-support scans
and bootstrap results assess sensitivity without establishing causality.

## Evidence boundaries

Boundary-updated city/background results are separate from exact historical
geometry reproduction. Within-city reclaimed/non-reclaimed and harmonised
three-zone contrasts require a verified full zone geometry version. An incomplete
boundary patch cannot establish that version by itself.

Model-performance exports must identify the actual selected model or ensemble
and keep calibration, validation and external evaluation partitions separate.
Do not compare a historical species summary with a later clean fit as though
they were the same accepted projection. Good discrimination does not demonstrate
absence of occurrence-sampling bias.

The 135-city selection comparison requires a verified city-identity crosswalk
and measured excluded-city covariates. Do not invent aliases or sampling effort.
SEM reproduction requires an unambiguous original composite table and formula.
Period-like filenames alone do not verify reclamation age. Stop only the affected
analysis and retain its blocker in the evidence package.

Saved figures must retain their numeric source tables. Technical checks,
numerical reproduction and scientific interpretation are reported separately.
The evidence package provides manuscript and reviewer-response drafts; it does
not edit submitted or working manuscript documents.
