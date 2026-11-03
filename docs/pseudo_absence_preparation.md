# SRE pseudo-absence preparation

Pseudo-absence selection belongs to SDM input preparation, before model fitting.
The maintained scripts are in `code/04_sdm/preparation/`. The workflow order is
occurrence preparation, predictor preparation, pseudo-absence selection,
model fitting and evaluation. Manuscript methods should describe the sampling
used for the reported fitted outputs. The separate SRE pilot does not establish
their sampling provenance.

The fitting entry points retain their historical sampling defaults:
`fit_sdm_batch.R` uses one random set of 1,000 pseudo-absences;
`fit_sdm_five_fold.R` uses one disk set of `min(1000, 2 * n_presence)`
pseudo-absences at distances of 20-50 km. Their existing input reuse and fitting
behavior is unchanged. Do not run either fitting entry point without
`--prepare-pa` when only preparation is intended.

The explicit preparation mode evaluates equal-count pseudo-absences outside a
species-specific surface range envelope (SRE), without fitting models or using
external sampling weights. It does not replace the legacy fitting defaults or
establish an approved production design.

## Preparation settings

- Determine the SRE from stored species-specific predictor names and processed
  occurrence files.
- Retain one presence per raster cell and remove cells with incomplete predictors.
- Use the 2.5% and 97.5% per-predictor quantiles as SRE bounds.
- Draw three pseudo-absence sets, each matching the number of retained presences.
- Use the fixed land-cell candidate sample, excluding presence cells and cells
  with missing predictors. This is not a species-specific accessibility mask.
- Record the configured seed, predictor names, presence counts and settings.
- Mark a species unprepared if the requested count cannot be drawn.

The standalone `format_equal_pa()` helper uses BIOMOD's `PA.strategy = 'sre'`
and `PA.sre.quant = 0.025` interface with three
equal-count sets across a supplied raster domain. It is not called by either
legacy fitting workflow. The pilot runner uses the fixed candidate sample
described below. No prepared design is automatically used for model fitting.

## Preparation without fitting

```sh
Rscript code/04_sdm/fit_sdm_batch.R --prepare-pa CONFIG.json
Rscript code/04_sdm/preparation/verify_pseudo_absence_pilot.R OUTPUT_DIRECTORY
```

The preparation branch exits before model fitting. The existing pilot runner
uses a seeded sample from the frozen 814-taxon inventory, processed occurrence
files and stored species-specific ENV predictor names. Its candidate pool
contains the configured number of native-resolution land cells. This feasibility
pilot does not sample the exhaustive global pool or recover saved historical
fitted presences.
Only `sre_equal` is prepared. Resume and reuse require content identities for the
inventory, raster, occurrence files, alias table and preparation source, plus
matching sampling settings and R/terra versions. Completed receipts also hash
every required saved artifact. Changed inputs, missing or altered artifacts, and historical receipts
without these identities stop. Existing unverified or incompatible directories
require a separate `output_root`; they are not upgraded in place.

Private configuration supplies `project_root`, `inventory`, `occurrence_root`,
`predictor_stack`, `output_root`, `seed`, `pilot_species`, `candidate_cells`,
`minimum_presence_cells`, `replicates: 3`, `sre_quantile: 0.025`, and
`allow_model_fitting: false`. A nonempty legacy `effort_raster` is rejected.
Optional `predictor_aliases` and `reuse_completed_from` support verified input
naming and cache reuse. The predictor stack must be one self-contained GeoTIFF;
external sidecars and backing rasters are rejected because their identity is
not covered by a single-file checksum. Keep private paths outside the repository.

Outputs include the species roster, occurrence-to-cell counts, SRE boundaries,
three separate PA membership columns, sampling diagnostics and a receipt that
records zero SDM fits. The verifier checks saved-artifact hashes, recomputes SRE
bounds from cached presence values, checks complete predictor values and requires
coverage of every configured pilot species before reporting `PASS`. Partial
preparation reports `INCOMPLETE` and exits unsuccessfully. Older receipts without
artifact identities require a new separately authorized preparation run.
`format_pa_for_biomod()` imports an existing design through the public
`user.defined` interface without fitting models.

## Input naming and checks

`code/04_sdm/preparation/verify_soil_predictor_aliases.R CONFIG.json` checks source-file hashes, geometry
and numerical values before creating explicit predictor-name aliases. Aliases
change names in memory, not values. Soil depth follows source preprocessing
provenance, not an inferred filename convention.

```sh
Rscript code/04_sdm/preparation/test_pseudo_absence_design.R --unit-only
Rscript code/04_sdm/preparation/test_pseudo_absence_design.R
```

The first command checks envelope boundaries, sampling counts, presence exclusion,
replicate membership and layer aliases. The second additionally exercises actual
BIOMOD SRE formatting on a small synthetic raster; neither fits an SDM.

SRE defines an environmental exclusion domain. Spatial thinning and occurrence
sampling-bias treatment should be reported separately. The production entry
points retain their existing occurrence preprocessing. The preparation pilot
only deduplicates occupied cells and does not repeat the production thinning.

References:
https://biomodhub.github.io/biomod2/reference/BIOMOD_FormatingData.html
https://doi.org/10.1038/s41559-026-03040-2
