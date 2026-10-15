# Optional input and cohort checks

These modules keep input feasibility separate from scientific interpretation.
They do not alter existing SDMs, response rasters or archived model definitions.

## Coastal reclamation timing

1. `inspect_timing_inputs.R CONFIG.json` inventories vector fields, source checksums
   and spatial extent. A private configuration supplies `layers` and `output_root`.
2. `coastal_timing_screen.R CONFIG.json` intersects reported change-year and land-use
   categories with the declared 108-city geometry, then extracts continuous FULL
   and ENV suitability at their native 2.5-arcminute grid using exact cell fractions.
   Supply `cities`, `city_id`, `layers`, `responses` and `screen_output`.
3. `coastal_period_association.R CONFIG.json` compares reported five-year cohorts
   within cities using area weights normalized within each city. City-bootstrap
   intervals preserve the city as the resampling unit. Supply `screen_output`,
   `association_output`, verified source identity, `period_source_verified`,
   `seed` and `bootstrap_replicates`.

Use Global Coastal Reclamation version 4, DOI 10.6084/m9.figshare.25556901.v4,
only after matching the downloaded archive checksums to the source record.
Reported endpoint labels 1990 through 2020 describe five-year change cohorts,
not precise completion dates. Retain this temporal uncertainty.

The spatial screen reports the number of native cells, summed cell fractions
and effective cells. The supported subset requires at least three native cells
and two effective cells per city-cohort. Association intervals require at least
20 cities. These thresholds are screening rules, not proof of independence.
Coastal polygons can share a native suitability cell. Static suitability is not
longitudinal invasion, and these comparisons do not identify colonisation lag.
Land-use categories are descriptive unless jointly controlled in a declared model.

The original spatial screen must retain its category-overlap checks. When those
checks fail, a separate cohort analysis may set `strict_geometry` to true and
use a new output directory. This excludes complete cities whose change-year
category excess exceeds the larger of 0.001 square kilometres and 0.1% of mapped
area. The excluded-city table remains part of that analysis. No year is assigned
arbitrarily to overlapping land. `verify_coastal_period.py --cohort-only` checks
this filtered cohort scope; it does not clear the original spatial package or
the land-use classifications. Preserve the original failed verification.

Mining land reclamation denotes post-mining land recovery. It is a separate
disturbance process and must not be used as coastal land-creation timing.

All data, configurations, outputs and logs belong outside this repository.
New modules remain optional until the author selects the publication scope.

## Mining reclamation

`mining_reclamation_screen.R CONFIG.json` assigns source mining centroids to
unchanged FULL and ENV native suitability cells. It aggregates reported damage,
reclamation and footprint areas before fitting exploratory landscape models.
Shared-cell mines contribute a single suitability response. A compact-mine
subset retains mines whose footprint is at most half the approximate cell area.
This is a centroid screen, not exact extraction from reclaimed patches.

Supply `mining_csv`, `responses` with FULL and ENV keys, `output_root`,
`maximum_mine_cell_area_ratio`, `minimum_cells_per_country` and
`spatial_block_degrees`. Amount models adjust for damage, footprint, country
fixed effects and spherical location trends. Timing models also include the
reclamation-area-weighted period endpoint. Five- and ten-degree spatial-block
cluster intervals describe uncertainty under two block definitions.
An endpoint is not a mine's exact completion date. Cumulative transitions are
not a bounded percentage of unique land restored. Modelled suitability does not
measure observed colonization or a change caused by reclamation.

Run `verify_mining_screen.py CONFIG.json` after the R module. It independently
reconstructs period totals, geographic cell indexing, grouped values and adjusted
least-squares point estimates. It does not validate ecological causation or
reconstruct the cluster intervals. Preserve failed checks and earlier runs.

To correct rounded metadata without recalculating valid tables, set
`metadata_only` to true in a separate private configuration. The R module writes
`grid_metadata_precise.json` with full numeric precision. A subsequent verifier
configuration may declare `grid_metadata` and a new `verification_filename`.
The verifier binds precise metadata to the original raster hashes and checks
unchanged extent and dimensions. Earlier metadata and checks remain available.

## Joint coastal comparisons and occurrence records

Run `coastal_joint_extract.R CONFIG.json`, then
`coastal_joint_association.R CONFIG.json`. Cached city geometries are intersected
by cohort and land use. Whole cities failing either source-category or joint
category overlap are excluded. Within-city estimates condition on land use;
matched use contrasts condition on city and cohort. Between-city models report
separate sample-size and rank gates. Run `verify_coastal_joint.R CONFIG.json`
afterwards to reconstruct native weighted means and within-city point estimates.
`verify_coastal_joint_statistics.py CONFIG.json` independently reconstructs
matched use support, use contrasts and between-city OLS point estimates.
These checks do not independently reproduce bootstrap intervals.

Run `gbif_temporal_prepare.R CONFIG.json`, `gbif_temporal_extract.py CONFIG.json`,
`gbif_temporal_join.R CONFIG.json`, then `verify_gbif_temporal.py OUTPUT_DIRECTORY RECEIPT.json` and
`verify_gbif_spatial.R CONFIG.json`.
The database is opened read-only and checked against its declared checksum.
Outputs describe first recorded target taxa, including pre-cohort and uncertain
records. Local alien status, establishment and arrival dates remain unresolved.
Mining footprints have no assigned reclamation completion date.

## Mining polygon support

Run `mining_polygon_support.R CONFIG.json`,
`verify_mining_polygon_support.py CONFIG.json`, then
`verify_mining_cluster_intervals.py CONFIG.json`. Native-cell fractions allocate
source totals across entire mine footprints; they do not identify reclaimed
patches. Shared response cells are collapsed before fitting. A separate
large-polygon subset requires at least three cells and two effective cells.
Chunk checkpoints allow continuation without replacing completed extraction.
The interval verifier also applies to the earlier centroid screen.

For a separate fraction-method sensitivity, supply `fraction_source_output`,
a new `output_root`, `extraction_configuration` and
`extraction_checkpoint_directory` in addition to the mining inputs. Run
`mining_fraction_sensitivity.py CONFIG.json` to reconstruct all saved
polygon-cell fractions using planar rectangle clipping.
`verify_mining_fraction_sf.R CONFIG.json` checks deterministic samples and
the largest method differences against GEOS, and reads response values afresh.
Then run `mining_polygon_association.R CONFIG.json` and both mining Python
verifiers. The association continuation validates the archived extraction
lineage and binds the reconstructed tables to their checksums. Preserve the
source extraction, its earlier checks and the separate method comparison.
Fresh full runs use `mining_polygon_support.R`; the continuation module is
for the archived extraction lineage declared in the configuration.

## Recorded invasive-species richness

Run `observed_richness_prepare.R`, `observed_richness_extract.py`,
`observed_richness_join.R`, `verify_observed_richness.R`,
`observed_richness_association.R`, and
`verify_observed_richness_association.R` with a private configuration.
These modules use full-city polygons, explicit species rosters and date windows.
Unrecorded cities have unknown observation support. Target-record intensity
is an endogenous recording proxy. Positive-count models condition on cities
with records and do not correct informative sampling selection.

Run `coastal_observed_association.R CONFIG.json`, then
`verify_coastal_observed.R CONFIG.json` to reuse dated coastal memberships.
Common windows occur after all eligible cohort endpoints. Within-city age
associations require declared minimum support and matched native responses.
Outputs concern recorded richness, rather than arrival times or true absence.
Land use is not adjusted in this module. The verifier recounts memberships and
compares fitted age slopes against a separate city-indicator parameterisation.

## Threshold-defined suitable area

`binary_prepare.py` separates historical main candidates and accepted paired
sources. `binary_calibration_extract.R` obtains scores directly from each stored
continuous product at retained presence and pseudoabsence coordinates. The
10th-percentile presence cutoff and maximum-TSS cutoff are calibration
sensitivities; they are not independent held-out validation. Never average
member thresholds or threshold an across-species mean map.

`binary_convert.R` creates native-grid 0/1 maps while preserving missing cells;
`binary_verify_thresholds.py` and `binary_verify_rasters.R` check cutoffs and maps.
`binary_city_extract.R` freezes common FULL/ENV finite cells per paired species
and calculates native-cell area-weighted responses. `binary_city_summarize.py`
requires the declared 684-species denominator. Sum of species suitable areas
is not the unique area suitable for at least one species.

`binary_city_association.R CONFIG.json` and
`verify_binary_city_association.R CONFIG.json` compare matched threshold
responses using the existing city predictor parameterisations. Area responses
include city-area adjustment. These are index sensitivities: binary conversion
does not remove human predictors already used to construct full-model outputs,
and cannot independently establish a causal structural-equation model.

`binary_city_prepare.R` caches native support once. Verify it with
`verify_binary_city_support.R`; out-of-range fractions must remain unaccepted.
For a bounded numerical precision discrepancy, use
`binary_city_support_precision.R` to recompute affected fractions with GEOS
into a fresh support directory, then verify that separate product.

`paired_response_sem.R CONFIG.json` and `verify_paired_response_sem.R CONFIG.json`
compare observed and threshold-defined responses in an adapted component
system. Predictor-only PCs are reconstructed before any outcome analysis;
historical response-supervised composite weights are excluded. City area is
included in every component equation and all directed-path products are retained.
City-bootstrap intervals describe exploratory associations. This is an alternative
specification, rather than reproduction or causal validation of the original SEM.

`binary_city_core.py ROOT PAIRED_MANIFEST FRESH_OUTPUT` offers a separately
declared coverage sensitivity when the complete source roster lacks common
predictions across cities. It freezes species with positive paired finite
coverage in every city before reading suitability outcomes. Missing reclaimed
land responses remain unknown. Declare `expected_species_count` consistently
in subsequent city-association and component-system configurations. A smaller
core changes the taxonomic estimand and does not replace the full-roster result.

`shap_pilot.R CONFIG.json` explains stored continuous predictions on a frozen
sample of current city cells. It first checks the saved prediction route against
the raster, then estimates permutation Shapley contributions using a declared
empirical background. Model fitting is excluded. Contributions describe the
specified fitted model and reference distribution.

`residual_city_models.R CONFIG.json` and its verifier compare fresh area/record
residual responses, partial-regression components, and verified continent groups.
Correction baselines exclude reclamation. Earlier count-model residuals already
include reclamation and are retained as diagnostic provenance only.

`temporal_record_lag_summary.py CONFIG.json` summarises interval-valued recording
delays from stored species/cohort first records, including city-balanced and
observation-opportunity summaries. These are recorded delays rather than
colonisation times; unrecorded cohorts remain unknown.
# Reclamation context and manuscript figures

`reclamation_human_associations.R` evaluates four prespecified raw human-activity
indicators using rank correlations, area-controlled log-area associations and
reclamation-fraction sensitivity. Its companion verifier reconstructs ranks,
coefficients, robust intervals and multiplicity adjustment. Outputs are
associations and do not establish causal mediation or habitat fragmentation.

`../../09_figures/render_reclamation_evidence.R` renders verified period,
first-record-delay and observed-richness component results in R. Both prediction
families and the record-intensity sensitivity are retained. Supply a private JSON
configuration with external input roots and an external output root. Figure
bundles include editable vector exports, raster previews, source tables and
captions. No species models are trained by these modules.

`reclamation_configuration.R` describes connected mapped reclamation components
after dissolving period boundaries. Its 1, 10 and 100 ha cutoffs are analyst
sensitivities, not a claimed source mapping unit. The companion verifier checks
polygon validity, interior overlap, numerical area conservation and component
classification after translation to a local origin. These metrics do not define
vegetation habitat fragmentation.

For a constant community in three land zones, the adjacent
`../nested_spatial_contrasts/common_land_baseline_fixed_extract.R` reads saved
continuous predictions for the frozen 261-species coverage roster at verified
native cells. Retained cells require every species to have paired finite values;
the same cell areas weight both prediction families. Shared statistics use the
explicit `FIXED_261` support policy. This is extraction from existing projections,
not species-model fitting or a global raster rebuild.

`../../09_figures/render_reclamation_context.R` draws the matched-zone contrasts,
human-activity associations, binary-fraction sensitivity and mapped configuration.
The R renderers require Ghostscript for font embedding if the R installation lacks
a Cairo PDF device; vector text remains editable. A fontconfig setup must be
available on the execution node. Runtime assets and result files remain external.


The shared-zone renderer requires at least 20 matched cities for a primary
comparison. An explicit `allow_small_sample_exploratory` setting can display a
smaller sample, labelled exploratory; it does not change the support criterion
or the primary coverage guard.

`../nested_spatial_contrasts/common_land_response_identity.R CONFIG.json`
compares a historical manuscript response with fixed-community FULL/ENV values
on the same new reclamation geometry and jointly finite cells. Supply verified
baseline/fixed-community roots, the original response and expected checksum, a
native grid template, fresh output root, and declared 0/1/5-cell support levels.
The zero-cell threshold retains any positive weighted support and is explicitly
exploratory. Run `common_land_response_identity_verify.R CONFIG.json RECEIPT.json`
afterwards; it independently reads native values, reconstructs all weighted
zone/whole-city means, eligibility and bootstrap intervals. This diagnostic does
not reproduce historical reclamation zones or establish the original species
roster and normalization. Differently normalized response levels need not be
comparable across model families.

`../nested_spatial_contrasts/common_land_full_surface.R CONFIG.json` performs
a separate descriptive three-zone comparison directly within each supplied
stored full-model surface. No fixed-species or cross-model finite-cell
intersection is added. Positive finite mapped support defines its descriptive
sample; 1/5-native-cell restrictions remain separate sensitivities. Original
and paired surfaces retain separate identities and eligibility sets.

`temporal_order_support.R CONFIG.json` checks dated city memberships, constructs
city-year recording support and first-record windows, and lists predictor
completeness exclusions. It is a feasibility inventory, not a fitted lag model
or a recalibration of the species distribution models.

`reclamation_record_lead_lag.R CONFIG.json` uses a fixed 813-taxon roster to
relate mapped reclamation in one interval to first city records in a later
window. Supply verified geometry eligibility, cached cohort geometries, the
record-support root, roster and a fresh output root. `minimum_outcome_start`
can align the focal analysis with the declared exposure period. City and period
effects, historical recording support, duration offsets, record-intensity
sensitivity and all declared lead/history variants are retained. Unrecorded
periods are excluded rather than treated as confirmed non-invasion.

`verify_reclamation_record_lead_lag.R CONFIG.json` independently recounts first
records, checks temporal order, refits the Poisson mean and reconstructs the
city-cluster covariance. The response is first recorded occurrence, not verified
arrival. Adjacent later exposure cohorts share a calendar boundary with outcome
windows because dates are five-year intervals; their lead term is a forward-bin
check, not an exact event-time negative control. Neither model refits SDMs or
recalibrates the original index.

Optional `exclude_city_keys` settings retain the original inputs while excluding
declared source keys from new surface, binary and temporal summaries. The
archived108/84-city numerical reproduction is not overwritten.

`../gam_robustness/city_exclusion_sensitivity.R CONFIG.json` refits the supplied
four formulas on the declared reduced city sample. Its common-absolute-IQR
profiles compare the same absolute change across parameterisations at each city
size, using local20-city area neighbours. This is not a geometry-preserving
null calibration or a fitted ecological breakpoint.

`../sampling_selection/original_pool_inventory.R CONFIG.json` inventories source
records, exact/candidate links, numeric-data availability and continent counts.
Matching coordinates and reclamation values can support source-record links,
but do not establish equivalent administrative polygons. Missing excluded-city
covariates and qualitative reclamation amounts are not replaced with zeros.

`../../09_figures/render_land_reference.R CONFIG.json` presents the original
index on one outside-city reference. Re-expression alone is algebraic; empirical
robustness comes from the declared matched sample, zones and extraction rules.
