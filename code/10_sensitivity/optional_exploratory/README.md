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

Run `gbif_temporal_prepare.R CONFIG.json`, `gbif_temporal_extract.py CONFIG.json`,
`gbif_temporal_join.R CONFIG.json`, then `verify_gbif_temporal.py CONFIG.json`.
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
