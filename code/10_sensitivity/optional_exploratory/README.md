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

Mining land reclamation denotes post-mining land recovery. It is a separate
disturbance process and must not be used as coastal land-creation timing.

All data, configurations, outputs and logs belong outside this repository.
New modules remain optional until the author selects the publication scope.
