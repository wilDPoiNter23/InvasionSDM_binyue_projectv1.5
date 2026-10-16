# Saved-surface processing comparison

`code/10_sensitivity/environment_only/compare_raster_processing.R` separates
input selection, normalization, missing-value denominators and land footprints
without fitting species distribution models. Run its `prepare`, `extract` and
`summarise` phases in order with one external JSON configuration.

The configuration supplies `output_root`, `cities`, `city_id`, named
`reclamation_sources`, named `surfaces`, `historical_surface_label`,
`paired_manifest`, `original_manifest`, `full_inventory`, `exclude_city_keys`,
`workers`, `bootstrap_replicates` and `seed`. Paths and outputs stay external.
The full inventory uses the existing sensitivity inventory schema. The original
manifest uses the existing frozen original aggregation schema.

All supplied city boundaries are assessed. Reclaimed polygons are united and
clipped to the city in native raster coordinates; other urban land is the city
minus that union. Exact overlap fractions multiplied by geodesic cell areas
weight the means. No outside zone, minimum-cell criterion, cohort overlap gate,
or cross-product finite-cell restriction is applied to the primary comparisons.
Zero geometry, absent model coverage and explicit author exclusions remain in
the coverage tables. Each surface supplies its own finite modeled area.

The retained original species inputs, their continuous counterparts, all FULL
inventory species with selected and continuous input versions, paired FULL and
paired ENV are processed separately. Every
available member is retained and missing input members are listed. A result
with an incomplete input set is not an exact reconstruction of that set.
Binary filenames and metadata ranges are reported. The scale recipe divides
continuous BIOMOD predictions by 1000 and retains binary values on their native
scale. Min-max uses each raster's global metadata extrema, calculating global
extrema if unavailable; constant rasters contribute zero where finite.
All-missing rasters remain in the input roster: they supply no valid cells to
available-species means and zero to fixed-denominator means. Their absent global
extrema are retained in the metadata rather than interpreted as a processing
failure. Per-file errors are recorded and successful extraction caches survive
an interrupted extraction.
Available-species means and fixed-denominator means with missing values replaced
by zero are both computed. A separate reconstruction reproduces the stored
paired products' joint finite-species membership. No native-range mask is
introduced, and stored-surface masking is not inferred from manuscript prose.

Input-cell caches permit restarting extraction. The output includes per-file
metadata and extracted-value hashes, manifest and stored-surface checksums,
cell values and species counts, city coverage, zonal means, and bootstrap
contrasts. Equal-city and pooled-area summaries are separate; intervals resample
whole cities. Direct numerical agreement with the historical surface is required
before attributing its provenance to a reconstruction.

After comparison, `decompose_missing_support.R CONFIG` separates the contribution
of all-missing grid cells from the denominator used where some species have
predictions. It reports each recipe's available city sample and an additional
comparison on all cities computable under both recipes. The latter isolates
processing steps; it never selects a historical fixed city list.
