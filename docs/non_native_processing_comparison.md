# Native-domain and prediction-coverage comparison

Run `code/10_sensitivity/environment_only/compare_non_native_processing.R` with one JSON configuration, then run `verify_non_native_processing.R` with the same configuration. Reuse the saved all-city grid support and per-species input caches from the processing comparison. No model fitting or projection is performed. Results cover those city grid cells; this workflow does not write a new global aggregate raster.

Run `test_non_native_processing.R` for synthetic checks that distinguish native exclusions, true zero suitability and missing predictions before interpreting numerical results.

Configuration keys are `output_root`, `cached_comparison`, `paired_manifest`, `native_vectors`, `native_projection_manifest`, `seed` and `bootstrap_replicates`. Supply the existing species native-country vectors, including the separately maintained supplementary-species vector. Store configuration, numerical outputs and logs outside the repository.

For a portable run, `prepare_native_comparison_bundle.py` packages the city-cell caches and exact native geometries without changing their coordinates. Identical WKB geometries are stored once with a separate species-membership table. Verify every transferred file against `transfer_manifest.json`. Set `native_geometry_bundle` with `geometry`, `membership` and `provenance` paths instead of `native_vectors`; set `cached_inputs_only` to true and provide the original raster specification as `grid_geometry`. Cached values retain their source identifiers and value hashes. `export_native_comparison_reference.R` exports independent samples from the source rasters; use `reference_source_values` and `reference_mask_values` for verification on the receiving machine. The optional `pilot_cells` limits a preliminary run only and must be absent from the full comparison.

All inputs are existing continuous predictions on the same grid. The input contract is BIOMOD values from zero to 1000, converted to zero to one. Binary and threshold-filtered paths are rejected. Per-species min-max normalization is not applied. Source-scale metadata and sampled raster checks are retained.

Saved native-country polygons identify native cells using polygon contact with the original grid cells. Native-domain contributions are zero by definition of the non-native index. Finite predictions outside the native domain contribute their suitability. Missing predictions outside the native domain are recorded as unknown, separately from native contributions and valid zero suitability. Taxa without mapped native polygons retain the earlier documented no-exclusion treatment and are explicitly listed; their non-native status is unresolved.

The coverage-adjusted index divides known non-native contributions by native plus predicted non-native species. It therefore estimates a reference-pool mean after excluding genuinely unknown predictions; it is not a fully observed fixed-pool mean. A separate alien-only mean divides by predicted non-native species. The two definitions answer different questions. Fixed-pool lower and upper bounds retain each unknown prediction's possible zero-to-one contribution. Lower bounds must not be described as evidence that missing predictions represent unsuitable habitat. A strict fixed-pool mean is supplied only where every eligible species has a prediction; its lost coverage is reported rather than used to restrict the principal comparison.

The complete FULL roster and the paired FULL/ENV roster are reported separately. Paired results use the same native domains and joint finite predictions. City-zone means use the existing exact area weights. Every computable city is retained for each definition and reclamation footprint; no earlier city-count gate is imposed. Both equal-city and pooled-area comparisons are retained. City bootstrap intervals describe sampling variability, whereas missing-prediction bounds describe uncertainty caused by incomplete prediction coverage.

Run `trace_city_response_processing.R` with an external JSON configuration to trace archived city responses without fitting models. It compares the original rectangle extraction and subsequent boundary updates, polygon-centre and area-weighted means, current prediction coverage, per-species normalization, native contribution removal and denominator changes. Reuse `analysis_support.rds`, `scaled_species_values.rds`, `native_cell_membership.rds` and `non_native_cell_values.rds`. Keep the same declared city roster throughout. The September coverage comparison retains undefined city means explicitly; its available-city summary reports its separate count. Ordered response changes are arithmetic checks, not estimates of which processing step changes a fitted GAM's sign. The historical raster is retained as its own input because current projection files need not reconstruct its original contributors.

`compare_env_fixed_pool_gam.R` compares the 684-species ENV roster under a fixed
denominator of 684, with missing predictions assigned zero contribution and native
contributions either retained or removed. A complete two-by-two comparison also
retains available-species means with and without native masking. All definitions
use identical saved city-polygon cell-area weights, the same 83-city sample, the
original absolute-area G0 formula and the original reclamation prediction range.
Missing-as-zero values are sensitivity definitions, not observations of absence.
Available-species city means omit cells with no eligible finite prediction;
fixed-pool city means retain those cells as numerical zeros. The saved weights
are shared, but their finite spatial denominators can therefore differ. The
tables report the affected area separately.
No SDM is fitted. The current masked available-species GAM is reused when its
response vector matches exactly; the remaining three definitions are fitted once.
Run `verify_env_fixed_pool_gam.R` to independently reconstruct the fixed-pool city
means by species-wise spatial aggregation and check all saved model contrasts.
Keep configuration, prediction caches, fitted objects and results outside the
repository.
