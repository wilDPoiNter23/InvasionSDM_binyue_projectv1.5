# Hierarchical-model and recorded-richness figures

`code/09_figures/render_species_richness_panels.R` renders saved contrasts and
prediction grids without fitting models. It exports the five figures described
below. The optional second argument `land_use` exports only `coastal_land_use`.

```sh
Rscript code/09_figures/render_species_richness_panels.R configuration.json
```

The private JSON configuration supplies `output_root` and an `inputs` object
with these keys: `full_contrasts`, `env_contrasts`, `full_predictions`,
`env_predictions`, `full_zones`, `env_zones`, `full_model_input`,
`env_model_input`, `richness_coefficients`, `richness_models`,
`richness_city_data`, `richness_fitted`, `continental_residuals`,
`continental_test` and `observed_sem`.

The `full_model_input` and `env_model_input` values point to saved RDS data frames
with `zone_id`, `city_id`, `landuse` and `area_km2`. All other inputs are CSVs.
The zone tables contain `zone_id`, `city_id`, `landuse` and `mean_suitability`;
zone identifiers must match the corresponding model input. The richness city
and fitted tables retain the common-window city identifiers and saved model
fields used to reconstruct fitted values. Inputs must retain the schemas of the
existing result exports. The output directory must already exist.

The `land_use` selection requires only the eight `full_` and `env_` inputs listed
above. The default invocation additionally requires the seven richness,
continental and pathway inputs. Existing output filenames are reused.

## Figure roles

- `coastal_land_use`: descriptive city-level distributions and saved model
  estimates for all ten land-use categories. Each city mean combines its mapped
  zones using zone area. The model estimates condition on age index 15. Points
  and intervals display absolute predictions for each land use. Asterisks mark
  saved contrasts against transport whose 95% credible intervals exclude zero.
- `hierarchical_formation_age`: saved transport-land predictions across the
  formation-age index, displayed separately.
- `S02_observed_richness_and_geography`: city observations in the common
  2010-2024 recording window and a conditional expected-count curve at the
  median city area. The curve uses the saved coefficients and dispersion and
  reconstructs the saved city-specific fitted values; it is not a refit.
  Pointwise 95% Wald intervals for the conditional mean use the delta method
  and the full covariance matrix reconstructed from the likelihood Hessian at
  saved estimates; reconstructed standard errors must match saved values.
  The displayed nominal two-sided Wald P value uses the saved standard error.
  All four windows and their four-window BH q values remain in the source data.
- `observed_richness_geography`: continental residuals after area adjustment
  and the saved global continental test.
- `observed_richness_pathways`: area-adjusted standardized component
  associations with saved bootstrap intervals.

These figures use the area-only observation model. Complete tables retain the
previously fitted record-count-adjusted sensitivity results. PDF/SVG plots,
PNG previews, source CSVs, legends and checksum records use the existing output
hierarchy. No models are refitted and all nine land-use contrasts are retained.

## Local runtime note

On one Windows R 4.5.1 installation, both svglite and ragg produced valid closed
files but triggered a native-runtime access violation during process shutdown.
The same fault occurred in isolated one-plot device checks. Export completion
is therefore recorded separately from process exit status. Check the PDF/SVG
files and render record independently; a nonzero exit must not be reported as
a successful process run. Use a compatible graphics-package/runtime installation
for unattended reproduction.
