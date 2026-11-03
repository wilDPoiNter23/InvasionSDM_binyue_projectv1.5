# Reclaimed-land response comparisons

The analysis compares reclamation extent with three responses measured inside
mapped reclaimed land: continuous mean suitability, threshold-defined suitable
fraction, and effective suitable area. The latter integrates the cellwise
fraction of eligible species above their existing threshold; it is not a union
of suitable habitat across species.

Continuous responses use the existing historical reclamation footprint, common
0-1 scaling, native exclusion and available non-native prediction denominator.
Missing predictions remain unknown. All cities with a continuous response and
the existing complete predictor set are retained. City inclusion is recorded.

Only the continuous-response GAMs are new. Their tensor smooth and four
covariate smooths follow the saved suitable-fraction models. Saved fraction
and area models are read without refitting; area models retain their existing
additive city-area adjustment. Neither SDMs nor thresholds are recalculated.
The same 70 cities contribute to all three responses.

The figure contract is a quantitative grid with three semantic columns:
mean suitability, suitable fraction and effective suitable area. Model families
occupy separate rows. FULL with 814 species and ENV with 684 species are shown
explicitly, with a separate 684-species paired comparison. T10 is the first
displayed threshold; MAX_TSS is reported with equal completeness in the
threshold companion. The design tests whether local suitability and absolute
habitat amount show the same association with reclamation extent.

Curves average model predictions over retained cities at each reclamation
extent, keeping each city's other covariates unchanged. The horizontal axis
uses square kilometres and a logarithmic display. The main display highlights
the existing interquartile comparison, with the complete observed marginal
range supplied separately. All cities remain in estimation and prediction
standardization. This standardization does not imply that every
covariate combination is jointly represented in the data.
Pointwise 95% intervals use the stored coefficient covariance. Area curves
back-transform average fitted log1p area and are not arithmetic mean areas.
The interquartile comparison reproduces existing binary contrasts. Negative
back-transformed area predictions at the low boundary of the saved Gaussian
models are retained in the complete-range companion and identified in its
caption; the interquartile area comparison lies above zero.

Figures use R, Arial text, restrained blue and orange, editable PDF and SVG,
and R-rendered PNG previews. Outputs and source data are configured outside
the repository. Input hashes, city support, model summaries and numerical
agreement with saved results accompany the figures.

## Reclamation evidence entry points

The legacy `render_reclamation_evidence.R CONFIG.json` entry point retains its
period, first-record-delay and observed-richness figures. Its configuration uses
`output_root`, `period_root`, `lag_root` and `residual_root`.

The separate saved-fit absolute-area and response-processing figures use
`render_reclamation_area_sensitivity.R CONFIG.json`, or the equivalent
`render_reclamation_evidence.R CONFIG.json --absolute-area`. This configuration
uses `output_root`, `reference_city_input`, `excluded_city_keys`, `model_root`,
`absolute_comparison` and `processing_comparison`; the output root must exist.
These modes read different saved inputs and do not refit models.

## Whole-city and reclaimed-land comparison

`code/09_figures/render_city_reclaimed_habitat_comparison.R` reads the saved
whole-city and mapped-reclamation fraction and area models. It takes the model
directory and existing figure directory as two command-line arguments. No model
or threshold is fitted again, and the original model samples are retained.

The two-panel figure uses the primary FULL predictions and T10 threshold.
Panel d overlays whole-city and reclaimed-land suitable fractions on one set of
axes. Panel e overlays their effective suitable areas on a broken linear axis,
with retained segments of 0-15 and 380-550 square kilometres. The omitted interval
is labelled and contains no fitted estimates or confidence intervals. Purple and
green solid lines distinguish whole-city and reclaimed-land summaries, with
circle and diamond endpoint symbols. Whole-city summaries include
reclaimed land and do not represent a non-reclaimed control.

Both regions are standardized over the same available city covariate profiles.
The horizontal display uses the existing reclaimed-land interquartile comparison
for both regions, rather than selecting a range according to fitted sign.
Each profile retains its observed city size and background covariates while
reclamation extent varies. Fraction models retain their tensor terms, and area
models retain their additive smooths and log1p response transformation.

Pointwise intervals and endpoint Wald contrasts use the saved coefficient
covariance. Area levels are expm1 of average fitted log1p area, without smearing;
area contrasts and their intervals remain on the log1p scale. Plot annotations
refer to endpoint contrasts, not to the overall smooth tests. The script also
exports both thresholds and full-range predictions as source data, preserving
negative boundary predictions without clipping or replacement. Separate regional
curves are not a formal test of differences between regional associations.

The exports use the `city_reclaimed_habitat_comparison` stem and include editable
PDF and SVG, an R-rendered preview, prediction profiles, contrasts, model
specifications, full-range source data and a receipt with input checksums.

The optional third argument `--render-only` reads saved curves and contrasts
without loading models or recalculating predictions. Both spatial extents share
the same axes within each panel. Existing output filenames are reused.

`Supplementary_Table_18.csv` preserves the original reclaimed-land comparisons
and adds the whole-city contrasts standardized over the common prediction
profiles. `code/09_figures/export_habitat_comparison_workbook.mjs` exports its
saved table payload as an editable workbook with figure-specific and complete
comparison sheets. It takes the table JSON and workbook destination as arguments
and resolves the spreadsheet runtime from its working directory. Area contrasts
remain on the log1p scale, and full numerical precision is retained in the source
CSV and numeric workbook cells.

## Matched full and environment-only sensitivity figure

`code/09_figures/render_city_reclaimed_full_env_sensitivity.R` takes the downstream
results directory and the existing figure directory as its two arguments. It
loads the saved matched-species models without refitting. Panels a-c use SDM
predictions based on environmental and human activity predictors; panels d-f use
predictions based on environmental predictors alone. Columns show mean invasion
suitability, suitable fraction and effective suitable habitat area, respectively.
Purple circles denote whole-city predictions and green diamonds denote reclaimed
land. Both use common city covariate profiles and the archived interquartile
reclamation contrast. Original model-fitting samples are retained.

Suitable fraction is the area-weighted mean of the cellwise fraction of eligible
species predicted suitable. The species denominator includes valid nonnative
predictions in each cell. The spatial averaging domain is the whole city for the
purple curve and mapped reclaimed land for the green curve. It is neither the
reclaimed share of city land nor the physical land fraction suitable for at least
one species. Effective suitable area integrates the same cellwise fractions over
the relevant land area. Binary summaries use species-specific 10% omission
thresholds. Corresponding columns share vertical scales between SDM predictor
sets; both area panels omit the same empty interval, explicitly labelled.

Outputs use the `city_reclaimed_full_env_sensitivity` stem: editable PDF and SVG,
an R-rendered PNG preview, numeric curves, endpoint contrasts, model formulas and
a checksum receipt. The primary full-species comparison is unchanged. The script
checks that all archived matched reclaimed-land endpoint estimates are reproduced.
Confidence bands are pointwise 95% intervals. Displayed P values test endpoint
contrasts using the saved coefficient covariance; they do not test regional
differences or entire smooth terms.

Suggested title: Reclamation associations with invasion suitability and suitable
habitat under alternative SDM predictor sets.

Suggested caption: a-c, Predictions from species distribution models including
environmental and human activity predictors (FULL). d-f, Predictions from models
using environmental predictors alone (ENV). Columns show mean predicted
suitability for invasive plants (a, d), the area-weighted fraction of eligible
species predicted suitable (b, e), and effective suitable habitat area (c, f).
Purple circles indicate whole-city summaries, including reclaimed land; green
diamonds indicate summaries restricted to mapped reclaimed land. Binary habitat
summaries use species-specific 10% omission thresholds. Curves are adjusted GAM
predictions averaged over common city covariate profiles; shading denotes
pointwise 95% confidence intervals. P values are two-sided Wald tests comparing
predictions at the displayed reclamation endpoints. Horizontal axes are
logarithmic. The linear vertical axes in c and f omit the labelled 15-225 square
kilometre interval, which contains no plotted predictions or confidence bands.
