# Absolute reclamation area comparisons

`code/10_sensitivity/gam_robustness/compare_absolute_area_models.R` compares a fixed
set of absolute-area models using an external JSON configuration. It reuses the
stored raw-city-area tensor GAM and additive GAM and fits three declared
alternatives: a log-city-area tensor, separate smooth main effects and a tensor
interaction, and a linear interaction between log absolute reclamation area and
log city area. All models retain the same environmental and human covariates,
Gaussian response family and REML method. No SDM is fitted.

The three responses are the complete FULL aggregate and the paired FULL and ENV
aggregates. The same 83 cities are retained for every comparison. A declaration
file is written before fitting the nine additional models. All 15 reused or new
model-response combinations are reported; no model is selected for its sign.

The principal contrast uses the same absolute reclamation-area interquartile
endpoints at small and large city sizes. The secondary contrast retains the
original full reclamation range and flags physically impossible combinations.
Neighbouring cities' observed reclamation ranges are reported separately.
Negative-to-positive reversal requires a negative small-city contrast and a
positive large-city contrast. Pointwise interval support is recorded separately
from the fitted direction. The tensor joint-term test is not labelled as an
interaction test. The separate-interaction GAM and linear interaction provide
explicit interaction tests.

Configuration keys are `current_model_root`, `paired_city_input`,
`reference_city_input`, `excluded_city_keys`, `seed` and `output_root`. Keep
configuration, fitted objects, complete numerical tables and logs outside the
repository. Run `verify_absolute_area_models.R` with the same configuration to
check source checksums, saved response vectors, sample consistency, interval
arithmetic and reversal classifications without refitting any models.
The verifier reconstructs all 570 contrasts and their intervals from the six
reused and nine additional saved models. Add `--read-only` after the configuration
path to print the verification receipt without writing or replacing any result.

`trace_response_gam_steps.R` holds the original absolute-area G0 formula, basis
dimensions, REML procedure, control variables, 83-city sample and full original
reclamation prediction range fixed across the nine saved response-processing
versions. It reuses the archived baseline predictions and the current fitted
model and fits only the seven intermediate responses. The original raster's
polygon-centre extraction is evaluated before changing contributors,
normalization, native contributions or missing-value denominators. This order
separates the first disappearance of a fitted reversal from later processing
effects. Use `verify_response_gam_steps.R` to check every saved intermediate
response, fixed predictor vector, original-range contrast and interval without
additional fitting. Opposite-direction fitted contrasts are labelled separately.
Keep these processing comparisons distinct from ecological mechanism tests.

`check_supported_local_slopes.R` checks a separate exploratory question using only
the three saved current G0 models: whether local reclamation slopes become
positive at observed large-city conditions. All 83 observed cities and a common
absolute-area grid are retained. Joint-domain support is flagged using the
observed convex hull in the fitted log-reclamation and raw-city-area coordinates;
both finite-difference endpoints must be supported. Report pointwise uncertainty
and simulated simultaneous bands, including unsupported positive fitted values.
Use `verify_supported_local_slopes.R` to check the saved observed-city derivatives
by direct predictions at each city's actual covariates. A local positive point
estimate is distinct from reproducing the original full-range scale reversal.
