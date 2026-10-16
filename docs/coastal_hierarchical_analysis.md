# Reclamation cohorts and land use

This analysis reuses saved per-species SDM predictions. It estimates associations
of formation cohort and land use with mean predicted suitability, with species
crossing cities and reclamation zones nested within cities.

## Inputs

Copy `config/coastal_bayes.example.json` to a private configuration and set
paths for external inputs and outputs. Relative paths are resolved against the
execution working directory. Inputs include the paired684 manifest, existing
valid city-by-cohort-by-use geometry checkpoints, and city metadata. The manifest
must provide species, ENV/FULL projection paths and their scale divisors.
Geometry checkpoints and the joint city summary come from
`coastal_joint_extract.R`. Exclusions are explicit city keys.

## Execution

From the repository root, with the required R packages installed:

```bash
code=code/10_sensitivity/optional_exploratory
Rscript "$code/test_coastal_bayes.R" "$code"
Rscript "$code/coastal_bayes_prepare.R" CONFIG.json
Rscript "$code/coastal_bayes_extract.R" CONFIG.json ENV 1 684
Rscript "$code/coastal_bayes_fit.R" CONFIG.json ENV
Rscript "$code/coastal_bayes_extract.R" CONFIG.json FULL 1 684
Rscript "$code/coastal_bayes_fit.R" CONFIG.json FULL
Rscript "$code/verify_coastal_bayes.R" CONFIG.json
Rscript "$code/coastal_bayes_report.R" CONFIG.json
```

Extraction supports disjoint species batches. Every finite species-zone mean
and its area denominator are retained; no minimum cell count is imposed.
Dependencies are sf, terra, jsonlite, digest, Rcpp and a C++ compiler.

## Model

For mean suitability S, the response is logit((S+epsilon)/(1+2epsilon)).
Fixed effects are within-city cohort age in five-year units, city mean cohort
age, land use and standardised log zone area. City and nested zone intercepts
are crossed with species intercepts. Fixed coefficients have normal(0,2) priors.
Residual and group variances have inverse-gamma(shape=2,rate=1) priors.

Four independent Gibbs chains use exact Gaussian and inverse-gamma conditional
updates. Joint coefficient/intercept shifts preserve the likelihood while
improving mixing of covariates constant within cities or zones. Analytical and
simulated checks accompany the sampler. The primary boundary adjustment is
epsilon=0.0005. A prior sensitivity uses coefficient SD4 and variance rate0.25;
a separate transform sensitivity uses epsilon=0.001.

## Interpretation and outputs

Predictions integrate random intercepts and residual variation on the response
scale. Ten-year comparisons describe differences between formation cohorts,
conditional on the declared model. They are not repeated observations of land
as it ages. Regression uncertainty is conditional on the saved SDM predictions.

Every coefficient and comparison is retained. `report_tables` consolidates all
groups and sensitivity cases, sample support and chain diagnostics. Relative
percentages are ratios of standardised posterior predictive means; accompanying
interval columns remain differences in the suitability score. Chain status is
separate from independent source and numerical verification.
