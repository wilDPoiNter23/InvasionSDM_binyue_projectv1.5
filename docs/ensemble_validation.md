# Fold-matched ensemble validation

The archived full-data EMmean and EMmedian objects contain calibration scores but no independent ensemble validation scores. The scripts here evaluate fold-specific ensembles built from archived single-model predictions, using the aggregation method identified in each accepted final projection. They do not assign the resulting scores to the saved full-data ensemble object.

`code/04_sdm/prepare_ensemble_validation_manifest.R` joins the accepted projection manifest to the archived model-asset manifest. It identifies EMmean or EMmedian from the continuous projection's filename or raster header. Fallback single-model projections and entries without an identifiable ensemble method are recorded separately. Model objects and prediction rasters remain outside the repository.

`code/04_sdm/evaluate_foldmatched_ensembles.R` evaluates one species at a time. Within each archived validation fold, it selects at least two available constituent models using their **training calibration** TSS and ROC scores: both scores above 0.8, or above 0.7 if fewer than two meet the first rule. It combines those models' predictions by the accepted projection's EMmean or EMmedian method. The cutoff maximizing TSS is calculated on the fold's training records and applied without change to the held-out records; held-out AUC is threshold independent. A fold with fewer than two qualified members or inadequate label support receives no ensemble score. No validation score is used to select members or set the TSS cutoff for that fold.

This is an out-of-fold evaluation of a fold-matched ensemble **procedure**. Its members are not necessarily the same fitted members included in the saved full-data ensemble. Selection of the final EMmean or EMmedian method also predates this evaluation. The renderer makes one framed replacement panel for Extended Data Fig. 1b, pooling species across those aggregation methods because they belong to different species rather than paired method comparisons. The source-data table retains each species' aggregation method. Report the figure using that scope, along with the number of species and folds passing. Do not label the archived ensemble's calibration score as cross-validated performance.

Run the manifest preparation once, then distribute numbered species rows. The batch wrapper accepts `ENSEMBLE_CV_MANIFEST`, `ENSEMBLE_CV_OUTPUT`, `RSCRIPT_BIN`, `ENSEMBLE_CV_STRIDE`, `ENSEMBLE_CV_LAST_INDEX` and Slurm's `SLURM_ARRAY_TASK_ID`. The manifest and outputs belong under the project's external evaluations area; the repository keeps only scripts and documentation.

```sh
Rscript code/04_sdm/prepare_ensemble_validation_manifest.R assets.csv accepted.csv input_manifest.csv excluded.csv
Rscript code/04_sdm/evaluate_foldmatched_ensembles.R input_manifest.csv 1 folds
Rscript code/09_figures/render_foldmatched_ensemble_validation.R input_manifest.csv folds figure_prefix
```

The figure renderer requires one complete result file for every manifest row, unless an explicit `unavailable.csv` register lists the missing row indices, species names and reasons. Its fourth argument is the register path. The renderer checks that this register exactly matches the absent result files and exports it with the figure. It plots species with valid multi-member ensemble scores in at least two held-out folds; all fold statuses remain in the source data and coverage table. This coverage rule avoids discarding a species solely because one fold lacked two qualified members. The renderer writes fold-level source data, species-level medians, coverage, summary statistics, SVG, PDF, PNG, TIFF and a caption. Check output counts and source-model identity before using the figure in a manuscript.
