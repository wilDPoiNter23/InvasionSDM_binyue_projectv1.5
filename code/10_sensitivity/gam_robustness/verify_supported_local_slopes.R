# Verify saved local slopes, uncertainty and sample-support counts without fitting.
suppressPackageStartupMessages({library(mgcv); library(jsonlite); library(digest)})
cfg <- fromJSON(commandArgs(TRUE)[1]); out <- cfg$output_root
observed <- read.csv(file.path(out, 'all_observed_city_local_slopes.csv'))
grid <- read.csv(file.path(out, 'all_grid_local_slopes.csv'))
summary <- read.csv(file.path(out, 'local_slope_summary.csv'))
models <- c(FULL_814 = 'ARCHIVED_GAM_RISK_G0.rds', PAIRED_FULL_684 = 'PAIRED_FULL_G0.rds', PAIRED_ENV_684 = 'PAIRED_ENV_G0.rds')
files <- read.csv(file.path(out, 'input_checksums.csv'))
for (i in seq_len(nrow(files))) {
  con <- file(files$path[i], 'rb')
  b <- readBin(con, 'raw', n = file.info(files$path[i], extra_cols = FALSE)$size); close(con)
  stopifnot(identical(digest(b, algo = 'sha256', serialize = FALSE), files$sha256[i]))
}
stopifnot(nrow(observed) == 249L, nrow(summary) == 3L)
maximum_error <- 0
for (label in names(models)) {
  m <- readRDS(file.path(cfg$model_root, models[[label]]))
  z <- observed[observed$response == label, ]; g <- grid[grid$response == label, ]
  stopifnot(nrow(z) == 83L, nrow(g) == 5063L)
  plus <- minus <- m$model
  plus$log_recl <- plus$log_recl + 1e-4
  minus$log_recl <- minus$log_recl - 1e-4
  slope <- (predict(m, plus) - predict(m, minus)) / 2e-4
  error <- max(abs(slope - z$slope)); stopifnot(error < 1e-8)
  maximum_error <- max(maximum_error, error)
  D <- (predict(m, plus, type = 'lpmatrix') - predict(m, minus, type = 'lpmatrix')) / 2e-4
  se <- sqrt(pmax(0, rowSums((D %*% m$Vp) * D)))
  stopifnot(max(abs(z$lower - (slope - 1.96 * se))) < 1e-8,
    max(abs(z$upper - (slope + 1.96 * se))) < 1e-8)
  s <- summary[summary$response == label, ]
  supported <- g$two_sided_hull_support & g$physically_possible
  stopifnot(s$observed_positive_slopes == sum(z$slope > 0),
    s$observed_large_positive_pointwise == sum(z$large_city & z$lower > 0),
    s$supported_large_positive_grid_points == sum(g$large_city & supported & g$slope > 0),
    s$supported_large_positive_pointwise == sum(g$large_city & supported & g$lower > 0),
    s$supported_large_positive_simultaneous == sum(g$large_city & supported & g$simultaneous_lower > 0),
    all(g$simultaneous_lower <= g$lower), all(g$simultaneous_upper >= g$upper))
}
write_json(list(status = 'PASS_LOCAL_SLOPE_VERIFICATION', fixed_cities_per_response = 83L,
  models = 3L, grid_points_per_response = 5063L,
  maximum_direct_prediction_derivative_error = maximum_error, no_model_fits = TRUE,
  supported_positive_large_city_slopes_with_pointwise_intervals = sum(summary$supported_large_positive_pointwise)),
  file.path(out, 'independent_verification.json'), pretty = TRUE, auto_unbox = TRUE)
cat('All three response versions and all city rows verified without fitting.\n')
