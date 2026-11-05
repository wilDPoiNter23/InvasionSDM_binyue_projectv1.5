# Independently check response-step models and original-range predictions.
suppressPackageStartupMessages({library(mgcv); library(jsonlite); library(digest)})
cfg <- fromJSON(commandArgs(TRUE)[1]); out <- cfg$output_root
read_table <- function(name) read.csv(file.path(out, name), check.names = FALSE)
ms <- read_table('step_model_summary.csv'); pt <- read_table('step_original_range_profiles.csv')
rs <- read_table('step_reversal_summary.csv'); inputs <- read.csv(cfg$response_steps, check.names = FALSE)
d <- read.csv(cfg$current_city_input); d$log_recl <- log1p(d$reclamation_area)
reference <- read.csv(cfg$reference_city_input)
reference <- reference[!tolower(reference$city) %in% cfg$excluded_city_keys, ]
reference$log_recl <- log1p(reference$reclamation_area)
files <- read_table('input_checksums.csv')
for (i in seq_len(nrow(files))) {
  con <- file(files$path[i], 'rb')
  b <- readBin(con, 'raw', n = file.info(files$path[i], extra_cols = FALSE)$size); close(con)
  stopifnot(identical(digest(b, algo = 'sha256', serialize = FALSE), files$sha256[i]))
}
stopifnot(nrow(ms) == 9L, nrow(rs) == 9L, nrow(pt) == 171L, nrow(d) == 83L,
  identical(d$city, inputs$city), all(ms$n == 83L), all(is.finite(pt$difference)))
current <- readRDS(cfg$current_model)
max_error <- 0
for (i in seq_len(nrow(ms))) {
  step <- ms$response_version[i]; z <- pt[pt$response_version == step, ]
  small <- z[abs(z$area_quantile - .1) < 1e-8, ]; large <- z[abs(z$area_quantile - .9) < 1e-8, ]
  stopifnot(nrow(z) == 19L, nrow(small) == 1L, nrow(large) == 1L,
    identical(small$difference < 0 && large$difference > 0, rs$p10_negative_p90_positive[i]),
    all(abs(z$lower_reclamation_km2 - .004) < 1e-10),
    all(abs(z$upper_reclamation_km2 - 349.78) < 1e-10))
  if (i == 1L) {
    h <- read.csv(cfg$historical_profiles)
    h <- h[h$response == 'ARCHIVED_GAM_RISK' & h$model == 'G0' & h$mode == 'WHOLE_INPUT_RANGE', ]
    stopifnot(max(abs(z$difference - h$difference)) < 1e-12)
    next
  }
  m <- if (i == nrow(ms)) current else readRDS(file.path(out, paste0(step, '.rds')))
  stopifnot(m$converged, identical(deparse1(formula(m)), deparse1(formula(current))),
    m$method == 'REML', nobs(m) == 83L,
    max(abs(m$model$risk - inputs[[step]])) < 1e-12)
  for (field in setdiff(names(m$model), 'risk')) stopifnot(max(abs(m$model[[field]] - d[[field]])) < 1e-12)
  for (j in seq_len(nrow(z))) {
    nd <- data.frame(log_recl = log1p(c(.4, 34978)), area_km2 = z$city_area_km2[j],
      anthro_pc1 = median(reference$anthro_pc1, na.rm = TRUE),
      climate_pc1 = median(reference$climate_pc1, na.rm = TRUE),
      soil_pc1 = median(reference$soil_pc1, na.rm = TRUE),
      marine_pc1 = median(reference$marine_pc1, na.rm = TRUE))
    p <- predict(m, nd, type = 'response')
    X <- predict(m, nd, type = 'lpmatrix'); contrast <- X[2, ] - X[1, ]
    se <- sqrt(sum(contrast * drop(m$Vp %*% contrast)))
    error <- max(abs(c(diff(p) - z$difference[j],
      diff(p) - 1.96 * se - z$lower[j], diff(p) + 1.96 * se - z$upper[j])))
    stopifnot(error < 1e-10); max_error <- max(max_error, error)
  }
}
directions <- rs
directions$p10_positive_p90_negative <- directions$small_difference > 0 & directions$large_difference < 0
directions$direction <- ifelse(directions$p10_negative_p90_positive, 'NEGATIVE_TO_POSITIVE',
  ifelse(directions$p10_positive_p90_negative, 'POSITIVE_TO_NEGATIVE',
  ifelse(directions$small_difference < 0 & directions$large_difference < 0, 'NEGATIVE_AT_BOTH', 'POSITIVE_AT_BOTH')))
write.csv(directions, file.path(out, 'verified_stage_directions.csv'), row.names = FALSE)
lost <- which(head(directions$p10_negative_p90_positive, -1) & !tail(directions$p10_negative_p90_positive, -1)) + 1L
receipt <- list(status = 'PASS_INDEPENDENT_STEP_MODEL_CHECKS', fixed_cities = nrow(d),
  response_versions = nrow(ms), profile_rows = nrow(pt), checked_input_files = nrow(files),
  maximum_prediction_and_interval_error = max_error, only_risk_changed_between_fits = TRUE,
  first_loss_step = if (length(lost)) directions$step[lost[1]] else NA_integer_,
  first_loss_response_version = if (length(lost)) directions$response_version[lost[1]] else NA_character_,
  original_reversal_reappears = if (length(lost)) any(tail(directions$p10_negative_p90_positive, nrow(directions) - lost[1])) else NA,
  no_model_fitting_in_verification = TRUE)
write_json(receipt, file.path(out, 'independent_verification.json'), pretty = TRUE, auto_unbox = TRUE)
print(receipt)
