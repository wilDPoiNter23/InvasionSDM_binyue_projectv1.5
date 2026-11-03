# Hold the GAM specification fixed while changing one response-processing step.
suppressPackageStartupMessages({library(mgcv); library(jsonlite); library(digest)})
args <- commandArgs(TRUE); stopifnot(length(args) == 1L)
cfg <- fromJSON(args[1]); out <- cfg$output_root
stopifnot(!dir.exists(out)); dir.create(out, recursive = TRUE)
write_table <- function(x, name) write.csv(x, file.path(out, name), row.names = FALSE)
hash <- function(path) {
  con <- file(path, 'rb'); on.exit(close(con))
  digest(readBin(con, 'raw', n = file.info(path, extra_cols = FALSE)$size),
    algo = 'sha256', serialize = FALSE)
}
input_paths <- c(args[1], cfg$response_steps, cfg$current_city_input, cfg$current_model,
  cfg$historical_city_input, cfg$historical_summary, cfg$historical_profiles,
  cfg$reference_city_input, cfg$processing_summary)
before_hashes <- vapply(input_paths, hash, '')
d <- read.csv(cfg$current_city_input)
h <- read.csv(cfg$historical_city_input)
r <- read.csv(cfg$response_steps, check.names = FALSE)
processing <- read.csv(cfg$processing_summary)
reference <- read.csv(cfg$reference_city_input)
reference <- reference[!tolower(reference$city) %in% cfg$excluded_city_keys, ]
historical_summary <- read.csv(cfg$historical_summary)
historical_profiles <- read.csv(cfg$historical_profiles)
historical_profiles <- historical_profiles[historical_profiles$response == 'ARCHIVED_GAM_RISK' &
  historical_profiles$model == 'G0' & historical_profiles$mode == 'WHOLE_INPUT_RANGE', ]
historical_summary <- historical_summary[historical_summary$response == 'ARCHIVED_GAM_RISK' &
  historical_summary$model == 'G0', ]
stopifnot(nrow(d) == 83L, nrow(h) == 83L, nrow(r) == 83L, nrow(reference) == 107L,
  identical(d$city, h$city), identical(d$city, r$city),
  identical(d[, setdiff(names(d), 'risk')], h[, setdiff(names(h), 'risk')]),
  max(abs(r$ARCHIVED_MIXED_EXTRACTION - h$risk)) < 1e-12,
  max(abs(r$NATIVE_REMOVED_FROM_DENOMINATOR - d$risk)) < 1e-12,
  nrow(historical_summary) == 1L, nrow(historical_profiles) == 19L)
d$log_recl <- log1p(d$reclamation_area)
reference$log_recl <- log1p(reference$reclamation_area)
area_quantiles <- seq(.05, .95, .05)
areas <- as.numeric(quantile(reference$area_km2, area_quantiles))
range_ha <- range(reference$reclamation_area)
stopifnot(max(abs(areas - historical_profiles$city_area_km2)) < 1e-8,
  abs(range_ha[1] / 100 - .004) < 1e-10, abs(range_ha[2] / 100 - 349.78) < 1e-10,
  all(abs(historical_profiles$reclamation_low_ha - range_ha[1]) < 1e-8),
  all(abs(historical_profiles$reclamation_high_ha - range_ha[2]) < 1e-8))
steps <- c('ARCHIVED_MIXED_EXTRACTION', processing$to)
stopifnot(length(steps) == 9L, !anyDuplicated(steps), all(steps %in% names(r)))
current <- readRDS(cfg$current_model)
form <- formula(current)
stopifnot(identical(current$method, 'REML'), current$family$family == 'gaussian',
  current$family$link == 'identity', nrow(current$model) == 83L,
  identical(current$smooth[[1]]$term, c('log_recl', 'area_km2')),
  all(vapply(current$smooth[[1]]$margin, function(x) x$bs.dim, numeric(1)) == 5))
write_json(list(status = 'DECLARED_BEFORE_INTERMEDIATE_FITS', steps = steps,
  formula = deparse1(form), method = 'REML', family = 'gaussian identity', cities = d$city,
  reclamation_range_km2 = range_ha / 100, city_size_quantiles = area_quantiles,
  city_size_reference = 'Original reference city table after the previously declared city exclusion',
  smoothing = 'Same basis dimensions and REML procedure; smoothing parameters estimated for each response',
  reused = c('Archived 83-city model summaries and original-range predictions', 'Current 83-city fitted G0 object'),
  new_fits = 7L, sdm_fits = 0L), file.path(out, 'declared_step_comparison.json'),
  pretty = TRUE, auto_unbox = TRUE)
write_table(d, 'fixed_predictors.csv')
base <- data.frame(log_recl = median(reference$log_recl), area_km2 = median(reference$area_km2),
  anthro_pc1 = median(reference$anthro_pc1, na.rm = TRUE),
  climate_pc1 = median(reference$climate_pc1, na.rm = TRUE),
  soil_pc1 = median(reference$soil_pc1, na.rm = TRUE),
  marine_pc1 = median(reference$marine_pc1, na.rm = TRUE))
summaries <- profiles <- coefficients <- fitted_values <- curves <- list()
maximum_prediction_error <- 0
for (i in seq_along(steps)) {
  step <- steps[i]; frame <- d; frame$risk <- r[[step]]
  stopifnot(all(is.finite(frame$risk)))
  if (i == 1L) {
    sm <- historical_summary
    summaries[[step]] <- data.frame(step = i - 1L, response_version = step,
      provenance = 'REUSED_HISTORICAL_83_CITY_RESULTS', n = sm$n,
      mean_risk = mean(frame$risk), adjusted_r2 = sm$adjusted_r2, AIC = sm$AIC,
      tensor_joint_p = sm$focal_p, tensor_edf = sm$focal_edf)
    z <- historical_profiles
    profiles[[step]] <- data.frame(step = i - 1L, response_version = step,
      area_quantile = z$area_quantile, city_area_km2 = z$city_area_km2,
      lower_reclamation_km2 = z$reclamation_low_ha / 100, upper_reclamation_km2 = z$reclamation_high_ha / 100,
      difference = z$difference, lower = z$lower, upper = z$upper,
      physically_possible = z$physically_within_city)
    next
  }
  if (i == length(steps)) {
    m <- current; provenance <- 'REUSED_CURRENT_MODEL'
    stopifnot(max(abs(m$model$risk - frame$risk)) < 1e-12)
  } else {
    set.seed(cfg$seed)
    m <- gam(form, data = frame, method = 'REML', family = gaussian())
    provenance <- 'NEW_INTERMEDIATE_RESPONSE_FIT'
    saveRDS(m, file.path(out, paste0(step, '.rds')))
  }
  stopifnot(m$converged, nobs(m) == 83L, identical(deparse1(formula(m)), deparse1(form)))
  for (field in setdiff(names(m$model), 'risk')) stopifnot(max(abs(m$model[[field]] - d[[field]])) < 1e-12)
  sm <- summary(m)
  summaries[[step]] <- data.frame(step = i - 1L, response_version = step, provenance = provenance,
    n = nobs(m), mean_risk = mean(frame$risk), adjusted_r2 = sm$r.sq, AIC = AIC(m),
    tensor_joint_p = sm$s.table[1, 4], tensor_edf = sm$s.table[1, 1])
  coefficients[[step]] <- data.frame(response_version = step, term = names(coef(m)), estimate = coef(m))
  fitted_values[[step]] <- data.frame(response_version = step, city = d$city, risk = frame$risk,
    fitted = fitted(m), residual = residuals(m))
  z <- lapply(seq_along(areas), function(j) {
    nd <- base[rep(1, 2), ]; nd$log_recl <- log1p(range_ha); nd$area_km2 <- areas[j]
    X <- predict(m, nd, type = 'lpmatrix'); delta <- X[2, ] - X[1, ]
    value <- sum(delta * coef(m)); se <- sqrt(drop(delta %*% m$Vp %*% delta))
    error <- abs(value - diff(predict(m, nd)))
    stopifnot(error < 1e-10); maximum_prediction_error <<- max(maximum_prediction_error, error)
    data.frame(step = i - 1L, response_version = step, area_quantile = area_quantiles[j], city_area_km2 = areas[j],
      lower_reclamation_km2 = range_ha[1] / 100, upper_reclamation_km2 = range_ha[2] / 100,
      difference = value, lower = value - 1.96 * se, upper = value + 1.96 * se,
      physically_possible = range_ha[2] <= 100 * areas[j])
  })
  profiles[[step]] <- do.call(rbind, z)
  for (q in c(.1, .5, .9)) {
    amounts <- expm1(seq(log1p(range_ha[1]), log1p(range_ha[2]), length.out = 401))
    nd <- base[rep(1, length(amounts)), ]; nd$log_recl <- log1p(amounts)
    nd$area_km2 <- as.numeric(quantile(reference$area_km2, q))
    pr <- predict(m, nd, se.fit = TRUE)
    curves[[paste(step, q)]] <- data.frame(step = i - 1L, response_version = step, area_quantile = q,
      city_area_km2 = nd$area_km2, reclamation_km2 = amounts / 100,
      fitted = pr$fit, lower = pr$fit - 1.96 * pr$se.fit, upper = pr$fit + 1.96 * pr$se.fit,
      physically_possible = amounts <= 100 * nd$area_km2)
  }
  cat('Step', i - 1L, step, provenance, '\n'); flush.console()
}
pt <- do.call(rbind, profiles); ms <- do.call(rbind, summaries)
reversals <- do.call(rbind, lapply(steps, function(step) {
  z <- pt[pt$response_version == step, ]; a <- z[abs(z$area_quantile - .1) < 1e-8, ]
  b <- z[abs(z$area_quantile - .9) < 1e-8, ]; crosses <- which(head(z$difference, -1) < 0 & tail(z$difference, -1) > 0)
  data.frame(step = z$step[1], response_version = step,
    small_city_km2 = a$city_area_km2, small_difference = a$difference, small_lower = a$lower, small_upper = a$upper,
    large_city_km2 = b$city_area_km2, large_difference = b$difference, large_lower = b$lower, large_upper = b$upper,
    p10_negative_p90_positive = a$difference < 0 && b$difference > 0,
    p10_p90_pointwise_intervals_exclude_zero = a$upper < 0 && b$lower > 0,
    scan_negative_to_positive = length(crosses) > 0,
    first_crossing_lower_city_km2 = if (length(crosses)) z$city_area_km2[crosses[1]] else NA_real_,
    first_crossing_upper_city_km2 = if (length(crosses)) z$city_area_km2[crosses[1] + 1L] else NA_real_)
}))
reversals$previous_step_p90_difference <- c(NA, head(reversals$large_difference, -1))
reversals$p90_difference_change <- reversals$large_difference - reversals$previous_step_p90_difference
write_table(ms, 'step_model_summary.csv'); write_table(pt, 'step_original_range_profiles.csv')
write_table(reversals, 'step_reversal_summary.csv')
write_table(do.call(rbind, fitted_values), 'intermediate_city_fits.csv')
write_table(do.call(rbind, coefficients), 'intermediate_coefficients.csv')
write_table(do.call(rbind, curves), 'intermediate_conditional_curves.csv')
stopifnot(identical(vapply(input_paths, hash, ''), before_hashes))
script_path <- sub('^--file=', '', grep('^--file=', commandArgs(FALSE), value = TRUE))
sources <- c(input_paths, script_path)
write_table(data.frame(path = sources, sha256 = vapply(sources, hash, '')), 'input_checksums.csv')
write_json(list(status = 'COMPLETED_STEPWISE_GAM_NUMERICAL_CHECKS', response_versions = 9L,
  new_gam_fits = 7L, reused_versions = 2L, sdm_fits = 0L, fixed_cities = 83L,
  unchanged_formula = deparse1(form), unchanged_predictors = TRUE,
  original_reclamation_range_km2 = range_ha / 100,
  maximum_direct_prediction_error = maximum_prediction_error,
  interpretation = 'Sequential response-processing sensitivity; joint tensor significance is not proof of a directional reversal'),
  file.path(out, 'step_comparison_receipt.json'), auto_unbox = TRUE, pretty = TRUE)
print(reversals)
