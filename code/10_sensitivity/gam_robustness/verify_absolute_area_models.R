# Check saved absolute-area comparisons without fitting models.
suppressPackageStartupMessages({library(mgcv); library(jsonlite); library(digest)})
args <- commandArgs(TRUE)
stopifnot(length(args) %in% c(1L, 2L), length(args) == 1L || args[2] == '--read-only')
read_only <- length(args) == 2L
cfg <- fromJSON(args[1]); out <- cfg$output_root
read_table <- function(name) read.csv(file.path(out, name), check.names = FALSE)
ms <- read_table('all_model_results.csv'); ct <- read_table('all_absolute_area_contrasts.csv')
rs <- read_table('all_reversal_results.csv'); frame <- read_table('fixed_city_input.csv')
stopifnot(nrow(ms) == 15L, nrow(ct) == 570L, nrow(rs) == 30L,
  nrow(frame) == 83L, all(ms$n == 83L), !anyDuplicated(frame$city),
  sum(ms$provenance == 'REUSED') == 6L,
  sum(ms$provenance == 'NEW_DECLARED_COMPARISON') == 9L,
  all(is.finite(ct$difference)), all(ct$lower <= ct$difference), all(ct$upper >= ct$difference))
files <- read_table('input_checksums.csv')
for (i in seq_len(nrow(files))) {
  con <- file(files$path[i], 'rb')
  z <- readBin(con, 'raw', n = file.info(files$path[i], extra_cols = FALSE)$size); close(con)
  stopifnot(identical(digest(z, algo = 'sha256', serialize = FALSE), files$sha256[i]))
}
for (i in seq_len(nrow(rs))) {
  z <- ct[ct$response == rs$response[i] & ct$model == rs$model[i] & ct$mode == rs$mode[i], ]
  a <- z[abs(z$area_quantile - .1) < 1e-8, ]; b <- z[abs(z$area_quantile - .9) < 1e-8, ]
  stopifnot(nrow(z) == 19L, nrow(a) == 1L, nrow(b) == 1L,
    identical(a$difference < 0 && b$difference > 0, rs$small_negative_large_positive[i]),
    identical(a$upper < 0 && b$lower > 0, rs$both_pointwise_intervals_exclude_zero[i]))
}
paired <- read.csv(cfg$paired_city_input)
source_frame <- read.csv(file.path(cfg$current_model_root, 'complete_city_input.csv'))
source_frame$log_recl <- log1p(source_frame$reclamation_area)
source_frame$log_area <- log1p(source_frame$area_km2)
stopifnot(identical(names(frame), names(source_frame)),
  identical(frame$city, source_frame$city))
for (field in setdiff(names(frame), 'city')) {
  stopifnot(isTRUE(all.equal(frame[[field]], source_frame[[field]], tolerance = 1e-12)))
}
reference <- read.csv(cfg$reference_city_input)
reference <- reference[!tolower(reference$city) %in% cfg$excluded_city_keys, ]
stopifnot(nrow(reference) == 107L, !anyDuplicated(tolower(paired$city)))
areas <- as.numeric(quantile(reference$area_km2, seq(.05, .95, .05)))
ranges <- list(COMMON_ABSOLUTE_IQR = as.numeric(quantile(frame$reclamation_area, c(.25, .75))),
  ORIGINAL_FULL_RANGE = range(reference$reclamation_area))
declared <- fromJSON(file.path(out, 'declared_comparison_design.json'))
model_names <- c('STORED_RAW_AREA_TENSOR', 'STORED_ADDITIVE', 'LOG_AREA_TENSOR',
  'LOG_AREA_SEPARATE_INTERACTION', 'LOG_AREA_LINEAR_INTERACTION')
declared_formulas <- unlist(declared$formulas, use.names = TRUE)
stopifnot(length(declared_formulas) == length(model_names))
if (is.null(names(declared_formulas))) names(declared_formulas) <- model_names
stopifnot(setequal(names(declared_formulas), model_names))
stored_names <- c(FULL_814 = 'ARCHIVED_GAM_RISK', PAIRED_FULL_684 = 'PAIRED_FULL', PAIRED_ENV_684 = 'PAIRED_ENV')
stopifnot(setequal(ms$response, names(stored_names)), setequal(ms$model, model_names),
  !anyDuplicated(paste(ms$response, ms$model)))
maximum_prediction_error <- maximum_interval_error <- 0
for (response in names(stored_names)) for (model in model_names) {
  path <- if (grepl('^STORED_', model)) {
    suffix <- if (model == 'STORED_RAW_AREA_TENSOR') 'G0' else 'G1'
    file.path(cfg$current_model_root, paste0(stored_names[[response]], '_', suffix, '.rds'))
  } else file.path(out, paste0(response, '_', model, '.rds'))
  m <- readRDS(path)
  stopifnot(m$converged, nobs(m) == 83L, identical(rownames(m$model), rownames(frame)),
    m$method == 'REML', m$family$family == 'gaussian', m$family$link == 'identity',
    identical(deparse1(formula(m)), declared_formulas[[model]]),
    !any(grepl('fraction|share|ratio', names(m$model))))
  for (field in setdiff(names(m$model), 'risk')) {
    stopifnot(max(abs(m$model[[field]] - frame[[field]])) < 1e-12)
  }
  ix <- match(tolower(frame$city), tolower(paired$city)); stopifnot(!anyNA(ix))
  expected <- switch(response, FULL_814 = frame$risk,
    PAIRED_FULL_684 = paired$PAIRED_FULL_684[ix],
    PAIRED_ENV_684 = paired$PAIRED_ENV_684[ix])
  stopifnot(max(abs(m$model$risk - expected)) < 1e-12)
  for (mode in names(ranges)) {
    z <- ct[ct$response == response & ct$model == model & ct$mode == mode, ]
    stopifnot(nrow(z) == 19L, max(abs(z$area_quantile - seq(.05, .95, .05))) < 1e-12,
      max(abs(z$city_area_km2 - areas)) < 1e-8)
    R <- ranges[[mode]]
    stopifnot(all(abs(z$lower_reclamation_km2 - R[1] / 100) < 1e-10),
      all(abs(z$upper_reclamation_km2 - R[2] / 100) < 1e-10),
      identical(z$physically_possible, R[2] <= 100 * areas))
    for (j in seq_len(nrow(z))) {
      nd <- data.frame(log_recl = log1p(R), area_km2 = areas[j], log_area = log1p(areas[j]),
        anthro_pc1 = median(reference$anthro_pc1, na.rm = TRUE),
        climate_pc1 = median(reference$climate_pc1, na.rm = TRUE),
        soil_pc1 = median(reference$soil_pc1, na.rm = TRUE),
        marine_pc1 = median(reference$marine_pc1, na.rm = TRUE))
      value <- as.numeric(diff(predict(m, nd, type = 'response')))
      X <- predict(m, nd, type = 'lpmatrix'); delta <- X[2, ] - X[1, ]
      se <- sqrt(sum(delta * drop(m$Vp %*% delta)))
      prediction_error <- abs(value - z$difference[j])
      interval_error <- max(abs(c(value - 1.96 * se - z$lower[j], value + 1.96 * se - z$upper[j])))
      stopifnot(prediction_error < 1e-9, interval_error < 1e-9)
      maximum_prediction_error <- max(maximum_prediction_error, prediction_error)
      maximum_interval_error <- max(maximum_interval_error, interval_error)
    }
  }
}
receipt <- list(status = 'PASS_SAVED_COMPARISON_CHECKS', models = nrow(ms),
  cities = nrow(frame), contrasts = nrow(ct), unchanged_source_files = nrow(files),
  primary_reversals = sum(rs$small_negative_large_positive[rs$mode == 'COMMON_ABSOLUTE_IQR']),
  secondary_reversals = sum(rs$small_negative_large_positive[rs$mode == 'ORIGINAL_FULL_RANGE']),
  maximum_direct_prediction_error = maximum_prediction_error,
  maximum_interval_reconstruction_error = maximum_interval_error,
  no_models_refitted_by_verification = TRUE, read_only = read_only)
if (!read_only) write_json(receipt, file.path(out, 'independent_verification.json'), auto_unbox = TRUE, pretty = TRUE)
print(receipt)
