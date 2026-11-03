# Independently verify fixed-684 city responses and saved GAM predictions.
suppressPackageStartupMessages({library(mgcv); library(Matrix); library(jsonlite); library(digest)})
cfg <- fromJSON(commandArgs(TRUE)[1]); out <- cfg$output_root
read_table <- function(name) read.csv(file.path(out, name), check.names = FALSE)
files <- read_table('input_checksums.csv')
for (i in seq_len(nrow(files))) {
  con <- file(files$path[i], 'rb')
  x <- readBin(con, 'raw', n = file.info(files$path[i], extra_cols = FALSE)$size); close(con)
  stopifnot(identical(digest(x, algo = 'sha256', serialize = FALSE), files$sha256[i]))
}
d <- read.csv(cfg$city_input); d$log_recl <- log1p(d$reclamation_area)
city <- read_table('city_response_definitions.csv')
s <- readRDS(file.path(cfg$native_result_root, 'analysis_support.rds'))
v <- readRDS(file.path(cfg$native_result_root, 'scaled_species_values.rds'))
native <- readRDS(file.path(cfg$native_result_root, 'native_cell_membership.rds'))
saved <- readRDS(file.path(out, 'fixed_pool_cell_products.rds'))
key <- function(x) trimws(gsub('[^a-z0-9]+', ' ', tolower(x)))
stopifnot(nrow(city) == 83L, identical(city$city, d$city), identical(saved$cells, s$cells),
  identical(saved$species, v$paired_species), ncol(v$paired_env) == 684L,
  all(saved$counts$pool == 684L))
mask <- native$native[, match(v$paired_species, native$species), drop = FALSE]
w <- s$weights[s$weights$footprint == 'HISTORICAL' & key(s$weights$city_id) %in% key(d$city), ]
W <- sparseMatrix(i = match(key(w$city_id), key(d$city)), j = w$cell_index,
  x = w$weight_km2, dims = c(83L, length(s$cells)))
area <- as.numeric(rowSums(W)); stopifnot(all(area > 0))
x <- v$paired_env; x[!is.finite(x)] <- 0
expected_unmasked <- rowSums(as.matrix(W %*% x)) / (684 * area)
x[mask] <- 0
expected_native <- rowSums(as.matrix(W %*% x)) / (684 * area)
city_error <- max(abs(c(expected_unmasked - city$ENV_UNMASKED_FIXED684,
  expected_native - city$ENV_NATIVE_FIXED684)))
stopifnot(city_error < 1e-10, all(city$ENV_NATIVE_FIXED684 <= city$ENV_UNMASKED_FIXED684 + 1e-12))
no_env <- rowSums(is.finite(v$paired_env)) == 0
stopifnot(all(saved$values$ENV_UNMASKED_FIXED684[no_env] == 0),
  all(saved$values$ENV_NATIVE_FIXED684[no_env] == 0))
current <- readRDS(cfg$current_env_model)
pt <- read_table('fixed_pool_original_range_profiles.csv'); rs <- read_table('fixed_pool_reversal_summary.csv')
stopifnot(nrow(pt) == 76L, nrow(rs) == 4L)
ref <- read.csv(cfg$reference_city_input); ref <- ref[!tolower(ref$city) %in% cfg$excluded_city_keys, ]
maximum_prediction_error <- 0
for (label in rs$response) {
  model <- if (label == 'ENV_NATIVE_AVAILABLE') current else readRDS(file.path(out, paste0(label, '.rds')))
  stopifnot(nobs(model) == 83L, model$method == 'REML', model$converged,
    identical(deparse1(formula(model)), deparse1(formula(current))),
    max(abs(model$model$risk - city[[label]])) < 1e-10)
  for (field in setdiff(names(model$model), 'risk')) stopifnot(max(abs(model$model[[field]] - d[[field]])) < 1e-12)
  z <- pt[pt$response == label, ]
  for (i in seq_len(nrow(z))) {
    nd <- data.frame(log_recl = log1p(c(.4, 34978)), area_km2 = z$city_area_km2[i],
      anthro_pc1 = median(ref$anthro_pc1, na.rm = TRUE), climate_pc1 = median(ref$climate_pc1, na.rm = TRUE),
      soil_pc1 = median(ref$soil_pc1, na.rm = TRUE), marine_pc1 = median(ref$marine_pc1, na.rm = TRUE))
    value <- diff(predict(model, nd)); X <- predict(model, nd, type = 'lpmatrix')
    delta <- X[2, ] - X[1, ]; se <- sqrt(sum(delta * drop(model$Vp %*% delta)))
    err <- max(abs(c(value - z$difference[i], value - 1.96 * se - z$lower[i],
      value + 1.96 * se - z$upper[i])))
    stopifnot(err < 1e-10); maximum_prediction_error <- max(maximum_prediction_error, err)
  }
  small <- z[abs(z$area_quantile - .1) < 1e-8, ]; large <- z[abs(z$area_quantile - .9) < 1e-8, ]
  q <- rs[rs$response == label, ]
  stopifnot(identical(small$difference < 0 && large$difference > 0, q$small_negative_large_positive))
}
write_json(list(status = 'PASS_FIXED684_ENV_INDEPENDENT_CHECKS', fixed_cities = 83L,
  fixed_pool = 684L, definitions = 4L, profiles = nrow(pt), no_env_prediction_cells_zero_filled = sum(no_env),
  city_mean_error_from_specieswise_spatial_aggregation = city_error,
  maximum_direct_prediction_and_interval_error = maximum_prediction_error,
  primary_reversals = sum(rs$small_negative_large_positive[rs$primary_fixed_pool]),
  no_models_fitted_by_verification = TRUE), file.path(out, 'independent_verification.json'),
  pretty = TRUE, auto_unbox = TRUE)
cat('Fixed denominators, native contributions, city weights and all GAM contrasts verified.\n')
