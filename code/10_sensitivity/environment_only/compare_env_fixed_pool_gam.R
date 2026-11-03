# Compare fixed-pool and available-species ENV means with native masking separated.
suppressPackageStartupMessages({library(mgcv); library(jsonlite); library(digest)})
args <- commandArgs(TRUE); stopifnot(length(args) == 1L)
cfg <- fromJSON(args[1]); out <- cfg$output_root
stopifnot(!dir.exists(out)); dir.create(out, recursive = TRUE)
write_table <- function(x, name) write.csv(x, file.path(out, name), row.names = FALSE)
hash <- function(path) {
  con <- file(path, 'rb'); on.exit(close(con))
  digest(readBin(con, 'raw', n = file.info(path, extra_cols = FALSE)$size), algo = 'sha256', serialize = FALSE)
}
paths <- c(args[1], cfg$city_input, cfg$reference_city_input, cfg$current_env_model,
  file.path(cfg$native_result_root, c('analysis_support.rds', 'scaled_species_values.rds',
    'native_cell_membership.rds', 'continuous_input_scales.csv')))
before <- vapply(paths, hash, '')
d <- read.csv(cfg$city_input)
reference <- read.csv(cfg$reference_city_input)
reference <- reference[!tolower(reference$city) %in% cfg$excluded_city_keys, ]
d$log_recl <- log1p(d$reclamation_area)
reference$log_recl <- log1p(reference$reclamation_area)
key <- function(x) trimws(gsub('[^a-z0-9]+', ' ', tolower(x)))
stopifnot(nrow(d) == 83L, !anyDuplicated(key(d$city)), nrow(reference) == 107L)
s <- readRDS(file.path(cfg$native_result_root, 'analysis_support.rds'))
values <- readRDS(file.path(cfg$native_result_root, 'scaled_species_values.rds'))
masks <- readRDS(file.path(cfg$native_result_root, 'native_cell_membership.rds'))
v <- values$paired_env
species_index <- match(values$paired_species, masks$species)
stopifnot(ncol(v) == 684L, length(values$paired_species) == 684L, !anyDuplicated(values$paired_species),
  !anyNA(species_index), identical(s$cells, masks$cells), nrow(v) == length(s$cells),
  all(v[is.finite(v)] >= 0 & v[is.finite(v)] <= 1))
native <- masks$native[, species_index, drop = FALSE]
finite <- is.finite(v)
unmasked <- v; unmasked[!finite] <- 0
masked <- unmasked; masked[native] <- 0
count <- rowSums(finite)
alien_count <- rowSums(finite & !native)
total <- rowSums(unmasked); alien_total <- rowSums(masked)
cell_values <- list(
  ENV_UNMASKED_FIXED684 = total / 684,
  ENV_NATIVE_FIXED684 = alien_total / 684,
  ENV_UNMASKED_AVAILABLE = ifelse(count > 0, total / count, NA_real_),
  ENV_NATIVE_AVAILABLE = ifelse(alien_count > 0, alien_total / alien_count, NA_real_))
stopifnot(all(cell_values$ENV_NATIVE_FIXED684 <= cell_values$ENV_UNMASKED_FIXED684 + 1e-12))
w <- s$weights[s$weights$footprint == 'HISTORICAL' & key(s$weights$city_id) %in% key(d$city), ]
groups <- lapply(key(d$city), function(k) which(key(w$city_id) == k))
stopifnot(all(lengths(groups) > 0L))
aggregate_cells <- function(x) vapply(groups, function(ix) {
  a <- x[w$cell_index[ix]]; good <- is.finite(a)
  if (any(good)) weighted.mean(a[good], w$weight_km2[ix][good]) else NA_real_
}, numeric(1))
city <- data.frame(city = d$city, area_km2 = d$area_km2,
  lapply(cell_values, aggregate_cells), check.names = FALSE)
city$no_env_prediction_area_km2 <- vapply(groups, function(ix)
  sum(w$weight_km2[ix][count[w$cell_index[ix]] == 0]), numeric(1))
city$no_alien_prediction_area_km2 <- vapply(groups, function(ix)
  sum(w$weight_km2[ix][alien_count[w$cell_index[ix]] == 0]), numeric(1))
city$total_city_area_km2 <- vapply(groups, function(ix) sum(w$weight_km2[ix]), numeric(1))
stopifnot(all(is.finite(as.matrix(city[, names(cell_values)]))))
write_table(city, 'city_response_definitions.csv')
saveRDS(list(cells = s$cells, species = values$paired_species, values = cell_values,
  counts = data.frame(env_predicted = count, alien_predicted = alien_count,
    native = rowSums(native), pool = 684L)), file.path(out, 'fixed_pool_cell_products.rds'))
write_table(data.frame(species = values$paired_species,
  native_mapping_present = masks$mapping$native_vector_present[species_index]), 'species_and_mask_contract.csv')
current <- readRDS(cfg$current_env_model); form <- formula(current)
stopifnot(nobs(current) == 83L, current$method == 'REML', current$family$family == 'gaussian',
  identical(current$smooth[[1]]$term, c('log_recl', 'area_km2')),
  max(abs(city$ENV_NATIVE_AVAILABLE - current$model$risk)) < 1e-10)
range_ha <- range(reference$reclamation_area)
stopifnot(abs(range_ha[1] / 100 - .004) < 1e-10, abs(range_ha[2] / 100 - 349.78) < 1e-10)
areas <- as.numeric(quantile(reference$area_km2, seq(.05, .95, .05)))
write_json(list(status = 'DECLARED_BEFORE_GAM_FITS', primary = names(cell_values)[1:2],
  reference_controls = names(cell_values)[3:4], species = 684L, cities = d$city,
  input_scale = 'Saved BIOMOD predictions divided by 1000, without min-max normalization',
  zero_rule = 'Missing ENV predictions contribute zero; every supported city cell remains in each fixed-pool mean',
  native_rule = 'Saved native-country cell membership; native contribution zero and denominator retained at 684',
  available_rule = 'Unknown predictions excluded from the denominator; native cells also excluded in the masked available mean',
  city_rule = 'Identical saved polygon-interior cell-area weights for all four definitions',
  formula = deparse1(form), method = 'REML', original_reclamation_range_km2 = range_ha / 100,
  env_finite_full_missing_pairs = sum(finite & !is.finite(values$paired_full)),
  sdm_fits = 0L, new_gam_fits = 3L, reused_models = 1L),
  file.path(out, 'declared_fixed_pool_comparison.json'), pretty = TRUE, auto_unbox = TRUE)
base <- data.frame(log_recl = median(reference$log_recl), area_km2 = median(reference$area_km2),
  anthro_pc1 = median(reference$anthro_pc1, na.rm = TRUE), climate_pc1 = median(reference$climate_pc1, na.rm = TRUE),
  soil_pc1 = median(reference$soil_pc1, na.rm = TRUE), marine_pc1 = median(reference$marine_pc1, na.rm = TRUE))
models <- summaries <- profiles <- curves <- list(); maximum_prediction_error <- 0
for (label in names(cell_values)) {
  frame <- d; frame$risk <- city[[label]]
  if (label == 'ENV_NATIVE_AVAILABLE') {
    model <- current; provenance <- 'REUSED_SAVED_MODEL'
  } else {
    set.seed(cfg$seed)
    model <- gam(form, data = frame, method = 'REML', family = gaussian())
    saveRDS(model, file.path(out, paste0(label, '.rds')))
    provenance <- 'NEW_RESPONSE_DEFINITION_FIT'
  }
  stopifnot(model$converged, nobs(model) == 83L, identical(deparse1(formula(model)), deparse1(form)))
  for (field in setdiff(names(model$model), 'risk')) stopifnot(max(abs(model$model[[field]] - d[[field]])) < 1e-12)
  models[[label]] <- model; sm <- summary(model)
  summaries[[label]] <- data.frame(response = label, provenance = provenance, cities = 83L, species = 684L,
    mean_risk = mean(frame$risk), adjusted_r2 = sm$r.sq, AIC = AIC(model),
    tensor_joint_p = sm$s.table[1, 4], tensor_edf = sm$s.table[1, 1])
  for (j in seq_along(areas)) {
    nd <- base[rep(1, 2), ]; nd$area_km2 <- areas[j]; nd$log_recl <- log1p(range_ha)
    X <- predict(model, nd, type = 'lpmatrix'); delta <- X[2, ] - X[1, ]
    value <- sum(delta * coef(model)); se <- sqrt(drop(delta %*% model$Vp %*% delta))
    err <- abs(value - diff(predict(model, nd))); stopifnot(err < 1e-10)
    maximum_prediction_error <- max(maximum_prediction_error, err)
    profiles[[paste(label, j)]] <- data.frame(response = label, area_quantile = j * .05,
      city_area_km2 = areas[j], lower_reclamation_km2 = range_ha[1] / 100,
      upper_reclamation_km2 = range_ha[2] / 100, difference = value,
      lower = value - 1.96 * se, upper = value + 1.96 * se,
      physically_possible = range_ha[2] <= 100 * areas[j])
  }
  for (q in c(.1, .5, .9)) {
    R <- expm1(seq(log1p(range_ha[1]), log1p(range_ha[2]), length.out = 401))
    nd <- base[rep(1, length(R)), ]; nd$area_km2 <- as.numeric(quantile(reference$area_km2, q))
    nd$log_recl <- log1p(R); p <- predict(model, nd, se.fit = TRUE)
    curves[[paste(label, q)]] <- data.frame(response = label, area_quantile = q,
      city_area_km2 = nd$area_km2, reclamation_km2 = R / 100,
      fitted = p$fit, lower = p$fit - 1.96 * p$se.fit, upper = p$fit + 1.96 * p$se.fit,
      physically_possible = R <= 100 * nd$area_km2)
  }
  cat(label, provenance, '\n'); flush.console()
}
pt <- do.call(rbind, profiles)
rs <- do.call(rbind, lapply(names(cell_values), function(label) {
  z <- pt[pt$response == label, ]; small <- z[abs(z$area_quantile - .1) < 1e-8, ]
  large <- z[abs(z$area_quantile - .9) < 1e-8, ]; crosses <- which(head(z$difference, -1) < 0 & tail(z$difference, -1) > 0)
  data.frame(response = label, primary_fixed_pool = grepl('FIXED684', label),
    small_difference = small$difference, small_lower = small$lower, small_upper = small$upper,
    large_difference = large$difference, large_lower = large$lower, large_upper = large$upper,
    small_negative_large_positive = small$difference < 0 && large$difference > 0,
    both_pointwise_intervals_support_reversal = small$upper < 0 && large$lower > 0,
    negative_to_positive_on_scan = length(crosses) > 0,
    first_crossing_lower_city_km2 = if (length(crosses)) z$city_area_km2[crosses[1]] else NA_real_,
    first_crossing_upper_city_km2 = if (length(crosses)) z$city_area_km2[crosses[1] + 1L] else NA_real_)
}))
write_table(do.call(rbind, summaries), 'fixed_pool_model_summary.csv')
write_table(pt, 'fixed_pool_original_range_profiles.csv'); write_table(rs, 'fixed_pool_reversal_summary.csv')
write_table(do.call(rbind, curves), 'fixed_pool_conditional_curves.csv')
stopifnot(identical(vapply(paths, hash, ''), before))
script <- sub('^--file=', '', grep('^--file=', commandArgs(FALSE), value = TRUE)); sources <- c(paths, script)
write_table(data.frame(path = sources, sha256 = vapply(sources, hash, '')), 'input_checksums.csv')
write_json(list(status = 'COMPLETED_FIXED684_ENV_COMPARISON', fixed_cities = 83L,
  fixed_species_pool = 684L, sdm_fits = 0L, new_gam_fits = 3L, reused_gam_models = 1L,
  maximum_direct_prediction_error = maximum_prediction_error, city_weights_identical = TRUE,
  interpretation = 'Missing-as-zero fixed-pool sensitivity; numerical zero contributions do not establish biological absence'),
  file.path(out, 'fixed_pool_receipt.json'), pretty = TRUE, auto_unbox = TRUE)
print(rs)
