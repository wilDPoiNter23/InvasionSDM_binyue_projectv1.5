# Compare a declared set of absolute-reclamation interaction models.
suppressPackageStartupMessages({library(mgcv); library(jsonlite); library(digest)})
args <- commandArgs(TRUE)
stopifnot(length(args) == 1L)
cfg <- fromJSON(args[1])
stopifnot(!dir.exists(cfg$output_root))
dir.create(cfg$output_root, recursive = TRUE)
out <- cfg$output_root
write_table <- function(x, name) write.csv(x, file.path(out, name), row.names = FALSE)
hash <- function(path) {
  con <- file(path, 'rb'); on.exit(close(con))
  digest(readBin(con, 'raw', n = file.info(path, extra_cols = FALSE)$size),
    algo = 'sha256', serialize = FALSE)
}
d <- read.csv(file.path(cfg$current_model_root, 'complete_city_input.csv'))
paired <- read.csv(cfg$paired_city_input)
reference <- read.csv(cfg$reference_city_input)
reference <- reference[!tolower(reference$city) %in% cfg$excluded_city_keys, ]
stopifnot(nrow(d) == 83L, !anyDuplicated(d$city), nrow(reference) == 107L)
ix <- match(tolower(d$city), tolower(paired$city)); stopifnot(!anyNA(ix))
d$log_recl <- log1p(d$reclamation_area)
d$log_area <- log1p(d$area_km2)
reference$log_recl <- log1p(reference$reclamation_area)
reference$log_area <- log1p(reference$area_km2)
frames <- list(FULL_814 = d, PAIRED_FULL_684 = d, PAIRED_ENV_684 = d)
frames$PAIRED_FULL_684$risk <- paired$PAIRED_FULL_684[ix]
frames$PAIRED_ENV_684$risk <- paired$PAIRED_ENV_684[ix]
stopifnot(all(vapply(frames, function(x) all(is.finite(x$risk)), logical(1))))
covariates <- 's(anthro_pc1,k=5)+s(climate_pc1,k=5)+s(soil_pc1,k=5)+s(marine_pc1,k=5)'
rhs <- c(STORED_RAW_AREA_TENSOR = 'te(log_recl,area_km2,k=c(5,5))',
  STORED_ADDITIVE = 's(log_recl,k=5)+s(log_area,k=5)',
  LOG_AREA_TENSOR = 'te(log_recl,log_area,k=c(5,5))',
  LOG_AREA_SEPARATE_INTERACTION = 's(log_recl,k=5)+s(log_area,k=5)+ti(log_recl,log_area,k=c(5,5))',
  LOG_AREA_LINEAR_INTERACTION = 'log_recl*log_area')
forms <- lapply(rhs, function(x) as.formula(paste('risk~', x, '+', covariates)))
area_quantiles <- seq(.05, .95, .05)
areas <- as.numeric(quantile(reference$area_km2, area_quantiles))
ranges <- list(COMMON_ABSOLUTE_IQR = as.numeric(quantile(d$reclamation_area, c(.25, .75))),
  ORIGINAL_FULL_RANGE = range(reference$reclamation_area))
stopifnot(abs(ranges$ORIGINAL_FULL_RANGE[1] / 100 - .004) < 1e-10,
  abs(ranges$ORIGINAL_FULL_RANGE[2] / 100 - 349.78) < 1e-10)
write_json(list(status = 'DECLARED_BEFORE_NEW_FITS', seed = cfg$seed,
  city_count = nrow(d), responses = names(frames), formulas = vapply(forms, deparse1, ''),
  fixed_covariates = covariates, method = 'REML', family = 'gaussian identity',
  new_models = names(forms)[3:5], reused_models = names(forms)[1:2],
  primary_range_hectares = ranges$COMMON_ABSOLUTE_IQR,
  primary_small_city_quantile = .1, primary_large_city_quantile = .9,
  secondary_range_hectares = ranges$ORIGINAL_FULL_RANGE,
  primary_definition = 'Negative small-city and positive large-city high-minus-low differences over identical absolute reclamation endpoints',
  intervals = 'Pointwise 95 percent intervals using stored coefficient covariance',
  reporting = 'Retain all model-response combinations; no outcome-driven model additions or exclusions'),
  file.path(out, 'declared_comparison_design.json'), auto_unbox = TRUE, pretty = TRUE)
write_table(d, 'fixed_city_input.csv')
models <- list(); summaries <- terms <- contrasts <- curves <- support <- list()
maximum_prediction_error <- 0
stored_names <- c(FULL_814 = 'ARCHIVED_GAM_RISK', PAIRED_FULL_684 = 'PAIRED_FULL', PAIRED_ENV_684 = 'PAIRED_ENV')
sources <- c(args[1], cfg$paired_city_input, cfg$reference_city_input,
  file.path(cfg$current_model_root, 'complete_city_input.csv'))
for (response in names(frames)) for (model in names(forms)) {
  frame <- frames[[response]]
  if (grepl('^STORED_', model)) {
    suffix <- if (model == 'STORED_RAW_AREA_TENSOR') 'G0' else 'G1'
    path <- file.path(cfg$current_model_root, paste0(stored_names[[response]], '_', suffix, '.rds'))
    m <- readRDS(path); sources <- c(sources, path)
    stopifnot(nrow(m$model) == 83L, max(abs(m$model$risk - frame$risk)) < 1e-12)
    for (field in setdiff(names(m$model), 'risk')) stopifnot(max(abs(m$model[[field]] - frame[[field]])) < 1e-12)
    provenance <- 'REUSED'
  } else {
    set.seed(cfg$seed)
    m <- gam(forms[[model]], data = frame, method = 'REML')
    stopifnot(m$converged)
    saveRDS(m, file.path(out, paste0(response, '_', model, '.rds')))
    provenance <- 'NEW_DECLARED_COMPARISON'
  }
  label <- paste(response, model, sep = '_'); models[[label]] <- m
  sm <- summary(m); st <- sm$s.table
  interaction_p <- NA_real_
  if (model == 'LOG_AREA_SEPARATE_INTERACTION') interaction_p <- st[grep('^ti\\(', rownames(st)), 4]
  if (model == 'LOG_AREA_LINEAR_INTERACTION') interaction_p <- sm$p.table['log_recl:log_area', 4]
  summaries[[label]] <- data.frame(response = response, model = model, provenance = provenance,
    n = nobs(m), adjusted_r2 = sm$r.sq, AIC = AIC(m), total_edf = sum(m$edf), interaction_p = interaction_p)
  terms[[label]] <- data.frame(response = response, model = model, term = rownames(st),
    edf = st[, 1], statistic = st[, 3], p = st[, 4])
  base <- data.frame(log_recl = median(reference$log_recl), area_km2 = median(reference$area_km2),
    log_area = median(reference$log_area), anthro_pc1 = median(reference$anthro_pc1, na.rm = TRUE),
    climate_pc1 = median(reference$climate_pc1, na.rm = TRUE), soil_pc1 = median(reference$soil_pc1, na.rm = TRUE),
    marine_pc1 = median(reference$marine_pc1, na.rm = TRUE))
  for (mode in names(ranges)) for (j in seq_along(areas)) {
    R <- ranges[[mode]]; nd <- base[rep(1, 2), ]
    nd$log_recl <- log1p(R); nd$area_km2 <- areas[j]; nd$log_area <- log1p(areas[j])
    X <- predict(m, nd, type = 'lpmatrix'); delta <- X[2, ] - X[1, ]
    effect <- sum(delta * coef(m)); se <- sqrt(drop(delta %*% m$Vp %*% delta))
    err <- abs(effect - diff(predict(m, nd, type = 'response')))
    stopifnot(err < 1e-10); maximum_prediction_error <- max(maximum_prediction_error, err)
    near <- frame[order(abs(frame$log_area - log1p(areas[j])))[1:20], ]
    support[[paste(label, mode, j)]] <- data.frame(response = response, model = model, mode = mode,
      area_quantile = area_quantiles[j], city_area_km2 = areas[j],
      neighbours = nrow(near), observed_neighbour_min_reclamation_km2 = min(near$reclamation_area) / 100,
      observed_neighbour_max_reclamation_km2 = max(near$reclamation_area) / 100,
      within_neighbour_range = min(R) >= min(near$reclamation_area) && max(R) <= max(near$reclamation_area))
    contrasts[[paste(label, mode, j)]] <- data.frame(response = response, model = model, mode = mode,
      area_quantile = area_quantiles[j], city_area_km2 = areas[j],
      lower_reclamation_km2 = R[1] / 100, upper_reclamation_km2 = R[2] / 100,
      physically_possible = R[2] <= 100 * areas[j], difference = effect,
      lower = effect - 1.96 * se, upper = effect + 1.96 * se)
  }
  for (q in c(.1, .5, .9)) {
    area <- as.numeric(quantile(reference$area_km2, q))
    R <- expm1(seq(log1p(min(reference$reclamation_area)), log1p(max(reference$reclamation_area)), length.out = 201))
    nd <- base[rep(1, length(R)), ]; nd$log_recl <- log1p(R); nd$area_km2 <- area; nd$log_area <- log1p(area)
    pr <- predict(m, nd, se.fit = TRUE)
    curves[[paste(label, q)]] <- data.frame(response = response, model = model, area_quantile = q,
      city_area_km2 = area, reclamation_km2 = R / 100, fitted = pr$fit,
      lower = pr$fit - 1.96 * pr$se.fit, upper = pr$fit + 1.96 * pr$se.fit,
      physically_possible = R <= 100 * area)
  }
  cat(response, model, provenance, '\n'); flush.console()
}
ct <- do.call(rbind, contrasts); ms <- do.call(rbind, summaries)
groups <- split(ct, interaction(ct$response, ct$model, ct$mode, drop = TRUE))
reversals <- do.call(rbind, lapply(groups, function(z) {
  small <- z[abs(z$area_quantile - .1) < 1e-8, ]; large <- z[abs(z$area_quantile - .9) < 1e-8, ]
  cross <- which(head(z$difference, -1) < 0 & tail(z$difference, -1) > 0)
  if (z$model[1] == 'STORED_ADDITIVE') stopifnot(diff(range(z$difference)) < 1e-10)
  data.frame(response = z$response[1], model = z$model[1], mode = z$mode[1],
    small_city_km2 = small$city_area_km2, large_city_km2 = large$city_area_km2,
    small_difference = small$difference, small_lower = small$lower, small_upper = small$upper,
    large_difference = large$difference, large_lower = large$lower, large_upper = large$upper,
    small_negative_large_positive = small$difference < 0 && large$difference > 0,
    both_pointwise_intervals_exclude_zero = small$upper < 0 && large$lower > 0,
    physical_at_both_sizes = small$physically_possible && large$physically_possible,
    sign_crossings = length(cross), first_crossing_lower_city_km2 = if (length(cross)) z$city_area_km2[cross[1]] else NA_real_,
    first_crossing_upper_city_km2 = if (length(cross)) z$city_area_km2[cross[1] + 1L] else NA_real_)
}))
write_table(ms, 'all_model_results.csv'); write_table(do.call(rbind, terms), 'all_smooth_terms.csv')
write_table(ct, 'all_absolute_area_contrasts.csv'); write_table(reversals, 'all_reversal_results.csv')
write_table(do.call(rbind, curves), 'all_conditional_curves.csv')
write_table(do.call(rbind, support), 'all_comparison_support.csv')
script_path <- sub('^--file=', '', grep('^--file=', commandArgs(FALSE), value = TRUE))
sources <- unique(c(sources, script_path))
write_table(data.frame(path = sources, sha256 = vapply(sources, hash, '')), 'input_checksums.csv')
write_json(list(status = 'COMPLETED_NUMERICAL_CHECKS', models = nrow(ms), reused_models = 6L,
  new_comparison_models = 9L, sdm_fits = 0L, fixed_cities = nrow(d),
  maximum_direct_prediction_error = maximum_prediction_error,
  no_proportion_predictor = TRUE, all_declared_models_retained = TRUE,
  scientific_scope = 'Exploratory specification comparisons; pointwise curve reversal is distinct from a supported interaction or independent validation'),
  file.path(out, 'comparison_receipt.json'), auto_unbox = TRUE, pretty = TRUE)
print(reversals[, c('response', 'model', 'mode', 'small_negative_large_positive', 'both_pointwise_intervals_exclude_zero')])
