# Check local absolute-area slopes and joint prediction support in saved GAMs.
suppressPackageStartupMessages({library(mgcv); library(jsonlite); library(digest)})
cfg <- fromJSON(commandArgs(TRUE)[1]); out <- cfg$output_root
stopifnot(!dir.exists(out)); dir.create(out, recursive = TRUE)
write_table <- function(x, name) write.csv(x, file.path(out, name), row.names = FALSE)
hash <- function(path) {
  con <- file(path, 'rb'); on.exit(close(con))
  digest(readBin(con, 'raw', n = file.info(path, extra_cols = FALSE)$size), algo = 'sha256', serialize = FALSE)
}
d <- read.csv(file.path(cfg$model_root, 'complete_city_input.csv'))
reference <- read.csv(cfg$reference_city_input)
reference <- reference[!tolower(reference$city) %in% cfg$excluded_city_keys, ]
d$log_recl <- log1p(d$reclamation_area)
stopifnot(nrow(d) == 83L, nrow(reference) == 107L)
large_cutoff <- as.numeric(quantile(reference$area_km2, .9))
range_ha <- range(reference$reclamation_area)
h <- 1e-4
observed <- data.frame(city = d$city, log_recl = d$log_recl, area_km2 = d$area_km2)
grid <- expand.grid(log_recl = seq(log1p(range_ha[1]), log1p(range_ha[2]), length.out = 61),
  area_km2 = sort(unique(d$area_km2)))
grid$city <- NA_character_
points <- rbind(transform(observed, kind = 'OBSERVED_CITY'), transform(grid[, names(observed)], kind = 'COMMON_GRID'))
coordinates <- cbind(d$log_recl, d$area_km2)
centre <- colMeans(coordinates); scales <- apply(coordinates, 2, sd)
standardize <- function(x) sweep(sweep(as.matrix(x), 2, centre, '-'), 2, scales, '/')
domain <- standardize(coordinates)
poly <- domain[chull(domain), , drop = FALSE]
inside <- function(x) {
  z <- standardize(x); cross <- matrix(NA_real_, nrow(z), nrow(poly))
  for (i in seq_len(nrow(poly))) {
    j <- if (i == nrow(poly)) 1L else i + 1L
    cross[, i] <- (poly[j, 1] - poly[i, 1]) * (z[, 2] - poly[i, 2]) -
      (poly[j, 2] - poly[i, 2]) * (z[, 1] - poly[i, 1])
  }
  apply(cross >= -1e-10, 1, all) | apply(cross <= 1e-10, 1, all)
}
xy <- cbind(points$log_recl, points$area_km2)
xp <- xy; xm <- xy; xp[, 1] <- xp[, 1] + h; xm[, 1] <- xm[, 1] - h
points$inside_observed_hull <- inside(xy)
points$two_sided_hull_support <- inside(xp) & inside(xm)
points$physically_possible <- expm1(points$log_recl) <= points$area_km2 * 100
points$large_city <- points$area_km2 >= large_cutoff
write_json(list(status = 'DECLARED_BEFORE_SLOPE_CHECK', models = c('FULL_814', 'PAIRED_FULL_684', 'PAIRED_ENV_684'),
  method = 'Central derivative of the same saved absolute-area G0 models with respect to log1p reclamation hectares',
  derivative_step = h, cities = 83L, grid_points = nrow(grid), large_city_cutoff_km2 = large_cutoff,
  response_and_model_changes = 'None', support = 'Convex hull in fitted log-reclamation and raw city-area coordinates, with both derivative endpoints inside',
  uncertainty = 'Pointwise 95 percent intervals plus simulated simultaneous bands over the supported grid',
  simulations = cfg$simulations, seed = cfg$seed,
  interpretation = 'Exploratory local slopes; distinct from reproducing the original full-range endpoint reversal'),
  file.path(out, 'declared_slope_check.json'), pretty = TRUE, auto_unbox = TRUE)
models <- c(FULL_814 = 'ARCHIVED_GAM_RISK_G0.rds', PAIRED_FULL_684 = 'PAIRED_FULL_G0.rds', PAIRED_ENV_684 = 'PAIRED_ENV_G0.rds')
sources <- c(file.path(cfg$model_root, 'complete_city_input.csv'), cfg$reference_city_input)
results <- summaries <- list(); maximum_step_error <- 0
for (label in names(models)) {
  path <- file.path(cfg$model_root, models[[label]]); sources <- c(sources, path)
  m <- readRDS(path); stopifnot(nobs(m) == 83L)
  nd <- data.frame(log_recl = points$log_recl, area_km2 = points$area_km2,
    anthro_pc1 = median(reference$anthro_pc1, na.rm = TRUE), climate_pc1 = median(reference$climate_pc1, na.rm = TRUE),
    soil_pc1 = median(reference$soil_pc1, na.rm = TRUE), marine_pc1 = median(reference$marine_pc1, na.rm = TRUE))
  derivative_matrix <- function(step) {
    plus <- minus <- nd; plus$log_recl <- plus$log_recl + step; minus$log_recl <- minus$log_recl - step
    (predict(m, plus, type = 'lpmatrix') - predict(m, minus, type = 'lpmatrix')) / (2 * step)
  }
  D <- derivative_matrix(h); slope <- drop(D %*% coef(m))
  check <- drop(derivative_matrix(h / 2) %*% coef(m)); err <- max(abs(check - slope))
  stopifnot(err < 1e-6); maximum_step_error <- max(maximum_step_error, err)
  se <- sqrt(pmax(0, rowSums((D %*% m$Vp) * D)))
  supported <- points$two_sided_hull_support & points$physically_possible
  eligible <- supported & points$kind == 'COMMON_GRID' & se > 1e-12
  eig <- eigen(m$Vp, symmetric = TRUE)
  stopifnot(min(eig$values) > -1e-8)
  L <- sweep(eig$vectors, 2, sqrt(pmax(eig$values, 0)), '*')
  set.seed(cfg$seed)
  draws <- L %*% matrix(rnorm(ncol(D) * cfg$simulations), nrow = ncol(D))
  Z <- sweep(D[eligible, , drop = FALSE] %*% draws, 1, se[eligible], '/')
  critical <- as.numeric(quantile(apply(abs(Z), 2, max), .95))
  z <- data.frame(response = label, points, reclamation_km2 = expm1(points$log_recl) / 100,
    slope = slope, lower = slope - 1.96 * se, upper = slope + 1.96 * se,
    simultaneous_lower = slope - critical * se, simultaneous_upper = slope + critical * se)
  obs <- z$kind == 'OBSERVED_CITY'; gr <- z$kind == 'COMMON_GRID'; lg <- z$large_city
  positive <- z$slope > 0
  summaries[[label]] <- data.frame(response = label, cities = sum(obs),
    observed_positive_slopes = sum(obs & positive), observed_large_positive_slopes = sum(obs & lg & positive),
    observed_large_positive_pointwise = sum(obs & lg & z$lower > 0),
    observed_large_positive_supported_pointwise = sum(obs & lg & supported & z$lower > 0),
    supported_large_grid_points = sum(gr & lg & supported),
    supported_large_positive_grid_points = sum(gr & lg & supported & positive),
    supported_large_positive_pointwise = sum(gr & lg & supported & z$lower > 0),
    supported_large_positive_simultaneous = sum(gr & lg & supported & z$simultaneous_lower > 0),
    unsupported_large_positive_grid_points = sum(gr & lg & !supported & positive),
    simultaneous_critical = critical)
  results[[label]] <- z
  cat('CHECKED', label, '\n'); flush.console()
}
all <- do.call(rbind, results); sm <- do.call(rbind, summaries)
write_table(all[all$kind == 'OBSERVED_CITY', ], 'all_observed_city_local_slopes.csv')
write_table(all[all$kind == 'COMMON_GRID', ], 'all_grid_local_slopes.csv')
write_table(sm, 'local_slope_summary.csv')
write_table(all[all$kind == 'OBSERVED_CITY' & all$large_city, ], 'all_large_city_local_slopes.csv')
script <- sub('^--file=', '', grep('^--file=', commandArgs(FALSE), value = TRUE))
sources <- c(sources, script)
write_table(data.frame(path = sources, sha256 = vapply(sources, hash, '')), 'input_checksums.csv')
write_json(list(status = 'COMPLETED_LOCAL_SLOPE_CHECKS', no_model_fits = TRUE, fixed_cities = 83L,
  maximum_derivative_step_error = maximum_step_error,
  all_observed_cities_retained = all(vapply(results, function(z) sum(z$kind == 'OBSERVED_CITY') == 83L, logical(1)))),
  file.path(out, 'local_slope_receipt.json'), auto_unbox = TRUE, pretty = TRUE)
print(sm)
