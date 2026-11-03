# Trace city response extraction and separate saved-prediction transformations.
suppressPackageStartupMessages({library(terra); library(jsonlite); library(digest)})
args <- commandArgs(TRUE)
stopifnot(length(args) == 1L)
cfg <- fromJSON(args[1])
out <- cfg$output_root
if (dir.exists(out)) stop('Existing response trace retained; use a fresh output_root.')
dir.create(out, recursive = TRUE, showWarnings = FALSE)
key <- function(x) trimws(gsub('[^a-z0-9]+', ' ', tolower(x)))
hash <- function(p) {
  con <- file(p, 'rb'); on.exit(close(con)); chunks <- list()
  repeat {
    b <- readBin(con, raw(), n = 1024L * 1024L)
    if (!length(b)) break
    chunks[[length(chunks) + 1L]] <- b
  }
  digest(do.call(c, chunks), algo = 'sha256', serialize = FALSE)
}
write_table <- function(x, name) write.csv(x, file.path(out, name), row.names = FALSE)
arch <- read.csv(cfg$original_city_input)
legacy <- read.csv(cfg$legacy_city_input)
target <- read.csv(cfg$current_city_input)
stopifnot(nrow(target) == 83L, !anyDuplicated(key(target$city)))
roster <- key(target$city)
arch_index <- match(roster, key(arch$city))
stopifnot(!anyNA(arch_index))
old <- rast(cfg$original_raster)
polygons <- vect(cfg$current_polygons)
original_polygons <- vect(cfg$original_polygons)
if (any(!is.valid(polygons))) polygons <- makeValid(polygons)
pi <- match(key(arch$city), key(polygons$shapeName))
oi <- match(key(arch$city), key(original_polygons$shapeName))
stopifnot(!anyNA(pi))
centre <- as.numeric(extract(old, polygons[pi, ], fun = mean, na.rm = TRUE)[[2]])
rectangle <- rep(NA_real_, nrow(arch))
for (i in which(!is.na(oi))) {
  rectangle[i] <- as.numeric(global(crop(old, original_polygons[oi[i], ]), 'mean', na.rm = TRUE)[1, 1])
}
updates <- read.csv(cfg$updated_city_comparison)
changed <- key(arch$city) %in% key(updates$city)
reconstructed <- ifelse(changed, centre, rectangle)
li <- match(key(arch$city), key(legacy$city))
lineage <- data.frame(city = arch$city, boundary_update = changed,
  legacy_risk = legacy$risk[li], original_rectangle_mean = rectangle,
  current_polygon_centre_mean = centre, archived_risk = arch$risk,
  reconstructed_risk = reconstructed, difference = reconstructed - arch$risk)
stopifnot(all(is.finite(lineage$difference)), max(abs(lineage$difference)) < 1e-8)
write_table(lineage, 'risk_raster_lineage.csv')
cat('Archived city response reproduced for', nrow(lineage), 'cities\n'); flush.console()

root <- cfg$native_result_root
support <- readRDS(file.path(root, 'analysis_support.rds'))
saved <- readRDS(file.path(root, 'scaled_species_values.rds'))
masks <- readRDS(file.path(root, 'native_cell_membership.rds'))
product <- readRDS(file.path(root, 'non_native_cell_values.rds'))
v <- saved$full
p <- product$products$FULL_814
stopifnot(ncol(v) == 814L, identical(support$cells, masks$cells),
  identical(support$cells, product$cells), nrow(v) == length(support$cells))
meta <- read.csv(file.path(root, 'continuous_input_scales.csv'))
meta <- meta[meta$group == 'FULL_814', ]
stopifnot(identical(meta$species, masks$species))
historical <- as.numeric(extract(old, support$xy)[[1]])
september <- as.numeric(extract(rast(cfg$september_raster), support$xy)[[1]])
valid <- is.finite(p$values$alien_mean)
old_september <- is.finite(september) & valid
restrict <- function(x, use) {x[!use] <- NA_real_; x}
w <- support$weights[support$weights$footprint == 'HISTORICAL', ]
w <- w[key(w$city_id) %in% roster, ]
groups <- lapply(roster, function(k) which(key(w$city_id) == k))
stopifnot(all(lengths(groups) > 0))
aggregate_cells <- function(x) vapply(groups, function(ix) {
  z <- x[w$cell_index[ix]]; ok <- is.finite(z)
  if (any(ok)) weighted.mean(z[ok], w$weight_km2[ix][ok]) else NA_real_
}, numeric(1))

# Fixed-pool sums are contribution diagnostics; missing predictions are not absences.
raw_fixed <- rowSums(v, na.rm = TRUE) / ncol(v)
u <- sweep(sweep(v * 1000, 2, meta$minimum, '-'), 2, meta$maximum - meta$minimum, '/')
constant <- which(is.finite(meta$maximum - meta$minimum) & meta$maximum == meta$minimum)
for (j in constant) u[is.finite(v[, j]), j] <- 0
minmax_fixed <- rowSums(u, na.rm = TRUE) / ncol(v)
unmasked_sum <- rowSums(v, na.rm = TRUE)
masked <- v
masked[!is.finite(masked) | masks$native] <- 0
masked_sum <- rowSums(masked)
stopifnot(max(abs(masked_sum - p$sum)) < 1e-10,
  all(p$counts$native + p$counts$observed + p$counts$missing == 814L),
  max(abs(raw_fixed * 814 - unmasked_sum)) < 1e-10)
scenarios <- list(
  ARCHIVED_MIXED_EXTRACTION = arch$risk[arch_index],
  ORIGINAL_POLYGON_CENTRE = centre[arch_index],
  ORIGINAL_POLYGON_AREA_WEIGHTED = aggregate_cells(historical),
  ORIGINAL_ON_CURRENT_DEFINED_CELLS = aggregate_cells(restrict(historical, valid)),
  CURRENT_MINMAX_FIXED_POOL = aggregate_cells(restrict(minmax_fixed, valid)),
  CURRENT_RAW_SCALE_FIXED_POOL = aggregate_cells(restrict(raw_fixed, valid)),
  NATIVE_REMOVED_FIXED_POOL = aggregate_cells(restrict(p$values$fixed_lower, valid)),
  UNKNOWN_REMOVED_FROM_DENOMINATOR = aggregate_cells(restrict(p$values$coverage_adjusted, valid)),
  NATIVE_REMOVED_FROM_DENOMINATOR = aggregate_cells(p$values$alien_mean))
wide <- data.frame(city = target$city, city_area_km2 = target$area_km2, scenarios,
  stored_updated_risk = target$risk, check.names = FALSE)
stopifnot(max(abs(wide$NATIVE_REMOVED_FROM_DENOMINATOR - target$risk)) < 1e-10)
write_table(wide, 'risk_processing_city_steps.csv')
labels <- c('Rectangle to polygon extraction, retaining the archived five boundary updates',
  'Cell-centre mean to polygon-area weighting',
  'Exclude cells with no defined current alien prediction',
  'Historical raster to current 814-species min-max control; contributor and projection differences remain combined',
  'Per-species min-max to the stored 0-1000 scale divided by 1000',
  'Remove native-range contributions, retaining the 814 denominator',
  'Exclude genuinely missing predictions from the denominator, retaining native zeros',
  'Exclude native species from the denominator')
step_rows <- lapply(2:length(scenarios), function(i) {
  before <- scenarios[[i - 1L]]; after <- scenarios[[i]]; d <- after - before
  data.frame(step = i - 1L, from = names(scenarios)[i - 1L], to = names(scenarios)[i],
    interpretation = labels[i - 1L], cities = length(d),
    before_mean = mean(before), after_mean = mean(after),
    mean_change_points = 100 * mean(d), mean_absolute_city_change_points = 100 * mean(abs(d)),
    minimum_city_change_points = 100 * min(d), maximum_city_change_points = 100 * max(d),
    spearman_rank = cor(before, after, method = 'spearman'))
})
steps <- do.call(rbind, step_rows)
stopifnot(abs(sum(steps$mean_change_points) - 100 * mean(target$risk - arch$risk[arch_index])) < 1e-9)
write_table(steps, 'risk_processing_step_summary.csv')

# Isolate September coverage restoration while holding all cell values fixed.
sep_mean <- aggregate_cells(restrict(p$values$alien_mean, old_september))
sep_stored <- aggregate_cells(september)
coverage <- data.frame(city = target$city, september_stored_mean = sep_stored,
  corrected_values_on_september_coverage = sep_mean,
  corrected_values_on_restored_coverage = target$risk,
  restoration_change_points = 100 * (target$risk - sep_mean))
for (label in c('original_finite', 'september_finite', 'current_defined', 'restored', 'current_undefined')) {
  use <- switch(label, original_finite = is.finite(historical),
    september_finite = is.finite(september), current_defined = valid,
    restored = !is.finite(september) & valid, current_undefined = !valid)
  coverage[[paste0(label, '_area_km2')]] <- vapply(groups, function(ix)
    sum(w$weight_km2[ix][use[w$cell_index[ix]]]), numeric(1))
}
write_table(coverage, 'risk_coverage_city_comparison.csv')
scope <- sort(unique(w$cell_index))
covered <- old_september & seq_along(valid) %in% scope
coverage_summary <- data.frame(cities = length(roster), supported_cells = length(scope),
  original_finite_cells = sum(is.finite(historical[scope])),
  september_finite_cells = sum(is.finite(september[scope])),
  current_defined_cells = sum(valid[scope]),
  september_restored_cells = sum(!is.finite(september[scope]) & valid[scope]),
  current_undefined_cells = sum(!valid[scope]),
  shared_september_maximum_cell_difference = max(abs(september[covered] - p$values$alien_mean[covered])),
  comparable_september_cities = sum(is.finite(coverage$restoration_change_points)),
  mean_restoration_change_points_available_cities = mean(coverage$restoration_change_points, na.rm = TRUE),
  mean_absolute_restoration_change_points_available_cities = mean(abs(coverage$restoration_change_points), na.rm = TRUE))
write_table(coverage_summary, 'risk_coverage_summary.csv')
cell_checks <- do.call(rbind, lapply(list(RAW_FIXED_814 = raw_fixed,
  MINMAX_FIXED_814 = minmax_fixed, MASKED_FIXED_814 = p$values$fixed_lower), function(x) {
    ok <- is.finite(x[scope]) & is.finite(historical[scope]); d <- x[scope][ok] - historical[scope][ok]
    data.frame(cells = sum(ok), maximum_absolute_difference = max(abs(d)),
      mean_absolute_difference = mean(abs(d)))
  }))
cell_checks$control <- rownames(cell_checks); rownames(cell_checks) <- NULL
write_table(cell_checks, 'risk_current_pool_raster_controls.csv')
ranges <- read.csv(cfg$stored_range_comparison)
ranges <- ranges[ranges$mode == 'WHOLE_INPUT_RANGE', ]
stopifnot(nrow(ranges) == 2L, all(abs(ranges$lower_reclamation_km2 - 0.004) < 1e-10),
  all(abs(ranges$upper_reclamation_km2 - 349.78) < 1e-10))
write_table(ranges, 'risk_stored_same_city_same_range_comparison.csv')
script_path <- sub('^--file=', '', grep('^--file=', commandArgs(FALSE), value = TRUE))
sources <- unique(c(script_path, args[1], cfg$original_raster, cfg$original_city_input, cfg$legacy_city_input,
  cfg$current_city_input, cfg$updated_city_comparison, cfg$september_raster,
  cfg$stored_range_comparison, cfg$original_aggregation_script, cfg$original_aggregation_log,
  cfg$original_polygons, cfg$current_polygons,
  file.path(root, c('analysis_support.rds', 'scaled_species_values.rds',
    'native_cell_membership.rds', 'non_native_cell_values.rds', 'continuous_input_scales.csv'))))
write_table(data.frame(path = sources, sha256 = vapply(sources, hash, character(1))), 'risk_trace_input_checksums.csv')
write_json(list(status = 'VERIFIED', no_model_fitting = TRUE, no_bootstrap = TRUE,
  cities = length(roster), original_city_response_maximum_reconstruction_error = max(abs(lineage$difference)),
  updated_response_maximum_reconstruction_error = max(abs(wide$NATIVE_REMOVED_FROM_DENOMINATOR - target$risk)),
  source_raster_sha256 = hash(cfg$original_raster),
  original_raster_not_identical_to_current_pool_controls = TRUE,
  attribution_scope = 'Ordered city-response arithmetic, not attribution of fitted GAM sign changes',
  denominator_scope = 'Fixed-pool controls are contribution diagnostics; missing predictions remain unknown',
  historical_contributor_scope = 'The historical script and interleaved log do not uniquely certify the exact contributing projection files'),
  file.path(out, 'risk_processing_trace_receipt.json'), pretty = TRUE, auto_unbox = TRUE)
cat('Response transformations checked for the same 83 cities; no models fitted.\n')
