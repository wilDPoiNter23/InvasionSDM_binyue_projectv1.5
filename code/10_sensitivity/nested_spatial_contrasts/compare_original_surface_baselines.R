#!/usr/bin/env Rscript
# Compare original response surfaces using saved urban zones and a common ring.
suppressPackageStartupMessages({
  library(terra); library(sf); library(jsonlite); library(digest)
  library(ggplot2); library(patchwork); library(svglite); library(ragg)
})
a <- commandArgs(TRUE)
stopifnot(length(a) == 1L)
cfg <- fromJSON(a[1]); out <- cfg$output_root
if (file.exists(file.path(out, 'receipt.json')))
  stop('Completed results retained; use a fresh output_root for another comparison.')
for (d in c("tables", "cells", "figures", "logs"))
  dir.create(file.path(out, d), recursive = TRUE, showWarnings = FALSE)
hash_file <- function(p) {
  digest(file = p, algo = "sha256")
}
script <- sub('^--file=', '', grep('^--file=', commandArgs(FALSE), value = TRUE))
paths <- c(a[1], script, cfg$support, unlist(cfg$surfaces), cfg$cities,
  sub("[.]shp$", ".dbf", cfg$cities), sub("[.]shp$", ".prj", cfg$cities),
  sub("[.]shp$", ".shx", cfg$cities))
hashes <- vapply(paths, hash_file, "")
contract <- list(ring_metres = cfg$ring_metres, input_paths = normalizePath(paths, winslash = '/', mustWork = TRUE),
  input_hashes = unname(hashes))
for (checkpoint in list.files(file.path(out, 'cells'), pattern = '^outside_[0-9]+[.]rds$', full.names = TRUE)) {
  saved <- readRDS(checkpoint)
  if (!identical(saved$cache_contract, contract))
    stop('Incompatible outside-cell cache: ', checkpoint, '. Use a fresh output_root; existing caches are retained.')
}
write.csv(data.frame(path = paths, sha256 = hashes),
          file.path(out, "tables", "input_checksums.csv"), row.names = FALSE)
support <- readRDS(cfg$support)
w <- support$weights[support$weights$footprint == "HISTORICAL", ]
stopifnot(all(w$zone %in% c("RECLAIMED", "OTHER_URBAN")),
          all(w$weight_km2 >= 0), !anyDuplicated(w[, c("city_id", "zone", "cell")]))
rs <- lapply(cfg$surfaces, rast); names(rs) <- names(cfg$surfaces)
stopifnot(all(vapply(rs, function(r) compareGeom(rs[[1]], r, stopOnError = FALSE), TRUE)),
  max(abs(xyFromCell(rs[[1]], support$cells) - support$xy)) < 1e-8)
r <- rast(rs); names(r) <- names(rs)
sf_use_s2(FALSE)
cities <- st_read(cfg$cities, quiet = TRUE)
stopifnot(!anyDuplicated(tolower(cities$shapeName)))
cities <- st_make_valid(st_transform(cities, 6933))
all_native <- st_set_crs(st_transform(st_geometry(cities), crs(r)), NA)
all_union <- st_union(all_native)
ids <- sort(unique(w$city_id))
zone_rows <- status <- list()
source_values <- lapply(rs, function(z) extract(z, support$cells)[[1]])
for (product in names(rs)) {
  value <- source_values[[product]][w$cell_index]
  for (city in ids) for (zone in c("OTHER_URBAN", "RECLAIMED", "CITY")) {
    ix <- which(w$city_id == city & (zone == "CITY" | w$zone == zone))
    ok <- is.finite(value[ix]) & w$weight_km2[ix] > 0
    area <- sum(w$weight_km2[ix][ok])
    zone_rows[[length(zone_rows) + 1L]] <- data.frame(product = product, city = city,
      zone = zone, mean = if (area > 0) weighted.mean(value[ix][ok], w$weight_km2[ix][ok]) else NA_real_,
      finite_area_km2 = area, finite_cells = length(unique(w$cell[ix][ok])))
  }
}
one_column <- rast(nrows = nrow(r), ncols = 1, xmin = xmin(r),
  xmax = xmin(r) + res(r)[1], ymin = ymin(r), ymax = ymax(r), crs = crs(r))
row_area <- as.numeric(values(cellSize(one_column, unit = "km", mask = FALSE)))
for (i in seq_along(ids)) {
  city <- ids[i]; j <- match(tolower(city), tolower(cities$shapeName))
  checkpoint <- file.path(out, "cells", sprintf("outside_%03d.rds", i))
  answer <- tryCatch({
    stopifnot(!is.na(j))
    if (file.exists(checkpoint)) {
      saved <- readRDS(checkpoint)
      stopifnot(saved$city == city, identical(saved$cache_contract, contract))
      e <- saved$cells
    } else {
      buffer <- st_buffer(st_geometry(cities[j, ]), cfg$ring_metres)
      buffer <- st_set_crs(st_transform(buffer, crs(r)), NA)
      ring <- suppressWarnings(st_difference(buffer, all_union))
      ring <- suppressWarnings(st_collection_extract(st_make_valid(ring), "POLYGON"))
      stopifnot(length(ring) > 0, !all(st_is_empty(ring)))
      ring <- st_sf(geometry = st_set_crs(st_union(ring), crs(r)))
      e <- extract(r, vect(ring), exact = TRUE, cells = TRUE)
      fraction <- intersect(c("fraction", "weight"), names(e))
      stopifnot(length(fraction) == 1)
      e$fraction <- e[[fraction]]
      # Recalculate exceptional cell fractions by planar polygon intersection.
      exceptional <- which(is.finite(e$fraction) & (e$fraction < 0 | e$fraction > 1))
      for (k in exceptional) {
        xy <- xyFromCell(r, e$cell[k]); half <- res(r) / 2
        box <- st_sfc(st_polygon(list(rbind(
          xy + c(-half[1], -half[2]), xy + c(half[1], -half[2]),
          xy + c(half[1], half[2]), xy + c(-half[1], half[2]),
          xy + c(-half[1], -half[2])))))
        clipped <- suppressWarnings(st_intersection(st_set_crs(st_geometry(ring), NA), box))
        clipped <- clipped[!st_is_empty(clipped) & st_dimension(clipped) == 2]
        actual <- if (length(clipped)) sum(as.numeric(st_area(clipped))) / prod(res(r)) else 0
        stopifnot(is.finite(actual), actual >= 0, actual <= 1 + 1e-8)
        e$fraction[k] <- min(1, actual)
      }
      stopifnot(all(e$fraction[is.finite(e$fraction)] >= 0),
        all(e$fraction[is.finite(e$fraction)] <= 1 + 1e-8))
      e$area_km2 <- pmin(1, e$fraction) * row_area[rowFromCell(r, e$cell)]
      saveRDS(list(city = city, input_hashes = hashes, cache_contract = contract, cells = e), checkpoint)
    }
    for (product in names(rs)) {
      ok <- is.finite(e[[product]]) & is.finite(e$area_km2) & e$area_km2 > 0
      area <- sum(e$area_km2[ok])
      zone_rows[[length(zone_rows) + 1L]] <- data.frame(product = product, city = city,
        zone = "OUTSIDE", mean = if (area > 0) weighted.mean(e[[product]][ok], e$area_km2[ok]) else NA_real_,
        finite_area_km2 = area, finite_cells = length(unique(e$cell[ok])))
    }
    data.frame(city = city, status = "EXTRACTED", reason = "")
  }, error = function(e) data.frame(city = city, status = "FAILED", reason = conditionMessage(e)))
  status[[i]] <- answer
  cat(i, city, answer$status, answer$reason, "\n"); flush.console()
}
z <- do.call(rbind, zone_rows)
write.csv(z, file.path(out, "tables", "city_zone_means.csv"), row.names = FALSE)
write.csv(do.call(rbind, status), file.path(out, "tables", "outside_extraction_status.csv"), row.names = FALSE)
set.seed(cfg$seed); B <- cfg$bootstrap_replicates
effects <- levels <- list()
contrast_pairs <- list(CITY_vs_OUTSIDE = c("CITY", "OUTSIDE"),
  RECLAIMED_vs_OUTSIDE = c("RECLAIMED", "OUTSIDE"),
  OTHER_URBAN_vs_OUTSIDE = c("OTHER_URBAN", "OUTSIDE"),
  RECLAIMED_vs_OTHER_URBAN = c("RECLAIMED", "OTHER_URBAN"))
for (product in names(rs)) {
  d <- z[z$product == product, c("city", "zone", "mean")]
  d <- reshape(d, idvar = "city", timevar = "zone", direction = "wide")
  names(d) <- sub("^mean[.]", "", names(d))
  for (zone in c("CITY", "RECLAIMED", "OTHER_URBAN", "OUTSIDE"))
    if (!zone %in% names(d)) d[[zone]] <- NA_real_
  complete <- apply(d[, c("CITY", "RECLAIMED", "OTHER_URBAN", "OUTSIDE")], 1,
    function(v) all(is.finite(v))) & is.finite(d$OUTSIDE) & d$OUTSIDE > 0
  for (sample in c("ALL_AVAILABLE_PAIRS", "COMMON_ZONES")) {
    available <- if (sample == "COMMON_ZONES") d[complete, ] else d
    for (label in names(contrast_pairs)) {
      pair <- contrast_pairs[[label]]
      good <- is.finite(available[[pair[1]]]) & is.finite(available[[pair[2]]])
      dd <- available[good, ]; n <- nrow(dd); if (n < 2) next
      x <- dd[[pair[1]]]; y <- dd[[pair[2]]]; delta <- x - y
      boot <- replicate(B, {ii <- sample.int(n, n, replace = TRUE)
        c(mean(delta[ii]), 100 * (mean(x[ii]) / mean(y[ii]) - 1))})
      ci <- apply(boot, 1, quantile, c(.025, .975), names = FALSE)
      effects[[length(effects) + 1L]] <- data.frame(product = product, sample = sample,
        contrast = label, n_cities = n, target_mean = mean(x), reference_mean = mean(y),
        difference = mean(delta), difference_lower = ci[1, 1], difference_upper = ci[2, 1],
        percent = 100 * (mean(x) / mean(y) - 1), percent_lower = ci[1, 2], percent_upper = ci[2, 2],
        paired_t_p = t.test(delta)$p.value,
        paired_wilcoxon_p = suppressWarnings(wilcox.test(delta, exact = FALSE)$p.value))
    }
  }
  dd <- d[complete, ]; n <- nrow(dd); stopifnot(n > 1)
  ix <- replicate(B, sample.int(n, n, replace = TRUE))
  for (zone in c("OUTSIDE", "CITY", "OTHER_URBAN", "RECLAIMED")) {
    v <- dd[[zone]]; boot <- colMeans(matrix(v[ix], nrow = n))
    ci <- quantile(boot, c(.025, .975), names = FALSE)
    levels[[length(levels) + 1L]] <- data.frame(product = product, zone = zone,
      n_cities = n, mean = mean(v), lower = ci[1], upper = ci[2],
      outside_100_index = 100 * mean(v) / mean(dd$OUTSIDE))
  }
  for (label in c("CITY_vs_OUTSIDE", "OTHER_URBAN_vs_OUTSIDE", "RECLAIMED_vs_OUTSIDE", "RECLAIMED_vs_OTHER_URBAN")) {
    pair <- contrast_pairs[[label]]
    v <- 100 * (dd[[pair[1]]] - dd[[pair[2]]]) / dd$OUTSIDE
    boot <- colMeans(matrix(v[ix], nrow = n)); ci <- quantile(boot, c(.025, .975), names = FALSE)
    effects[[length(effects) + 1L]] <- data.frame(product = product,
      sample = "COMMON_OUTSIDE_DENOMINATOR", contrast = label, n_cities = n,
      target_mean = mean(dd[[pair[1]]]), reference_mean = mean(dd[[pair[2]]]),
      difference = mean(dd[[pair[1]]] - dd[[pair[2]]]), difference_lower = NA, difference_upper = NA,
      percent = mean(v), percent_lower = ci[1], percent_upper = ci[2],
      paired_t_p = t.test(v)$p.value,
      paired_wilcoxon_p = suppressWarnings(wilcox.test(v, exact = FALSE)$p.value))
    delta <- dd[[pair[1]]] - dd[[pair[2]]]
    outside_boot <- colMeans(matrix(dd$OUTSIDE[ix], nrow = n))
    delta_boot <- colMeans(matrix(delta[ix], nrow = n))
    percent_boot <- 100 * delta_boot / outside_boot
    ci <- quantile(percent_boot, c(.025, .975), names = FALSE)
    dc <- quantile(delta_boot, c(.025, .975), names = FALSE)
    effects[[length(effects) + 1L]] <- data.frame(product = product,
      sample = "COMMON_OUTSIDE_GROUP_MEAN", contrast = label, n_cities = n,
      target_mean = mean(dd[[pair[1]]]), reference_mean = mean(dd[[pair[2]]]),
      difference = mean(delta), difference_lower = dc[1], difference_upper = dc[2],
      percent = 100 * mean(delta) / mean(dd$OUTSIDE),
      percent_lower = ci[1], percent_upper = ci[2], paired_t_p = t.test(delta)$p.value,
      paired_wilcoxon_p = suppressWarnings(wilcox.test(delta, exact = FALSE)$p.value))
  }
  write.csv(d, file.path(out, "tables", paste0(product, "_city_comparisons.csv")), row.names = FALSE)
}
effects <- do.call(rbind, effects); levels <- do.call(rbind, levels)
write.csv(effects, file.path(out, "tables", "comparison_statistics.csv"), row.names = FALSE)
write.csv(levels, file.path(out, "tables", "common_city_zone_levels.csv"), row.names = FALSE)
primary <- cfg$primary_surface
l <- levels[levels$product == primary, ]
labels <- c(OUTSIDE = "Surroundings", CITY = "Whole city", OTHER_URBAN = "Non-reclaimed city land", RECLAIMED = "Reclaimed city land")
l$zone <- factor(l$zone, levels = rev(names(labels)))
palette <- c(OUTSIDE = "#7B858A", CITY = "#6F4C9B", OTHER_URBAN = "#A388BE", RECLAIMED = "#178F7A")
theme_set(theme_classic(base_family = "Arial", base_size = 8))
p1 <- ggplot(l, aes(mean, zone, colour = zone)) +
  geom_errorbar(aes(xmin = lower, xmax = upper), orientation = "y", width = .12, linewidth = .5) +
  geom_point(size = 2.3) + scale_colour_manual(values = palette, guide = "none") +
  scale_y_discrete(labels = labels) + labs(tag = "a", title = "Same suitability surface",
    subtitle = paste0("Common cities: n = ", unique(l$n_cities)), x = "Mean predicted invasion suitability", y = NULL)
q <- effects[effects$product == primary & effects$sample == "COMMON_OUTSIDE_GROUP_MEAN", ]
cl <- c(CITY_vs_OUTSIDE = "City vs surroundings", OTHER_URBAN_vs_OUTSIDE = "Non-reclaimed vs surroundings",
  RECLAIMED_vs_OUTSIDE = "Reclaimed vs surroundings", RECLAIMED_vs_OTHER_URBAN = "Reclaimed vs non-reclaimed")
q$contrast <- factor(q$contrast, levels = rev(names(cl)))
q$p_label <- ifelse(q$paired_t_p < .001, "P < 0.001", sprintf("P = %.3f", q$paired_t_p))
p2 <- ggplot(q, aes(percent, contrast)) +
  geom_vline(xintercept = 0, colour = "#B4B8BB", linewidth = .35) +
  geom_errorbar(aes(xmin = percent_lower, xmax = percent_upper), orientation = "y", width = .12, linewidth = .5) +
  geom_point(size = 2.1, colour = "#6F4C9B") +
  geom_text(aes(label = p_label), x = Inf, hjust = 1, size = 2.2, colour = "#353A3D") +
  scale_y_discrete(labels = cl) + scale_x_continuous(expand = expansion(mult = c(.1, .48))) +
  labs(tag = "b", title = "One common reference",
    subtitle = "Same cities; all differences divided by mean surrounding suitability",
    x = "Difference relative to surroundings (%)", y = NULL)
fig <- p1 / p2 + plot_layout(heights = c(1, 1)) &
  theme(plot.title = element_text(face = "bold", size = 9),
    plot.subtitle = element_text(size = 7), plot.tag = element_text(face = "bold"),
    axis.text = element_text(colour = "#353A3D", size = 7), plot.margin = margin(7, 10, 6, 6))
stem <- file.path(out, "figures", "original_surface_common_baseline")
svglite(paste0(stem, ".svg"), width = 183 / 25.4, height = 150 / 25.4); print(fig); dev.off()
cairo_pdf(paste0(stem, ".pdf"), width = 183 / 25.4, height = 150 / 25.4, family = "Arial"); print(fig); dev.off()
agg_png(paste0(stem, ".png"), width = 183, height = 150, units = "mm", res = 300); print(fig); dev.off()
stopifnot(identical(hashes, vapply(paths, hash_file, "")))
failed <- sum(vapply(status, function(x) x$status == 'FAILED', logical(1)))
write_json(list(status = if (failed) 'COMPLETE_WITH_FAILED_OUTSIDE_EXTRACTIONS' else 'COMPLETE',
  failed_outside_extractions = failed, input_paths = paths, input_sha256 = as.list(hashes),
  inputs_unchanged = TRUE, sdm_refits = 0, surface_reaggregation = FALSE,
  native_grid_resampling = FALSE, ring_metres = cfg$ring_metres,
  city_zones = "Saved historical native-grid fractions and geodesic area weights, without new species restrictions",
  outside = "50 km EPSG6933 buffer minus all city polygons, exact native raster cell fractions",
  land_support = "Finite values on the original surface; no new shoreline or native-range mask",
  missing_values = "Excluded, not replaced by zero; existing finite zero values retained",
  common_baseline = "Displayed contrasts share the mean surrounding suitability of the same cities; city-specific normalization is also retained in the tables",
  samples = "All available pairs retained separately; common-zone subset used for the common-reference figure",
  bootstrap = B, seed = cfg$seed, primary_surface = primary),
  file.path(out, "receipt.json"), pretty = TRUE, auto_unbox = TRUE)
print(effects[effects$product == primary, c("sample", "contrast", "n_cities", "percent", "percent_lower", "percent_upper", "paired_t_p")])
message("Saved comparisons: ", out)
