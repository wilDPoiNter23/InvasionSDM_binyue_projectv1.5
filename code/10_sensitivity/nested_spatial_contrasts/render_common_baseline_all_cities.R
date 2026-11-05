#!/usr/bin/env Rscript
# Extend the saved original-surface comparison to all mapped cities.
suppressPackageStartupMessages({
  library(sf); library(terra); library(ggplot2); library(patchwork)
  library(svglite); library(ragg); library(jsonlite); library(digest)
})
a <- commandArgs(TRUE)
stopifnot(length(a) == 4L)
out <- a[1]; raster_file <- a[2]; cities_file <- a[3]; reclamation_file <- a[4]
targets <- c('tables/all_108_city_comparisons.csv', 'tables/all_108_city_statistics.csv',
  'cells/additional_city_original_surface.rds', 'all_108_figure_receipt.json',
  paste0('figures/original_surface_all_cities.', c('svg', 'pdf', 'png')))
if (any(file.exists(file.path(out, targets))))
  stop('Existing all-city exports retained; review the saved results before another export.')
key <- function(x) tolower(trimws(x))
hash_file <- function(p) {
  con <- file(p, "rb"); on.exit(close(con))
  digest(readBin(con, "raw", n = file.info(p, extra_cols = FALSE)$size),
         algo = "sha256", serialize = FALSE)
}
input <- c(file.path(out, "tables", "ORIGINAL_MANUSCRIPT_city_comparisons.csv"),
           raster_file, cities_file, sub("[.]shp$", ".dbf", cities_file),
           sub("[.]shp$", ".prj", cities_file), reclamation_file,
           sub("[.]shp$", ".dbf", reclamation_file), sub("[.]shp$", ".prj", reclamation_file))
hashes <- vapply(input, hash_file, "")
stopifnot(hashes[2] == "e4f9011ac720471135e7f73f245558cc62522291ca664700180ebf4310e31680")
sf_use_s2(FALSE)
cities <- st_make_valid(st_transform(st_read(cities_file, quiet = TRUE), 6933))
stopifnot(nrow(cities) == 108L, !anyDuplicated(key(cities$shapeName)))
saved <- read.csv(input[1], stringsAsFactors = FALSE)
missing <- which(!key(cities$shapeName) %in% key(saved$city))
stopifnot(length(missing) == 1L, nrow(saved) == 107L)
city <- cities[missing, ]; missing_name <- city$shapeName
reclamation <- st_read(reclamation_file, quiet = TRUE)
reclamation <- reclamation[key(reclamation$shapeName) == key(missing_name), ]
stopifnot(nrow(reclamation) > 0)
reclamation <- st_make_valid(st_transform(reclamation, 6933))
reclaimed <- suppressWarnings(st_intersection(st_geometry(city), st_union(st_geometry(reclamation))))
stopifnot(length(reclaimed) > 0, !all(st_is_empty(reclaimed)))
other <- suppressWarnings(st_difference(st_geometry(city), reclaimed))
stopifnot(!all(st_is_empty(other)))
r <- rast(raster_file)
all_native <- st_set_crs(st_transform(st_geometry(cities), crs(r)), NA)
buffer <- st_set_crs(st_transform(st_buffer(st_geometry(city), 50000), crs(r)), NA)
ring <- suppressWarnings(st_difference(buffer, st_union(all_native)))
ring <- suppressWarnings(st_collection_extract(st_make_valid(ring), "POLYGON"))
stopifnot(length(ring) > 0, !all(st_is_empty(ring)))
native <- function(x) st_set_crs(st_transform(x, crs(r)), NA)
zones <- list(CITY = native(st_geometry(city)), RECLAIMED = native(reclaimed),
              OTHER_URBAN = native(other), OUTSIDE = st_union(ring))
col_raster <- rast(nrows = nrow(r), ncols = 1, xmin = xmin(r),
  xmax = xmin(r) + res(r)[1], ymin = ymin(r), ymax = ymax(r), crs = crs(r))
row_area <- as.numeric(values(cellSize(col_raster, unit = "km", mask = FALSE)))
extract_zone <- function(geom, zone) {
  polygon <- st_sf(geometry = st_set_crs(st_union(geom), crs(r)))
  e <- extract(r, vect(polygon), exact = TRUE, cells = TRUE)
  frac <- intersect(c("fraction", "weight"), names(e)); stopifnot(length(frac) == 1L)
  e$fraction <- e[[frac]]
  for (i in which(is.finite(e$fraction) & (e$fraction < 0 | e$fraction > 1))) {
    xy <- xyFromCell(r, e$cell[i]); half <- res(r) / 2
    box <- st_sfc(st_polygon(list(rbind(xy + c(-half[1], -half[2]),
      xy + c(half[1], -half[2]), xy + c(half[1], half[2]),
      xy + c(-half[1], half[2]), xy + c(-half[1], -half[2])))))
    v <- suppressWarnings(st_intersection(st_set_crs(st_geometry(polygon), NA), box))
    v <- v[!st_is_empty(v) & st_dimension(v) == 2]
    e$fraction[i] <- if (length(v)) min(1, sum(as.numeric(st_area(v))) / prod(res(r))) else 0
  }
  e$area_km2 <- row_area[rowFromCell(r, e$cell)] * e$fraction
  value_col <- names(r)[1]
  ok <- is.finite(e[[value_col]]) & is.finite(e$area_km2) & e$area_km2 > 0
  if (!any(ok)) stop(paste("No finite original raster values for", zone))
  list(mean = weighted.mean(e[[value_col]][ok], e$area_km2[ok]),
       area = sum(e$area_km2[ok]), cells = e)
}
ex <- lapply(names(zones), function(z) extract_zone(zones[[z]], z)); names(ex) <- names(zones)
new <- data.frame(city = missing_name, OTHER_URBAN = ex$OTHER_URBAN$mean,
  RECLAIMED = ex$RECLAIMED$mean, CITY = ex$CITY$mean, OUTSIDE = ex$OUTSIDE$mean)
stopifnot(abs(ex$CITY$area - ex$OTHER_URBAN$area - ex$RECLAIMED$area) < max(1e-4, ex$CITY$area * 1e-6))
combined <- rbind(saved, new)
stopifnot(nrow(combined) == 108, !anyDuplicated(key(combined$city)))
tables <- file.path(out, "tables"); figures <- file.path(out, "figures")
write.csv(combined, file.path(tables, "all_108_city_comparisons.csv"), row.names = FALSE)
saveRDS(ex, file.path(out, "cells", "additional_city_original_surface.rds"))
complete <- combined[complete.cases(combined[, c("OUTSIDE", "CITY", "RECLAIMED", "OTHER_URBAN")]), ]
whole <- combined[complete.cases(combined[, c("OUTSIDE", "CITY")]), ]
set.seed(20261027); B <- 9999L
summarize_pair <- function(d, target, ref, comparison) {
  x <- d[[target]]; y <- d[[ref]]; n <- nrow(d)
  ix <- replicate(B, sample.int(n, n, replace = TRUE))
  delta <- x - y
  boot <- 100 * (colMeans(matrix(x[ix], nrow = n)) / colMeans(matrix(y[ix], nrow = n)) - 1)
  ci <- quantile(boot, c(.025, .975), names = FALSE)
  data.frame(comparison = comparison, n = n, target_mean = mean(x), reference_mean = mean(y),
    percent = 100 * (mean(x) / mean(y) - 1), lower = ci[1], upper = ci[2],
    p = t.test(delta)$p.value, wilcoxon_p = suppressWarnings(wilcox.test(delta, exact = FALSE)$p.value))
}
stats <- do.call(rbind, list(
  summarize_pair(whole, "CITY", "OUTSIDE", "city_vs_surroundings_all"),
  summarize_pair(complete, "CITY", "OUTSIDE", "city_vs_surroundings_matched"),
  summarize_pair(complete, "RECLAIMED", "OUTSIDE", "reclaimed_vs_surroundings_matched"),
  summarize_pair(complete, "OTHER_URBAN", "OUTSIDE", "other_vs_surroundings_matched"),
  summarize_pair(complete, "RECLAIMED", "OTHER_URBAN", "reclaimed_vs_other_matched")))
write.csv(stats, file.path(tables, "all_108_city_statistics.csv"), row.names = FALSE)
levels <- function(d, cols, panel) {
  vals <- do.call(rbind, lapply(cols, function(z) data.frame(city = d$city, zone = z,
    value = d[[z]], panel = panel)))
  vals$zone <- factor(vals$zone, levels = cols); vals
}
v1 <- levels(complete, c("OUTSIDE", "RECLAIMED", "OTHER_URBAN"), "matched")
v2 <- levels(whole, c("OUTSIDE", "CITY"), "all")
summarize_levels <- function(d) do.call(rbind, lapply(split(d, d$zone), function(x) {
  n <- nrow(x); b <- replicate(B, mean(sample(x$value, n, replace = TRUE)))
  ci <- quantile(b, c(.025, .975), names = FALSE)
  data.frame(zone = x$zone[1], mean = mean(x$value), low = ci[1], high = ci[2])
}))
s1 <- summarize_levels(v1); s2 <- summarize_levels(v2)
col <- c(OUTSIDE = "#8E999B", RECLAIMED = "#168873", OTHER_URBAN = "#8666A4", CITY = "#6F4C9B")
display <- c(OUTSIDE = "Outside\ncity", RECLAIMED = "Reclaimed\nland", OTHER_URBAN = "Other\ncity land", CITY = "Whole\ncity")
theme_set(theme_classic(base_family = "Arial", base_size = 8))
make_panel <- function(raw, sm, title, subtitle, tag) {
  ggplot(raw, aes(zone, value)) +
    geom_line(aes(group = city), colour = "#A8B1B4", alpha = .11, linewidth = .27) +
    geom_point(aes(colour = zone), alpha = .32, size = .8) +
    geom_errorbar(data = sm, aes(y = mean, ymin = low, ymax = high), width = .13, linewidth = .55, colour = "#252A2D") +
    geom_point(data = sm, aes(y = mean, fill = zone), size = 3.1, shape = 21, colour = "white", stroke = .65) +
    geom_text(data = sm, aes(y = mean, label = sprintf("%.2f", mean)),
      nudge_x = .18, nudge_y = .025, size = 2.5, colour = "#252A2D") +
    scale_fill_manual(values = col, guide = "none") + scale_colour_manual(values = col, guide = "none") +
    scale_x_discrete(labels = display) + scale_y_continuous(limits = c(0, .85), breaks = seq(0, .8, .2), expand = expansion(mult = c(0, .02))) +
    labs(tag = tag, title = title, subtitle = subtitle, x = NULL, y = "Predicted invasion suitability") +
    theme(axis.line = element_line(linewidth = .35), axis.ticks = element_line(linewidth = .35),
      axis.text = element_text(size = 7, colour = "#343A3C"), axis.text.x = element_text(lineheight = .95),
      axis.title = element_text(size = 8), plot.title = element_text(size = 9, face = "bold"),
      plot.subtitle = element_text(size = 7, lineheight = 1.1), plot.tag = element_text(size = 10, face = "bold"),
      plot.margin = margin(6, 5, 6, 5))
}
p1 <- make_panel(v1, s1, "Within cities and their surroundings",
  sprintf("Matched cities: n = %d\nReclaimed vs other city land: %s", nrow(complete),
    if (stats$p[stats$comparison == 'reclaimed_vs_other_matched'] < .001) 'P < 0.001' else
      sprintf('P = %.3f', stats$p[stats$comparison == 'reclaimed_vs_other_matched'])), "a")
p2 <- make_panel(v2, s2, "The urban contrast",
  sprintf("All mapped cities: n = %d\nCity vs surroundings: %s", nrow(whole),
    if (stats$p[stats$comparison == 'city_vs_surroundings_all'] < .001) 'P < 0.001' else
      sprintf('P = %.3f', stats$p[stats$comparison == 'city_vs_surroundings_all'])), "b")
fig <- p1 + p2 + plot_layout(widths = c(1.25, 1))
stem <- file.path(figures, "original_surface_all_cities")
svglite(paste0(stem, ".svg"), width = 183 / 25.4, height = 105 / 25.4); print(fig); dev.off()
cairo_pdf(paste0(stem, ".pdf"), width = 183 / 25.4, height = 105 / 25.4, family = "Arial"); print(fig); dev.off()
agg_png(paste0(stem, ".png"), width = 183, height = 105, units = "mm", res = 300); print(fig); dev.off()
stopifnot(identical(hashes, vapply(input, hash_file, "")))
write_json(list(status = "EXPORTED", source_city_name = missing_name,
  input_paths = input, input_sha256 = as.list(hashes), input_unchanged = TRUE,
  total_cities = nrow(combined), all_city_pairs = nrow(whole), common_zone_cities = nrow(complete),
  city_finite_area_km2 = ex$CITY$area, reclaimed_finite_area_km2 = ex$RECLAIMED$area,
  outside_finite_area_km2 = ex$OUTSIDE$area, native_grid_resampled = FALSE,
  model_refits = 0, bootstrap_replicates = B, group_means_are_equally_weighted_by_city = TRUE,
  ci = "95% city bootstrap; no multiple comparison correction",
  p_test = "Two-sided paired t test; no multiple comparison correction",
  city_source_name = "The original boundary field is retained without relabelling"),
  file.path(out, "all_108_figure_receipt.json"), pretty = TRUE, auto_unbox = TRUE)
print(new); print(stats)
message("Figures and results exported: ", out)
