# Usage: Rscript render_reclamation_area_sensitivity.R CONFIG.json
# Render absolute-area evidence and response-processing sensitivity from saved fits.
suppressPackageStartupMessages({library(mgcv); library(ggplot2); library(patchwork)
  library(jsonlite); library(svglite); library(ragg); library(digest)})
cfg <- fromJSON(commandArgs(TRUE)[1]); out <- cfg$output_root
stopifnot(dir.exists(out), capabilities('cairo'))
for (name in c('source_data', 'preview')) dir.create(file.path(out, name), showWarnings = FALSE)
hash <- function(path) {
  con <- file(path, 'rb'); on.exit(close(con))
  digest(readBin(con, 'raw', n = file.info(path, extra_cols = FALSE)$size), algo = 'sha256', serialize = FALSE)
}
reference <- read.csv(cfg$reference_city_input)
reference <- reference[!tolower(reference$city) %in% cfg$excluded_city_keys, ]
reference$log_recl <- log1p(reference$reclamation_area)
pred <- read.csv(file.path(cfg$model_root, 'complete_city_input.csv'))
stopifnot(nrow(pred) == 83L)
base <- data.frame(log_recl = median(reference$log_recl), log_area = log1p(median(reference$area_km2)),
  anthro_pc1 = median(reference$anthro_pc1, na.rm = TRUE), climate_pc1 = median(reference$climate_pc1, na.rm = TRUE),
  soil_pc1 = median(reference$soil_pc1, na.rm = TRUE), marine_pc1 = median(reference$marine_pc1, na.rm = TRUE))
range_ha <- range(reference$reclamation_area)
iqr_ha <- as.numeric(quantile(pred$reclamation_area, c(.25, .75)))
sources <- c(cfg$reference_city_input, file.path(cfg$model_root, 'complete_city_input.csv'),
  cfg$absolute_comparison, cfg$processing_comparison)
curves <- estimates <- list(); errors <- numeric()
for (type in c('FULL', 'ENV')) {
  path <- file.path(cfg$model_root, paste0('PAIRED_', type, '_G1.rds'))
  sources <- c(sources, path); m <- readRDS(path)
  stopifnot(nobs(m) == 83L)
  amounts <- expm1(seq(log1p(range_ha[1]), log1p(range_ha[2]), length.out = 401))
  nd <- base[rep(1, length(amounts)), ]; nd$log_recl <- log1p(amounts)
  b <- base; b$log_recl <- log1p(range_ha[1])
  X <- predict(m, nd, type = 'lpmatrix'); xb <- predict(m, b, type = 'lpmatrix')
  D <- sweep(X, 2, as.numeric(xb), '-')
  change <- drop(D %*% coef(m)); se <- sqrt(pmax(0, rowSums((D %*% m$Vp) * D)))
  stopifnot(max(abs(change - (predict(m, nd) - as.numeric(predict(m, b))))) < 1e-10)
  curves[[type]] <- data.frame(model = type, reclamation_km2 = amounts / 100,
    change = change, lower = change - 1.96 * se, upper = change + 1.96 * se)
  ni <- base[rep(1, 2), ]; ni$log_recl <- log1p(iqr_ha)
  XI <- predict(m, ni, type = 'lpmatrix'); di <- XI[2, ] - XI[1, ]
  value <- sum(di * coef(m)); sei <- sqrt(drop(di %*% m$Vp %*% di))
  check <- read.csv(cfg$absolute_comparison)
  check <- check[check$response == paste0('PAIRED_', type, '_684') &
    check$model == 'STORED_ADDITIVE' & check$mode == 'COMMON_ABSOLUTE_IQR', ]
  errors[type] <- max(abs(c(value - check$small_difference, value - check$large_difference)))
  stopifnot(errors[type] < 1e-10)
  estimates[[type]] <- data.frame(model = type, difference = value, lower = value - 1.96 * sei,
    upper = value + 1.96 * sei, lower_reclamation_km2 = iqr_ha[1] / 100,
    upper_reclamation_km2 = iqr_ha[2] / 100, n_cities = 83L, n_species = 684L,
    focal_p = summary(m)$s.table[grep('log_recl', rownames(summary(m)$s.table)), 4])
}
cv <- do.call(rbind, curves); ef <- do.call(rbind, estimates)
write.csv(cv, file.path(out, 'source_data/reclamation_absolute_area_curves.csv'), row.names = FALSE)
write.csv(ef, file.path(out, 'source_data/reclamation_absolute_area_effects.csv'), row.names = FALSE)
palette <- c(FULL = '#447C9D', ENV = '#C5824B')
theme_set(theme_classic(base_family = 'Arial', base_size = 7) + theme(
  axis.text = element_text(size = 7, colour = '#343A40'), axis.title = element_text(size = 7.5),
  axis.line = element_line(linewidth = .3, colour = '#70767C'),
  axis.ticks = element_line(linewidth = .3, colour = '#70767C'),
  plot.title = element_text(size = 8, face = 'bold', margin = margin(b = 5)),
  plot.subtitle = element_text(size = 7, colour = '#5C6268', margin = margin(b = 7)),
  plot.tag = element_text(size = 8, face = 'bold'),
  legend.title = element_blank(), legend.text = element_text(size = 7),
  legend.position = 'bottom', plot.margin = margin(7, 8, 6, 7)))
pa <- ggplot(cv, aes(reclamation_km2, change, colour = model, fill = model)) +
  geom_hline(yintercept = 0, colour = '#91979D', linewidth = .3, linetype = 2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .14, colour = NA) +
  geom_line(linewidth = .65) +
  geom_rug(data = data.frame(reclamation_km2 = pred$reclamation_area / 100),
    aes(x = reclamation_km2), inherit.aes = FALSE, sides = 'b', alpha = .35,
    colour = '#71777D', length = grid::unit(1, 'mm')) +
  scale_x_log10(breaks = c(.01, .1, 1, 10, 100), labels = c('0.01', '0.1', '1', '10', '100')) +
  scale_colour_manual(values = palette) + scale_fill_manual(values = palette) +
  labs(title = 'Absolute reclamation area', x = expression('Reclamation area ('*km^2*'; log scale)'),
    y = 'Change in predicted suitability (0-1)')
ef$position <- ifelse(ef$model == 'FULL', 2, 1)
pb <- ggplot(ef, aes(difference, position, colour = model)) +
  geom_vline(xintercept = 0, linewidth = .3, colour = '#91979D', linetype = 2) +
  geom_segment(aes(x = lower, xend = upper, yend = position), linewidth = .65) +
  geom_point(size = 2) + scale_colour_manual(values = palette, guide = 'none') +
  scale_y_continuous(breaks = c(1, 2), labels = c('ENV', 'FULL'), limits = c(.45, 2.55)) +
  scale_x_continuous(breaks = c(-.06, -.03, 0), labels = c('-0.06', '-0.03', '0')) +
  labs(title = 'A common area increase', subtitle = sprintf('%.2f to %.2f km\u00b2', iqr_ha[1] / 100, iqr_ha[2] / 100),
    x = 'Suitability difference (0-1)', y = NULL)
figure1 <- pa + pb + plot_layout(widths = c(1.85, 1), guides = 'collect') +
  plot_annotation(title = 'Negative associations persist in environmental-only predictions',
    subtitle = 'Same 83 cities and 684 species; absolute-area additive GAMs', tag_levels = 'a',
    theme = theme(plot.title = element_text(family = 'Arial', size = 9, face = 'bold'),
      plot.subtitle = element_text(family = 'Arial', size = 7, colour = '#5C6268'))) &
  theme(legend.position = 'bottom')
tr <- read.csv(cfg$processing_comparison)
stopifnot(nrow(tr) == 9L, identical(tr$step, 0:8))
tr$label <- c('Archived response', 'City polygon mean', 'Area-weighted city mean',
  'Match prediction coverage', 'Current projections, min-max', 'Common 0-1 input scale',
  'Remove native contribution', 'Exclude missing predictions', 'Exclude native species')
tr$position <- 9 - tr$step
tr$group <- ifelse(tr$step == 0, 'Archived', ifelse(tr$step == 1, 'Polygon extraction', 'Later processing'))
write.csv(tr, file.path(out, 'source_data/reclamation_response_processing.csv'), row.names = FALSE)
figure2 <- ggplot(tr, aes(large_difference, position)) +
  annotate('rect', xmin = -Inf, xmax = Inf, ymin = 7.5, ymax = 8.5, fill = '#F4EEE6') +
  geom_vline(xintercept = 0, linewidth = .35, colour = '#92989E', linetype = 2) +
  geom_segment(aes(x = large_lower, xend = large_upper, yend = position, colour = group), linewidth = .65) +
  geom_point(aes(colour = group), size = 2) +
  scale_colour_manual(values = c(Archived = '#454B51', 'Polygon extraction' = '#B67943',
    'Later processing' = '#447C9D'), guide = 'none') +
  scale_y_continuous(breaks = tr$position, labels = tr$label, limits = c(.45, 9.55)) +
  scale_x_continuous(breaks = c(-.2, -.1, 0, .1, .2, .3), limits = c(-.265, .32)) +
  labs(title = 'City extraction changes the fitted large-city contrast',
    subtitle = 'Same 83 cities, absolute-area GAM and original prediction range',
    x = 'High-minus-low reclamation suitability difference (0-1)', y = NULL) +
  theme(axis.ticks.y = element_blank(), axis.line.y = element_blank())
exports <- list()
save_figure <- function(plot, name, width, height) {
  target <- file.path(out, name)
  svglite(paste0(target, '.svg'), width = width / 25.4, height = height / 25.4, bg = 'white')
  print(plot); dev.off()
  cairo_pdf(paste0(target, '.pdf'), width = width / 25.4, height = height / 25.4, family = 'Arial', bg = 'white')
  print(plot); dev.off()
  agg_png(file.path(out, 'preview', paste0(name, '.png')), width = width, height = height,
    units = 'mm', res = 300, background = 'white')
  print(plot); dev.off()
  exports[[name]] <<- list(width_mm = width, height_mm = height,
    pdf_sha256 = hash(paste0(target, '.pdf')), svg_sha256 = hash(paste0(target, '.svg')))
  message('EXPORTED ', name)
}
save_figure(figure1, 'reclamation_absolute_area_sensitivity', 183, 91)
save_figure(figure2, 'reclamation_response_processing', 183, 112)
captions <- c(
  '# Reclamation absolute-area sensitivity',
  'a, Changes in fitted suitability relative to the lowest original reclamation-area endpoint (0.004 km2). Lines and ribbons show fitted differences and pointwise 95% intervals using the saved GAM coefficient covariance. Rugs mark the absolute reclamation areas of the same 83 cities. FULL and ENV retain the same 684-species roster; FULL includes environmental and human predictors in SDM construction, whereas ENV excludes human predictors. The additive GAMs retain the same city-area and environmental and human covariates. Other covariates are held at the existing reference medians. b, High-minus-low contrasts for the same absolute reclamation-area increase, 0.48 to 13.37 km2, with 95% intervals. Response differences are on the 0-1 suitability scale, not relative percentages. The additive comparison does not test a scale reversal. No model was refitted.',
  '', '# Response-processing sensitivity',
  'Fitted high-minus-low reclamation contrasts and pointwise 95% intervals at a fixed city area of 8681.44 km2, using the original reclamation prediction range of 0.004-349.78 km2. The same 83 cities, absolute-area tensor GAM formula, covariates, basis dimensions and REML procedure are retained across all nine processing versions. The highlighted row changes only the original raster extraction from the archived mixture of rectangle and updated-city means to polygon-interior means. Later rows show the successive changes in weighting, coverage, projection pool, input scale, native contributions and denominators. An interval crossing zero does not establish a positive or negative association. This figure documents processing sensitivity; it is not a test of an ecological mechanism. All fitted model and response versions are retained in the source tables.')
writeLines(captions, file.path(out, 'RECLAMATION_EVIDENCE_CAPTIONS.md'))
sources <- unique(sources)
write.csv(data.frame(path = sources, sha256 = vapply(sources, hash, '')),
  file.path(out, 'source_data/reclamation_evidence_input_checksums.csv'), row.names = FALSE)
write_json(list(status = 'EXPORTED', no_model_refits = TRUE, sdm_fits = 0L, backend = 'R',
  matched_cities = 83L, paired_species = 684L, exports = exports,
  maximum_agreement_error_with_saved_contrasts = max(errors)),
  file.path(out, 'reclamation_evidence_render.json'), auto_unbox = TRUE, pretty = TRUE)
