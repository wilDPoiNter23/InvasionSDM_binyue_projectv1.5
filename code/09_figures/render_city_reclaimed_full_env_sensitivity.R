#!/usr/bin/env Rscript
# Render matched prediction comparisons from saved models, without refitting.
suppressPackageStartupMessages({
  library(mgcv); library(ggplot2); library(patchwork); library(ggbreak)
  library(svglite); library(ragg); library(jsonlite); library(digest)
})
args <- commandArgs(TRUE)
stopifnot(length(args) == 2L, dir.exists(args[1]), dir.exists(args[2]))
root <- args[1]; out <- args[2]
stem <- "city_reclaimed_full_env_sensitivity"
groups <- c("FULL", "ENV")
regions <- c("CITY", "MAPPED_RECLAMATION")
responses <- c("mean", "fraction", "area")
fields <- c("log_recl", "area_km2", "log_area", "anthro_pc1",
            "climate_pc1", "soil_pc1", "marine_pc1")
profile_file <- file.path(out, "source_data", "city_reclaimed_habitat_comparison_profiles.csv")
profiles <- read.csv(profile_file)
binary_file <- file.path(root, "binary_native", "paired_gam", "matched_city_data.csv")
binary <- read.csv(binary_file)
for (g in groups) for (r in regions) {
  d <- binary[binary$group == g & binary$region == r & binary$threshold == "T10", ]
  d <- d[match(profiles$key, d$key), ]
  stopifnot(!anyNA(d$key), all(d$frozen_species_n == 684))
  for (f in fields) stopifnot(max(abs(d[[f]] - profiles[[f]])) < 1e-8)
}
reference_file <- file.path(out, "tables", "reclaimed_land_response_contrasts.csv")
reference <- read.csv(reference_file)
lo <- log1p(reference$low_reclamation_km2[1] * 100)
hi <- log1p(reference$high_reclamation_km2[1] * 100)
xs <- seq(lo, hi, length.out = 201)
model_file <- function(g, r, y) {
  if (y == "mean") {
    if (r == "CITY") return(file.path(root, "city_gam", paste0("PAIRED_", g, "_G0.rds")))
    return(file.path(root, "city_gam", "reclaimed_land_responses_20261020",
                     paste0("PAIRED_", g, "_mean_suitability.rds")))
  }
  file.path(root, "binary_native", "paired_gam",
            paste0(r, "_", g, "_T10_mean_species_suitable_", y, ".rds"))
}
paths <- c(profile_file, binary_file, reference_file,
  unlist(lapply(groups, function(g) unlist(lapply(regions, function(r)
    vapply(responses, function(y) model_file(g, r, y), ""))))))
stopifnot(all(file.exists(paths)))
hash_file <- function(p) {
  con <- file(p, "rb"); on.exit(close(con))
  digest(readBin(con, "raw", n = file.info(p, extra_cols = FALSE)$size),
         algo = "sha256", serialize = FALSE)
}
hashes <- vapply(paths, hash_file, "")
curves <- contrasts <- specs <- list()
for (g in groups) for (y in responses) for (r in regions) {
  m <- readRDS(model_file(g, r, y))
  X <- t(vapply(xs, function(x) {
    nd <- profiles; nd$log_recl <- x
    colMeans(predict(m, nd, type = "lpmatrix"))
  }, numeric(length(coef(m)))))
  eta <- drop(X %*% coef(m))
  se <- sqrt(pmax(0, rowSums((X %*% vcov(m)) * X)))
  inv <- if (y == "area") expm1 else identity
  curves[[length(curves) + 1L]] <- data.frame(group = g, response = y, region = r,
    reclamation_km2 = expm1(xs) / 100, estimate = inv(eta),
    lower = inv(eta - 1.96 * se), upper = inv(eta + 1.96 * se))
  dx <- X[nrow(X), ] - X[1, ]
  delta <- sum(dx * coef(m)); ds <- sqrt(drop(dx %*% vcov(m) %*% dx))
  contrasts[[length(contrasts) + 1L]] <- data.frame(group = g, response = y, region = r,
    low_reclamation_km2 = expm1(lo) / 100, high_reclamation_km2 = expm1(hi) / 100,
    low_fitted = inv(eta[1]), high_fitted = inv(tail(eta, 1)),
    difference = delta, se = ds, lower = delta - 1.96 * ds, upper = delta + 1.96 * ds,
    p_value = 2 * pnorm(-abs(delta / ds)),
    contrast_scale = if (y == "area") "log1p_km2" else "0_to_1",
    n_model_cities = nobs(m), n_prediction_profiles = nrow(profiles))
  specs[[length(specs) + 1L]] <- data.frame(group = g, response = y, region = r,
    model = model_file(g, r, y), formula = paste(deparse(formula(m)), collapse = " "))
}
curves <- do.call(rbind, curves); contrasts <- do.call(rbind, contrasts)
specs <- do.call(rbind, specs)
stopifnot(all(is.finite(curves$estimate)), all(curves$lower >= 0))
# Independently reproduce archived reclaimed-land contrasts.
for (g in groups) for (y in responses) {
  old <- reference[reference$group == paste0("PAIRED_", g) & reference$response == y &
    reference$threshold == if (y == "mean") "NONE" else "T10", ]
  new <- contrasts[contrasts$group == g & contrasts$response == y &
    contrasts$region == "MAPPED_RECLAMATION", ]
  stopifnot(nrow(old) == 1, max(abs(old$low_fitted - new$low_fitted),
    abs(old$high_fitted - new$high_fitted)) < 1e-8)
}
av <- curves[curves$response == "area", ]
lower_top <- ceiling(max(av$upper[av$region == "MAPPED_RECLAMATION"]) / 5) * 5
upper_bottom <- floor(min(av$lower[av$region == "CITY"]) / 25) * 25
upper_top <- ceiling(max(av$upper) / 50) * 50
stopifnot(upper_bottom > lower_top,
  !any(av$lower < upper_bottom & av$upper > lower_top))
colours <- c(CITY = "#6F4C9B", MAPPED_RECLAMATION = "#178F7A")
scope <- c(CITY = "Whole city", MAPPED_RECLAMATION = "Reclaimed land")
titles <- c(mean = "Invasion suitability", fraction = "Suitable fraction", area = "Effective suitable area")
ylabels <- c(mean = "Mean predicted suitability", fraction = "Suitable species fraction (%)",
             area = "Effective suitable habitat area (km\u00b2)")
fmt <- function(p) if (p < .001) "P < 0.001" else sprintf("P = %.3f", p)
panels <- list()
for (g in groups) for (y in responses) {
  v <- curves[curves$group == g & curves$response == y, ]
  v$region <- factor(v$region, levels = regions)
  e <- contrasts[contrasts$group == g & contrasts$response == y, ]
  e <- e[match(regions, e$region), ]
  if (y == "fraction") v[c("estimate", "lower", "upper")] <-
    v[c("estimate", "lower", "upper")] * 100
  subtitle <- paste(paste0(scope[regions], ": ", vapply(e$p_value, fmt, "")), collapse = "\n")
  extra <- if (y == "area") sprintf("Axis break: %g\u2013%g km\u00b2", lower_top, upper_bottom) else " "
  p <- ggplot(v, aes(reclamation_km2, estimate, colour = region, fill = region)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .13, colour = NA) +
    geom_line(linewidth = .65) +
    geom_point(data = v[v$reclamation_km2 %in% range(v$reclamation_km2), ],
      aes(shape = region), fill = "white", size = 1.65, stroke = .55) +
    scale_colour_manual(values = colours, labels = scope) +
    scale_fill_manual(values = colours, labels = scope) +
    scale_shape_manual(values = c(CITY = 21, MAPPED_RECLAMATION = 23), labels = scope) +
    scale_x_log10(breaks = c(1, 2, 5, 10, 20), expand = expansion(mult = .04)) +
    labs(title = titles[y], subtitle = paste0(subtitle, "\n", extra),
      x = "Reclamation area (km\u00b2)", y = ylabels[y], colour = NULL, fill = NULL,
      shape = NULL, tag = letters[length(panels) + 1L]) +
    theme_classic(base_family = "Arial", base_size = 7) +
    theme(axis.text = element_text(size = 6.5, colour = "#353A3D"),
      axis.title = element_text(size = 7), axis.line = element_line(linewidth = .3),
      axis.ticks = element_line(linewidth = .3), plot.title = element_text(face = "bold", size = 8),
      plot.subtitle = element_text(size = 6.5, lineheight = 1.1),
      plot.tag = element_text(face = "bold", size = 10),
      legend.position = "bottom", legend.text = element_text(size = 8),
      plot.margin = margin(5, 4, 4, 3))
  if (y == "area") p <- p + scale_y_continuous(limits = c(0, upper_top),
    breaks = unique(c(seq(0, lower_top, 5), seq(upper_bottom, upper_top, 50))), expand = c(0, 0)) +
    scale_y_break(c(lower_top, upper_bottom), scales = "free", space = .35, expand = FALSE) +
    theme(axis.text.y.right = element_blank(), axis.ticks.y.right = element_blank(),
      axis.line.y.right = element_blank(), axis.title.y.right = element_blank())
  if (y != "area") {
    all_y <- curves[curves$response == y, c("lower", "upper")]
    lim <- range(all_y) * if (y == "fraction") 100 else 1
    p <- p + scale_y_continuous(limits = lim + c(-1, 1) * diff(lim) * .04)
  }
  panels[[length(panels) + 1L]] <- p
}
cairo_pdf(tempfile(fileext = ".pdf"), width = 183 / 25.4, height = 190 / 25.4, family = "Arial")
lg <- ggplotGrob(panels[[1]])
legend <- lg$grobs[[which(lg$layout$name == "guide-box-bottom")]]
grobs <- lapply(seq_along(panels), function(i) {
  p <- panels[[i]] + theme(legend.position = "none")
  if (i %% 3 != 0) return(ggplotGrob(p))
  grid::grid.grabExpr(print(p), width = 61 / 25.4, height = 82 / 25.4,
    device = function(width, height) cairo_pdf(tempfile(fileext = ".pdf"),
      width = width, height = height, family = "Arial"))
})
dev.off()
row_plot <- function(indices, title) {
  wrap_plots(lapply(grobs[indices], function(g) wrap_elements(full = g)), ncol = 3) +
    plot_annotation(title = title, theme = theme(plot.title = element_text(
      family = "Arial", size = 9, face = "bold", margin = margin(b = 3))))
}
fig <- (wrap_elements(full = row_plot(1:3, "FULL | Environmental and human activity predictors")) /
        wrap_elements(full = row_plot(4:6, "ENV | Environmental predictors only")) /
        wrap_elements(full = legend)) + plot_layout(heights = c(1, 1, .075))
prefix <- file.path(out, stem)
svglite(paste0(prefix, ".svg"), width = 183 / 25.4, height = 190 / 25.4, bg = "white")
print(fig); dev.off()
cairo_pdf(paste0(prefix, ".pdf"), width = 183 / 25.4, height = 190 / 25.4, family = "Arial")
print(fig); dev.off()
agg_png(file.path(out, "preview", paste0(stem, ".png")), width = 183, height = 190,
        units = "mm", res = 300, background = "white")
print(fig); dev.off()
write.csv(curves, file.path(out, "source_data", paste0(stem, "_curves.csv")), row.names = FALSE)
write.csv(contrasts, file.path(out, "tables", paste0(stem, "_contrasts.csv")), row.names = FALSE)
write.csv(specs, file.path(out, "tables", paste0(stem, "_models.csv")), row.names = FALSE)
stopifnot(identical(hashes, vapply(paths, hash_file, "")))
write_json(list(status = "EXPORTED", model_refits = 0, sdm_refits = 0,
  matched_species = 684, inputs_unchanged = TRUE, input_sha256 = as.list(hashes),
  input_paths = paths, original_model_samples_retained = TRUE,
  prediction_profiles = nrow(profiles), threshold = "Species-specific 10% omission",
  common_area_axis_break_km2 = c(lower_top, upper_bottom),
  area_inverse = "expm1 of average fitted log1p area; no smearing correction",
  p_values = "Two-sided Wald endpoint contrasts; not tests of entire smooths",
  fraction = "Area-weighted cellwise proportion of eligible nonnative species predicted suitable",
  whole_city_includes_reclaimed_land = TRUE, archived_reclaimed_contrasts_reproduced = TRUE),
  paste0(prefix, "_receipt.json"), pretty = TRUE, auto_unbox = TRUE)
print(contrasts[, c("group", "response", "region", "low_fitted", "high_fitted", "p_value")])
message("Exported six-panel sensitivity figure: ", prefix)
quit(save = "no", status = 0)
