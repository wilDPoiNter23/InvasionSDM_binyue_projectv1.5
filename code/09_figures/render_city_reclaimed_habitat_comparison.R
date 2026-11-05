#!/usr/bin/env Rscript
# Compare spatial summaries using existing models and common prediction profiles.
suppressPackageStartupMessages({
  library(ggplot2); library(patchwork); library(ggbreak)
  library(svglite); library(ragg); library(jsonlite); library(digest)
})
args <- commandArgs(TRUE)
stopifnot(length(args) %in% c(2L, 3L), dir.exists(args[1]), dir.exists(args[2]))
root <- args[1]; out <- args[2]
stopifnot(length(args) == 2L || args[3] == "--render-only")
render_only <- length(args) == 3L && args[3] == "--render-only"
stem <- "city_reclaimed_habitat_comparison"
responses <- c("fraction", "area")
for (folder in c("preview", "source_data", "tables")) {
  dir.create(file.path(out, folder), showWarnings = FALSE)
}
if (render_only) {
  curves <- read.csv(file.path(out, "source_data", paste0(stem, "_curves.csv")))
  contrasts <- read.csv(file.path(out, "tables", paste0(stem, "_contrasts.csv")))
  full_curves <- read.csv(file.path(out, "source_data", paste0(stem, "_full_range.csv")))
  fits <- read.csv(file.path(out, "tables", paste0(stem, "_models.csv")))
  profiles <- read.csv(file.path(out, "source_data", paste0(stem, "_profiles.csv")))
  fields <- setdiff(names(profiles), "key")
  lo <- log1p(contrasts$low_reclamation_km2[1] * 100)
  hi <- log1p(contrasts$high_reclamation_km2[1] * 100)
  regions <- c("CITY", "MAPPED_RECLAMATION")
} else {
library(mgcv)
d <- read.csv(file.path(root, "matched_city_data.csv"))
saved <- read.csv(file.path(root, "reclamation_contrasts.csv"))
original_contrasts_file <- file.path(out, "tables", "reclaimed_land_response_contrasts.csv")
original <- read.csv(original_contrasts_file)
regions <- c("CITY", "MAPPED_RECLAMATION")
responses <- c("fraction", "area")
thresholds <- c("T10", "MAX_TSS")
fields <- c("log_recl", "area_km2", "log_area", "anthro_pc1",
            "climate_pc1", "soil_pc1", "marine_pc1")
model_file <- function(region, threshold, response) {
  file.path(root, paste0(region, "_FULL_", threshold,
                        "_mean_species_suitable_", response, ".rds"))
}
paths <- c(original_contrasts_file, file.path(root, c("matched_city_data.csv", "reclamation_contrasts.csv")),
           unlist(lapply(regions, function(r) unlist(lapply(thresholds, function(t) {
             vapply(responses, function(y) model_file(r, t, y), "")
           })))))
hash <- function(f) {
  con <- file(f, "rb"); on.exit(close(con))
  digest(readBin(con, "raw", n = file.info(f, extra_cols = FALSE)$size),
         algo = "sha256", serialize = FALSE)
}
hashes <- vapply(paths, hash, "")
support <- split(d[d$group == "FULL" & d$threshold == "T10", ],
                 d$region[d$group == "FULL" & d$threshold == "T10"])
common <- sort(Reduce(intersect, lapply(support[regions], function(q) q$key)))
profiles <- support$CITY[match(common, support$CITY$key), ]
other <- support$MAPPED_RECLAMATION[match(common, support$MAPPED_RECLAMATION$key), ]
stopifnot(length(common) > 0L, !anyNA(profiles$key),
          identical(profiles$key, other$key))
for (v in fields) stopifnot(max(abs(profiles[[v]] - other[[v]])) < 1e-10)
reference <- saved[saved$region == "MAPPED_RECLAMATION" &
                     saved$threshold == "T10" &
                     saved$response == "mean_species_suitable_fraction", ]
stopifnot(nrow(reference) == 1L)
lo <- reference$reclamation_log_q25; hi <- reference$reclamation_log_q75
grid <- sort(unique(c(seq(lo, hi, length.out = 201), lo, hi)))
curves <- contrasts <- full_curves <- fits <- list()
predict_average <- function(m, xs, response) {
  X <- t(vapply(xs, function(x) {
    nd <- profiles; nd$log_recl <- x
    colMeans(predict(m, nd, type = "lpmatrix"))
  }, numeric(length(coef(m)))))
  eta <- drop(X %*% coef(m))
  se <- sqrt(pmax(0, rowSums((X %*% vcov(m)) * X)))
  inv <- if (response == "area") expm1 else identity
  list(X = X, data = data.frame(reclamation_km2 = expm1(xs) / 100,
    estimate = inv(eta), lower = inv(eta - 1.96 * se),
    upper = inv(eta + 1.96 * se), model_estimate = eta, model_se = se))
}
for (region in regions) for (th in thresholds) for (response in responses) {
  m <- readRDS(model_file(region, th, response))
  q <- d[d$region == region & d$group == "FULL" & d$threshold == th, ]
  stopifnot(nobs(m) == nrow(q), all(is.finite(q[, fields][[1]])),
            lo >= min(q$log_recl), hi <= max(q$log_recl))
  p <- predict_average(m, grid, response)
  p$data$region <- region; p$data$response <- response; p$data$threshold <- th
  curves[[length(curves) + 1L]] <- p$data
  ix <- match(c(lo, hi), grid); dx <- p$X[ix[2], ] - p$X[ix[1], ]
  delta <- sum(dx * coef(m)); se <- sqrt(drop(dx %*% vcov(m) %*% dx))
  contrasts[[length(contrasts) + 1L]] <- data.frame(region = region,
    threshold = th, response = response, n_model_cities = nobs(m),
    n_prediction_profiles = nrow(profiles),
    low_reclamation_km2 = expm1(lo) / 100,
    high_reclamation_km2 = expm1(hi) / 100,
    low_fitted = p$data$estimate[ix[1]], high_fitted = p$data$estimate[ix[2]],
    difference = delta, se = se, lower = delta - 1.96 * se,
    upper = delta + 1.96 * se, p_value = 2 * pnorm(-abs(delta / se)),
    contrast_scale = if (response == "area") "log1p_km2" else "0_to_1",
    minimum_displayed_estimate = min(p$data$estimate),
    minimum_displayed_lower_interval = min(p$data$lower))
  full <- predict_average(m, seq(min(q$log_recl), max(q$log_recl), length.out = 301), response)$data
  full$region <- region; full$response <- response; full$threshold <- th
  full_curves[[length(full_curves) + 1L]] <- full
  fits[[length(fits) + 1L]] <- data.frame(region = region, threshold = th,
    response = response, model_path = model_file(region, th, response),
    formula = paste(deparse(formula(m)), collapse = " "),
    minimum_full_range_estimate = min(full$estimate),
    negative_full_range_predictions = sum(full$estimate < 0))
}
curves <- do.call(rbind, curves); contrasts <- do.call(rbind, contrasts)
full_curves <- do.call(rbind, full_curves); fits <- do.call(rbind, fits)
}
stopifnot(all(is.finite(curves$estimate)),
          all(curves$estimate >= 0), all(curves$lower >= 0))
colours <- c(CITY = "#6F4C9B", MAPPED_RECLAMATION = "#178F7A")
scope_labels <- c(CITY = "Whole city", MAPPED_RECLAMATION = "Reclaimed land")
format_p <- function(p) if (p < .001) "P < 0.001" else sprintf("P = %.3f", p)
panels <- list()
for (response in responses) {
  v <- curves[curves$response == response & curves$threshold == "T10", ]
  v$region <- factor(v$region, levels = regions)
  e <- contrasts[contrasts$response == response & contrasts$threshold == "T10", ]
  e <- e[match(regions, e$region), ]
  stopifnot(nrow(e) == 2L)
  if (response == "fraction") v[c("estimate", "lower", "upper")] <- v[c("estimate", "lower", "upper")] * 100
  endpoint_x <- range(v$reclamation_km2)
  points <- v[v$reclamation_km2 %in% endpoint_x, ]
  subtitle <- paste(paste0(scope_labels[regions], ": ", vapply(e$p_value, format_p, "")), collapse = "\n")
  caption <- if (response == "fraction") {
    paste(sprintf("%s: %.1f%% to %.1f%%", scope_labels[regions],
                  100 * e$low_fitted, 100 * e$high_fitted), collapse = "\n")
  } else paste(sprintf("%s: %.2f to %.2f km\u00b2", scope_labels[regions],
                       e$low_fitted, e$high_fitted), collapse = "\n")
  p <- ggplot(v, aes(reclamation_km2, estimate, colour = region, fill = region)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .13, colour = NA) +
    geom_line(linewidth = .75, linetype = "solid") +
    geom_point(data = points, aes(shape = region), fill = "white", size = 2, stroke = .65) +
    scale_colour_manual(values = colours, labels = scope_labels) +
    scale_fill_manual(values = colours, labels = scope_labels) +
    scale_shape_manual(values = c(CITY = 21, MAPPED_RECLAMATION = 23), labels = scope_labels) +
    scale_x_log10(breaks = c(1, 2, 5, 10, 20), expand = expansion(mult = .04)) +
    labs(title = if (response == "fraction") "Suitable fraction" else "Effective suitable area",
         subtitle = paste0(subtitle, "\n "), x = "Reclamation area (km\u00b2)",
         y = if (response == "fraction") "Suitable fraction (%)" else "Effective suitable habitat area (km\u00b2)",
         colour = NULL, fill = NULL, shape = NULL, caption = caption,
         tag = if (response == "fraction") "d" else "e") +
    theme_classic(base_family = "Arial", base_size = 8) +
    theme(axis.text = element_text(colour = "#353A3D", size = 7),
          axis.title = element_text(size = 8), axis.line = element_line(linewidth = .3),
          axis.ticks = element_line(linewidth = .3),
          plot.title = element_text(face = "bold", size = 9),
          plot.subtitle = element_text(size = 7.5, lineheight = 1.1),
          plot.caption = element_text(size = 7, hjust = 0, lineheight = 1.2, margin = margin(t = 5)),
          plot.tag = element_text(face = "bold", size = 10),
          legend.position = "bottom", legend.text = element_text(size = 8),
          plot.margin = margin(7, 8, 6, 8))
  if (response == "area") {
    stopifnot(all(v$lower > 0),
              all(v$upper[v$region == "MAPPED_RECLAMATION"] < 15),
              all(v$lower[v$region == "CITY"] > 380), all(v$upper < 550))
    p <- p + scale_y_continuous(limits = c(0, 550),
      breaks = c(0, 5, 10, 15, 380, 400, 450, 500, 550), expand = c(0, 0)) +
      scale_y_break(c(15, 380), scales = "free", space = .45, expand = FALSE) +
      labs(subtitle = paste0(subtitle, "\nAxis break: 15\u2013380 km\u00b2")) +
      theme(axis.text.y.right = element_blank(), axis.ticks.y.right = element_blank(),
            axis.line.y.right = element_blank(), axis.title.y.right = element_blank())
  }
  panels[[length(panels) + 1L]] <- p
}
cairo_pdf(filename = tempfile(fileext = ".pdf"), width = 183 / 25.4,
          height = 112 / 25.4, family = "Arial")
legend_plot <- ggplotGrob(panels[[1]])
legend_index <- which(legend_plot$layout$name == "guide-box-bottom")
stopifnot(length(legend_index) == 1L)
legend <- legend_plot$grobs[[legend_index]]
fraction_grob <- ggplotGrob(panels[[1]] + theme(legend.position = "none"))
area_grob <- grid::grid.grabExpr(
  print(panels[[2]] + theme(legend.position = "none")),
  width = 91.5 / 25.4, height = 102 / 25.4,
  device = function(width, height) cairo_pdf(filename = tempfile(fileext = ".pdf"),
    width = width, height = height, family = "Arial"))
dev.off()
fig <- ((wrap_elements(full = fraction_grob) | wrap_elements(full = area_grob)) /
  wrap_elements(full = legend)) + plot_layout(heights = c(1, .09))
prefix <- file.path(out, stem)
svglite(paste0(prefix, ".svg"), width = 183 / 25.4, height = 112 / 25.4, bg = "white")
print(fig); dev.off()
cairo_pdf(paste0(prefix, ".pdf"), width = 183 / 25.4, height = 112 / 25.4, family = "Arial", bg = "white")
print(fig); dev.off()
agg_png(file.path(out, "preview", paste0(stem, ".png")), width = 183,
        height = 112, units = "mm", res = 300, background = "white")
print(fig); dev.off()
if (!render_only) {
write.csv(curves, file.path(out, "source_data", paste0(stem, "_curves.csv")), row.names = FALSE)
write.csv(full_curves, file.path(out, "source_data", paste0(stem, "_full_range.csv")), row.names = FALSE)
write.csv(contrasts, file.path(out, "tables", paste0(stem, "_contrasts.csv")), row.names = FALSE)
write.csv(fits, file.path(out, "tables", paste0(stem, "_models.csv")), row.names = FALSE)
write.csv(profiles[, c("key", fields)], file.path(out, "source_data", paste0(stem, "_profiles.csv")), row.names = FALSE)
after <- vapply(paths, hash, "")
stopifnot(identical(hashes, after))
}
if (!render_only) {
original$region <- "MAPPED_RECLAMATION"
original$n_model_cities <- original$n_cities
original$n_prediction_profiles <- original$n_cities
original$p_value <- 2 * pnorm(-abs(original$difference / original$se))
added <- contrasts[contrasts$region == "CITY", ]
added$group <- "FULL_814"
added$n_cities <- added$n_model_cities
columns <- c("region", "group", "response", "threshold", "n_model_cities",
  "n_prediction_profiles", "low_reclamation_km2", "high_reclamation_km2",
  "low_fitted", "high_fitted", "difference", "se", "lower", "upper", "p_value",
  "contrast_scale")
complete <- rbind(original[, columns], added[, columns])
complete$figure_panel <- ""
for (j in seq_len(nrow(complete))) {
  if (complete$group[j] == "FULL_814" && complete$threshold[j] == "T10") {
    complete$figure_panel[j] <- if (complete$response[j] == "fraction") "d" else "e"
  }
}
stopifnot(nrow(complete) == nrow(original) + nrow(added),
  !anyDuplicated(complete[, c("region", "group", "response", "threshold")]))
main <- complete[complete$figure_panel != "", ]
main <- main[order(main$figure_panel, match(main$region, regions)), ]
write.csv(complete, file.path(out, "tables", "Supplementary_Table_18.csv"), row.names = FALSE)
write_json(list(title = "Supplementary Table 18. Reclamation associations with invasion suitability and suitable habitat area.",
  comparison = "Adjusted predictions at reclamation extents of 0.806 and 23.733 km2.",
  notes = c("Whole-city habitat includes reclaimed land. Reclaimed-land summaries include only the mapped reclamation footprint.",
    "Fraction contrasts are in 0-1 units. Effective-area contrasts and confidence intervals are on the log(1 + area) scale; displayed area levels are in km2.",
    "P values are two-sided Wald endpoint contrasts, not tests of entire smooth terms. Confidence intervals are 95% intervals.",
    "New whole-city comparisons use common city prediction profiles; original fitting samples are retained.",
    "Source tables: reclaimed_land_response_contrasts.csv and city_reclaimed_habitat_comparison_contrasts.csv."),
  figure_sheet_name = "Fig4_d_e",
  figure_title = "Fig. 4d-e | Whole-city and reclaimed-land habitat comparisons",
  primary = main, complete = complete),
  file.path(out, "tables", "Supplementary_Table_18_workbook.json"),
  pretty = TRUE, auto_unbox = TRUE, dataframe = "rows", digits = 16)
}
if (!render_only) {
write_json(list(status = "COMPLETE", model_refits = 0L, sdm_refits = 0L,
  input_paths = paths, input_sha256 = as.list(hashes),
  n_prediction_profiles = nrow(profiles), original_model_samples_retained = TRUE,
  prediction = "Common city covariate profiles, with reclamation extent varied",
  display = "Existing reclaimed-land interquartile comparison, applied to both regions",
  displayed_reclamation_km2 = expm1(c(lo, hi)) / 100,
  area_display = "expm1 of average fitted log1p area, without smearing",
  negative_predictions_replaced_or_clipped = FALSE,
  full_range_predictions_retained = TRUE,
  whole_city_includes_reclaimed_land = TRUE,
  independent_region_difference_test = FALSE,
  inputs_unchanged = TRUE), paste0(prefix, "_receipt.json"),
  pretty = TRUE, auto_unbox = TRUE)
}
print(contrasts[contrasts$threshold == "T10", ])
message("Saved editable figures and source data: ", prefix)
warning_messages <- unique(names(warnings()))
if (length(warning_messages)) cat(paste(head(warning_messages, 8), collapse = "\n"), "\n")
quit(save = "no", status = 0)
