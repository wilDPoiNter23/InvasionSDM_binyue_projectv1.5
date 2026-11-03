#!/usr/bin/env Rscript
# Render continuous and binary reclaimed-land responses from saved predictions.
suppressPackageStartupMessages({library(ggplot2); library(patchwork); library(jsonlite)
  library(svglite); library(ragg); library(digest)})
cfg <- fromJSON(commandArgs(TRUE)[1])
root <- cfg$output_root; out <- cfg$figure_root
stopifnot(dir.exists(out), capabilities("cairo"))
for (folder in c("preview", "source_data", "tables")) {
  dir.create(file.path(out, folder), showWarnings = FALSE)
}
curves <- read.csv(file.path(root, "curves.csv"))
effects <- read.csv(file.path(root, "effect.csv"))
input <- read.csv(file.path(root, "continuous_city_inputs.csv"))
stopifnot(all(effects$n_cities == 70L), nrow(effects) == 15L)
labels <- c(FULL_814 = "FULL | 814 species", PAIRED_FULL = "FULL | 684 species",
            PAIRED_ENV = "ENV | 684 species")
palette <- c(FULL_814 = "#447C9D", PAIRED_FULL = "#447C9D", PAIRED_ENV = "#C5824B")
titles <- c(mean = "Mean suitability", fraction = "Suitable fraction", area = "Effective suitable area")
ylabs <- c(mean = "Predicted suitability (0-1)", fraction = "Suitable fraction (%)",
           area = "Effective suitable area (km\u00b2)")
theme_set(theme_classic(base_family = "Arial", base_size = 7) + theme(
  axis.text = element_text(size = 7, colour = "#343A40"),
  axis.title = element_text(size = 7.5),
  axis.line = element_line(linewidth = .3, colour = "#70767C"),
  axis.ticks = element_line(linewidth = .3, colour = "#70767C"),
  plot.title = element_text(size = 8, face = "bold", margin = margin(b = 4)),
  plot.subtitle = element_text(size = 7, margin = margin(b = 6)),
  plot.tag = element_text(size = 9, face = "bold"),
  plot.caption = element_text(size = 6.5, colour = "#565C62", hjust = 0),
  legend.title = element_blank(), legend.text = element_text(size = 7),
  legend.position = "bottom", plot.margin = margin(7, 7, 6, 7)))
hash <- function(path) {
  con <- file(path, "rb"); on.exit(close(con))
  digest(readBin(con, "raw", n = file.info(path, extra_cols = FALSE)$size),
         algo = "sha256", serialize = FALSE)
}
panel <- function(g, response, th = "T10", both = FALSE, full_range = FALSE) {
  threshold <- if (response == "mean") "NONE" else if (both) c("T10", "MAX_TSS") else th
  v <- curves[curves$group == g & curves$response == response &
                curves$threshold %in% threshold, ]
  ef <- effects[effects$group == g & effects$response == response &
                  effects$threshold %in% threshold, ]
  comparison_range <- c(ef$low_reclamation_km2[1], ef$high_reclamation_km2[1])
  bounds <- curves[curves$response == response & curves$threshold %in% threshold, ]
  if (!full_range) {
    v <- v[v$reclamation_km2 >= comparison_range[1] &
             v$reclamation_km2 <= comparison_range[2], ]
    bounds <- bounds[bounds$reclamation_km2 >= comparison_range[1] &
                       bounds$reclamation_km2 <= comparison_range[2], ]
  }
  if (response == "fraction") {
    v[c("estimate", "lower", "upper")] <- v[c("estimate", "lower", "upper")] * 100
    ef[c("low_fitted", "high_fitted")] <- ef[c("low_fitted", "high_fitted")] * 100
  }
  rug <- unique(input[input$group == g, c("key", "reclamation_area")])
  rug$x <- rug$reclamation_area / 100
  if (!full_range) rug <- rug[rug$x >= comparison_range[1] & rug$x <= comparison_range[2], ]
  if (both) {
    v$threshold <- factor(v$threshold, levels = c("T10", "MAX_TSS"),
                          labels = c("T10", "Maximum TSS"))
    plt <- ggplot(v, aes(reclamation_km2, estimate, colour = threshold, fill = threshold)) +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .09, colour = NA) +
      geom_line(aes(linetype = threshold), linewidth = .65) +
      scale_colour_manual(values = c(T10 = "#447C9D", "Maximum TSS" = "#C5824B")) +
      scale_fill_manual(values = c(T10 = "#447C9D", "Maximum TSS" = "#C5824B")) +
      scale_linetype_manual(values = c(T10 = "solid", "Maximum TSS" = "longdash"))
    caption <- "Lines: T10 and maximum-TSS thresholds"
  } else {
    pts <- data.frame(x = c(ef$low_reclamation_km2, ef$high_reclamation_km2),
                      y = c(ef$low_fitted, ef$high_fitted))
    plt <- ggplot(v, aes(reclamation_km2, estimate)) +
      geom_ribbon(aes(ymin = lower, ymax = upper), fill = palette[g], alpha = .16) +
      geom_line(colour = palette[g], linewidth = .7) +
      geom_point(data = pts, aes(x, y), inherit.aes = FALSE, shape = 21,
                 fill = "white", colour = palette[g], stroke = .65, size = 1.8)
    caption <- if (response == "mean") {
      sprintf("Area Q25 to Q75: %.3f \u2192 %.3f", ef$low_fitted, ef$high_fitted)
    } else if (response == "fraction") {
      sprintf("Area Q25 to Q75: %.1f%% \u2192 %.1f%%", ef$low_fitted, ef$high_fitted)
    } else {
      sprintf("Area Q25 to Q75: %.2f \u2192 %.2f km\u00b2", ef$low_fitted, ef$high_fitted)
    }
  }
  plt <- plt +
    geom_rug(data = rug, aes(x = x), inherit.aes = FALSE, colour = "#71777D",
             alpha = .35, length = grid::unit(1, "mm"), linewidth = .25) +
    scale_x_log10(breaks = if (full_range) c(.1, 1, 10, 100) else c(1, 2, 5, 10, 20),
                  expand = expansion(mult = c(.035, .035))) +
    labs(title = titles[response], subtitle = labels[g],
         x = "Reclamation area (km\u00b2; log scale)", y = ylabs[response], caption = caption) +
    theme(plot.subtitle = element_text(colour = palette[g]))
  yl <- range(c(bounds$lower, bounds$upper))
  if (response == "fraction") yl <- yl * 100
  if (response == "area" && full_range) {
    plt <- plt + scale_y_continuous(trans = scales::transform_log1p(),
      breaks = c(-.5, 0, 1, 5, 20, 100), labels = c("-0.5", "0", "1", "5", "20", "100")) +
      geom_hline(yintercept = 0, colour = "#777777", linetype = "dotted", linewidth = .3) +
      labs(y = "Effective suitable area\n(km\u00b2; log1p scale)")
  }
  plt + coord_cartesian(ylim = yl)
}
exports <- list()
save_figure <- function(plot, name, width, height) {
  target <- file.path(out, name)
  svglite(paste0(target, ".svg"), width = width / 25.4, height = height / 25.4, bg = "white")
  print(plot); dev.off()
  cairo_pdf(paste0(target, ".pdf"), width = width / 25.4, height = height / 25.4,
            family = "Arial", bg = "white")
  print(plot); dev.off()
  agg_png(file.path(out, "preview", paste0(name, ".png")),
          width = width, height = height, units = "mm", res = 300, background = "white")
  print(plot); dev.off()
  exports[[name]] <<- list(width_mm = width, height_mm = height,
    pdf_sha256 = hash(paste0(target, ".pdf")), svg_sha256 = hash(paste0(target, ".svg")))
  message("EXPORTED ", name)
}
triptych <- function(gs, subtitle, full_range = FALSE) {
  plots <- unlist(lapply(gs, function(g) {
    lapply(c("mean", "fraction", "area"), function(response) panel(g, response, full_range = full_range))
  }), recursive = FALSE)
  wrap_plots(plots, ncol = 3) + plot_annotation(
    title = if (full_range) "Reclaimed-land responses across the full observed extent range" else
      "Lower suitability, greater suitable area within reclaimed land",
    subtitle = subtitle, tag_levels = "a",
    caption = "Lines: adjusted GAM predictions. Shading: pointwise 95% intervals. Ticks: observed cities.",
    theme = theme(plot.title = element_text(family = "Arial", size = 10, face = "bold"),
      plot.subtitle = element_text(family = "Arial", size = 7),
      plot.caption = element_text(family = "Arial", size = 6.7, hjust = 0)))
}
main <- triptych(c("FULL_814", "PAIRED_ENV"),
  "Same 70 cities | Common comparison: 0.81 to 23.73 km\u00b2 of reclamation | T10 threshold")
paired <- triptych(c("PAIRED_FULL", "PAIRED_ENV"),
  "Same 70 cities and 684 species | Common comparison: 0.81 to 23.73 km\u00b2 | T10 threshold")
full_range <- triptych(c("FULL_814", "PAIRED_ENV"),
  "Same 70-city fits | Complete curves, including boundary behaviour | T10 threshold", TRUE)
thresholds <- wrap_plots(unlist(lapply(names(labels), function(g) {
  lapply(c("fraction", "area"), function(response) panel(g, response, both = TRUE))
}), recursive = FALSE), ncol = 2, guides = "collect") +
  plot_annotation(title = "Threshold choice preserves the area-fraction contrast",
    subtitle = "Same 70 cities | Common comparison: 0.81 to 23.73 km\u00b2 | Existing fits", tag_levels = "a",
    theme = theme(plot.title = element_text(family = "Arial", size = 10, face = "bold"),
                  plot.subtitle = element_text(family = "Arial", size = 7))) &
  theme(legend.position = "bottom")
save_figure(main, "reclaimed_land_three_responses", 183, 149)
save_figure(paired, "reclaimed_land_three_responses_paired684", 183, 149)
save_figure(thresholds, "reclaimed_land_binary_thresholds", 183, 195)
save_figure(full_range, "reclaimed_land_three_responses_full_range", 183, 149)
file.copy(file.path(root, "curves.csv"),
  file.path(out, "source_data/reclaimed_land_response_curves.csv"), overwrite = TRUE)
file.copy(file.path(root, "effect.csv"),
  file.path(out, "tables/reclaimed_land_response_contrasts.csv"), overwrite = TRUE)
writeLines(c(
  "# Lower suitability but greater effective suitable area within reclaimed land",
  "",
  "a-c, FULL projections for 814 species. d-f, environmental-only (ENV) projections for 684 species. Columns show continuous mean suitability (a,d), T10-defined suitable fraction (b,e), and effective suitable area (c,f) within the historical mapped reclamation footprint. All panels use the same 70 cities with complete covariates. Curves average GAM predictions across those cities while varying reclamation extent and retaining each city's remaining covariates. Shading denotes pointwise 95% intervals from the fitted coefficient covariance. The display focuses on the existing interquartile comparison, 0.81-23.73 square kilometres, using quartiles on the log1p hectare scale. All 70 cities remain in model estimation and standardization. Bottom ticks show observed reclamation extents within the displayed range; open circles mark its endpoints. Panel footnotes report the corresponding fitted levels. Reclamation extent is shown on a logarithmic axis; response axes are linear. Complete-range curves are supplied separately.",
  "",
  "Continuous suitability is an area-weighted average of cellwise mean predictions over finite non-native species. Suitable fraction averages thresholded species predictions within cells and then weights by mapped cell area. Effective suitable area integrates those cellwise fractions over the finite prediction footprint, rather than measuring the union of land suitable for at least one species. Native ranges are excluded and missing predictions remain unknown. Area curves back-transform average fitted log1p areas without residual smearing; they are not arithmetic mean area predictions. Continuous and fraction models contain a tensor smooth of reclamation extent and city area; saved area models use additive smooths. All models retain the existing human, climate, soil and marine principal-component covariates. Cross-city associations do not represent within-city temporal changes.",
  "",
  "Only three continuous-response GAMs were newly fitted, using REML. All twelve binary-response GAMs and their T10 and maximum-TSS thresholds were reused. The interquartile contrasts and intervals are unadjusted exploratory comparisons; the complete set is supplied in the source table. No SDMs were refitted.",
  "",
  "# Paired species-pool comparison",
  "",
  "The same arrangement and estimators are repeated for FULL and ENV projections restricted to their existing common 684-species roster, using the same 70 cities. This comparison separates model-family differences from species-pool differences.",
  "",
  "# Threshold comparison for suitable fraction and effective suitable area",
  "",
  "Rows show FULL814, paired FULL684 and ENV684. Columns show suitable fraction and effective suitable area. Solid blue and dashed orange curves represent the saved T10 and maximum-TSS models, respectively; shading denotes pointwise 95% intervals. These are the same city samples, definitions, interquartile display range and standardization used in the main figure. All saved model results are included.",
  "",
  "# Complete-range curves",
  "",
  "The main six-panel comparison is also shown across the full marginal range of reclamation extent. These are the identical fitted models and source curves. The low-area boundary of the saved Gaussian log1p-area models can yield negative back-transformed standardized predictions; they are displayed rather than clipped and have no physical area interpretation. The interquartile area comparison lies above zero. Complete-range mean-suitability curves are nonlinear, so the negative interquartile contrasts should not be described as a monotonic decrease across the entire range. Area axes use a log1p display. No model was changed to alter this boundary behaviour."
), file.path(out, "RECLAIMED_LAND_RESPONSE_CAPTIONS.md"))
write_json(list(status = "EXPORTED", backend = "R", no_model_refits_in_render = TRUE,
  exports = exports, source_files = c(file.path(root, "curves.csv"), file.path(root, "effect.csv"))),
  file.path(out, "reclaimed_land_response_render.json"), auto_unbox = TRUE, pretty = TRUE)
