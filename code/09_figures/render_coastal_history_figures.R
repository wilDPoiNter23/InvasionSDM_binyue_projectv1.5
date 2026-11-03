# Render three standalone figures from existing coastal-history estimates.
# Usage: Rscript render_coastal_history_figures.R configuration.json [figure]
# Optional figure: land_use, formation_cohorts or first_records.
suppressPackageStartupMessages({
  library(ggplot2)
  library(patchwork)
  library(jsonlite)
  library(svglite)
  library(ragg)
  library(digest)
})

args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) %in% 1:2)
cfg <- fromJSON(args[1])
selected <- if (length(args) == 2) args[2] else "all"
stopifnot(selected %in% c("all", "land_use", "formation_cohorts", "first_records"))
out <- cfg$output_root
stopifnot(dir.exists(out), dir.exists(file.path(out, "source_data")))
dir.create(file.path(out, "preview"), showWarnings = FALSE)

hash_file <- function(path) {
  con <- file(path, "rb")
  on.exit(close(con))
  digest(readBin(con, "raw", n = file.info(path, extra_cols = FALSE)$size),
         algo = "sha256", serialize = FALSE)
}
source_manifest <- read.csv(file.path(out, "source_manifest.csv"))
used_sources <- list()
read_existing <- function(label) {
  entry <- source_manifest[source_manifest$label == label, ]
  stopifnot(nrow(entry) == 1, file.exists(entry$source))
  stopifnot(identical(hash_file(entry$source), entry$sha256))
  d <- read.csv(entry$source, stringsAsFactors = FALSE, check.names = FALSE)
  stopifnot(nrow(d) == entry$rows)
  used_sources[[label]] <<- entry
  d
}

font <- "Arial"
stopifnot(capabilities("cairo"))
colours <- c(FULL = "#447C9D", ENV = "#C5824B")
theme_set(theme_classic(base_family = font, base_size = 7) + theme(
  axis.text = element_text(size = 7, colour = "#30343A"),
  axis.title = element_text(size = 7.3, colour = "#30343A"),
  axis.line = element_line(linewidth = 0.3, colour = "#6B7075"),
  axis.ticks = element_line(linewidth = 0.3, colour = "#6B7075"),
  plot.title = element_text(size = 8, face = "bold", margin = margin(b = 5)),
  plot.subtitle = element_text(size = 6.8, colour = "#5C6269", margin = margin(b = 8)),
  plot.caption = element_text(size = 6.3, colour = "#5C6269", hjust = 0,
                            margin = margin(t = 9), lineheight = 1.2),
  plot.tag = element_text(size = 8, face = "bold"),
  plot.margin = margin(7, 9, 7, 8),
  legend.position = "none"
))
exports <- list()
legends <- character()
save_figure <- function(p, name, width, height, legend) {
  base <- file.path(out, name)
  svglite(paste0(base, ".svg"), width = width / 25.4, height = height / 25.4,
          bg = "white")
  print(p)
  dev.off()
  cairo_pdf(paste0(base, ".pdf"), width = width / 25.4, height = height / 25.4,
            family = font, bg = "white")
  print(p)
  dev.off()
  agg_png(file.path(out, "preview", paste0(name, ".png")),
          width = width, height = height, units = "mm", res = 300,
          background = "white")
  print(p)
  dev.off()
  exports[[name]] <<- list(width_mm = width, height_mm = height,
                          pdf_sha256 = hash_file(paste0(base, ".pdf")),
                          svg_sha256 = hash_file(paste0(base, ".svg")))
  legends <<- c(legends, paste0("## ", name), legend, "")
  message("EXPORTED ", name)
}

if (selected %in% c("all", "land_use")) {
  full <- read_existing("full814_bayesian_contrasts")
  env <- read_existing("paired_bayesian_contrasts")
  use_codes <- c("transp", "comm", "public", "resid", "indus", "crop",
                 "bare", "grass", "forest", "water")
  use_labels <- c("Transport (reference)", "Commercial", "Public use", "Residential",
                  "Industrial", "Cropland", "Bare land", "Grassland", "Forest", "Water")
  land_panel <- function(d, model) {
    d <- d[d$type == "USE_VS_REFERENCE" & d$case == "primary", ]
    stopifnot(nrow(d) == 9, !anyDuplicated(d$to), all(d$from == "transp"),
              all(d$age_index_years == 15), all(d$chain_status == "PASS_CHAIN_DIAGNOSTICS"))
    d$position <- match(d$to, rev(use_codes))
    d$estimate <- 100 * d$difference
    d$lo <- 100 * d$lower
    d$hi <- 100 * d$upper
    d$display <- ifelse(abs(d$estimate) < 0.05, "0.0", sprintf("%+.1f", d$estimate))
    stopifnot(all(d$lo > -15.5), all(d$hi < 7))
    ggplot(d, aes(estimate, position)) +
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = 3.5, ymax = 4.5,
               fill = "#F0F2F3") +
      geom_vline(xintercept = 0, colour = "#9FA4AA", linewidth = 0.35, linetype = 2) +
      geom_segment(aes(x = lo, xend = hi, yend = position),
                   colour = colours[model], linewidth = 0.65) +
      geom_point(colour = colours[model], size = 1.9) +
      annotate("point", x = 0, y = 10, shape = 15, size = 1.5, colour = "#70767C") +
      geom_text(aes(x = 6.4, label = display), hjust = 1, size = 2.35,
                family = font, colour = "#454B51") +
      annotate("text", x = 6.4, y = 10, label = "0.0", hjust = 1,
               size = 2.35, family = font, colour = "#70767C") +
      scale_y_continuous(breaks = 1:10, labels = rev(use_labels),
                         limits = c(0.45, 10.6), expand = expansion(mult = 0)) +
      scale_x_continuous(breaks = c(-15, -10, -5, 0, 5), limits = c(-15.5, 7)) +
      labs(x = "Suitability difference from transport land\n(points on a 0-100 scale)",
           y = NULL, title = paste0(model, if (model == "FULL")
             " | 814-species roster" else " | 684-species roster")) +
      theme(axis.line.y = element_blank(), axis.ticks.y = element_blank())
  }
  p <- (land_panel(full, "FULL") | land_panel(env[env$group == "ENV", ], "ENV")) +
    plot_annotation(
      title = "Reclaimed land uses differ in predicted suitability",
      subtitle = "71 cities | All land uses compared with transport land",
      caption = paste("Dots: model estimates; lines: 95% credible intervals.",
                      "Shaded row: bare land. Zero = the same suitability as transport land."),
      tag_levels = "a",
      theme = theme(plot.title = element_text(size = 9, face = "bold"),
                    plot.subtitle = element_text(size = 7),
                    plot.caption = element_text(size = 6.5)))
  save_figure(p, "coastal_land_use", 183, 113,
    paste("Land-use differences from the transport reference in the existing primary species-level",
          "hierarchical models, evaluated at the saved age index of 15 years. All nine prespecified",
          "contrasts are shown, without selection by significance. Dots are posterior mean differences",
          "and lines are saved 95% credible intervals; both are multiplied by 100 solely for display.",
          "These are absolute suitability-score differences, not relative percentage changes.",
          "FULL uses an 814-species roster (811 finite contributors); ENV uses 684 species, across 71 cities.",
          "Native ranges are excluded and missing predictions are not replaced by zero.",
          "The two panels are separate response analyses and use identical axis scales.",
          "No model was refitted. Source labels: full814_bayesian_contrasts, paired_bayesian_contrasts."))
}

if (selected %in% c("all", "formation_cohorts")) {
  city <- read_existing("cohort_city_values")
  effect <- read_existing("cohort_contrasts")
  series <- list(FULL = "FULL_814", ENV = "PAIRED_ENV_684")
  colours <- c(FULL = "#A2554B", ENV = "#3C7983")
  validated <- lapply(names(series), function(nm) {
    d <- city[city$scope == "all_mapped" & city$response == series[[nm]], ]
    e <- effect[effect$scope == "all_mapped" & effect$response == series[[nm]], ]
    stopifnot(nrow(d) == 75, !anyDuplicated(d$city_id), nrow(e) == 1, e$n_cities == 75,
      abs(mean(d$recent_minus_old) - e$recent_minus_old) < 1e-10,
      max(abs(d$recent_mean - d$old_mean - d$recent_minus_old)) < 1e-10)
    list(city = d, effect = e)
  })
  names(validated) <- names(series)
  stopifnot(setequal(validated$FULL$city$city_id, validated$ENV$city$city_id))
  tests <- do.call(rbind, lapply(names(series), function(nm) {
    d <- validated[[nm]]$city
    test <- t.test(d$recent_mean, d$old_mean, paired = TRUE, alternative = "two.sided")
    dif <- d$recent_mean - d$old_mean
    expected_t <- mean(dif) / (sd(dif) / sqrt(length(dif)))
    stopifnot(length(dif) == 75, abs(unname(test$statistic) - expected_t) < 1e-10,
      abs(test$p.value - 2 * pt(-abs(expected_t), df = 74)) < 1e-12)
    data.frame(model = nm, n = length(dif), mean_difference = mean(dif),
      statistic = unname(test$statistic), df = unname(test$parameter), p_value = test$p.value,
      bootstrap_low = validated[[nm]]$effect$ci_low, bootstrap_high = validated[[nm]]$effect$ci_high)
  }))
  write.csv(tests, file.path(out, "source_data", "coastal_formation_cohorts_paired_t_tests.csv"), row.names = FALSE)
  pair_panel <- function(nm, show_y = TRUE) {
    d <- validated[[nm]]$city
    e <- validated[[nm]]$effect
    values <- data.frame(x = rep(c(1, 2), each = nrow(d)),
      suitability = 100 * c(d$old_mean, d$recent_mean))
    stopifnot(nrow(values) == 150, all(values$suitability >= 0), all(values$suitability <= 100))
    set.seed(1901)
    values$draw_x <- values$x + runif(nrow(values), -.16, .16)
    means <- aggregate(suitability ~ x, values, mean)
    means$label <- sprintf("%.1f", means$suitability)
    test <- tests[tests$model == nm, ]
    ci_label <- sprintf("P = %.3f", test$p_value)
    delta_label <- sprintf("Difference: %+.2f [%.2f, %.2f]",
      100 * e$recent_minus_old, 100 * e$ci_low, 100 * e$ci_high)
    ggplot() +
      geom_point(data = values, aes(draw_x, suitability), colour = "#9DA8AC", alpha = .30, size = .9) +
      geom_segment(data = means, aes(x = x - .23, xend = x + .23,
        y = suitability, yend = suitability), linewidth = 1.05, colour = colours[nm]) +
      geom_text(data = means, aes(x = x, y = suitability + 4, label = label),
        family = font, size = 2.8, colour = colours[nm], fontface = "bold") +
      annotate("segment", x = 1, xend = 2, y = 80, yend = 80, linewidth = .35, colour = "#616E73") +
      annotate("segment", x = c(1, 2), xend = c(1, 2), y = 78, yend = 80,
        linewidth = .35, colour = "#616E73") +
      annotate("text", x = 1.5, y = 85, label = ci_label, family = font, size = 2.55, colour = "#354048") +
      annotate("text", x = 1.5, y = 92, label = delta_label, family = font, size = 2.3, colour = "#68737A") +
      scale_x_continuous(breaks = c(1, 2), labels = c("Earlier", "Recent"), limits = c(.55, 2.45),
        expand = expansion(mult = 0)) +
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25), expand = expansion(mult = 0)) +
      labs(title = nm, x = NULL, y = if (show_y) "Predicted suitability (0-100)" else NULL) +
      theme(axis.text.y = if (show_y) element_text() else element_blank(),
        axis.ticks.y = if (show_y) element_line() else element_blank(),
        axis.line.y = if (show_y) element_line() else element_blank(),
        plot.margin = margin(6, 5, 5, 5))
  }
  p <- (pair_panel("FULL") | pair_panel("ENV", FALSE)) + plot_annotation(
    caption = "Grey points: cities; coloured bars: equal-city means. Differences are recent minus earlier.",
    theme = theme(plot.caption = element_text(size = 6.5, hjust = 0, colour = "#5C6269")))
  save_figure(p, "coastal_formation_cohorts", 150, 103,
    paste("Supplementary Fig. 10 | Current suitability on earlier and recent reclaimed land.",
          "Each grey point represents a city's area-weighted mean predicted suitability for an earlier",
          "or recent cohort; points are horizontally jittered for visibility. Coloured horizontal bars",
          "show equal-city means across 75 cities. Brackets compare the two cohorts within the same",
          "cities and report the saved equal-city mean difference (recent minus earlier), with its 95%",
          "percentile interval from city bootstrap resampling. P values are from two-sided paired t-tests",
          "of the city-level differences against zero; they are unadjusted for multiple comparisons.",
          "The bootstrap interval is for the paired difference, not for each cohort mean. Earlier and",
          "recent cohorts have five-year formation-class",
          "endpoints in or before 2000 and in or after 2015, respectively. Values are multiplied by 100",
          "and expressed as points on a 0-100 suitability scale, not relative percentage changes.",
          "FULL and ENV use 814- and 684-species rosters, respectively.",
          "The comparison is cross-sectional and uses current predictions, not repeated observations."))
}

if (selected %in% c("all", "first_records")) {
  city <- read_existing("record_interval_city_values")
  summary <- read_existing("record_interval_summary")
  d <- city[city$unit == "cohort_first_any_target" & city$scope == "all_frozen_pairs" &
              city$cohort_endpoint == "ALL" & city$n_after_units > 0, ]
  s <- summary[summary$unit == "cohort_first_any_target" & summary$scope == "all_frozen_pairs" &
                 summary$cohort_endpoint == "ALL", ]
  stopifnot(nrow(d) == 33, nrow(s) == 1, s$n_after_cities == 33, s$n_after_units == 96,
            sum(d$n_after_units) == 96, all(d$median_lag_upper_years >= d$median_lag_lower_years))
  lower <- s$city_balanced_median_lag_lower_years
  upper <- s$city_balanced_median_lag_upper_years
  stopifnot(lower == 13, upper == 18)
  d$display_city <- tools::toTitleCase(d$city_id)
  aliases <- c(zhanzhou = "Danzhou", hongkong = "Hong Kong", newtaipei = "New Taipei")
  for (id in names(aliases)) d$display_city[d$city_id == id] <- aliases[[id]]
  d <- d[order(d$median_lag_lower_years, d$display_city), ]
  d$position <- seq_len(nrow(d))
  p <- ggplot(d, aes(median_lag_lower_years, position)) +
    annotate("rect", xmin = lower, xmax = upper, ymin = -Inf, ymax = Inf,
             fill = "#EAF0F4") +
    geom_segment(aes(xend = median_lag_upper_years, yend = position),
                 linewidth = 0.8, colour = colours["FULL"]) +
    geom_point(size = 1.3, colour = colours["FULL"]) +
    geom_point(aes(x = median_lag_upper_years), shape = 21, fill = "white",
               size = 1.3, stroke = 0.5, colour = colours["FULL"]) +
    scale_y_continuous(breaks = d$position, labels = d$display_city,
                       expand = expansion(add = 0.7)) +
    scale_x_continuous(breaks = seq(0, 40, 5), limits = c(0, 38),
                       expand = expansion(mult = c(0.01, 0.02))) +
    labs(x = "Years from reclamation to first database record", y = NULL,
         title = "First-record interval: typically 13-18 years",
         subtitle = "33 cities | 96 cohorts with post-reclamation records",
         caption = paste("Each line: a city's median lower-upper bounds.\n",
                         "Shaded band: overall city-balanced median bounds.\n",
                         "Five-year date resolution creates these bounds; they are not confidence intervals.", sep = "")) +
    theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(),
          plot.title = element_text(size = 8.5), plot.subtitle = element_text(size = 7),
          plot.margin = margin(9, 12, 8, 9))
  save_figure(p, "coastal_first_records", 133, 178,
    paste("First database records after reclamation among the existing 33 cities and 96 reclamation",
          "cohorts with an eligible post-formation record. For each city, the line joins the saved",
          "median lower and upper elapsed-year bounds (filled and open endpoints, respectively).",
          "The shaded band marks the saved overall city-balanced median bounds of 13 and 18 years.",
          "Bounds arise from five-year reclamation-date intervals; they are not confidence intervals.",
          "This is the original conditional first-record sample, including the historical Danzhou entry,",
          "not the updated city-selection sample. Its frozen identifiers and records are unchanged.",
          "Of 512 mapped cohort units, 96 had an eligible post-formation first record; 398 had no valid",
          "record year, seven had a record within the formation interval and eleven before it.",
          "Database detection dates can follow actual establishment and do not identify arrival dates.",
          "No data query, species filtering, temporal model or interval estimation was rerun.",
          "Source labels: record_interval_city_values, record_interval_summary."))
}

for (entry in used_sources) stopifnot(identical(hash_file(entry$source), entry$sha256))
writeLines(legends, file.path(out, paste0("COASTAL_HISTORY_LEGENDS_", selected, ".md")))
writeLines(capture.output(sessionInfo()), file.path(out, paste0("coastal_history_session_", selected, ".txt")))
write_json(list(status = "RENDERED_EXISTING_ESTIMATES", backend = "R",
                selection = selected, exports = exports,
                input_sources = do.call(rbind, used_sources),
                no_model_refitting = TRUE, no_new_statistical_intervals = TRUE,
                completed = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")),
           file.path(out, paste0("coastal_history_render_", selected, ".json")),
           auto_unbox = TRUE, pretty = TRUE)
message("All requested figures exported; input checksums unchanged.")
