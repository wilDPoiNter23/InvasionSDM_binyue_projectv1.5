#!/usr/bin/env Rscript
# Plot saved component estimates for city-wide effective suitable habitat area.
suppressPackageStartupMessages({
  library(ggplot2); library(svglite); library(ragg); library(jsonlite); library(digest)
})
a <- commandArgs(TRUE)
stopifnot(length(a) == 2L, dir.exists(a[1]), dir.exists(a[2]))
root <- a[1]; out <- a[2]
hash_file <- function(p) {
  con <- file(p, "rb"); on.exit(close(con))
  digest(readBin(con, "raw", n = file.info(p, extra_cols = FALSE)$size),
         algo = "sha256", serialize = FALSE)
}
dirs <- c(FULL_814 = file.path(root, "full814_sem"),
          MATCHED = file.path(root, "paired_sem"))
paths <- unlist(lapply(dirs, function(d) file.path(d, c("effects.csv", "edges.csv", "draws.csv", "support.csv", "verification.json"))))
hashes <- vapply(paths, hash_file, "")
stopifnot(all(file.exists(paths)))
sources <- lapply(dirs, function(dir) {
  verification <- fromJSON(file.path(dir, "verification.json"))
  stopifnot(verification$status == "PASS_COMPONENT_PATH_NUMERICAL_QA")
  list(effects = read.csv(file.path(dir, "effects.csv")),
       edges = read.csv(file.path(dir, "edges.csv")),
       draws = read.csv(file.path(dir, "draws.csv")),
       support = read.csv(file.path(dir, "support.csv")))
})
rows <- list()
for (pool in names(sources)) for (model in if (pool == "FULL_814") "FULL" else c("FULL", "ENV")) {
  for (threshold in c("T10", "MAX_TSS")) {
    case <- paste(model, threshold, "mean_area", sep = "_")
    z <- sources[[pool]]; eff <- z$effects[z$effects$case == case, ]
    edge <- z$edges[z$edges$case == case & z$edges$response == "y" & z$edges$predictor == "recl", ]
    ds <- z$draws[z$draws$case == case, ]; support <- z$support[z$support$case == case, ]
    stopifnot(nrow(edge) == 1L, nrow(support) == 1L, nrow(ds) == 1999L,
      support$n == 83L, identical(as.integer(support$frozen_species_n), if (pool == "FULL_814") 814L else 684L))
    for (type in c("direct", "indirect", "total")) {
      e <- eff[eff$effect == type, ]; stopifnot(nrow(e) == 1L, sum(is.finite(ds[[type]])) == 1999L)
      ci <- quantile(ds[[type]], c(.025, .975), na.rm = TRUE, names = FALSE)
      stopifnot(max(abs(ci - c(e$lower, e$upper))) < 1e-10,
                type != "direct" || abs(edge$estimate - e$estimate) < 1e-10)
      lower_tail <- sum(ds[[type]] <= 0)
      upper_tail <- sum(ds[[type]] >= 0)
      # Invert the equal-tailed percentile interval with a finite-sample correction.
      bootstrap_p <- min(1, 2 * (min(lower_tail, upper_tail) + 1) / (nrow(ds) + 1))
      rows[[length(rows) + 1L]] <- data.frame(pool = pool, model = model,
        threshold = threshold, effect = type, estimate = e$estimate, lower = e$lower,
        upper = e$upper, direct_t_p = if (type == "direct") edge$p else NA_real_,
        bootstrap_p = bootstrap_p, bootstrap_nonpositive = lower_tail,
        bootstrap_nonnegative = upper_tail,
        n_cities = support$n, n_species = support$frozen_species_n,
        bootstrap_draws = nrow(ds), ci_excludes_zero = e$lower > 0 || e$upper < 0,
        model_case = case)
    }
  }
}
d <- do.call(rbind, rows)
stopifnot(nrow(d) == 18L, all(d$lower <= d$estimate & d$estimate <= d$upper))
d$series <- ifelse(d$pool == "FULL_814", "FULL\nall species",
                   ifelse(d$model == "FULL", "FULL\nmatched species", "ENV\nmatched species"))
d$series <- factor(d$series, levels = c("FULL\nall species", "FULL\nmatched species", "ENV\nmatched species"))
d$threshold_label <- factor(ifelse(d$threshold == "T10", "10% omission threshold", "Maximum TSS threshold"),
  levels = c("10% omission threshold", "Maximum TSS threshold"))
d$effect <- factor(d$effect, levels = c("total", "indirect", "direct"),
  labels = c("Total", "Indirect", "Direct"))
d$p_label <- sprintf("P = %.3f", d$bootstrap_p)
colours <- c(Direct = "#6F4C9B", Indirect = "#178F7A", Total = "#7D898D")
p <- ggplot(d, aes(estimate, effect, colour = effect)) +
  geom_vline(xintercept = 0, colour = "#BAC1C3", linewidth = .35) +
  geom_errorbar(aes(xmin = lower, xmax = upper), orientation = "y", width = .14, linewidth = .55) +
  geom_point(aes(shape = effect), size = 2.4, stroke = .65) +
  geom_text(aes(x = .185, label = p_label), hjust = 0, size = 2.2,
    colour = "#303638", family = "Arial", show.legend = FALSE) +
  facet_grid(series ~ threshold_label, switch = "y") +
  scale_colour_manual(values = colours, guide = "none") +
  scale_shape_manual(values = c(Direct = 16, Indirect = 18, Total = 15), guide = "none") +
  scale_x_continuous(limits = c(-.26, .33), breaks = c(-.2, -.1, 0, .1), expand = c(0, 0)) +
  labs(x = "Standardized pathway estimate", y = NULL,
       caption = "Points: estimates; bars: 95% city bootstrap intervals; P: unadjusted two-sided bootstrap values.") +
  theme_classic(base_family = "Arial", base_size = 8) +
  theme(strip.background = element_rect(fill = "#F1F3F3", colour = NA),
    strip.text.x = element_text(size = 8, face = "bold", margin = margin(4, 0, 4, 0)),
    strip.text.y.left = element_text(size = 7.5, face = "bold", angle = 0, margin = margin(0, 5, 0, 5)),
    axis.text = element_text(size = 7, colour = "#343A3C"), axis.title = element_text(size = 8),
    axis.line = element_line(linewidth = .35), axis.ticks = element_line(linewidth = .35),
    panel.spacing = grid::unit(3, "mm"),
    plot.caption = element_text(size = 7, hjust = 0, margin = margin(t = 7)),
    plot.margin = margin(5, 6, 6, 6))
stem <- "suitable_area_pathway_sensitivity"
dest <- file.path(out, stem)
svglite(paste0(dest, ".svg"), width = 183 / 25.4, height = 140 / 25.4, bg = "white"); print(p); dev.off()
cairo_pdf(paste0(dest, ".pdf"), width = 183 / 25.4, height = 140 / 25.4, family = "Arial", bg = "white"); print(p); dev.off()
agg_png(file.path(out, "preview", paste0(stem, ".png")), width = 183, height = 140,
  units = "mm", res = 300, background = "white"); print(p); dev.off()
write.csv(d, file.path(out, "tables", paste0(stem, "_statistics.csv")), row.names = FALSE)
stopifnot(identical(hashes, vapply(paths, hash_file, "")))
write_json(list(status = "EXPORTED", source_paths = paths, source_sha256 = as.list(hashes),
  source_unchanged = TRUE, model_refits = 0, sdm_refits = 0,
  rows = nrow(d), bootstrap = "1999 saved city resamples; percentile 95% confidence intervals",
  figure_display = "All three pathway components use percentile bootstrap intervals and two-sided bootstrap-derived P values",
  bootstrap_p = "min(1, 2*(1+min(number of resampled estimates <=0, number >=0))/(B+1)); B=1999; unadjusted",
  bootstrap_p_interpretation = "Approximate inversion of the equal-tailed percentile bootstrap interval; not a null-constrained bootstrap test",
  direct_t_p = "Original two-sided component-regression t test retained in the table but not plotted",
  response = "Standardized log1p city-wide effective suitable habitat area; native ranges excluded",
  scope = "Exploratory fixed predictor PCA component model; not original SEM replication",
  species = c(full_all = 814, matched = 684), complete_cities = 83),
  paste0(dest, "_receipt.json"), pretty = TRUE, auto_unbox = TRUE)
print(d[d$effect == "Direct", c("series", "threshold", "estimate", "lower", "upper", "direct_t_p")])
message("Exported figure: ", dest)
