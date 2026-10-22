# Plot the selected ensemble procedures as replacement Extended Data Fig. 1b.
# Usage: Rscript render_foldmatched_ensemble_validation.R manifest.csv folds_dir output_prefix [unavailable.csv]

args <- commandArgs(trailingOnly = TRUE)
if (!length(args) %in% c(3L, 4L)) {
  stop("Usage: Rscript render_foldmatched_ensemble_validation.R manifest.csv folds_dir output_prefix [unavailable.csv]")
}
required <- c("ggplot2", "dplyr", "patchwork", "svglite", "ragg")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Missing R packages: ", paste(missing, collapse = ", "))

library(ggplot2)
library(dplyr)
library(patchwork)

manifest <- read.csv(args[1], stringsAsFactors = FALSE, check.names = FALSE)
if (anyDuplicated(manifest$accepted_taxon)) stop("Manifest has duplicate species")
files <- file.path(args[2], sprintf("%04d.csv", seq_len(nrow(manifest))))
missing <- which(!file.exists(files))
unavailable <- data.frame(row_index = integer(), accepted_taxon = character(), reason = character())
if (length(args) == 4L) {
  unavailable <- read.csv(args[4], stringsAsFactors = FALSE, check.names = FALSE)
  if (!all(c("row_index", "accepted_taxon", "reason") %in% names(unavailable)) ||
      anyDuplicated(unavailable$row_index) || any(!nzchar(unavailable$reason)) ||
      !setequal(as.integer(unavailable$row_index), missing) ||
      any(unavailable$accepted_taxon != manifest$accepted_taxon[as.integer(unavailable$row_index)])) {
    stop("Unavailable-species register does not exactly match missing result files")
  }
} else if (length(missing)) {
  stop("Missing species results require an explicit unavailable-species register: ",
       paste(missing, collapse = ", "))
}
raw <- lapply(setdiff(seq_along(files), missing), function(i) {
  d <- read.csv(files[i], stringsAsFactors = FALSE, check.names = FALSE)
  if (!nrow(d) || any(d$accepted_taxon != manifest$accepted_taxon[i]) ||
      any(d$method != manifest$selected_method[i])) {
    stop("Result does not match manifest row ", i)
  }
  d
})
folds <- bind_rows(raw)
if (anyDuplicated(paste(folds$accepted_taxon, folds$pa, folds$run))) {
  stop("Duplicate species, PA and fold combination")
}
passed <- folds$status == "PASS"
if (any(!is.finite(folds$auc[passed])) || any(!is.finite(folds$tss[passed])) ||
    any(folds$auc[passed] < 0 | folds$auc[passed] > 1) ||
    any(folds$tss[passed] < -1 | folds$tss[passed] > 1) ||
    any(folds$n_members[passed] < 2L)) {
  stop("Passed fold contains invalid metric or member count")
}
coverage <- folds |>
  group_by(accepted_taxon, source_generation, method) |>
  summarise(n_folds = n(), n_pass = sum(status == "PASS"),
            min_members = if (any(status == "PASS")) min(n_members[status == "PASS"]) else NA_integer_,
            .groups = "drop") |>
  mutate(eligible = n_pass >= 2L)
species <- folds |>
  filter(status == "PASS") |>
  group_by(accepted_taxon, source_generation, method) |>
  summarise(auc = median(auc), tss = median(tss), .groups = "drop") |>
  inner_join(coverage |> filter(eligible),
             by = c("accepted_taxon", "source_generation", "method"))
if (!nrow(species)) stop("No species has complete fold-matched ensemble validation")

source_folds <- folds |>
  select(accepted_taxon, source_generation, method, pa, run, tier, n_members,
         n_train, n_holdout, n_train_scored, n_holdout_scored,
         threshold, auc, tss, status)
source_species <- species |>
  select(accepted_taxon, source_generation, method, n_folds, n_pass, min_members, auc, tss)
prefix <- args[3]
dir.create(dirname(prefix), recursive = TRUE, showWarnings = FALSE)
write.csv(source_folds, paste0(prefix, "_fold_source_data.csv"), row.names = FALSE, na = "")
write.csv(source_species, paste0(prefix, "_species_source_data.csv"), row.names = FALSE, na = "")
write.csv(coverage, paste0(prefix, "_coverage.csv"), row.names = FALSE, na = "")
write.csv(unavailable, paste0(prefix, "_unavailable.csv"), row.names = FALSE, na = "")

plot_data <- rbind(
  data.frame(accepted_taxon = species$accepted_taxon, metric = "AUC",
             score = species$auc),
  data.frame(accepted_taxon = species$accepted_taxon, metric = "TSS",
             score = species$tss)
)
plot_data$metric <- factor(plot_data$metric, levels = c("AUC", "TSS"))
palette <- c(AUC = "#82B8D5", TSS = "#DF9992")
figure <- ggplot(plot_data, aes(x = metric, y = score, fill = metric)) +
  geom_violin(width = 0.77, trim = TRUE, alpha = 0.82, colour = NA) +
  geom_point(aes(colour = metric),
             position = position_jitter(width = 0.18, height = 0, seed = 20261022),
             size = 0.34, alpha = 0.22) +
  geom_boxplot(width = 0.19, outlier.shape = NA, fill = "white",
               colour = "#3C3C3C", linewidth = 0.39) +
  scale_fill_manual(values = palette) +
  scale_colour_manual(values = palette) +
  scale_y_continuous(limits = c(-0.10, 1.02),
                     breaks = c(0, 0.25, 0.50, 0.75, 1),
                     expand = expansion(mult = c(0, 0))) +
  labs(x = NULL, y = "Held-out validation score", tag = "b") +
  theme_classic(base_size = 7.5, base_family = "sans") +
  theme(axis.line = element_blank(),
        axis.ticks = element_line(linewidth = 0.3, colour = "#333333"),
        axis.text = element_text(size = 7, colour = "#333333"),
        axis.title = element_text(size = 7.4, colour = "#333333"),
        panel.border = element_rect(fill = NA, colour = "#333333", linewidth = 0.4),
        panel.grid = element_blank(), legend.position = "none",
        plot.tag = element_text(face = "bold", size = 9),
        plot.tag.position = c(0.01, 0.99),
        plot.margin = margin(5, 5, 4, 5),
        plot.background = element_rect(fill = "white", colour = NA))
width_in <- 82 / 25.4
height_in <- 62 / 25.4
svglite::svglite(paste0(prefix, ".svg"), width = width_in, height = height_in,
                 bg = "white", system_fonts = list(sans = "Liberation Sans"))
print(figure)
dev.off()
grDevices::pdf(paste0(prefix, ".pdf"), width = width_in,
               height = height_in, family = "Helvetica", bg = "white")
print(figure)
dev.off()
ragg::agg_png(paste0(prefix, ".png"), width = width_in, height = height_in,
              units = "in", res = 300, background = "white")
print(figure)
dev.off()
ragg::agg_tiff(paste0(prefix, ".tiff"), width = width_in, height = height_in,
               units = "in", res = 600, background = "white", compression = "lzw")
print(figure)
dev.off()

summary <- species |>
  summarise(n_species = n(), median_auc = median(auc), q25_auc = quantile(auc, .25),
            q75_auc = quantile(auc, .75), median_tss = median(tss),
            q25_tss = quantile(tss, .25), q75_tss = quantile(tss, .75))
write.csv(summary, paste0(prefix, "_summary.csv"), row.names = FALSE)
caption <- paste0(
  "Extended Data Fig. 1b | Held-out performance of fold-matched ensemble species distribution models. ",
  "The aggregation rule matches each species' retained projection (n = ", nrow(species), " species). ",
  "Each point is a species median across at least two evaluable held-out folds; violins show ",
  "species-level distributions, boxes show medians and interquartile ranges, and whiskers extend ",
  "to 1.5 times the interquartile range. Constituent models and TSS cutoffs were selected using ",
  "training-fold data before evaluation on held-out records. AUC, area under the receiver ",
  "operating characteristic curve; TSS, true skill statistic."
)
writeLines(caption, paste0(prefix, "_caption.txt"), useBytes = TRUE)
print(summary)
cat("Species with two or more valid folds:", nrow(species), "of", nrow(manifest), "\n")
cat("Unavailable archived fold data:", nrow(unavailable), "\n")
cat("Passing folds:", sum(passed), "of", nrow(folds), "\n")
