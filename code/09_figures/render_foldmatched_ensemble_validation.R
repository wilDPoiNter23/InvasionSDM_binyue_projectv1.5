# Plot held-out performance of fold-matched EMmean and EMmedian reconstructions.
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

counts <- table(factor(species$method, levels = c("EMmean", "EMmedian")))
labels <- paste0(names(counts), "\n(n = ", as.integer(counts), ")")
names(labels) <- names(counts)
palette <- c(EMmean = "#89B8B4", EMmedian = "#D7AF91")
theme_fig <- theme_classic(base_size = 7.5, base_family = "sans") +
  theme(axis.line = element_line(linewidth = 0.35, colour = "#252525"),
        axis.ticks = element_line(linewidth = 0.30, colour = "#252525"),
        axis.text = element_text(size = 7, colour = "#252525"),
        axis.title = element_text(size = 7.5, colour = "#252525"),
        panel.border = element_rect(fill = NA, colour = "#555555", linewidth = 0.3),
        panel.grid = element_blank(), legend.position = "none",
        plot.tag = element_text(face = "bold", size = 9),
        plot.tag.position = c(0.01, 0.99),
        plot.margin = margin(8, 7, 6, 7),
        plot.background = element_rect(fill = "white", colour = NA))

panel <- function(metric, y_label, lower) {
  ggplot(species, aes(x = method, y = .data[[metric]], fill = method)) +
    geom_violin(width = 0.76, trim = TRUE, alpha = 0.52, colour = NA) +
    geom_point(position = position_jitter(width = 0.16, height = 0, seed = 20261022),
               size = 0.38, alpha = 0.16, colour = "#343434") +
    geom_boxplot(width = 0.18, outlier.shape = NA, fill = "white",
                 colour = "#343434", linewidth = 0.38) +
    scale_fill_manual(values = palette, drop = FALSE) +
    scale_x_discrete(limits = c("EMmean", "EMmedian"), labels = labels) +
    scale_y_continuous(limits = c(lower, 1.01), breaks = pretty(c(lower, 1), 6),
                       expand = expansion(mult = c(0, 0))) +
    labs(x = NULL, y = y_label) + theme_fig
}

p_auc <- panel("auc", "Held-out AUC", min(0, min(species$auc) - 0.03))
p_tss <- panel("tss", "Held-out TSS", min(-0.2, min(species$tss) - 0.03))
figure <- (p_auc | p_tss) + plot_layout(widths = c(1, 1)) +
  plot_annotation(tag_levels = "a")
width_in <- 183 / 25.4
height_in <- 76 / 25.4
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
  group_by(method) |>
  summarise(n_species = n(), median_auc = median(auc), q25_auc = quantile(auc, .25),
            q75_auc = quantile(auc, .75), median_tss = median(tss),
            q25_tss = quantile(tss, .25), q75_tss = quantile(tss, .75),
            .groups = "drop")
write.csv(summary, paste0(prefix, "_summary.csv"), row.names = FALSE)
caption <- paste0(
  "Extended Data Fig. 1 | Held-out performance of fold-matched ensemble species distribution models. ",
  "a, Area under the receiver operating characteristic curve (AUC); b, true skill statistic (TSS). ",
  "For each archived validation fold, at least two constituent models were selected by their calibration scores ",
  "on training records, and predictions were combined by the EMmean or EMmedian method used for that species' final projection. ",
  "The TSS threshold was set on training records and applied to the held-out fold. ",
  "Points are species medians across held-out folds; boxes show medians and interquartile ranges, ",
  "and violins show species-level distributions. Of ", nrow(manifest),
  " final projections with an identifiable ensemble method, ", nrow(unavailable),
  " lacked readable archived fold data and ", nrow(coverage) - nrow(species),
  " had fewer than two evaluable folds. The remaining species are shown ",
  "(EMmean, n = ", counts[["EMmean"]], "; EMmedian, n = ", counts[["EMmedian"]], "). ",
  "The two methods apply to different species and are not a paired comparison. ",
  "These fold-specific ensembles test the aggregation procedure used for the final projections; ",
  "their scores are not validation scores stored for the final full-data ensemble objects."
)
writeLines(caption, paste0(prefix, "_caption.txt"), useBytes = TRUE)
print(summary)
cat("Species with two or more valid folds:", nrow(species), "of", nrow(manifest), "\n")
cat("Unavailable archived fold data:", nrow(unavailable), "\n")
cat("Passing folds:", sum(passed), "of", nrow(folds), "\n")
