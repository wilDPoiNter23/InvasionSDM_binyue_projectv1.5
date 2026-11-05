# Extended Data Fig. 1: archived candidate-model validation.
# Panel a: unique single-model fits. Panel b: per-species median validation scores.
# The figure does not measure validation performance of final ensembles.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3L) {
  stop("Usage: Rscript render_candidate_model_validation.R all_models.csv species_summary.csv output_prefix")
}
all_path <- args[[1]]
species_path <- args[[2]]
output_prefix <- args[[3]]

required <- c("ggplot2", "dplyr", "tidyr", "patchwork", "svglite", "ragg")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Missing R packages: ", paste(missing, collapse = ", "))

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

all <- read.csv(all_path, stringsAsFactors = FALSE, check.names = FALSE)
species <- read.csv(species_path, stringsAsFactors = FALSE, check.names = FALSE)
stopifnot(nrow(species) == 813L, length(unique(species$accepted_taxon)) == 813L)

candidate <- all |>
  filter(Type == "individual", metric.eval %in% c("TSS", "ROC"), is.finite(validation)) |>
  select(Species, full.name, algo, metric.eval, validation)

conflicts <- candidate |>
  group_by(Species, full.name, algo, metric.eval) |>
  summarise(n_score = n_distinct(validation), .groups = "drop") |>
  filter(n_score > 1L)
stopifnot(nrow(conflicts) == 0L)

candidate <- candidate |>
  distinct(Species, full.name, algo, metric.eval, .keep_all = TRUE) |>
  mutate(
    algo = recode(algo, GLM = "GLM", MAXNET = "MAXNET", RFd = "RF"),
    algo = factor(algo, levels = c("GLM", "MAXNET", "RF")),
    metric = factor(metric.eval, levels = c("ROC", "TSS"), labels = c("AUC", "TSS"))
  )
stopifnot(n_distinct(candidate$Species) == 784L)
stopifnot(nrow(candidate) == 16554L)
stopifnot(all(candidate$validation >= -1 & candidate$validation <= 1))
stopifnot(all(unique(candidate$Species) %in% species$accepted_taxon))

species_cv <- candidate |>
  group_by(Species, metric) |>
  summarise(score = median(validation), .groups = "drop")
stopifnot(nrow(species_cv) == 1568L, all(is.finite(species_cv$score)))

palette_alg <- c(GLM = "#A7D0C7", MAXNET = "#E8DEA9", RF = "#C7C4D9")
palette_metric <- c(AUC = "#75B2D4", TSS = "#D87979")

theme_fig <- theme_classic(base_size = 7.5, base_family = "Arial") +
  theme(
    axis.line = element_line(linewidth = 0.35, colour = "#202020"),
    axis.ticks = element_line(linewidth = 0.30, colour = "#202020"),
    axis.text = element_text(colour = "#202020", size = 7),
    axis.title = element_text(colour = "#202020", size = 7.5),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 7.5),
    panel.border = element_rect(fill = NA, colour = "#404040", linewidth = 0.30),
    panel.grid = element_blank(),
    legend.position = "none",
    plot.margin = margin(9, 5, 6, 6),
    plot.tag = element_text(face = "bold", size = 9, colour = "#202020"),
    plot.tag.position = c(0.01, 0.99)
  )

p_a <- ggplot(candidate, aes(x = algo, y = validation, fill = algo)) +
  geom_boxplot(
    width = 0.64, linewidth = 0.35,
    outlier.size = 0.28, outlier.alpha = 0.22,
    outlier.stroke = 0, alpha = 0.92
  ) +
  facet_wrap(~ metric, nrow = 1, scales = "free_y") +
  scale_fill_manual(values = palette_alg, drop = FALSE) +
  scale_y_continuous(breaks = seq(-1, 1, 0.25), expand = expansion(mult = c(0.03, 0.04))) +
  labs(x = NULL, y = "Validation score") +
  theme_fig +
  theme(axis.text.x = element_text(angle = 35, hjust = 1, vjust = 1))

p_b <- ggplot(species_cv, aes(x = metric, y = score, fill = metric)) +
  geom_violin(width = 0.78, trim = TRUE, alpha = 0.60, colour = NA) +
  geom_point(
    position = position_jitter(width = 0.20, height = 0, seed = 20261021),
    size = 0.38, alpha = 0.13, shape = 16, colour = "#3E4145"
  ) +
  geom_boxplot(width = 0.18, fill = "white", colour = "#343434", linewidth = 0.36,
               outlier.shape = NA, alpha = 0.83) +
  scale_fill_manual(values = palette_metric, drop = FALSE) +
  scale_y_continuous(limits = c(-0.20, 1.02), breaks = seq(-0.2, 1, 0.2),
                     expand = expansion(mult = c(0, 0))) +
  labs(x = NULL, y = "Median candidate CV score") +
  theme_fig

fig <- (p_a | p_b) +
  plot_layout(widths = c(1.65, 1)) +
  plot_annotation(tag_levels = "a") &
  theme(plot.background = element_rect(fill = "white", colour = NA))

dir.create(dirname(output_prefix), recursive = TRUE, showWarnings = FALSE)
width_in <- 183 / 25.4
height_in <- 78 / 25.4

svglite::svglite(paste0(output_prefix, ".svg"), width = width_in, height = height_in,
                 bg = "white", system_fonts = list(sans = "Arial"))
print(fig)
dev.off()

grDevices::cairo_pdf(paste0(output_prefix, ".pdf"), width = width_in, height = height_in,
                     family = "Arial", bg = "white")
print(fig)
dev.off()

ragg::agg_png(paste0(output_prefix, ".png"), width = width_in, height = height_in,
              units = "in", res = 300, background = "white")
print(fig)
dev.off()

ragg::agg_tiff(paste0(output_prefix, ".tiff"), width = width_in, height = height_in,
               units = "in", res = 600, background = "white", compression = "lzw")
print(fig)
dev.off()

cat("candidate fits:", nrow(candidate) / 2, "across", n_distinct(candidate$Species), "species\n")
cat("species with candidate CV scores:", n_distinct(species_cv$Species), "\n")
print(species_cv |> group_by(metric) |> summarise(median = median(score), q25 = quantile(score, 0.25),
                                             q75 = quantile(score, 0.75), .groups = "drop"))
