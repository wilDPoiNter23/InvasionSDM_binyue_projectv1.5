# Original source: make_figures_skill_v3.R
# Original source location: ${NON_NATIVE_ROOT}/temp/make_figures_skill_v3.R
# Original source SHA-256: 8651d93874357ec568cee9382383e214195e2755f7bb16bf8b93ac3b504126e7
# Recovery: live read-only SSH, 2026-10-12
# Classification: SUPPORTING (HISTORICAL_SUPPORTING_AUDIT_LINEAGE_HELPER)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
.libPaths(c("<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu", .libPaths()))
suppressPackageStartupMessages({
  library(ggplot2); library(patchwork); library(ragg); library(svglite)
  library(terra)   # data preparation only (non-visual)
})
FIG <- "<NON_NATIVE_ROOT>/publication_figures"

# ---- unified theme (nature-figure R quick-start, Helvetica since Arial absent on this host) ----
theme_set(
  theme_classic(base_size = 6.5, base_family = "Helvetica") +
    theme(
      axis.line = element_line(linewidth = 0.35, colour = "black"),
      axis.ticks = element_line(linewidth = 0.35, colour = "black"),
      legend.title = element_text(size = 6.2),
      legend.text = element_text(size = 5.8),
      strip.text = element_text(size = 6.2, face = "bold"),
      plot.title = element_text(size = 7, face = "bold"),
      panel.grid = element_blank()
    )
)
PAL_TSS <- "#4C72B0"; PAL_ROC <- "#55A868"; PAL_SCAT <- "#8172B3"
PAL_FB <- "#C44E52"; PAL_PRIM <- "#4C72B0"; PAL_BT <- "#DD8452"
MAP_PAL <- c("#F7FBFF", "#DEEBF7", "#9ECAE1", "#4292C6", "#2171B5", "#08519C")

save_pub <- function(plot, name, w_mm, h_mm) {
  w <- w_mm / 25.4; h <- h_mm / 25.4
  svglite::svglite(file.path(FIG, paste0(name, "_v3.svg")), width = w, height = h)
  print(plot); dev.off()
  grDevices::pdf(file.path(FIG, paste0(name, "_v3.pdf")), width = w, height = h, family = "Helvetica")
  print(plot); dev.off()
  ragg::agg_png(file.path(FIG, paste0(name, "_v3.png")), width = w, height = h, units = "in", res = 600)
  print(plot); dev.off()
  cat("saved", name, "\n")
}

# ================= FIGURE 1 — schematic (ggplot) =================
stages <- data.frame(
  x = 1:6, y = 3,
  title = c("1. Occurrence data", "2. Predictors", "3. SDM fitting",
            "4. Evaluation", "5. Projection", "6. Aggregation"),
  l1 = c("GBIF + supplementary", "35 layers, 2.5 arc-min", "GLM + RF + MAXNET",
         "PRIMARY: TSS>0.8, ROC>0.8", "global 4320 x 8640 grid", "per-species min-max norm."),
  l2 = c("813 accepted taxa", "climate/soil/topo/", "4-fold CV, 4 PA sets",
         "FALLBACK: TSS>0.7, ROC>0.7", "ensemble + single-model", "mean across 813 taxa"),
  l3 = c("precision + uncertainty QC", "human + nightlights", "BIOMOD2 4.2-6-1",
         "held-out validation only", "SHA-256 checksums", "37,324,800 cells")
)
g1 <- ggplot(stages) +
  geom_rect(aes(xmin = x - 0.42, xmax = x + 0.42, ymin = y - 0.72, ymax = y + 0.72),
            fill = "#DEEBF7", colour = PAL_PRIM, linewidth = 0.45) +
  geom_segment(data = stages[1:5, ],
               aes(x = x + 0.42, xend = x + 0.58, y = y, yend = y),
               arrow = arrow(length = unit(0.09, "cm"), type = "closed"),
               linewidth = 0.6, colour = PAL_PRIM) +
  geom_text(aes(x, y + 0.5, label = title), fontface = "bold", size = 2.1) +
  geom_text(aes(x, y + 0.18, label = l1), size = 1.7) +
  geom_text(aes(x, y - 0.02, label = l2), size = 1.7) +
  geom_text(aes(x, y - 0.22, label = l3), size = 1.7) +
  scale_x_continuous(expand = expansion(mult = 0.02)) +
  scale_y_continuous(limits = c(1.6, 4.15), expand = c(0, 0)) +
  labs(title = "Workflow: occurrence data to global invasion-pressure dataset") +
  theme_void(base_size = 6.5, base_family = "Helvetica") +
  theme(plot.title = element_text(size = 7, face = "bold", hjust = 0.5))
save_pub(g1, "figure1_workflow", 183, 110)

# ================= FIGURE 2 — occurrence coverage =================
ps <- read.csv(file.path(FIG, "figure2_records_per_species_813.csv"), stringsAsFactors = FALSE)
rich <- rast(file.path(FIG, "figure2_species_richness_813.tif"))
moll <- project(rich, "ESRI:54009", method = "near")
agg2 <- aggregate(moll, fact = 16, fun = "max", na.rm = TRUE)
d2 <- as.data.frame(agg2, xy = TRUE, na.rm = TRUE)
names(d2)[3] <- "rich"

pa <- ggplot(ps, aes(x = log10(N))) +
  geom_histogram(bins = 40, fill = PAL_ROC, colour = "white", linewidth = 0.15) +
  labs(x = expression(log[10] ~ "records per taxon"), y = "Number of taxa") +
  annotate("text", x = Inf, y = Inf, label = "a", fontface = "bold", size = 3.2,
           hjust = 1.6, vjust = 1.5) +
  annotate("text", x = -Inf, y = Inf, hjust = -0.15, vjust = 1.6, size = 2.0,
           label = sprintf("n = %d taxa\nmedian = %s records",
                           nrow(ps), format(median(ps$N), big.mark = ",")))
pb <- ggplot(d2, aes(x = x, y = y, fill = rich)) +
  geom_raster() +
  scale_fill_gradientn(colours = MAP_PAL, na.value = "white",
                       name = "Taxa per cell", limits = c(0, 315)) +
  coord_equal() +
  annotate("text", x = Inf, y = Inf, label = "b", fontface = "bold", size = 3.2,
           hjust = 1.6, vjust = 1.5) +
  theme_void(base_size = 6.5, base_family = "Helvetica") +
  theme(legend.title = element_text(size = 6.2), legend.text = element_text(size = 5.8),
        legend.key.size = unit(0.32, "cm"), legend.position = "right")
g2 <- pa + pb + plot_layout(widths = c(1, 1.4)) +
  plot_annotation(title = "Occurrence-data coverage (cleaned archive, before spatial thinning)",
                  theme = theme(plot.title = element_text(size = 7, face = "bold", hjust = 0.5)))
save_pub(g2, "figure2_occurrence_coverage", 183, 95)

# ================= FIGURE 3 — model validation =================
d <- read.csv(file.path(FIG, "figure3_model_validation_data_v2.csv"), stringsAsFactors = FALSE)
d$tier <- factor(d$selection_tier, levels = c("PRIMARY", "FALLBACK", "BELOW_THRESHOLD"))
f3a <- ggplot(d, aes(x = best_TSS)) +
  geom_histogram(bins = 40, fill = PAL_TSS, colour = "white", linewidth = 0.15) +
  geom_vline(xintercept = 0.8, linetype = "dashed", colour = "red3", linewidth = 0.3) +
  labs(x = "TSS", y = "Number of taxa")
f3b <- ggplot(d, aes(x = best_ROC)) +
  geom_histogram(bins = 40, fill = PAL_ROC, colour = "white", linewidth = 0.15) +
  geom_vline(xintercept = 0.8, linetype = "dashed", colour = "red3", linewidth = 0.3) +
  labs(x = "ROC (AUC)", y = "Number of taxa")
f3c <- ggplot(d, aes(x = best_TSS, y = best_ROC)) +
  geom_point(size = 0.35, alpha = 0.45, colour = PAL_SCAT) +
  geom_hline(yintercept = 0.8, linetype = "dashed", colour = "red3", linewidth = 0.3) +
  geom_vline(xintercept = 0.8, linetype = "dashed", colour = "red3", linewidth = 0.3) +
  coord_cartesian(xlim = c(0.45, 1.02), ylim = c(0.75, 1.02)) +
  labs(x = "TSS", y = "ROC (AUC)")
f3d <- ggplot(d, aes(x = tier, y = best_TSS, fill = tier)) +
  geom_boxplot(outlier.size = 0.3, linewidth = 0.3) +
  scale_fill_manual(values = c("PRIMARY" = PAL_PRIM, "FALLBACK" = PAL_FB, "BELOW_THRESHOLD" = PAL_BT), guide = "none") +
  scale_x_discrete(labels = c("PRIMARY" = "PRIMARY\n(n = 663)", "FALLBACK" = "FALLBACK\n(n = 114)",
                              "BELOW_THRESHOLD" = "BELOW\n(n = 36)")) +
  geom_hline(yintercept = 0.8, linetype = "dashed", colour = "red3", linewidth = 0.3) +
  labs(x = "Selection tier", y = "TSS")
g3 <- (f3a | f3b) / (f3c | f3d) +
  plot_annotation(tag_levels = "a", tag_prefix = "", tag_suffix = "",
                  title = "Cross-validated model performance (n = 813 taxa)",
                  theme = theme(plot.title = element_text(size = 7, face = "bold", hjust = 0.5),
                                plot.tag = element_text(size = 8, face = "bold")))
save_pub(g3, "figure3_model_validation", 183, 140)

# ================= FIGURE 4 — aggregate map =================
agg <- rast("<WORK_ROOT>/results/aggregation_publication_813_clean_v1_20260813/hotspot_mean_suitability_normalized.tif")
moll4 <- project(agg, "ESRI:54009", method = "bilinear")
v <- values(moll4); v[v < 0] <- 0; v <- ifelse(v > 0.6, 0.6, v); values(moll4) <- v
agg4 <- aggregate(moll4, fact = 12, fun = "mean", na.rm = TRUE)
d4 <- as.data.frame(agg4, xy = TRUE, na.rm = TRUE)
names(d4)[3] <- "suit"
g4 <- ggplot(d4, aes(x = x, y = y, fill = suit)) +
  geom_raster() +
  scale_fill_gradientn(colours = MAP_PAL, na.value = "white",
                       name = "Mean normalized\nsuitability", limits = c(0, 0.6)) +
  coord_equal() +
  theme_void(base_size = 6.5, base_family = "Helvetica") +
  theme(legend.title = element_text(size = 6.2), legend.text = element_text(size = 5.8),
        legend.key.size = unit(0.34, "cm"), legend.position = "right") +
  labs(title = "Mean normalized suitability across 813 invasive plant taxa",
       caption = "colour scale capped at 0.6 for display (data max = 0.7650)") +
  theme(plot.title = element_text(size = 7, face = "bold", hjust = 0.5),
        plot.caption = element_text(size = 5.5, hjust = 0.5, colour = "grey30"))
save_pub(g4, "figure4_aggregate_map", 183, 95)

cat("DONE ALL\n")
