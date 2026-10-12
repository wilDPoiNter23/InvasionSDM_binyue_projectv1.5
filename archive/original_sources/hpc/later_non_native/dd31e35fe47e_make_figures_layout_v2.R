# Original source: make_figures_layout_v2.R
# Original source location: ${NON_NATIVE_ROOT}/temp/make_figures_layout_v2.R
# Original source SHA-256: dd31e35fe47eeb5d3ff9518a96b8741556d8e7a770b620f1fbfe32fee7c8e3e8
# Recovery: live read-only SSH, 2026-10-12
# Classification: SUPPORTING (HISTORICAL_SUPPORTING_AUDIT_LINEAGE_HELPER)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
.libPaths(c("<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu", .libPaths()))
suppressPackageStartupMessages(library(terra))
FIG <- "<NON_NATIVE_ROOT>/publication_figures"

# ---- unified journal style helpers ----
panel_label <- function(txt, x = 0.02, y = 0.96, cex = 1.25) {
  u <- par("usr")
  text(u[1] + (u[2] - u[1]) * x, u[3] + (u[4] - u[3]) * y, txt, font = 2, cex = cex, adj = c(0, 1))
}
PAL_TSS <- "#4C72B0"; PAL_ROC <- "#55A868"; PAL_SCAT <- "#8172B3"
PAL_FB <- "#C44E52"; PAL_PRIM <- "#4C72B0"; PAL_BT <- "#DD8452"
MAP_PAL <- colorRampPalette(c("#F7FBFF", "#DEEBF7", "#9ECAE1", "#4292C6", "#2171B5", "#08519C"))(256)

# ================= FIGURE 1 — workflow (double column, 180 mm) =================
pdf(file.path(FIG, "figure1_workflow.pdf"), width = 7.09, height = 4.0)
par(mar = c(1.2, 1.2, 2.2, 1.2))
plot(NA, xlim = c(0, 126), ylim = c(0, 62), axes = FALSE, xlab = "", ylab = "")
title("Workflow: occurrence data to global invasion-pressure dataset", cex.main = 1.15)
boxw <- 18; boxh <- 14; y <- 32
xs <- seq(5, 121, length.out = 6)
drawbox <- function(x, num, title, lines, col = "#DEEBF7") {
  rect(x - boxw / 2, y - boxh / 2, x + boxw / 2, y + boxh / 2, col = col, border = "#4C72B0", lwd = 1.4)
  text(x, y + boxh / 2 - 2.4, paste0(num, ". ", title), font = 2, cex = 0.82)
  for (i in seq_along(lines)) text(x, y + boxh / 2 - 5.0 - (i - 1) * 2.9, lines[i], cex = 0.66)
}
drawbox(xs[1], 1, "Occurrence data", c("GBIF + supplementary", "813 accepted taxa", "precision + uncertainty QC"))
drawbox(xs[2], 2, "Predictors", c("35 layers, 2.5 arc-min", "climate / soil / topo /", "human / nightlights"))
drawbox(xs[3], 3, "SDM fitting", c("GLM + RF + MAXNET", "4-fold CV, 4 PA sets", "BIOMOD2 4.2-6-1"))
drawbox(xs[4], 4, "Evaluation", c("PRIMARY: TSS>0.8, ROC>0.8", "FALLBACK: TSS>0.7, ROC>0.7", "held-out validation only"))
drawbox(xs[5], 5, "Projection", c("global 4320 x 8640 grid", "ensemble + single-model", "SHA-256 checksums"))
drawbox(xs[6], 6, "Aggregation", c("per-species min-max norm.", "mean across 813 taxa", "37,324,800 cells"))
arrows(xs[-6] + boxw / 2, y, xs[-1] - boxw / 2, y, length = 0.11, lwd = 2, col = "#4C72B0")
text(63, 9, "All products pass lineage / geometry / numeric / execution QC gates", cex = 0.72, col = "grey30", font = 3)
dev.off()

# ================= FIGURE 2 — occurrence coverage (double column) =================
ps <- read.csv(file.path(FIG, "figure2_records_per_species_813.csv"), stringsAsFactors = FALSE)
rich <- rast(file.path(FIG, "figure2_species_richness_813.tif"))
moll <- project(rich, "ESRI:54009", method = "near")

pdf(file.path(FIG, "figure2_occurrence_coverage.pdf"), width = 7.09, height = 3.2)
layout(matrix(c(1, 1, 2), nrow = 1), widths = c(1, 1.35))
par(mar = c(4.2, 4.4, 3.2, 0.6), mgp = c(2.6, 0.7, 0))
hist(log10(ps$N), breaks = 40, col = PAL_ROC, border = "white",
     main = "", xlab = expression(log[10]~"records per taxon"),
     ylab = "Number of taxa", cex.lab = 1.05, cex.axis = 0.95)
panel_label("a")
mtext("Presence records per taxon", side = 3, line = 0.6, cex = 0.9, font = 2)
mtext(sprintf("n = %d taxa; median = %s records", nrow(ps), format(median(ps$N), big.mark = ",")),
      side = 1, line = 2.8, cex = 0.68)
par(mar = c(0.4, 0.4, 3.2, 0.4))
plot(moll, col = MAP_PAL, axes = FALSE, box = FALSE, legend = TRUE,
     main = "", legend.shrink = 0.75)
panel_label("b")
mtext("Occurrence richness (taxa per 2.5 arc-min cell)", side = 3, line = 0.6, cex = 0.9, font = 2)
dev.off()

# ================= FIGURE 3 — model validation (1.5 column) =================
d <- read.csv(file.path(FIG, "figure3_model_validation_data_v2.csv"), stringsAsFactors = FALSE)

pdf(file.path(FIG, "figure3_model_validation.pdf"), width = 6.7, height = 5.6)
par(mfrow = c(2, 2), mar = c(4.2, 4.4, 2.8, 1.2), mgp = c(2.6, 0.7, 0))
hist(d$best_TSS, breaks = 40, col = PAL_TSS, border = "white",
     main = "", xlab = "TSS", ylab = "Number of taxa", cex.lab = 1.05, cex.axis = 0.95)
panel_label("a"); abline(v = 0.8, col = "red3", lty = 2, lwd = 1.2)
legend("topleft", legend = "threshold 0.8", lty = 2, col = "red3", bty = "n", cex = 0.75)
hist(d$best_ROC, breaks = 40, col = PAL_ROC, border = "white",
     main = "", xlab = "ROC (AUC)", ylab = "Number of taxa", cex.lab = 1.05, cex.axis = 0.95)
panel_label("b"); abline(v = 0.8, col = "red3", lty = 2, lwd = 1.2)
plot(d$best_TSS, d$best_ROC, pch = 16, cex = 0.45, col = adjustcolor(PAL_SCAT, 0.45),
     main = "", xlab = "TSS", ylab = "ROC (AUC)", cex.lab = 1.05, cex.axis = 0.95,
     xlim = c(0.45, 1), ylim = c(0.75, 1))
panel_label("c"); abline(h = 0.8, v = 0.8, col = "red3", lty = 2, lwd = 1.2)
tiers <- split(d$best_TSS, factor(d$selection_tier, levels = c("PRIMARY", "FALLBACK", "BELOW_THRESHOLD")))
boxplot(tiers, col = c(PAL_PRIM, PAL_FB, PAL_BT), main = "",
        xlab = "Selection tier", ylab = "TSS", cex.lab = 1.05, cex.axis = 0.95,
        ylim = c(0.4, 1.05), names = c("PRIMARY\n(n=663)", "FALLBACK\n(n=114)", "BELOW\n(n=36)"))
panel_label("d"); abline(h = 0.8, col = "red3", lty = 2, lwd = 1.2)
dev.off()

# ================= FIGURE 4 — aggregate map (double column) =================
agg <- rast("<WORK_ROOT>/results/aggregation_publication_813_clean_v1_20260813/hotspot_mean_suitability_normalized.tif")
moll4 <- project(agg, "ESRI:54009", method = "bilinear")
v <- values(moll4); v[v < 0] <- 0; v <- ifelse(v > 0.6, 0.6, v); values(moll4) <- v

pdf(file.path(FIG, "figure4_aggregate_map.pdf"), width = 7.09, height = 3.9)
par(mar = c(0.4, 0.4, 2.6, 0.4))
plot(moll4, col = MAP_PAL, axes = FALSE, box = FALSE, legend = TRUE, main = "",
     legend.shrink = 0.75, plg = list(title = "Mean normalized\nsuitability", cex = 0.75))
mtext("Mean normalized suitability across 813 invasive plant taxa", side = 3, line = 0.4, cex = 1.0, font = 2)
mtext("colour scale capped at 0.6 for display (data max = 0.7650)", side = 1, line = -0.8, cex = 0.65, col = "grey30")
dev.off()

cat("ALL FIGURES RELAID OUT\n")
