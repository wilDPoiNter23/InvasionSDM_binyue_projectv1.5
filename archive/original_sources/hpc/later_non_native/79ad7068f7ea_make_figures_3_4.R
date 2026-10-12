# Original source: make_figures_3_4.R
# Original source location: ${NON_NATIVE_ROOT}/temp/make_figures_3_4.R
# Original source SHA-256: 79ad7068f7ead989a60bc0c122b9bb0b379f62abe6ad4e0c4301e2707fc8835d
# Recovery: live read-only SSH, 2026-10-12
# Classification: SUPPORTING (HISTORICAL_SUPPORTING_AUDIT_LINEAGE_HELPER)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
.libPaths(c("<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu", .libPaths()))
suppressPackageStartupMessages(library(terra))
SD <- "<NON_NATIVE_ROOT>"
FIG <- file.path(SD, "publication_figures")

# ================= FIGURE 3 =================
d <- read.csv(file.path(FIG, "figure3_model_validation_data_v2.csv"), stringsAsFactors = FALSE)
cat("rows:", nrow(d), " TSS complete:", sum(is.finite(d$best_TSS)),
    " ROC complete:", sum(is.finite(d$best_ROC)), "\n")

pdf(file.path(FIG, "figure3_model_validation.pdf"), width = 8.67, height = 6.67)
par(mfrow = c(2, 2), mar = c(4.2, 4.2, 2.6, 1.5), mgp = c(2.6, 0.8, 0))
# (a) TSS
hist(d$best_TSS, breaks = 40, col = "#4C72B0", border = "white",
     main = sprintf("(a) Validation TSS (%d taxa)", nrow(d)),
     xlab = "TSS", xlim = c(0, 1))
abline(v = 0.8, col = "red", lty = 2)
legend("topleft", legend = "PRIMARY threshold 0.8", lty = 2, col = "red", bty = "n", cex = 0.8)
# (b) ROC
hist(d$best_ROC, breaks = 40, col = "#55A868", border = "white",
     main = sprintf("(b) Validation ROC (%d taxa)", nrow(d)),
     xlab = "ROC (AUC)", xlim = c(0, 1))
abline(v = 0.8, col = "red", lty = 2)
# (c) scatter
plot(d$best_TSS, d$best_ROC, pch = 16, cex = 0.5, col = rgb(0.2, 0.2, 0.5, 0.45),
     main = "(c) TSS vs ROC", xlab = "TSS", ylab = "ROC (AUC)",
     xlim = c(0, 1), ylim = c(0, 1))
abline(h = 0.8, v = 0.8, col = "red", lty = 2)
# (d) by tier
tiers <- split(d$best_TSS, d$selection_tier)
tiers <- tiers[order(names(tiers))]
boxplot(tiers, col = c("#C44E52", "#4C72B0"), main = "(d) TSS by selection tier",
        xlab = "Selection tier", ylab = "TSS", ylim = c(0, 1))
abline(h = 0.8, col = "red", lty = 2)
dev.off()
cat("figure3 written.\n")

# ================= FIGURE 4 =================
agg <- rast("<WORK_ROOT>/results/aggregation_publication_813_clean_v1_20260813/hotspot_mean_suitability_normalized.tif")
moll <- project(agg, "ESRI:54009", method = "bilinear")
# coastal outline from raster extent polygons is complex; use landmask from a coarse raster
land <- !is.na(moll)
v <- values(moll)
v[v < 0] <- 0
v <- ifelse(v > 0.6, 0.6, v)  # cap for colour scale
values(moll) <- v

pdf(file.path(FIG, "figure4_aggregate_map.pdf"), width = 10, height = 5.5)
par(mar = c(0.5, 0.5, 1.8, 0.5))
pal <- colorRampPalette(c("#F7FBFF", "#DEEBF7", "#9ECAE1", "#4292C6", "#2171B5", "#08519C"))(256)
plot(moll, col = pal, axes = FALSE, box = FALSE, legend = TRUE,
     main = "Mean normalized suitability across 813 invasive plant taxa")
dev.off()
cat("figure4 written.\n")
cat("DONE\n")
