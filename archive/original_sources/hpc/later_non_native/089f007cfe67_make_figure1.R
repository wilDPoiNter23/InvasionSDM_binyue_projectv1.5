# Original source: make_figure1.R
# Original source location: ${NON_NATIVE_ROOT}/temp/make_figure1.R
# Original source SHA-256: 089f007cfe67f699f4ab3c2eac77f266eb409486f95d293ed477ef3f5f963184
# Recovery: live read-only SSH, 2026-10-12
# Classification: SUPPORTING (HISTORICAL_SUPPORTING_AUDIT_LINEAGE_HELPER)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
.libPaths(c("<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu", .libPaths()))
FIG <- "<NON_NATIVE_ROOT>/publication_figures"

pdf(file.path(FIG, "figure1_workflow.pdf"), width = 10, height = 5.6)
par(mar = c(1, 1, 2.4, 1))

plot(NA, xlim = c(0, 120), ylim = c(0, 60), axes = FALSE, xlab = "", ylab = "")
title("Workflow for the global invasion-pressure dataset (813 taxa)", cex.main = 1.35)

boxw <- 17; boxh <- 12; y <- 30
xs <- seq(2, 100, length.out = 6)

drawbox <- function(x, title, lines, col = "#DEEBF7") {
  rect(x - boxw / 2, y - boxh / 2, x + boxw / 2, y + boxh / 2, col = col, border = "#4C72B0", lwd = 1.6)
  text(x, y + boxh / 2 - 2.2, title, font = 2, cex = 0.92)
  for (i in seq_along(lines)) text(x, y + boxh / 2 - 4.5 - (i - 1) * 2.6, lines[i], cex = 0.68)
}

drawbox(xs[1], "1. Occurrence data",
        c("GBIF + regional sources", "813 accepted taxa", "coordinate cleaning"))
drawbox(xs[2], "2. Environmental",
        c("predictors", "35 layers, 2.5 arc-min", "climate + soil + human"))
drawbox(xs[3], "3. SDM fitting",
        c("GLM + RF + MAXNET", "4-fold CV, 4 PA sets", "TSS/ROC selection"))
drawbox(xs[4], "4. Evaluation",
        c("PRIMARY: TSS>0.8, ROC>0.8", "FALLBACK: TSS>0.7, ROC>0.7", "813 taxa pass QC"))
drawbox(xs[5], "5. Projection",
        c("global 2.5 arc-min", "ensemble + single-model", "0-1000 BIOMOD scale"))
drawbox(xs[6], "6. Aggregation",
        c("mean suitability / taxon", "normalized [0,1]", "37,324,800 cells"))

arrows(xs[-6] + boxw / 2, y, xs[-1] - boxw / 2, y, length = 0.12, lwd = 2, col = "#4C72B0")
text(60, y - 14, "35 scientific predictors shared across all taxa; final product = mean normalized suitability per terrestrial cell", cex = 0.78, col = "grey30")
dev.off()
cat("figure1 written.\n")
