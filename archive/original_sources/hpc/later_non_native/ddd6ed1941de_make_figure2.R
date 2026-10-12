# Original source: make_figure2.R
# Original source location: ${NON_NATIVE_ROOT}/temp/make_figure2.R
# Original source SHA-256: ddd6ed1941de71379a84ba9d79984d9e16357529b44637ed231761db5474940d
# Recovery: live read-only SSH, 2026-10-12
# Classification: SUPPORTING (HISTORICAL_SUPPORTING_AUDIT_LINEAGE_HELPER)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
.libPaths(c("<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu", .libPaths()))
suppressPackageStartupMessages(library(terra))
FIG <- "<NON_NATIVE_ROOT>/publication_figures"

ps <- read.csv(file.path(FIG, "figure2_records_per_species_813.csv"), stringsAsFactors = FALSE)
rich <- rast(file.path(FIG, "figure2_species_richness_813.tif"))
moll <- project(rich, "ESRI:54009", method = "near")

pdf(file.path(FIG, "figure2_occurrence_coverage.pdf"), width = 10.5, height = 4.6)
layout(matrix(c(1, 1, 2), nrow = 1), widths = c(1, 1.45))
par(mar = c(4.2, 4.2, 2.4, 0.8))

# (a) histogram log10 records per species
hist(log10(ps$N), breaks = 40, col = "#55A868", border = "white",
     main = sprintf("(a) Presence records per taxon (n = %d)", nrow(ps)),
     xlab = expression(log[10]~records), ylab = "Number of taxa")
mtext(sprintf("median = %s records", format(median(ps$N), big.mark = ",")), side = 1, line = 3, cex = 0.72)

# (b) species richness map
par(mar = c(0.5, 0.5, 2.4, 0.5))
pal <- colorRampPalette(c("#F7FBFF", "#C6DBEF", "#6BAED6", "#2171B5", "#08306B"))(256)
plot(moll, col = pal, axes = FALSE, box = FALSE, legend = TRUE,
     main = "(b) Occurrence richness (taxa per cell, 2.5 arc-min)")
dev.off()
cat("figure2 written.\n")
