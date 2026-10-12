# Original source: 19_publication_figures_v4v5.R
# Original source location: ${NON_NATIVE_ROOT}/02_derived_non_native_potential/07_scripts/19_publication_figures_v4v5.R
# Original source SHA-256: 7cb5290b3b8453b99a6d5c11a80c4025ea900a78e5783a6003260315cc130426
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_NON_NATIVE)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# 19_publication_figures_v4v5.R — final Figure 1 (workflow incl. non-native stages)
# and Figure 4 (global vs non-native aggregate, user palette, capped at 0.6).
# No cairo on this R: base pdf() then pdftoppm PNG.
source("<NON_NATIVE_ROOT>/02_derived_non_native_potential/07_scripts/00_config.R")
suppressMessages(library(terra))

FIGDIR <- "<NON_NATIVE_ROOT>/publication_figures"
pal <- c("#FFFFFF", "#4CAF50", "#FFD700", "#FF8C00", "#B22222")

# ---------------- Figure 1: workflow v4 ----------------
pdf(file.path(FIGDIR, "figure1_workflow_v4.pdf"), width = 10, height = 7)
par(mar = c(0.5, 0.5, 0.5, 0.5))
plot.new(); plot.window(xlim = c(0, 100), ylim = c(0, 70))

boxes <- rbind(
  c("1. Occurrence data",                 "GBIF 2024; precision/uncertainty QC", "blue"),
  c("2. Taxonomic harmonisation",         "GBIF backbone + WCVP",                 "blue"),
  c("3. Environmental predictors",        "35 layers (climate/soil/topo/anthro)", "blue"),
  c("4. Species distribution modelling",  "GLM + RF + MAXNET (biomod2)",          "steelblue"),
  c("5. Model validation and selection",  "TSS / ROC; 4-fold CV",                 "steelblue"),
  c("6. Global suitability projection",   "813 rasters; 2.5 arc-min; EPSG:4326", "steelblue"),
  c("7. Native-range source and harmonisation", "GloNAF TDWG L3; geoBoundaries CGAZ", "orange"),
  c("8. Mapped native geographic domains -> masking", "810 masked; 3 approved global-equal", "orange"),
  c("9. Non-native suitability products", "813 rasters (Level 2)",                "darkgreen"),
  c("Multi-species non-native invasion potential", "aggregate over 813 (Level 3)", "darkgreen"))
n <- nrow(boxes)
cols_per_row <- 3
grid <- data.frame(
  x = rep(c(5, 38, 71), length.out = n),
  y = rep(c(56, 38, 20), each = cols_per_row, length.out = n))
w <- 28; h <- 13
colmap <- c(blue = "#DCE9F7", steelblue = "#BDD7EE", orange = "#FBE2D5", darkgreen = "#D9EAD3")
for (i in seq_len(n)) {
  rect(grid$x[i], grid$y[i], grid$x[i] + w, grid$y[i] + h, col = colmap[boxes[i, 3]], border = "grey30", lwd = 1.2)
  text(grid$x[i] + w/2, grid$y[i] + h - 4, boxes[i, 1], cex = 0.95, font = 2)
  text(grid$x[i] + w/2, grid$y[i] + 3.2, boxes[i, 2], cex = 0.72, col = "grey20")
}
arrows_snake <- function(rows = 3, cols = 3) {
  for (r in seq_len(rows)) {
    y <- grid$y[1 + (r - 1) * cols]
    if (r %% 2 == 1) {
      for (c in 1:(cols - 1)) {
        x <- grid$x[c + (r - 1) * cols] + w
        arrows(x, y + h/2, x + 3, y + h/2, length = 0.12, lwd = 1.5)
      }
    } else {
      for (c in 1:(cols - 1)) {
        x <- grid$x[c + (r - 1) * cols]
        arrows(x, y + h/2, x - 3, y + h/2, length = 0.12, lwd = 1.5)
      }
    }
    if (r < rows) {
      x <- if (r %% 2 == 1) grid$x[cols + (r - 1) * cols] + w else grid$x[1 + (r - 1) * cols]
      arrows(x, y + h/2, x, y + h/2 - 8, length = 0.12, lwd = 1.5)
    }
  }
}
# rows 1-3 for boxes 1-9
for (r in 1:3) {
  idx <- ((r - 1) * 3 + 1):(r * 3)
  g <- grid[idx, ]
  y <- g$y[1]
  if (r %% 2 == 1) {
    for (c in 1:2) { x <- g$x[c] + w; arrows(x, y + h/2, x + 3, y + h/2, length = 0.12, lwd = 1.5) }
  } else {
    for (c in 1:2) { x <- g$x[c]; arrows(x, y + h/2, x - 3, y + h/2, length = 0.12, lwd = 1.5) }
  }
  if (r < 3) {
    x <- if (r %% 2 == 1) g$x[3] + w else g$x[1]
    arrows(x, y + h/2, x, y + h/2 - 8, length = 0.12, lwd = 1.5)
  }
}
# arrow from box 9 to final product (box 10 below row 3, centre)
x9 <- grid$x[9] + w
arrows(x9, grid$y[9] + h/2, x9, grid$y[10] + h, length = 0.12, lwd = 1.8)
rect(grid$x[10], grid$y[10], grid$x[10] + w, grid$y[10] + h, col = "#C6E0B4", border = "grey30", lwd = 1.6)
text(grid$x[10] + w/2, grid$y[10] + h - 4, boxes[10, 1], cex = 1.0, font = 2)
text(grid$x[10] + w/2, grid$y[10] + 3.2, boxes[10, 2], cex = 0.72, col = "grey20")
dev.off()
cat("Figure 1 workflow v4 written\n")

# ---------------- Figure 4: global vs non-native (v5) ----------------
g <- rast(FROZEN_AGG_RASTER)
n <- rast(AGG_OUT_RASTER)
brk <- seq(0, 0.6, length.out = 100)
pdf(file.path(FIGDIR, "figure4_global_vs_non_native_v5.pdf"), width = 12, height = 5.6)
par(mfrow = c(1, 2), mar = c(1, 1, 1.6, 1))
plot(g, col = pal, breaks = brk, main = "a) Global mean normalised suitability (813 taxa)", axes = FALSE, box = FALSE, legend = FALSE)
plot(n, col = pal, breaks = brk, main = "b) Multi-species non-native invasion potential (813 taxa)", axes = FALSE, box = FALSE, legend = FALSE)
par(mfrow = c(1, 1), mar = c(0.5, 1, 0.5, 1), new = FALSE, xpd = NA)
dev.off()
cat("Figure 4 v5 written\n")

system2("pdftoppm", c("-png", "-r", "300", file.path(FIGDIR, "figure1_workflow_v4.pdf"), file.path(FIGDIR, "figure1_workflow_v4")))
system2("pdftoppm", c("-png", "-r", "300", file.path(FIGDIR, "figure4_global_vs_non_native_v5.pdf"), file.path(FIGDIR, "figure4_global_vs_non_native_v5")))
cat("DONE\n")
