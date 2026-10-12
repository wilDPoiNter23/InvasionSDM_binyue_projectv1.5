# Original source: 13_figures.R
# Original source location: ${NON_NATIVE_ROOT}/02_derived_non_native_potential/07_scripts/13_figures.R
# Original source SHA-256: 0caf342b097be8a08c0ce73c45f82a8bbdec743e15fdfc99f6f952f0b65bc108
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_NON_NATIVE)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# 13_figures.R — PHASE 22: publication figures
# No PNG/cairo device on this R -> PDF first, then pdftoppm to PNG.
ca <- commandArgs(trailingOnly = FALSE)
source(file.path(dirname(sub("^--file=", "", ca[grep("^--file=", ca)])), "00_config.R"))

suppressMessages({ library(terra); library(sf) })
sf_use_s2(FALSE)

mani <- read.csv(FROZEN_MANIFEST_V3, stringsAsFactors = FALSE)
native <- vect(NATIVE_GPKG)

colmap <- rev(terrain.colors(64))

# ---------- Figure A: masking concept (Spartina alterniflora) ----------
tx <- "Spartina alterniflora"
row <- mani[mani$accepted_taxon == tx, ]
r <- rast(row$final_projection_path)
poly <- native[native$accepted_taxon == tx, ]
poly_sf <- st_wrap_dateline(st_as_sf(poly), options = c("WRAPDATELINE=YES"))
poly_v <- project(vect(poly_sf), "EPSG:4326")
out <- mask(r, poly_v, inverse = TRUE)

pdf(file.path(DIR_FIGURES, "Figure_A_native_range_masking_concept.pdf"),
    width = 15, height = 5.5)
par(mfrow = c(1, 3), mar = c(2, 2, 3, 2))
plot(r, main = sprintf("a) %s — frozen global suitability", tx),
     maxcell = 6e5, col = colmap, legend = FALSE)
plot(poly_v, add = TRUE, border = "black", lwd = 0.6)
plot(poly_v, main = "b) mapped native geographic domain",
     col = "darkred", border = "darkred", background = "grey92")
plot(out, main = "c) non-native suitability (native cells -> NA)",
     maxcell = 6e5, col = colmap, legend = FALSE)
dev.off()

# ---------- Figure B: global non-native invasion/introduction potential ----------
agg <- rast(AGG_OUT_RASTER)
pdf(file.path(DIR_FIGURES, "Figure_B_non_native_invasion_potential_global.pdf"),
    width = 12, height = 6)
par(mar = c(2, 2, 3, 2))
plot(agg, main = "Multi-species invasion / introduction potential outside mapped native domains",
     maxcell = 1e6, col = colmap)
dev.off()

# ---------- Figure C: global vs non-native difference (descriptive) ----------
diff_path <- file.path(DIR_AGG_RASTERS, "global_minus_non_native_aggregate_difference.tif")
if (file.exists(diff_path)) {
  d <- rast(diff_path)
  pdf(file.path(DIR_FIGURES, "Figure_C_global_minus_non_native_difference.pdf"),
      width = 12, height = 6)
  par(mar = c(2, 2, 3, 2))
  plot(d, main = "Contribution removed by native-range masking (global - non-native)",
       maxcell = 1e6, col = rev(heat.colors(64)))
  dev.off()
}

# PNG renders
for (f in c("Figure_A_native_range_masking_concept",
            "Figure_B_non_native_invasion_potential_global",
            "Figure_C_global_minus_non_native_difference")) {
  pdf_file <- file.path(DIR_FIGURES, paste0(f, ".pdf"))
  if (file.exists(pdf_file)) {
    system2("pdftoppm", c("-png", "-r", "150", pdf_file,
                          file.path(DIR_FIGURES, paste0(f, "__page"))))
  }
}
cat("Figures written.\n")
