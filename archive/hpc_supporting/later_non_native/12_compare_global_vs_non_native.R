# SUPPORTING LATER PRODUCT: not established as submitted 2025 workflow.
# Original SHA256: 2cc80d70c9fa100696e860b14f5e872dbac7455c994bd5edead356eaa037357f
# 12_compare_global_vs_non_native.R — PHASE 21: descriptive comparison of aggregates
ca <- commandArgs(trailingOnly = FALSE)
source(file.path(dirname(sub("^--file=", "", ca[grep("^--file=", ca)])), "00_config.R"))

suppressMessages(library(terra))

g <- rast(FROZEN_AGG_RASTER)
n <- rast(AGG_OUT_RASTER)

geom_match <- isTRUE(ext(g) == ext(n)) &&
              isTRUE(all(res(g) == res(n))) &&
              isTRUE(all(dim(g) == dim(n))) &&
              isTRUE(crs(g, describe = TRUE)$code == crs(n, describe = TRUE)$code)
cat("geometry match:", geom_match, "\n")

# difference raster (descriptive): global - non-native
d <- g - n
diff_path <- file.path(DIR_AGG_RASTERS, "global_minus_non_native_aggregate_difference.tif")
writeRaster(d, diff_path, overwrite = TRUE, datatype = "FLT4S", NAflag = -3.4e+38,
            gdal = c("COMPRESS=LZW", "PREDICTOR=2", "TILED=YES", "BIGTIFF=YES"))

# matched-cell sample (cell-index sampling; spatSample can hang on these rasters)
ncell <- ncell(g)
set.seed(42)
idx <- sample(seq_len(ncell), 2e5)
vg <- values(g, mat = FALSE)[idx]
vn <- values(n, mat = FALSE)[idx]
keep <- is.finite(vg) & is.finite(vn)
vg <- vg[keep]; vn <- vn[keep]
cat("sampled matched finite cells:", length(vg), "\n")

dd <- vg - vn
cmp <- data.frame(
  geometry_match = geom_match,
  sampled_cells = length(vg),
  Pearson_r = cor(vg, vn),
  mean_global = mean(vg), mean_non_native = mean(vn),
  mean_difference = mean(dd),
  median_difference = median(dd),
  MAE = mean(abs(dd)),
  RMSE = sqrt(mean(dd^2)),
  Q05_difference = quantile(dd, 0.05),
  Q95_difference = quantile(dd, 0.95),
  pct_cells_equal = mean(abs(dd) < 1e-9),
  pct_cells_lower_non_native = mean(dd > 0),
  stringsAsFactors = FALSE)
write.csv(cmp, file.path(DIR_TABLES, "global_vs_non_native_aggregate_comparison.csv"),
          row.names = FALSE)
print(cmp)
cat("Diff raster:", diff_path, "\n")
