# Original source: 27_city_risk_check.R
# Original source location: ${NON_NATIVE_ROOT}/02_derived_non_native_potential/07_scripts/27_city_risk_check.R
# Original source SHA-256: b4f8ec7d6192be570868adab19587606e93fd450d1ed9961d1e32829c898478b
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_NON_NATIVE)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# 27_city_risk_check.R — zonal statistics of the 108 cities on the non-native
# (and global, for diagnosis) aggregates; comparison against
# 01_data_package/city_risk_summary_fromGIS.csv
ca <- commandArgs(trailingOnly = FALSE)
source(file.path(dirname(sub("^--file=", "", ca[grep("^--file=", ca)])), "00_config.R"))
suppressMessages(library(terra))

OUTDIR <- file.path(DIR_TABLES, "city_analysis_108")
dir.create(OUTDIR, recursive = TRUE, showWarnings = FALSE)

v <- vect("<HPC_INPUT_ROOT>/climate/cities/108finalCities_recheck.shp")
csv <- read.csv("<NON_NATIVE_ROOT>/01_data_package/city_risk_summary_fromGIS.csv",
                stringsAsFactors = FALSE)

v$city_l <- tolower(trimws(v$shapeName))
csv$city_l <- tolower(trimws(csv$city))
csv_key <- csv[!is.na(csv$city_l) & nzchar(csv$city_l), ]
cat("csv rows with city name:", nrow(csv_key), " rows without:", sum(is.na(csv$city_l) | !nzchar(csv$city_l)), "\n")

nn <- rast(AGG_OUT_RASTER)
glob <- rast(FROZEN_AGG_RASTER)
stack <- rast("<WORK_ROOT>/data/predictors/full_stack_terrestrial_plus_VIIRS_v1.tif")
recl <- stack[["reclamation_raster"]]

zonal <- function(r) {
  e <- extract(r, v, ID = TRUE, na.rm = TRUE)
  n <- tapply(e[, 2], e$ID, length)
  m <- tapply(e[, 2], e$ID, mean)
  md <- tapply(e[, 2], e$ID, median)
  mx <- tapply(e[, 2], e$ID, max)
  ids <- as.integer(names(n))
  data.frame(idx = ids, n = as.integer(n), mean = as.numeric(m),
             median = as.numeric(md), max = as.numeric(mx))
}
zn <- zonal(nn); zg <- zonal(glob); zr <- zonal(recl)
names(zn) <- c("idx", "cells_2_5min", "nn813_mean", "nn813_median", "nn813_max")
names(zg) <- c("idx", "cells_2_5min_g", "global_mean", "global_median", "global_max")
names(zr) <- c("idx", "cells_recl_", "recl_mean", "recl_median", "recl_max")
zr$reclamation_fraction <- zr$recl_mean

city_of <- data.frame(idx = seq_len(nrow(v)), city = v$shapeName, city_l = v$city_l)
m <- merge(city_of, zn, by = "idx", all.x = TRUE)
m <- merge(m, zg[, c("idx", "global_mean", "global_median")], by = "idx", all.x = TRUE)
m <- merge(m, zr[, c("idx", "reclamation_fraction")], by = "idx", all.x = TRUE)

mm <- match(csv_key$city_l, m$city_l)
unmatched <- csv_key$city[is.na(mm)]
cat("csv rows unmatched to shapefile:", length(unmatched), "\n")
if (length(unmatched)) print(unmatched)
cmp <- cbind(csv_key[, c("city", "total_pixels", "mean_risk", "median_risk", "reclamation_percentage")],
             m[mm, c("cells_2_5min", "nn813_mean", "nn813_median",
                     "global_mean", "global_median", "reclamation_fraction")])
rownames(cmp) <- NULL
cmp$csv_mean_risk <- as.numeric(cmp$mean_risk)
cmp$csv_median_risk <- as.numeric(cmp$median_risk)
cmp$diff_mean_csv_vs_nn813 <- cmp$csv_mean_risk - cmp$nn813_mean
cmp$diff_mean_csv_vs_global <- cmp$csv_mean_risk - cmp$global_mean
cmp$diff_median_csv_vs_nn813 <- cmp$csv_median_risk - cmp$nn813_median

out_cols <- c("city", "total_pixels", "cells_2_5min", "csv_mean_risk",
              "nn813_mean", "global_mean", "csv_median_risk", "nn813_median",
              "global_median", "reclamation_percentage", "reclamation_fraction",
              "diff_mean_csv_vs_nn813", "diff_mean_csv_vs_global", "diff_median_csv_vs_nn813")
write.csv(cmp[, out_cols], file.path(OUTDIR, "city_risk_comparison_108.csv"), row.names = FALSE)

keep <- cmp[!is.na(cmp$csv_mean_risk) & !is.na(cmp$nn813_mean), ]
cat("comparable cities:", nrow(keep), "\n")
cat("cor csv vs nn813 mean:", cor(keep$csv_mean_risk, keep$nn813_mean), "\n")
cat("cor csv vs global mean:", cor(keep$csv_mean_risk, keep$global_mean), "\n")
cat("cor csv median vs nn813 median:", cor(keep$csv_median_risk, keep$nn813_median), "\n")
cat("MAE csv vs nn813 mean:", mean(abs(keep$diff_mean_csv_vs_nn813)), "\n")
cat("MAE csv vs global mean:", mean(abs(keep$diff_mean_csv_vs_global)), "\n")
cat("cities |diff|>0.05 vs nn813:", sum(abs(keep$diff_mean_csv_vs_nn813) > 0.05),
    " vs global:", sum(abs(keep$diff_mean_csv_vs_global) > 0.05), "\n")
print(keep[order(-abs(keep$diff_mean_csv_vs_nn813)), ][1:12,
  c("city", "total_pixels", "cells_2_5min", "csv_mean_risk", "nn813_mean", "global_mean", "diff_mean_csv_vs_nn813")])
cat("rows in csv without city name (reported):\n")
print(csv[is.na(csv$city_l) | !nzchar(csv$city_l), 1:5])
cat("DONE\n")
