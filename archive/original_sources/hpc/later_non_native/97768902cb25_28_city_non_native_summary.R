# Original source: 28_city_non_native_summary.R
# Original source location: ${NON_NATIVE_ROOT}/02_derived_non_native_potential/07_scripts/28_city_non_native_summary.R
# Original source SHA-256: 97768902cb25ced38f34ab1c054e555a4bfef2ebecd6a4473c9a3ee209654d2c
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_NON_NATIVE)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# 28_city_non_native_summary.R — recompute the 108-city summary table from the
# non-native aggregates (813 and 814) with reclamation split, mirroring the
# schema of 01_data_package/city_risk_summary_fromGIS.csv:
# city,total_pixels,mean_risk,median_risk,non_reclamation_pixels,reclamation_pixels,
# mean_risk_non_reclamation,mean_risk_reclamation,reclamation_percentage,risk_difference
ca <- commandArgs(trailingOnly = FALSE)
source(file.path(dirname(sub("^--file=", "", ca[grep("^--file=", ca)])), "00_config.R"))
suppressMessages(library(terra))

OUTDIR <- file.path(DIR_TABLES, "city_analysis_108")
dir.create(OUTDIR, recursive = TRUE, showWarnings = FALSE)

v <- vect("<HPC_INPUT_ROOT>/climate/cities/108finalCities_recheck.shp")
nn813 <- rast(AGG_OUT_RASTER)
nn814 <- rast(file.path(DIR_AGG_RASTERS, "supplementary_814", "multispecies_non_native_invasion_potential_814.tif"))
stack <- rast("<WORK_ROOT>/data/predictors/full_stack_terrestrial_plus_VIIRS_v1.tif")
recl <- stack[["reclamation_raster"]]

# reclamation layer coding check
rvals <- unique(values(recl, mat = FALSE, na.rm = TRUE))[1:6]
cat("reclamation raster unique values:", paste(rvals, collapse = ","), "\n")
recl_bin <- recl > 0

# total pixels via rasterize (cell centres inside polygon)
tot <- rasterize(v, recl_bin, fun = "count")  # returns count per polygon? use field
tot2 <- rasterize(v, recl_bin, field = 1, touches = FALSE)
tot_cnt <- extract(tot2, v, fun = sum, na.rm = TRUE)
names(tot_cnt)[2] <- "total_pixels"

compute_city <- function(r, recl_layer, out_name) {
  e <- extract(c(r, recl_layer), v, ID = TRUE, na.rm = TRUE)
  names(e)[2:3] <- c("risk", "recl")
  e$recl <- as.numeric(e$recl > 0)
  out <- do.call(rbind, lapply(sort(unique(e$ID)), function(id) {
    s <- e[e$ID == id, ]
    s <- s[!is.na(s$risk) & !is.na(s$recl), ]
    nr <- sum(s$recl == 0, na.rm = TRUE); nrc <- sum(s$recl == 1, na.rm = TRUE)
    data.frame(idx = id, total_pixels = nrow(s),
      mean_risk = mean(s$risk, na.rm = TRUE), median_risk = median(s$risk, na.rm = TRUE),
      non_reclamation_pixels = nr, reclamation_pixels = nrc,
      mean_risk_non_reclamation = if (nr > 0) mean(s$risk[s$recl == 0], na.rm = TRUE) else NA_real_,
      mean_risk_reclamation = if (nrc > 0) mean(s$risk[s$recl == 1], na.rm = TRUE) else NA_real_,
      reclamation_percentage = if (nrow(s) > 0) round(100 * nrc / nrow(s), 2) else NA_real_,
      risk_difference = if (nr > 0 && nrc > 0) mean(s$risk[s$recl == 1], na.rm = TRUE) - mean(s$risk[s$recl == 0], na.rm = TRUE) else NA_real_)
  }))
  out$city <- v$shapeName[out$idx]
  out <- out[, c("city", "total_pixels", "mean_risk", "median_risk",
                 "non_reclamation_pixels", "reclamation_pixels",
                 "mean_risk_non_reclamation", "mean_risk_reclamation",
                 "reclamation_percentage", "risk_difference")]
  out$mean_risk <- round(out$mean_risk, 4); out$median_risk <- round(out$median_risk, 4)
  out$mean_risk_non_reclamation <- round(out$mean_risk_non_reclamation, 4)
  out$mean_risk_reclamation <- round(out$mean_risk_reclamation, 4)
  out$risk_difference <- round(out$risk_difference, 4)
  write.csv(out, file.path(OUTDIR, out_name), row.names = FALSE)
  cat("wrote", out_name, "rows:", nrow(out), "\n")
  out
}

c813 <- compute_city(nn813, recl, "city_risk_summary_non_native_813.csv")
c814 <- compute_city(nn814, recl, "city_risk_summary_non_native_814.csv")

# comparison vs GIS table
gis <- read.csv("<NON_NATIVE_ROOT>/01_data_package/city_risk_summary_fromGIS.csv",
                stringsAsFactors = FALSE)
gis$city_l <- tolower(trimws(gis$city))
c813$city_l <- tolower(trimws(c813$city)); c814$city_l <- tolower(trimws(c814$city))
cmp <- data.frame(city = gis$city,
  gis_mean = as.numeric(gis$mean_risk), gis_median = as.numeric(gis$median_risk),
  nn813_mean = c813$mean_risk[match(gis$city_l, c813$city_l)],
  nn813_median = c813$median_risk[match(gis$city_l, c813$city_l)],
  nn814_mean = c814$mean_risk[match(gis$city_l, c814$city_l)],
  nn814_median = c814$median_risk[match(gis$city_l, c814$city_l)])
cmp$diff_gis_vs_nn813 <- cmp$gis_mean - cmp$nn813_mean
cmp$diff_gis_vs_nn814 <- cmp$gis_mean - cmp$nn814_mean
write.csv(cmp, file.path(OUTDIR, "city_risk_gis_vs_non_native_813_814.csv"), row.names = FALSE)

ok <- cmp[!is.na(cmp$gis_mean) & !is.na(cmp$nn813_mean), ]
cat("GIS vs NN813: r =", cor(ok$gis_mean, ok$nn813_mean),
    " MAE =", mean(abs(ok$diff_gis_vs_nn813)),
    " |diff|>0.05:", sum(abs(ok$diff_gis_vs_nn813) > 0.05), "\n")
cat("GIS vs NN814: r =", cor(ok$gis_mean, ok$nn814_mean),
    " MAE =", mean(abs(ok$diff_gis_vs_nn814)),
    " |diff|>0.05:", sum(abs(ok$diff_gis_vs_nn814) > 0.05), "\n")
cat("NN813 vs NN814: r =", cor(ok$nn813_mean, ok$nn814_mean), "\n")
cat("mean NN813:", mean(ok$nn813_mean), " mean NN814:", mean(ok$nn814_mean),
    " mean GIS:", mean(ok$gis_mean), "\n")
cat("DONE\n")
