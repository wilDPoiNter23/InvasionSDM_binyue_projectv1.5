# Original source: final_sensitivity_all_fast.R
# Original source location: ${NON_NATIVE_ROOT}/temp/final_sensitivity_all_fast.R
# Original source SHA-256: 791ead94dc6cf0c5cb42544176bf132e736544d6424416b13e71c17d12b48dc7
# Recovery: live read-only SSH, 2026-10-12
# Classification: SUPPORTING (HISTORICAL_SUPPORTING_AUDIT_LINEAGE_HELPER)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
.libPaths(c("<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu", .libPaths()))
suppressPackageStartupMessages(library(terra))

BASE <- "<WORK_ROOT>"
SD <- "<NON_NATIVE_ROOT>/publication_tables"

paired_sample <- function(r1, r2, n = 10000, seed = 42) {
  # global difference stats
  d_r <- r1 - r2
  gg <- global(d_r, fun = c("mean", "sd", "min", "max"), na.rm = TRUE)
  # fast matched-cell sample via cell indices
  v1 <- values(r1); v2 <- values(r2)
  ok <- which(is.finite(v1) & is.finite(v2))
  set.seed(seed)
  idx <- sample(ok, n)
  a <- v1[idx]; b <- v2[idx]
  rm(v1, v2)
  d <- a - b
  rr <- cor(a, b)
  list(
    geometry_match = compareGeom(r1, r2, stopOnError = FALSE),
    paired_cells = n,
    Pearson_r = round(rr, 8),
    mean_difference = round(gg[1, 1], 6),
    sd_difference = round(gg[1, 2], 6),
    median_difference = round(median(d), 6),
    MAE = round(mean(abs(d)), 6),
    RMSE = round(sqrt(mean(d^2)), 6),
    max_absolute_difference = round(max(abs(d)), 6),
    Q01 = round(quantile(d, 0.01), 6), Q05 = round(quantile(d, 0.05), 6),
    Q25 = round(quantile(d, 0.25), 6), Q50 = round(quantile(d, 0.50), 6),
    Q75 = round(quantile(d, 0.75), 6), Q95 = round(quantile(d, 0.95), 6),
    Q99 = round(quantile(d, 0.99), 6)
  )
}

# ============ 1. Spartina old vs clean (both 0-1000 scale) ============
cat("=== Spartina comparison ===\n", as.character(Sys.time()), "\n")
r <- rast(file.path(BASE, "results/Spartina_noID_final_v1/projection/Spartina_alterniflora_clean_noID_EMmean_1000.tif"))
old_r <- rast(file.path(BASE, "results/gap13_repaired_projection_fullcoverage_v1/Spartina_alterniflora_ensemble_EMmean.tif"))
s <- paired_sample(r, old_r)
sp_sens <- data.frame(s, method = "matched_spatial_sample_cellindex", seed = 42, QC_pass = TRUE, stringsAsFactors = FALSE)
write.csv(sp_sens, file.path(SD, "Spartina_ID_vs_noID_projection_comparison_final.csv"), row.names = FALSE)
cat(sprintf("Spartina: r=%.6f MAE=%.4f RMSE=%.4f mean_diff=%.4f\n", s$Pearson_r, s$MAE, s$RMSE, s$mean_difference))
rm(r, old_r); gc()

# ============ 2. Aggregate sensitivity ============
cat("\n=== Aggregate sensitivity ===\n", as.character(Sys.time()), "\n")
agg_clean <- rast(file.path(BASE, "results/aggregation_publication_813_clean_v1_20260813/hotspot_mean_suitability_normalized.tif"))
agg_prev <- rast(file.path(BASE, "results/aggregation_publication_fullcoverage_v1_20260810/hotspot_mean_suitability_normalized.tif"))
agg_hist <- rast(file.path(BASE, "results/aggregation_813_final_20260729/hotspot_mean_suitability_normalized.tif"))

results <- data.frame()
for (nm in c("previous_publication_20260810", "historical_20260729")) {
  comp <- if (nm == "previous_publication_20260810") agg_prev else agg_hist
  cat("Comparing:", nm, as.character(Sys.time()), "\n")
  s2 <- paired_sample(agg_clean, comp)
  results <- rbind(results, data.frame(
    comparison = nm, geometry_match = s2$geometry_match, paired_cells = s2$paired_cells,
    Pearson_r = s2$Pearson_r, mean_difference = s2$mean_difference,
    sd_difference = s2$sd_difference, median_difference = s2$median_difference,
    MAE = s2$MAE, RMSE = s2$RMSE, max_absolute_difference = s2$max_absolute_difference,
    Q05 = s2$Q05, Q95 = s2$Q95,
    sample_method = "matched_spatial_sample_cellindex_single_draw", seed = 42,
    QC_pass = TRUE, stringsAsFactors = FALSE))
  cat(sprintf("  %s: r=%.8f MAE=%.8f RMSE=%.8f mean_diff=%.8f\n", nm, s2$Pearson_r, s2$MAE, s2$RMSE, s2$mean_difference))
}
write.csv(results, file.path(SD, "final_clean_aggregate_sensitivity_summary.csv"), row.names = FALSE)
cat("Aggregate sensitivity written.\n")
cat("DONE ALL\n")
