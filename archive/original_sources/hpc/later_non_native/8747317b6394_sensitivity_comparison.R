# Original source: sensitivity_comparison.R
# Original source location: ${NON_NATIVE_ROOT}/temp/sensitivity_comparison.R
# Original source SHA-256: 8747317b639418f9eecc86a09a707ec7ba4a7b6200c3c9f7bfdad096d42a5774
# Recovery: live read-only SSH, 2026-10-12
# Classification: SUPPORTING (HISTORICAL_SUPPORTING_AUDIT_LINEAGE_HELPER)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
.libPaths(c(.libPaths(), "<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu"))
suppressPackageStartupMessages(library(terra))

# ============================================================
# Historical Sensitivity Comparison
# Compare final v5 aggregate vs historical baselines
# ============================================================

final_path <- "<WORK_ROOT>/results/aggregation_publication_fullcoverage_v1_20260810/hotspot_mean_suitability_normalized.tif"
historical_path <- "<WORK_ROOT>/results/aggregation_813_final_20260729/hotspot_mean_suitability_normalized.tif"
diagnostic_path <- "<WORK_ROOT>/results/aggregation_publication_v1_20260807/hotspot_mean_suitability_normalized.tif"

final <- rast(final_path)

compare_rasters <- function(ref, comp, label) {
  cat("\n=== Comparing:", label, "===\n")
  
  # Check if rasters align
  cat("Ref dims:", paste(dim(ref), collapse="x"), "\n")
  cat("Comp dims:", paste(dim(comp), collapse="x"), "\n")
  cat("Ref extent:", paste(ext(ref)[], collapse=" "), "\n")
  cat("Comp extent:", paste(ext(comp)[], collapse=" "), "\n")
  
  if (!all(dim(ref) == dim(comp))) {
    cat("DIMENSION MISMATCH — resampling needed\n")
    comp <- resample(comp, ref, method="bilinear")
  }
  
  # Sample values
  set.seed(123)
  n_sample <- 200000
  v_ref <- spatSample(ref, n_sample, na.rm=TRUE)[,1]
  v_comp <- spatSample(comp, n_sample, na.rm=TRUE)[,1]
  
  # Remove any Inf/NaN
  valid <- is.finite(v_ref) & is.finite(v_comp)
  v_ref <- v_ref[valid]
  v_comp <- v_comp[valid]
  
  cat("Comparable cells:", length(v_ref), "\n")
  
  # Compute statistics
  r <- cor(v_ref, v_comp, method="pearson")
  diff <- v_ref - v_comp
  mae <- mean(abs(diff))
  rmse <- sqrt(mean(diff^2))
  
  cat(sprintf("Pearson r: %.6f\n", r))
  cat(sprintf("Mean difference: %.6f\n", mean(diff)))
  cat(sprintf("Median difference: %.6f\n", median(diff)))
  cat(sprintf("MAE: %.6f\n", mae))
  cat(sprintf("RMSE: %.6f\n", rmse))
  cat(sprintf("Max absolute difference: %.6f\n", max(abs(diff))))
  cat(sprintf("Q05 difference: %.6f\n", quantile(diff, 0.05)))
  cat(sprintf("Q95 difference: %.6f\n", quantile(diff, 0.95)))
  
  return(data.frame(
    comparison_target = label,
    comparable_cells = length(v_ref),
    Pearson_r = r,
    mean_difference = mean(diff),
    median_difference = median(diff),
    MAE = mae,
    RMSE = rmse,
    max_absolute_difference = max(abs(diff)),
    Q05_difference = quantile(diff, 0.05),
    Q95_difference = quantile(diff, 0.95),
    stringsAsFactors = FALSE
  ))
}

results <- data.frame()

# Compare vs historical
if (file.exists(historical_path)) {
  cat("\n========== FINAL v5 vs HISTORICAL v4 ==========\n")
  historical <- rast(historical_path)
  res <- compare_rasters(final, historical, "historical_v4_20260729")
  results <- rbind(results, res)
}

# Compare vs diagnostic
if (file.exists(diagnostic_path)) {
  cat("\n========== FINAL v5 vs DIAGNOSTIC v1 ==========\n")
  diagnostic <- rast(diagnostic_path)
  res <- compare_rasters(final, diagnostic, "diagnostic_v1_20260807")
  results <- rbind(results, res)
}

write.csv(results, "<NON_NATIVE_ROOT>/publication_tables/final_aggregate_sensitivity_summary.csv", row.names = FALSE)
cat("\n=== Results written to publication_tables/final_aggregate_sensitivity_summary.csv ===\n")
print(results)
