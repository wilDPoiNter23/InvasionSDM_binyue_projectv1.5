# Original source: compute_TSS_ROC_from_evals.R
# Original source location: ${NON_NATIVE_ROOT}/temp/compute_TSS_ROC_from_evals.R
# Original source SHA-256: 17390745c8b6d2046832336b6e3b27c17ac348dd55d6404d6edf0deda707868f
# Recovery: live read-only SSH, 2026-10-12
# Classification: SUPPORTING (HISTORICAL_SUPPORTING_AUDIT_LINEAGE_HELPER)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
.libPaths(c(.libPaths(), "<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu"))

# Read all comprehensive evaluation CSV files
eval_dir <- "<WORK_ROOT>/results/evaluations/comprehensive"
files <- list.files(eval_dir, pattern = "_evaluationTotalReport\\.csv$", full.names = TRUE)
cat("Reading", length(files), "evaluation files\n")

all_eval <- do.call(rbind, lapply(files, function(f) {
  tryCatch(read.csv(f, stringsAsFactors = FALSE), error = function(e) NULL)
}))
cat("Total rows:", nrow(all_eval), "\n")
cat("Unique species:", length(unique(all_eval$Species)), "\n")

# Focus on held-out validation metrics for individual models
# Filter to individual models with validation column populated
indiv <- all_eval[all_eval$Type == "individual" & !is.na(all_eval$validation), ]
cat("Individual model rows with validation:", nrow(indiv), "\n")

# Per-species summary: best TSS and ROC from held-out validation
sp_summary <- do.call(rbind, by(indiv, indiv$Species, function(sp_data) {
  tss_rows <- sp_data[sp_data$metric.eval == "TSS", ]
  roc_rows <- sp_data[sp_data$metric.eval == "ROC", ]
  
  data.frame(
    accepted_taxon = sp_data$Species[1],
    n_models = length(unique(sp_data$full.name)),
    best_TSS_val = if(nrow(tss_rows) > 0) max(tss_rows$validation, na.rm = TRUE) else NA,
    best_ROC_val = if(nrow(roc_rows) > 0) max(roc_rows$validation, na.rm = TRUE) else NA,
    median_TSS_val = if(nrow(tss_rows) > 0) median(tss_rows$validation, na.rm = TRUE) else NA,
    median_ROC_val = if(nrow(roc_rows) > 0) median(roc_rows$validation, na.rm = TRUE) else NA,
    stringsAsFactors = FALSE
  )
}))

cat("Species with TSS/ROC extracted:", nrow(sp_summary), "\n")
cat("TSS distribution:\n")
print(summary(sp_summary$best_TSS_val))
cat("\nROC distribution:\n")
print(summary(sp_summary$best_ROC_val))

# Count by selection tier
sp_summary$selection_tier <- ifelse(
  sp_summary$best_TSS_val > 0.8 & sp_summary$best_ROC_val > 0.8, "PRIMARY",
  ifelse(sp_summary$best_TSS_val > 0.7 & sp_summary$best_ROC_val > 0.7, "FALLBACK",
         "BELOW_THRESHOLD")
)
cat("\nSelection tier counts:\n")
print(table(sp_summary$selection_tier))

# Write summary
write.csv(sp_summary, "<NON_NATIVE_ROOT>/temp/eval_TSS_ROC_784.csv", row.names = FALSE)
cat("\nWritten to temp/eval_TSS_ROC_784.csv\n")

# Compute descriptive statistics for manuscript
cat("\n=== TSS descriptive statistics ===\n")
tss <- sp_summary$best_TSS_val[!is.na(sp_summary$best_TSS_val)]
cat(sprintf("N=%d Mean=%.4f SD=%.4f Min=%.4f Q05=%.4f Q25=%.4f Median=%.4f Q75=%.4f Q95=%.4f Max=%.4f\n",
    length(tss), mean(tss), sd(tss), min(tss),
    quantile(tss, 0.05), quantile(tss, 0.25), median(tss),
    quantile(tss, 0.75), quantile(tss, 0.95), max(tss)))

cat("\n=== ROC descriptive statistics ===\n")
roc <- sp_summary$best_ROC_val[!is.na(sp_summary$best_ROC_val)]
cat(sprintf("N=%d Mean=%.4f SD=%.4f Min=%.4f Q05=%.4f Q25=%.4f Median=%.4f Q75=%.4f Q95=%.4f Max=%.4f\n",
    length(roc), mean(roc), sd(roc), min(roc),
    quantile(roc, 0.05), quantile(roc, 0.25), median(roc),
    quantile(roc, 0.75), quantile(roc, 0.95), max(roc)))
