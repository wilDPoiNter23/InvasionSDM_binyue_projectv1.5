# Original source: extract_missing18.R
# Original source location: ${NON_NATIVE_ROOT}/temp/extract_missing18.R
# Original source SHA-256: ee8601194f53b93aa7de9644c5fa0700f6be4583917238a64012a04f33f15e4a
# Recovery: live read-only SSH, 2026-10-12
# Classification: SUPPORTING (HISTORICAL_SUPPORTING_AUDIT_LINEAGE_HELPER)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
.libPaths(c(.libPaths(), "<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu"))
suppressPackageStartupMessages(library(biomod2))

# Read missing taxa list
missing <- readLines("/tmp/missing18_taxa.txt")
cat("Target: extract TSS/ROC for", length(missing), "taxa\n\n")

models_dir <- "<WORK_ROOT>/models"

# Results containers
long_results <- data.frame()
summary_results <- data.frame()

for (sp_name in missing) {
  sp_dir_name <- gsub(" ", ".", sp_name)
  sp_dir <- file.path(models_dir, sp_dir_name)
  
  cat("---", sp_name, "---\n")
  cat("  Dir exists:", dir.exists(sp_dir), "\n")
  
  if (!dir.exists(sp_dir)) {
    summary_results <- rbind(summary_results, data.frame(
      accepted_taxon = sp_name,
      best_TSS = NA, median_TSS = NA, best_ROC = NA, median_ROC = NA,
      valid_model_count = NA, evaluation_source = "DIR_NOT_FOUND",
      extraction_QC_pass = FALSE, stringsAsFactors = FALSE
    ))
    next
  }
  
  # Find .models.out files — EXCLUDE ensemble ones
  all_mo <- list.files(sp_dir, pattern = "\\.models\\.out$", recursive = TRUE, full.names = TRUE)
  # Also check subdirectory structure
  if (length(all_mo) == 0) {
    all_mo <- list.files(file.path(sp_dir, sp_dir_name), pattern = "\\.models\\.out$", recursive = TRUE, full.names = TRUE)
  }
  
  # Filter out ensemble model files
  mo_files <- all_mo[!grepl("ensemble", all_mo, ignore.case = TRUE)]
  cat("  .models.out files (non-ensemble):", length(mo_files), "\n")
  
  if (length(mo_files) == 0) {
    summary_results <- rbind(summary_results, data.frame(
      accepted_taxon = sp_name,
      best_TSS = NA, median_TSS = NA, best_ROC = NA, median_ROC = NA,
      valid_model_count = NA, evaluation_source = "NO_NON_ENSEMBLE_MODELS_OUT",
      extraction_QC_pass = FALSE, stringsAsFactors = FALSE
    ))
    next
  }
  
  # Load first non-ensemble .models.out
  mo_file <- mo_files[1]
  cat("  Loading:", basename(mo_file), "\n")
  
  tryCatch({
    mo <- get(load(mo_file))
    cat("  Class:", class(mo)[1], "\n")
    
    # Get evaluations
    ev <- get_evaluations(mo)
    cat("  Eval rows:", nrow(ev), "\n")
    
    # Inspect columns
    cat("  Eval columns:", paste(colnames(ev), collapse=", "), "\n")
    
    # The 'validation' column should contain numeric TSS/ROC values
    # for held-out validation. Let's check what's in it.
    cat("  Unique metric.eval values:", paste(unique(ev$metric.eval), collapse=", "), "\n")
    cat("  validation column class:", class(ev$validation), "\n")
    
    # Filter to rows where validation is non-NA (these are held-out metrics)
    val_ev <- ev[!is.na(ev$validation), ]
    cat("  Rows with non-NA validation:", nrow(val_ev), "\n")
    
    if (nrow(val_ev) == 0) {
      # Try alternative: maybe validation column is logical?
      cat("  Trying validation as logical filter...\n")
      if (is.logical(ev$validation)) {
        val_ev <- ev[ev$validation == TRUE, ]
        cat("  Rows with validation==TRUE:", nrow(val_ev), "\n")
      } else if ("evaluation" %in% colnames(ev)) {
        # Maybe evaluation column has the values
        val_ev <- ev[!is.na(ev$evaluation), ]
        cat("  Rows with non-NA evaluation:", nrow(val_ev), "\n")
      }
    }
    
    if (nrow(val_ev) == 0) {
      cat("  WARNING: No validation rows found. Trying all rows with metric values.\n")
      val_ev <- ev[!is.na(ev$calibration) | !is.na(ev$validation), ]
      cat("  Rows:", nrow(val_ev), "\n")
    }
    
    # Extract TSS and ROC
    tss_rows <- val_ev[val_ev$metric.eval == "TSS", ]
    roc_rows <- val_ev[val_ev$metric.eval == "ROC", ]
    
    # Get the validation values
    tss_vals <- if (nrow(tss_rows) > 0) {
      if ("validation" %in% colnames(tss_rows) && any(!is.na(tss_rows$validation))) {
        tss_rows$validation[!is.na(tss_rows$validation)]
      } else if ("evaluation" %in% colnames(tss_rows)) {
        tss_rows$evaluation[!is.na(tss_rows$evaluation)]
      } else {
        tss_rows$calibration[!is.na(tss_rows$calibration)]
      }
    } else { numeric(0) }
    
    roc_vals <- if (nrow(roc_rows) > 0) {
      if ("validation" %in% colnames(roc_rows) && any(!is.na(roc_rows$validation))) {
        roc_rows$validation[!is.na(roc_rows$validation)]
      } else if ("evaluation" %in% colnames(roc_rows)) {
        roc_rows$evaluation[!is.na(roc_rows$evaluation)]
      } else {
        roc_rows$calibration[!is.na(roc_rows$calibration)]
      }
    } else { numeric(0) }
    
    cat("  TSS values (n=", length(tss_vals), "):", 
        if(length(tss_vals)>0) sprintf("min=%.3f max=%.3f median=%.3f", min(tss_vals), max(tss_vals), median(tss_vals)) else "NONE", "\n")
    cat("  ROC values (n=", length(roc_vals), "):", 
        if(length(roc_vals)>0) sprintf("min=%.3f max=%.3f median=%.3f", min(roc_vals), max(roc_vals), median(roc_vals)) else "NONE", "\n")
    
    # Build long-format rows
    for (j in seq_len(nrow(val_ev))) {
      long_results <- rbind(long_results, data.frame(
        accepted_taxon = sp_name,
        model_name = val_ev$full.name[j] %||% val_ev$models[j] %||% NA,
        algorithm = val_ev$algo[j] %||% NA,
        PA = val_ev$PA[j] %||% NA,
        RUN = val_ev$run[j] %||% NA,
        metric = val_ev$metric.eval[j],
        validation_value = val_ev$validation[j] %||% NA,
        calibration_value = val_ev$calibration[j] %||% NA,
        metric_source = "get_evaluations_from_models.out",
        model_file = basename(mo_file),
        heldout_valid = !is.na(val_ev$validation[j]),
        notes = "",
        stringsAsFactors = FALSE
      ))
    }
    
    # Build summary row
    summary_results <- rbind(summary_results, data.frame(
      accepted_taxon = sp_name,
      best_TSS = if(length(tss_vals)>0) max(tss_vals) else NA,
      median_TSS = if(length(tss_vals)>0) median(tss_vals) else NA,
      best_ROC = if(length(roc_vals)>0) max(roc_vals) else NA,
      median_ROC = if(length(roc_vals)>0) median(roc_vals) else NA,
      valid_model_count = length(unique(val_ev$full.name %||% val_ev$models)),
      evaluation_source = "models.out_heldout_validation",
      extraction_QC_pass = length(tss_vals) > 0 && length(roc_vals) > 0,
      stringsAsFactors = FALSE
    ))
    
  }, error = function(e) {
    cat("  ERROR:", e$message, "\n")
    summary_results <- rbind(summary_results, data.frame(
      accepted_taxon = sp_name,
      best_TSS = NA, median_TSS = NA, best_ROC = NA, median_ROC = NA,
      valid_model_count = NA, evaluation_source = paste0("ERROR:", e$message),
      extraction_QC_pass = FALSE, stringsAsFactors = FALSE
    ))
  })
}

# Write outputs
dir.create("<NON_NATIVE_ROOT>/publication_tables", showWarnings=FALSE, recursive=TRUE)
write.csv(long_results, "<NON_NATIVE_ROOT>/publication_tables/missing18_model_evaluations_long.csv", row.names=FALSE)
write.csv(summary_results, "<NON_NATIVE_ROOT>/publication_tables/missing18_model_performance_summary.csv", row.names=FALSE)

cat("\n=== FINAL SUMMARY ===\n")
cat("Taxa processed:", nrow(summary_results), "/", length(missing), "\n")
cat("TSS recovered:", sum(!is.na(summary_results$best_TSS)), "\n")
cat("ROC recovered:", sum(!is.na(summary_results$best_ROC)), "\n")
cat("Both recovered:", sum(!is.na(summary_results$best_TSS) & !is.na(summary_results$best_ROC)), "\n")
cat("QC pass:", sum(summary_results$extraction_QC_pass), "\n")

# Print the summaries
print(summary_results[, c("accepted_taxon","best_TSS","best_ROC","extraction_QC_pass")])
