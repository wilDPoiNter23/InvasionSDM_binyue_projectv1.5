# Original source: extract_missing18_v2.R
# Original source location: ${NON_NATIVE_ROOT}/temp/extract_missing18_v2.R
# Original source SHA-256: f9c067b426391471431d408e8bb97777b99cca3807e8cf39c7da829c0737ce60
# Recovery: live read-only SSH, 2026-10-12
# Classification: SUPPORTING (HISTORICAL_SUPPORTING_AUDIT_LINEAGE_HELPER)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
.libPaths(c(.libPaths(), "<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu"))
suppressPackageStartupMessages(library(biomod2))

models_dir <- "<WORK_ROOT>/models"

missing_sp <- c(
  "Bryophyllum fedtschenkoi", "Euphorbia esula", "Euphorbia maculata",
  "Mimosa bimucronata", "Mimosa diplotricha", "Mimosa pudica",
  "Mirabilis jalapa", "Molineria capitulata", "Moorochloa eruciformis",
  "Morus alba", "Mucuna bracteata", "Musa ornata",
  "Pennisetum macrostachyum", "Psidium cattleyanum", "Pueraria phaseoloides",
  "Pyracantha rogersiana", "Soehrensia schickendantzii", "Xanthium italicum"
)

results <- data.frame(stringsAsFactors = FALSE)

for (sp_name in missing_sp) {
  cat("\n=== ", sp_name, " ===\n")
  sp_dir <- file.path(models_dir, gsub(" ", ".", sp_name))
  
  # Find INDIVIDUAL models.out (NOT ensemble.models.out)
  # Pattern: ends with .models.out but NOT .ensemble.models.out
  all_mo <- list.files(sp_dir, pattern = "\\.models\\.out$", full.names = TRUE)
  indiv_mo <- all_mo[!grepl("ensemble\\.models\\.out$", all_mo, ignore.case = TRUE)]
  
  if (length(indiv_mo) == 0) {
    # Try .BIOMOD_DATA subdirectory
    biomod_dir <- file.path(sp_dir, ".BIOMOD_DATA")
    all_mo <- list.files(biomod_dir, pattern = "\\.models\\.out$", recursive = TRUE, full.names = TRUE)
    indiv_mo <- all_mo[!grepl("ensemble\\.models\\.out$", all_mo, ignore.case = TRUE)]
  }
  
  if (length(indiv_mo) == 0) {
    cat("  SKIP: no individual .models.out found (only ensemble)\n")
    results <- rbind(results, data.frame(
      accepted_taxon = sp_name, n_models = NA, best_TSS_val = NA, best_ROC_val = NA,
      median_TSS_val = NA, median_ROC_val = NA,
      selection_tier = "EXTRACTION_FAILED", source = "ONLY_ENSEMBLE",
      stringsAsFactors = FALSE
    ))
    next
  }
  
  mo_file <- indiv_mo[1]
  cat("  Individual models.out:", basename(mo_file), "\n")
  
  tryCatch({
    mo <- get(load(mo_file))
    cat("  Class:", class(mo)[1], "\n")
    
    # Get evaluations — validation column has NUMERIC TSS/ROC values
    ev <- get_evaluations(mo)
    cat("  Evaluation rows:", nrow(ev), "cols:", paste(colnames(ev), collapse=", "), "\n")
    
    # Check column names for validation data
    # validation column = cross-validated metric values (NUMERIC, not boolean)
    if (!"validation" %in% colnames(ev)) {
      cat("  No validation column. Cols:", paste(colnames(ev), collapse=", "), "\n")
      results <- rbind(results, data.frame(
        accepted_taxon = sp_name, n_models = NA, best_TSS_val = NA, best_ROC_val = NA,
        median_TSS_val = NA, median_ROC_val = NA,
        selection_tier = "EXTRACTION_FAILED", source = "NO_VAL_COL",
        stringsAsFactors = FALSE
      ))
    } else {
      # TSS: rows where metric.eval == "TSS", get validation values
      tss_rows <- ev[ev$metric.eval == "TSS" & !is.na(ev$validation), ]
      roc_rows <- ev[ev$metric.eval == "ROC" & !is.na(ev$validation), ]
      
      tss_vals <- tss_rows$validation
      roc_vals <- roc_rows$validation
      
      cat("  TSS validation values:", length(tss_vals), "\n")
      cat("  ROC validation values:", length(roc_vals), "\n")
      
      if (length(tss_vals) > 0) {
        cat(sprintf("  TSS: min=%.4f max=%.4f median=%.4f\n", 
                    min(tss_vals), max(tss_vals), median(tss_vals)))
      }
      if (length(roc_vals) > 0) {
        cat(sprintf("  ROC: min=%.4f max=%.4f median=%.4f\n",
                    min(roc_vals), max(roc_vals), median(roc_vals)))
      }
      
      best_tss <- if(length(tss_vals) > 0) max(tss_vals) else NA
      best_roc <- if(length(roc_vals) > 0) max(roc_vals) else NA
      med_tss <- if(length(tss_vals) > 0) median(tss_vals) else NA
      med_roc <- if(length(roc_vals) > 0) median(roc_vals) else NA
      
      n_models <- length(unique(ev$full.name))
      
      # Frozen thresholds
      tier <- if (is.na(best_tss) || is.na(best_roc)) {
        "PENDING"
      } else if (best_tss > 0.8 && best_roc > 0.8) {
        "PRIMARY"
      } else if (best_tss > 0.7 && best_roc > 0.7) {
        "FALLBACK"
      } else {
        "BELOW_THRESHOLD"
      }
      
      results <- rbind(results, data.frame(
        accepted_taxon = sp_name,
        n_models = n_models,
        best_TSS_val = best_tss,
        best_ROC_val = best_roc,
        median_TSS_val = med_tss,
        median_ROC_val = med_roc,
        selection_tier = tier,
        source = "MODELS_OUT_VALIDATION",
        stringsAsFactors = FALSE
      ))
    }
  }, error = function(e) {
    cat("  ERROR:", conditionMessage(e), "\n")
    results <- rbind(results, data.frame(
      accepted_taxon = sp_name, n_models = NA, best_TSS_val = NA, best_ROC_val = NA,
      median_TSS_val = NA, median_ROC_val = NA,
      selection_tier = "EXTRACTION_FAILED", 
      source = paste0("ERROR:", substr(conditionMessage(e), 1, 80)),
      stringsAsFactors = FALSE
    ))
  })
}

# Summary
cat("\n\n========================================\n")
cat("EXTRACTION COMPLETE\n")
cat("========================================\n")
cat("Extracted:", sum(results$selection_tier %in% c("PRIMARY","FALLBACK","BELOW_THRESHOLD")), "/ 18\n")
cat("Failed:", sum(results$selection_tier == "EXTRACTION_FAILED"), "/ 18\n")
cat("Pending:", sum(results$selection_tier == "PENDING"), "/ 18\n")

cat("\nSelection tiers:\n")
print(table(results$selection_tier))

cat("\nTSS summary:\n")
tss <- results$best_TSS_val[!is.na(results$best_TSS_val)]
if (length(tss) > 0) print(summary(tss))

cat("\nROC summary:\n")
roc <- results$best_ROC_val[!is.na(results$best_ROC_val)]
if (length(roc) > 0) print(summary(roc))

write.csv(results, "<NON_NATIVE_ROOT>/temp/missing18_TSS_ROC_v2.csv", row.names = FALSE)
cat("\nWritten to temp/missing18_TSS_ROC_v2.csv\n")
