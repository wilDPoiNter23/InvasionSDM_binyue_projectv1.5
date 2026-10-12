# Original source: extract_missing18_TSS_ROC.R
# Original source location: ${NON_NATIVE_ROOT}/temp/extract_missing18_TSS_ROC.R
# Original source SHA-256: b45ab386cb11e2eb87b7500d1b109d8aa2d9cd7ec950b4edbbc93073440bac44
# Recovery: live read-only SSH, 2026-10-12
# Classification: SUPPORTING (HISTORICAL_SUPPORTING_AUDIT_LINEAGE_HELPER)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
.libPaths(c(.libPaths(), "<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu"))
suppressPackageStartupMessages(library(biomod2))

# ============================================================
# Extract TSS/ROC for 18 missing ORIGINAL_FROZEN species
# ============================================================

models_dir <- "<WORK_ROOT>/models"
manifest <- read.csv("<WORK_ROOT>/publication_release_v1/manifests/final_accepted_projection_manifest_813_v1.csv", stringsAsFactors = FALSE)

# 18 missing species
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
  cat("\n=== Processing:", sp_name, "===\n")
  sp_dir_name <- gsub(" ", ".", sp_name)
  sp_dir <- file.path(models_dir, sp_dir_name)
  
  # Find .models.out (handles both patterns)
  mo_files <- list.files(sp_dir, pattern = "\\.models\\.out$", recursive = FALSE, full.names = TRUE)
  # Also check for .BIOMOD_DATA subdirectory
  if (length(mo_files) == 0) {
    biomod_dir <- file.path(sp_dir, ".BIOMOD_DATA")
    if (dir.exists(biomod_dir)) {
      mo_files <- list.files(biomod_dir, pattern = "\\.models\\.out$", recursive = TRUE, full.names = TRUE)
    }
  }
  
  if (length(mo_files) == 0) {
    cat("  SKIP: no .models.out found\n")
    results <- rbind(results, data.frame(
      accepted_taxon = sp_name, n_models = NA, best_TSS_val = NA, best_ROC_val = NA,
      median_TSS_val = NA, median_ROC_val = NA,
      selection_tier = "EXTRACTION_FAILED", source = "NO_MODEL_FILE",
      stringsAsFactors = FALSE
    ))
    next
  }
  
  # Use the first models.out (typically the primary one, not ensemble)
  # Prefer non-ensemble .models.out for individual model evaluations
  primary_mo <- mo_files[!grepl("ensemble", mo_files, ignore.case = TRUE)]
  if (length(primary_mo) == 0) primary_mo <- mo_files
  
  mo_file <- primary_mo[1]
  cat("  Using:", basename(mo_file), "\n")
  
  tryCatch({
    mo <- get(load(mo_file))
    cat("  Class:", class(mo)[1], "\n")
    
    # Get evaluation data from the models.out object
    ev <- get_evaluations(mo)
    cat("  Evaluation rows:", nrow(ev), "\n")
    
    # Determine which column has validation values
    # biomod2 stores: calibration (training), validation (CV held-out), evaluation (final)
    val_col <- NULL
    if ("validation" %in% colnames(ev)) {
      val_col <- "validation"
    } else if ("evaluation" %in% colnames(ev)) {
      val_col <- "evaluation"
    } else if ("calibration" %in% colnames(ev)) {
      val_col <- "calibration"
    }
    
    if (is.null(val_col)) {
      cat("  No evaluation columns found. Cols:", paste(colnames(ev), collapse=", "), "\n")
      results <- rbind(results, data.frame(
        accepted_taxon = sp_name, n_models = NA, best_TSS_val = NA, best_ROC_val = NA,
        median_TSS_val = NA, median_ROC_val = NA,
        selection_tier = "EXTRACTION_FAILED", source = "NO_VAL_COL",
        stringsAsFactors = FALSE
      ))
    } else {
      cat("  Using validation column:", val_col, "\n")
      
      # Get TSS and ROC from validation
      tss_vals <- ev$TSS[!is.na(ev[[val_col]]) & ev[[val_col]] == TRUE]
      roc_vals <- ev$ROC[!is.na(ev[[val_col]]) & ev[[val_col]] == TRUE]
      
      # If no TRUE validation, use all rows with non-NA validation values
      if (length(tss_vals) == 0) {
        tss_vals <- ev$TSS[!is.na(ev[[val_col]])]
        roc_vals <- ev$ROC[!is.na(ev[[val_col]])]
      }
      
      cat("  TSS values:", length(tss_vals), "ROC values:", length(roc_vals), "\n")
      
      if (length(tss_vals) > 0) {
        cat(sprintf("  TSS range: %.4f - %.4f\n", min(tss_vals), max(tss_vals)))
      }
      if (length(roc_vals) > 0) {
        cat(sprintf("  ROC range: %.4f - %.4f\n", min(roc_vals), max(roc_vals)))
      }
      
      best_tss <- if(length(tss_vals) > 0) max(tss_vals, na.rm = TRUE) else NA
      best_roc <- if(length(roc_vals) > 0) max(roc_vals, na.rm = TRUE) else NA
      med_tss <- if(length(tss_vals) > 0) median(tss_vals, na.rm = TRUE) else NA
      med_roc <- if(length(roc_vals) > 0) median(roc_vals, na.rm = TRUE) else NA
      
      # Determine selection tier
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
        n_models = length(unique(ev$full.name)),
        best_TSS_val = best_tss,
        best_ROC_val = best_roc,
        median_TSS_val = med_tss,
        median_ROC_val = med_roc,
        selection_tier = tier,
        source = "MODELS_OUT_EXTRACTION",
        stringsAsFactors = FALSE
      ))
    }
  }, error = function(e) {
    cat("  ERROR:", conditionMessage(e), "\n")
    results <- rbind(results, data.frame(
      accepted_taxon = sp_name, n_models = NA, best_TSS_val = NA, best_ROC_val = NA,
      median_TSS_val = NA, median_ROC_val = NA,
      selection_tier = "EXTRACTION_FAILED", source = paste0("ERROR:", substr(conditionMessage(e), 1, 80)),
      stringsAsFactors = FALSE
    ))
  })
}

# Print summary
cat("\n\n========================================\n")
cat("EXTRACTION COMPLETE\n")
cat("========================================\n")
cat("Successfully extracted:", sum(results$selection_tier != "EXTRACTION_FAILED" & results$selection_tier != "PENDING"), "/ 18\n")
cat("Failed:", sum(results$selection_tier == "EXTRACTION_FAILED"), "/ 18\n")
cat("\nSelection tiers:\n")
print(table(results$selection_tier))

cat("\nTSS summary:\n")
tss <- results$best_TSS_val[!is.na(results$best_TSS_val)]
if (length(tss) > 0) print(summary(tss))

cat("\nROC summary:\n")
roc <- results$best_ROC_val[!is.na(results$best_ROC_val)]
if (length(roc) > 0) print(summary(roc))

# Write output
write.csv(results, "<NON_NATIVE_ROOT>/temp/missing18_TSS_ROC.csv", row.names = FALSE)
cat("\nWritten to temp/missing18_TSS_ROC.csv\n")
