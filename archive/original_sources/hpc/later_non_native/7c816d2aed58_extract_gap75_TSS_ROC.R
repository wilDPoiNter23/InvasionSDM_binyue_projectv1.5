# Original source: extract_gap75_TSS_ROC.R
# Original source location: ${NON_NATIVE_ROOT}/temp/extract_gap75_TSS_ROC.R
# Original source SHA-256: 7c816d2aed581d9a168d5f4a7214583fcfe8c2355b36b2b5b32a095530569f8a
# Recovery: live read-only SSH, 2026-10-12
# Classification: SUPPORTING (HISTORICAL_SUPPORTING_AUDIT_LINEAGE_HELPER)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
.libPaths(c(.libPaths(), "<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu"))
suppressPackageStartupMessages(library(biomod2))

# ============================================================
# Extract TSS/ROC for gap75 species (54 VIIRS35 + 8 DIRECT34)
# and fill in any remaining gaps
# ============================================================

manifest <- read.csv("<WORK_ROOT>/publication_release_v1/manifests/final_accepted_projection_manifest_813_v1.csv", stringsAsFactors = FALSE)
cat("Manifest species:", nrow(manifest), "\n")

# Read existing 784 eval
eval_784 <- read.csv("<NON_NATIVE_ROOT>/temp/eval_TSS_ROC_784.csv", stringsAsFactors = FALSE)
cat("Existing eval species:", nrow(eval_784), "\n")

# Read gap13 repair selection
repair <- read.csv("<WORK_ROOT>/publication_release_v1/manifests/gap13_repair_projection_selection_final_v2.csv", stringsAsFactors = FALSE)
cat("Repair species:", nrow(repair), "\n")

# Find gap75 species (both VIIRS35 and DIRECT34)
gap75_species <- manifest$accepted_taxon[manifest$source_generation %in% c("GAP75_VIIRS35_V1", "GAP75_DIRECT34_V1")]
cat("Gap75 species total:", length(gap75_species), "\n")

# Find which gap75 are NOT in 784 eval
gap75_missing <- setdiff(gap75_species, eval_784$accepted_taxon)
cat("Gap75 missing from eval:", length(gap75_missing), "\n")
cat("Gap75 missing list:\n")
print(gap75_missing)

# Find repair species
repair_species <- manifest$accepted_taxon[manifest$source_generation == "GAP13_REPAIR35_V1"]
cat("\nRepair species:", length(repair_species), "\n")

# Find Spartina
spartina_sp <- manifest$accepted_taxon[manifest$source_generation == "SPARTINA_FINAL_V1"]
cat("Spartina:", spartina_sp, "\n")

# For gap75 species, extract from .models.out files
models_dir <- "<WORK_ROOT>/models"
results <- data.frame()

for (sp_name in gap75_missing) {
  # Convert taxon name to directory pattern
  sp_dir_name <- gsub(" ", ".", sp_name)
  sp_dir <- file.path(models_dir, sp_dir_name)
  
  cat("\n--- Processing:", sp_name, "---\n")
  cat("Dir:", sp_dir, " exists:", dir.exists(sp_dir), "\n")
  
  if (!dir.exists(sp_dir)) {
    cat("  SKIP: directory not found\n")
    results <- rbind(results, data.frame(
      accepted_taxon = sp_name,
      n_models = NA,
      best_TSS_val = NA,
      best_ROC_val = NA,
      median_TSS_val = NA,
      median_ROC_val = NA,
      selection_tier = "EXTRACTION_FAILED",
      source = "DIR_NOT_FOUND",
      stringsAsFactors = FALSE
    ))
    next
  }
  
  # Find .models.out file
  mo_files <- list.files(sp_dir, pattern = "\\.models\\.out$", recursive = TRUE, full.names = TRUE)
  # Also check the subdirectory structure
  if (length(mo_files) == 0) {
    # Try looking in the gap13 repair results directory
    alt_dir <- file.path("<WORK_ROOT>/results/gap13_model_repair_fullcoverage_v1")
    alt_files <- list.files(alt_dir, pattern = paste0(gsub(" ", "_", sp_name), ".*\\.rds$"), full.names = TRUE)
    cat("  Alt RDS files:", length(alt_files), "\n")
    
    if (length(alt_files) == 0) {
      results <- rbind(results, data.frame(
        accepted_taxon = sp_name,
        n_models = NA,
        best_TSS_val = NA,
        best_ROC_val = NA,
        median_TSS_val = NA,
        median_ROC_val = NA,
        selection_tier = "EXTRACTION_FAILED",
        source = "NO_MODEL_FILE",
        stringsAsFactors = FALSE
      ))
      next
    }
  }
  
  cat("  Model files found:", length(mo_files), "\n")
  
  # Try to load and extract
  tryCatch({
    mo <- get(load(mo_files[1]))
    cat("  Class:", class(mo)[1], "\n")
    
    # Get evaluation data
    ev <- get_evaluations(mo)
    cat("  Eval rows:", nrow(ev), "\n")
    
    # Filter to held-out validation
    val_ev <- ev[ev$validation == TRUE | !is.na(ev$evaluation), ]
    if (nrow(val_ev) == 0) val_ev <- ev  # fallback to all
    
    tss_vals <- val_ev$TSS[!is.na(val_ev$TSS)]
    roc_vals <- val_ev$ROC[!is.na(val_ev$ROC)]
    
    cat("  TSS values:", length(tss_vals), " ROC values:", length(roc_vals), "\n")
    
    results <- rbind(results, data.frame(
      accepted_taxon = sp_name,
      n_models = length(unique(val_ev$full.name)),
      best_TSS_val = if(length(tss_vals)>0) max(tss_vals) else NA,
      best_ROC_val = if(length(roc_vals)>0) max(roc_vals) else NA,
      median_TSS_val = if(length(tss_vals)>0) median(tss_vals) else NA,
      median_ROC_val = if(length(roc_vals)>0) median(roc_vals) else NA,
      selection_tier = "EXTRACTED",
      source = "GAP75_MODELS_OUT",
      stringsAsFactors = FALSE
    ))
  }, error = function(e) {
    cat("  ERROR:", e$message, "\n")
    results <- rbind(results, data.frame(
      accepted_taxon = sp_name,
      n_models = NA,
      best_TSS_val = NA,
      best_ROC_val = NA,
      median_TSS_val = NA,
      median_ROC_val = NA,
      selection_tier = "EXTRACTION_FAILED",
      source = paste0("ERROR:", e$message),
      stringsAsFactors = FALSE
    ))
  })
}

# Write partial results
write.csv(results, "<NON_NATIVE_ROOT>/temp/gap75_TSS_ROC_extraction.csv", row.names = FALSE)
cat("\n\n=== Extraction complete ===\n")
cat("Extracted:", sum(results$selection_tier == "EXTRACTED"), "/", nrow(results), "\n")
cat("Failed:", sum(results$selection_tier == "EXTRACTION_FAILED"), "/", nrow(results), "\n")
