# Original source: extract_evaluations_v2.R
# Original source location: ${WORK_ROOT}/archive/v1_deprecated/extract_evaluations_v2.R
# Original source SHA-256: 23f272f743b324895b45a7280b5363666583bb9a4a2a4acf2112f7fc6b8755e7
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/usr/bin/env Rscript
# Fixed evaluation extraction for all 813 species
# Fixes: calib.lines slot name, better raster handling

renv_lib <- "<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu"
.libPaths(c(renv_lib, .libPaths()))

library(biomod2)
library(dplyr)
library(terra)

setwd("<WORK_ROOT>")

lineage <- read.csv("<NON_NATIVE_ROOT>/00_audit/phase1b/species_lineage_matrix.csv", stringsAsFactors = FALSE)
lineage <- lineage[lineage$species != "Opuntia.ficus-indica", ]
cat("Total species:", nrow(lineage), "\n")

eval_out <- "<NON_NATIVE_ROOT>/audit/technical_validation/model_technical_validation.csv"

# ---- Part 1: Model evaluations ----
cat("\n=== Extracting model evaluations ===\n")
eval_results <- data.frame()

for (i in seq_len(nrow(lineage))) {
  sp <- lineage$species[i]
  rds_file <- file.path("results/individual_models", paste0(sp, "_individual.rds"))
  
  if (!file.exists(rds_file)) {
    eval_results <- rbind(eval_results, data.frame(
      species=sp, n_occ_raw="NO_RDS", n_occ_clean="NO_RDS", n_pseudoabsence="NO_RDS",
      algorithms="NO_RDS", cv_scheme="NO_RDS", auc_median=NA, tss_median=NA,
      ensemble_method="NO_RDS", passed_threshold_yes_no="NO_RDS",
      raster_min=NA, raster_max=NA, raster_mean=NA, raster_na_fraction=NA,
      raster_extent_ok=NA, raster_resolution_ok=NA, raster_crs_ok=NA,
      validation_status="no_model_rds", notes="Individual model RDS not found",
      stringsAsFactors=FALSE))
    next
  }
  
  tryCatch({
    mod <- readRDS(rds_file)
    evals <- get_evaluations(mod)
    
    tss_vals <- evals %>% filter(metric.eval == "TSS") %>% pull(validation)
    roc_vals <- evals %>% filter(metric.eval == "ROC") %>% pull(validation)
    algos <- paste(unique(evals$algo), collapse=";")
    
    # Get CV strategy from options
    cv_strat <- tryCatch(mod@models.options@CV.strategy, error=function(e) "unknown")
    cv_k <- tryCatch(mod@models.options@CV.k, error=function(e) NA)
    
    eval_results <- rbind(eval_results, data.frame(
      species = sp,
      n_occ_raw = "[UNCLEAR]",
      n_occ_clean = "[UNCLEAR]",
      n_pseudoabsence = "[UNCLEAR]",
      algorithms = algos,
      cv_scheme = paste0(cv_strat, if(!is.na(cv_k)) paste0("_k",cv_k) else ""),
      auc_median = round(median(roc_vals, na.rm = TRUE), 4),
      tss_median = round(median(tss_vals, na.rm = TRUE), 4),
      ensemble_method = "[UNCLEAR]",
      passed_threshold_yes_no = ifelse(median(tss_vals, na.rm = TRUE) >= 0.8, "yes", "no"),
      raster_min = NA,
      raster_max = NA,
      raster_mean = NA,
      raster_na_fraction = NA,
      raster_extent_ok = NA,
      raster_resolution_ok = NA,
      raster_crs_ok = NA,
      validation_status = ifelse(median(tss_vals, na.rm = TRUE) >= 0.7, "pass", "marginal"),
      notes = "",
      stringsAsFactors = FALSE
    ))
    if (i %% 100 == 0) cat(i, "/", nrow(lineage), "evaluations extracted\n")
  }, error = function(e) {
    eval_results <<- rbind(eval_results, data.frame(
      species=sp, n_occ_raw="[UNCLEAR]", n_occ_clean="[UNCLEAR]", n_pseudoabsence="[UNCLEAR]",
      algorithms="EXTRACTION_FAILED", cv_scheme="EXTRACTION_FAILED", auc_median=NA, tss_median=NA,
      ensemble_method="[UNCLEAR]", passed_threshold_yes_no="EXTRACTION_FAILED",
      raster_min=NA, raster_max=NA, raster_mean=NA, raster_na_fraction=NA,
      raster_extent_ok=NA, raster_resolution_ok=NA, raster_crs_ok=NA,
      validation_status="extraction_failed", notes=conditionMessage(e),
      stringsAsFactors=FALSE))
  })
}

write.csv(eval_results, eval_out, row.names = FALSE)
cat("Evaluations written:", nrow(eval_results), "species\n")

# Summary stats
ok_rows <- eval_results$validation_status %in% c("pass", "marginal")
cat("Extracted:", sum(ok_rows), "/ Failed:", sum(!ok_rows), "\n")
if (sum(ok_rows) > 0) {
  cat("TSS median (extracted):", median(eval_results$tss_median[ok_rows], na.rm=TRUE), "\n")
  cat("AUC median (extracted):", median(eval_results$auc_median[ok_rows], na.rm=TRUE), "\n")
}
cat("\n=== DONE ===\n")
