# Original source: extract_evaluations.R
# Original source location: ${WORK_ROOT}/archive/v1_deprecated/extract_evaluations.R
# Original source SHA-256: 1f340758d2818a2eda477e5651b9192c8479d02a130c3701c936f1ed6b7c24cb
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/usr/bin/env Rscript
# Extract model evaluation metrics for all 813 species
# Usage: Rscript extract_evaluations.R

renv_lib <- "<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu"
.libPaths(c(renv_lib, .libPaths()))

library(biomod2)
library(dplyr)
library(tidyr)
library(terra)

setwd("<WORK_ROOT>")

# Load species list
lineage <- read.csv("<NON_NATIVE_ROOT>/00_audit/phase1b/species_lineage_matrix.csv", stringsAsFactors = FALSE)
lineage <- lineage[lineage$species != "Opuntia.ficus-indica", ]
cat("Total species:", nrow(lineage), "\n")

# Output files
eval_out <- "<NON_NATIVE_ROOT>/audit/technical_validation/model_technical_validation.csv"
raster_out <- "<NON_NATIVE_ROOT>/audit/technical_validation/raster_validation.csv"

# ---- Part 1: Model evaluations ----
cat("\n=== Extracting model evaluations ===\n")
eval_results <- data.frame()

for (i in seq_len(nrow(lineage))) {
  sp <- lineage$species[i]
  rds_file <- file.path("results/individual_models", paste0(sp, "_individual.rds"))
  
  if (!file.exists(rds_file)) {
    cat(i, "/", nrow(lineage), sp, "- NO MODEL RDS\n")
    next
  }
  
  tryCatch({
    mod <- readRDS(rds_file)
    evals <- get_evaluations(mod)
    
    tss_vals <- evals %>% filter(metric.eval == "TSS") %>% pull(validation)
    roc_vals <- evals %>% filter(metric.eval == "ROC") %>% pull(validation)
    
    eval_results <- rbind(eval_results, data.frame(
      species = sp,
      n_occ_raw = "[UNCLEAR]",
      n_occ_clean = "[UNCLEAR]",
      n_pseudoabsence = ifelse("PA" %in% names(mod@calibration.lines), 
                                nrow(mod@calibration.lines$PA), "[UNCLEAR]"),
      algorithms = paste(unique(evals$algo), collapse = ";"),
      cv_scheme = mod@models.options@CV.strategy,
      auc_median = round(median(roc_vals, na.rm = TRUE), 4),
      tss_median = round(median(tss_vals, na.rm = TRUE), 4),
      ensemble_method = "[UNCLEAR]",
      passed_threshold_yes_no = ifelse(median(tss_vals, na.rm = TRUE) >= 0.8, "yes", "no"),
      raster_min = "[UNCLEAR]",
      raster_max = "[UNCLEAR]",
      raster_mean = "[UNCLEAR]",
      raster_na_fraction = "[UNCLEAR]",
      raster_extent_ok = "[UNCLEAR]",
      raster_resolution_ok = "[UNCLEAR]",
      raster_crs_ok = "[UNCLEAR]",
      validation_status = ifelse(median(tss_vals, na.rm = TRUE) >= 0.7, "pass", "marginal"),
      notes = "",
      stringsAsFactors = FALSE
    ))
    
    if (i %% 50 == 0) cat(i, "/", nrow(lineage), "processed\n")
  }, error = function(e) {
    cat(i, "/", nrow(lineage), sp, "- ERROR:", conditionMessage(e), "\n")
  })
}

write.csv(eval_results, eval_out, row.names = FALSE)
cat("Evaluations written to", eval_out, "-", nrow(eval_results), "species\n")

# ---- Part 2: Raster validation ----
cat("\n=== Extracting raster statistics ===\n")
raster_results <- data.frame()
ref_extent <- c(-180, 180, -90, 90)  # Expected global extent
ref_res <- c(0.04166667, 0.04166667)  # ~2.5 arc-minutes
ref_crs <- "EPSG:4326"

for (i in seq_len(min(20, nrow(lineage)))) {  # Sample first 20 for raster check
  sp <- lineage$species[i]
  # Check for projection raster  
  proj_rds <- character(0)
  if (file.exists(file.path("results/projections", paste0(sp, "_ensemble_projection.rds")))) {
    proj_rds <- file.path("results/projections", paste0(sp, "_ensemble_projection.rds"))
  } else if (file.exists(file.path("results/projections", paste0(sp, "_fallback_projection.rds")))) {
    proj_rds <- file.path("results/projections", paste0(sp, "_fallback_projection.rds"))
  }
  
  if (length(proj_rds) == 0) next
  
  tryCatch({
    proj <- readRDS(proj_rds)
    # Try to get raster from projection object
    if (inherits(proj, "BIOMOD.projection.out")) {
      # Get the projected raster
      ras <- tryCatch(get_predictions(proj), error = function(e) NULL)
    } else {
      ras <- NULL
    }
    
    if (!is.null(ras) && inherits(ras, "SpatRaster")) {
      rmin <- global(ras, "min", na.rm = TRUE)[1,1]
      rmax <- global(ras, "max", na.rm = TRUE)[1,1]
      rmean <- global(ras, "mean", na.rm = TRUE)[1,1]
      na_frac <- global(is.na(ras), "mean", na.rm = TRUE)[1,1]
      ext_ok <- all(abs(c(xmin(ras), xmax(ras), ymin(ras), ymax(ras)) - ref_extent) < 0.1)
      res_ok <- all(abs(c(xres(ras), yres(ras)) - ref_res) < 0.001)
      crs_ok <- grepl("4326", crs(ras))
    } else {
      rmin <- rmax <- rmean <- na_frac <- NA
      ext_ok <- res_ok <- crs_ok <- NA
    }
    
    raster_results <- rbind(raster_results, data.frame(
      species = sp,
      raster_min = rmin,
      raster_max = rmax,
      raster_mean = rmean,
      raster_na_fraction = na_frac,
      raster_extent_ok = ext_ok,
      raster_resolution_ok = res_ok,
      raster_crs_ok = crs_ok,
      stringsAsFactors = FALSE
    ))
    
    cat(i, sp, "- min:", rmin, "max:", rmax, "NA%:", round(na_frac*100,1), "\n")
  }, error = function(e) {
    cat(i, sp, "- RASTER ERROR:", conditionMessage(e), "\n")
  })
}

write.csv(raster_results, raster_out, row.names = FALSE)
cat("Raster stats written to", raster_out, "-", nrow(raster_results), "species\n")
cat("\n=== DONE ===\n")
