# Original source: run_negative_tss_reproject.R
# Original source location: ${WORK_ROOT}/archive/v1_deprecated/run_negative_tss_reproject.R
# Original source SHA-256: 7eb88206eeb2c11b1ec60a21c9c6606a7cab4589247e67d4ba67452fd9714cb2
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/usr/bin/env Rscript
# Targeted reprojection for 3 negative-TSS species using their best single algorithm
# These species currently use fallback with wrong algo (GLM) instead of best (RFd/MAXNET)

renv_lib <- "<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu"
.libPaths(c(renv_lib, .libPaths()))
library(biomod2)
library(terra)

base_dir <- "<WORK_ROOT>"
out_dir <- file.path(base_dir, "results", "negative_tss_reprocess_20260729")
dir.create(out_dir, showWarnings=FALSE, recursive=TRUE)

# Species to fix: current_projection → best_algo
fixes <- list(
  list(species="Adiantum latifolium", best_algo="RFd", current="GLM", 
       model_to_project="Adiantum.latifolium_PA1_allRun_RFd"),
  list(species="Diodia teres", best_algo="RFd", current="GLM",
       model_to_project="Diodia.teres_PA1_allRun_RFd"),
  list(species="Musa ornata", best_algo="MAXNET", current="GLM",
       model_to_project="Musa.ornata_PA1_allRun_MAXNET")
)

cat("=== Targeted reprojection for 3 negative-TSS species ===\n")
cat("These use fallback with wrong algorithm; switching to best single algorithm.\n\n")

for (fix in fixes) {
  sp <- fix$species
  sp_dot <- gsub(" ", ".", sp)
  best_algo <- fix$best_algo
  model_name <- fix$model_to_project
  
  cat(sprintf("--- %s: %s → %s ---\n", sp, fix$current, best_algo))
  
  # Load individual model
  mod_rds <- file.path(base_dir, "results", "individual_models", paste0(sp, "_individual.rds"))
  if (!file.exists(mod_rds)) {
    cat("  ERROR: Individual model not found\n")
    next
  }
  mod <- readRDS(mod_rds)
  
  # Check if the specific model exists
  if (!(model_name %in% mod@models.computed)) {
    # Try to find the best available model
    candidates <- grep(best_algo, mod@models.computed, value=TRUE)
    candidates <- grep("allRun", candidates, value=TRUE)  # prefer full-data models
    if (length(candidates) == 0) {
      candidates <- grep(best_algo, mod@models.computed, value=TRUE)
    }
    if (length(candidates) == 0) {
      cat(sprintf("  ERROR: No %s model found among computed models\n", best_algo))
      next
    }
    model_name <- candidates[1]
    cat(sprintf("  Using model: %s\n", model_name))
  } else {
    cat(sprintf("  Model confirmed: %s\n", model_name))
  }
  
  # Load environmental stack
  env_rds <- file.path(base_dir, "data", "environmental", paste0(sp, "_env_stack_final.rds"))
  if (!file.exists(env_rds)) {
    cat("  ERROR: Environmental stack not found\n")
    next
  }
  env_stack <- readRDS(env_rds)
  
  # Run BIOMOD_Projection with the single best model
  cat("  Running BIOMOD_Projection...\n")
  proj_name <- paste0("proj_corrected_", sp_dot)
  
  proj <- tryCatch(
    BIOMOD_Projection(
      bm.mod = mod,
      new.env = env_stack,
      proj.name = proj_name,
      models.chosen = model_name,
      binary.meth = NULL,
      filtered.meth = NULL,
      build.clamping.mask = FALSE,
      do.stack = FALSE
    ),
    error = function(e) {
      cat(sprintf("  BIOMOD_Projection ERROR: %s\n", conditionMessage(e)))
      return(NULL)
    }
  )
  
  if (is.null(proj)) next
  
  # Save corrected projection RDS
  out_rds <- file.path(out_dir, paste0(sp, "_corrected_", best_algo, "_projection.rds"))
  saveRDS(proj, out_rds)
  cat(sprintf("  Corrected projection saved: %s\n", out_rds))
  
  # Also generate and save the raster
  cat("  Extracting prediction raster...\n")
  sp_dir <- file.path(base_dir, "models", sp_dot)
  rasters <- list.files(sp_dir, pattern=paste0(proj_name, ".*\\.(tif|grd)$"), recursive=TRUE, full.names=TRUE, ignore.case=TRUE)
  suit <- rasters[!grepl("(TSSbin|TSSfilt|ClampingMask)", rasters, ignore.case=TRUE)]
  
  if (length(suit) > 0) {
    r <- rast(suit[1])
    mm <- minmax(r)
    gm <- global(r, "mean", na.rm=TRUE)
    nz <- global(r > 0, "mean", na.rm=TRUE)
    cat(sprintf("  Raster: min=%.1f, max=%.1f, mean=%.1f, nonzero=%.1f%%\n", mm[1], mm[2], gm[1,1], nz[1,1]*100))
  }
  
  cat(sprintf("  ✅ DONE: %s\n\n", sp))
}

cat("=== All reprojections complete ===\n")
