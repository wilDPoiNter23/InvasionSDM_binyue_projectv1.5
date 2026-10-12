# Original source: proper_ensemble_project.r
# Original source location: ${WORK_ROOT}/archive/v1_deprecated/proper_ensemble_project.r
# Original source SHA-256: ca82929ad3e50fa5dc75553faf8068f40177365f8e0636cdab16c208e592420f
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# proper_ensemble_project.r
# Mirror the batch script's ensemble logic EXACTLY
# Usage: Rscript proper_ensemble_project.r "Species name"

args <- commandArgs(trailingOnly = TRUE)
species_name <- args[1]

renv_lib <- "<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu"
.libPaths(c(renv_lib, .libPaths()))
suppressMessages(library(biomod2))
suppressMessages(library(terra))

# Limit terra memory to avoid overflow
terra::terraOptions(memfrac = 0.3)

# Force terra to use disk cache to avoid memory overflow
terra::terraOptions(memfrac = 0.3)
cat(sprintf("terra: todisk=%s, memfrac=%s\n", terra::terraOptions()$todisk, terra::terraOptions()$memfrac))
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))

cat(sprintf("\n========== %s ==========\n", species_name))

base_dir <- "<WORK_ROOT>"
setwd(file.path(base_dir, "models"))

# ── Load existing artifacts ──
ind_path <- file.path(base_dir, "results/individual_models", paste0(species_name, "_individual.rds"))
individual_models <- readRDS(ind_path)
cat(sprintf("Individual models: %d\n", length(get_built_models(individual_models))))

env_path <- file.path(base_dir, "data/environmental", paste0(species_name, "_env_stack_final.rds"))
env_stack_final <- readRDS(env_path)
cat(sprintf("Env layers: %d\n", nlyr(env_stack_final)))

# ── Step 1: Model selection (exactly as batch script) ──
individual_evaluation <- get_evaluations(individual_models)

tss_roc_df <- individual_evaluation %>%
  filter(metric.eval %in% c("TSS", "ROC")) %>%
  select(full.name, algo, PA, run, metric.eval, validation)

tss_roc_summary <- tss_roc_df %>%
  pivot_wider(names_from = metric.eval, values_from = validation) %>%
  arrange(desc(TSS))

good_models <- tss_roc_summary %>% filter(TSS > 0.8, ROC > 0.8)

if (nrow(good_models) == 0) {
  cat("No models TSS>0.8 & ROC>0.8, trying TSS>0.7...\n")
  good_models <- tss_roc_summary %>% filter(TSS > 0.7, ROC > 0.7)
}

if (nrow(good_models) == 0) {
  cat("No models TSS>0.7 either — using top model by TSS\n")
  best_row <- tss_roc_summary %>% slice(1)
  good_models <- best_row
}

selected_models <- good_models$full.name
cat(sprintf("Selected models: %d\n", length(selected_models)))
for (m in selected_models) cat(sprintf("  %s\n", m))

# ── Top-3 to avoid memory overflow ──
if (length(selected_models) > 3) {
  selected_models <- head(selected_models, 3)
  cat(sprintf("Trimmed to top-3: %s\n", paste(selected_models, collapse=", ")))
}

# Safety: if still empty, take absolute best by TSS
if (length(selected_models) == 0) {
  best_row <- tss_roc_summary %>% slice(1)
  selected_models <- best_row$full.name
  cat(sprintf("Fallback to best individual: %s\n", selected_models))
}

# ── Step 2: Ensemble (exactly as batch script) ──
if (is.null(selected_models) || length(selected_models) < 2) {
  best_ensemble_model <- selected_models
  cat(sprintf("Fallback: single model %s\n", best_ensemble_model))
  use_ensemble <- FALSE
} else {
  cat(sprintf("Building ensemble with %d models...\n", length(selected_models)))
  
  ensemble_models <- tryCatch({
    BIOMOD_EnsembleModeling(
      bm.mod = individual_models,
      models.chosen = selected_models,
      em.by = 'all',
      em.algo = c('EMmean', 'EMmedian'),
      metric.select = c('TSS'),
      metric.select.thresh = c(0.8),
      metric.eval = c('TSS', 'ROC', 'KAPPA'),
      var.import = 3,
      EMci.alpha = 0.05,
      EMwmean.decay = 'proportional'
    )
  }, error = function(e) {
    cat(sprintf("ERROR: %s\n", e$message))
    return(NULL)
  })
  
  if (is.null(ensemble_models)) {
    cat("Ensemble failed → fallback to best individual\n")
    best_ensemble_model <- selected_models[1]
    use_ensemble <- FALSE
  } else {
    saveRDS(ensemble_models, file.path(base_dir, "results/ensemble_models", paste0(species_name, "_ensemble.rds")))
    saveRDS(selected_models, file.path(base_dir, "results/ensemble_models", paste0(species_name, "_best_ensemble.rds")))
    use_ensemble <- TRUE
    cat(sprintf("Ensemble saved: %d models\n", length(get_built_models(ensemble_models))))
  }
}

# ── Step 3: Project (exactly as batch script) ──
if (use_ensemble) {
  cat("Running BIOMOD_EnsembleForecasting...\n")
  # Get available ensemble model names
  em_models <- get_built_models(ensemble_models)
  cat(sprintf("Available ensemble models: %s\n", paste(em_models, collapse=", ")))
  # Try EMmeanByTSS first, fallback to EMmean
  if ("EMmeanByTSS" %in% em_models) {
    chosen <- "EMmeanByTSS"
  } else if ("EMmean" %in% em_models) {
    chosen <- "EMmean"
  } else {
    chosen <- em_models[1]
  }
  cat(sprintf("Using model: %s\n", chosen))
  
  my_proj <- tryCatch({
    BIOMOD_EnsembleForecasting(
      bm.em = ensemble_models,
      proj.name = 'ensemble_projection',
      new.env = env_stack_final,
      models.chosen = chosen,
      metric.binary = 'TSS',
      build.clamping.mask = TRUE,
      compress = TRUE,
      nb.cpu = 2
    )
  }, error = function(e) {
    cat(sprintf("ERROR: %s\n", e$message))
    return(NULL)
  })
  
  # If ensemble forecasting failed (e.g. memory), fallback to single model
  if (is.null(my_proj)) {
    cat("Ensemble forecasting failed → retrying with single best model...\n")
    best_ensemble_model <- selected_models[1]
    my_proj <- tryCatch({
      BIOMOD_Projection(
        bm.mod = individual_models,
        proj.name = 'fallback_projection',
        new.env = env_stack_final,
        models.chosen = best_ensemble_model,
        metric.binary = 'TSS',
        build.clamping.mask = TRUE,
        compress = TRUE,
        nb.cpu = 1
      )
    }, error = function(e2) {
      cat(sprintf("ERROR: %s\n", e2$message))
      return(NULL)
    })
    proj_type <- "fallback"
  } else {
    proj_type <- "ensemble"
  }
} else {
  cat("Running BIOMOD_Projection (fallback)...\n")
  my_proj <- tryCatch({
    BIOMOD_Projection(
      bm.mod = individual_models,
      proj.name = 'fallback_projection',
      new.env = env_stack_final,
      models.chosen = best_ensemble_model,
      metric.binary = 'TSS',
      build.clamping.mask = TRUE,
      compress = TRUE,
      nb.cpu = 2
    )
  }, error = function(e) {
    cat(sprintf("ERROR: %s\n", e$message))
    return(NULL)
  })
  proj_type <- "fallback"
}

if (is.null(my_proj)) stop("Projection failed")

# ── Save ──
rds_out <- file.path(base_dir, "results/projections", paste0(species_name, "_", proj_type, "_projection.rds"))
saveRDS(my_proj, file = rds_out)
cat(sprintf("Saved: %s\n", rds_out))

# ── Verify ──
pred <- tryCatch(get_predictions(my_proj, as.raster = TRUE), error = function(e) NULL)
if (!is.null(pred)) {
  mm <- terra::minmax(pred)
  nz <- terra::global(pred > 0, "sum", na.rm = TRUE)[1, 1]
  nc <- terra::ncell(pred)
  cat(sprintf("VERIFY: dim=%dx%d range=[%.4f,%.4f] nonzero=%.2f%%\n",
    dim(pred)[1], dim(pred)[2], mm[1], mm[2], nz / nc * 100))
}

cat(sprintf("Done: %s\n", Sys.time()))
