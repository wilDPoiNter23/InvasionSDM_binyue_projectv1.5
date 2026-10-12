# Original source: rebuild_ensemble_and_project.r
# Original source location: ${WORK_ROOT}/archive/v1_deprecated/rebuild_ensemble_and_project.r
# Original source SHA-256: 0c2e6aae9735f2dc57b59a7f1d4e52a431991abf01c7853a55de50a6ee8c05d0
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# rebuild_ensemble_and_project.r
# For a given species: rebuild ensemble using ALL good models, then do ensemble projection
# Usage: Rscript rebuild_ensemble_and_project.r "Vulpia fasciculata"

args <- commandArgs(trailingOnly = TRUE)
species_name <- args[1]

renv_lib <- "<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu"
.libPaths(c(renv_lib, .libPaths()))
suppressMessages(library(biomod2))
suppressMessages(library(terra))
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))

cat(sprintf("=== %s: Rebuilding ensemble ===\n", species_name))

base_dir <- "<WORK_ROOT>"

# 1. Load individual models
ind_path <- file.path(base_dir, "results/individual_models", paste0(species_name, "_individual.rds"))
individual_models <- readRDS(ind_path)
cat(sprintf("Individual models: %d\n", length(get_built_models(individual_models))))

# 2. Get evaluation, find ALL models with TSS>0.8 & ROC>0.8
ind_eval <- get_evaluations(individual_models)

tss_roc_df <- ind_eval %>%
  filter(metric.eval %in% c("TSS", "ROC")) %>%
  select(full.name, algo, PA, run, metric.eval, validation)

# Pivot to get TSS and ROC side by side (one row per model)
tss_roc_wide <- tss_roc_df %>%
  pivot_wider(names_from = metric.eval, values_from = validation) %>%
  filter(!is.na(TSS), !is.na(ROC))

good_models <- tss_roc_wide %>% filter(TSS > 0.8, ROC > 0.8)

if (nrow(good_models) == 0) {
    cat("No models with TSS>0.8 & ROC>0.8, trying TSS>0.7...\n")
    good_models <- tss_roc_wide %>% filter(TSS > 0.7, ROC > 0.7)
}
if (nrow(good_models) == 0) {
    cat("Still no models, using single best by TSS...\n")
    best_row <- tss_roc_wide %>% arrange(desc(TSS)) %>% slice(1)
    good_models <- best_row
}
cat(sprintf("Models with TSS>0.8 & ROC>0.8: %d\n", nrow(good_models)))
print(good_models %>% select(full.name, TSS, ROC))

selected_models <- unique(good_models$full.name)
cat(sprintf("Unique selected models: %d\n", length(selected_models)))
cat(paste(selected_models, collapse="\n  "), "\n")

# 3. Rebuild ensemble with ALL good models
if (length(selected_models) < 2) {
    cat("WARNING: Only 1 model passes threshold — cannot build ensemble\n")
    cat("Using best individual model for projection instead\n")
    best_model <- selected_models[1]
    proj_obj <- individual_models
    proj_type <- "fallback"
    use_ensemble <- FALSE
} else {
    cat(sprintf("Building ensemble with %d models...\n", length(selected_models)))
    
    ensemble_models <- tryCatch({
        BIOMOD_EnsembleModeling(
            bm.mod = individual_models,
            models.chosen = selected_models,
            em.by = "all",
            em.algo = c("EMmean", "EMmedian"),
            metric.select = c("TSS"),
            metric.select.thresh = c(0.8),
            metric.eval = c("TSS", "ROC", "KAPPA"),
            var.import = 3,
            EMci.alpha = 0.05,
            EMwmean.decay = "proportional"
        )
    }, error = function(e) {
        cat(sprintf("ERROR building ensemble: %s\n", e$message))
        return(NULL)
    })
    
    if (is.null(ensemble_models)) {
        cat("Ensemble build failed, falling back to best individual\n")
        best_model <- selected_models[1]
        proj_obj <- individual_models
        proj_type <- "fallback"
        use_ensemble <- FALSE
    } else {
        # Save new ensemble
        saveRDS(ensemble_models, file.path(base_dir, "results/ensemble_models", 
            paste0(species_name, "_ensemble.rds")))
        saveRDS(selected_models, file.path(base_dir, "results/ensemble_models", 
            paste0(species_name, "_best_ensemble.rds")))
        
        proj_obj <- ensemble_models
        proj_type <- "ensemble"
        use_ensemble <- TRUE
        cat("Ensemble built successfully\n")
    }
}

# 4. Load env_stack
env_path <- file.path(base_dir, "data/environmental", paste0(species_name, "_env_stack_final.rds"))
env_stack_final <- readRDS(env_path)
cat(sprintf("Env stack: %d layers\n", nlyr(env_stack_final)))

# 5. Project
setwd(file.path(base_dir, "models"))

if (use_ensemble) {
    # Ensemble forecasting
    my_proj <- tryCatch({
        BIOMOD_EnsembleForecasting(
            bm.em = proj_obj,
            proj.name = "ensemble_projection",
            new.env = env_stack_final,
            models.chosen = "EMmeanByTSS",
            metric.binary = "TSS",
            build.clamping.mask = FALSE,
            compress = TRUE,
            nb.cpu = 4
        )
    }, error = function(e) {
        cat(sprintf("ERROR in ensemble forecasting: %s\n", e$message))
        return(NULL)
    })
} else {
    # Individual projection
    my_proj <- tryCatch({
        BIOMOD_Projection(
            bm.mod = proj_obj,
            proj.name = "fallback_projection",
            new.env = env_stack_final,
            models.chosen = best_model,
            metric.binary = "TSS",
            build.clamping.mask = FALSE,
            compress = TRUE,
            nb.cpu = 4
        )
    }, error = function(e) {
        cat(sprintf("ERROR in projection: %s\n", e$message))
        return(NULL)
    })
}

if (is.null(my_proj)) stop("Projection failed")

# 6. Save
rds_out <- file.path(base_dir, "results/projections", 
    paste0(species_name, "_", proj_type, "_projection.rds"))
saveRDS(my_proj, file = rds_out)
cat(sprintf("Saved: %s\n", rds_out))

# 7. Verify
pred <- tryCatch(get_predictions(my_proj, as.raster = TRUE), error = function(e) NULL)
if (!is.null(pred)) {
    mm <- terra::minmax(pred)
    nz <- terra::global(pred > 0, "sum", na.rm = TRUE)[1, 1]
    nc <- terra::ncell(pred)
    cat(sprintf("VERIFY: dim=%dx%d range=[%.4f,%.4f] nonzero=%.2f%%\n",
        dim(pred)[1], dim(pred)[2], mm[1], mm[2], nz / nc * 100))
}

cat(sprintf("Done: %s\n", Sys.time()))
