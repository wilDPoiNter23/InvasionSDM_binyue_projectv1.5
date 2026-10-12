# Original source: reproject_single_species.r
# Original source location: ${WORK_ROOT}/archive/v1_deprecated/reproject_single_species.r
# Original source SHA-256: ceb2ca44d1caf2aaa482c92a586604411a8832538058cff58a111231e22cf05b
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# reproject_single_species.r
# Re-run BIOMOD projection for a single species, using its existing model objects
# Key change: build.clamping.mask = FALSE to avoid all-zero rasters
# Usage: Rscript reproject_single_species.r "Vulpia fasciculata"

args <- commandArgs(trailingOnly = TRUE)
species_name <- args[1]

# Use renv library for all R packages
renv_lib <- "<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu"
.libPaths(c(renv_lib, .libPaths()))

cat(sprintf("=== Re-projecting: %s ===\n", species_name))
cat(sprintf("R lib paths: %s\n", paste(.libPaths(), collapse=", ")))

suppressMessages(library(biomod2))
suppressMessages(library(terra))

base_dir <- "<WORK_ROOT>"
clean_name <- gsub("[^A-Za-z0-9_]", "_", species_name)
dot_name <- gsub(" ", ".", species_name)

# ── 1. Load env_stack_final ──
env_final_path <- file.path(base_dir, "data/environmental",
    paste0(species_name, "_env_stack_final.rds"))

if (!file.exists(env_final_path)) {
    cat(sprintf("env_stack_final not found, using full stack: %s\n", env_final_path))
    env_stack_final <- terra::rast("<HPC_INPUT_ROOT>/climate/full_stack.tif")
} else {
    env_stack_final <- readRDS(env_final_path)
}
cat(sprintf("Loaded env_stack: %d layers\n", nlyr(env_stack_final)))

# ── 2. Load individual models ──
ind_path <- file.path(base_dir, "results/individual_models",
    paste0(species_name, "_individual.rds"))
if (!file.exists(ind_path)) stop("individual_models not found: ", ind_path)
individual_models <- readRDS(ind_path)
cat(sprintf("Loaded individual_models: %d models\n",
    length(get_built_models(individual_models))))

# ── 3. Determine best model ──
ens_path <- file.path(base_dir, "results/ensemble_models",
    paste0(species_name, "_ensemble.rds"))
best_path <- file.path(base_dir, "results/ensemble_models",
    paste0(species_name, "_best_ensemble.rds"))

has_ensemble <- file.exists(ens_path) && file.exists(best_path)

if (has_ensemble) {
    best_ensemble_model <- readRDS(best_path)
    if (length(best_ensemble_model) > 1) {
        proj_type <- "ensemble"
        proj_obj <- readRDS(ens_path)
    } else {
        proj_type <- "fallback"
        proj_obj <- individual_models
    }
} else {
    proj_type <- "fallback"
    proj_obj <- individual_models
    ind_eval <- get_evaluations(individual_models)
    tss_scores <- ind_eval["TSS", "Testing.data", ]
    if (!all(is.na(tss_scores))) {
        best_ensemble_model <- names(which.max(tss_scores))
    } else {
        best_ensemble_model <- get_built_models(individual_models)[1]
    }
}
cat(sprintf("Type: %s, Model: %s\n", proj_type, paste(best_ensemble_model, collapse=", ")))

# ── 4. Run projection (NO clamping mask) ──
setwd(file.path(base_dir, "models"))

my_proj <- tryCatch({
    BIOMOD_Projection(
        bm.mod         = proj_obj,
        proj.name      = paste0(proj_type, "_projection"),
        new.env        = env_stack_final,
        models.chosen  = best_ensemble_model,
        metric.binary  = "TSS",
        build.clamping.mask = FALSE,
        compress       = TRUE,
        nb.cpu         = 4
    )
}, error = function(e) {
    cat(sprintf("ERROR: %s\n", e$message))
    return(NULL)
})

if (is.null(my_proj)) stop("Projection failed")

# ── 5. Save projection RDS ──
rds_out <- file.path(base_dir, "results/projections",
    paste0(species_name, "_", proj_type, "_projection.rds"))
saveRDS(my_proj, file = rds_out)
cat(sprintf("Saved: %s\n", rds_out))

# ── 6. Verify ──
pred <- tryCatch(get_predictions(my_proj, as.raster = TRUE), error = function(e) NULL)
if (!is.null(pred)) {
    mm <- terra::minmax(pred)
    nz <- terra::global(pred > 0, "sum", na.rm = TRUE)[1, 1]
    nc <- terra::ncell(pred)
    cat(sprintf("VERIFY: dim=%dx%d range=[%.4f,%.4f] nonzero=%.2f%%\n",
        dim(pred)[1], dim(pred)[2], mm[1], mm[2], nz / nc * 100))
} else {
    cat("WARNING: cannot verify projection output\n")
}

cat(sprintf("Done: %s\n", Sys.time()))
