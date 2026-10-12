# Original source: manual_ensemble_project.r
# Original source location: ${WORK_ROOT}/archive/v1_deprecated/manual_ensemble_project.r
# Original source SHA-256: 062cd8769e9b75edade479c965fa1275e5afee5156c19e5397deb92567fbe945
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# manual_ensemble_project.r
# Bypass BIOMOD_EnsembleModeling bug: project each algo individually, then average
# Usage: Rscript manual_ensemble_project.r "Vulpia fasciculata"

args <- commandArgs(trailingOnly = TRUE)
species_name <- args[1]

renv_lib <- "<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu"
.libPaths(c(renv_lib, .libPaths()))
suppressMessages(library(biomod2))
suppressMessages(library(terra))

cat(sprintf("\n========== %s (manual ensemble) ==========\n", species_name))

base_dir <- "<WORK_ROOT>"
dot_name <- gsub(" ", ".", species_name)

# Load individual models
ind_path <- file.path(base_dir, "results/individual_models", paste0(species_name, "_individual.rds"))
individual_models <- readRDS(ind_path)

# Load env stack
env_path <- file.path(base_dir, "data/environmental", paste0(species_name, "_env_stack_final.rds"))
env_stack_final <- readRDS(env_path)
cat(sprintf("Env: %d layers\n", nlyr(env_stack_final)))

# Get full model names for GLM, RFd, MAXNET
all_models <- get_built_models(individual_models)
glm_models <- grep("_GLM$", all_models, value = TRUE)
rfd_models <- grep("_RFd$", all_models, value = TRUE)
maxnet_models <- grep("_MAXNET$", all_models, value = TRUE)

# Use only "allData_allRun" full models 
glm_full <- grep("allData_allRun", glm_models, value = TRUE)
rfd_full <- grep("allData_allRun", rfd_models, value = TRUE)
maxnet_full <- grep("allData_allRun", maxnet_models, value = TRUE)

# If no allData_allRun, use PA1_allRun
if (length(glm_full) == 0) glm_full <- grep("PA1_allRun", glm_models, value = TRUE)
if (length(rfd_full) == 0) rfd_full <- grep("PA1_allRun", rfd_models, value = TRUE)
if (length(maxnet_full) == 0) maxnet_full <- grep("PA1_allRun", maxnet_models, value = TRUE)

cat(sprintf("GLM full: %s\n", paste(glm_full, collapse=", ")))
cat(sprintf("RFd full: %s\n", paste(rfd_full, collapse=", ")))
cat(sprintf("MAXNET full: %s\n", paste(maxnet_full, collapse=", ")))

setwd(file.path(base_dir, "models"))

# Project each algorithm
rasters <- list()
for (algo_name in c("GLM", "RFd", "MAXNET")) {
    models <- get(paste0(tolower(algo_name), "_full"))
    if (length(models) == 0) next
    
    cat(sprintf("\nProjecting %s: %s...\n", algo_name, models[1]))
    
    proj <- tryCatch({
        BIOMOD_Projection(
            bm.mod = individual_models,
            proj.name = paste0("manual_", tolower(algo_name)),
            new.env = env_stack_final,
            models.chosen = models[1],
            metric.binary = "TSS",
            build.clamping.mask = FALSE,
            compress = FALSE,
            nb.cpu = 8
        )
    }, error = function(e) {
        cat(sprintf("  ERROR: %s\n", e$message))
        return(NULL)
    })
    
    if (!is.null(proj)) {
        pred <- tryCatch(get_predictions(proj, as.raster = TRUE), error = function(e) NULL)
        if (!is.null(pred)) {
            # Normalize to 0-1
            mm <- terra::minmax(pred)
            if (mm[2] > mm[1]) {
                pred <- (pred - mm[1]) / (mm[2] - mm[1])
            }
            rasters[[algo_name]] <- pred
            cat(sprintf("  OK: range=[%.4f,%.4f]\n", mm[1], mm[2]))
        }
    }
    gc()
}

cat(sprintf("\nRasters collected: %d\n", length(rasters)))

if (length(rasters) == 0) stop("No successful projections")

# Manual ensemble: mean of all algorithm projections
if (length(rasters) == 1) {
    ensemble_raster <- rasters[[1]]
} else {
    ensemble_raster <- mean(rast(rasters), na.rm = TRUE)
}

cat(sprintf("Ensemble raster created\n"))

# Save as GeoTIFF
tif_out <- file.path(base_dir, "results/projections", 
    paste0(species_name, "_ensemble_manual.tif"))
terra::writeRaster(ensemble_raster, tif_out, overwrite = TRUE)
cat(sprintf("Saved: %s\n", tif_out))

# Also save to the standard RDS location
rds_out <- file.path(base_dir, "results/projections", 
    paste0(species_name, "_ensemble_projection.rds"))
# We can't save as BIOMOD.projection.out, but we can save the raster
saveRDS(ensemble_raster, file = rds_out)
cat(sprintf("Saved RDS: %s\n", rds_out))

# Verify
mm <- terra::minmax(ensemble_raster)
nz <- terra::global(ensemble_raster > 0, "sum", na.rm = TRUE)[1, 1]
nc <- terra::ncell(ensemble_raster)
cat(sprintf("VERIFY: dim=%dx%d range=[%.4f,%.4f] nonzero=%.2f%%\n",
    dim(ensemble_raster)[1], dim(ensemble_raster)[2], mm[1], mm[2], nz / nc * 100))

cat(sprintf("Done: %s\n", Sys.time()))
