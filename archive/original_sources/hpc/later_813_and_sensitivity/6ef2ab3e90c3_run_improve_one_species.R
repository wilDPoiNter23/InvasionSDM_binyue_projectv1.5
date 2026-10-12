# Original source: run_improve_one_species.R
# Original source location: ${WORK_ROOT}/archive/v1_deprecated/run_improve_one_species.R
# Original source SHA-256: 6ef2ab3e90c31745cddbe3270f08cbead7a7bf9f29951eaa56ca1f85e8e2ef40
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/usr/bin/env Rscript
# Targeted model improvement: reproject species using best existing algorithm
# Fix: use full_stack.tif + setwd() to model directory
# Usage: Rscript this_script.R "Species name"

args <- commandArgs(trailingOnly=TRUE)
if (length(args) < 1) { cat("Usage: Rscript run_improve_one_species.R 'Species name'\n"); quit(status=1) }
sp <- args[1]

renv_lib <- "<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu"
.libPaths(c(renv_lib, .libPaths()))
library(biomod2)
library(terra)
library(dplyr)

base_dir <- "<WORK_ROOT>"
out_root <- file.path(base_dir, "results", "targeted_model_improvement_20260729")
dir.create(out_root, showWarnings=FALSE, recursive=TRUE)

sp_dot <- gsub(" ", ".", sp)

cat(sprintf("=== Improving: %s ===\n", sp))

# 1. Load individual model
mod_rds <- file.path(base_dir, "results", "individual_models", paste0(sp, "_individual.rds"))
if (!file.exists(mod_rds)) { cat("ERROR: No individual model\n"); quit(status=1) }
mod <- readRDS(mod_rds)

# 2. Find best algorithm by AUC
evals <- get_evaluations(mod)
best_algo <- ""; best_auc <- -1
for (algo in unique(evals$algo)) {
  auc_m <- median(evals %>% filter(algo==!!algo, metric.eval=="ROC") %>% pull(validation), na.rm=TRUE)
  if (!is.na(auc_m) && auc_m > best_auc) { best_auc <- auc_m; best_algo <- algo }
}
cat(sprintf("Best algo: %s (AUC=%.4f)\n", best_algo, best_auc))

# 3. Find best model name for that algo (prefer allRun)
candidates <- grep(best_algo, mod@models.computed, value=TRUE)
allRun_candidates <- grep("allRun", candidates, value=TRUE)
if (length(allRun_candidates) > 0) {
  model_name <- allRun_candidates[1]
} else if (length(candidates) > 0) {
  model_name <- candidates[1]
} else {
  cat("ERROR: No model found for best algorithm\n"); quit(status=1)
}
cat(sprintf("Model: %s\n", model_name))

# 4. Load environmental stack
full_stack <- rast("<HPC_INPUT_ROOT>/climate/full_stack.tif")
env <- full_stack[[mod@expl.var.names]]
cat(sprintf("Env: %d layers, %dx%d\n", nlyr(env), nrow(env), ncol(env)))

# 5. Run BIOMOD_Projection from models/ directory (parent of species dirs)
orig_wd <- getwd()
setwd(file.path(base_dir, "models"))
cat(sprintf("Working dir: %s\n", getwd()))

proj_name <- paste0("proj_improved_", best_algo)
proj <- tryCatch(
  BIOMOD_Projection(
    bm.mod = mod, new.env = env,
    proj.name = proj_name,
    models.chosen = model_name,
    binary.meth = NULL, filtered.meth = NULL,
    build.clamping.mask = FALSE
  ),
  error = function(e) {
    cat(sprintf("BIOMOD_Projection ERROR: %s\n", conditionMessage(e)))
    return(NULL)
  }
)
setwd(orig_wd)

if (is.null(proj)) { cat("FAILED\n"); quit(status=1) }
cat("Projection created successfully\n")

# 6. Save projection RDS to output directory
out_rds <- file.path(out_root, paste0(sp, "_improved_", best_algo, "_projection.rds"))
saveRDS(proj, out_rds)
cat(sprintf("Saved: %s\n", out_rds))

# 7. Extract and validate raster
proj_dir <- file.path(sp_dot, proj_name)
rasters <- list.files(proj_dir, pattern="\\.(tif|grd)$", recursive=TRUE, full.names=TRUE, ignore.case=TRUE)
suit <- rasters[!grepl("(TSSbin|TSSfilt|ClampingMask|\\.aux)", rasters, ignore.case=TRUE)]

if (length(suit) > 0) {
  r <- rast(suit[1])
  mm <- minmax(r); gm <- global(r, "mean", na.rm=TRUE); nz <- global(r > 0, "mean", na.rm=TRUE)
  cat(sprintf("Raster: min=%.1f max=%.1f mean=%.1f nonzero=%.1f%%\n", mm[1], mm[2], gm[1,1], nz[1,1]*100))
  cat(sprintf("Dim: %s, CRS: %s\n", paste(dim(r),collapse="x"), crs(r)))
}

cat(sprintf("✅ DONE: %s\n", sp))
