# Original source: slurm_spartina_noID.sh
# Original source location: ${WORK_ROOT}/publication_release_v1/scripts/slurm_spartina_noID.sh
# Original source SHA-256: 0d3382151d062c8bffd50271a97987a21dee360c90ad451ffc08fd1b751dcc0c
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/bin/bash
#SBATCH --job-name=sp_noID
#SBATCH --partition=thcp1
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8
#SBATCH --mem=32G
#SBATCH --time=04:00:00
#SBATCH --output=<WORK_ROOT>/logs/spartina_noID_%j.out
#SBATCH --error=<WORK_ROOT>/logs/spartina_noID_%j.err

source ~/.bashrc
module add loginnode
export R_LIBS_USER="<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu"

cd <WORK_ROOT>

Rscript -e '
.libPaths(c("<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu", .libPaths()))
library(biomod2)
library(terra)

setwd("<WORK_ROOT>")

cat("=== Spartina NO-ID Sensitivity Test ===\n")
cat("Start:", Sys.time(), "\n")

# ---- 1. Load occurrence data (TSV, absolute path) ----
occ <- read.delim("<HPC_INPUT_ROOT>/occ_patch/Spartina.alterniflora.csv", stringsAsFactors=FALSE)
cat("Occurrence points:", nrow(occ), "\n")

# ---- 2. Load predictor stack (35 scientific layers, NO ID) ----
stack_35 <- rast("<WORK_ROOT>/data/predictors/full_stack_terrestrial_plus_VIIRS_v1.tif")
cat("Stack layers:", nlyr(stack_35), "\n")

# ---- 3. Prepare response and coordinates ----
myRespXY <- occ[, c("decimalLongitude", "decimalLatitude")]
myResp <- rep(1, nrow(myRespXY))
# ---- 4. Format BIOMOD data (pass raster directly for PA sampling) ----
bm_data <- BIOMOD_FormatingData(
  resp.var = myResp,
  expl.var = stack_35,
  resp.xy = myRespXY,
  resp.name = "Spartina.alterniflora",
  PA.nb.rep = 4,
  PA.nb.absences = 10000,
  PA.strategy = "random"
)
cat("Formatted data: done\n")

# ---- 5. Model options (same as original) ----
bm_opt <- bm_ModelingOptions(
  GLM = list(type = "quadratic", interaction.level = 0, test = "AIC"),
  RF = list(ntree = 500, mtry = 1),
  MAXNET = list(regmult = 1)
)

# ---- 6. Run BIOMOD_Modeling (4 full-model runs, same as original) ----
bm_out <- BIOMOD_Modeling(
  bm.format = bm_data,
  modeling.id = "Spartina_noID_sensitivity",
  models = c("GLM", "RF", "MAXNET"),
  bm.options = bm_opt,
  CV.strategy = "kfold",
  CV.k = 4,
  CV.nb.rep = 1,
  metric.eval = c("TSS", "ROC"),
  var.import = 3,
  nb.cpu = 8,
  seed.val = 42,
  do.full.models = TRUE,
  NbRunEval = 4
)
)

# ---- 7. Get evaluations ----
ev <- get_evaluations(bm_out)
cat("Evaluations extracted: nrow=", nrow(ev), "\n")

# Use validation column (CV held-out) if available, otherwise evaluation
val_col <- if ("validation" %in% colnames(ev) && any(!is.na(ev$validation))) "validation" else "evaluation"
cat("Using validation column:", val_col, "\n")

tss_vals <- ev[[val_col]][ev$metric.eval == "TSS" & !is.na(ev[[val_col]])]
roc_vals <- ev[[val_col]][ev$metric.eval == "ROC" & !is.na(ev[[val_col]])]

# Save model
dir.create("<WORK_ROOT>/results/Spartina_noID_sensitivity_v1", showWarnings=FALSE, recursive=TRUE)
saveRDS(bm_out, "<WORK_ROOT>/results/Spartina_noID_sensitivity_v1/Spartina_alterniflora_noID_models.rds")

# ---- 8. Summary ----
cat(sprintf("No-ID TSS: n=%d mean=%.4f median=%.4f min=%.4f max=%.4f\n",
    length(tss_vals), mean(tss_vals), median(tss_vals), min(tss_vals), max(tss_vals)))
cat(sprintf("No-ID ROC: n=%d mean=%.4f median=%.4f min=%.4f max=%.4f\n",
    length(roc_vals), mean(roc_vals), median(roc_vals), min(roc_vals), max(roc_vals)))

cat("Done:", Sys.time(), "\n")
' 2>&1

echo "Job exit: $?"
date
