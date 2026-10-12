# Original source: slurm_musa_rfd.sh
# Original source location: ${WORK_ROOT}/archive/slurm_old/slurm_musa_rfd.sh
# Original source SHA-256: a1c639151e0f2b89966c90fe2612c15edcc9dde1d0cb08c97ff41cae66e66b89
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/bin/bash
#SBATCH --job-name=musa_rfd
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=2
#SBATCH --mem=32G
#SBATCH --time=0:30:00
#SBATCH --partition=thcp1
#SBATCH --output=logs/musa_rfd_%j.out
source ~/.bashrc
module add loginnode
cd <WORK_ROOT>
# Force RFd for Musa by setting best_algo manually
Rscript -e '
renv_lib <- "<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu"
.libPaths(c(renv_lib, .libPaths()))
library(biomod2); library(terra)
sp <- "Musa ornata"; sp_dot <- "Musa.ornata"
mod <- readRDS(paste0("results/individual_models/",sp,"_individual.rds"))
env <- rast("<HPC_INPUT_ROOT>/climate/full_stack.tif")[[mod@expl.var.names]]
orig_wd <- getwd(); setwd("models")
model_name <- "Musa.ornata_PA1_allRun_RFd"
proj <- BIOMOD_Projection(bm.mod=mod, new.env=env, proj.name="proj_strict_RFd", models.chosen=model_name, binary.meth=NULL, filtered.meth=NULL, build.clamping.mask=FALSE)
setwd(orig_wd)
out_rds <- "results/targeted_model_improvement_20260729/Musa ornata_improved_RFd_projection.rds"
saveRDS(proj, out_rds)
cat("Saved:", out_rds, "\n")
r <- get_predictions(proj)
cat(sprintf("Raster: dim=%s, min=%.1f, max=%.1f\n", paste(dim(r),collapse="x"), minmax(r)[1], minmax(r)[2]))
'
