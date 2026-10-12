# Original source: slurm_spartina_clean_final.sh
# Original source location: ${WORK_ROOT}/publication_release_v1/scripts/slurm_spartina_clean_final.sh
# Original source SHA-256: 57d435cc6f43a6114046bffdff9ca22e9a296bb7b9e98c9103dbabb80eadabb2
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/bin/bash
#SBATCH --job-name=sp_clean
#SBATCH --partition=thcp1
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8
#SBATCH --mem=32G
#SBATCH --time=06:00:00
#SBATCH --output=<WORK_ROOT>/logs/spartina_clean_%j.out
#SBATCH --error=<WORK_ROOT>/logs/spartina_clean_%j.err

source ~/.bashrc
module add loginnode

export R_LIBS_USER="<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu"
cd <WORK_ROOT>

Rscript publication_release_v1/scripts/refit_Spartina_noID_final_v1.R 2>&1

echo "Job exit: $?"
date
