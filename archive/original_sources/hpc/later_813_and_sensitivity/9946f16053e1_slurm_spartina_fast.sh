# Original source: slurm_spartina_fast.sh
# Original source location: ${WORK_ROOT}/publication_release_v1/scripts/slurm_spartina_fast.sh
# Original source SHA-256: 9946f16053e126d550db702d44fce66a0ba65111d2ffe429f5c64422242d59cc
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/bin/bash
#SBATCH --job-name=sp_fast
#SBATCH --partition=thcp1
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=64G
#SBATCH --time=04:00:00
#SBATCH --output=<WORK_ROOT>/logs/spartina_fast_%j.out
#SBATCH --error=<WORK_ROOT>/logs/spartina_fast_%j.err

source ~/.bashrc
module add loginnode

export R_LIBS_USER="<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu"
cd <WORK_ROOT>

Rscript publication_release_v1/scripts/project_Spartina_noID_fast.R 2>&1

echo "Job exit: $?"
date
