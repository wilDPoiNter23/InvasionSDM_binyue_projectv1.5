# Original source: slurm_spartina_post.sh
# Original source location: ${WORK_ROOT}/publication_release_v1/scripts/slurm_spartina_post.sh
# Original source SHA-256: d607fc757d854717f2614ece0268f4f4811ae9233788dd2546e68fe1cda2abe1
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/bin/bash
#SBATCH --job-name=sp_post
#SBATCH --partition=thcp1
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=32G
#SBATCH --time=04:00:00
#SBATCH --output=<WORK_ROOT>/logs/spartina_post_%j.out
#SBATCH --error=<WORK_ROOT>/logs/spartina_post_%j.err

source ~/.bashrc
module add loginnode

export R_LIBS_USER="<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu"
cd <WORK_ROOT>

Rscript publication_release_v1/scripts/refit_Spartina_noID_postmodeling.R 2>&1

echo "Job exit: $?"
date
