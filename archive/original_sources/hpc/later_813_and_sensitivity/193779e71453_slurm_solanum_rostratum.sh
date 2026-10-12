# Original source: slurm_solanum_rostratum.sh
# Original source location: ${WORK_ROOT}/publication_release_v1/scripts/slurm_solanum_rostratum.sh
# Original source SHA-256: 193779e714533ad5727902e630ecf24557c8d63ee0dba79c7c4d85aec891c258
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/bin/bash
#SBATCH --job-name=solrostr
#SBATCH --partition=thcp1
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8
#SBATCH --mem=32G
#SBATCH --time=06:00:00
#SBATCH --output=<WORK_ROOT>/logs/solrostr_%j.out
#SBATCH --error=<WORK_ROOT>/logs/solrostr_%j.err
source ~/.bashrc
module add loginnode
export R_LIBS_USER="<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu"
Rscript <WORK_ROOT>/publication_release_v1/scripts/refit_Solanum_rostratum_v1.R
