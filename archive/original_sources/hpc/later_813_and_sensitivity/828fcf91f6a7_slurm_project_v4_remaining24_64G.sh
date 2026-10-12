# Original source: slurm_project_v4_remaining24_64G.sh
# Original source location: ${WORK_ROOT}/slurm_project_v4_remaining24_64G.sh
# Original source SHA-256: 828fcf91f6a7ea1e055084e8885f90ee74de455cee31b4d6b29ab7d523ac1216
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/bin/bash
#SBATCH --job-name=v4_24_64G
#SBATCH --output=<WORK_ROOT>/results/targeted_negative_TSS_projection_multifold_v4/remaining24_%A_%a.out
#SBATCH --error=<WORK_ROOT>/results/targeted_negative_TSS_projection_multifold_v4/remaining24_%A_%a.err
#SBATCH --array=1-24
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=64G
#SBATCH --time=12:00:00
#SBATCH --partition=thcp1
set -euo pipefail
source ~/.bashrc; module add loginnode
export R_LIBS_USER="<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu"
export R_LIBS_SITE="$R_LIBS_USER"; export R_LIBS="$R_LIBS_USER"

SPECIES_FILE="<WORK_ROOT>/publication_release_v1/manifests/projection_v4_remaining24_species.csv"
SN=$(sed -n "${SLURM_ARRAY_TASK_ID}p" "$SPECIES_FILE")

echo "=== V4 64GB Task ${SLURM_ARRAY_TASK_ID}/24: ${SN} ==="
echo "Job: ${SLURM_JOB_ID}  Task: ${SLURM_ARRAY_TASK_ID}  Node: $(hostname)"
echo "Requested: 64GB 1CPU 12h"

Rscript --no-save <WORK_ROOT>/project_26_repaired_v4_64G.r "${SN}"
RC=$?
echo "Exit: ${RC}"
exit ${RC}
