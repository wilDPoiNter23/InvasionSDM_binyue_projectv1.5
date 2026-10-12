# Original source: slurm_2sp_stratified_PA.sh
# Original source location: ${WORK_ROOT}/slurm_2sp_stratified_PA.sh
# Original source SHA-256: d4157f2d1d282adf67d977a180e7e9c98a2f284ffecfd769920894e7beee8c86
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/bin/bash
# slurm_2sp_stratified_PA.sh — 2-species spatially stratified PA repair
#SBATCH --job-name=strat_PA
#SBATCH --output=<WORK_ROOT>/results/targeted_negative_TSS_repair_v1/logs/slurm_strat_%A_%a.out
#SBATCH --error=<WORK_ROOT>/results/targeted_negative_TSS_repair_v1/logs/slurm_strat_%A_%a.err
#SBATCH --array=1-2
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=32G
#SBATCH --time=12:00:00
#SBATCH --partition=thcp1

set -euo pipefail
source ~/.bashrc
module add loginnode
export TMPDIR="<WORK_ROOT>/results/targeted_negative_TSS_repair_v1/tmp"

WORK_DIR="<WORK_ROOT>"
R_LIBS="<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu"

SPECIES_LIST=("Cinnamomum camphora" "Croton setiger")

TASK_ID=$((SLURM_ARRAY_TASK_ID - 1))
SP_NAME="${SPECIES_LIST[$TASK_ID]}"

mkdir -p "${WORK_DIR}/results/targeted_negative_TSS_repair_v1/logs"

echo "=== Stratified PA Repair: ${SP_NAME} ==="
echo "Node: $(hostname) | Started: $(date)"

export R_LIBS_USER="$R_LIBS" R_LIBS_SITE="$R_LIBS" R_LIBS="$R_LIBS"
Rscript --no-save "${WORK_DIR}/repair_targeted_modeling_v1.r" "${SP_NAME}"
echo "Exit: $? | Finished: $(date)"
