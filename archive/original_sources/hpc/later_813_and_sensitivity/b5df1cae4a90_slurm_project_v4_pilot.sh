# Original source: slurm_project_v4_pilot.sh
# Original source location: ${WORK_ROOT}/slurm_project_v4_pilot.sh
# Original source SHA-256: b5df1cae4a90aa0164bf8cb245b3b55a772e93362f415c15fa388d93dbd6802c
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/bin/bash
#SBATCH --job-name=v4_pilot
#SBATCH --output=<WORK_ROOT>/results/targeted_negative_TSS_projection_multifold_v4/pilot_%A_%a.out
#SBATCH --error=<WORK_ROOT>/results/targeted_negative_TSS_projection_multifold_v4/pilot_%A_%a.err
#SBATCH --array=1-2 --ntasks=1 --cpus-per-task=1 --mem=32G --time=12:00:00 --partition=thcp1
set -euo pipefail
source ~/.bashrc; module add loginnode
export R_LIBS_USER="<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu"
export R_LIBS_SITE="$R_LIBS_USER"; export R_LIBS="$R_LIBS_USER"
SP=("Pachira aquatica" "Geranium dissectum")
SN="${SP[$((SLURM_ARRAY_TASK_ID-1))]}"
echo "=== v4 Pilot ${SLURM_ARRAY_TASK_ID}/2: ${SN} ==="
Rscript --no-save <WORK_ROOT>/project_26_repaired_v4.r "${SN}"
echo "Exit: $?"
