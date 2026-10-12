# Original source: slurm_gap13_array.sh
# Original source location: ${WORK_ROOT}/publication_release_v1/scripts/slurm_gap13_array.sh
# Original source SHA-256: 2a5cc7c0414ced3e19b9d3be7fa888da8dc13bf74e5bc754874404cd025b9350
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/bin/bash
#SBATCH --job-name=gap13arr
#SBATCH --partition=thcp1
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=64G
#SBATCH --time=4:00:00
#SBATCH --array=1-13
#SBATCH --output=<WORK_ROOT>/logs/gap13arr_%A_%a.out
#SBATCH --error=<WORK_ROOT>/logs/gap13arr_%A_%a.err
source ~/.bashrc
module add loginnode
export R_LIBS_USER="<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu"
export R_LIBS_SITE="$R_LIBS_USER"
export R_LIBS="$R_LIBS_USER"

BASE="<WORK_ROOT>"
SCRIPT="${BASE}/publication_release_v1/scripts/project_gap13_repaired.r"
MANIFEST="${BASE}/publication_release_v1/manifests/gap13_repair_projection_selection_final_v2.csv"

# Extract species name from manifest by array task ID (line number = task ID + 1 for header)
SP=$(sed -n "$((SLURM_ARRAY_TASK_ID + 1))p" "$MANIFEST" | cut -d',' -f1 | tr -d '"')
echo "Task ${SLURM_ARRAY_TASK_ID}: ${SP}"
echo "Started: $(date)"
if Rscript "$SCRIPT" "$SP"; then
    echo "SUCCESS: ${SP}"
else
    echo "FAILED: ${SP}"
fi
echo "Finished: $(date)"
