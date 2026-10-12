# Original source: slurm_bulk_proj.sh
# Original source location: ${WORK_ROOT}/archive/slurm_old/slurm_bulk_proj.sh
# Original source SHA-256: 36156f750625c8a6f296296dae9748e3e542d103007fadf63bb37fd981874391
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/bin/bash
#SBATCH --job-name=bulk_proj
#SBATCH --array=1-58
#SBATCH --output=<WORK_ROOT>/logs/bulk_%A_%a.out
#SBATCH --error=<WORK_ROOT>/logs/bulk_%A_%a.err
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8
#SBATCH --mem=64G
#SBATCH --time=2-00:00:00
#SBATCH --partition=thcp1

source ~/.bashrc
module add loginnode
export TMPDIR=<WORK_ROOT>/temp
mkdir -p $TMPDIR

cd <WORK_ROOT>

# Read species from file
SPECIES=$(sed -n "${SLURM_ARRAY_TASK_ID}p" species_to_project.txt)

echo "Task $SLURM_ARRAY_TASK_ID/58: $SPECIES"
echo "Node: $(hostname)"
echo "Started: $(date)"

Rscript manual_ensemble_project.r "$SPECIES"

echo "Done: $(date)"
