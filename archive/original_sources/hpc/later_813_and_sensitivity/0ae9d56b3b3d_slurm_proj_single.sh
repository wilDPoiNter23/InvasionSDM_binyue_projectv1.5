# Original source: slurm_proj_single.sh
# Original source location: ${WORK_ROOT}/archive/slurm_old/slurm_proj_single.sh
# Original source SHA-256: 0ae9d56b3b3d0c976f18a7cca83a6a9e4b2a843a36d89f09c6e1efce4c92a4d8
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/bin/bash
#SBATCH --job-name=proj_single
#SBATCH --array=1-29
#SBATCH --output=<WORK_ROOT>/logs/proj_s_%A_%a.out
#SBATCH --error=<WORK_ROOT>/logs/proj_s_%A_%a.err
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=62
#SBATCH --mem=120G
#SBATCH --time=1-00:00:00
#SBATCH --partition=thcp1

source ~/.bashrc
module add loginnode
export TMPDIR=<WORK_ROOT>/temp
mkdir -p $TMPDIR
cd <WORK_ROOT>

SPECIES=$(sed -n "${SLURM_ARRAY_TASK_ID}p" species_batch_00)
echo "Task $SLURM_ARRAY_TASK_ID/29: $SPECIES"
echo "Node: $(hostname) Started: $(date)"
Rscript proper_ensemble_project.r "$SPECIES"
echo "Done: $(date)"
