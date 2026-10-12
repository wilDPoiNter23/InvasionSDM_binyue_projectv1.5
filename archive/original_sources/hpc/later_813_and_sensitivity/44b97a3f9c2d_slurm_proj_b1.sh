# Original source: slurm_proj_b1.sh
# Original source location: ${WORK_ROOT}/archive/slurm_old/slurm_proj_b1.sh
# Original source SHA-256: 44b97a3f9c2d1deae04500932533c499c1fb31e538598814f07b65585e1303de
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/bin/bash
#SBATCH --job-name=proj_b1
#SBATCH --array=1-29
#SBATCH --output=<WORK_ROOT>/logs/proj_b1_%A_%a.out
#SBATCH --error=<WORK_ROOT>/logs/proj_b1_%A_%a.err
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8
#SBATCH --mem=128G
#SBATCH --time=2-00:00:00
#SBATCH --partition=thcp1

source ~/.bashrc
module add loginnode
export TMPDIR=<WORK_ROOT>/temp
mkdir -p $TMPDIR
cd <WORK_ROOT>

SPECIES=$(sed -n "${SLURM_ARRAY_TASK_ID}p" species_batch_00)
echo "Batch1 Task $SLURM_ARRAY_TASK_ID/29: $SPECIES"
echo "Started: $(date)"
Rscript proper_ensemble_project.r "$SPECIES"
echo "Done: $(date)"
