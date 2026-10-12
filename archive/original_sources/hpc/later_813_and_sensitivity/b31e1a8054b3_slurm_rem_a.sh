# Original source: slurm_rem_a.sh
# Original source location: ${WORK_ROOT}/archive/slurm_old/slurm_rem_a.sh
# Original source SHA-256: b31e1a8054b31f7a5ad2e5f397700cfe41b74b618b154c81195ba91e9533e7dd
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/bin/bash
#SBATCH --job-name=proj_A
#SBATCH --array=1-27
#SBATCH --output=<WORK_ROOT>/logs/proj_A_%A_%a.out
#SBATCH --error=<WORK_ROOT>/logs/proj_A_%A_%a.err
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

SPECIES=$(sed -n "${SLURM_ARRAY_TASK_ID}p" species_rem_batch_00)
echo "BatchA $SLURM_ARRAY_TASK_ID/27: $SPECIES ($(date))"
Rscript proper_ensemble_project.r "$SPECIES"
echo "Done: $(date)"
