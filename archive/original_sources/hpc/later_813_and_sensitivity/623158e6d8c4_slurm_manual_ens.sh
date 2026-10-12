# Original source: slurm_manual_ens.sh
# Original source location: ${WORK_ROOT}/archive/slurm_old/slurm_manual_ens.sh
# Original source SHA-256: 623158e6d8c4ee39c1937b97780071241bb4d7fcca5eb33a00d153727d796068
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/bin/bash
#SBATCH --job-name=manual_ens
#SBATCH --array=1-3
#SBATCH --output=<WORK_ROOT>/logs/manual_%A_%a.out
#SBATCH --error=<WORK_ROOT>/logs/manual_%A_%a.err
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

case $SLURM_ARRAY_TASK_ID in
    1) SPECIES="Vulpia fasciculata" ;;
    2) SPECIES="Sonchus oleraceus" ;;
    3) SPECIES="Lepidium draba" ;;
esac

echo "Task $SLURM_ARRAY_TASK_ID: $SPECIES"
echo "Node: $(hostname)"
echo "Started: $(date)"

Rscript manual_ensemble_project.r "$SPECIES"

echo "Done: $(date)"
