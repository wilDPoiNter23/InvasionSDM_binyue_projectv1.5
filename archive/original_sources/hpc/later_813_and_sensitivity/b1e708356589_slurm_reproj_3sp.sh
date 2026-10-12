# Original source: slurm_reproj_3sp.sh
# Original source location: ${WORK_ROOT}/archive/slurm_old/slurm_reproj_3sp.sh
# Original source SHA-256: b1e70835658906b721cb6523d1860ec4bc3002af95f58f9e9714dfbce58dd967
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/bin/bash
#SBATCH --job-name=reproj_3sp
#SBATCH --array=1-3
#SBATCH --output=<WORK_ROOT>/logs/reproj_%A_%a.out
#SBATCH --error=<WORK_ROOT>/logs/reproj_%A_%a.err
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=32G
#SBATCH --time=5-00:00:00
#SBATCH --partition=thcp1

source ~/.bashrc
module add loginnode

cd <WORK_ROOT>

# Map array task to species
case $SLURM_ARRAY_TASK_ID in
    1) SPECIES="Vulpia fasciculata" ;;
    2) SPECIES="Sonchus oleraceus" ;;
    3) SPECIES="Lepidium draba" ;;
esac

echo "Task $SLURM_ARRAY_TASK_ID: $SPECIES"
echo "Node: $(hostname)"
echo "Started: $(date)"

Rscript reproject_single_species.r "$SPECIES"

echo "Done: $(date)"
