# Original source: slurm_5sp_repair.sh
# Original source location: ${WORK_ROOT}/archive/slurm_old/slurm_5sp_repair.sh
# Original source SHA-256: f99d67073fdf18f8f8524ab63ca204b512522cb89b7b9f089291eb04932670c3
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/bin/bash
#SBATCH --job-name=repair_5sp
#SBATCH --array=1-5
#SBATCH --output=<WORK_ROOT>/logs/repair_%A_%a.out
#SBATCH --error=<WORK_ROOT>/logs/repair_%A_%a.err
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8
#SBATCH --mem=120G
#SBATCH --time=1-00:00:00
#SBATCH --partition=thcp1

source ~/.bashrc
module add loginnode
export TMPDIR=<WORK_ROOT>/temp
mkdir -p $TMPDIR
cd <WORK_ROOT>

case $SLURM_ARRAY_TASK_ID in
  1) SP="Ailanthus altissima" ;;
  2) SP="Tamarix aphylla" ;;
  3) SP="Veronica arvensis" ;;
  4) SP="Geranium dissectum" ;;
  5) SP="Ageratum houstonianum" ;;
esac

echo "Task $SLURM_ARRAY_TASK_ID: $SP"
echo "Started: $(date)"
Rscript repair_single_species.r "$SP"
echo "Done: $(date)"
