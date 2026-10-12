# Original source: slurm_eval_extract.sh
# Original source location: ${WORK_ROOT}/archive/slurm_old/slurm_eval_extract.sh
# Original source SHA-256: 9661dc58b9217e946b92f95408292d4a2456c5f56cf90e3ce8d0f5235b715340
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/bin/bash
#SBATCH --job-name=eval_extract
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=16G
#SBATCH --time=4:00:00
#SBATCH --partition=thcp1
#SBATCH --output=logs/eval_extract_%j.out

source ~/.bashrc
module add loginnode

cd <WORK_ROOT>
Rscript extract_evaluations.R
