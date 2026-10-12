# Original source: slurm_eval_v2.sh
# Original source location: ${WORK_ROOT}/archive/slurm_old/slurm_eval_v2.sh
# Original source SHA-256: b62924b8f3b3030c597ee7b41f284437d9474a2c8f8c8cec9a367f48aa0ce3df
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/bin/bash
#SBATCH --job-name=eval_v2
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=16G
#SBATCH --time=4:00:00
#SBATCH --partition=thcp1
#SBATCH --output=logs/eval_v2_%j.out

source ~/.bashrc
module add loginnode

cd <WORK_ROOT>
Rscript extract_evaluations_v2.R
