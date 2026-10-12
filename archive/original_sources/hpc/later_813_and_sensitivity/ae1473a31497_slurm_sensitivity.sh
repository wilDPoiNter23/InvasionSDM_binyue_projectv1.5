# Original source: slurm_sensitivity.sh
# Original source location: ${WORK_ROOT}/archive/slurm_old/slurm_sensitivity.sh
# Original source SHA-256: ae1473a31497585004c5d2e0ea61f88b6ca43a6be03224acc529e2ea33a7f3c9
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/bin/bash
#SBATCH --job-name=sensitivity
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=64G
#SBATCH --time=4:00:00
#SBATCH --partition=thcp1
#SBATCH --output=logs/sensitivity_%j.out
source ~/.bashrc
module add loginnode
Rscript run_sensitivity_aggregation.R
