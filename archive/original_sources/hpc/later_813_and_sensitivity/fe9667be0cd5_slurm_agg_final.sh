# Original source: slurm_agg_final.sh
# Original source location: ${WORK_ROOT}/archive/slurm_old/slurm_agg_final.sh
# Original source SHA-256: fe9667be0cd558bd8a6707590ee64074081c47a1c445d2a14ce26ccd7c5e48d6
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/bin/bash
#SBATCH --job-name=agg_final
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=64G
#SBATCH --time=12:00:00
#SBATCH --partition=thcp1
#SBATCH --output=logs/agg_final_%j.out

source ~/.bashrc
module add loginnode

cd <WORK_ROOT>
Rscript run_aggregation_final.R
