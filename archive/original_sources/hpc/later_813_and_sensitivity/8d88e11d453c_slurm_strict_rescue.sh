# Original source: slurm_strict_rescue.sh
# Original source location: ${WORK_ROOT}/archive/slurm_old/slurm_strict_rescue.sh
# Original source SHA-256: 8d88e11d453c65256f4807aea6d0f8e11556e4a63ac8963f79dcbed36b97c935
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/bin/bash
#SBATCH --job-name=strict_rescue
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=64G
#SBATCH --time=4:00:00
#SBATCH --partition=thcp1
#SBATCH --output=logs/strict_rescue_%j.out
source ~/.bashrc
module add loginnode
Rscript run_strict_rescue_aggregate.R
