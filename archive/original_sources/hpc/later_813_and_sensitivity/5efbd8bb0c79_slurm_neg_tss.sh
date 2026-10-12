# Original source: slurm_neg_tss.sh
# Original source location: ${WORK_ROOT}/archive/slurm_old/slurm_neg_tss.sh
# Original source SHA-256: 5efbd8bb0c79c3af8ddfeb44982293256d17244df5f0d60a58555441c1f274dd
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/bin/bash
#SBATCH --job-name=neg_tss_fix
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=32G
#SBATCH --time=2:00:00
#SBATCH --partition=thcp1
#SBATCH --output=logs/neg_tss_fix_%j.out
source ~/.bashrc
module add loginnode
Rscript run_negative_tss_reproject.R
