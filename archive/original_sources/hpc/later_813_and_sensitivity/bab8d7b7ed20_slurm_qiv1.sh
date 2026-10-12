# Original source: slurm_qiv1.sh
# Original source location: ${WORK_ROOT}/archive/slurm_old/slurm_qiv1.sh
# Original source SHA-256: bab8d7b7ed206d35fd0d4332fa1479f991d3ac730e3ac65a9ee32382e9ef5e07
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/bin/bash
#SBATCH --job-name=qiv1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=64G
#SBATCH --time=4:00:00
#SBATCH --partition=thcp1
#SBATCH --output=logs/qiv1_%j.out
source ~/.bashrc
module add loginnode
Rscript run_quality_improved_aggregate_v1.R
