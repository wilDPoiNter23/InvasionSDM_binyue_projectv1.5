# Original source: slurm_agg_813_fixed.sh
# Original source location: ${WORK_ROOT}/archive/slurm_old/slurm_agg_813_fixed.sh
# Original source SHA-256: 16714d0585888d66eb5050e551c6fe4a4553f89bfb67839172f3b297b5f9758d
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/bin/bash
#SBATCH --job-name=agg_813_fix
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=64G
#SBATCH --time=8:00:00
#SBATCH --partition=thcp1
#SBATCH --output=logs/agg_813_fix_%j.out

source ~/.bashrc
module add loginnode

cd <WORK_ROOT>
Rscript run_aggregation_813_optimized.R
