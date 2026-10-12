# Original source: slurm_aggregate_813_clean_v2.sh
# Original source location: ${WORK_ROOT}/publication_release_v1/scripts/slurm_aggregate_813_clean_v2.sh
# Original source SHA-256: 3fdac3837e64aba7634a2643fc645b6053d64cf4799829dee0c081cbba9f9ade
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/bin/bash
#SBATCH --job-name=agg813v2
#SBATCH --partition=thcp1
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=64G
#SBATCH --time=12:00:00
#SBATCH --output=<WORK_ROOT>/logs/agg813v2_%j.out
#SBATCH --error=<WORK_ROOT>/logs/agg813v2_%j.err

source ~/.bashrc
module add loginnode
export R_LIBS_USER="<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu"
export R_LIBS_SITE="$R_LIBS_USER"
export R_LIBS="$R_LIBS_USER"
cd <WORK_ROOT>
Rscript publication_release_v1/scripts/aggregate_publication_813_clean_v2.R
echo "Aggregation done: $(date)"
