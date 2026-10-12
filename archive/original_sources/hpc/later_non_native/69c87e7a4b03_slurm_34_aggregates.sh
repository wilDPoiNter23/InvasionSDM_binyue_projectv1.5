# Original source: slurm_34_aggregates.sh
# Original source location: ${NON_NATIVE_ROOT}/02_derived_non_native_potential/07_scripts/slurm_34_aggregates.sh
# Original source SHA-256: 69c87e7a4b035cd2f03c832462c44831087fd9c40a7278227d73749823cb2244
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_NON_NATIVE)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/bin/bash
#SBATCH --job-name=var_aggs
#SBATCH --partition=thcp1
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=32G
#SBATCH --time=10:00:00
#SBATCH --output=<NON_NATIVE_ROOT>/02_derived_non_native_potential/08_logs/34_aggs_%j.out
#SBATCH --error=<NON_NATIVE_ROOT>/02_derived_non_native_potential/08_logs/34_aggs_%j.err
source ~/.bashrc
module add loginnode
export R_LIBS_USER="<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu"
cd <NON_NATIVE_ROOT>/02_derived_non_native_potential/07_scripts
Rscript 34_aggregate_variants.R
