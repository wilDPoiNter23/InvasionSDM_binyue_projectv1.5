# Original source: slurm_36_aggs814.sh
# Original source location: ${NON_NATIVE_ROOT}/02_derived_non_native_potential/07_scripts/slurm_36_aggs814.sh
# Original source SHA-256: 632083a8ff8fb9c00036b7ba7b7463bbbca0904121a9803429adae029bd7d4c0
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_NON_NATIVE)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/bin/bash
#SBATCH --job-name=var_aggs814
#SBATCH --partition=thcp1
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=32G
#SBATCH --time=10:00:00
#SBATCH --output=<NON_NATIVE_ROOT>/02_derived_non_native_potential/08_logs/36_aggs814_%j.out
#SBATCH --error=<NON_NATIVE_ROOT>/02_derived_non_native_potential/08_logs/36_aggs814_%j.err
source ~/.bashrc
module add loginnode
export R_LIBS_USER="<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu"
cd <NON_NATIVE_ROOT>/02_derived_non_native_potential/07_scripts
Rscript 36_aggregate_variants_814.R
