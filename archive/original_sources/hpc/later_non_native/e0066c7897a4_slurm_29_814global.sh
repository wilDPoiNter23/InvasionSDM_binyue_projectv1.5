# Original source: slurm_29_814global.sh
# Original source location: ${NON_NATIVE_ROOT}/02_derived_non_native_potential/07_scripts/slurm_29_814global.sh
# Original source SHA-256: e0066c7897a49d84bf26279bbd4d8f335a78618fb62e17c65ef9bae4150615fa
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_NON_NATIVE)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/bin/bash
#SBATCH --job-name=nn814_glob
#SBATCH --partition=thcp1
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=48G
#SBATCH --time=04:00:00
#SBATCH --output=<NON_NATIVE_ROOT>/02_derived_non_native_potential/08_logs/29_814g_%j.out
#SBATCH --error=<NON_NATIVE_ROOT>/02_derived_non_native_potential/08_logs/29_814g_%j.err
source ~/.bashrc
module add loginnode
export R_LIBS_USER="<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu"
cd <NON_NATIVE_ROOT>/02_derived_non_native_potential/07_scripts
Rscript 29_aggregate_814_global.R
