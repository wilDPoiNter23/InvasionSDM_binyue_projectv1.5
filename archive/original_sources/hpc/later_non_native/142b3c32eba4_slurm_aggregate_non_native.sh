# Original source: slurm_aggregate_non_native.sh
# Original source location: ${NON_NATIVE_ROOT}/02_derived_non_native_potential/07_scripts/slurm_aggregate_non_native.sh
# Original source SHA-256: 142b3c32eba43f538db43ec38051d7e3c4158e6cd9b56fa961683e1e57c58079
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_NON_NATIVE)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/bin/bash
#SBATCH --job-name=nn_aggregate
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=2
#SBATCH --mem=12G
#SBATCH --partition=thcp1
#SBATCH --output=<NON_NATIVE_ROOT>/02_derived_non_native_potential/08_logs/non_native_aggregate_%j.out
#SBATCH --error=<NON_NATIVE_ROOT>/02_derived_non_native_potential/08_logs/non_native_aggregate_%j.err
#SBATCH --time=08:00:00
#
# slurm_aggregate_non_native.sh — PHASE 17-20 (runs 11_aggregate_non_native.R)

source ~/.bashrc
module add loginnode
export R_LIBS="<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu"

Rscript <NON_NATIVE_ROOT>/02_derived_non_native_potential/07_scripts/11_aggregate_non_native.R
exit $?
