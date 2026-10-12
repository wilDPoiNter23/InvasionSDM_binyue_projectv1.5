# Original source: slurm_non_native_projection_array.sh
# Original source location: ${NON_NATIVE_ROOT}/02_derived_non_native_potential/07_scripts/slurm_non_native_projection_array.sh
# Original source SHA-256: b83ef450a3e7d0b865080e03903c268924d49002b84f7af6e99c6f04aaa9a2a4
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_NON_NATIVE)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/bin/bash
#SBATCH --job-name=nn_mask
#SBATCH --array=1-813%20
#SBATCH --cpus-per-task=1
#SBATCH --mem=4G
#SBATCH --partition=thcp1
#SBATCH --output=<NON_NATIVE_ROOT>/02_derived_non_native_potential/08_logs/non_native_mask_%A_%a.out
#SBATCH --error=<NON_NATIVE_ROOT>/02_derived_non_native_potential/08_logs/non_native_mask_%A_%a.err
#SBATCH --time=02:00:00
#
# slurm_non_native_projection_array.sh — PHASE 15/16
# One SLURM task = one taxon (row index into frozen manifest v3, 1..813).
# Fail-safe: failed tasks are logged; retry only failed indices (see
# 12_retry_failed.R / retry logic in the run book).

source ~/.bashrc
module add loginnode
export R_LIBS="<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu"

SCRIPT_DIR="<NON_NATIVE_ROOT>/02_derived_non_native_potential/07_scripts"
Rscript "$SCRIPT_DIR/09_mask_one_taxon.R" "$SLURM_ARRAY_TASK_ID"
exit $?
