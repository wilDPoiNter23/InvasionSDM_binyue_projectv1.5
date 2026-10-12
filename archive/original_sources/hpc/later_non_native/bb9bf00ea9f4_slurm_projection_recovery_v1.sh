# Original source: slurm_projection_recovery_v1.sh
# Original source location: ${NON_NATIVE_ROOT}/audit/environment_only_sensitivity/review_triage/slurm_projection_recovery_v1.sh
# Original source SHA-256: bb9bf00ea9f417b5193611744cc7666635aae1fff27e4855b1d6a36612cf132a
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_ENVIRONMENT_ONLY_SENSITIVITY)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/bin/bash
#SBATCH --job-name=s814_review_proj
#SBATCH --partition=thcp1
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=64G
#SBATCH --time=24:00:00
#SBATCH --output=<NON_NATIVE_ROOT>/audit/environment_only_sensitivity/logs/review_projection_%j.out
#SBATCH --error=<NON_NATIVE_ROOT>/audit/environment_only_sensitivity/logs/review_projection_%j.err
set -euo pipefail
: "$SENSITIVITY_FROZEN_MODULE_INITIALIZED"
: "$SENSITIVITY_FROZEN_LD_SHA256"
[[ "$(printf '%s' "$LD_LIBRARY_PATH" | sha256sum | cut -d' ' -f1)" == "$SENSITIVITY_FROZEN_LD_SHA256" ]] || { echo FROZEN_LOADER_ENV_CHANGED >&2; exit 70; }
export R_LIBS_USER=<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu
export R_LIBS_SITE="$R_LIBS_USER" R_LIBS="$R_LIBS_USER"
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1
AUDIT=<NON_NATIVE_ROOT>/audit/environment_only_sensitivity
<HPC_HOME>/R-4.4.1/bin/Rscript --vanilla "$AUDIT/review_triage/review_projection_recovery_v1.R" "$1"
