# Original source: slurm_bulk814.sh
# Original source location: ${NON_NATIVE_ROOT}/audit/environment_only_sensitivity/slurm_bulk814.sh
# Original source SHA-256: 618b03d46850c99b086e3c96ec1842b3bd60c3b8079bebc7a9137eeafc83e25e
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_ENVIRONMENT_ONLY_SENSITIVITY)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/bin/bash
#SBATCH --job-name=s814_v2_bulk
#SBATCH --partition=thcp1
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=64G
#SBATCH --time=24:00:00
#SBATCH --output=<NON_NATIVE_ROOT>/audit/environment_only_sensitivity/logs/bulk_%A_%a.out
#SBATCH --error=<NON_NATIVE_ROOT>/audit/environment_only_sensitivity/logs/bulk_%A_%a.err
set -euo pipefail
# The module backup file exists on the submit/login host, not on compute hosts.
: "${SENSITIVITY_FROZEN_MODULE_INITIALIZED:?Submit shell did not initialise frozen modules}"
: "${SENSITIVITY_FROZEN_LD_SHA256:?Missing exported frozen loader fingerprint}"
[[ "$(printf '%s' "${LD_LIBRARY_PATH:-}" | sha256sum | cut -d' ' -f1)" == "$SENSITIVITY_FROZEN_LD_SHA256" ]] || { echo 'FROZEN_LOADER_ENV_CHANGED' >&2; exit 70; }
export R_LIBS_USER=<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu
export R_LIBS_SITE="$R_LIBS_USER" R_LIBS="$R_LIBS_USER"
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1
AUDIT=<NON_NATIVE_ROOT>/audit/environment_only_sensitivity
mapfile -t TASK < <(python3 "$AUDIT/production_controller.py" resolve "$1" "$SLURM_ARRAY_TASK_ID")
[[ "${#TASK[@]}" == 2 ]]
<HPC_HOME>/R-4.4.1/bin/Rscript --vanilla "$AUDIT/run_sensitivity814.R" "${TASK[0]}" "${TASK[1]}"
if [[ "${TASK[1]}" == CLEAN_FULL || "${TASK[1]}" == CLEAN_ENV ]]; then
 python3 "$AUDIT/production_controller.py" pair-if-ready "${TASK[0]}"
fi
