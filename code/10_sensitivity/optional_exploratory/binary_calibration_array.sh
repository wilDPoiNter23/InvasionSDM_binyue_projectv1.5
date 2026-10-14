#!/usr/bin/env bash
# Extract current-map calibration thresholds in bounded species batches.
# Submit with an explicit array range and exported external paths.
#SBATCH --job-name=binary_calibration
#SBATCH --cpus-per-task=1
#SBATCH --mem=4G
#SBATCH --time=04:00:00
set -euo pipefail
: "${BINARY_REPOSITORY:?}" "${BINARY_MANIFEST:?}" "${BINARY_OUTPUT:?}" "${BINARY_RSCRIPT:?}"
: "${SLURM_ARRAY_TASK_ID:?}"
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1
batch="${BINARY_BATCH_SIZE:-10}"
first=$((SLURM_ARRAY_TASK_ID * batch + 1))
output="${BINARY_OUTPUT}/batch_${SLURM_ARRAY_TASK_ID}"
"${BINARY_RSCRIPT}" --vanilla "${BINARY_REPOSITORY}/code/10_sensitivity/optional_exploratory/binary_calibration_extract.R" \
  "${BINARY_MANIFEST}" "${output}" "${first}" "${batch}"
