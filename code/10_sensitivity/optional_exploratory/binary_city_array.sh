#!/usr/bin/env bash
# Extract species batches on a fixed native polygon support.
#SBATCH --job-name=binary_city
#SBATCH --cpus-per-task=1
#SBATCH --mem=8G
#SBATCH --time=06:00:00
set -euo pipefail
: "${BINARY_REPOSITORY:?}" "${BINARY_CITY_CONFIG:?}" "${BINARY_CITY_OUTPUT:?}" "${BINARY_RSCRIPT:?}" "${SLURM_ARRAY_TASK_ID:?}"
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1
batch="${BINARY_BATCH_SIZE:-10}"
first=$((SLURM_ARRAY_TASK_ID * batch + 1))
"${BINARY_RSCRIPT}" --vanilla "${BINARY_REPOSITORY}/code/10_sensitivity/optional_exploratory/binary_city_extract.R" \
  "${BINARY_CITY_CONFIG}" "${BINARY_CITY_OUTPUT}/batch_${SLURM_ARRAY_TASK_ID}" "${first}" "${batch}"
