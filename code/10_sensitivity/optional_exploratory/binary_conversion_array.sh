#!/usr/bin/env bash
# Convert bounded rows from an existing continuous-product manifest.
#SBATCH --job-name=binary_maps
#SBATCH --cpus-per-task=1
#SBATCH --mem=4G
#SBATCH --time=06:00:00
set -euo pipefail
: "${BINARY_REPOSITORY:?}" "${BINARY_CONVERSION_MANIFEST:?}" "${BINARY_CONVERSION_SUMMARIES:?}" "${BINARY_RSCRIPT:?}" "${SLURM_ARRAY_TASK_ID:?}"
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1
batch="${BINARY_CONVERSION_BATCH_SIZE:-4}"
first=$((SLURM_ARRAY_TASK_ID * batch + 1))
mkdir -p "${BINARY_CONVERSION_SUMMARIES}"
"${BINARY_RSCRIPT}" --vanilla "${BINARY_REPOSITORY}/code/10_sensitivity/optional_exploratory/binary_convert.R" \
  "${BINARY_CONVERSION_MANIFEST}" "${BINARY_CONVERSION_SUMMARIES}/batch_${SLURM_ARRAY_TASK_ID}.csv" "${first}" "${batch}"
