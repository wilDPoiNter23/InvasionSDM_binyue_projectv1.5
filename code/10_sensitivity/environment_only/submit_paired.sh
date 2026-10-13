#!/bin/bash
# Slurm template for paired continuous aggregation and city comparison.
# Set the account, partition and R environment when submitting.
#SBATCH --job-name=paired_sensitivity
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8
#SBATCH --mem=64G
#SBATCH --time=24:00:00
#SBATCH --output=paired_%j.out
#SBATCH --error=paired_%j.err
set -euo pipefail
[[ "$#" -ge 3 && "$#" -le 4 ]] || { echo 'Usage: sbatch submit_paired.sh MANIFEST OUTPUT_DIR CITY_POLYGONS [CITY_ID_FIELD]' >&2; exit 2; }
: "${INVASIONSDM_PROJECT_ROOT:?Set the repository root}"
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1
SCRIPT_DIR="$INVASIONSDM_PROJECT_ROOT/code/10_sensitivity/environment_only"
mkdir -p "$2"
"${RSCRIPT:-Rscript}" --vanilla "$SCRIPT_DIR/validate_paired_inputs.R" "$1" "$2/paired_input_checks.csv"
"${RSCRIPT:-Rscript}" --vanilla "$SCRIPT_DIR/aggregate_paired.R" --manifest "$1" --output-dir "$2" --workers "${SLURM_CPUS_PER_TASK:-1}"
"${RSCRIPT:-Rscript}" --vanilla "$SCRIPT_DIR/compare_paired.R" --manifest "$1" --output-dir "$2" --cities "$3" --city-id "${4:-shapeName}"
