#!/bin/bash
#SBATCH --job-name=environment_only
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=64G
#SBATCH --time=24:00:00
#SBATCH --output=environment_only_%j.out
#SBATCH --error=environment_only_%j.err
# Input: accepted taxon and variant (ENV, CLEAN_ENV or CLEAN_FULL)
# Output: one species model and projection in the configured working directory
# Set the partition/account at submission and load the appropriate R environment.
set -euo pipefail
: "${INVASIONSDM_PROJECT_ROOT:?Set the repository root}"
[[ "$#" -eq 2 ]] || { echo 'Usage: sbatch submit_species.sh "Accepted taxon" VARIANT' >&2; exit 2; }
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1
cd "$INVASIONSDM_PROJECT_ROOT"
"${RSCRIPT:-Rscript}" --vanilla code/10_sensitivity/environment_only/fit_species.R "$1" "$2"
