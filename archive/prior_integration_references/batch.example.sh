# ARCHIVED PRIOR TEMPLATE: target script moved to prior_integration_references; not an approved submission entrypoint.
#!/usr/bin/env bash
#SBATCH --job-name=invasion-sdm
#SBATCH --cpus-per-task=2
#SBATCH --mem=16G
#SBATCH --time=24:00:00
#SBATCH --output=logs/sdm_%A_%a.log
# Add your account/partition directives yourself; no institutional values supplied.
# Array bounds must be calculated from your species list and preserved batch size.
# mkdir -p logs before sbatch; this template does not submit jobs itself.
set -euo pipefail
: "${INVASIONSDM_PROJECT_ROOT:?Set repository root}"
: "${INVASIONSDM_CONFIG:?Set private config file}"
batch_id="${SLURM_ARRAY_TASK_ID:-${1:-}}"
[[ "$batch_id" =~ ^[1-9][0-9]*$ ]] || { echo 'Positive batch ID required' >&2; exit 2; }
cd "$INVASIONSDM_PROJECT_ROOT"
Rscript code/04_sdm/hpc/fit_and_project_batch.R "$batch_id"
