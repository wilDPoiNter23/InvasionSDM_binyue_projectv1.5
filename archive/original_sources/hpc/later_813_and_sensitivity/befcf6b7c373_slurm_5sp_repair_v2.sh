# Original source: slurm_5sp_repair_v2.sh
# Original source location: ${WORK_ROOT}/slurm_5sp_repair_v2.sh
# Original source SHA-256: befcf6b7c373bb187222922b221fc57ad66f285d7b638acfacc9e737f70f8ead
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/bin/bash
# slurm_5sp_repair_v2.sh — 5-species negative-TSS repair pilot
# SLURM array: 1 job per species (5 species total)
# Usage: sbatch slurm_5sp_repair_v2.sh

#SBATCH --job-name=repair_v2
#SBATCH --output=<WORK_ROOT>/results/repair_pilot_v2/logs/slurm_%A_%a.out
#SBATCH --error=<WORK_ROOT>/results/repair_pilot_v2/logs/slurm_%A_%a.err
#SBATCH --array=1-5
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=32G
#SBATCH --time=12:00:00
#SBATCH --partition=thcp1

set -euo pipefail

# ── Environment ─────────────────────────────────────────────────────────────
source ~/.bashrc
module add loginnode
export TMPDIR="<WORK_ROOT>/results/repair_pilot_v2/tmp"

# ── Species map (SLURM array → species name) ────────────────────────────────
SPECIES_LIST=(
  "Pachira aquatica"         # 1: ranking-generalisation failure
  "Geranium dissectum"       # 2: threshold-transfer failure
  "Ailanthus altissima"      # 3: mixed failure
  "Tamarix aphylla"          # 4: complete-case collapse
  "Euphorbia terracina"      # 5: successful control
)

TASK_ID=$((SLURM_ARRAY_TASK_ID - 1))
SP_NAME="${SPECIES_LIST[$TASK_ID]}"

WORK_DIR="<WORK_ROOT>"
R_LIBS="<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu"

LOG_DIR="${WORK_DIR}/results/repair_pilot_v2/logs"
mkdir -p "$LOG_DIR" "$TMPDIR"

echo "============================================"
echo "Repair Pilot v2 — SLURM Array Job ${SLURM_ARRAY_TASK_ID}/5"
echo "Species: ${SP_NAME}"
echo "Node: $(hostname)"
echo "Started: $(date)"
echo "============================================"

export R_LIBS_USER="$R_LIBS"
export R_LIBS_SITE="$R_LIBS"
export R_LIBS="$R_LIBS"

Rscript --no-save "${WORK_DIR}/repair_pilot_modeling_v2.r" "${SP_NAME}"
RC=$?

echo "Exit code: ${RC}"
echo "Finished: $(date)"

exit $RC
