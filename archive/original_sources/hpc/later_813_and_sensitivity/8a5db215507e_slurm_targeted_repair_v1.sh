# Original source: slurm_targeted_repair_v1.sh
# Original source location: ${WORK_ROOT}/slurm_targeted_repair_v1.sh
# Original source SHA-256: 8a5db215507e897664ed2fa75687f79bfc7cf19af98ea0b08ee1f46662535e38
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/bin/bash
# slurm_targeted_repair_v1.sh — Targeted repair of 22 negative-TSS / collapse species
# SLURM array: 1 job per species (22 species total)
# Usage: sbatch slurm_targeted_repair_v1.sh

#SBATCH --job-name=target_rep
#SBATCH --output=<WORK_ROOT>/results/targeted_negative_TSS_repair_v1/logs/slurm_%A_%a.out
#SBATCH --error=<WORK_ROOT>/results/targeted_negative_TSS_repair_v1/logs/slurm_%A_%a.err
#SBATCH --array=1-22
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=32G
#SBATCH --time=12:00:00
#SBATCH --partition=thcp1

set -euo pipefail

source ~/.bashrc
module add loginnode
export TMPDIR="<WORK_ROOT>/results/targeted_negative_TSS_repair_v1/tmp"

WORK_DIR="<WORK_ROOT>"
R_LIBS="<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu"

# ── Species list (must match audit/targeted_repair_queue_preflight.csv PASS rows) ──
SPECIES_LIST=(
  "Ageratum houstonianum"
  "Amaranthus polygonoides"
  "Asystasia gangetica"
  "Cenchrus echinatus"
  "Cinnamomum camphora"
  "Croton setiger"
  "Dracaena trifasciata"
  "Glebionis coronaria"
  "Harrisia tortuosa"
  "Lepidium latifolium"
  "Manihot esculenta"
  "Melinis minutiflora"
  "Pluchea carolinensis"
  "Pseudelephantopus spicatus"
  "Urochloa dictyoneura"
  "Veronica arvensis"
  "Adiantum latifolium"
  "Diodia teres"
  "Hibiscus trionum"
  "Urochloa eminii"
  "Musa ornata"
  "Vicia eriocarpa"
)

TASK_ID=$((SLURM_ARRAY_TASK_ID - 1))
SP_NAME="${SPECIES_LIST[$TASK_ID]}"

LOG_DIR="${WORK_DIR}/results/targeted_negative_TSS_repair_v1/logs"
OUT_DIR="${WORK_DIR}/results/targeted_negative_TSS_repair_v1"
mkdir -p "$LOG_DIR" "$OUT_DIR/tmp"

echo "============================================"
echo "Targeted Repair v1 — SLURM Array Job ${SLURM_ARRAY_TASK_ID}/22"
echo "Species: ${SP_NAME}"
echo "Node: $(hostname)"
echo "Started: $(date)"
echo "============================================"

export R_LIBS_USER="$R_LIBS"
export R_LIBS_SITE="$R_LIBS"
export R_LIBS="$R_LIBS"

Rscript --no-save "${WORK_DIR}/repair_targeted_modeling_v1.r" "${SP_NAME}"
RC=$?

echo "Exit code: ${RC}"
echo "Finished: $(date)"
exit $RC
