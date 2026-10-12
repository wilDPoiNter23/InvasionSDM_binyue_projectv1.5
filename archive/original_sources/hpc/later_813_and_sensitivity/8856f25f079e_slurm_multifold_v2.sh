# Original source: slurm_multifold_v2.sh
# Original source location: ${WORK_ROOT}/slurm_multifold_v2.sh
# Original source SHA-256: 8856f25f079ef57c62b90d01b47c7830a7493fbdedbe31a343e2212d1a37280e
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/bin/bash
# slurm_multifold_v2.sh — 26-species multifold CV repair
# 24 primary (BIOMOD2 block4) + 2 fallback (kfold3)
#SBATCH --job-name=multifold_v2
#SBATCH --output=<WORK_ROOT>/results/targeted_negative_TSS_repair_multifold_v2/logs/slurm_%A_%a.out
#SBATCH --error=<WORK_ROOT>/results/targeted_negative_TSS_repair_multifold_v2/logs/slurm_%A_%a.err
#SBATCH --array=1-26
#SBATCH --ntasks=1 --cpus-per-task=4 --mem=32G --time=12:00:00 --partition=thcp1

set -euo pipefail
source ~/.bashrc; module add loginnode

export R_LIBS_USER="<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu"
export R_LIBS_SITE="$R_LIBS_USER"; export R_LIBS="$R_LIBS_USER"
WORK_DIR="<WORK_ROOT>"
mkdir -p "${WORK_DIR}/results/targeted_negative_TSS_repair_multifold_v2/logs"

# 26 species: 24 primary + 2 kfold fallback
SPECIES_LIST=(
  "Pachira aquatica"          # 1
  "Geranium dissectum"        # 2
  "Ailanthus altissima"       # 3
  "Tamarix aphylla"           # 4
  "Euphorbia terracina"       # 5
  "Ageratum houstonianum"     # 6
  "Amaranthus polygonoides"   # 7
  "Asystasia gangetica"       # 8
  "Cenchrus echinatus"        # 9
  "Dracaena trifasciata"      # 10
  "Glebionis coronaria"       # 11
  "Harrisia tortuosa"         # 12
  "Lepidium latifolium"       # 13
  "Manihot esculenta"         # 14
  "Melinis minutiflora"       # 15
  "Pluchea carolinensis"      # 16
  "Pseudelephantopus spicatus" # 17
  "Urochloa dictyoneura"      # 18
  "Veronica arvensis"         # 19
  "Adiantum latifolium"       # 20
  "Hibiscus trionum"          # 21
  "Urochloa eminii"           # 22
  "Musa ornata"               # 23
  "Vicia eriocarpa"           # 24
  "Cinnamomum camphora"       # 25 kfold
  "Croton setiger"            # 26 kfold
)

TASK_ID=$((SLURM_ARRAY_TASK_ID - 1))
SP_NAME="${SPECIES_LIST[$TASK_ID]}"

echo "=== Multifold v2: ${SP_NAME} (task ${SLURM_ARRAY_TASK_ID}/26) ==="
echo "Node: $(hostname) | Started: $(date)"

Rscript --no-save "${WORK_DIR}/multifold_repair_v2.r" "${SP_NAME}"
RC=$?
echo "Exit: ${RC} | Finished: $(date)"
exit $RC
