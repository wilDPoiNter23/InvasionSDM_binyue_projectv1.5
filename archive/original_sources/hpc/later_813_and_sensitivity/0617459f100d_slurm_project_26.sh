# Original source: slurm_project_26.sh
# Original source location: ${WORK_ROOT}/slurm_project_26.sh
# Original source SHA-256: 0617459f100dbc17a8ec06d4079b2a4ccf4c621f7dd6202d91fdab8f46fd465c
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/bin/bash
#SBATCH --job-name=proj_26
#SBATCH --output=<WORK_ROOT>/results/targeted_negative_TSS_projection_multifold_v2/logs/slurm_%A_%a.out
#SBATCH --error=<WORK_ROOT>/results/targeted_negative_TSS_projection_multifold_v2/logs/slurm_%A_%a.err
#SBATCH --array=1-26 --ntasks=1 --cpus-per-task=4 --mem=32G --time=12:00:00 --partition=thcp1
set -euo pipefail
source ~/.bashrc; module add loginnode
export R_LIBS_USER="<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu"
export R_LIBS_SITE="$R_LIBS_USER"; export R_LIBS="$R_LIBS_USER"
SP=( "Pachira aquatica" "Geranium dissectum" "Ailanthus altissima" "Tamarix aphylla" "Euphorbia terracina" "Ageratum houstonianum" "Amaranthus polygonoides" "Asystasia gangetica" "Cenchrus echinatus" "Dracaena trifasciata" "Glebionis coronaria" "Harrisia tortuosa" "Lepidium latifolium" "Manihot esculenta" "Melinis minutiflora" "Pluchea carolinensis" "Pseudelephantopus spicatus" "Urochloa dictyoneura" "Veronica arvensis" "Adiantum latifolium" "Hibiscus trionum" "Urochloa eminii" "Musa ornata" "Vicia eriocarpa" "Cinnamomum camphora" "Croton setiger" )
SN="${SP[$((SLURM_ARRAY_TASK_ID-1))]}"
echo "=== Project ${SLURM_ARRAY_TASK_ID}/26: ${SN} ==="
Rscript --no-save <WORK_ROOT>/project_26_repaired.r "${SN}"
echo "Exit: $?"
