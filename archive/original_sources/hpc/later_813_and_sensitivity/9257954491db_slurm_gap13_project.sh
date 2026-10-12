# Original source: slurm_gap13_project.sh
# Original source location: ${WORK_ROOT}/publication_release_v1/scripts/slurm_gap13_project.sh
# Original source SHA-256: 9257954491db10b034b9f1794366716792993636028370e8dc924abba437850a
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/bin/bash
#SBATCH --job-name=gap13_proj
#SBATCH --partition=thcp1
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=64G
#SBATCH --time=24:00:00
#SBATCH --output=<WORK_ROOT>/logs/gap13_proj_%j.out
#SBATCH --error=<WORK_ROOT>/logs/gap13_proj_%j.err
source ~/.bashrc
module add loginnode
export R_LIBS_USER="<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu"
export R_LIBS_SITE="$R_LIBS_USER"
export R_LIBS="$R_LIBS_USER"

BASE="<WORK_ROOT>"
SCRIPT="${BASE}/publication_release_v1/scripts/project_gap13_repaired.r"
MANIFEST="${BASE}/publication_release_v1/manifests/gap13_repair_projection_selection_final_v2.csv"
mkdir -p ${BASE}/logs

total=0; success=0; failed=0
while IFS= read -r line; do
    [[ "$line" == '"species'* || "$line" == species* ]] && continue
    sp=$(echo "$line" | cut -d',' -f1 | tr -d '"')
    [[ -z "$sp" ]] && continue
    total=$((total+1))
    echo "=== [${total}/13] ${sp} ==="
    echo "Started: $(date)"
    if Rscript "$SCRIPT" "$sp"; then
        success=$((success+1))
        echo "SUCCESS: ${sp} (${success}/${total})"
    else
        failed=$((failed+1))
        echo "FAILED: ${sp}"
    fi
    echo "Finished: $(date)"
    echo ""
done < <(tail -n +2 "$MANIFEST")
echo "=== Projections: ${success} success, ${failed} failed ==="
