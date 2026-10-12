# Original source: slurm_repair_gap13.sh
# Original source location: ${WORK_ROOT}/publication_release_v1/scripts/slurm_repair_gap13.sh
# Original source SHA-256: ff187a9399d609b74b7215497eec74f44623682e282d5ccfed1e23679fc7ffa7
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/bin/bash
#SBATCH --job-name=gap13_repair
#SBATCH --partition=thcp1
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=64G
#SBATCH --time=24:00:00
#SBATCH --output=<WORK_ROOT>/logs/gap13_repair_%j.out
#SBATCH --error=<WORK_ROOT>/logs/gap13_repair_%j.err
source ~/.bashrc
module add loginnode
export R_LIBS_USER="<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu"
export R_LIBS_SITE="$R_LIBS_USER"
export R_LIBS="$R_LIBS_USER"
BASE="<WORK_ROOT>"
SCRIPT="${BASE}/publication_release_v1/scripts/repair_gap13_v2.r"
OUTDIR="${BASE}/results/gap13_model_repair_fullcoverage_v1"
MANIFEST="${BASE}/publication_release_v1/manifests/gap13_model_repair_manifest_v1.csv"
mkdir -p ${BASE}/logs ${OUTDIR}
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
done < "$MANIFEST"
echo "=== Repair Done: ${success} success, ${failed} failed, ${total} total ==="
