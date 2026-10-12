# Original source: slurm_gap75_g1_direct8.sh
# Original source location: ${WORK_ROOT}/publication_release_v1/scripts/slurm_gap75_g1_direct8.sh
# Original source SHA-256: ebc5ff0b17cfa17c15968a8ccefd46041e92d4308a4bef51067354916d553012
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/bin/bash
#SBATCH --job-name=gap75_g1_d8
#SBATCH --partition=thcp1
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=64G
#SBATCH --time=12:00:00
#SBATCH --output=<WORK_ROOT>/logs/gap75_g1_direct8_%j.out
#SBATCH --error=<WORK_ROOT>/logs/gap75_g1_direct8_%j.err

# Group 1: 8 DIRECT_34STACK species on 34-layer stack
# Each species ~30-60 min disk-backed, 8 species ~6-10 hours
source ~/.bashrc
module add loginnode

export R_LIBS_USER="<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu"
export R_LIBS_SITE="$R_LIBS_USER"
export R_LIBS="$R_LIBS_USER"

BASE="<WORK_ROOT>"
STACK="${BASE}/data/predictors/full_stack_terrestrial_primary_v1.tif"
SCRIPT="${BASE}/publication_release_v1/scripts/project_gap75_final.r"
GROUP="g1_direct8"
OUTDIR="${BASE}/results/gap75_publication_${GROUP}"

mkdir -p ${BASE}/logs
mkdir -p ${OUTDIR}

SPECIES_CSV="${BASE}/publication_release_v1/audits/projection_group1_direct8.csv"

total=0; success=0; failed=0

while IFS= read -r line; do
    [[ "$line" == species* ]] && continue
    sp=$(echo "$line" | cut -d',' -f1 | tr -d '"')
    [[ -z "$sp" ]] && continue
    
    total=$((total+1))
    echo "=== [${total}/8] ${sp} ==="
    echo "Started: $(date)"
    
    if Rscript "$SCRIPT" "$sp" "$STACK" "$GROUP" "$OUTDIR"; then
        success=$((success+1))
        echo "SUCCESS: ${sp} (${success}/${total})"
    else
        failed=$((failed+1))
        echo "FAILED: ${sp}"
    fi
    echo "Finished: $(date)"
    echo ""
done < "$SPECIES_CSV"

echo "=== Group1 Done: ${success} success, ${failed} failed, ${total} total ==="
