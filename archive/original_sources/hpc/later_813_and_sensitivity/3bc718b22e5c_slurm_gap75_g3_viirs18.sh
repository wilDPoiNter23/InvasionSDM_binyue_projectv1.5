# Original source: slurm_gap75_g3_viirs18.sh
# Original source location: ${WORK_ROOT}/publication_release_v1/scripts/slurm_gap75_g3_viirs18.sh
# Original source SHA-256: 3bc718b22e5cbd6c73ec27a4383e0cea930b41666a2c4f875e21504eb4abec76
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/bin/bash
#SBATCH --job-name=gap75_g3_v18
#SBATCH --partition=thcp1
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=64G
#SBATCH --time=24:00:00
#SBATCH --output=<WORK_ROOT>/logs/gap75_g3_viirs18_%j.out
#SBATCH --error=<WORK_ROOT>/logs/gap75_g3_viirs18_%j.err
source ~/.bashrc
module add loginnode
export R_LIBS_USER="<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu"
export R_LIBS_SITE="$R_LIBS_USER"
export R_LIBS="$R_LIBS_USER"
BASE="<WORK_ROOT>"
STACK="${BASE}/data/predictors/full_stack_terrestrial_plus_VIIRS_v1.tif"
SCRIPT="${BASE}/publication_release_v1/scripts/project_gap75_final.r"
GROUP="g3_viirs18"
OUTDIR="${BASE}/results/gap75_publication_${GROUP}"
mkdir -p ${BASE}/logs ${OUTDIR}
SPECIES_CSV="${BASE}/publication_release_v1/audits/projection_group3_viirs.csv"
total=0; success=0; failed=0
while IFS= read -r line; do
    [[ "$line" == species* ]] && continue
    sp=$(echo "$line" | cut -d',' -f1 | tr -d '"')
    [[ -z "$sp" ]] && continue
    total=$((total+1))
    echo "=== [${total}/18] ${sp} ==="
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
echo "=== Group3 Done: ${success} success, ${failed} failed, ${total} total ==="
