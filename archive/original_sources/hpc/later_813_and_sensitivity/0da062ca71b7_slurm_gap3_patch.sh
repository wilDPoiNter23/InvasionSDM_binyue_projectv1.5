# Original source: slurm_gap3_patch.sh
# Original source location: ${WORK_ROOT}/publication_release_v1/scripts/slurm_gap3_patch.sh
# Original source SHA-256: 0da062ca71b7cf3ef67e892473ae50b64868eb921da8dd4c0ed4a57d39e1f7dc
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/bin/bash
#SBATCH --job-name=gap3_patch
#SBATCH --partition=thcp1
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=64G
#SBATCH --time=12:00:00
#SBATCH --output=<WORK_ROOT>/logs/gap3_patch_%j.out
#SBATCH --error=<WORK_ROOT>/logs/gap3_patch_%j.err
source ~/.bashrc
module add loginnode
export R_LIBS_USER="<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu"
export R_LIBS_SITE="$R_LIBS_USER"
export R_LIBS="$R_LIBS_USER"
BASE="<WORK_ROOT>"
SCRIPT="${BASE}/publication_release_v1/scripts/repair_gap3_occpatch.r"
OUTDIR="${BASE}/results/gap13_model_repair_fullcoverage_v1"
mkdir -p ${BASE}/logs ${OUTDIR}

for sp in "Acacia stricta" "Melaleuca hypericifolia" "Soliva sessilis"; do
    echo "=== ${sp} ==="
    echo "Started: $(date)"
    if Rscript "$SCRIPT" "$sp"; then
        echo "SUCCESS: ${sp}"
    else
        echo "FAILED: ${sp}"
    fi
    echo "Finished: $(date)"
    echo ""
done
echo "=== Patch Done ==="
