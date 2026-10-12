# Original source: slurm_agg_813.sh
# Original source location: ${WORK_ROOT}/archive/slurm_old/slurm_agg_813.sh
# Original source SHA-256: 6758b8c299ee02da7cefe07a307c8b8d1c039135488d37f66363fff8dea007f6
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/bin/bash
#SBATCH --job-name=agg_813
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8
#SBATCH --mem=64G
#SBATCH --time=12:00:00
#SBATCH --partition=thcp1
#SBATCH --output=logs/agg_813_%j.out

source ~/.bashrc
module add loginnode

# Backup existing outputs
cd <WORK_ROOT>/results
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
mkdir -p aggregation_backup_${TIMESTAMP}

for f in hotspot_mean_suitability.tif hotspot_mean_suitability_normalized2.0.tif richness_binary_map.tif continent_risk_tifs/; do
    if [ -e "$f" ]; then
        cp -r "$f" aggregation_backup_${TIMESTAMP}/
        echo "Backed up: $f"
    fi
done

# Run aggregation
echo "=== Starting aggregation with 813-species CSV ==="
echo "CSV rows: $(wc -l < <WORK_ROOT>/results/evaluations/projection_summary.csv)"
echo "Time: $(date)"

Rscript <WORK_ROOT>/run_aggregation_813.R

echo "=== Aggregation complete ==="
echo "Time: $(date)"
echo "Output:"
ls -la <WORK_ROOT>/results/hotspot_mean_suitability*.tif 2>/dev/null
