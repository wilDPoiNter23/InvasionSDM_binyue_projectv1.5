# Original source: slurm_soliva.sh
# Original source location: ${WORK_ROOT}/archive/slurm_old/slurm_soliva.sh
# Original source SHA-256: 589c832494c3ab21799eeb5b9207f91c4dd6327a4b99dac112af24c68717b077
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/bin/bash
#SBATCH --job-name=soliva
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=62
#SBATCH --mem=120G
#SBATCH --time=2:00:00
#SBATCH --partition=thcp1
#SBATCH --output=logs/soliva_%j.out

source ~/.bashrc
module add loginnode

Rscript proper_ensemble_project.r "Soliva sessilis"
