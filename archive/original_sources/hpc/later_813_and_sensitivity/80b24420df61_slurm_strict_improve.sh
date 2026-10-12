# Original source: slurm_strict_improve.sh
# Original source location: ${WORK_ROOT}/archive/slurm_old/slurm_strict_improve.sh
# Original source SHA-256: 80b24420df6126f0fb4d9f621de5ebad0b0479156ac15cbbcee07184b2c9e8d5
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/bin/bash
#SBATCH --job-name=strict
#SBATCH --array=1-2
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=2
#SBATCH --mem=32G
#SBATCH --time=0:30:00
#SBATCH --partition=thcp1
#SBATCH --output=logs/strict_%A_%a.out
source ~/.bashrc
module add loginnode
cd <WORK_ROOT>
SP=$(sed -n "${SLURM_ARRAY_TASK_ID}p" species_strict_improve.txt)
echo "Task ${SLURM_ARRAY_TASK_ID}: ${SP}"
Rscript run_improve_one_species.R "${SP}"
