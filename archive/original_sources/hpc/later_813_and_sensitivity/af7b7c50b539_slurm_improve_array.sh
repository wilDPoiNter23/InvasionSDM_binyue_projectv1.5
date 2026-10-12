# Original source: slurm_improve_array.sh
# Original source location: ${WORK_ROOT}/archive/slurm_old/slurm_improve_array.sh
# Original source SHA-256: af7b7c50b53959b177b9ae317cad89960db74f62c2b9fab415bdf6f4dfc3d7de
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/bin/bash
#SBATCH --job-name=improve
#SBATCH --array=1-3
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=2
#SBATCH --mem=32G
#SBATCH --time=1:00:00
#SBATCH --partition=thcp1
#SBATCH --output=logs/improve_%A_%a.out

source ~/.bashrc
module add loginnode

cd <WORK_ROOT>
SP=$(sed -n "${SLURM_ARRAY_TASK_ID}p" species_improve_priority.txt)
echo "Task ${SLURM_ARRAY_TASK_ID}: ${SP}"
Rscript run_improve_one_species.R "${SP}"
