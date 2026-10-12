# Original source: slurm_glebionis_retry.sh
# Original source location: ${WORK_ROOT}/slurm_glebionis_retry.sh
# Original source SHA-256: a10ddf4c5d1c91eec5ddabcb8fbce264a0a0c0943cf07d09637d9d77cbc01024
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/bin/bash
#SBATCH --job-name=gleb_retry
#SBATCH --output=results/targeted_negative_TSS_projection_multifold_v4/glebionis_retry_%j.out
#SBATCH --error=results/targeted_negative_TSS_projection_multifold_v4/glebionis_retry_%j.err
#SBATCH --ntasks=1 --cpus-per-task=1 --mem=96G --time=6:00:00 --partition=thcp1
set -euo pipefail
source ~/.bashrc; module add loginnode
export R_LIBS_USER="<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu"
export R_LIBS_SITE="$R_LIBS_USER"; export R_LIBS="$R_LIBS_USER"
Rscript --no-save <WORK_ROOT>/glebionis_ensemble_retry.r
echo "Exit: $?"
