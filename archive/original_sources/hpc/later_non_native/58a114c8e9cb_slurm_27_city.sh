# Original source: slurm_27_city.sh
# Original source location: ${NON_NATIVE_ROOT}/02_derived_non_native_potential/07_scripts/slurm_27_city.sh
# Original source SHA-256: 58a114c8e9cbaf88c08dea3930a9b9e6bbaec72e0fc95a89e682aa4bdd1fe842
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_NON_NATIVE)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/bin/bash
#SBATCH --job-name=city108
#SBATCH --partition=thcp1
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=2
#SBATCH --mem=16G
#SBATCH --time=01:00:00
#SBATCH --output=<NON_NATIVE_ROOT>/02_derived_non_native_potential/08_logs/27_city_%j.out
#SBATCH --error=<NON_NATIVE_ROOT>/02_derived_non_native_potential/08_logs/27_city_%j.err
source ~/.bashrc
module add loginnode
export R_LIBS_USER="<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu"
cd <NON_NATIVE_ROOT>/02_derived_non_native_potential/07_scripts
Rscript 27_city_risk_check.R
