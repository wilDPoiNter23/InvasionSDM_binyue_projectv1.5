# Original source: fig2_data_job.sh
# Original source location: ${NON_NATIVE_ROOT}/temp/fig2_data_job.sh
# Original source SHA-256: 8e413b25c7a948b6fd67afe4e4d7ab9d24917ba730c76858e1adc744a2faa972
# Recovery: live read-only SSH, 2026-10-12
# Classification: SUPPORTING (HISTORICAL_SUPPORTING_AUDIT_LINEAGE_HELPER)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/bin/bash
#SBATCH --job-name=fig2data
#SBATCH --partition=thcp1
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=16G
#SBATCH --time=02:00:00
#SBATCH --output=<NON_NATIVE_ROOT>/temp/fig2_%j.log

source ~/.bashrc
module add loginnode
export R_LIBS_USER=<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu
Rscript <NON_NATIVE_ROOT>/temp/figure2_data_job.R
