# Original source: fig2b_data_job.sh
# Original source location: ${NON_NATIVE_ROOT}/temp/fig2b_data_job.sh
# Original source SHA-256: 6fb567ea2ee3661731af64ff2caa2e11f6b0429aa51cf2be2801a376de850bc3
# Recovery: live read-only SSH, 2026-10-12
# Classification: SUPPORTING (HISTORICAL_SUPPORTING_AUDIT_LINEAGE_HELPER)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/bin/bash
#SBATCH --job-name=fig2bdata
#SBATCH --partition=thcp1
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=16G
#SBATCH --time=02:00:00
#SBATCH --output=<NON_NATIVE_ROOT>/temp/fig2b_%j.log

source ~/.bashrc
module add loginnode
export R_LIBS_USER=<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu
Rscript <NON_NATIVE_ROOT>/temp/figure2b_data_job.R
