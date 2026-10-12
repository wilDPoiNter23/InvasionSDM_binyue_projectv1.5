# Original source: simpleversion.sh
# Original source location: <LOCAL_CODE_ROOT>/simpleversion.sh
# Original source SHA-256: ea530f327e0dca5b949eca3df091e8ace5fab3cfd1aa33a0e954932ce5606062
# Public-safe original snapshot: archive/original_sources/local/simpleversion.sh
# Classification: EXPLORATORY
# Publication modifications:
# - machine-specific absolute path literals replaced by configured publication_path aliases
# - local encoding decoded to UTF-8; source bytes hashed above
# - comments and historical private machine paths sanitized
# - working-directory calls retained with configured paths; no analyses executed
# Scientific modification: NONE
# This is full historical source, not a certified unattended/reproducible run.
# Snapshot for inspection; path placeholders are not executable locations.
#!/bin/bash
#SBATCH --job-name=040601Optimized_linux_response_curves_job    # 作业名称
#SBATCH --output=040601Optimized_linux_response_curves.out  # 标准输出文件
#SBATCH --error=040601Optimized_linux_response_curves.err   # 错误输出文件
#SBATCH --ntasks=1                         # 任务数
#SBATCH --cpus-per-task=62             # 使用的核心数量
#SBATCH --time=10-00:00:00           # 最大运行时间 

source ~/.bashrc
cd <HPC_INPUT_ROOT>/script
# module av
module add loginnode

# module list

Rscript 040601Optimized_linux_response_curves.r



end_time=$(date +%s)
echo "Job $SLURM_JOB_ID ended at $(date)" >>  040601Optimized_linux_response_curves.out

runtime=$((end_time - start_time))
echo "Total execution time: $runtime seconds" >> 040601Optimized_linux_response_curves.out