# Original source: simple_batch.sh
# Original source location: <LOCAL_CODE_ROOT>/simple_batch.sh
# Original source SHA-256: 3a034dd6fbf055b8c983af79d76e3a8c4f3e548f5b5b08761349cb820d01a52b
# Public-safe original snapshot: archive/original_sources/local/simple_batch.sh
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
#SBATCH --job-name=simple_batch
#SBATCH --array=1-30           # 20个数组任务
#SBATCH --output=simple_batch.out  # 标准输出文件
#SBATCH --error=simple_batch.err   # 错误输出文件
#SBATCH --ntasks=1                         # 任务数
#SBATCH --cpus-per-task=62               # 使用的核心数量
#SBATCH --time=10-00:00:00           # 最大运行时间 


source ~/.bashrc
cd <HPC_INPUT_ROOT>/script
# module av
module add loginnode

BATCH_ID=$SLURM_ARRAY_TASK_ID
# module list
echo "开始处理批次 $BATCH_ID"

Rscript 0402simple_batch.r $BATCH_ID

echo "批次 $BATCH_ID 完成"
echo "Finished at $(date)"
duration=$(($(date +"%s")-from))
echo "$(((duration)/3600))h $(((duration)%3600/60))m $(((duration)%60))s is used."

echo "CPUs per task: $SLURM_CPUS_PER_TASK"
echo "Mem per node:  $SLURM_MEM_PER_NODE"
echo "Mem per CPU:   $SLURM_MEM_PER_CPU"
echo "Memory info:"
free -h
