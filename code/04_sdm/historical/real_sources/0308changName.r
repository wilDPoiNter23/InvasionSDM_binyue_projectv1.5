# Original source: 0308changName.r
# Original source location: <LOCAL_CODE_ROOT>/0308changName.r
# Original source SHA-256: 2f2cb4a838f25244ed829f91014759f00e15c6bed4fd61084e9a02cfc0664d80
# Public-safe original snapshot: archive/original_sources/local/0308changName.r
# Classification: EXPLORATORY
# Publication modifications:
# - machine-specific absolute path literals replaced by configured publication_path aliases
# - local encoding decoded to UTF-8; source bytes hashed above
# - comments and historical private machine paths sanitized
# - working-directory calls retained with configured paths; no analyses executed
# Scientific modification: NONE
# This is full historical source, not a certified unattended/reproducible run.
if (!exists("publication_path", mode="function")) stop("Load config/bootstrap.R first")
library(stringr)

# 设置文件路径
file_path <- publication_path("WORK_ROOT", "results/projections")

# 获取目录下所有.rds文件
file_list <- list.files(path = file_path, 
                       pattern = "\\.rds$", 
                       full.names = FALSE)

# 创建空的数据框来存储结果
results <- data.frame(
  species = character(),
  type = character(),
  stringsAsFactors = FALSE
)

# 遍历每个文件名并提取信息
for (file_name in file_list) {
  # 移除.rds扩展名
  base_name <- gsub("\\.rds$", "", file_name)
  
  # 处理文件名格式：Genus species_type_projection
  # 例如：Rhaphiolepis bibas_ensemble_projection.rds
  # 或者：Typha domingensis_fallback_projection.rds
  
  # 使用正则表达式分离物种名和类型
  # 匹配格式：前面是物种名（可能包含空格），后面是_类型_projection
  pattern <- "^(.+?)_(ensemble|fallback)_projection$"
  
  if (grepl(pattern, base_name)) {
    # 提取匹配的部分
    matches <- regmatches(base_name, regexec(pattern, base_name))[[1]]
    
    if (length(matches) >= 3) {
      species_part <- matches[2]  # 物种名部分
      type <- matches[3]          # 类型部分
      
      # 处理物种名：将下划线替换为空格，只保留前两个词
      species_words <- str_split(species_part, "[_\\s]+")[[1]]
      
      if (length(species_words) >= 2) {
        species_name <- paste(species_words[1], species_words[2], sep = " ")
        
        # 添加到结果数据框
        results <- rbind(results, data.frame(
          species = species_name,
          type = type,
          stringsAsFactors = FALSE
        ))
      }
    }
  } else {
    # 如果不匹配标准格式，输出警告信息
    cat("警告：文件名格式不符合预期:", file_name, "\n")
  }
}

# 移除重复行
results <- unique(results)

# 按物种名排序
results <- results[order(results$species), ]

# 重置行名
rownames(results) <- NULL

# 显示结果
print("提取的物种信息：")
print(results)

# 保存为CSV文件
output_file <- publication_path("WORK_ROOT", "results/evaluations/projection_summary.csv")
write.csv(results, file = output_file, row.names = FALSE, quote = FALSE)

cat("结果已保存到:", output_file, "\n")
cat("总共处理了", nrow(results), "个物种\n")

# 显示类型统计
cat("\n类型统计：\n")
print(table(results$type))
