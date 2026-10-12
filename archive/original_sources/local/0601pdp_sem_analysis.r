# Original source: 0601pdp_sem_analysis.r
# Original source location: <LOCAL_CODE_ROOT>/0601pdp_sem_analysis.r
# Original source SHA-256: eefccfbc00f5441e550463788d29aa1c93bc2f0517a4f24012c0cd60cd92f60d
# Public-safe original snapshot: archive/original_sources/local/0601pdp_sem_analysis.r
# Classification: EXPLORATORY
# Publication modifications:
# - machine-specific absolute path literals replaced by configured publication_path aliases
# - local encoding decoded to UTF-8; source bytes hashed above
# - comments and historical private machine paths sanitized
# - working-directory calls retained with configured paths; no analyses executed
# Scientific modification: NONE
# This is full historical source, not a certified unattended/reproducible run.
# Snapshot for inspection; path placeholders are not executable locations.
library(randomForest)
library(terra)
library(ranger)
library(pdp)
library(partykit)
library(data.table)
library(xgboost)
library(pdp)
library(DALEX)
library(patchwork)
library(ggplot2)
library(lavaan)
library(semPlot)
library(mgcv)
library(piecewiseSEM)
library(sf)
library(dplyr)
library(gratia)
library(ggeffects)

.libPaths(c("<HISTORICAL_PATH_006>", .libPaths()))
unlink("<HISTORICAL_PATH_007>", recursive = TRUE)
install.packages("scales", lib = "<HISTORICAL_PATH_006>", dependencies = TRUE)
find.package("ggplot2")
find.package("scales")
packageVersion("ggplot2")
packageVersion("scales")
remove.packages("scales", lib = "<HISTORICAL_PATH_006>")
install.packages("scales", lib = "<HISTORICAL_PATH_006>", type = "binary")
install.packages("httpgd")
library(scales, lib.loc = "<HISTORICAL_PATH_006>")

#==== 1. 加载响应变量（richness / risk map）=======
r <- terra::rast("<LOCAL_DATABASE_ROOT>/evaluations/hotspot_mean_suitability_normalized2.0.tif")  # or .tif
reclamation_city_shp <- st_read("<LOCAL_ANALYSIS_ROOT>/data/data/reclaimed43cities.shp")

vars_to_remove <- c(
  "Pop2020_2.5min",
  "temperature_mean",
  "wc2.1_2.5m_bio_11",
  "wc2.1_2.5m_bio_16",
  "wc2.1_2.5m_bio_17",
  "wc2.1_2.5m_bio_4",
  "wc2.1_2.5m_bio_5",
  "wc2.1_2.5m_bio_6",
  "wc2.1_2.5m_bio_7",
  "wc2.1_2.5m_bio_9",
  "wc2.1_2.5m_elev"
)
city_mean_table <- fread("<LOCAL_ANALYSIS_ROOT>/data/10_01city_variable_means_updated.csv")

#====== 2. 加载解释变量（多层因子栅格）=======
factor_stack <- rast("<HISTORICAL_PATH_008>")

full_stack <- c(r, factor_stack)

names(full_stack)

names(full_stack)[1] <- "risk"

city_sf <- st_make_valid(reclamation_city_shp)
city_vect <- vect(city_sf)

city_vect <- project(city_vect, full_stack)
names(city_vect)

crs(city_vect)
crs(full_stack)

cities <- unique(city_vect$shapeName)
cities
#======= 3. 循环每个城市区域 =======
all_samples <- list()
cities <- unique(city_vect$shapeName)

# 创建进度追踪
start_time <- Sys.time()
failed_cities <- character(0)
success_count <- 0
sample_df
 city_name <- cities[1]

 one_city <- city_vect[city_vect$shapeName == city_name, ]
 one_city
 plot(one_city)
 city_stack <- tryCatch({
    # 首先裁剪，然后遮罩
    cropped <- crop(full_stack, one_city)
    cropped
  }, error = function(e) {
    cat("❌ Failed to crop/mask for", city_name, ":", e$message, "\n")
    return(NULL)
  })

  cropped <- crop(full_stack, one_city)
  plot(cropped) 
  print(cropped)
  n_valid <- global(!is.na(cropped[[1]]), "sum", na.rm = TRUE)
print(n_valid)
all_vals <- terra::values(cropped)

# 只保留 risk 不为 NA 的行
non_na_idx <- !is.na(all_vals[, "risk"])
filtered_vals <- all_vals[non_na_idx, ]

# 提取坐标
coords <- as.data.frame(xyFromCell(cropped, which(non_na_idx)))
sample_df <- cbind(coords, filtered_vals)
sample_df

sample_df <- sample_df[, colSums(!is.na(sample_df)) > 0]
sample_df
names(sample_df)
sample_df[3]


# 改进的城市栅格数据提取循环
for (i in seq_along(cities)) {
  city_name <- cities[i]
  cat(sprintf("[%02d/%02d] 🚧 Processing: %s\n", i, length(cities), city_name))
  
  # 提取当前城市 polygon
  one_city <- city_vect[city_vect$shapeName == city_name, ]
  
  if (nrow(one_city) == 0) {
    cat("⚠️ No geometry found for city:", city_name, "\n")
    failed_cities <- c(failed_cities, city_name)
    next
  }
  
  # 裁剪 & 遮罩 - 修复：添加遮罩操作
  city_stack <- tryCatch({
    # 首先裁剪，然后遮罩
    cropped <- crop(full_stack, one_city)
    cropped 
  }, error = function(e) {
    cat("❌ Failed to crop/mask for", city_name, ":", e$message, "\n")
    return(NULL)
  })
  
  if (is.null(city_stack)) {
    failed_cities <- c(failed_cities, city_name)
    next
  }
  
  # 检查栅格数据是否有效
  if (all(is.na(values(city_stack[[1]], na.rm = FALSE)))) {
    cat("⚠️ All values are NA for", city_name, "\n")
    failed_cities <- c(failed_cities, city_name)
    next
  }
  
  # 检查有效像元数量和数据质量
  n_cell <- tryCatch({
    # 修复：使用更稳健的计数方法
    sum(!is.na(values(city_stack[[1]])))
  }, error = function(e) {
    cat("⚠️ Error counting cells for", city_name, ":", e$message, "\n")
    return(0)
  })
  
  if (is.na(n_cell) || n_cell < 10) {
    cat("⚠️ Too few non-NA pixels (", n_cell, ") for:", city_name, "\n")
    failed_cities <- c(failed_cities, city_name)
    next
  }
  
  # 额外的数据质量检查
  cat("🔍 Checking data quality for", city_name, "...\n")
  value_range <- tryCatch({
    range(values(city_stack), na.rm = TRUE)
  }, error = function(e) {
    cat("⚠️ Error checking value range:", e$message, "\n")
    return(c(NA, NA))
  })
  
  if (any(is.na(value_range))) {
    cat("⚠️ Cannot determine value range for", city_name, "\n")
  } else {
    cat("📊 Value range:", round(value_range[1], 4), "to", round(value_range[2], 4), "\n")
  }
  
  # 检查是否所有值都相同（这会导致采样问题）
  unique_values <- tryCatch({
    unique_vals <- unique(values(city_stack[[1]]), na.rm = TRUE)
    if (length(unique_vals) > 100) unique_vals[1:100] else unique_vals  # 限制检查数量
  }, error = function(e) {
    return(NULL)
  })
  
  if (!is.null(unique_values) && length(unique_values) <= 1) {
    cat("⚠️ All pixels have the same value (", unique_values[1], ") for", city_name, "\n")
  }
  
  # 提取所有有效栅格点（改进的提取方法）
  cat("📊 Available pixels:", n_cell, ", extracting all pixels\n")
  
  # 改进的数据提取方法
  sample_df <- NULL
  
  cat("🔄 Extracting all pixels using improved method...\n")
  
  all_vals <- terra::values(city_stack)

# 只保留 risk 不为 NA 的行
non_na_idx <- !is.na(all_vals[, "risk"])
filtered_vals <- all_vals[non_na_idx, ]

# 提取坐标
coords <- as.data.frame(xyFromCell(city_stack, which(non_na_idx)))
sample_df <- cbind(coords, filtered_vals)

sample_df <- sample_df[, colSums(!is.na(sample_df)) > 0]

  if (!is.null(sample_df) && nrow(sample_df) > 0) {
    # 添加城市标识和其他元信息
    sample_df$city <- city_name
    sample_df$city_id <- i
    sample_df$extraction_date <- Sys.Date()
    
    # 数据质量检查（修改：不以完整案例为标准，而是检查每个变量的有效性）
    numeric_cols <- sapply(sample_df, is.numeric)
    numeric_cols[c("city_id")] <- FALSE  # 排除ID列
    
    if (sum(numeric_cols) > 0) {
      # 检查每个变量的有效数据比例
      var_completeness <- sapply(sample_df[, numeric_cols, drop = FALSE], 
                                function(x) sum(!is.na(x)) / length(x))
      
      cat("📊 Variable completeness rates:\n")
      for (var_name in names(var_completeness)) {
        cat(sprintf("  - %s: %.1f%% (%d/%d valid)\n", 
                   var_name, 
                   var_completeness[var_name] * 100,
                   sum(!is.na(sample_df[[var_name]])),
                   nrow(sample_df)))
      }
      
      # 检查是否有risk变量，如果有，确保其100%有效
      if (!is.null(risk_col) && risk_col %in% names(sample_df)) {
        risk_completeness <- var_completeness[risk_col]
        if (risk_completeness < 1.0) {
          cat("❌ Risk variable has missing values, this should not happen!\n")
          failed_cities <- c(failed_cities, city_name)
          next
        }
        cat("✅ Risk variable is 100% complete as expected\n")
      }
      
      # 检查是否有太多变量缺失率过高
      high_missing_vars <- names(var_completeness)[var_completeness < 0.3]
      if (length(high_missing_vars) > length(var_completeness) * 0.7) {
        cat("⚠️ Warning: Many variables have high missing rates in", city_name, "\n")
        cat("Variables with >70% missing:", paste(high_missing_vars, collapse = ", "), "\n")
      }
    }
    
    # 检查数据范围是否合理（针对每个变量单独检查）
    for (col in names(sample_df)[numeric_cols]) {
      valid_values <- sample_df[[col]][!is.na(sample_df[[col]])]
      if (length(valid_values) > 0) {
        col_range <- range(valid_values)
        if (diff(col_range) == 0) {
          cat("⚠️ Warning: Variable", col, "has no variation in", city_name, 
              "(value:", col_range[1], ")\n")
        } else {
          cat("📈 Variable", col, "range:", 
              round(col_range[1], 4), "to", round(col_range[2], 4), 
              sprintf("(%d valid values)\n", length(valid_values)))
        }
      } else {
        cat("❌ Warning: Variable", col, "has no valid values in", city_name, "\n")
      }
    }
    
    all_samples[[city_name]] <- sample_df
    success_count <- success_count + 1
    cat("✅ Successfully processed", city_name, "with", nrow(sample_df), "pixels\n")
    
    # 显示内存使用情况（当数据量大时）
    if (nrow(sample_df) > 10000) {
      cat("📊 Large dataset (", nrow(sample_df), " rows). Current memory usage:\n")
      print(gc())
    }
    
  } else {
    cat("❌ No valid pixels extracted from", city_name, "\n")
    failed_cities <- c(failed_cities, city_name)
  }
  
  # 清理临时变量
  rm(city_stack, one_city)
  
  # 定期内存清理
  if (i %% 3 == 0) {
    gc(verbose = FALSE)
    cat("🧹 Memory cleanup completed\n")
  }
  
  cat(sprintf("Progress: %d/%d cities completed, %d successful, %d failed\n", 
              i, length(cities), success_count, length(failed_cities)))
  cat(paste(rep("-", 60), collapse = ""), "\n\n")
}


# 合并所有城市数据
merged_data <- tryCatch({
  # 使用rbindlist（如果有data.table包）或rbind合并
  if (require(data.table, quietly = TRUE)) {
    rbindlist(all_samples, fill = TRUE)  # fill=TRUE处理列数不同的情况
  } else {
    do.call(rbind, all_samples)
  }
}, error = function(e) {
  cat("⚠️ 直接合并失败，尝试逐步合并...\n")
  
  # 逐步合并，处理可能的列名不一致问题
  result <- all_samples[[1]]
  for (i in 2:length(all_samples)) {
    current_data <- all_samples[[i]]
    
    # 确保列名一致
    common_cols <- intersect(names(result), names(current_data))
    if (length(common_cols) < ncol(result) - 3) {  # 允许少量列名差异
      cat("⚠️ 城市", names(all_samples)[i], "的列名与前面不完全一致\n")
    }
    
    # 合并数据
    result <- rbind(result[, common_cols], current_data[, common_cols])
  }
  return(result)
})

cat("✅ 数据合并完成！总样本量:", nrow(merged_data), "个像素点\n")
cat("📋 变量数量:", ncol(merged_data), "个\n")

write.csv(merged_data, "<LOCAL_ANALYSIS_ROOT>/data/pixelfromcity.csv")
str(merged_data)
risk_vars <- grep("risk", names(merged_data), ignore.case = TRUE, value = TRUE)
risk_vars <- risk_vars[!grepl("city|id|date", risk_vars, ignore.case = TRUE)]  # 排除辅助变量
target_var <- risk_vars[1]
# 识别预测变量（排除坐标、城市标识等）
exclude_vars <- c("x", "y", "city", "city_id", "extraction_date", target_var)
predictor_vars <- names(merged_data)[!names(merged_data) %in% exclude_vars]
cat("📈 预测变量 (", length(predictor_vars), "个):", paste(predictor_vars, collapse = ", "), "\n")

# 2.3 数据质量检查
cat("\n🔍 数据质量检查:\n")

# 检查目标变量
if (!is.null(target_var)) {
  target_na <- sum(is.na(merged_data[[target_var]]))
  cat("🎯 目标变量缺失:", target_na, "个 (", round(target_na/nrow(merged_data)*100, 2), "%)\n")
  
  if (target_na > 0) {
    cat("⚠️ 警告：目标变量有缺失值，移除这些观测\n")
    merged_data <- merged_data[!is.na(merged_data[[target_var]]), ]
    cat("✅ 移除后样本量:", nrow(merged_data), "\n")
  }
}

# 检查预测变量的缺失情况
cat("\n📋 预测变量缺失情况:\n")
missing_summary <- sapply(merged_data[predictor_vars], function(x) {
  c(n_missing = sum(is.na(x)), 
    pct_missing = round(sum(is.na(x))/length(x)*100, 2),
    n_valid = sum(!is.na(x)))
})

missing_df <- as.data.frame(t(missing_summary))
missing_df <- missing_df[order(missing_df$pct_missing, decreasing = TRUE), ]
print(missing_df)

# 识别高缺失变量
high_missing_vars <- rownames(missing_df)[missing_df$pct_missing > 30]
if (length(high_missing_vars) > 0) {
  cat("\n⚠️ 高缺失变量 (>30%):", paste(high_missing_vars, collapse = ", "), "\n")
  cat("建议：考虑删除或特殊处理这些变量\n")
}

# 3. 为不同分析准备数据集
# ================================================================================

cat("\n📊 准备分析数据集...\n")

# 3.1 PDP分析数据集（需要完整案例或插补）
cat("🔄 准备PDP分析数据集...\n")

# 选择缺失率较低的变量用于PDP
low_missing_vars <- rownames(missing_df)[missing_df$pct_missing <= 20]  # 缺失率<=20%
pdp_predictors <- intersect(predictor_vars, low_missing_vars)

if (!is.null(target_var)) {
  pdp_data <- merged_data[, c(target_var, pdp_predictors, "city", "x", "y")]
} else {
  pdp_data <- merged_data[, c(pdp_predictors, "city", "x", "y")]
}

# 移除完全缺失的观测（对于PDP分析）
if (!is.null(target_var)) {
  complete_cases_pdp <- complete.cases(pdp_data[, c(target_var, pdp_predictors)])
  pdp_data_complete <- pdp_data[complete_cases_pdp, ]
  
  cat("✅ PDP数据集:\n")
  cat("   - 变量数:", length(pdp_predictors), "个预测变量\n")
  cat("   - 完整案例:", nrow(pdp_data_complete), "个 (", 
      round(nrow(pdp_data_complete)/nrow(merged_data)*100, 2), "% of total)\n")
  cat("   - 城市数:", length(unique(pdp_data_complete$city)), "个\n")
}

# 3.2 SEM分析数据集（可以保留更多缺失值，后续用FIML处理）
cat("\n🔄 准备SEM分析数据集...\n")

# SEM可以处理缺失值，保留更多变量
moderate_missing_vars <- rownames(missing_df)[missing_df$pct_missing <= 50]  # 缺失率<=50%
sem_predictors <- intersect(predictor_vars, moderate_missing_vars)

if (!is.null(target_var)) {
  sem_data <- merged_data[, c(target_var, sem_predictors, "city", "x", "y")]
} else {
  sem_data <- merged_data[, c(sem_predictors, "city", "x", "y")]
}

cat("✅ SEM数据集:\n")
cat("   - 变量数:", length(sem_predictors), "个预测变量\n")
cat("   - 总样本:", nrow(sem_data), "个\n")
cat("   - 城市数:", length(unique(sem_data$city)), "个\n")

# 4. 数据描述性统计
# ================================================================================

cat("\n📈 生成描述性统计...\n")

# 4.1 目标变量统计
if (!is.null(target_var) && target_var %in% names(merged_data)) {
  target_stats <- summary(merged_data[[target_var]])
  cat("🎯 目标变量", target_var, "统计:\n")
  print(target_stats)
  
  # 检查分布
  cat("📊 目标变量分布信息:\n")
  cat("   - 偏度:", round(moments::skewness(merged_data[[target_var]], na.rm = TRUE), 3), "\n")
  cat("   - 峰度:", round(moments::kurtosis(merged_data[[target_var]], na.rm = TRUE), 3), "\n")
}

# 4.2 各城市样本量统计
city_counts <- table(merged_data$city)
cat("\n🏙️ 各城市样本量:\n")
city_summary <- data.frame(
  city = names(city_counts),
  n_pixels = as.numeric(city_counts),
  percentage = round(as.numeric(city_counts)/sum(city_counts)*100, 2)
)
city_summary <- city_summary[order(city_summary$n_pixels, decreasing = TRUE), ]
print(city_summary)

# 5. 保存结果和生成报告
# ================================================================================

cat("\n💾 保存分析数据集...\n")

# 保存为RDS格式（保持数据类型）
if (!is.null(target_var)) {
  saveRDS(pdp_data_complete, "pdp_analysis_data.rds")
  cat("✅ PDP分析数据已保存: pdp_analysis_data.rds\n")
}

saveRDS(sem_data, "sem_analysis_data.rds")
cat("✅ SEM分析数据已保存: sem_analysis_data.rds\n")

saveRDS(merged_data, "merged_all_data.rds")
cat("✅ 完整合并数据已保存: merged_all_data.rds\n")

# 也可以保存为CSV格式
if (!is.null(target_var)) {
  write.csv(pdp_data_complete, "pdp_analysis_data.csv", row.names = FALSE)
}
write.csv(sem_data, "sem_analysis_data.csv", row.names = FALSE)

# 6. 生成数据质量报告
# ================================================================================

cat("\n📋 生成数据质量报告...\n")

# 创建报告
report <- list(
  summary = list(
    total_pixels = nrow(merged_data),
    total_cities = length(unique(merged_data$city)),
    total_variables = ncol(merged_data),
    target_variable = target_var,
    predictor_variables = length(predictor_vars)
  ),
  data_quality = list(
    missing_summary = missing_df,
    high_missing_vars = high_missing_vars,
    city_distribution = city_summary
  ),
  datasets = list(
    pdp_data_size = if(!is.null(target_var)) nrow(pdp_data_complete) else NA,
    pdp_variables = length(pdp_predictors),
    sem_data_size = nrow(sem_data),
    sem_variables = length(sem_predictors)
  )
)

saveRDS(report, "data_quality_report.rds")
cat("✅ 数据质量报告已保存: data_quality_report.rds\n")

# 7. 输出最终总结
# ================================================================================

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("🎉 数据合并与预处理完成！\n")
cat(paste(rep("=", 80), collapse = ""), "\n")

cat("📊 数据概要:\n")
cat("   - 总样本量:", nrow(merged_data), "个像素点\n")
cat("   - 城市数量:", length(unique(merged_data$city)), "个\n")
cat("   - 变量总数:", ncol(merged_data), "个\n")
if (!is.null(target_var)) {
  cat("   - 目标变量:", target_var, "\n")
  cat("   - PDP分析可用:", nrow(pdp_data_complete), "个样本\n")
}
cat("   - SEM分析可用:", nrow(sem_data), "个样本\n")

cat("\n🎯 后续分析建议:\n")
cat("   1. PDP分析: 使用 pdp_analysis_data.rds\n")
cat("   2. SEM分析: 使用 sem_analysis_data.rds (可用FIML处理缺失值)\n")
cat("   3. 检查高缺失变量是否需要特殊处理\n")
cat("   4. 考虑变量变换以改善分布\n")

cat("\n🔗 分析流程建议:\n")
cat("   PDP分析 → 识别重要变量 → SEM建模 → GLM验证\n")

# 显示可以直接使用的数据对象
cat("\n💡 当前环境中的数据对象:\n")
cat("   - merged_data: 完整合并数据\n")
if (!is.null(target_var)) {
  cat("   - pdp_data_complete: PDP分析用完整数据\n")
}
cat("   - sem_data: SEM分析用数据\n")
cat("   - report: 数据质量报告\n")

cat("\n✨ 准备就绪，可以开始PDP和SEM分析！\n")

results <- fread("<LOCAL_ANALYSIS_ROOT>/data/piecewise_analysis_city_results.csv")
merged_data <- fread("<LOCAL_ANALYSIS_ROOT>/data/pixelfromcity.csv")
# 定义显著非线性标准
nonlinear_vars <- results[
  !is.na(Breakpoint) &                                # 存在断点
  abs(Delta_Slope) >= 0.01 &                          # 斜率变化显著
  Breakpoint > quantile(results$Breakpoint, 0.01, na.rm = TRUE) & 
  Breakpoint < quantile(results$Breakpoint, 0.99, na.rm = TRUE) & # 断点位置合理
  R_squared >= 0.1 &                                  # 有较强的解释能力
  Valid_points > 5000                                 # 数据点足够多
]

print(nonlinear_vars[order(-abs(Delta_Slope))])
selected_vars <- nonlinear_vars$Variable
# 仅选取risk及selected_vars，并去掉缺失值

model_data <- merged_data[, c("risk", selected_vars), with=FALSE]
model_data <- na.omit(model_data)
model_data

# 设定XGBoost参数（示例参数）
params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  max_depth = 6,
  eta = 0.1
)



plots <- list()
features <- as.data.frame(model_data[, -'risk', with = FALSE])
labels <- model_data$risk

dtrain <- xgb.DMatrix(data = as.matrix(features), label = labels)

xgb_model <- xgb.train(
  params = list(objective = "reg:squarederror", eta = 0.1, max_depth = 6),
  data = dtrain,
  nrounds = 200
)


# 明确告诉 DALEX 如何对新数据做预测
predict_xgb <- function(model, newdata) {
  newdata <- xgb.DMatrix(data = as.matrix(newdata))
  predict(model, newdata)
}

# 创建 explainer 时添加 predict_function 参数
explainer <- explain(
  model = xgb_model,
  data = features,
  y = labels,
  predict_function = predict_xgb,
  label = "XGBoost"
)


pdp_plot_list <- list()

for (var in selected_vars) {
  # 提取断点
  bp <- results[Variable == var, Breakpoint]

  # 安全检查
  if (length(bp) == 0 || is.na(bp)) next

  # 计算 PDP
  pdp_data <- partial(
    object = xgb_model,
    pred.var = var,
    train = as.matrix(features),
    grid.resolution = 100,
    plot = FALSE
  )

  # 构建图形
  p <- ggplot(pdp_data, aes_string(x = var, y = "yhat")) +
    geom_line(size = 1.2, color = "#0072B2") +
    geom_vline(xintercept = bp, linetype = "dashed", color = "red", linewidth = 0.8) +
    annotate("text",
             x = bp, y = max(pdp_data$yhat, na.rm = TRUE),
             label = paste0("Breakpoint = ", round(bp, 2)),
             hjust = -0.1, vjust = 0.5, size = 4, color = "red") +
    labs(
      x = var,
      y = "Partial dependence",
      title = paste("PDP:", var)
    ) +
    theme(
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
      axis.line = element_line(color = "black"),
      axis.ticks = element_line(color = "black"),
      text = element_text(size = 13)
    )

  pdp_plot_list[[var]] <- p
}
wrap_plots(pdp_plot_list, ncol = 3)
ggsave("<LOCAL_DATABASE_ROOT>/evaluations/PDP_panel_plot.pdf", width = 8, height = 12)

lm_fit <- lm(risk ~ reclamation_area + GDP2020_2.5min + Pop2020_2.5min + BUILT_S_E2025_2.5min, data = city_mean_table)
summary(lm_fit)

city_mean_table

city_mean_table_scaled <- city_mean_table
city_mean_table_scaled$reclamation_area <- as.numeric(city_mean_table_scaled$reclamation_area)
num_cols <- names(which(sapply(city_mean_table_scaled, is.numeric)))

scaled_data <- as.data.table(scale(city_mean_table_scaled[, ..num_cols]))
names(city_mean_table_scaled)

# 替换原数据
city_mean_table_scaled[, (num_cols) := scaled_data]
names_clean <- trimws(names(city_mean_table_scaled))
vars_clean <- trimws(vars_to_remove)
 
names(city_mean_table_scaled) <- trimws(names(city_mean_table_scaled))
# 再次删除高共线性变量
cols_to_keep <- setdiff(names(city_mean_table_scaled), vars_to_remove)

city_mean_table_filtered <- city_mean_table_scaled[, ..cols_to_keep]


city_mean_table_scaled <- city_mean_table_filtered

names(city_mean_table_scaled)
# 示例简化模型（你可以自定义）
# 模型2
model2 <- '
 	reclamation_area ➝ BUILT
risk ~ BUILT
'

# 重新拟合
fit <- sem(model2, data = city_mean_table_scaled, missing = "fiml")
summary(fit, standardized = TRUE, fit.measures = TRUE)

semPaths(fit, whatLabels = "std", layout = "tree", edge.label.cex = 1.2, fade = FALSE)


model3 <- '
  BUILT_S_E2025_2.5min ~ reclamation_area
  salinity_mean ~ reclamation_area
  risk ~ BUILT_S_E2025_2.5min + salinity_mean
'
fit <- sem(model3, data = city_mean_table_scaled)


summary(fit, standardized = TRUE, fit.measures = TRUE)
semPaths(fit, whatLabels = "std", layout = "tree", edge.label.cex = 1.2, fade = FALSE)



model <- '

# 潜变量设定
urban_dev =~ BUILT_S_E2025_2.5min + GDP2020_2.5min + Pop2020_2.5min
disturbance =~ reclamation_area + VIIRS_2023_2.5min
climate =~ wc2.1_2.5m_bio_1 + wc2.1_2.5m_bio_12 + wc2.1_2.5m_bio_15
soil =~ sand_mean_2024_2_5min_aligned + phh2o_mean_2024_2_5min_aligned

# 城市化与扰动互相关联
urban_dev ~~ disturbance

# 城市化/扰动影响气候和土壤
climate ~ urban_dev + disturbance
soil ~ urban_dev + disturbance

# 风险受城市化、环境因子综合影响
risk ~ urban_dev + disturbance + climate + soil
'

gam_model <- gam(risk ~ s(reclamation_area), data = city_mean_table_scaled, method = "REML")
summary(gam_model)
plot(gam_model, shade = TRUE, seWithMean = TRUE)

library(corrplot)
num_data <- city_mean_table_scaled[, .SD, .SDcols = is.numeric]

cor_matrix <- cor(num_data, use = "pairwise.complete.obs")
corrplot(cor(num_data, use = "pairwise.complete.obs"))

cor_matrix

names(num_data )

num_data_clean <- city_mean_table_scaled
names(city_mean_table_scaled)
library(piecewiseSEM)


summary(model4)
model_env_mediation <- psem(
  lm(salinity_mean ~ reclamation_area, data = num_data_clean),
  lm(temperature_mean ~ reclamation_area, data = num_data_clean),
  lm(risk ~ salinity_mean + temperature_mean + reclamation_area, data = num_data_clean)
)
summary(model_env_mediation)



model_psem <- psem(
  lm(salinity_mean ~ reclamation_area, data = num_data_clean),
  lm(temperature_mean ~ reclamation_area, data = num_data_clean),
  lm(VIIRS_2023_2.5min ~ reclamation_area, data = num_data_clean),
  lm(urban_index ~ BUILT_S_E2025_2.5min + GDP2020_2.5min + Pop2020_2.5min, data = num_data_clean),  # 构建因子得分
  lm(risk ~ salinity_mean + temperature_mean + VIIRS_2023_2.5min + urban_index + climate_index, data = num_data_clean)
)
summary(model_psem)


model_B <- '
  # 环境因素之间的结构路径（如有必要）
  salinity_mean ~ wc2.1_2.5m_bio_1 + soc_mean_2024_2_5min_aligned

  # risk 的回归路径
  risk ~ salinity_mean + wc2.1_2.5m_bio_1 + wc2.1_2.5m_bio_12 + soc_mean_2024_2_5min_aligned + sand_mean_2024_2_5min_aligned
'
library(psych)
fa(city_mean_table_scaled[, c("BUILT_S_E2025_2.5min", "GDP2020_2.5min", "reclamation_area")], nfactors = 1, rotate = "none")
fa(city_mean_table_scaled[, c("wc2.1_2.5m_bio_1", "wc2.1_2.5m_bio_12", "wc2.1_2.5m_bio_15")], nfactors = 1)
fa(city_mean_table_scaled[, c("salinity_mean", "salinity_range")], nfactors = 1, rotate = "none")
# 拟合
fit <- sem(model_B, data = city_mean_table_scaled, missing = "fiml")

# 输出结果
summary(fit, standardized = TRUE, fit.measures = TRUE)

model_piecewise <- psem(
  # 环境响应由城市和扰动共同驱动
  lm(salinity_mean ~ reclamation_area + BUILT_S_E2025_2.5min + VIIRS_2023_2.5min, data = df),
  lm(temperature_mean ~ reclamation_area + BUILT_S_E2025_2.5min + VIIRS_2023_2.5min, data = df),

  # 风险模型
  lm(risk ~ salinity_mean + temperature_mean + BUILT_S_E2025_2.5min + VIIRS_2023_2.5min + reclamation_area, data = df)
)


env_vars <- c(
  "wc2.1_2.5m_bio_1", "wc2.1_2.5m_bio_12", "wc2.1_2.5m_bio_13",
  "wc2.1_2.5m_bio_14", "wc2.1_2.5m_bio_15", "wc2.1_2.5m_bio_18"
)

# 构建公式
formula <- as.formula(paste("risk ~", paste(env_vars, collapse = " + ")))

# 拟合模型
fit_env <- lm(formula, data = city_mean_table_scaled)

# 输出回归结果
summary(fit_env)


soil_vars <- city_mean_table_scaled[, c(
  "bdod_mean_2024_2_5min_aligned",
  "cfvo_mean_2024_2_5min_aligned",
  "nitrogen_mean_2024_2_5min_aligned",
  "ocd_mean_2024_2_5min_aligned",
  "phh2o_mean_2024_2_5min_aligned",
  "sand_mean_2024_2_5min_aligned",
  "silt_mean_2024_2_5min_aligned",
  "soc_mean_2024_2_5min_aligned"
)]

fa_soil <- fa(soil_vars, nfactors = 1, rotate = "none")
print(fa_soil)

clim_vars <- city_mean_table_scaled[, c( "wc2.1_2.5m_bio_1", "wc2.1_2.5m_bio_12", "wc2.1_2.5m_bio_13",
  "wc2.1_2.5m_bio_14", "wc2.1_2.5m_bio_15", "wc2.1_2.5m_bio_18")]
fa(clim_vars, nfactors = 1, rotate = "none", fm = "minres")




soil_model <- '
soil_condition =~ bdod_mean_2024_2_5min_aligned +
                    nitrogen_mean_2024_2_5min_aligned +
                    ocd_mean_2024_2_5min_aligned +
                    phh2o_mean_2024_2_5min_aligned +
                    soc_mean_2024_2_5min_aligned
urban_dev =~ BUILT_S_E2025_2.5min
risk ~ soil_condition + urban_dev
'

soil_model <- '
soil_condition =~ bdod_mean_2024_2_5min_aligned +
                    nitrogen_mean_2024_2_5min_aligned +
                    ocd_mean_2024_2_5min_aligned +
                    phh2o_mean_2024_2_5min_aligned +
                    soc_mean_2024_2_5min_aligned
climate =~ wc2.1_2.5m_bio_1 + wc2.1_2.5m_bio_15

soil_condition ~ a * climate
risk ~ b * soil_condition + c_prime * climate
indirect := a * b
total := indirect + c_prime

'
fit <- sem(soil_model, data = city_mean_table_scaled, missing = "fiml")
summary(fit, standardized = TRUE, fit.measures = TRUE)

model <- '
  # 潜变量构建
  soil_condition =~ bdod_mean_2024_2_5min_aligned +
                    nitrogen_mean_2024_2_5min_aligned +
                    ocd_mean_2024_2_5min_aligned +
                    phh2o_mean_2024_2_5min_aligned +
                    soc_mean_2024_2_5min_aligned
  urban_dev =~ BUILT_S_E2025_2.5min
  climate =~ wc2.1_2.5m_bio_1  

  # 显变量路径，并为路径加标签
  salinity_mean ~ a1*climate + a2*urban_dev
  soil_condition ~ b1*climate + b2*urban_dev 
  risk ~ c1*soil_condition + c2*salinity_mean + c3*climate

  # 间接效应定义（基于路径标签）
  indirect1 := b1 * c1         # climate → soil_condition → risk
  indirect2 := a1 * c2         # climate → salinity_mean → risk
  indirect3 := a1  * c1    # climate → salinity_mean → soil_condition → risk

  total := indirect1 + indirect2 + indirect3
'
fit <- sem(model, data = city_mean_table_scaled, missing = "fiml")
summary(fit, standardized = TRUE, fit.measures = TRUE)
modindices(fit) 



model <- '

  # 潜变量构建（latent variables）
  climate        =~ wc2.1_2.5m_bio_1 + wc2.1_2.5m_bio_12
  soil_condition =~ bdod_mean_2024_2_5min_aligned +
                    nitrogen_mean_2024_2_5min_aligned +
                    ocd_mean_2024_2_5min_aligned +
                    phh2o_mean_2024_2_5min_aligned +
                    soc_mean_2024_2_5min_aligned
  urban_dev      =~ BUILT_S_E2025_2.5min

  # 显变量回归路径（带label）
  salinity_mean ~ a1*urban_dev + a2*reclamation_area + a3*climate
  climate       ~ b1*reclamation_area
  soil_condition ~ c1*climate + c2*salinity_mean + c3*urban_dev
  risk          ~ d1*soil_condition + d2*urban_dev + d3*salinity_mean + d4*climate

  # 间接效应定义（indirect effects）
  # reclaim → climate → soil → risk
  ind_reclaim_climate_soil_risk := b1 * c1 * d1

  # reclaim → salinity → soil → risk
  ind_reclaim_salinity_soil_risk := a2 * c2 * d1

  # climate → soil → risk
  ind_climate_soil_risk := c1 * d1

  # salinity → soil → risk
  ind_salinity_soil_risk := c2 * d1

  # 总间接效应（reclaim 到 risk）
  total_indirect_reclaim := ind_reclaim_climate_soil_risk + ind_reclaim_salinity_soil_risk

  # 总效应（reclaim 到 risk）
  total_reclaim := total_indirect_reclaim  # 目前无直接路径，如有可加 e1*reclaim_area 再加 e1

'

model <- '
  # 潜变量构建（latent variables）
  climate        =~ wc2.1_2.5m_bio_1 + wc2.1_2.5m_bio_13+  wc2.1_2.5m_bio_18
  soil_condition =~ bdod_mean_2024_2_5min_aligned +
                    nitrogen_mean_2024_2_5min_aligned +
                    ocd_mean_2024_2_5min_aligned +
                    phh2o_mean_2024_2_5min_aligned +
                    soc_mean_2024_2_5min_aligned
  urban_dev      =~ BUILT_S_E2025_2.5min
 salinity =~ salinity_mean 

  # 显变量回归路径（带 label）
  salinity       ~ a1*urban_dev + a2*reclamation_area + a3*climate
  climate        ~ b1*reclamation_area
  soil_condition ~ c1*climate + c2*salinity + c3*urban_dev
  risk           ~ d1*soil_condition + d2*urban_dev + d3*salinity + d4*climate

  # 间接效应（indirect effects）
  ind_reclaim_climate_soil_risk := b1 * c1 * d1
  ind_reclaim_salinity_soil_risk := a2 * c2 * d1
  ind_climate_soil_risk := c1 * d1
  ind_salinity_soil_risk := c2 * d1

  # 总间接效应（reclaim 到 risk）
  total_indirect_reclaim := ind_reclaim_climate_soil_risk + ind_reclaim_salinity_soil_risk

  # 总效应（reclaim 到 risk）
  total_reclaim := total_indirect_reclaim  # 若加入直接路径，可加 e1*reclamation_area 再加 e1
'
parameterEstimates(fit) %>%
  filter(op == "~~" & est < 0)
fa(clim_vars[, c("wc2.1_2.5m_bio_1", "wc2.1_2.5m_bio_13", "wc2.1_2.5m_bio_15", "wc2.1_2.5m_bio_18")], nfactors = 1)

#=====检验是否存在弯曲/饱和/倒U（非线性）======
city_df <- csv_2

# 1) 基础GAM：检验非线性（REML更稳）
city_df$Regions <- as.factor(city_df$Regions)
city_df$Land_Use_Types <- as.factor(city_df$Land_Use_Types)
city_df$climate_zone <- as.factor(city_df$climate_zone)
city_df$risk_class <- as.factor(city_df$risk_class)
city_df$reclamation_level_manual <- as.factor(city_df$reclamation_level_manual)

city_df$recl_frac <- with(city_df, pmin(reclamation_area / area_km2, 1))
city_df$log_recl <- log1p(city_df$reclamation_area)
# 关键派生变量：围填海占比 & 对数变换
# 1) 基础 GAM：检验 Reclamation 的非线性响应（控制气候/人类强度/盐度/城市面积）
m1 <- gam(
  risk ~ s(recl_frac, k = 5) +                          # 围填海非线性
         s(`BUILT_S_E2025_2.5min`, k = 5) +            # 建成区强度
         s(`wc2.1_2.5m_bio_1`, k = 5) +                # 年均温
         s(`wc2.1_2.5m_bio_15`, k = 5) +               # 降水季节性
         s(salinity_mean, k = 5) +
         s(phh2o_mean_2024_2_5min_aligned, k = 5) +
         s(soc_mean_2024_2_5min_aligned, k = 5) +
         s(area_km2, k = 5) +                          # 城市面积控制
         s(Regions, bs = "re"),                        # 大洲/区域随机效应
  data = city_df, method = "REML")
gam.check(m1)
summary(m1)     # 看 s(log_recl) 的 edf 与 p 值，判断是否有非线性
p5 <- plot(m1, pages = 1, shade = TRUE)
save(p5, "<LOCAL_ANALYSIS_ROOT>/figure/gam_draft/gam_draft1.jpg")
# 2) 与线性项比较：非线性是否更好（AIC/检验）
m0 <- gam(
  risk ~ log_recl + 
         `BUILT_S_E2025_2.5min` + 
         `wc2.1_2.5m_bio_1` + `wc2.1_2.5m_bio_15` +
         salinity_mean + phh2o_mean_2024_2_5min_aligned + soc_mean_2024_2_5min_aligned +
         area_km2 + s(Regions, bs = "re"),
  data = city_df, method = "REML")
AIC(m0, m1)# 非线性是否提高拟合

m_fs <- gam(
  risk ~ s(recl_frac, recla_level_byfraction, bs="fs", k=5) +  # 每个 risk_class 一条平滑
 s(`BUILT_S_E2025_2.5min`, k = 5) +            # 建成区强度
         s(`wc2.1_2.5m_bio_1`, k = 5) +                # 年均温
         s(`wc2.1_2.5m_bio_15`, k = 5) +               # 降水季节性
         s(salinity_mean, k = 5) +
         s(phh2o_mean_2024_2_5min_aligned, k = 5) +
         s(soc_mean_2024_2_5min_aligned, k = 5) +
         s(area_km2, k = 5) +                          # 城市面积控制
         s(Regions, bs = "re"),      
  data = city_df, method="REML")
gam.check(m_fs)
# 与共同曲线比较
AIC(m1, m_fs)

# 或 ML 下的 LRT
anova(update(m1, method="ML"),
      update(m_fs,     method="ML"), test="Chisq")

summary(m_fs) 
library(gratia)
# fs 基会生成每组一条平滑，直接绘图即可
draw(m_fs, parametric = FALSE)     # 或：
draw(m_fs, select = "s(recl_frac):risk_classlow")
draw(m_fs, select = "s(recl_frac):risk_class2")
plot(m_fs, pages=1, shade=TRUE)

install.packages('gratia')
m_lin <- lm(risk ~ log_recl + `BUILT_S_E2025_2.5min` + 
                  `wc2.1_2.5m_bio_1` + `wc2.1_2.5m_bio_15` +
                  salinity_mean + area_km2, data = city_df)
m_seg <- segmented(m_lin, seg.Z = ~ log_recl, psi = list(log_recl = 1))
summary(m_seg)    
