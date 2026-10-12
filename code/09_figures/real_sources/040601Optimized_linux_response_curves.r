# Original source: 040601Optimized_linux_response_curves.r
# Original source location: <LOCAL_CODE_ROOT>/040601Optimized_linux_response_curves.r
# Original source SHA-256: 2b7a5e97dba7d752a9ffcde6f11ec3ee4662c15ee66b7a5f6354039976e37ce8
# Public-safe original snapshot: archive/original_sources/local/040601Optimized_linux_response_curves.r
# Classification: SUPPORTING
# Publication modifications:
# - machine-specific absolute path literals replaced by configured publication_path aliases
# - local encoding decoded to UTF-8; source bytes hashed above
# - comments and historical private machine paths sanitized
# - working-directory calls retained with configured paths; no analyses executed
# Scientific modification: NONE
# This is full historical source, not a certified unattended/reproducible run.
if (!exists("publication_path", mode="function")) stop("Load config/bootstrap.R first")
################################################################################
# TASK: (Deep Diagnostic)
# ------------------------------------------------------------------------------
# Goal:  Extract and plot response curves for 800+ species without re-running models.
# Author: Binyue Kang
# Date:   December 2025
# ==============================================================================
#
# DESCRIPTION:
# This script generates an aggregated "response curve cloud" to visualize the 
# general ecological response of [800+] invasive species to a dominant 
# environmental driver (e.g., Built-up Area / Temperature).
#
# METHODOLOGY:
# 1. Background Sampling: Instead of loading full environmental rasters, we use 
#    'spatSample' to extract 5,000 random background points, significantly 
#    optimizing computation time.
# 2. Data Extraction: Iterates through pre-calibrated biomod2 ensemble models 
#    (.out objects) to calculate marginal response curves for each species.
# 3. Visualization: Uses ggplot2 to create a "spaghetti plot" (semi-transparent 
#    individual curves) overlaid with a global GAM trend line, highlighting 
#    macro-ecological patterns amidst species-specific idiosyncrasies.
#
# INPUTS: 
# - biomod2 ensemble model objects (.models.out)
# - Environmental raster stack (.tif)
#
# OUTPUTS: 
# - Figure: SI_Fig_Response_Cloud.png (Nature-style visualization)
# - Data: Combined response table (optional)
# ==============================================================================

library(biomod2)
library(terra)
library(dplyr)
library(ggplot2)
library(stringr)
# 1. 准备工作
setwd(publication_path("WORK_ROOT", "models"))
env_stack <- rast(publication_path("HPC_INPUT_ROOT", "climate/full_stack.tif"))
target_var <- "GDP2020_2.5min"
cat("=== 优化步骤 1: 生成背景采样数据 (只做一次) ===\n")
# 【关键优化】
# 不要每次都读 TIF。我们随机采 1000 个点代表研究区的环境背景。
# 这样 biomod2 计算中位数时瞬间就能算完，不用读硬盘。
set.seed(123) # 保证可重复
cat("=== 优化步骤 1: 生成背景采样数据 (修复版) ===\n")

# 方案 A：使用规则采样 (Regular Sampling)
# 这会在地图上铺一张网，总能捞到有数据的格子
# size 设大一点 (比如 50000)，最后 na.rm 之后剩下的才是我们要的
bg_data <- spatSample(env_stack, size = 50000, method = "regular", na.rm = TRUE, as.df = TRUE)
names(bg_data) <- names(env_stack)
# 检查一下现在有多少行
cat("修复后背景数据大小:", nrow(bg_data), "行。\n")

# 如果还是 0 或者非常少 (<100)，说明你的 TIF 可能有问题
if(nrow(bg_data) < 100) {
  cat("警告：采样点依然过少！尝试暴力全量计算中位数...\n")
  
  # 方案 B：保底大招 (如果采样失败，直接算全图的中位数构造一行数据)
  # 这会慢一点点，但绝对稳
  env_medians <- global(env_stack, median, na.rm=TRUE)
  # 构造一个只有 1 行的数据框，里面全是中位数
  bg_data <- as.data.frame(t(env_medians[,1]))
  # 复制成 100 行，骗过函数
  bg_data <- bg_data[rep(1, 100), ]
  cat("已通过全量计算生成背景均值数据。\n")
}

# 再次确认
if(nrow(bg_data) > 0) {
  cat("背景数据准备成功！可以进入循环。\n")
} else {
  stop("致命错误：无法从环境图层提取任何有效数值！请检查你的 TIF 文件是否损坏或全是空值。")
}

cat("背景数据准备完毕，大小:", nrow(bg_data), "行。后续计算将飞快。\n")


# 2. test which step error获取第一个模型文件
test_file_list <- list.files(path = publication_path("WORK_ROOT", "models"), 
                             pattern = "\\.models\\.out$", 
                             recursive = TRUE, 
                             full.names = TRUE)
# 为了提高命中率，我们找一个包含 'ensemble' 字样的文件来测
ensemble_files <- grep("ensemble", test_file_list, value = TRUE)
if(length(ensemble_files) > 0) {
  target_file <- ensemble_files[1]
} else {
  target_file <- test_file_list[1]
}

cat("=== 正在诊断文件 ===\n")
cat("路径:", target_file, "\n")

# 3. 加载模型
my_model_out <- get(load(target_file))
sp_name <- my_model_out@sp.name
cat("物种名:", sp_name, "\n")

# ------------------------------------------------------------------------------
# 诊断 A: 变量名是否完全一致？
# ------------------------------------------------------------------------------
cat("\n>>> [诊断 A] 变量名比对 <<<\n")
vars_in_model <- my_model_out@expl.var.names
vars_in_data  <- names(bg_data)

cat("模型需要的变量 (前5个): ", paste(head(vars_in_model, 5), collapse=", "), "\n")
cat("背景数据提供的变量 (前5个): ", paste(head(vars_in_data, 5), collapse=", "), "\n")

# 找差异
missing_in_data <- setdiff(vars_in_model, vars_in_data)
if(length(missing_in_data) > 0) {
  cat("\n❌ 致命错误：背景数据缺少以下变量 (模型需要但数据没有):\n")
  print(missing_in_data)
} else {
  cat("\n✅ 变量名检查通过！模型所需变量全部存在。\n")
}

# ------------------------------------------------------------------------------
# 诊断 B: 模型选择逻辑是否有效？
# ------------------------------------------------------------------------------
cat("\n>>> [诊断 B] 模型选择逻辑 <<<\n")
all_built_models <- get_built_models(my_model_out)
cat("该对象包含的模型数量:", length(all_built_models), "\n")
cat("模型名示例 (前3个):\n")
print(head(all_built_models, 3))

# 测试筛选逻辑
chosen_model <- NULL
if (any(grepl("EMmean|EMwmean", all_built_models))) {
  chosen_model <- grep("EMmean|EMwmean", all_built_models, value = TRUE)[1]
  cat("逻辑判断: 选中了集成模型 ->", chosen_model, "\n")
} else if (any(grepl("Full|AllData", all_built_models))) {
  chosen_model <- grep("Full|AllData", all_built_models, value = TRUE)[1]
  cat("逻辑判断: 选中了全数据模型 ->", chosen_model, "\n")
} else {
  chosen_model <- all_built_models[1]
  cat("逻辑判断: 选中了第一个模型 (Fallback) ->", chosen_model, "\n")
}

if(is.null(chosen_model) || is.na(chosen_model)) {
  cat("\n❌ 致命错误：无法选中任何模型！请检查 grep 关键词。\n")
} else {
  cat("\n✅ 模型选择成功。\n")
}

# ------------------------------------------------------------------------------
# 诊断 C: 强行运行函数 (不带 tryCatch)
# ------------------------------------------------------------------------------
cat("\n>>> [诊断 C] 尝试提取数据 <<<\n")

# 这一步如果报错，会直接显示具体的错误信息
rp_dat <- bm_PlotResponseCurves(
  bm.out = my_model_out,
  models.chosen = chosen_model,
  new.env = bg_data,
  fixed.var = 'median',
  do.plot = FALSE
)

cat("函数运行结束。\n")
cat("返回结果类型:", class(rp_dat), "\n")
cat("返回数据行数:", nrow(rp_dat), "\n")

if(nrow(rp_dat) > 0) {
  cat("\n🎉 诊断成功！数据提取正常。\n")
  print(head(rp_dat))
} else {
  cat("\n⚠️ 警告：函数没报错，但返回了空表。\n")
}
# # 4. 合并与绘图
# cat("\n提取完成，正在绘图...\n")
# big_response_df <- do.call(rbind, response_data_list)
# write.csv(big_response_df, '<WORK_ROOT>/results/Response_Curves_Plots/GDP2020_2.5min_Response.csv')


# # 归一化
# if(max(big_response_df$Probability, na.rm=T) > 1.5) {
#   big_response_df$Probability <- big_response_df$Probability / 1000
# }

# p_cloud <- ggplot(big_response_df, aes(x = Env_Value, y = Probability)) +
#   geom_line(aes(group = Species), color = "#377EB8", alpha = 0.03, linewidth = 0.3) +
#   geom_smooth(method = "gam", color = "#D55E00", fill = "#D55E00", 
#               alpha = 0.3, linewidth = 1.5) +
#   labs(title = "c | Macro-ecological response pattern",
#        subtitle = paste0("Response to ", target_var, " (n=", length(unique(big_response_df$Species)), ")"),
#        x = target_var, y = "Probability") +
#  theme(
#     plot.background = element_rect(fill = "transparent", color = NA),
#     panel.background = element_rect(fill = "transparent", color = NA),
#     panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank()
#   )

# ggsave("<WORK_ROOT>/results/Response_Curves_Plots/GDP2020_2.5min_Response.png", p_cloud, width = 8, height = 6, bg = "white")
# cat("大功告成！图片已保存。\n")