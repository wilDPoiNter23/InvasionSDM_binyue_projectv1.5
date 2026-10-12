# Original source: 040602Batchrun_linux_response.r
# Original source location: <LOCAL_CODE_ROOT>/040602Batchrun_linux_response.r
# Original source SHA-256: 37d0e0e61f1953dea60f52099c7231a7a97295b8cfec56560d6ae95ac7f002e8
# Public-safe original snapshot: archive/original_sources/local/040602Batchrun_linux_response.r
# Classification: SUPPORTING
# Publication modifications:
# - machine-specific absolute path literals replaced by configured publication_path aliases
# - local encoding decoded to UTF-8; source bytes hashed above
# - comments and historical private machine paths sanitized
# - working-directory calls retained with configured paths; no analyses executed
# Scientific modification: NONE
# This is full historical source, not a certified unattended/reproducible run.
# Snapshot for inspection; path placeholders are not executable locations.
# ==============================================================================
# Project: Global Invasion SDM Response Curves (Batch Processing)
# Environment: Server / High Performance Computing
# ==============================================================================
# 1. 加载包
library(biomod2)
library(terra)
library(dplyr)
library(ggplot2)
library(stringr)
library(data.table) # 处理大数据表更快

# ==============================================================================
# 2. 配置路径与环境
# ==============================================================================
# 服务器路径配置
model_dir <- "<WORK_ROOT>/models"
env_tif_path <- "<HPC_INPUT_ROOT>/climate/full_stack.tif"
out_dir_csv <- "<WORK_ROOT>/results/Response_Curves_Plots/Response_Data_CSVs" # CSV 输出目录
out_dir_img <- "<WORK_ROOT>/results/Response_Curves_Plots/Response_Curves_Plots" # 图片输出目录
cat("输出目录已就绪:\n")
cat("CSV:", out_dir_csv, "\n")
cat("IMG:", out_dir_img, "\n")
# 创建输出目录
if(!dir.exists(out_dir_csv)) dir.create(out_dir_csv)
if(!dir.exists(out_dir_img)) dir.create(out_dir_img)

setwd(model_dir)

# ==============================================================================
# 2. 准备背景数据 (使用 Global 方法，确保无 NA)
# ==============================================================================
cat("正在加载环境图层...\n")
env_stack <- rast(env_tif_path) 
cat("Calculating global statistics (Min, Max, Median)...\n")
env_min <- global(env_stack, min, na.rm = TRUE)
env_max <- global(env_stack, max, na.rm = TRUE)
env_med <- global(env_stack, median, na.rm = TRUE)

# 3. 转置并修正列名
df_min <- as.data.frame(t(env_min[,1]))
names(df_min) <- names(env_stack)

df_max <- as.data.frame(t(env_max[,1]))
names(df_max) <- names(env_stack)

df_med <- as.data.frame(t(env_med[,1]))
names(df_med) <- names(env_stack)

# 4. 【核心大招】拼合数据
# 第1行：最小值 (撑开下限)
# 第2行：最大值 (撑开上限)
# 第3-1000行：中位数 (锁定背景)
bg_med_rep <- df_med[rep(1, 998), ] 

bg_data <- rbind(df_min, df_max, bg_med_rep)

# 5. 最后的保险：检查 NA 并填补
# (极少数情况下，某些边缘图层可能全是NA导致计算失败，这里填0防止程序崩溃)
if(any(is.na(bg_data))) {
  bg_data[is.na(bg_data)] <- 0
  cat("Warning: NAs found in stats and filled with 0. (Check your TIF if this happens often)\n")
}

cat("Background data ready.\n")
cat("Rows:", nrow(bg_data), "\n")

# 打印一下第一个变量的范围，确认它不再是一个定值
check_var <- names(bg_data)[1]
cat("Check Range for", check_var, ":\n")
cat("  Min:", min(bg_data[[check_var]]), "\n")
cat("  Max:", max(bg_data[[check_var]]), "\n")

if(min(bg_data[[check_var]]) == max(bg_data[[check_var]])) {
  stop("🔴 警告：最大值等于最小值！你的环境图层可能只有一个值，或者计算出错了！")
} else {
  cat("🟢 验证通过：背景数据具有正确的梯度范围。\n")
}

# ==============================================================================
# 4. 获取并筛选模型文件列表
# ==============================================================================
cat(">>> [Step 2/4] listing model files...\n")

raw_file_list <- list.files(pattern = "\\.models\\.out$", recursive = TRUE, full.names = TRUE)

# 智能去重：优先取 Ensemble，其次取 Full/AllData
file_df <- data.frame(fullpath = raw_file_list) %>%
  mutate(
    folder_path = dirname(fullpath),
    filename = basename(fullpath),
    Priority = case_when(
      str_detect(filename, "ensemble") ~ 1,
      str_detect(filename, "Full|AllData") ~ 2,
      TRUE ~ 3
    )
  ) %>%
  group_by(folder_path) %>%
  arrange(Priority, filename) %>%
  slice(1) %>%
  ungroup()

file_list <- file_df$fullpath
cat("Target species count:", length(file_list), "\n")

# ==============================================================================
# 5. 核心循环：提取所有数据 (一次性提取所有变量)
# ==============================================================================
cat(">>> [Step 3/4] Extracting response data for ALL variables...\n")

# 初始化大列表
response_list_all <- list()
counter <- 0

for (f_path in file_list) {
  counter <- counter + 1
  
  tryCatch({
    # A. 加载
    capture.output({
      my_model_out <- get(load(f_path))
    })
    sp_name <- my_model_out@sp.name
    
    # B. 选模型
    all_models <- get_built_models(my_model_out)
    chosen_model <- NULL
    if (any(grepl("EMmean|EMwmean", all_models))) {
      chosen_model <- grep("EMmean|EMwmean", all_models, value = TRUE)[1]
    } else if (any(grepl("Full|AllData", all_models))) {
      chosen_model <- grep("Full|AllData", all_models, value = TRUE)[1]
    } else {
      chosen_model <- all_models[1]
    }
    
    # C. 计算曲线 (静默运行)
    capture.output({
      rp_res <- bm_PlotResponseCurves(
        bm.out = my_model_out,
        models.chosen = chosen_model,
        new.env = bg_data,
        fixed.var = 'median',
        do.plot = FALSE
      )
    })
    
    # D. 【关键修复】从 List 中提取 Data Frame
    # 不同的 biomod2 版本返回结构不同，这里做一个兼容检查
    rp_dat <- NULL
    
    if (is.data.frame(rp_res)) {
      rp_dat <- rp_res # 旧版本直接返回 DF
    } else if (is.list(rp_res) && "tab" %in% names(rp_res)) {
      rp_dat <- rp_res$tab # 新版本在 $tab 里
    } else {
      # 尝试直接转，或者它是 list 的第一个元素
      try(rp_dat <- as.data.frame(rp_res), silent=TRUE)
    }
    
    # E. 存入列表
    if (!is.null(rp_dat) && nrow(rp_dat) > 0) {
      # 标准化列名 (Biomod2输出列名通常是 expl.name, expl.val, pred.val)
      clean_dat <- rp_dat %>%
        select(Variable = expl.name, Env_Value = expl.val, Probability = pred.val) %>%
        mutate(Species = sp_name)
      
      response_list_all[[sp_name]] <- clean_dat
    }
    
    if(counter %% 10 == 0) cat(".")
    if(counter %% 500 == 0) cat("\n")
    
  }, error = function(e) {
    # 依然跳过错误
  })
}

cat("\nMerging dataframes...\n")
# 合并为一个巨大的数据框 (可能有一两百万行，R处理没问题)
master_df <- rbindlist(response_list_all) 

cat("Total rows extracted:", nrow(master_df), "\n")
cat("Variables found in models:", paste(unique(master_df$Variable), collapse=", "), "\n")

# ==============================================================================
# 6. 按变量拆分：保存 CSV 并绘图
# ==============================================================================
cat(">>> [Step 4/4] Splitting by variable, saving CSVs and plotting...\n")

# 获取实际提取到了数据的变量列表
available_vars <- unique(master_df$Variable)

for (var_name in available_vars) {
  
  cat("Processing variable:", var_name, "...\n")
  
  # 1. 筛选该变量的数据
  plot_df <- master_df[Variable == var_name]
  
  # 2. 归一化 (0-1000 -> 0-1)
  if(max(plot_df$Probability, na.rm=T) > 1.5) {
    plot_df$Probability <- plot_df$Probability / 1000
  }
  
  # 3. 保存 CSV (Requirement 1)
  csv_name <- file.path(out_dir_csv, paste0("Response_Data_", var_name, ".csv"))
  fwrite(plot_df, csv_name)
  
  # 4. 绘图 (Requirement 2: Nature Style Cloud Plot)
  # 只有当物种数足够多时画云图才有意义
  n_sp <- length(unique(plot_df$Species))
  
  p <- ggplot(plot_df, aes(x = Env_Value, y = Probability)) +
    # 蓝色迷雾背景
    geom_line(aes(group = Species), color = "#377EB8", alpha = 0.02, linewidth = 0.2) +
    # 红色趋势线
    geom_smooth(method = "gam", color = "#D55E00", fill = "#D55E00", 
                alpha = 0.3, linewidth = 1.2) +
    labs(
      title = paste("Global Response:", var_name),
      subtitle = paste0("Aggregated response (n=", n_sp, " species)"),
      x = var_name,
      y = "Probability of Occurrence"
    ) +
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    theme_minimal(base_size = 14) +
    theme(
      plot.background = element_rect(fill = "white", color = NA), # 服务器保存建议用白色底防变黑
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
      panel.grid.major = element_line(color = "grey90"),
      panel.grid.minor = element_blank()
    )
  
  # 保存图片
  ggsave(filename = file.path(out_dir_img, paste0("Response_Curve_", var_name, ".png")),
         plot = p, width = 8, height = 6, dpi = 300)
}

cat(">>> ALL DONE. Check folders:", out_dir_csv, "and", out_dir_img, "\n")