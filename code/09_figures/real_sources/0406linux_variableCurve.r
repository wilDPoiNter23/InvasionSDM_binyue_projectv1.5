# Original source: 0406linux_variableCurve.r
# Original source location: <LOCAL_CODE_ROOT>/0406linux_variableCurve.r
# Original source SHA-256: 3b45c27e2614c5868e50a3f017af6af68639065361d9641b4c33b86d7db7e56f
# Public-safe original snapshot: archive/original_sources/local/0406linux_variableCurve.r
# Classification: SUPPORTING
# Publication modifications:
# - machine-specific absolute path literals replaced by configured publication_path aliases
# - local encoding decoded to UTF-8; source bytes hashed above
# - comments and historical private machine paths sanitized
# - working-directory calls retained with configured paths; no analyses executed
# Scientific modification: NONE
# This is full historical source, not a certified unattended/reproducible run.
if (!exists("publication_path", mode="function")) stop("Load config/bootstrap.R first")
library(biomod2)
library(terra)
library(ggplot2)
library(stringr)
library(dplyr)
# ==============================================================================
# 1. 设置工作环境 (请根据实际情况修改)
# ==============================================================================
# 设置工作目录到存放所有物种文件夹的根目录
setwd(publication_path("WORK_ROOT", "models"))

# 加载环境变量 (必须有这个才能画响应曲线)
# 替换为你真实的环境变量路径
env_stack <- rast(publication_path("HPC_INPUT_ROOT", "climate/full_stack.tif"))

# (去看看你之前的变量重要性箱线图，选排第一那个)
target_var <- "BUILT_S_E2025_2.5min"  # 举例：年均温
cat("TIF 文件包含的变量:\n")
print(names(env_stack))

# 检查目标变量是否存在于 stack 中
if(!target_var %in% names(env_stack)) {
  stop(paste("错误：你的 TIF 文件里没有找到变量", target_var))
}

cat("正在计算背景变量的平均值 (这可能需要一点时间)...\n")
# 计算所有层的平均值 (作为固定背景)
env_means <- global(env_stack, mean, na.rm=TRUE)

# 计算目标变量的范围 (最小值到最大值)
target_range <- global(env_stack[[target_var]], range, na.rm=TRUE)

# --- 构造预测用的数据表 ---
n_points <- 1000
# 1. 先生成全均值的一行
base_data <- as.data.frame(t(env_means[,1]))
names(base_data) <- names(env_stack)
# 2. 复制 100 行
eval_data <- base_data[rep(1, n_points), ]
# 3. 把目标变量替换成梯度值
eval_data[[target_var]] <- seq(target_range[1,1], target_range[1,2], length.out = n_points)

cat("预测数据表准备完毕！包含列数:", ncol(eval_data), "\n")

response_prep_data <- list(
  env_means = env_means,       # 背景均值
  target_range = target_range, # 目标变量范围
  target_var = target_var,     # 目标变量名字
  base_data = base_data,       # 单行基础数据
  eval_data = eval_data        # 最终生成的1000行梯度数据
)

# 2. 保存 (文件小，读取快，不乱)
saveRDS(response_prep_data, publication_path("WORK_ROOT", "results/Response_Curves_Plots/Response_Curve_Prep_Data.rds"))

cat("数据已打包保存为 Response_Curve_Prep_Data.rds \n")

cat("=== 步骤 2: 获取模型文件列表 ===\n")
target_path <- publication_path("WORK_ROOT", "models")

# 1. 获取所有 .out 文件
raw_file_list <- list.files(path = target_path, 
                            pattern = "\\.models\\.out$", 
                            recursive = TRUE, 
                            full.names = TRUE)

cat("------------------------------------------------\n")
cat("原始文件总数:", length(raw_file_list), "\n")

# 如果找不到文件，直接报错停止
if(length(raw_file_list) == 0) stop("错误：在该路径下未找到任何 .models.out 文件！请检查路径拼写。")

# 打印前 5 个看看长什么样
cat("前 5 个文件路径示例:\n")
print(head(raw_file_list, 5))
cat("------------------------------------------------\n")


# 2. 智能去重逻辑
file_df_clean <- data.frame(fullpath = raw_file_list) %>%
  mutate(
    # 提取文件夹路径（代表一个物种）
    folder_path = dirname(fullpath),
    # 提取文件名
    filename = basename(fullpath),
    
    # --- 核心修改：显式定义优先级 ---
    Priority = case_when(
      # 如果文件名包含 ensemble，优先级为 1 (首选)
      str_detect(filename, "ensemble") ~ 1,
      # 其他情况 (单模型)，优先级为 2
      TRUE ~ 2
    )
  ) %>%
  # 按文件夹分组 (即按物种分组)
  group_by(folder_path) %>%
  # 先按优先级排序(1在2前)，如果优先级一样，按文件名排序
  arrange(Priority, filename) %>%
  # 取排第一的那个
  slice(1) %>%
  ungroup()

# 提取最终列表
file_list <- file_df_clean$fullpath

cat("筛选后文件数 (即物种数):", length(file_list), "\n")
# 检查前 10 个被选中的文件，看看是不是 ensemble
cat("------------------------------------------------\n")
cat("检查筛选结果 (前 10 个):\n")

check_df <- file_df_clean %>% 
  select(filename, Priority) %>% 
  head(10)

print(check_df)

# 统计一下选中了多少个集成模型，多少个单模型
table(file_df_clean$Priority)
cat("(1 代表集成模型，2 代表单模型)\n")

cat("=== 步骤 3: 开始批量提取响应数据 ===\n")
# 创建一个空列表存结果
response_data_list <- list()
counter <- 0
for (f_path in file_list) {
  counter <- counter + 1
  
  tryCatch({
    # --- A. 加载模型对象 ---
    capture.output({
      my_model_out <- get(load(f_path))
    })
    
    sp_name <- my_model_out@sp.name
    
    # --- B. 选择模型 ---
    all_models <- get_built_models(my_model_out)
    
    chosen_model <- NULL
    if (any(grepl("EMmean|EMwmean", all_models))) {
      chosen_model <- grep("EMmean|EMwmean", all_models, value = TRUE)[1]
    } else if (any(grepl("Full|AllData", all_models))) {
      chosen_model <- grep("Full|AllData", all_models, value = TRUE)[1]
    } else {
      chosen_model <- all_models[1]
    }
    
    # --- C. 【核心】使用官方函数提取数据 ---
    # do.plot = FALSE 是关键，这样它会返回数据而不是画图
    rp_dat <- bm_PlotResponseCurves(
      bm.out = my_model_out,
      models.chosen = chosen_model,
      new.env = env_stack,    # 传入环境变量，防止找不到数据
      fixed.var = "median",   # 固定其他变量为中位数
      do.plot = FALSE         # 不画图，只返回数据！
    )
    
    # --- D. 整理数据 ---
    # rp_dat 包含所有变量的结果，我们需要筛选出 target_var
    # 并且统一列名
    
    # 检查返回的数据结构通常是: expl.name, expl.val, pred.val, pred.name
    if(target_var %in% rp_dat$expl.name) {
      
      # 筛选目标变量
      sp_dat <- rp_dat %>%
        filter(expl.name == target_var) %>%
        select(
          Env_Value = expl.val, 
          Probability = pred.val
        ) %>%
        mutate(Species = sp_name) # 添加物种名
      
      # 存入列表
      response_data_list[[sp_name]] <- sp_dat
    }
    
    if(counter %% 10 == 0) cat(".")
    if(counter %% 500 == 0) cat("\n")
    
  }, error = function(e) {
    # cat(paste0("\n[跳过] ", sp_name, ": ", conditionMessage(e), "\n"))
  })
}

# 合并
big_response_df <- do.call(rbind, response_data_list)
cat("\n数据提取完成！共获取:", nrow(big_response_df), "行数据。\n")

# ==============================================================================
# 4. 画图 (Nature Style)
# ==============================================================================
# 确保 Probability 是 0-1
if(max(big_response_df$Probability, na.rm=T) > 1.5) {
  big_response_df$Probability <- big_response_df$Probability / 1000
}

p_cloud <- ggplot(big_response_df, aes(x = Env_Value, y = Probability)) +
  # 蓝色迷雾
  geom_line(aes(group = Species), color = "#377EB8", alpha = 0.03, linewidth = 0.3) +
  # 红色趋势
  geom_smooth(method = "gam", color = "#D55E00", fill = "#D55E00", 
              alpha = 0.3, linewidth = 1.5) +
  labs(
    title = "c | Macro-ecological response pattern",
    subtitle = paste0("Aggregated response of ", length(unique(big_response_df$Species)), " species"),
    x = target_var,
    y = "Probability of Occurrence"
  ) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.minor = element_blank()
  )

# 保存
ggsave("<HISTORICAL_RELATIVE_WORK_ROOT>/results/Response_Curves_Plots/SI_Fig_Response_Cloud_Final.png", p_cloud, width = 8, height = 6, bg = "transparent", dpi = 300)
cat("图片已保存！\n")
