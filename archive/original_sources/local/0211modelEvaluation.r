# Original source: 0211modelEvaluation.r
# Original source location: <LOCAL_CODE_ROOT>/0211modelEvaluation.r
# Original source SHA-256: 3b43109f6b9afee7ddbdcabd9f68791997fdf9c5bed8368c5643c2fdab619298
# Public-safe original snapshot: archive/original_sources/local/0211modelEvaluation.r
# Classification: SUPPORTING
# Publication modifications:
# - machine-specific absolute path literals replaced by configured publication_path aliases
# - local encoding decoded to UTF-8; source bytes hashed above
# - comments and historical private machine paths sanitized
# - working-directory calls retained with configured paths; no analyses executed
# Scientific modification: NONE
# This is full historical source, not a certified unattended/reproducible run.
# Snapshot for inspection; path placeholders are not executable locations.
# ============================================================================
# Model evaluation reports summary for 800+ Species
# Exprcially fot supplymentary figure
# Author: Binyue Kang 02/12/2025
# 有一个重大的问题就是发现在选出的最优sdm中，有近五百多个模型的validation列是空的。对于这种情况没根据原理：全数据模型（Full Model）虽然没有验证分，但它是由之前的 RUN1, RUN2, RUN3... 筛选出来的。因此把那些参与构建这个 Full Model 的 RUN 的验证分找出来，求平均值。
# ============================================================================
library(dplyr)
library(readr)
library(stringr)
library(purrr)

# 1. read the data
 df_all <- read.csv("<LOCAL_ANALYSIS_ROOT>/data/organized_table/03_All_Models_evaluationCombined.csv")
df_best <- read.csv("<LOCAL_ANALYSIS_ROOT>/data/organized_table/04_All_Best_Models_Combined.csv")
alg_TSS <- df_all  %>%
  filter(Type == "individual",
         metric.eval == "TSS",
         !is.na(validation)) %>%
  group_by(algo) %>%
  summarise(
    n = n(),
    median_TSS = median(validation),
    IQR_low  = quantile(validation, 0.25),
    IQR_high = quantile(validation, 0.75)
  )

alg_ROC <- df_all  %>%
  filter(Type == "individual",
         metric.eval == "ROC",
         !is.na(validation)) %>%
  group_by(algo) %>%
  summarise(
    n = n(),
    median_AUC = median(validation),
    IQR_low  = quantile(validation, 0.25),
    IQR_high = quantile(validation, 0.75)
  )

alg_TSS
alg_ROC
# --- 2. 图 A: 算法大比拼 (使用 df_all) ---
# 清洗数据：提取需要的算法列
plot_data_A <- df_all %>%
  filter(metric.eval %in% c("TSS", "ROC")) %>%
  # 这里的 validation 是验证集分数，最重要
  mutate(Score = as.numeric(validation),
         # 简化算法名称，把 EMmean, EMmedian 等统称为 Ensemble
         Algorithm = case_when(
           grepl("EM", algo) ~ "Ensemble", 
           algo == "RFd" ~ "Random Forest",
           algo == "MAXNET" ~ "MaxEnt",
           algo == "GLM" ~ "GLM",
           TRUE ~ "Other"
         )) %>%
  filter(!is.na(Score))

p_algo <- ggplot(plot_data_A, aes(x = reorder(Algorithm, Score, median), y = Score, fill = Algorithm)) +
  geom_boxplot(outlier.size = 0.2, outlier.alpha = 0.3, width = 0.6, alpha = 0.8) +
  facet_wrap(~metric.eval, scales = "free_y") + # 分面展示 TSS 和 ROC
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "a | Performance comparison across modeling algorithms",
    y = "Evaluation Score (Validation)",
    x = NULL
  ) +
  theme_minimal(base_size = 20) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.border = element_rect(color = "black", fill = NA, size = 0.8),
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
p_algo 

 ggsave(
  filename = "<LOCAL_ANALYSIS_ROOT>/figure/sdm_evalueation/03_All_Models_evaluationCombined.png",
  plot     = p_algo ,
  width    = 7,
  height   = 6,
  device = grDevices::png  
)
 ggsave(
  filename = "<LOCAL_ANALYSIS_ROOT>/figure/sdm_evalueation/03_All_Models_evaluationCombined.pdf",
  plot     = p_algo ,
  width    = 7,
  height   = 6)
# --- 3. 图 B: 最终模型质量分布 (使用 df_best) ---
# 策略：不区分 Type，统称为 "Final Models"

best_TSS_val <- df_best %>%
  filter(metric.eval == "TSS",
         !is.na(validation))

best_ROC_val <- df_best %>%
  filter(metric.eval == "ROC",
         !is.na(validation))

summary_TSS_val <- best_TSS_val %>%
  summarise(
    n_models   = n(),
    median_TSS = median(validation),
    IQR_TSS_low  = quantile(validation, 0.25),
    IQR_TSS_high = quantile(validation, 0.75)
  )

summary_ROC_val <- best_ROC_val %>%
  summarise(
    n_models   = n(),
    median_AUC = median(validation),
    IQR_AUC_low  = quantile(validation, 0.25),
    IQR_AUC_high = quantile(validation, 0.75)
  )

summary_TSS_val
summary_ROC_val

best_TSS_cal_only <- df_best %>%
  filter(metric.eval == "TSS",
         is.na(validation),
         !is.na(calibration))

summary_TSS_cal_only <- best_TSS_cal_only %>%
  summarise(
    n_models   = n(),
    median_TSS = median(calibration),
    IQR_TSS_low  = quantile(calibration, 0.25),
    IQR_TSS_high = quantile(calibration, 0.75)
  )

best_ROC_cal_only <- df_best %>%
  filter(metric.eval == "ROC",
         is.na(validation),
         !is.na(calibration))

summary_ROC_cal_only <- best_ROC_cal_only %>%
  summarise(
    n_models   = n(),
    median_TSS = median(calibration),
    IQR_TSS_low  = quantile(calibration, 0.25),
    IQR_TSS_high = quantile(calibration, 0.75)
  )
summary_TSS_cal_only
summary_ROC_cal_only 

# ==============================================================================
# 第二步：从全记录表中计算“替身分数” (Proxy Scores)
# ==============================================================================
# 我们需要算出每个物种、每种指标在交叉验证(RUN1, RUN2...)中的平均表现
# 用这个平均值来代表那些没有验证分的 Final Model
df_proxy <- df_all %>%
  # 1. 排除全数据运行 (allRun / allData)，只保留交叉验证的运行
  filter(!run %in% c("allRun", "allData")) %>%
  # 2. 筛选需要的指标
  filter(metric.eval %in% c("TSS", "ROC")) %>%
  # 3. 确保分数是数字
  mutate(validation = as.numeric(validation)) %>%
  filter(!is.na(validation)) %>%
  # 4. 分组计算平均分
  # 注意：这里我们按 Species 和 metric 分组。
  # 逻辑是：不管最终选了哪个算法，该物种在交叉验证中的平均表现是其可预测性的最佳估计。
  group_by(Species, metric.eval) %>%
  summarise(Mean_CV_Score = mean(validation, na.rm = TRUE), .groups = "drop")

# ==============================================================================
# 第三步：清洗最终模型表并填补空缺
# ==============================================================================

plot_data <- df_best %>%
  # 1. 筛选指标
  filter(metric.eval %in% c("TSS", "ROC")) %>%
  # 2. 关联替身分数表
  left_join(df_proxy, by = c("Species", "metric.eval")) %>%
  mutate(
    # 3. 核心逻辑：优先用现有的 validation，如果是 NA，就用 Mean_CV_Score 顶替
    # 这里的 as.numeric 很重要，防止原始数据里有字符
    Real_Validation = as.numeric(validation),
    
    Final_Score = case_when(
      !is.na(Real_Validation) ~ Real_Validation, # 如果本来就有验证分(那271个)，就用本来的
      is.na(Real_Validation) & !is.na(Mean_CV_Score) ~ Mean_CV_Score, # 如果是NA(那598个)，用替身
      TRUE ~ as.numeric(calibration) # 万一替身也没有(极少见)，用 calibration 兜底，防止报错
    )
  ) %>%
  # 4. 再次过滤，确保没有 NA (双重保险)
  filter(!is.na(Final_Score))

# --- 检查一下现在的统计数据 (给你自己看的，定心丸) ---
summary_stats <- plot_data %>%
  group_by(metric.eval) %>%
  summarise(
    Min = min(Final_Score),
    Median = median(Final_Score),
    Mean = mean(Final_Score),
    Max = max(Final_Score)
  )
  plot_data_clean <- plot_data %>%
  filter(Final_Score > 0)  # 剔除 0 和 -1 的异常值

# 然后再算中位数
final_stats <- plot_data_clean %>%
  group_by(metric.eval) %>%
  summarise(
    n = n(),
    Median = median(Final_Score),
    Mean = mean(Final_Score),
    IQR_25 = quantile(Final_Score, 0.25),
    IQR_75 = quantile(Final_Score, 0.75)
  )

print(final_stats)
# plot_data_B <- df_best %>%
#   filter(metric.eval %in% c("TSS", "ROC")) %>%
#   # 注意：Best 表里的 calibration 和 validation 可能需要确认用哪个
#   # 通常 Best Model 报告的是 calibration (全数据训练) 或者 validation (交叉验证均值)
#   # 假设这里我们关注 validation 或 calibration，或者取两者的均值
#   mutate(Score = as.numeric(calibration)) # 这里假设用 calibration 展示最终拟合能力，或者用 validation 展示预测能力

# 2. 定义 Nature 风格配色 (更高级的深蓝和暗红/橙)
nature_colors <- c("ROC" = "#377EB8", "TSS" = "#E41A1C") 
# 或者蓝绿配色: c("ROC" = "#0072B2", "TSS" = "#009E73")

# 3. 绘图
p_performance <- ggplot(plot_data_clean, aes(x = metric.eval, y = Final_Score, fill = metric.eval, color = metric.eval)) +
  
  # 1. 抖动散点 (Jitter)：展示数据量，增加可信度
  # alpha 设低一点，让点看起来像云雾
  geom_jitter(position = position_jitter(width = 0.25), size = 0.6, alpha = 0.15) +
  
  # 2. 小提琴图 (Violin)：展示分布密度
  # alpha=0.5 让背景的点能透出来一点
  geom_violin(trim = FALSE, alpha = 0.5, color = NA, width = 0.6) +
  
  # 3. 箱线图 (Boxplot)：展示统计量
  # 放在最上层，白色填充，黑色细边框
  geom_boxplot(width = 0.1, fill = "white", color = "black", 
               outlier.shape = NA, # 离群点已由 jitter 展示，这里关掉
               alpha = 0.3, size = 0.4) +
  
  # 4. 颜色与标尺
  scale_fill_manual(values = nature_colors) +
  scale_color_manual(values = nature_colors) +
  scale_y_continuous(limits = c(0, 1.05), breaks = seq(0, 1, 0.2), expand = c(0, 0)) +
  
  # 5. 标题与标签 (符合子刊 Supplementary Figure 格式)
  labs(
    title = "b | Predictive performance of final selected models",
    # 自动计算物种数量填入副标题
    subtitle = paste0("Evaluated via cross-validation metrics across ", length(unique(plot_data$Species)), " species"),
    y = "Evaluation Score",
    x = NULL
  ) +
  
  # 6. 透明主题
theme_minimal(base_size = 24) +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.border = element_rect(color = "black", fill = NA, size = 2),
    
    # --- 修改了这里：一键去掉所有网格线 ---
    panel.grid = element_blank(),
    # ------------------------------------
    
    legend.position = "none",
    
    text = element_text(color = "black"),
    axis.text = element_text(color = "black", size = 22),
    plot.title = element_text(face = "bold", size = 14)
  )

# 预览
print(p_performance)
 ggsave(
  filename = "<LOCAL_ANALYSIS_ROOT>/figure/sdm_evalueation/03.b_Best_Models_evaluationCombined.png",
  plot     = p_performance ,
  width    = 7,
  height   = 6,
  device = grDevices::png  
)
 ggsave(
  filename = "<LOCAL_ANALYSIS_ROOT>/figure/sdm_evalueation/03.b_Best_Models_evaluationCombined.pdf",
  plot     = p_performance ,
  width    = 7,
  height   = 6)
# 2. 提取 cross-validated TSS:
#    - individual 模型
#    - 使用 TSS 指标
#    - 用 calibration 列（你之前就是用它来汇总性能的）
#    - 选一条代表 allRun 的记录（这里简单用 grepl("allRun", full.name) 过滤）
tss_alg <-  df_all %>%
  filter(Type == "individual",
         metric.eval == "TSS",
         !is.na(calibration),
         grepl("allRun", full.name)) %>%
  group_by(Species, algo) %>%
  summarise(
    TSS = mean(calibration),  # 万一还有多个 allRun 记录，就取平均
    .groups = "drop"
  )

# 3. 按物种归一化 TSS 得到权重（负值截为 0）
weights_raw <- tss_alg %>%
  group_by(Species) %>%
  mutate(
    TSS_pos = pmax(TSS, 0),
    w = ifelse(sum(TSS_pos) > 0, TSS_pos / sum(TSS_pos), NA_real_)
  ) %>%
  ungroup()
weights_raw 
# 4. 只保留真正“ensemble”的物种：至少两个算法有正权重
weights_multi <- weights_raw %>%
  group_by(Species) %>%
  filter(sum(!is.na(w) & w > 0) >= 2) %>%
  ungroup()
alg_w_summary <- weights_multi %>%
  group_by(algo) %>%
  summarise(
    n_species = n_distinct(Species),
    median_w = median(w, na.rm = TRUE),
    IQR_low  = quantile(w, 0.25, na.rm = TRUE),
    IQR_high = quantile(w, 0.75, na.rm = TRUE)
  )

alg_w_summary
# 5. 画 panel a：各算法权重分布
p_weights <- ggplot(weights_multi, aes(x = algo, y = w, fill = algo)) +
  geom_violin(trim = TRUE, alpha = 0.6, colour = NA) +
  geom_boxplot(width = 0.15, outlier.size = 0.4, alpha = 0.9) +
  scale_y_continuous("Ensemble weight", limits = c(0.2, 0.6)) +
  scale_x_discrete("Algorithm") +
  scale_fill_brewer(palette = "Set3") +
 theme_minimal(base_size = 24) +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.border = element_rect(color = "black", fill = NA, size = 2),
    
    # --- 修改了这里：一键去掉所有网格线 ---
    panel.grid = element_blank(),
    # ------------------------------------
    
    legend.position = "none",
    
    text = element_text(color = "black"),
    axis.text = element_text(color = "black", size = 22),
    plot.title = element_text(face = "bold", size = 14)
  )

p_weights
 ggsave(
  filename = "<LOCAL_ANALYSIS_ROOT>/figure/sdm_evalueation/Ensemble_weight.png",
  plot     = p_weights,
  width    = 7,
  height   = 6,
  device = grDevices::png  
)
 ggsave(
  filename = "<LOCAL_ANALYSIS_ROOT>/figure/sdm_evalueation/Ensemble_weight.pdf",
  plot     = p_weights,
  width    = 7,
  height   = 6)
