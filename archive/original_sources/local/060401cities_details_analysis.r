# Original source: 060401cities_details_analysis.r
# Original source location: <LOCAL_CODE_ROOT>/060401cities_details_analysis.r
# Original source SHA-256: 016f9f9b83ff9b1dce7ede9d23e557ba5a0deb0c9a8c7b821aca6ebb6f973c30
# Public-safe original snapshot: archive/original_sources/local/060401cities_details_analysis.r
# Classification: SUPPORTING
# Publication modifications:
# - machine-specific absolute path literals replaced by configured publication_path aliases
# - local encoding decoded to UTF-8; source bytes hashed above
# - comments and historical private machine paths sanitized
# - working-directory calls retained with configured paths; no analyses executed
# Scientific modification: NONE
# This is full historical source, not a certified unattended/reproducible run.
# Snapshot for inspection; path placeholders are not executable locations.
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggtext)
library(dunn.test) # 用于非参数多重比较
library(rstatix)   # 用于更方便的统计标注
library(multcompView)
library(stringr)
library(patchwork)
library(ggsci)
library(forcats)
library(openxlsx)
library(tibble)
target_lib <- "<HISTORICAL_PATH_006>"
install.packages(c("cli", "rlang", "vctrs", "lifecycle"), lib = target_lib)
install.packages(c("dplyr", "tidyr", "tibble", "pillar"), lib = target_lib)
packageVersion("ggplot2")
install.packages(c("dplyr", "tidyr", "tibble"))
update.packages('ggplot2')
remotes::install_github("nanxstats/ggsci")
df_analysis <- read.csv("<LOCAL_ANALYSIS_ROOT>/data/organized_table/0502_risk_city_netscoreStdVersion.csv",
               stringsAsFactors = FALSE)
head(df)
# ---- 1) 改成你真实列名 ----
# 例：假设你的列名就是这些；不一样就把右边替换成你的列名
df <- df %>%
  rename(
    City      = City,
    Continent = Continent,     # 或 Region
    Direct    = Direct_Effect,
    Indirect  = Indirect_Effect,
    Net       = Total_Effect,
    Pressure  = Pressure,
    Dependency = Dependency_mech
    # 如果没有这一列，见下一段
  )

# 如果没有 Dependency，就生成
if(!("Dependency" %in% names(df))){
  df <- df %>% mutate(Dependency = Net / Pressure)
}
median(  df$Pressure )
# ---- 2) 设定 Fig.5b 的阈值（必须与你作图一致）----
thr_pressure <- median(df$Pressure)  # 例子：你自己替换
thr_dep      <- 0.5  # 例子：你自己替换

# ---- 3) 全局：direct/indirect/net ----
direct_neg_pct <- 100 * mean(df$Direct < 0, na.rm = TRUE)
direct_med <- median(df$Direct, na.rm = TRUE)
direct_q1  <- quantile(df$Direct, 0.25, na.rm = TRUE)
direct_q3  <- quantile(df$Direct, 0.75, na.rm = TRUE)

ind_pos_pct <- 100 * mean(df$Indirect > 0, na.rm = TRUE)
ind_mag_med <- median(abs(df$Indirect), na.rm = TRUE)
ind_mag_q1  <- quantile(abs(df$Indirect), 0.25, na.rm = TRUE)
ind_mag_q3  <- quantile(abs(df$Indirect), 0.75, na.rm = TRUE)

net_pos_n   <- sum(df$Net > 0, na.rm = TRUE)
net_pos_pct <- 100 * mean(df$Net > 0, na.rm = TRUE)

# ---- 4) 各洲对比 ----
cont <- df %>%
  group_by(Continent) %>%
  summarise(
    n = n(),
    med_direct = median(Direct, na.rm = TRUE),
    med_indirect = median(Indirect, na.rm = TRUE),
    med_net = median(Net, na.rm = TRUE),
    net_sum = sum(Net, na.rm = TRUE),
    prop_net_pos = mean(Net > 0, na.rm = TRUE)
  ) %>% ungroup()

asia <- cont %>% filter(grepl("Asia", Continent, ignore.case = TRUE))
others <- cont %>% filter(!grepl("Asia", Continent, ignore.case = TRUE))

# “factor ~X–X”：用亚洲 median net / 其他洲 median net 最大值（方案1）
factor1_low <- as.numeric(asia$med_net) / max(others$med_net, na.rm = TRUE)

# 或者用 net_sum（方案2）
factor2_low <- as.numeric(asia$net_sum) / max(others$net_sum, na.rm = TRUE)

# 其他洲 median net 的范围（用于“clustered near zero”）
others_med_net_min <- min(others$med_net, na.rm = TRUE)
others_med_net_max <- max(others$med_net, na.rm = TRUE)

# “fewer than X% strongly positive”：定义 strong positive 阈值
thr_strong_net <- quantile(df$Net, 0.75, na.rm = TRUE)
others_strong_pos_pct <- 100 * mean(df$Net > thr_strong_net & !grepl("Asia", df$Continent, ignore.case=TRUE), na.rm = TRUE)

# ---- 5) dependency 框架：四象限与关键比例 ----
low_dep_pct <- 100 * mean(df$Dependency < thr_dep, na.rm = TRUE)

recl_sensitive_n <- sum(df$Dependency >= thr_dep & df$Pressure < thr_pressure, na.rm = TRUE)

dep_range_sensitive <- df %>%
  filter(Dependency >= thr_dep, Pressure < thr_pressure) %>%
  summarise(
    dep_pct_q1 = quantile(Dependency, 0.25, na.rm = TRUE) * 100,
    dep_pct_q3 = quantile(Dependency, 0.75, na.rm = TRUE) * 100,
    dep_pct_min = min(Dependency, na.rm = TRUE) * 100,
    dep_pct_max = max(Dependency, na.rm = TRUE) * 100
  )

# ---- 6) Shanghai 专项 ----
sh <- df %>% filter(grepl("^Shanghai$", City, ignore.case = TRUE))

sh_pressure_pctile <- ecdf(df$Pressure)(sh$Pressure)    # 0-1
sh_top_pct <- (1 - sh_pressure_pctile) * 100
sh_dep_pct <- sh$Dependency * 100

hi_hi <- df %>% filter(Pressure >= thr_pressure, Dependency >= thr_dep)

# ---- 输出 ----
cat("\nDIRECT negative %:", round(direct_neg_pct, 1),
    " median:", round(direct_med, 3),
    " IQR:", round(direct_q1, 3), "to", round(direct_q3, 3), "\n")

cat("INDIRECT positive %:", round(ind_pos_pct, 1),
    " |magnitude| median:", round(ind_mag_med, 3),
    " IQR:", round(ind_mag_q1, 3), "to", round(ind_mag_q3, 3), "\n")

cat("NET positive:", net_pos_n, "cities (", round(net_pos_pct, 1), "%)\n")

cat("\nCONTINENT summary:\n"); print(cont)

cat("\nAsia median direct:", round(asia$med_direct, 3),
    " median indirect:", round(asia$med_indirect, 3),
    " median net:", round(asia$med_net, 3), "\n")

cat("Factor (median net Asia / max median net others):", round(factor1_low, 2), "\n")
cat("Factor (net sum Asia / max net sum others):", round(factor2_low, 2), "\n")

cat("Others median net range:", round(others_med_net_min, 3), "to", round(others_med_net_max, 3), "\n")
cat("Others strongly positive net % (Net > global 75th):", round(others_strong_pos_pct, 1), "\n")

cat("\nLow dependency %:", round(low_dep_pct, 1), "\n")
cat("Reclamation-sensitive cities (high dep, lower pressure):", recl_sensitive_n, "\n")
print(dep_range_sensitive)

cat("\nHigh-pressure & high-dependency cities n:", nrow(hi_hi), "\n")
print(hi_hi[, c("City","Continent","Pressure","Dependency","Net","Direct","Indirect")])

cat("\nShanghai top % (by Pressure):", round(sh_top_pct, 1),
    " Shanghai dependency %:", round(sh_dep_pct, 1), "\n")
# 3. 确定阈值 (Define Thresholds)
# X轴阈值：Invasion Pressure 的中位数
x_threshold <- median(df_analysis$Pressure, na.rm = TRUE)

# Y轴阈值：Dependency = 0.5 (即50%)
# 【自动修正逻辑】：防止数据是百分制(0-100)而阈值是小数(0.5)
# 如果数据最大值大于 1，说明是百分制，阈值自动设为 50；否则设为 0.5
y_threshold <- if(max(df_analysis$Dependency_mech, na.rm=TRUE) > 1.5) 50 else 0.5

# 4. 划分象限 (Add Quadrant Column)
df_final <- df %>%
  mutate(
    # 添加象限标签 (详细描述)
    Quadrant_Label = case_when(
      #  (右上)：高风险 + 高依赖 (填海是主谋)
      Pressure >= x_threshold & Dependency >= y_threshold ~ "Q1: Amplified Hotspots",
      
      #  (左上)：低风险 + 高依赖 (潜在风险/填海主导)
      Pressure < x_threshold & Dependency >= y_threshold  ~ "Q2: Latent Risk",
      
      #  (左下)：低风险 + 低依赖 (安全区/背景主导)
      Pressure < x_threshold & Dependency < y_threshold   ~ "Q3: Safe Coldspots",
      
      #  (右下)：高风险 + 低依赖 (由于气候好等原因导致的高风险)
      Pressure >= x_threshold & Dependency < y_threshold  ~ "Q4: Natural Hotspots"
    ),
    
    # 添加简单的象限数字 (1, 2, 3, 4) 方便画图
    Quadrant_Num = case_when(
      Pressure >= x_threshold & Dependency >= y_threshold ~ 1,
      Pressure < x_threshold & Dependency >= y_threshold  ~ 2,
      Pressure < x_threshold & Dependency < y_threshold   ~ 3,
      Pressure >= x_threshold & Dependency < y_threshold  ~ 4
    )
  )

# 5. 查看结果分布
cat("\n各象限城市数量统计：\n")
print(table(df_analysis$Quadrant_Label))

head(df_final)
df_analysis <- df_analysis %>%
  mutate(N_dir = ifelse(Total_Effect >= 0, "Amplifying (N>0)", "Suppressive (N<0)"))
p <- ggplot(df_analysis, aes(x = Pressure, y = Dependency_mech)) +
  
  # --- A. 绘制象限分割线 (放在最底层) ---
  # 使用虚线，颜色淡一点，不要抢视觉重点
  geom_vline(xintercept = x_threshold, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  geom_hline(yintercept = 50 , linetype = "dashed", color = "gray50", linewidth = 0.5) +
  
  # --- B. 绘制气泡 (核心层) ---
  geom_point(aes(size = Reclamation_area,   
                 fill = N_dir,     # 填充色映射
                 color = N_dir),    # 颜色映射：象限标签                   # 21号形状：支持填色+边框             # 白色边框 (高对比度)
             stroke = 0.6, 
             shape=21,                 # 边框粗细
             alpha = 0.85) +                # 轻微透明
  
  # --- C. 智能标注城市名称 ---
  # # 策略：标注面积比较大，或者位置比较极端的点
  # geom_text_repel(
  #   data = subset(df_analysis, Reclamation_area > mean(Reclamation_area, na.rm=TRUE)), 
  #   aes(label = City, color = N_dir),
  #   size = 3,                 # 字号适中
  #   fontface = "bold",        # 字体加粗
  #   bg.color = "white",       # 文字周围加白边(Halo)，防止看不清
  #   bg.r = 0.15,              # 白边半径
  #   box.padding = 0.5,        # 文字框间距
  #   max.overlaps = 30,        # 允许重叠的最大数量
  #   show.legend = FALSE       # 标签不显示在图例里
  # ) +
  
  # --- D. 标尺调整 (Scales) ---
  
#  scale_fill_manual(name = "Quadrant", values = c("#7B5C90", "#5D7894", "#A2B7A8", "#C0485A")) +
#   scale_color_manual(name = "Quadrant", values = c("#7B5C90", "#5D7894", "#A2B7A8", "#C0485A")) +
   scale_fill_manual(name = "Net reclamation effect", values = c("#5D7894", "#C0485A")) +
  scale_color_manual(name = "Net reclamation effect", values = c("#5D7894",  "#C0485A")) +
  # 大小：按面积缩放 (关键！)
  scale_size_area(
   range = c(4, 16),                # 控制最大气泡的视觉尺寸 (根据画布大小微调)
    breaks = c(100, 1000, 10000, 100000), # 图例刻度：根据你数据范围手动调整
    name = "Reclamation\nArea (ha)"
  ) +

  # 坐标轴：适当扩展边界，防止最大的球被切掉
  scale_x_continuous(expand = expansion(mult = 0.02)) + 
  # scale_y_continuous(expand = expansion(mult = 0.1)) +

  # --- E. 美化主题 (Theme) ---
  theme_classic() +  # 经典黑白主题，最符合学术规范
  theme(
    # 字体统一
    text = element_text(family = "sans", color = "black"),
    
    # 坐标轴标题与刻度
    axis.title = element_text(size = 22, face = "bold"),
    axis.text = element_text(size = 22, color = "black"),
    
    # 图例设置 (放在右侧)
    legend.position = "right", 
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 5),
    legend.background = element_blank(),
    
    # 边框
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  ) +
  
  # --- F. 标签 ---
  labs(
    x = "Pressure",    # 记得写清楚 X 轴含义
    y = "Dependency Index (Socio-economic)",     # 记得写清楚 Y 轴含义
    title = "Socio-ecological Trade-offs"        # 标题
  )

# 3. 输出预览
print(p)
windows()
ggsave("<LOCAL_ANALYSIS_ROOT>/figure/gam_draft/2colors_mech.png", p, width = 10, height = 8.5, dpi = 900,device = grDevices::png)



write.csv(df_analysis,"<LOCAL_ANALYSIS_ROOT>/data/organized_table/0501_risk_city_netscorewithQ.csv")
quadrant_stats <- df_final %>%
  # 确保分类是因子，并指定合理的顺序 (用于画图堆叠顺序)
  mutate(Risk_Typology = factor(Quadrant_Label, levels = c(
    "Q1: Amplified Hotspots", # 右上 (最红)
    "Q4: Natural Hotspots",  # 右下 (深蓝)
    "Q2: Latent Risk", # 左上 (浅红/橙)
    "Q3: Safe Coldspots"                 # 左下 (浅灰/浅蓝)
  ))) %>%
  # 分组统计
  group_by(Continent, Risk_Typology) %>%
  summarise(Count = n(), .groups = "drop_last") %>%
  # 计算百分比
  mutate(Percentage = Count / sum(Count)) %>%
  ungroup()

# 查看计算结果表
print(quadrant_stats)
# 定义对应四个象限的颜色 (保持与散点图一致)
type_colors <- c(
  "Q1: Amplified Hotspots" = "#E64B35", # 红 (人为热点)
  "Q2: Natural Hotspots"  = "#3C5488", # 深蓝 (天然热点)
  "Q3: Latent Risk" = "#F39B7F", # 浅红 (潜在风险)
  "Q4: Safe Coldspots"                = "#91D1C2"  # 浅绿/灰 (安全区)
)

p_composition <- ggplot(quadrant_stats, aes(x = Continent, y = Percentage, fill = Risk_Typology)) +
  
  # 堆叠条形图
  geom_bar(stat = "identity", width = 0.7, color = "white") +
  
  # 添加百分比数字标签 (只显示 > 5% 的，避免太挤)
  geom_text(aes(label = ifelse(Percentage > 0.05, scales::percent(Percentage, accuracy = 1), "")),
            position = position_stack(vjust = 0.5), size = 3, color = "white", fontface = "bold") +
  
  # 翻转坐标轴 (便于阅读大洲名字)
  coord_flip() +
  
  # 颜色映射
  scale_fill_manual(values = type_colors, name = NULL) +
  
  # Y轴百分比格式
  scale_y_continuous(labels = scales::percent, expand = c(0, 0)) +
  
  # 标题与主题
  labs(
    title = "<b>Regional Composition of Invasion Risk Types</b>",
    subtitle = "Proportion of cities in each quadrant of the risk-attribution typology.",
    x = NULL, y = "Percentage of Cities"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_markdown(face = "bold"),
    legend.position = "top",
    legend.text = element_text(size = 9),
    #legend.key.size = unit(0.4, "cm"),
    legend.box = "vertical", # 图例如果太长就换行
    panel.grid.major.y = element_blank()
  )


# ==============================================================================
# 步骤 1：合并为七大洲 (Data Preparation)
# ==============================================================================
# 假设 df_final 是你包含 Regions 列的原始数据
df_total_dummy <- df_analysis %>%
  mutate(Regions= "Total")

df_analysis <- df_updated_2 %>%
  mutate(
    Regions = case_when(
      # 亚洲
      str_detect(Regions, "Asia") ~ "Asia",
      # 欧洲
      str_detect(Regions, "Europe") ~ "Europe",
      # 非洲
      str_detect(Regions, "Africa") ~ "Africa",
      # 北美 (含中美)
      str_detect(Regions, "North America|Central America") ~ "North America",
      # 南美
      str_detect(Regions, "South America") ~ "South America",
      # 大洋洲
      str_detect(Regions, "Oceania") ~ "Oceania",
      TRUE ~ "Other"
    )
  ) %>%
  # 过滤掉数据太少无法统计的组 (比如 Other)
  filter(Regions != "Other")

# ==============================================================================
# 2. 定义通用统计函数 (The Magic Function)
# ==============================================================================
# 这个函数会自动完成：计算中位数 -> 差异检验 -> 生成字母 -> 输出格式化列
get_stats_column <- function(data, var_name, display_name) {
  
  # A. 提取数据
  sub_df <- data %>% select(Regions, Value = all_of(var_name))
  
  # B. 统计检验 (Kruskal-Wallis + Dunn)
  # 如果所有组的值都一样，或者只有一组，跳过检验
  if (n_distinct(sub_df$Regions) > 1) {
    dunn_res <- dunn_test(sub_df, Value ~ Regions, p.adjust.method = "bonferroni")
    
    # 转换为字母
    p_vec <- dunn_res$p.adj
    names(p_vec) <- paste(dunn_res$group1, dunn_res$group2, sep = "-")
    letters <- multcompLetters(p_vec)$Letters
    
    letter_df <- data.frame(Regions = names(letters), Letter = letters)
  } else {
    letter_df <- data.frame(Regions = unique(sub_df$Regions), Letter = "")
  }
  
  # C. 计算汇总统计量
  stats <- sub_df %>%
    group_by(Regions) %>%
    summarise(
      Median = median(Value, na.rm = TRUE),
      IQR_25 = quantile(Value, 0.25, na.rm = TRUE),
      IQR_75 = quantile(Value, 0.75, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(letter_df, by = "Regions") %>%
    # 格式化: "0.12 (0.05-0.20) a"
    mutate(
      !!display_name := paste0(
  sprintf("%.3f", Median), 
  " (", sprintf("%.3f", IQR_25), "-", sprintf("%.3f", IQR_75), ")",
  ifelse(Letter != "", paste0(" ", Letter), "")
)
    ) %>%
    select(Regions, !!display_name)
  
  return(stats)
}

# ==============================================================================
# 3. 批量执行并合并表格
# ==============================================================================
df_analysis_all <- bind_rows(df_analysis, df_total_dummy)

# (可选) 设置因子顺序，确保 "Total" 永远排在表格的最后一行
df_analysis_all$Regions<- fct_relevel(as.factor(df_analysis_all$Regions), "Total", after = Inf)

# ==========================================================
# 2. 接下来的代码几乎不用动，只需要把输入数据换成 df_analysis_all
# ==========================================================

# 计算 N (样本量)
base_table <- df_analysis_all %>%
  group_by(Regions) %>%
  summarise(N = n()) %>%
  arrange(Regions) # 确保排序

# 需要分析的变量清单
target_vars <- c(
  "Reclamation_area" = "Reclamation Area (km2)",
  "Pressure"         = "Total Invasion Risk",
  "Direct_Effect"           = "Direct Effect (Cost)",
  "Indirect_Effect"         = "Indirect Effect (Benefit)",
  "Total_Effect"              = "Net Effect"
)

# 循环合并
final_si_table <- base_table

for (v in names(target_vars)) {
  cat("正在分析:", target_vars[v], "...\n")
  
  # 注意：这里传入的是含有 Total 的全量数据集 df_analysis_all
  # 只要你的 get_stats_column 函数内部是基于 group_by(Continent) 计算的，
  # 它就会自动把 "Total" 这一组也算出来
  temp_res <- get_stats_column(df_analysis_all, v, target_vars[v])
  
  final_si_table <- left_join(final_si_table, temp_res, by = "Regions")
}

# 查看结果
print(final_si_table)
# ==============================================================================
# 4. 查看并保存
# ==============================================================================
print(head(final_si_table))
final_si_table
write.xlsx(final_si_table, "<LOCAL_ANALYSIS_ROOT>/data/organized_table/06SI_Table_Comprehensive_Regional_Stats.xlsx")
plot_long <- df_analysis %>%
  select(Continent, Reclamation_area, Pressure, Direct, Indirect, Net) %>%
  pivot_longer(cols = -Continent, names_to = "Metric", values_to = "Value") %>%
  mutate(
    Metric = factor(Metric, 
                    levels = c("reclamation_area", "Pressure", "Direct", "Indirect", "Net"),
                    labels = c("a | Reclamation Area", "b | Total Risk", 
                               "c | Direct Effect", "d | Indirect Effect", "e | Net Effect"))
  )
final_si_table
p_multi <- ggplot(plot_long, aes(x = reorder(Continent, Value, median), y = Value, fill = Continent)) +
  
  # 1. 抖动点 (展示样本量不平衡)
  geom_jitter(width = 0.2, alpha = 0.3, color = "grey40", size = 0.8) +
  
  # 2. 箱线图
  geom_boxplot(width = 0.6, alpha = 0.8, outlier.shape = NA) +
  
  # 3. 分面展示
  facet_wrap(~Metric, scales = "free", ncol = 3) +
  
  # 4. 美化
  scale_fill_brewer(palette = "Set3") +
  coord_flip() + # 翻转坐标轴，大洲名字横着写
  labs(x = NULL, y = "Value (Units vary by metric)") +
  
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 11, hjust = 0),
    panel.border = element_rect(color = "grey", fill = NA)
  )

print(p_multi)
windows()
