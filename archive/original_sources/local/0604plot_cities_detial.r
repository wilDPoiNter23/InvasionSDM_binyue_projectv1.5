# Original source: 0604plot_cities_detial.r
# Original source location: <LOCAL_CODE_ROOT>/0604plot_cities_detial.r
# Original source SHA-256: 3959969d0ade6fc2cbf9c62cd56622572907b4c9b9a7eceae6a0148a748b6add
# Public-safe original snapshot: archive/original_sources/local/0604plot_cities_detial.r
# Classification: SUPPORTING
# Publication modifications:
# - machine-specific absolute path literals replaced by configured publication_path aliases
# - local encoding decoded to UTF-8; source bytes hashed above
# - comments and historical private machine paths sanitized
# - working-directory calls retained with configured paths; no analyses executed
# Scientific modification: NONE
# This is full historical source, not a certified unattended/reproducible run.
# Snapshot for inspection; path placeholders are not executable locations.
# ====== 加载包 ======
library(plotbiomes)
library(ggplot2)
library(dplyr)
library(readr)
library(scales)
library(scico)
library(viridis) 
library(tidyr)
library(ggtext) # 用于高级文本渲染
library(patchwork)
library(ggrepel)
library(grid)
library(stringr)
library(ggbreak)
install.packages('ggbreak')
devtools::install_github("valentinitnelav/plotbiomes")
# ====== 获取 Whittaker 多边形数据 ======

df <- read.csv("<LOCAL_ANALYSIS_ROOT>/data/organized_table/0502_risk_city_netscoreStdVersion.csv")

# 1) 构造城市级分析表
df <- df %>%    # 每行=城市
  mutate(
    diff = mean_risk_reclamation - mean_risk_non_reclamation,
    recl_pct = reclamation_percentage/100
  ) %>%
  select(city, diff, recl_pct, latitude, pop, gdp, climate_code)  # 示例

num_vars <- c("diff","recl_pct","latitude","pop","gdp","climate_code")

# 2) 相关矩阵（右侧热图）
cor_mat <- cor(df[, num_vars], use = "pairwise.complete.obs")
cor_long <- as.data.frame(as.table(cor_mat)) %>% 
  rename(var1=Var1, var2=Var2, r=Freq)

p_cor <- ggplot(cor_long, aes(var1, var2, fill = r)) +
  geom_tile() +
  scale_fill_gradient2(limits=c(-1,1)) +
  coord_equal() +
  theme_minimal() +
  theme(axis.title=element_blank())

# 3) 标准化回归（左侧箭头）
std <- function(x) as.numeric(scale(x))
fit <- lm(std(diff) ~ std(recl_pct) + std(latitude) + std(pop) + std(gdp) + std(climate_code),
          data = df)
coef_tab <- tidy(fit) %>% filter(term != "(Intercept)") %>%
  mutate(beta = estimate,
         p = p.value,
         sig = ifelse(p < 0.05, "<0.05", ">=0.05"),
         sign = ifelse(beta >= 0, "pos", "neg"))

# 手动放置节点坐标
nodes <- tibble::tibble(
  term = c("std(recl_pct)","std(latitude)","std(pop)","std(gdp)","std(climate_code)"),
  x = 0, y = c(5,4,3,2,1)
)
target <- tibble::tibble(label="diff", x=4, y=3)

edges <- coef_tab %>%
  left_join(nodes, by=c("term")) %>%
  mutate(xend = target$x, yend = target$y)

p_paths <- ggplot() +
  geom_point(data = nodes, aes(x,y), size=2) +
  geom_point(data = target, aes(x,y), size=3) +
  geom_curve(data = edges,
             aes(x=x, y=y, xend=xend, yend=yend,
                 size = abs(beta),
                 colour = sign,
                 linetype = sig),
             curvature = 0.2, arrow = arrow(length = unit(2,"mm"))) +
  scale_size(range = c(0.3, 1.6), guide="none") +
  scale_colour_manual(values=c(pos="#1f78b4", neg="#e66101"), guide="none") +
  scale_linetype_manual(values=c("<0.05"="solid", ">=0.05"="dashed"), guide="none") +
  theme_minimal() + labs(x=NULL,y=NULL)

# 组合：左路径 + 右相关热图（用 patchwork 或 cowplot）
library(patchwork)
p_a_like <- p_paths + p_cor + plot_layout(widths = c(1,1.2))



# ====== 构建主变量 ======
df <- city_df %>%
  mutate(
    temperature = wc2.1_2.5m_bio_1,
    precipitation = wc2.1_2.5m_bio_12 / 10,  # 转换为 cm
    reclamation_area = reclamation_area
  )

# ====== Biome 背景数据（近似定义） ======

# ====== 绘图 ======
# ==== 准备需要的变量 ====
df$MAT <- df$`wc2.1_2.5m_bio_1`  # Mean Annual Temperature
df$MAP <- df$`wc2.1_2.5m_bio_12` / 10  # Annual Precipitation (mm -> cm)
df$reclamation_area <- df$reclamation_area
max(df$reclamation_area)
# ==== 设置颜色渐变（根据围填海面积） ====
fill_col <- scales::col_numeric(
  palette = c("#b460b4", "#fd8d3c", "#bd0026"),
  domain = df$reclamation_area
)
# 对数刻度：自动生成好看的 log10 刻度
rng   <- range(df$reclamation_area, na.rm = TRUE)
rng
brks  <- log_breaks(base = 10, n = 6)(rng)  # 6 个左右的整数量级刻度
labsF <- label_number(accuracy = 1, scale_cut = cut_si("k"))

p <- whittaker_base_plot() + guides(fill = "none") +                # 隐藏背景 biome 的填充图例
  ggnewscale::new_scale_fill() +         # 开启新的 fill 标度，专供点使用
  geom_point(
    data = df,
    aes(x = MAT, y = MAP, fill = reclamation_area),
    shape = 21, size = 5.8, colour = "black", stroke = 0.30, alpha = 0.9
  ) + 
  # ---- 关键：连续型填色标度（选择一种即可） ----
  # 方案 A：scico 高级配色（推荐 "batlow" 或 "hawaii"）
  scale_fill_scico(
    palette = "vik",    # 可换 "hawaii" / "roma"
    trans   = "log10",
    breaks  = brks,
    labels  = label_number(accuracy = 1, big.mark = ""),  # 1000 而非 1,000；无小数
    name    = "Reclamation area (km²)"
  )+
  labs(x = "Temperature (°C)", y = "Precipitation (cm)") +
  # ==== 主题设置 ====
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(fill = NA),
    axis.title = element_text(size = 16, face = "bold", colour = "black"),
    axis.text = element_text(size = 14, colour = "black"),
    legend.position.inside = c(0.02, 0.85),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = alpha("white", 0.7)),
    legend.key.height = unit(0.6, "cm"),
    legend.key.width = unit(0.4, "cm")
  ) +
  labs(
    x = "Mean Annual Temperature (°C)",
    y = "Annual Precipitation (cm)",
    title = "Distribution of Coastal Reclamation Cities in Climate Biomes"
  )
p
# ==== 导出图像 ====
ggsave("Fig1_Whittaker_Biome_Cities.png", p, width = 8, height = 6, dpi = 600)

# -------------------------------------------------------------------------
# 数据预处理：核心步骤是创建“分栏”变量
# -------------------------------------------------------------------------
plot_data_prep <- df %>%
  # A. 按 Risk 降序排列
  arrange(desc(risk)) %>%
  # B. 创建排序索引 (1, 2, 3...)
  mutate(rank_idx = row_number()) %>%
  # C. 创建分栏组 (假设分成 3 栏，每栏放总数/3 个)
  mutate(
    # n() 是总行数， ceiling 向上取整
    # 这会生成 "1-30", "31-60", "61-90" 这样的组
    facet_group = ceiling(rank_idx / (n() / 3)) 
  ) %>%
  # D. 将 city 转为因子，确保画图顺序是对的
  mutate(city = reorder(city, risk))
plot_data_prep
# E. 转换为长格式用于画条形 (只转换 direct 和 indirect)
plot_data_long <- plot_data_prep %>%
  pivot_longer(cols = c(Effect_Direct, Ind_Socio), 
               names_to = "Effect_Type", 
               values_to = "Effect_Value")
plot_data_long 
# -------------------------------------------------------------------------
# 美学定义
# -------------------------------------------------------------------------
# 颜色：Direct(红), Indirect(蓝), Net(黑点), Risk(填充色)
col_direct   <- "#E64B35" 
col_indirect <- "#4DBBD5"
col_net      <- "black"

# -------------------------------------------------------------------------
# 绘图
# -------------------------------------------------------------------------
p <- ggplot() +
  
  # --- 1. 零线 ---
  geom_vline(xintercept = 0, color = "grey60", linetype = "dotted", linewidth = 0.4) +
  
  # --- 2. 条形图 (Direct & Indirect) ---
  # 使用 geom_segment 绘制
  geom_segment(data = plot_data_long,
               aes(y = city, yend = city, x = 0, xend = Effect_Value, color = Effect_Type),
               linewidth = 3, alpha = 0.7) + # linewidth 控制条形宽度
  
  # --- 3. 净效应点图 (Net Effect) ---
  # 放在条形图上面
  geom_point(data = plot_data_prep,
             aes(y = city, x = Effect_Indirect_Total, fill = risk),
             shape = 21,    # 实心圆带边框
             color = "black", # 边框颜色
             stroke = 0.4,  # 边框粗细
             size = 2.5) +  # 点的大小
  
  # --- 4. 分栏 (关键) ---
  # scales="free_y" 让每一栏只显示自己的城市
  facet_wrap(~facet_group, scales = "free_y", ncol = 3) +
  
  # --- 5. 颜色映射 ---
  # 条形颜色
 scale_color_manual(
    # 【修改点】键名必须和 pivot_longer 之前的列名完全一致！
    values = c("Effect_Direct" = col_direct, "Ind_Socio" = col_indirect),
    
    # 标签可以自定义，这是显示在图例上的文字
    labels = c("Direct Effect", "Indirect Effect"),
    name = NULL
  ) +
  # 风险颜色 (点填充)
  scale_fill_gradientn(
    colors = c("#fcfdbf", "#fc8961", "#b73779", "#51127c"), # Magma 风格
    name = "Total Risk"
  ) +
  
  # --- 6. 标签与坐标 ---
  labs(
    title = "<b>Risk Decomposition: Direct, Indirect, and Net Effects</b>",
    subtitle = "Cities are ordered by total invasion risk (high to low). <span style='color:#E64B35;'>Red bars</span>: Direct effects; <span style='color:#4DBBD5;'>Blue bars</span>: Indirect effects; <b>Circles</b>: Net outcome.",
    x = "Standardized Effect Magnitude",
    y = NULL
  ) +
  
  # --- 7. 主题设置 ---
  theme_minimal(base_size = 11) +
  theme(
    # 字体
    text = element_text(color = "black", family = "sans"),
    
    # 标题富文本
    plot.title = element_markdown(face = "bold", size = 14),
    plot.subtitle = element_markdown(size = 10, color = "grey30", margin = margin(b = 10)),
    
    # 分栏标题隐藏 (我们不需要显示 1, 2, 3)
    strip.text = element_blank(),
    
    # 网格线优化
    panel.grid.major.y = element_line(color = "grey95", linewidth = 0.2),
    panel.grid.major.x = element_line(color = "grey90", linewidth = 0.2),
    panel.grid.minor = element_blank(),
    
    # 调整间距
    panel.spacing.x = unit(1, "cm"), # 栏与栏之间的距离
    
    # 图例
    legend.position = "top",
    legend.box = "horizontal",
    legend.margin = margin(b = -5),
    
    # 坐标轴文字
    axis.text.y = element_text(size = 8, hjust = 1)
  )
windows()
p 
# 2. 数据清洗与预处理
plot_data <- df %>%
  # 筛选填海效应显著的 Top 20 城市 (否则80个挤在一起看不清)
  # 使用 Indirect_Total 排序
  arrange(desc(Effect_Indirect_Total)) %>%
  slice_head(n = 20) %>%
  # 转换为长格式以便堆叠
  pivot_longer(cols = c(Ind_Socio, Ind_Soil, Effect_Direct), 
               names_to = "Component", values_to = "Value") %>%
  mutate(
    # 设置因子顺序，保证画图顺序
    Component = factor(Component, levels = c("Ind_Socio", "Ind_Soil", "Effect_Direct")),
    # 城市按总效应排序
    city = reorder(city, abs(Value))
  )

# ==============================================================================
# 图 1: 机制分解棒棒糖图 (Top 20 Cities)
# ==============================================================================
# 定义颜色：Direct(红), Socio(深蓝), Soil(浅蓝)
cols <- c("Effect_Direct" = "#E64B35", "Ind_Socio" = "#3C5488", "Ind_Soil" = "#4DBBD5")

p1 <- ggplot(plot_data, aes(y = city, x = Value, fill = Component)) +
  # 零线
  geom_vline(xintercept = 0, color = "grey60", linetype = "dashed") +
  
  # 堆叠条形图 (Direct是负的，Ind是正的，会自动分开)
  geom_bar(stat = "identity", width = 0.6, alpha = 0.9) +
  
  # 标注净效应点 (Net Effect)
  # 需要从原始df里取这20个城市的 Net Effect (Direct + Ind_Total)
  geom_point(data = df %>% arrange(desc(Effect_Indirect_Total)) %>% slice_head(n=20),
             aes(y = reorder(city, Effect_Indirect_Total), 
                 x = Effect_Direct + Effect_Indirect_Total, 
                 fill = NULL), 
             color = "black", size = 3, shape = 18) + # 菱形代表净效应
  
  scale_fill_manual(values = cols, 
                    labels = c("Direct (Habitat Loss)", "Indirect (Socioeconomic)", "Indirect (Soil)")) +
  
  labs(
    title = "a | Mechanistic decomposition of reclamation impacts",
    subtitle = "Top 20 cities ranked by reclamation intensity. ♦ = Net Effect.",
    x = "Standardized Effect Magnitude", y = NULL, fill = NULL
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold")
  )

# ==============================================================================
# 图 2: 区域不平衡性 (Regional Disparity)
# ==============================================================================
# 使用完整数据
p2 <- ggplot(df, aes(x = Regions, y = Effect_Indirect_Total, fill = Regions)) +
  geom_jitter(width = 0.2, alpha = 0.3, color = "grey50") +
  geom_boxplot(width = 0.5, outlier.shape = NA, alpha = 0.8) +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "b | Regional disparity in reclamation-induced risk",
    y = "Total Indirect Effect (Facilitation)", x = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )
p2
# ==============================================================================
# 组合图表
# ==============================================================================
# 使用 patchwork 拼图：左边是大图(p1)，右边是小图(p2)
final_plot <- p1 + p2 + plot_layout(widths = c(1.5, 1))
final_plot 


explore_data <- df %>%
  mutate(
    # 计算填海净效应
    Net_Recl_Effect = Effect_Direct + Effect_Indirect_Total,
    
    # 计算填海对总风险的"贡献率" (注意：risk是概率，Net是效应值，量纲不同，主要看相对大小)
    # 为了可视化，我们看两者的比值
    Recl_Contribution = Net_Recl_Effect / risk
  )

# ==============================================================================
# 图 A: 填海驱动力分析 (Scater Plot)
# X轴：填海净效应 (Reclamation Net Effect)
# Y轴：总入侵风险 (Total Risk)
# ==============================================================================

p_driver <- ggplot(explore_data, aes(x = Net_Recl_Effect, y = risk)) +
  # 辅助线
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
  
  # 散点
  geom_point(aes(fill = Regions, size = reclamation_area), 
             shape = 21, color = "white", alpha = 0.8) +
  
  # 标签 (只标出偏离对角线的，或者特别极端的)
  geom_text_repel(data = filter(explore_data, Net_Recl_Effect > 0.1 | risk > 0.4), 
                  aes(label = city), size = 3) +
  
  scale_size_continuous(range = c(2, 8), guide = "none") +
  scale_fill_brewer(palette = "Set3") +
  
  labs(
    title = "Is Risk Driven by Reclamation?",
    subtitle = "Cities above the line have risks driven by other factors (Climate etc.)\nCities on the right are heavily driven by Reclamation.",
    x = "Net Effect of Reclamation (Model Estimated)",
    y = "Total Invasion Risk"
  ) +
  theme_minimal()
p_driver
print(p_driver)
p_region_contrib <- ggplot(explore_data, aes(x = reorder(Regions, Net_Recl_Effect, median), 
                                             y = Net_Recl_Effect, 
                                             fill = Regions)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.5, size = 1) +
  
  scale_fill_brewer(palette = "Set3") +
  
  labs(
    title = "Regional Disparity in Reclamation Impact",
    subtitle = "East Asia is the hotspot where reclamation actively drives invasion risk",
    x = NULL,
    y = "Net Reclamation Effect (Risk Increase)"
  ) +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

print(p_region_contrib)
# 1. 数据准备 (归一化 + 非标准化系数)
# 假设 b_direct = -0.562, b_indirect = 0.971 (社会经济) + 0.193 (土壤) = 1.164
b_direct <- -0.562
b_indirect <- 1.164

plot_data_vector <- df %>%
  mutate(
    # 归一化填海 (0-1)
    Recl_Norm = (reclamation_area - min(reclamation_area, na.rm=TRUE)) / 
                (max(reclamation_area, na.rm=TRUE) - min(reclamation_area, na.rm=TRUE)),
    # 计算效应
    Direct = Recl_Norm * b_direct,
    Indirect = Recl_Norm * b_indirect,
    Net = Direct + Indirect
  ) %>%
  # 只标记 Top 10 城市和几个典型非东亚城市
  mutate(Label = ifelse(rank(desc(Recl_Norm)) <= 10 | city %in% c("Dubai", "Singapore", "Dar Es Salaam"), city, NA))

# 2. 绘图
p_vector <- ggplot(plot_data_vector) +
  
  # --- 背景区域划分 ---
  # 绿色区域：Net < 0 (抑制区，但在你的系数下不存在)
  # 红色区域：Net > 0 (促进区)
  geom_abline(intercept = 0, slope = abs(b_direct)/b_indirect, linetype = "dotted", color = "grey") +
  
  # --- 矢量箭头 (核心) ---
  geom_segment(aes(x = 0, y = 0, xend = Direct, yend = Indirect, color = Regions), 
               arrow = arrow(length = unit(0.2, "cm")), size = 0.8, alpha = 0.7) +
  
  # --- 终点散点 ---
  geom_point(aes(x = Direct, y = Indirect, fill = Regions, size = risk), 
             shape = 21, color = "white", stroke = 0.5) +
  
  # --- 标签 ---
  geom_text_repel(aes(x = Direct, y = Indirect, label = Label), 
                  size = 3, fontface = "italic", box.padding = 0.5, max.overlaps = 20) +
  
  # --- 辅助说明 ---
  annotate("text", x = -0.1, y = 0.8, label = "Net Risk Amplification\n(Indirect > |Direct|)", 
           color = "grey50", fontface = "bold", hjust = 0.5) +
  
  # --- 标尺与颜色 ---
  scale_color_brewer(palette = "Set1", name = "Region") +
  scale_fill_brewer(palette = "Set1", name = "Region") +
  scale_size_continuous(range = c(1, 6), name = "Total Risk") +
  
  # --- 坐标轴与主题 ---
  labs(
    title = "<b>Mechanistic Vector of Reclamation Impacts</b>",
    subtitle = "Reclamation exerts a consistent 'pull' towards risk amplification driven by socioeconomic facilitation.",
    x = "Ecological Suppression (Habitat Loss) [-]",
    y = "Socioeconomic Facilitation (Propagule Pressure) [+]"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_markdown(face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.grid.minor = element_blank()
  )

print(p_vector)
# 1. 数据筛选 (选 Top 25 填海城市)
plot_data_bar <- plot_data_vector %>%
  arrange(desc(Recl_Norm)) %>%
  slice_head(n = 25) %>%
  mutate(city = reorder(city, Recl_Norm))

# 2. 绘图
p_balance <- ggplot(plot_data_bar) +
  # 0线
  geom_hline(yintercept = 0, color = "black") +
  
  # 间接效应 (收益，向上)
  geom_col(aes(x = city, y = Indirect), fill = "#4DBBD5", alpha = 0.8, width = 0.6) +
  
  # 直接效应 (代价，向下)
  geom_col(aes(x = city, y = Direct), fill = "#E64B35", alpha = 0.8, width = 0.6) +
  
  # 净效应 (点)
  geom_point(aes(x = city, y = Net), shape = 18, size = 4, color = "black") +
  
  # 连接线 (可选，增加视觉连贯性)
  geom_segment(aes(x = city, xend = city, y = Direct, yend = Indirect), 
               color = "grey30", size = 0.2, alpha = 0.5) +
  
  coord_flip() +
  
  labs(
    title = "<b>The Ecological Balance Sheet of Reclamation</b>",
    subtitle = "<span style='color:#4DBBD5;'><b>Socioeconomic Benefit</b></span> vs. <span style='color:#E64B35;'><b>Ecological Cost</b></span>. <br>Diamond (♦) indicates Net Invasion Pressure.",
    y = "Effect Magnitude", x = NULL
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_markdown(face = "bold"),
    plot.subtitle = element_markdown(),
    panel.grid.major.y = element_blank()
  )
print(p_balance)

# ==============================================================================
# 1. 数据准备：区域合并与效应计算
# ==============================================================================
names(df)
continent_data <- df %>%
  # mutate(
  #   # --- A. 定义大洲归类逻辑 ---
  #   Continent = case_when(
  #     str_detect(Regions, "Asia") ~ "Asia",
  #     str_detect(Regions, "Europe") ~ "Europe",
  #     str_detect(Regions, "Africa") ~ "Africa",
  #     str_detect(Regions, "America") ~ "America",
  #     str_detect(Regions, "Oceania") ~ "Oceania",
  #     TRUE ~ "Other" # 防止有漏网之鱼
  #   ),
    
  #   # --- B. 计算每个城市的效应 (基于非标准化系数) ---
  #   # 归一化填海强度 (0-1)
  #   Recl_Norm = (reclamation_area - min(reclamation_area, na.rm=TRUE)) / 
  #               (max(reclamation_area, na.rm=TRUE) - min(reclamation_area, na.rm=TRUE)),
    
  #   # 计算三股力量
  #   Eff_Direct = Recl_Norm * (-0.562),     # 物理抑制 (Cost)
  #   Eff_Socio  = Recl_Norm * (0.971),      # 经济促进 (Benefit 1)
  #   Eff_Soil   = Recl_Norm * (0.193),      # 土壤促进 (Benefit 2)
  #   Eff_Net    = Eff_Direct + Eff_Socio + Eff_Soil # 净效应
  # ) %>%
  
  # --- C. 按大洲聚合取平均值 ---
  group_by(Regions) %>%
  summarise(
    Mean_Direct = mean(Direct_Effect, na.rm=TRUE),
    Mean_Socio  = mean(Ind_Socio, na.rm=TRUE),
    Mean_Soil   = mean(Ind_Soil, na.rm=TRUE),
    Mean_Net    = mean(Total_Effect, na.rm=TRUE),
    n_cities    = n() # 记录每个洲有多少城市
  ) %>%
  # 过滤掉 "Other" 或空值
  filter(Regions != "Other") %>%
  # 转换为长格式用于堆叠
  pivot_longer(cols = c(Mean_Direct, Mean_Socio, Mean_Soil), 
               names_to = "Component", values_to = "Value") %>%
  mutate(
    # 设置堆叠顺序：Soil 在最上面，Socio 在中间，Direct 单独在左边
    Component = factor(Component, levels = c("Mean_Soil", "Mean_Socio", "Mean_Direct"))
  )

# 提取 Net 数据用于画点
net_points_cont <- continent_data %>%
  select(Regions, Mean_Net) %>%
  distinct()

continent_data <- df %>%
  # --- C. 按大洲聚合取median ---
  group_by(Regions) %>%
  summarise(
    Mean_Direct = median(Direct_Effect, na.rm=TRUE),
    Mean_Socio  = median(Ind_Socio, na.rm=TRUE),
    Mean_Soil   = median(Ind_Soil, na.rm=TRUE),
    Mean_Net    = median(Total_Effect, na.rm=TRUE),
    n_cities    = n() # 记录每个洲有多少城市
  ) %>%
  # 过滤掉 "Other" 或空值
  filter(Regions != "Other") %>%
  # 转换为长格式用于堆叠
  pivot_longer(cols = c(Mean_Direct, Mean_Socio, Mean_Soil), 
               names_to = "Component", values_to = "Value") %>%
  mutate(
    # 设置堆叠顺序：Soil 在最上面，Socio 在中间，Direct 单独在左边
    Component = factor(Component, levels = c("Mean_Soil", "Mean_Socio", "Mean_Direct"))
  )

# 提取 Net 数据用于画点
net_points_cont <- continent_data %>%
  select(Regions, Mean_Net) %>%
  distinct()
# ==============================================================================
# 2. 绘图：大洲级非对称堆叠图
# ==============================================================================

# 定义高级配色
cols <- c("Mean_Direct" = "#E64B35", # 红 (代价)
          "Mean_Socio" = "#3C5488",  # 深蓝 (经济)
          "Mean_Soil" = "#4DBBD5")   # 浅蓝 (土壤)

p_continent <- ggplot() +
  
  # --- 0线 ---
  geom_vline(xintercept = 0, color = "black", linewidth = 0.6) +
  
  # --- 1. 条形图 ---
  geom_col(data = continent_data, 
           aes(x = Value, y = reorder(Regions, Mean_Net), fill = Component),
           width = 0.5, alpha = 0.9) +
  
  # --- 2. 净效应点 ---
  geom_point(data = net_points_cont,
             aes(x = Mean_Net, y = Continent),
             shape = 18, size = 6, color = "black") + # 菱形大黑点
  
  # --- 3. 颜色设置 ---
  scale_fill_manual(
    values = cols,
    labels = c("Soil Facilitation (+)", "Socioeconomic Facilitation (+)", "Physical Constraint (-)")
  ) +
  
  # --- 4. 辅助标注 ---
  annotate("text", x = min(continent_data$Value)*0.9, y = 0.6, 
           label = "Ecological Cost", fontface = "bold", color = "#E64B35", size = 4) +
  annotate("text", x = max(continent_data$Value)*0.9, y = 0.6, 
           label = "Cumulative Benefit", fontface = "bold", color = "#3C5488", size = 4) +
  
  # --- 5. 坐标轴与标题 ---
  labs(
    title = "<b>Continental Breakdown of Reclamation Mechanisms</b>",
    subtitle = "Interplay of <span style='color:#E64B35;'>Direct Constraints</span> vs. <span style='color:#3C5488;'>Socioeconomic</span> & <span style='color:#4DBBD5;'>Soil</span> Facilitation.<br>♦ = Net Risk Outcome.",
    x = "Mean Standardized Effect Magnitude",
    y = NULL,
    fill = NULL
  ) +
  
  # --- 6. 主题 ---
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_markdown(face = "bold", size = 16),
    plot.subtitle = element_markdown(size = 12, color = "grey30"),
    
    # 坐标轴文字
    axis.text.y = element_text(size = 12, face = "bold", color = "black"),
    axis.text.x = element_text(size = 10),
    
    # 去除多余网格
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    
    # 加黑框
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.2),
    
    # 图例设置
    legend.position = "top",
    legend.justification = "right",
    legend.box = "horizontal",
    legend.margin = margin(b = -10)
  )

# 打印与保存
print(p_continent)

continent_data


plot_data_attrib <- df %>%
  mutate(
    # A. 重新计算净效应 (非标准化, 确保逻辑一致)
    Recl_Norm = (reclamation_area - min(reclamation_area, na.rm=TRUE)) / 
                (max(reclamation_area, na.rm=TRUE) - min(reclamation_area, na.rm=TRUE)),
    Effect_Net_Real = Recl_Norm * 0.602, # 你的真实净系数
    
    # B. 计算归因比例 (Dependency)
    # 注意：如果 E_net > Risk (模型非线性导致)，截断为 100%，这在解释"责任"时是合理的
    # 意为：当前的风险完全(100%)由填海解释
    Dependency_Pct = (Effect_Net_Real / risk) * 100,
    Dependency_Pct = ifelse(Dependency_Pct > 100, 100, Dependency_Pct)
  ) %>%
  # 定义分类 (用于画图颜色或形状)
  mutate(
    City_Type = case_when(
      risk > median(risk) & Dependency_Pct > 50 ~ "Man-made Hotspot",  # 高风险，主要怪填海
      risk > median(risk) & Dependency_Pct <= 50 ~ "Natural Hotspot",   # 高风险，主要怪背景
      TRUE ~ "Low Risk / Latent"
    )
  )

# 筛选标签
label_data <- plot_data_attrib %>%
  filter(risk > quantile(risk, 0.85) | Dependency_Pct > 80)
label_data 
# 2. 绘图
p_attrib_scatter <- ggplot(plot_data_attrib, aes(x = risk, y = Dependency_Pct)) +
  
  # --- 背景分区 (可选，用虚线划分) ---
  geom_hline(yintercept = 50, linetype = "dashed", color = "grey80") +
  geom_vline(xintercept = median(plot_data_attrib$risk), linetype = "dashed", color = "grey80") +
  
  # --- 散点 ---
  geom_point(aes(fill = City_Type, size = reclamation_area), 
             shape = 21, color = "white", stroke = 0.5, alpha = 0.85) +
  
  # --- 标签 ---
  geom_text_repel(data = label_data, aes(label = city), 
                  size = 8, box.padding = 0.5, max.overlaps = 20) +
  
  # # --- 区域注释 ---
  # annotate("text", x = max(df$risk), y = 100, label = "Man-made\nHotspots", 
  #          color = "#E64B35", fontface = "bold", hjust = 1, vjust = 1, size = 9) +
  # annotate("text", x = max(df$risk), y = 0, label = "Natural/Background\nHotspots", 
  #          color = "#3C5488", fontface = "bold", hjust = 1, vjust = 0, size = 9) +
  
  # --- 颜色与标尺 ---
  scale_fill_manual(values = c(
    "Man-made Hotspot" = "#E64B35", # 红色警报
    "Natural Hotspot" = "#3C5488",  # 蓝色背景
    "Low Risk / Latent" = "grey70"  # 灰色
  ), name = "City Typology") +
  
  scale_size_continuous(range = c(2, 10), guide = "none") +
  
  # --- 标题与坐标 ---
  labs(
    title = "<b>Who is responsible for the risk?</b>",
    subtitle = "Differentiating <span style='color:#E64B35;'>Reclamation-Driven</span> vs. <span style='color:#3C5488;'>Background-Driven</span> invasion risks.",
    x = "Total Invasion Risk (Current Status)",
    y = "Reclamation Dependency (% of Risk Attributed to Reclamation)"
  ) +
  
  theme_minimal(base_size = 24) +
  theme(
    plot.title = element_markdown(face = "bold"),
    plot.subtitle = element_markdown(color = "grey30"),
       panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.grid = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top"
  )

print(p_attrib_scatter)
ggsave( "<LOCAL_ANALYSIS_ROOT>/figure/gam_draft/plot5.2_reclamationDependency.png", plot = p_attrib_scatter, width = 10, height = 12, bg = "transparent", dpi = 300)

plot_data_main <- continent_data %>%
  # 确保 Component 因子顺序正确
  mutate(Component = factor(Component, levels = c("Mean_Soil", "Mean_Socio", "Mean_Direct")))

# 2. 定义颜色
cols <- c("Mean_Direct" = "#4C6E72", "Mean_Socio" = "#D2807D", "Mean_Soil" = "#EADDC9")

# 3. 绘制主图 (包含所有大洲)
p_main <- ggplot() +
  geom_vline(xintercept = 0, color = "black", linewidth = 1) +
  
  # 条形
  geom_col(data = plot_data_main, 
           aes(x = Value, y = reorder(Regions, Mean_Net), fill = Component),
           width = 0.8, alpha = 0.9) +
  
  # 净效应点
  geom_point(data = net_points_cont,
             aes(x = Mean_Net, y = Regions),
             shape = 18, size = 12, color = "black") +
  
  scale_fill_manual(values = cols) +
  
  labs(
    title = "<b>b | Regional Asymmetry in Reclamation Impacts</b>",
    subtitle = "East Asia dominates global risk, overshadowing other regions.",
    x = "Mean Standardized Effect Magnitude", y = NULL
  ) +
  
  theme_minimal(base_size = 25) +
  theme(
    plot.title = element_markdown(face = "bold"),
    legend.position = "top",
       panel.grid = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.5)
  )

# 4. 绘制插图 (剔除 East Asia/Asia)
# 找出数值最大的那个洲 (假设是 Asia)
max_continent <- "Asia" # 或者根据数据自动判断

p_inset <- ggplot() +
  geom_vline(xintercept = 0, color = "black", linewidth = 1) +
  # 过滤掉最大的洲
  geom_col(data = plot_data_main %>% filter(Regions != max_continent), 
           aes(x = Value, y = reorder(Regions, Mean_Net), fill = Component),
           width = 0.8, alpha = 0.9) +
  
  geom_point(data = net_points_cont %>% filter(Regions != max_continent),
             aes(x = Mean_Net, y = Regions),
             shape = 18, size = 5, color = "black") +
  
  scale_fill_manual(values = cols) +
  
  labs(title = "Other Regions", x = NULL, y = NULL,size = 15) +
  
  theme_minimal(base_size = 25) +
  theme(
    legend.position = "none", # 插图不要图例
     panel.grid = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "grey50", fill = NA, linewidth = 1.5), # 插图加个灰框
    plot.background = element_rect(fill = "white", color = NA) # 插图背景设为白，遮挡主图网格
  )

# 5. 组合 (把插图贴在主图的空白处)
# inset_element(图, 左, 下, 右, 上) 范围是 0-1
# 根据你的数据分布，东亚条很长，所以右下角或者是右上角通常是空的
final_plot <- p_main + inset_element(p_inset, 0.4, 0.05, 0.95, 0.55)
windows()
print(p_main)
ggsave( "<LOCAL_ANALYSIS_ROOT>/figure/gam_draft/plot5.1_Mediannetscore_regional.png", plot = p_main, width = 14, height = 13, bg = "transparent", dpi = 900)

p_break <- ggplot() +
  geom_vline(xintercept = 0, color = "black") +
  
  geom_col(data = plot_data_main, 
           aes(x = Value, y = reorder(Continent, Mean_Net), fill = Component),
           width = 0.6, alpha = 0.9) +
  
  geom_point(data = net_points_cont,
             aes(x = Mean_Net, y = Continent),
             shape = 18, size = 5, color = "black") +
  
  scale_fill_manual(values = cols) +
  
  # --- 核心：设置断点 ---
  # 假设其他洲都在 -0.2 到 0.2 之间，只有亚洲到了 1.0
  # 我们把 0.3 到 0.8 这一段切掉 (根据你的数据调整)
  scale_x_break(c(0.13, 1.0), scale = 0.5) + 
  
  labs(
    title = "Regional Mechanism Breakdown (Broken Axis)",
    x = "Effect Magnitude", y = NULL
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    panel.grid.major.y = element_blank()
  )

print(p_break)
