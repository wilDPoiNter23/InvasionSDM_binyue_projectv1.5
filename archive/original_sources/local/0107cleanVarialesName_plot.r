# Original source: 0107cleanVarialesName_plot.r
# Original source location: <LOCAL_CODE_ROOT>/0107cleanVarialesName_plot.r
# Original source SHA-256: 6b3d2afa64bb2b9a4c6aa6df941530cf4260f537d8646b57654afe3e76ddf9e5
# Public-safe original snapshot: archive/original_sources/local/0107cleanVarialesName_plot.r
# Classification: SUPPORTING
# Publication modifications:
# - machine-specific absolute path literals replaced by configured publication_path aliases
# - local encoding decoded to UTF-8; source bytes hashed above
# - comments and historical private machine paths sanitized
# - working-directory calls retained with configured paths; no analyses executed
# Scientific modification: NONE
# This is full historical source, not a certified unattended/reproducible run.
# Snapshot for inspection; path placeholders are not executable locations.
library(dplyr)
library(stringr)
library(viridis)
library(terra)
library(ggplot2)
library(tidyterra)
library(forcats)
data <- read.csv("<LOCAL_DATABASE_ROOT>/evaluations/importance/importance_summary_cleaned.csv")
data <- read.csv("<LOCAL_ANALYSIS_ROOT>/data/organized_table/02_importance_summary_cleaned.csv")
# 温度型变量：蓝 -> 白 -> 红（无绿色）
pal_temp <- colorRampPalette(c("#2166AC", "#F7F7F7", "#B2182B"))

# “强度/密度”型（比如 built-up, GDP）：浅黄 -> 橙 -> 深棕
pal_intensity <- colorRampPalette(c("#fff7bc00", "#FEC44F", "#D95F0E", "#8C2D04"))

# 土壤 / SOC：浅灰绿 -> 深棕绿
pal_soil <- colorRampPalette(c("#F7FCF5", "#C7E9C0", "#74C476", "#238B45", "#00441B"))
pal_pop <- colorRampPalette(c(
  "#F7FBFF",  # very light blue
  "#51a3e6",  # light blue
  "#2d64da",  # medium blue
  "#b81c7c",  # deep blue
  "#be0a0a"   # deep purple
))
# 如果懒得选，直接用 viridis
pal_viridis <- function(n) {
  viridis::viridis(n, option="E")
}
# 1) 建一个变量信息表 ---------------------------------------------------
nature_palette <- c(
  "Climate"        = "#61998B", # 暖橙色 (气候/温度)
  "Soil"           = "#d58d3a", # 土褐色 (土壤)
  "Topography"     = "#EDE6C1", # 青岩色 (山地/地形)
  "Marine"         = "#A4D1DB", # 海洋蓝 (水体)
  "Anthropogenic" = "#F06256" # 砖红色 (人为/城市)

)
var_info <- tibble::tribble(
  ~raw,                           ~label,                                      ~category,
  # Climate
  "wc2.1_2.5m_bio_1",  "Annual mean temperature (°C)",              "Climate",
  "wc2.1_2.5m_bio_2",  "Mean diurnal temperature range (°C)",       "Climate",
  "wc2.1_2.5m_bio_3",  "Isothermality (%)",                         "Climate",
  "wc2.1_2.5m_bio_4",  "Temperature seasonality (SD × 100)",        "Climate",
  "wc2.1_2.5m_bio_5",  "Max temperature of warmest month (°C)",     "Climate",
  "wc2.1_2.5m_bio_6",  "Min temperature of coldest month (°C)",     "Climate",
  "wc2.1_2.5m_bio_7",  "Temperature annual range (°C)",             "Climate",
  "wc2.1_2.5m_bio_8",  "Mean temperature of wettest quarter (°C)",  "Climate",
  "wc2.1_2.5m_bio_9",  "Mean temperature of driest quarter (°C)",   "Climate",
  "wc2.1_2.5m_bio_10", "Mean temperature of warmest quarter (°C)",  "Climate",
  "wc2.1_2.5m_bio_11", "Mean temperature of coldest quarter (°C)",  "Climate",
  "wc2.1_2.5m_bio_12", "Annual precipitation (mm)",                 "Climate",
  "wc2.1_2.5m_bio_13", "Precipitation of wettest month (mm)",       "Climate",
  "wc2.1_2.5m_bio_14", "Precipitation of driest month (mm)",        "Climate",
  "wc2.1_2.5m_bio_15", "Precipitation seasonality (CV)",            "Climate",
  "wc2.1_2.5m_bio_16", "Precipitation of wettest quarter (mm)",     "Climate",
  "wc2.1_2.5m_bio_17", "Precipitation of driest quarter (mm)",      "Climate",
  "wc2.1_2.5m_bio_18", "Precipitation of warmest quarter (mm)",     "Climate",
  "wc2.1_2.5m_bio_19", "Precipitation of coldest quarter (mm)",     "Climate",
  # Soil
  "bdod_mean_2024_2_5min_aligned",      "Soil bulk density (g cm⁻³)",       "Soil",
  "cfvo_mean_2024_2_5min_aligned",      "Coarse fragments volume (%)",      "Soil",
  "clay_mean_2024_2_5min_aligned",      "Clay content (%)",                 "Soil",
  "sand_mean_2024_2_5min_aligned",      "Sand content (%)",                 "Soil",
  "silt_mean_2024_2_5min_aligned",      "Silt content (%)",                 "Soil",
  "nitrogen_mean_2024_2_5min_aligned",  "Soil nitrogen (g kg⁻¹)",           "Soil",
  "ocd_mean_2024_2_5min_aligned",       "SOC density (kg m⁻²)",             "Soil",
  "soc_mean_2024_2_5min_aligned",       "SOC content (g kg⁻¹)",             "Soil",
  "phh2o_mean_2024_2_5min_aligned",     "Soil pH (H₂O)",                    "Soil",
  # Socio-economic
  "BUILT_S_E2025_2.5min",   "Built-up fraction",              "Anthropogenic",
  "GDP2020_2.5min",         "GDP density",          "Anthropogenic",
  "Pop2020_2.5min",         "Population density","Anthropogenic",
  "VIIRS_2023_2.5min",      "Night-time lights",       "Anthropogenic",
  "reclamation_raster",     "Reclamation area",                      "Anthropogenic",
  # Topography
  "wc2.1_2.5m_elev", "Elevation (m)", "Topography",
  "slope",           "Slope (°)",     "Topography",
  "aspect",          "Aspect (°)",    "Topography",
  # Marine
  "salinity_mean",   "Mean sea surface salinity",     "Marine",
  "salinity_range",  "Sea surface salinity range",    "Marine",
  "temperature_mean","Mean sea surface temperature (°C)", "Marine",
  "temperature_range","Sea surface temperature range (°C)","Marine"
)

# --- 2. 定义辅助函数 ---
pretty_label <- function(x) {
  out <- var_info$label[match(x, var_info$raw)]
  out[is.na(out)] <- x[is.na(out)] # 找不到对应名字的保留原名
  out
}

get_category <- function(x) {
  out <- var_info$category[match(x, var_info$raw)]
  out[is.na(out)] <- "Other" # 找不到分类的归为 Other
  out
}

# --- 3. 定义自定义绘图主题 (透明背景 + 边框) ---
theme_transparent_box <- function() {
  theme_minimal(base_size = 14) +
    theme(
      # 1. 背景透明设置
      plot.background = element_rect(fill = "transparent", color = NA), 
      panel.background = element_rect(fill = "transparent", color = NA),
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.box.background = element_rect(fill = "transparent", color = NA),
      
      # 2. 四周线框设置 (关键点)
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1.2),
      
      # 3. 去掉网格线 (可选，为了更干净，或者保留)
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      
      # 4. 字体颜色 (保证在不同背景下可见)
      text = element_text(color = "black"),
      axis.text = element_text(color = "black"),
      
      # 图例
      legend.position = "bottom",
      legend.title = element_text(face = "bold")
    )
}
# 数据清洗与重命名
plot_data <- data %>%
  mutate(
    # 调用你的函数进行转换
    Label = pretty_label(Variable),
    Category = get_category(Variable)
  ) %>%
  # 过滤掉 Importance 为 NA 的行（如果有）
  filter(!is.na(Importance))

# 查看转换结果是否正确
head(plot_data)


  # 通用函数：给一个 raster 文件名，画全球图 -------------------------
plot_global_raster <- function(
  tif_path,
  var_raw_name,      # 例如 "wc2.1_2.5m_bio_1"
  palette_fun,
  ncol = 256,
  agg_factor = 4     # 聚合因子，可根据机器调：4, 8, 12...
) {
  # 读栅格
  r <- terra::rast(tif_path)
  
  # 适度降分辨率（否则 ggplot 会很慢）
  if (!is.null(agg_factor) && agg_factor > 1) {
    r <- terra::aggregate(r, fact = agg_factor, fun = "mean", na.value = "transparent")
  }
  
  # 转成 data.frame 供 ggplot 使用
  df <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
  colname <- names(df)[3]
  
  # 取得人话 label
  lab <- pretty_label(var_raw_name)
  
  # 配色
  pal <- palette_fun(ncol)
  
  ggplot(df) +
    geom_raster(aes(x = x, y = y, fill = .data[[colname]])) +
    scale_fill_gradientn(colours = pal, name = lab,na.value = "transparent", trans = "log1p") +
    coord_equal() +
    theme_minimal(base_size = 11) +
    theme(
      axis.title = element_blank(),
      panel.grid = element_blank(),
      legend.position = "right"
    )
}

plot_global_raster <- function(
  tif_path,
  var_raw_name,
  palette_fun,
  ncol = 256,
  agg_factor = 4
) {
  r <- terra::rast(tif_path)
  
  if (!is.null(agg_factor) && agg_factor > 1) {
    r <- terra::aggregate(r, fact = agg_factor, fun = "mean", na.rm = TRUE)
  }
  
  df <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
  colname <- names(df)[3]
  
  # 【改动1】过滤掉 0 值，去掉难看的背景色块
  df <- df[df[[colname]] > 0, ]
  
  lab <- pretty_label(var_raw_name)
  pal <- palette_fun(ncol)
  
  ggplot(df) +
    geom_raster(aes(x = x, y = y, fill = .data[[colname]])) +
    scale_fill_gradientn(
        colours = pal, 
        name = lab, 
        na.value = "transparent"
        # trans = "log1p" # 【改动2】这里推荐用 sqrt (比较柔和) 或者 log1p (对比更强烈)
    ) +
    coord_equal() +
    theme_minimal(base_size = 11) +
    theme(
      axis.title = element_blank(),
      panel.grid = element_blank(),
      legend.position = "right",
      panel.background = element_blank(),
      plot.background = element_blank()
    )
}
# for viirs
plot_global_raster <- function(
  tif_path,
  var_raw_name,
  palette_fun,
  ncol = 256,
  agg_factor = 4
) {
  r <- terra::rast(tif_path)
  
  if (!is.null(agg_factor) && agg_factor > 1) {
    r <- terra::aggregate(r, fact = agg_factor, fun = "mean", na.rm = TRUE)
  }
  
  df <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
  colname <- names(df)[3]
  
  # 【步骤1】去掉 0 值（背景）
  df <- df[df[[colname]] > 0, ]
  
  # 【步骤2：核心修正】计算 99% 分位数，截断极值
  # 意思是：找到那个数字，全世界 99% 的灯光都比它暗
  limit_high <- quantile(df[[colname]], 0.99, na.rm = TRUE)
  
  # 把所有大于这个值的点，强行赋值为这个值
  # 这样色标就不会被那些超级亮点带偏了
  df[[colname]][df[[colname]] > limit_high] <- limit_high
  
  lab <- pretty_label(var_raw_name)
  pal <- palette_fun(ncol)
  
  ggplot(df) +
    geom_raster(aes(x = x, y = y, fill = .data[[colname]])) +
    
    scale_fill_gradientn(
        colours = pal, 
        name = lab, 
        na.value = "transparent",
        # 【步骤3】这里依然保留 log 变换，效果会更好，
        # 或者是 sqrt，取决于截断后数据的分布
        trans = "sqrt", 
        
        # 让图例显示更好看：告诉读者这个最大值其实代表 ">= X"
        labels = scales::label_number(accuracy = 1)
    ) +
    
    coord_equal() +
    theme_minimal(base_size = 11) +
    theme(
      axis.title = element_blank(),
      panel.grid = element_blank(),
      legend.position = "right",
     panel.background = element_rect(fill = "black", color = NA),
  plot.background = element_rect(fill = "white", color = NA) # 外部保持白底
    )
}
p_salinity <- plot_global_raster(
  tif_path = "<LOCAL_ANALYSIS_ROOT>/data/data/Topo/wc2.1_2.5m_elev.tif",
  var_raw_name = "wc2.1_2.5m_elev",
  palette_fun = terrain.colors,,
  agg_factor = 4   # 可以视机器调大调小
)
p_salinity
ggsave("<LOCAL_AUX_ROOT>/ppt/svg-pic/elev_forppt.pdf", p_salinity, width = 8, height = 6, dpi = 600)


#---------for importance value plot----------------
var_order <- plot_data %>%
  group_by(Label) %>%
  summarise(median_imp = median(Importance)) %>%
  arrange(desc(median_imp)) %>%
  head(25) %>% 
  pull(Label)

# 绘图
p1 <- plot_data %>%
  filter(Label %in% var_order) %>%
  ggplot(aes(x = factor(Label, levels = rev(var_order)), y = Importance)) +
  
  # 箱线图：使用 fill 映射 Category
  geom_boxplot(aes(fill = Category), 
               outlier.size = 0.8, 
               outlier.alpha = 0.6,
               alpha = 0.8,          # 设置透明度让颜色更柔和
               width = 0.7,
               color = "black") +    # 箱线图边框为黑色
  
  coord_flip() +
  
  # 核心：使用 scale_fill_manual 应用自定义颜色
  scale_fill_manual(values = nature_palette) +
  
  labs(
    title = "Top 25 Environmental Drivers",
    subtitle = "Ranked by median importance across all species",
    x = NULL,
    y = "Importance Score"
  ) +
theme_transparent_box()
windows()
p1
ggsave("<LOCAL_ANALYSIS_ROOT>/figure/sdm_evalueation/plot1_importance.png", plot = p1, width = 10, height = 8, bg = "transparent", dpi = 300)
ggsave("<LOCAL_ANALYSIS_ROOT>/figure/sdm_evalueation/plot1_importance.svg", plot = p1, width = 10, height = 8, bg = "transparent", dpi = 300)
# -------------------------------------------------------------------------
# 图表 2: 不同驱动因子类别的重要性对比 (Violin Plot)
# 目的: 比较气候、人为、土壤等大类因素的影响力
# -------------------------------------------------------------------------

p2 <- ggplot(plot_data, aes(x = Category, y = Importance, fill = Category)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  labs(
    title = "Importance Distribution by Variable Category",
    subtitle = "Comparison of Anthropogenic vs. Natural drivers",
    x = "Category",
    y = "Importance Score"
  ) +
  theme_bw() +
  theme(legend.position = "none")
p2
print(p2)

# -------------------------------------------------------------------------
# 图表 3: 模型来源对比 (Ensemble vs Individual)
# 目的: 检查集成模型是否倾向于给某些变量更低或更高的权重
# -------------------------------------------------------------------------

# 计算每个变量在不同来源下的平均重要性
source_comp <- clean_data %>%
  group_by(Category, Source) %>%
  summarise(Mean_Importance = mean(Importance), .groups = 'drop')
  # --- 2. 计算统计差异 (Analysis Part) ---
# 我们计算每种 Source 在每个 Category 下的：
# 1. 中位数 (Median): 代表中心趋势
# 2. 四分位距 (IQR): 代表数据的离散程度（不确定性）
# 3. 极值比率: 重要性 > 0.5 的极端值数量

stats_summary <- plot_data %>%
  group_by(Category, Source) %>%
  summarise(
    Median_Imp = median(Importance),
    IQR_Imp = IQR(Importance), # 四分位距，越大说明波动越大
    Mean_Imp = mean(Importance),
    Max_Imp = max(Importance),
    .groups = 'drop'
  ) %>%
  arrange(Category, Source)
print(as.data.frame(stats_summary))


p3 <- ggplot(plot_data, aes(x = Category, y = Importance, fill = Source)) + # 如果没有geom_split_violin，可以用普通的 dodged boxplot
  # 这里使用标准的 boxplot 替代，因为 split violin 需要额外的辅助函数
  geom_boxplot(position = position_dodge(width = 0.8), width = 0.6, outlier.size = 0.2) +
  labs(
    title = "Variable Importance: Ensemble vs. Individual Models",
    x = "Variable Category",
    y = "Importance Score"
  ) +
  scale_fill_manual(values = c("ensemble" = "#D4AD93", "individual" = "#957EAC")) +
  theme_light()+theme_minimal(base_size = 24) +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.border = element_rect(color = "black", fill = NA, size = 2),
    
    # --- 修改了这里：一键去掉所有网格线 ---
    panel.grid = element_blank(),
    # ------------------------------------
    
    legend.position = "bottom",
    
    text = element_text(color = "black"),
    axis.text = element_text(color = "black", size = 22),
    plot.title = element_text(face = "bold", size = 14)
  )
p3
print(p3)
ggsave("<LOCAL_ANALYSIS_ROOT>/figure/sdm_evalueation/plot3_Ensemble_vs_Individual.png", plot = p3, width = 10, height = 8, bg = "transparent", dpi = 300,
  device = grDevices::png  )
ggsave("<LOCAL_ANALYSIS_ROOT>/figure/sdm_evalueation/plot3_Ensemble_vs_Individual.pdf", plot = p3, width = 10, height = 8, bg = "transparent", dpi = 300)
# -------------------------------------------------------------------------
# p4- 频次柱状图,设定阈值：只统计每个物种重要性排名前 5 的变量
top_drivers_data <- plot_data %>%
  group_by(Species) %>%
  arrange(desc(Importance)) %>%
  slice_head(n = 5) %>% # 取前5名
  ungroup() %>%
  # 统计每个变量出现的次数
  count(Label, Category, name = "Frequency") %>%
  # 只展示出现频次最高的前 20 个变量，避免图表太拥挤
  arrange(desc(Frequency)) %>%
  head(20) 

# -------------------------------------------------------------------------
# 绘图：频次柱状图 (Nature Style)
# -------------------------------------------------------------------------

p_freq <- ggplot(top_drivers_data, aes(x = reorder(Label, Frequency), y = Frequency, fill = Category)) +
  
  # 柱状图
  geom_bar(stat = "identity", 
           width = 0.7, 
           alpha = 0.8,       # 稍微透明
           color = "black",   # 黑色边框
           size = 0.6) +      # 边框粗细
  
  # 翻转坐标轴，方便阅读长标签
  coord_flip() +
  
  # 应用之前的 Nature 配色
  scale_fill_manual(values = nature_palette) +
  
  # 扩展 Y 轴，防止标签顶到边框
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  
  labs(
    title = "Frequency of Top 5 Drivers Across Species",
    subtitle = "Number of species where the variable ranks in the top 5 importance",
    x = NULL,
    y = "Count of Species"
  ) +
theme_transparent_box() +
  
  # 针对柱状图的微调
  theme(
    panel.grid.major.y = element_blank(), # 去掉水平网格线（翻转后是垂直的）
    legend.position = "bottom"
  )
p_freq
windows()
ggsave("<LOCAL_ANALYSIS_ROOT>/figure/sdm_evalueation/plot4_Frequency.png", plot = p_freq, width = 10, height = 8, bg = "transparent", dpi = 300)
ggsave("<LOCAL_ANALYSIS_ROOT>/figure/sdm_evalueation/plot4_Frequency.svg", plot = p_freq, width = 10, height = 8, bg = "transparent", dpi = 300)
