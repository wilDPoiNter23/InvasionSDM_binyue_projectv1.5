# Original source: 040603mapResponseCurve.r
# Original source location: <LOCAL_CODE_ROOT>/040603mapResponseCurve.r
# Original source SHA-256: 8fa1099a0cf6d623950a4fa9a2ab509e90bdffd97bc30af6ba292cda1f6783a6
# Public-safe original snapshot: archive/original_sources/local/040603mapResponseCurve.r
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
library(stringr)
library(tools)
# 1. 确保数据没问题 (如果之前跑过提取代码，直接用 big_response_df)
getwd()
while(!is.null(dev.list())) dev.off()
csv_folder <- "<LOCAL_AUX_ROOT>/Response_Curves_Plots/Response_Data_CSVs/" 
file_list <- list.files(path= csv_folder, pattern = "\\.csv$", full.names = TRUE)
file_list
big_response_df <- read.csv("<LOCAL_AUX_ROOT>/Response_Curves_Plots/Response_Data_CSVs/Response_Data_Pop2020_2.5min.csv")
scientifi_10 <- function(x) {
  func <- function(y) {
    # 【修复点】先检查是不是 NA，如果是直接返回 NA
    if (is.na(y)) return(NA)
    # 再检查是不是 0
    if (y == 0) return("0")
    # 正常的转换
    parse(text=gsub("e\\+?", " %*% 10^", scales::scientific_format()(y)))
  }
  return(lapply(x, func))
}
bg_color <- "#113A42" 
# 趋势线颜色 (推荐：Nature橙 #E64B35 或 亮红 #D55E00)
trend_color <- "#A13C46" 

# --- 1. 定义变量信息表 (你的最新版本) ---
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
  "GDP2020_2.5min",         "GDP density",                    "Anthropogenic",
  "Pop2020_2.5min",         "Population density",             "Anthropogenic",
  "VIIRS_2023_2.5min",      "Night-time lights",              "Anthropogenic",
  "reclamation_raster",     "Reclamation area",               "Anthropogenic",
  # Topography
  "wc2.1_2.5m_elev", "Elevation (m)", "Topography",
  "slope",           "Slope (°)",     "Topography",
  "aspect",          "Aspect (°)",    "Topography",
  # Marine
  "salinity_mean",   "Mean sea surface salinity",             "Marine",
  "salinity_range",  "Sea surface salinity range",            "Marine",
  "temperature_mean","Mean sea surface temperature (°C)",     "Marine",
  "temperature_range","Sea surface temperature range (°C)",   "Marine"
)

# --- 2. 定义查找小助手 (Helper Function) ---
# 给它一个原始文件名，它返回漂亮的标签
get_pretty_name <- function(raw_name) {
  # 去 var_info 里找对应的 label
  match_idx <- match(raw_name, var_info$raw)
  
  if (!is.na(match_idx)) {
    return(var_info$label[match_idx]) # 找到了就返回漂亮名字
  } else {
    return(raw_name) # 找不到就返回原名
  }
}
for (f_path in file_list) {
  tryCatch({
    # 1. 准备 ID
    raw_filename <- file_path_sans_ext(basename(f_path))
      var_id <- str_remove(raw_filename, "^Response_Data_")
    
    # 2. 【关键】使用查找表获取漂亮的名字
    x_label_text <- get_pretty_name(var_id)
    
    cat("处理:", var_id, "--> 显示为:", x_label_text, "\n")
    
    # 3. 读取数据
    plot_data <- read.csv(f_path)
    
   # 检查列数是否足够 (防止空文件)
    if(ncol(plot_data) < 4) {
      stop("数据列数不足 4 列，无法解析！")
    }
    
    # 提取第4, 2, 3列
    plot_data <- plot_data[, c(4, 2, 3)]
    
    # 强制重命名 (确保 ggplot 能识别)
    colnames(plot_data) <- c("Species", "Env_Value", "Probability")
    
    # --- 数据清洗 ---
    # 确保 Species 是因子
    plot_data$Species <- as.factor(plot_data$Species)
    
    # 确保数值列是数字
    plot_data$Env_Value <- as.numeric(plot_data$Env_Value)
    plot_data$Probability <- as.numeric(plot_data$Probability)
    
    # 归一化：如果是 0-1000 的数据，除以 1000
    if(max(plot_data$Probability, na.rm=TRUE) > 1.5) {
      plot_data$Probability <- plot_data$Probability / 1000
    }
    # 4. 绘图
 p_classic <- ggplot(plot_data , aes(x = Env_Value, y = Probability)) +
  
  # --- 图层1: 背景线 (The Cloud) ---
  # 关键改动：颜色改用深灰色 (grey30)，透明度极低 (0.01)
  # 这样重叠的地方会自然变深，显示出数据的"密度"
  geom_line(aes(group = Species), color = "#255DAA", alpha = 0.1, linewidth = 0.2) +
  
  # --- 图层2: 趋势线 (The Signal) ---
  # 关键改动：使用醒目的颜色 (比如 Nature 橙色 #E64B35 或 翠绿色 #00A087)
  geom_smooth(method = "gam", 
              color = "#DE525E", # 亮橙红色
              fill = "#DE525E",  # 置信区间填充同色
              alpha = 0.2,       # 置信区间半透明
              linewidth = 1.9) + # 线条加粗
  
  # --- 图层3: 美化标签 (全部英文) ---
  labs(
     x = x_label_text, 
    y = "Probability of Occurrence"
  ) +
  
  # --- 图层4: 标尺控制 ---
  # 强制 Y 轴 0-1，X 轴贴边
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0), breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(expand = c(0, 0)) +
  
  # --- 图层5: Nature 风格主题 ---
  theme_minimal(base_size = 30) +
  theme(
    # 字体设置 (解决乱码的关键，使用通用 sans 字体)
    text = element_text(family = "sans", color = "black"),
    
    # 背景全透明
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    
    # 边框：加黑框
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    
    # 网格线：只保留极淡的网格，或者干脆去掉
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    # 标题位置
    plot.title = element_text(face = "bold", margin = ggplot2::margin(b = 10)),
    axis.title.x = element_text(margin = ggplot2::margin(t = 10)),
    axis.title.y = element_text(margin = ggplot2::margin(r = 10))
  )

 out_name <- paste0("<LOCAL_AUX_ROOT>/Response_Curves_Plots/Response_Curves_Plots/newResponse_", var_id, ".png")
      # 打开图形设备 (指定透明背景)
    png(filename = out_name, 
        width = 2400, height = 1800, res = 300, # 对应 8x6 inch
        bg = "transparent") 
    
    # 打印图片到设备
    print(p_classic)
    
    # 关闭设备 (这一步才真正保存文件)
    dev.off()
    
    cat("成功！\n")}, error = function(e) {
    cat("  [跳过] 出错:", conditionMessage(e), "\n")
    try(dev.off(), silent = TRUE)
  })
}

cat("全部完成！\n")

p_classic <- ggplot(big_response_df , aes(x = Env_Value, y = Probability)) +
  
  # --- 图层1: 背景线 (The Cloud) ---
  # 关键改动：颜色改用深灰色 (grey30)，透明度极低 (0.01)
  # 这样重叠的地方会自然变深，显示出数据的"密度"
  geom_line(aes(group = Species), color = "#255DAA", alpha = 0.1, linewidth = 0.2) +
  
  # --- 图层2: 趋势线 (The Signal) ---
  # 关键改动：使用醒目的颜色 (比如 Nature 橙色 #E64B35 或 翠绿色 #00A087)
  geom_smooth(method = "gam", 
              color = "#DE525E", # 亮橙红色
              fill = "#DE525E",  # 置信区间填充同色
              alpha = 0.2,       # 置信区间半透明
              linewidth = 1.5) + # 线条加粗
  
  # --- 图层3: 美化标签 (全部英文) ---
  labs(
     x = 'test', 
    y = "Probability of Occurrence"
  ) +
  
  # --- 图层4: 标尺控制 ---
  # 强制 Y 轴 0-1，X 轴贴边
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0), breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(expand = c(0, 0)) +
  
  # --- 图层5: Nature 风格主题 ---
  theme_minimal(base_size = 34) +
  theme(
    # 字体设置 (解决乱码的关键，使用通用 sans 字体)
    text = element_text(family = "sans", color = "black"),
    
    # 背景全透明
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    
    # 边框：加黑框
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    
    # 网格线：只保留极淡的网格，或者干脆去掉
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    # 标题位置
    plot.title = element_text(face = "bold", margin = ggplot2::margin(b = 10)),
    axis.title.x = element_text(margin = ggplot2::margin(t = 10)),
    axis.title.y = element_text(margin = ggplot2::margin(r = 10))
  )
p_classic
windows()
