# Original source: 0407buffer_city_compare.r
# Original source location: <LOCAL_CODE_ROOT>/0407buffer_city_compare.r
# Original source SHA-256: bd736e2be98904195fe4eb6ce7e8ccbc8bcf23ffebff514a78c352a4ef1a7b64
# Public-safe original snapshot: archive/original_sources/local/0407buffer_city_compare.r
# Classification: SUPPORTING
# Publication modifications:
# - machine-specific absolute path literals replaced by configured publication_path aliases
# - local encoding decoded to UTF-8; source bytes hashed above
# - comments and historical private machine paths sanitized
# - working-directory calls retained with configured paths; no analyses executed
# Scientific modification: NONE
# This is full historical source, not a certified unattended/reproducible run.
# Snapshot for inspection; path placeholders are not executable locations.
library(terra)
library(sf)
library(ggplot2)
library(patchwork)
library(viridis)
library(scales)
library(ggnewscale)
library(rnaturalearth)
library(tidyr)
install.packages("rnaturalearth")
# 1. 保存所有已安装包的列表到一个文件
packages <- installed.packages()[, "Package"]
packages
packages_to_install <- readRDS("<RECOVERED_CODE_ROOT>/project_packages.rds")

# 找出新 R 里还没装的包
newly_installed <- installed.packages()[, "Package"]
for (p in packages_to_install) {
  if (!p %in% newly_installed) {
    install.packages(p)
  }
}
saveRDS(packages, "<RECOVERED_CODE_ROOT>/project_packages.rds")
# 把文件保存在一个你能找到的地方
my_lib_path <- "<HISTORICAL_PATH_006>"
.libPaths()
.libPaths("<HISTORICAL_PATH_006>")
install.packages(c("rlang", "ggplot2", "patchwork", "scales", "tibble", "dplyr"),
  dependencies = TRUE
)
R.version.string
# 1) 读取 invasion pressure raster（你的 IRI / pressure surface）
# 假设已经是等面积投影或米单位投影；若是经纬度，请先投影（下面有说明）
press <- rast("<LOCAL_ANALYSIS_ROOT>/data/data/hotspot_mean_suitability_normalized2.0.tif")

# 2) 读取城市边界（108个城市 polygon）
cities <- vect("<LOCAL_ANALYSIS_ROOT>/data/data/108finalCities.shp") # 必须有一个城市ID字段，比如 "city_id" 或 "name"
cities_sf <- st_read("<LOCAL_ANALYSIS_ROOT>/data/data/108finalCities.shp")
# 检查几何是否有效（TRUE/FALSE）
is_ok <- is.valid(cities)
table(is_ok, useNA = "ifany")

# 看看哪些城市有问题（如果你有 city_id 字段）
bad <- cities[!is_ok, ]
bad
cities_v <- makeValid(cities)
# 确保 CRS 一致
press_m <- project(press, "EPSG:6933")
cities_m <- project(cities_v, "EPSG:6933")

cities_m$shapeName
# 3) 设定 buffer 距离（单位：米；50 km = 50000）
buf_m <- 50000
# 4) 计算城市内部平均压力
city_stat <- extract(press_m, cities_m, fun = mean, na.rm = TRUE)
# 方法1：使用缓冲区外围区域作为背景
# 创建缓冲区
cities_buf <- buffer(cities_m, width = buf_m)

# 计算缓冲区内（包含城市）的压力
buffer_stat <- extract(press_m, cities_buf, fun = mean, na.rm = TRUE)

# 方法2（推荐）：使用环形区域（buffer - city）作为纯背景
# 计算环形区域的压力
cities_buf_ring <- erase(cities_buf, cities_m) # 环形 = 缓冲区 - 城市内部
background_stat <- extract(press_m, cities_buf_ring, fun = mean, na.rm = TRUE)

# ============================================================
# 第二部分：整合数据框
# ============================================================

# 假设你的城市数据中有城市ID字段（例如 "city_id" 或 "name"）
# 这里用 cities_m 的第一个字段作为示例
city_ids <- values(cities_m)[, 1] # 根据实际字段名调整
city_ids
# 创建分析数据框
df_analysis <- data.frame(
  city_id = city_ids,

  # City-specific pressure（城市内部）
  city_pressure = city_stat[, 2], # extract() 的第2列是统计值

  # Background pressure（环形区域）
  background_pressure = background_stat[, 2],

  # 你需要加入 reclamation net effect 数据
  # 这里假设你有一个 reclamation 数据框，需要 merge
  # reclamation_effect = ...  # 从你的 reclamation 分析中获取

  stringsAsFactors = FALSE
)
df_analysis
# ============================================================
# 第三部分：计算 ΔPressure（关键指标）
# ============================================================
city_pressure <- city_stat[, 2]
background_pressure <- background_stat[, 2]

df_delta <- data.frame(
  city_id = city_ids,
  city_pressure = city_stat[, 2],
  background_pressure = background_stat[, 2],

  # ⭐ 核心指标：城市相对背景的偏离
  delta_pressure = city_pressure - background_pressure,

  # 相对偏离（标准化版本）
  relative_deviation = (city_pressure - background_pressure) /
    background_pressure
)
df_delta

df_clean <- df_delta %>%
  #  %>%
  #   select(
  #     city_id,
  #     city_pressure,
  #     background_pressure,
  #     delta_pressure = delta_pressure.Abutilon.grandifolium_PA1_RUN1_GLM,  # 使用正确的列
  #     relative_deviation = relative_deviation.Abutilon.grandifolium_PA1_RUN1_GLM
  #   ) %>%
  mutate(
    # 重命名城市名称（首字母大写）
    City = tools::toTitleCase(tolower(city_id)),

    # 计算城市角色
    City_Role = case_when(
      delta_pressure > 0.1 ~ "Amplifier",
      delta_pressure < -0.1 ~ "Sink",
      TRUE ~ "Neutral"
    ),

    # 四舍五入到合适的小数位
    City_Pressure = round(city_pressure, 3),
    Background_Pressure = round(background_pressure, 3),
    Delta_Pressure = round(delta_pressure, 3),
    Relative_Deviation_Pct = round(relative_deviation * 100, 1) # 转换为百分比
  ) %>%
  select(
    City,
    City_Pressure,
    Background_Pressure,
    Delta_Pressure,
    Relative_Deviation_Pct,
    City_Role
  ) %>%
  arrange(desc(Delta_Pressure)) # 按 Delta 降序排列


summary_table <- df_clean %>%
  group_by(City_Role) %>%
  summarise(
    N_Cities = n(),
    Mean_City_Pressure = round(mean(City_Pressure), 3),
    Mean_Background = round(mean(Background_Pressure), 3),
    Mean_Delta = round(mean(Delta_Pressure), 3),
    Median_Delta = round(median(Delta_Pressure), 3),
    SD_Delta = round(sd(Delta_Pressure), 3),
    Max_Deviation_Pct = round(max(Relative_Deviation_Pct), 1),
    .groups = "drop"
  ) %>%
  arrange(desc(Mean_Delta))

print(summary_table)

df_supplement <- df_clean %>%
  mutate(
    # 添加排名
    Rank_Delta = dense_rank(desc(Delta_Pressure)),

    # 添加解释性标签
    Deviation_Category = case_when(
      Relative_Deviation_Pct > 200 ~ "Very High (>200%)",
      Relative_Deviation_Pct > 100 ~ "High (100-200%)",
      Relative_Deviation_Pct > 50 ~ "Moderate (50-100%)",
      Relative_Deviation_Pct > 0 ~ "Low (0-50%)",
      TRUE ~ "Negative"
    )
  ) %>%
  select(
    Rank_Delta,
    City,
    City_Pressure,
    Background_Pressure,
    Delta_Pressure,
    Relative_Deviation_Pct,
    City_Role,
    Deviation_Category
  )

df_supplement
write.csv(df_supplement, "<LOCAL_ANALYSIS_ROOT>/data/organized_table/0701_50kmbufferclassified.csv", row.names = FALSE)
write.csv(summary_table, "<LOCAL_ANALYSIS_ROOT>/data/organized_table/0702_50kmbuffersummary.csv", row.names = FALSE)
# ============================================================
# 方法7：创建三列分组表格（紧凑展示）
# ============================================================

# 按城市角色分三组
amplifiers_list <- df_clean %>%
  filter(City_Role == "Amplifier") %>%
  pull(City)

neutral_list <- df_clean %>%
  filter(City_Role == "Neutral") %>%
  pull(City)

sinks_list <- df_clean %>%
  filter(City_Role == "Sink") %>%
  pull(City)

# 创建分组摘要
role_summary <- data.frame(
  Category = c("Amplifiers", "Neutral", "Sinks"),
  Count = c(
    length(amplifiers_list),
    length(neutral_list),
    length(sinks_list)
  ),
  Percentage = round(c(
    length(amplifiers_list) / nrow(df_clean) * 100,
    length(neutral_list) / nrow(df_clean) * 100,
    length(sinks_list) / nrow(df_clean) * 100
  ), 1),
  Example_Cities = c(
    paste(head(amplifiers_list, 5), collapse = ", "),
    paste(head(neutral_list, 5), collapse = ", "),
    paste(head(sinks_list, 5), collapse = ", ")
  )
)

print(role_summary)


p_delta <- ggplot(df_clean, aes(x = Delta_Pressure, fill = City_Role)) +
  geom_histogram(bins = 30, color = "white", alpha = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1) +
  scale_fill_manual(values = c(
    "Amplifier" = "#E41A1C",
    "Neutral" = "#999999",
    "Sink/Filter" = "#377EB8"
  )) +
  labs(
    x = "ΔPressure (City − Background)",
    y = "Number of Cities",
    title = "City-level Deviation from Coastal Baseline",
    subtitle = "Positive values indicate invasion amplification",
    fill = "City Role"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "top"
  )

print(p_delta)
names(df_clean)
city_results <- df_clean %>%
  select(City, City_Pressure, Background_Pressure, Delta_Pressure, City_Role) %>%
  mutate(
    # 城市角色细分
    Role_Detail = case_when(
      Delta_Pressure > 0.2 ~ "Strong Amplifier",
      Delta_Pressure > 0.1 ~ "Moderate Amplifier",
      Delta_Pressure > 0 ~ "Weak Amplifier",
      Delta_Pressure > -0.1 ~ "Neutral"
    ),

    # 相对于背景的倍数
    Pressure_Ratio = City_Pressure / (Background_Pressure + 1e-6),

    # 是否是热点中的热点
    Hotspot_Type = case_when(
      Background_Pressure > 0.3 & Delta_Pressure > 0.1 ~ "Hotspot Amplifier",
      Background_Pressure > 0.3 & Delta_Pressure <= 0.1 ~ "Embedded Hotspot",
      Background_Pressure <= 0.3 & Delta_Pressure > 0.1 ~ "Isolated Hotspot",
      TRUE ~ "Low Background"
    )
  )
write.csv(city_results, "<LOCAL_ANALYSIS_ROOT>/data/organized_table/0703_city_results.csv", row.names = FALSE)
city_results <- read.csv("<LOCAL_ANALYSIS_ROOT>/data/organized_table/0703_city_results.csv")
cities_sf_match <- cities_sf %>%
  mutate(
    # 创建一个专门用于匹配的新列，不破坏原来的 shapeName (因为原来的可能要用来显示标签)
    join_key = tolower(shapeName)
  )
city_results_match <- city_results %>%
  mutate(
    # 同样处理 City 列
    join_key = tolower(City)
  )
cities_final <- cities_sf_match %>%
  left_join(city_results_match, by = "join_key")
na_count <- sum(is.na(cities_final$Delta_Pressure))
na_count
# ============================================================
# 主图方案 1：全球地图 + 城市偏离度（强烈推荐）
# ============================================================

# 转换 raster 为 data.frame 用于绘图
press_df <- as.data.frame(press, xy = TRUE)
colnames(press_df) <- c("x", "y", "pressure")

col_fun <- colorRampPalette(c("white", "#4CAF50", "#FFD700", "#FF8C00", "#B22222"))
my_colors <- col_fun(100) # 生成100个过渡色

# -------------------------------------------------------------------------
# 2. 准备数据 (确保之前清洗过)
# -------------------------------------------------------------------------
# 确保城市数据没有空值，并且提取了经纬度 (为了避免之前的报错)
cities_clean <- cities_final %>%
  filter(!is.na(Delta_Pressure))
names(cities_final)
# 1. 计算多边形的几何中心 (Centroid)
# 这一步会把每个城市的形状变成一个中心点，警告信息可以忽略
sf_use_s2(FALSE)
cities_centroids <- st_centroid(cities_clean)

# 2. 从中心点提取坐标
# 现在 city_coords 的行数就会和 cities_clean 一样了 (都是 59 行)
city_coords <- st_coordinates(cities_centroids)

# 3. 赋值
cities_clean$lon <- city_coords[, 1]
cities_clean$lat <- city_coords[, 2]

# 获取世界边界
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
world_map <- map_data("world")
# 主图
p_main <- ggplot() +
  # # 1) 全球入侵压力底图（背景）
  # geom_raster(data = press_df,
  #             aes(x = x, y = y, fill = pressure),
  #             alpha = 0.8) +
  # scale_fill_gradientn(
  #  colors = my_colors,
  #   name = "Global Invasion\nPressure",
  #   na.value = "transparent",
  #   limits = c(0, 0.7),
  #   guide = guide_colorbar(
  #     title.position = "top",
  #     barwidth = 15,
  #     barheight = 0.5,
  #     order = 1
  #   )
  # ) +

  # 2) 世界边界
  geom_sf(data = world, fill = NA, color = "gray50", linewidth = 0.5) +
  new_scale_fill() + # 开启新的 fill 标尺，避免和底图颜色冲突

  geom_point(
    data = cities_clean,
    aes(
      x = lon, y = lat,
      fill = Delta_Pressure,
      size = abs(Delta_Pressure)
    ),
    shape = 21, # 空心圆
    color = "black", # 黑色边框
    stroke = 0.4, # 边框细一点更精致
    alpha = 1
  ) + # 城市点不透明，突出显示

  # 城市点的颜色标尺 (红蓝配色)
  scale_fill_gradient2(
    low = "#2166AC", # 蓝色 Sink
    mid = "#F7F7F7", # 白色 Neutral
    high = "#B2182B", # 红色 Amplifier
    midpoint = 0,
    name = "ΔPressure\n(City - Background)",
    limits = c(-0.3, 0.5), # 根据您的数据范围调整
    guide = guide_colorbar(
      title.position = "top",
      barwidth = 15,
      barheight = 0.5,
      order = 2
    )
  ) +

  # 城市点的大小标尺
  scale_size_continuous(
    range = c(1, 7), # 调整气泡大小
    name = "|ΔPressure|",
    guide = guide_legend(order = 3, nrow = 1)
  ) +
  # 4) 主题
  coord_sf(expand = FALSE) +
  labs(
    title = "Global Invasion Pressure and City-level Deviations",
    subtitle = "Cities embedded in varying invasion backgrounds reshape local pressure patterns",
    caption = "Point color: deviation direction | Point size: deviation magnitude"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    legend.position = "bottom",
    legend.box = "horizontal",
    # 绘图区背景 (坐标轴围起来的区域) -> 透明
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.grid = element_blank(), # 去除所有网格线
    panel.grid.major = element_blank(), # 双重保险
    panel.grid.minor = element_blank(), # 双重保险
    # 整张图背景 (包括标题、图例周围的区域) -> 透明
    plot.background = element_rect(fill = "transparent", color = NA),

    # 图例背景 -> 透明 (防止图例带白色方块)
    legend.background = element_rect(fill = "transparent"),

    # 图例边框背景 -> 透明
    legend.box.background = element_rect(fill = "transparent")
  )

print(p_main)
ggsave("<LOCAL_ANALYSIS_ROOT>/figure/study_area_draft/city_bufferonly.png", bg = "transparent", p_main, width = 10, height = 8.5, dpi = 1200, device = grDevices::png)

# ============================================================
# 统计检验：回答关键科学问题
# ============================================================

# 问题1：城市是否系统性偏离背景？
t_test_deviation <- t.test(city_results$Delta_Pressure, mu = 0)
cat("\n=== 问题1：城市是否系统性偏离背景？ ===\n")

cat("平均 ΔPressure:", round(mean(city_results$Delta_Pressure), 4), "\n")
cat("t-test p-value:", format.pval(t_test_deviation$p.value), "\n")
cat("结论:", ifelse(t_test_deviation$p.value < 0.05,
  "是，城市系统性高于背景",
  "否，城市与背景无显著差异"
), "\n\n")
print(t_test_deviation)
# 问题2：不同背景水平的城市，偏离程度是否不同？
kruskal_test <- kruskal.test(Delta_Pressure ~ BG_Group, data = city_results_grouped)
print(kruskal_test)
cat("=== 问题2：高/中/低背景区域的城市偏离是否不同？ ===\n")
cat("Kruskal-Wallis p-value:", format.pval(kruskal_test$p.value), "\n")

# 事后检验
pairwise_result <- pairwise.wilcox.test(
  city_results_grouped$Delta_Pressure,
  city_results_grouped$BG_Group,
  p.adjust.method = "bonferroni"
)
cat("事后检验（Bonferroni 校正）:\n")
print(pairwise_result$p.value)

# 问题3：背景压力能预测城市偏离吗？
correlation_test <- cor.test(city_results$Background_Pressure, city_results$Delta_Pressure)
cat("\n=== 问题3：背景压力与城市偏离的相关性 ===\n")
cat("Pearson r:", round(correlation_test$estimate, 3), "\n")
cat("p-value:", format.pval(correlation_test$p.value), "\n")

# 线性回归
lm_model <- lm(Delta_Pressure ~ Background_Pressure, data = city_results)
cat("\n线性回归结果:\n")
print(summary(lm_model))
results_summary <- city_results %>%
  summarise(
    N_cities = n(),
    N_amplifiers = sum(Delta_Pressure > 0.1),
    Pct_amplifiers = round(100 * N_amplifiers / N_cities, 1),
    N_strong_amplifiers = sum(Delta_Pressure > 0.2),
    Mean_DeltaP = round(mean(Delta_Pressure), 4),
    Median_DeltaP = round(median(Delta_Pressure), 4),
    SD_DeltaP = round(sd(Delta_Pressure), 4),
    SD_background = round(sd(Background_Pressure), 4),
    Mean_city_pressure = round(mean(City_Pressure), 3),
    Mean_bg_pressure = round(mean(Background_Pressure), 3),

    # 最极端的例子
    Max_amplifier = City[which.max(Delta_Pressure)],
    Max_amplifier_delta = round(max(Delta_Pressure), 3),
    Max_sink = City[which.min(Delta_Pressure)],
    Max_sink_delta = round(min(Delta_Pressure), 3)
  )

print("\n=== 用于 Results 的关键统计 ===")
print(results_summary)

# ============================================================
# 主图方案 3：分组箱线图（清晰展示角色差异）
# ============================================================

# 将背景压力分为三组
city_results_grouped <- city_results %>%
  mutate(
    BG_Group = cut(
      Background_Pressure,
      breaks = quantile(Background_Pressure, probs = c(0, 0.33, 0.67, 1)),
      labels = c("Low\nBackground", "Medium\nBackground", "High\nBackground"),
      include.lowest = TRUE
    )
  )

p_grouped <- ggplot(
  city_results_grouped,
  aes(x = BG_Group, y = Delta_Pressure, fill = BG_Group)
) +

  # 箱线图
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +

  # 添加抖动点
  geom_jitter(width = 0.2, alpha = 0.4, size = 2) +

  # 零线
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  scale_fill_manual(
    values = c("#E8F4F8", "#A1D5E8", "#4A90A4")
  ) +
  labs(
    x = "Background Invasion Pressure Level",
    y = "ΔPressure (City - Background)",
    title = "Do Cities Amplify More in High vs Low Background Regions?",
    subtitle = "Positive values indicate cities amplify invasion pressure beyond regional baseline"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "none",
    axis.text.x = element_text(size = 11)
  ) +

  # 添加统计注释
  stat_summary(
    fun = median,
    geom = "text",
    aes(label = sprintf("Median: %.3f", after_stat(y))),
    vjust = -1,
    size = 3.5,
    color = "darkred"
  )

print(p_grouped)
# ==============sensitive test for buffer zone =========================
calc_city_bg <- function(press_m, cities_m, r_km = 50,
                         id_field = "shapeName",
                         fun_stat = mean,
                         na_rm = TRUE) {
  # --- CRS check (avoid extract warnings) ---
  if (!same.crs(press_m, cities_m)) {
    cities_m <- project(cities_m, crs(press_m))
  }

  # --- geometry cleanup to avoid TopologyException ---
  cities_v <- makeValid(cities_m)
  cities_v <- buffer(cities_v, 0) # classic topology clean

  # 1) City pressure (within city polygon)
  city_stat <- extract(press_m, cities_v, fun = fun_stat, na.rm = na_rm)
  colnames(city_stat)[2] <- "P_city"

  # 2) Buffer polygon (includes city)
  r_m <- r_km * 1000
  cities_buf <- buffer(cities_v, width = r_m)
  cities_buf <- makeValid(cities_buf)
  cities_buf <- buffer(cities_buf, 0)

  buffer_stat <- extract(press_m, cities_buf, fun = fun_stat, na.rm = na_rm)
  colnames(buffer_stat)[2] <- "P_buffer"

  # 3) Ring polygon = buffer - city (pure background)
  cities_ring <- erase(cities_buf, cities_v)
  cities_ring <- makeValid(cities_ring)
  cities_ring <- buffer(cities_ring, 0)

  bg_stat <- extract(press_m, cities_ring, fun = fun_stat, na.rm = na_rm)
  colnames(bg_stat)[2] <- "P_bg"

  # 4) Assemble output
  out <- data.frame(
    buffer_km = r_km,
    city_id   = cities_v[[id_field]],
    P_city    = city_stat$P_city,
    P_buffer  = buffer_stat$P_buffer,
    P_bg      = bg_stat$P_bg
  )

  out$DeltaP <- out$P_city - out$P_bg
  out$RelDelta <- out$DeltaP / out$P_bg

  out
}

buffers <- c(10, 50, 100)

res_all <- do.call(rbind, lapply(buffers, function(bk) {
  calc_city_bg(
    press_m  = press_m,
    cities_m = cities_m,
    r_km     = bk,
    id_field = "shapeName",
    fun_stat = mean # 你要 mean 就改成 mean
  )
}))
names(res_all)
write.csv(res_all, "<LOCAL_ANALYSIS_ROOT>/data/organized_table/city_vs_background_buffer_sensitivity.csv", row.names = FALSE)
res_all <- read.csv("<LOCAL_ANALYSIS_ROOT>/data/organized_table/07city_vs_background_buffer_sensitivity.csv")
wide <- reshape(res_all[, c("shapeName", "buffer_km", "DeltaP")],
  idvar = "shapeName", # 唯一标识符是城市名
  timevar = "buffer_km", # 变成列头的是 buffer 距离
  direction = "wide"
)

rho_10_50 <- cor(wide$DeltaP.10, wide$DeltaP.50, method = "spearman", use = "complete.obs")
rho_50_100 <- cor(wide$DeltaP.50, wide$DeltaP.100, method = "spearman", use = "complete.obs")
rho_10_100 <- cor(wide$DeltaP.10, wide$DeltaP.100, method = "spearman", use = "complete.obs")

c(rho_10_50 = rho_10_50, rho_50_100 = rho_50_100, rho_10_100 = rho_10_100)
sign_consistent <- mean(
  sign(wide$DeltaP.10) == sign(wide$DeltaP.50) &
    sign(wide$DeltaP.50) == sign(wide$DeltaP.100),
  na.rm = TRUE
)
sign_consistent
# 1. 定义一个画图函数，避免重复写代码
plot_corr <- function(data, x_col, y_col, x_lab, y_lab, rho_val) {
  ggplot(data, aes_string(x = x_col, y = y_col)) +
    # 添加散点
    geom_point(alpha = 0.6, color = "#2e63d6", size = 2) +
    # 添加拟合线 (线性回归)
    geom_smooth(method = "lm", color = "#dd6e70", fill = "#dd6e70", alpha = 0.2) +
    # 添加对角虚线 (y=x 参考线)
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
    # 在图上标注相关系数
    annotate("text",
      x = -Inf, y = Inf,
      label = paste0("Spearman's rho = ", round(rho_val, 2)),
      hjust = -0.2, vjust = 2, size = 4, fontface = "bold"
    ) +
    labs(x = x_lab, y = y_lab) +
    theme_bw() +
    theme(
      panel.background = element_rect(fill = "transparent", color = NA), # 图形区域透明
      plot.background = element_rect(fill = "transparent", color = NA), # 整个图背景透明
      panel.grid = element_blank(), aspect.ratio = 1
    ) # 保持正方形比例
}

# 2. 生成三张图
p1 <- plot_corr(
  wide, "DeltaP.10", "DeltaP.50",
  "DeltaP (10 km buffer)", "DeltaP (50 km buffer)", 0.79
)

p2 <- plot_corr(
  wide, "DeltaP.50", "DeltaP.100",
  "DeltaP (50 km buffer)", "DeltaP (100 km buffer)", 0.93
)

p3 <- plot_corr(
  wide, "DeltaP.10", "DeltaP.100",
  "DeltaP (10 km buffer)", "DeltaP (100 km buffer)", 0.63
)

# 3. 拼接图片 (需要 patchwork 包)
# 如果没安装: install.packages("patchwork")
final_plot <- p1 + p2 + p3 +
  plot_layout(ncol = 3) +
  plot_annotation(tag_levels = "a") # 自动加 a, b, c 标签
p_d <- ggplot(res_all, aes(x = buffer_km_factor, y = DeltaP)) +
  geom_boxplot(fill = "#A0CBE8", alpha = 0.8, outlier.shape = 21, outlier.fill = "white", outlier.size = 1.5, width = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#d73027", linewidth = 0.8) +
  labs(
    x = "Buffer Radius",
    y = expression(paste(Delta, "Pressure (City - Background)"))
  ) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA)
  )
windows()
final_plot <- (p1 + p2 + p3) / p_d +
  plot_layout(heights = c(1, 1)) +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(size = 18, face = "bold"))

final_plot
print(final_plot)
ggsave("<LOCAL_ANALYSIS_ROOT>/figure/Fig.S/buffer_sensitive.jpg", final_plot, width = 10, height = 8.5, dpi = 1200)
# ============================================================
# 可视化：缓冲区敏感性分析
# ============================================================
res_all_final <- res_all %>%
  mutate(
    # 绝对变化（始终有效）
    AbsDelta = DeltaP,

    # 相对变化（仅当 bg > 0 时有效）
    RelDelta_valid = ifelse(P_bg > 0.001, RelDelta, NA),

    # 标准化的相对变化（cap 在合理范围）
    RelDelta_capped = case_when(
      is.na(RelDelta_valid) ~ NA_real_,
      RelDelta_valid > 1000 ~ 1000, # 上限 1000%
      RelDelta_valid < -100 ~ -100, # 下限 -100%
      TRUE ~ RelDelta_valid
    ),

    # 城市角色分类（基于绝对差值）
    City_Role = case_when(
      AbsDelta > 0.1 ~ "Strong Amplifier",
      AbsDelta > 0.05 ~ "Moderate Amplifier",
      AbsDelta > -0.05 ~ "Neutral",
      AbsDelta > -0.1 ~ "Moderate Sink",
      TRUE ~ "Strong Sink"
    )
  )

# 图1：每个城市在不同缓冲区下的 DeltaP 变化（绝对值）
p1 <- res_all_final %>%
  ggplot(aes(x = factor(buffer_km), y = AbsDelta, group = shapeName)) +
  geom_line(alpha = 0.3, color = "gray50") +
  geom_point(aes(color = City_Role), alpha = 0.6, size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  scale_color_manual(values = c(
    "Strong Amplifier" = "#D73027",
    "Moderate Amplifier" = "#FC8D59",
    "Neutral" = "#999999",
    "Moderate Sink" = "#91BFDB",
    "Strong Sink" = "#4575B4"
  )) +
  labs(
    x = "Buffer Distance (km)",
    y = "ΔPressure (City - Background)",
    title = "Sensitivity of City-Background Deviation to Buffer Size",
    color = "City Role"
  ) +
  theme_classic() +
  theme(legend.position = "bottom")

print(p1)

# 图2：箱线图对比不同缓冲区
p2 <- res_all_final %>%
  ggplot(aes(x = factor(buffer_km), y = AbsDelta)) +
  geom_boxplot(fill = "steelblue", alpha = 0.6, outlier.alpha = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    x = "Buffer Distance (km)",
    y = "ΔPressure (City - Background)",
    title = "Distribution of City Deviation Across Buffer Sizes"
  ) +
  theme_classic()

print(p2)

# 图3：相对变化（仅展示有效值）
p3 <- res_all_final %>%
  filter(!is.na(RelDelta_capped)) %>% # 只保留有效值
  ggplot(aes(x = factor(buffer_km), y = RelDelta_capped)) +
  geom_boxplot(fill = "coral", alpha = 0.6, outlier.shape = NA) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_cartesian(ylim = c(-100, 500)) + # 限制 y 轴范围
  labs(
    x = "Buffer Distance (km)",
    y = "Relative Deviation (%, capped at ±1000%)",
    title = "Relative City Deviation (valid cases only)",
    subtitle = paste(
      "Excluded", sum(is.na(res_all_final$RelDelta_capped)),
      "cases with invalid background pressure"
    )
  ) +
  theme_classic()

print(p3)

# ============================================================
# 统计分析：缓冲区选择的影响
# ============================================================

buffer_sensitivity <- res_all_final %>%
  group_by(buffer_km) %>%
  summarise(
    N_cities = n(),
    N_valid_bg = sum(P_bg > 0.001),
    Pct_valid = round(100 * N_valid_bg / N_cities, 1),

    # 绝对差值统计
    Mean_AbsDelta = round(mean(AbsDelta, na.rm = TRUE), 4),
    Median_AbsDelta = round(median(AbsDelta, na.rm = TRUE), 4),
    SD_AbsDelta = round(sd(AbsDelta, na.rm = TRUE), 4),

    # 相对差值统计（仅有效值）
    Mean_RelDelta = round(mean(RelDelta_capped, na.rm = TRUE), 2),
    Median_RelDelta = round(median(RelDelta_capped, na.rm = TRUE), 2),

    # 城市角色分布
    N_Amplifiers = sum(AbsDelta > 0.05),
    N_Sinks = sum(AbsDelta < -0.05),
    N_Neutral = sum(abs(AbsDelta) <= 0.05),
    .groups = "drop"
  )

print("\n=== 缓冲区敏感性统计 ===")
print(buffer_sensitivity)
best_buffer <- buffer_sensitivity %>%
  filter(Pct_valid >= 90) %>% # 至少 90% 有效
  slice_min(SD_AbsDelta, n = 1)

cat("推荐缓冲区：", best_buffer$buffer_km, "km\n")
cat("理由：有效率", best_buffer$Pct_valid, "%, 标准差", best_buffer$SD_AbsDelta, "\n")
