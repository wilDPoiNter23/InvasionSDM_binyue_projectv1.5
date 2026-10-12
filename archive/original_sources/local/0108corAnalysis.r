# Original source: 0108corAnalysis.r
# Original source location: <LOCAL_CODE_ROOT>/0108corAnalysis.r
# Original source SHA-256: 15b7855f1d9c2843b04ae075314e3af259a376ce48ff171b9fc5fbb0a639df72
# Public-safe original snapshot: archive/original_sources/local/0108corAnalysis.r
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
library(dplyr)
library(stats)
library(tidyr)
library(ggplot2)
library(ggcorrplot)
library(tibble)
library(pheatmap)

library(igraph)
install.packages("<HISTORICAL_PATH_001>", repos = NULL)
install.packages("pheatmap", repos = "https://cloud.r-project.org")
cor_mat <- read.csv("<LOCAL_ANALYSIS_ROOT>/figure/sdm_evalueation/Supp_Fig_S4_cor_matrix.csv")
 head(cor_mat)
# 1) 读入多层栅格 ---------------------------------------------------------
r_stack <- terra::rast("<LOCAL_ANALYSIS_ROOT>/data/data/full_stack.tif")
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
  "BUILT_S_E2025_2.5min",   "Built-up fraction",           "Anthropogenic",
  "GDP2020_2.5min",         "GDP density",                 "Anthropogenic",
  "Pop2020_2.5min",         "Population density",          "Anthropogenic",
  "VIIRS_2023_2.5min",      "Night-time lights",           "Anthropogenic",
  "reclamation_raster",     "Reclamation area",            "Anthropogenic",
  # Topography
  "wc2.1_2.5m_elev", "Elevation (m)", "Topography",
  "slope",           "Slope (°)",     "Topography",
  "aspect",          "Aspect (°)",    "Topography",
  # Marine
  "salinity_mean",     "Mean sea surface salinity",          "Marine",
  "salinity_range",    "Sea surface salinity range",         "Marine",
  "temperature_mean",  "Mean sea surface temperature (°C)",  "Marine",
  "temperature_range", "Sea surface temperature range (°C)", "Marine"
)

pretty_label <- function(x) {
  out <- var_info$label[match(x, var_info$raw)]
  out[is.na(out)] <- x[is.na(out)]
  out
}
# # 看一下变量名（确保是你40个候选变量）
# names(r_stack)
# set.seed(123)

# # 2) 从全局校准域随机采样像元 ---------------------------------------------
# r_sample <- terra::spatSample(
#   r_stack,
#   size   = 50000,      # 可按机器能力调大/调小
#   method = "random",
#   na.rm  = TRUE,
#   as.points = FALSE
# )

# # 可选：去掉有大量NA的列（一般spatSample已经处理了）
# vals <- vals %>% select(where(~ sum(!is.na(.)) > 1000))
# str(vals)
# # 3) 计算 Pearson 相关矩阵 -----------------------------------------------
# cor_mat <- cor(vals, use = "pairwise.complete.obs", method = "pearson")

  

# 1. 基础绘图 (带分层聚类，自动把相关的变量放在一起)
# 1. 补全对称性 (因为你的表里可能只有 A-B，没有 B-A)
#    如果不补全，转成矩阵会有很多 NA，导致聚类失败
df_mirror <- cor_mat%>%
  rename(var1 = var2, var2 = var1) %>% # 交换列名
  select(var1, var2, r)

df_full <- bind_rows(cor_mat %>% select(var1, var2, r), df_mirror) %>%
  distinct(var1, var2, .keep_all = TRUE) # 去重，防止重复

# 2. 也是补全：把自己和自己的相关性设为 1 (如果有缺失的话)
vars <- unique(c(df_full$var1, df_full$var2))
df_diag <- data.frame(var1 = vars, var2 = vars, r = 1)
df_final <- bind_rows(df_full, df_diag) %>% distinct(var1, var2, .keep_all = TRUE)

# 3. 转成矩阵
cor_matrix <- df_final %>%
  pivot_wider(names_from = var2, values_from = r, values_fill = 0) %>%
  column_to_rownames("var1") %>%
  as.matrix()
dim(cor_matrix)

dist_mat <- as.dist(1 - abs(cor_matrix))

# 2. 层次聚类
hc <- hclust(dist_mat, method = "complete")

# 3. 获取排序后的变量名列表
ordered_vars <- rownames(cor_matrix)[hc$order]


# 给原始数据指定“因子顺序”
# 这步最重要！它决定了 ggplot 画图时谁排在前面
df_full$var1 <- factor(df_full$var1, levels = ordered_vars)
df_full$var2 <- factor(df_full$var2, levels = ordered_vars)

# 开始绘图
p_cor <- ggplot(df_full, aes(x = var1, y = var2, fill = r)) +
  geom_tile(color = "white", size = 0.05) + # 极细白边增加精致感
  
  # 颜色：深蓝-白-深红
  scale_fill_gradient2(
    low = "#2166AC", mid = "white", high = "#B2182B",
    midpoint = 0, limit = c(-1, 1),
    name = "Pearson\nCorr"
  ) +
  
  coord_fixed() + # 保证格子是正方形
  
  theme_minimal() +
  theme(
    # X轴文字：旋转90度，字号调小 (适应40个变量)
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 7),
    # Y轴文字
    axis.text.y = element_text(size = 7),
    
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right"
  )

pheatmap(cor_matrix ,
         # 1. 颜色
         color = colorRampPalette(c("#2166AC", "white", "#B2182B"))(100),
         
         # 2. 自动聚类
         cluster_rows = TRUE,
         cluster_cols = TRUE,
         
         # 3. 【核心】自动切分！(比如切成 5 大块)
         cutree_rows = 3, 
         cutree_cols = 4,
         
         # 4. 美化
         show_rownames = TRUE,
         show_colnames = TRUE,
         fontsize = 8, 
         border_color = NA,  # 去掉格子边框，让色块连成片
         treeheight_row = 20, 
         treeheight_col = 20,
         
         main = "Cluster Heatmap with Gaps"
)

  ggsave(
  filename = "<LOCAL_ANALYSIS_ROOT>/figure/sdm_evalueation/Supp_Fig_S4_cor_matrix.png",
  plot     = p_cor,
  width    = 7,
  height   = 6,
device = grDevices::png  
)
# 1. 设定阈值，只保留强相关 (比如 |r| > 0.7)
threshold <- 0.8
cor_filter <- cor_matrix
cor_filter[abs(cor_filter) < threshold] <- 0 # 把弱相关的变成0
cor_filter[cor_filter == 1] <- 0             # 把对角线变成0

# 2. 构建网络
net <- graph_from_adjacency_matrix(cor_filter, mode = "undirected", weighted = TRUE, diag = FALSE)

# 3. 简单的绘图
# 去掉孤立点 (没有高共线性的变量，不需要显示)
net_clean <- delete_vertices(net, V(net)[degree(net) == 0]) 

net <- plot(net_clean,
     layout = layout_with_fr(net_clean), # 力导向布局
     vertex.size = 5,                    # 点的大小
     vertex.label.cex = 0.7,             # 标签大小
     vertex.color = "orange",            # 点颜色
     edge.width = E(net_clean)$weight * 3, # 连线越粗，相关性越高
     main = "High Collinearity Network (|r| > 0.7)"
)
windows()

  ggsave(
  filename = "<LOCAL_ANALYSIS_ROOT>/figure/sdm_evalueation/Supp_Fig_net.pdf",
  plot     = net,
  width    = 7,
 height   = 6

# device = grDevices::png  
)
corrplot(cor_matrix, 
         method = "square",       # 用方块充满格子
         order = "hclust",        # 层次聚类排序
         hclust.method = "ward.D2", 
         
         # --- 颜色设置 ---
         col = colorRampPalette(c("#4477AA", "white", "#BB4444"))(200),
         
         # --- 极简设置 ---
         tl.col = "black",        # 文字颜色
         tl.cex = 0.6,            # 文字调得非常小 (0.6 或 0.5)
         tl.srt = 90,             # 文字垂直旋转，节省空间
         
         cl.cex = 0.8,            # 图例文字大小
         
         # 去掉对角线
         diag = FALSE,
         grid = FALSE             # 去掉背景网格
)
