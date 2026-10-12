# Original source: 010801linux_corAnalysis.r
# Original source location: <LOCAL_CODE_ROOT>/010801linux_corAnalysis.r
# Original source SHA-256: 90b7b16d36403413b7a44769e929511994caac24ea9b5413db321491e935a034
# Public-safe original snapshot: archive/original_sources/local/010801linux_corAnalysis.r
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

library(tidyr)
library(ggplot2)
r_stack <- terra::rast( "<HPC_INPUT_ROOT>/climate/full_stack.tif")

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
names(r_stack) 
set.seed(123)
n_total <- terra::ncell(r_stack)
n_samp  <- min(50000, n_total) 
print(n_total)
print(n_samp)
# 2) 从全局校准域随机采样像元 ---------------------------------------------
r_sample <- terra::spatSample(
  r_stack,
  size      = n_samp,
  method    = "random",
  na.rm     = FALSE,      # ❗ 不在这里丢 NA，后面 cor 用 pairwise 处理
  as.points = FALSE
)

saveRDS(r_sample, "<WORK_ROOT>/results/variable/r_sample.rds")
# r_sample <- readRDS('<WORK_ROOT>/results/variable/r_sample.rds')
vals <- as.data.frame(r_sample)  
# 可选：去掉有大量NA的列（一般spatSample已经处理了）
vals <- vals[, colSums(!is.na(vals)) > 0, drop = FALSE]
str(vals)
# 3) 计算 Pearson 相关矩阵 -----------------------------------------------
cor_mat <- cor(vals, use = "pairwise.complete.obs", method = "pearson")

dim(cor_mat)   # 应该是 40x40 或接近这个维度
# 用 |r| 做距离，聚类后重排，使结构更清晰
dist_mat <- as.dist(1 - abs(cor_mat))
hc <- hclust(dist_mat, method = "average")
ord <- hc$order

cor_reord <- cor_mat[ord, ord]

# 换成 label
lab <- pretty_label(rownames(cor_reord))
rownames(cor_reord) <- lab
colnames(cor_reord) <- lab

cor_df <- as.data.frame(cor_reord) %>%
  mutate(var1 = rownames(cor_reord)) %>%
  relocate(var1)

cor_long <- cor_df %>%
  pivot_longer(-var1, names_to = "var2", values_to = "r")

# 保证因子顺序按聚类后的顺序
var_levels <- lab
cor_long <- cor_long %>%
  mutate(
    var1 = factor(var1, levels = var_levels),
    var2 = factor(var2, levels = var_levels),
    i = as.numeric(var1),
    j = as.numeric(var2)
  ) %>%
  # 只保留下三角（i > j）
  filter(i > j)

write.csv(cor_long,"<WORK_ROOT>/results/variable/Supp_Fig_S4_cor_matrix.csv")

# 自定义蓝-白-红的 diverging palette（避免彩虹）
pal_cor <- colorRampPalette(c("#2166AC", "white", "#B2182B"))

p_cor <- ggplot(cor_long, aes(x = var2, y = var1, fill = r)) +
  geom_tile(color = NA) +
  scale_fill_gradientn(
    colours = pal_cor(201),
    limits  = c(-1, 1),
    name    = "Pearson r"
  ) +
  coord_fixed() +
  theme_minimal(base_size = 9) +
  theme(
    axis.title      = element_blank(),
    axis.text.x     = element_text(angle = 45, hjust = 0, vjust = 0),
    axis.text.y     = element_text(hjust = 1),
    panel.grid      = element_blank(),
    legend.position = "right"
  )

  ggsave(
  filename = "<WORK_ROOT>/results/variable/Supp_Fig_S4_cor_matrix.pdf",
  plot     = p_cor,
  width    = 7,
  height   = 6
)