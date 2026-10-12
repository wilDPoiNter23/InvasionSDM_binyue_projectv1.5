# Original source: 060501final_pc_table.r
# Original source location: <LOCAL_CODE_ROOT>/060501final_pc_table.r
# Original source SHA-256: 9d4d04c5cb04f9cceae07dea265de9c95f5534cdd46226c5a6e439ca848dd3f6
# Public-safe original snapshot: archive/original_sources/local/060501final_pc_table.r
# Classification: CANONICAL
# Publication modifications:
# - machine-specific absolute path literals replaced by configured publication_path aliases
# - local encoding decoded to UTF-8; source bytes hashed above
# - comments and historical private machine paths sanitized
# - working-directory calls retained with configured paths; no analyses executed
# Scientific modification: NONE
# This is full historical source, not a certified unattended/reproducible run.
if (!exists("publication_path", mode="function")) stop("Load config/bootstrap.R first")
library(dplyr)
library(tidyr)
library(mgcv)
library(spdep)
install.packages(publication_path("HISTORICAL_PATH_009"), repos = NULL)
coords <- cbind(city_df_res$Longitude, city_df_res$Latitude)

# 1. 读取数据
df_city <- read.csv(publication_path("LOCAL_ANALYSIS_ROOT", "figure/gam_draft/gamtest_withpca.csv"))
# 2. 定义变量组 (必须和你当初做 PCA 时一模一样)
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
get_pretty_name <- function(raw_name) {
  # 去 var_info 表里找对应的 label
  match_idx <- match(raw_name, var_info$raw)
  
  if (!is.na(match_idx)) {
    return(var_info$label[match_idx]) # 找到了返回 Label
  } else {
    return(raw_name) # 没找到返回原名
  }
}
# 根据你提供的 CSV 列名，我整理了对应的变量名
group_list <- list(
  Climate = paste0("wc2.1_2.5m_bio_", 1:19),
  
  Anthropogenic = c("BUILT_S_E2025_2.5min", "GDP2020_2.5min", 
                    "Pop2020_2.5min", "VIIRS_2023_2.5min", "reclamation_area"), # 这一项很可能是嫌疑人
  
  Soil = c("bdod_mean_2024_2_5min_aligned", "cfvo_mean_2024_2_5min_aligned", 
           "clay_mean_2024_2_5min_aligned", "nitrogen_mean_2024_2_5min_aligned", 
           "ocd_mean_2024_2_5min_aligned", "phh2o_mean_2024_2_5min_aligned", 
           "sand_mean_2024_2_5min_aligned", "silt_mean_2024_2_5min_aligned", 
           "soc_mean_2024_2_5min_aligned"),
  
  Marine = c("salinity_mean", "salinity_range", "temperature_mean", "temperature_range"),
  
  Geomorphic = c("wc2.1_2.5m_elev", "slope", "aspect")
)

# 3. 循环计算 (加入方差检测)
pc_interpretation_list <- list()

cat("=== 开始 PCA 载荷提取 ===\n")

for (grp_name in names(group_list)) {
  
  vars <- group_list[[grp_name]]
  
  # A. 检查变量是否存在
  valid_vars <- vars[vars %in% names(df_city)]
  
  if(length(valid_vars) > 1) {
    # 提取数据矩阵
    data_subset <- df_city[, valid_vars]
    
    # B. 填充 NA (用均值填充)
    for(i in 1:ncol(data_subset)) {
      if(any(is.na(data_subset[,i]))) {
        data_subset[is.na(data_subset[,i]), i] <- mean(data_subset[,i], na.rm = TRUE)
      }
    }
    
    # --- C. 【核心修复】剔除零方差变量 ---
    # 计算每一列的方差
    col_vars <- apply(data_subset, 2, var)
    
    # 找出方差为0（或NA，意味着全是NA）的列
    bad_cols <- names(col_vars)[col_vars == 0 | is.na(col_vars)]
    
    if(length(bad_cols) > 0) {
      cat("  [警告] 组别", grp_name, "剔除无变化变量:", paste(bad_cols, collapse=", "), "\n")
      # 从数据中移除这些列
      data_subset <- data_subset[, !names(data_subset) %in% bad_cols, drop = FALSE]
    }
    
    # D. 再次检查剩余变量数量 (PCA至少需要2个变量)
    if(ncol(data_subset) > 1) {
      # 执行 PCA
      pca_res <- prcomp(data_subset, center = TRUE, scale. = TRUE)
      
      # 提取信息
      loadings <- pca_res$rotation[, 1]
      var_exp <- summary(pca_res)$importance[2, 1] * 100 
      
      # 存表
      pc_interpretation_list[[grp_name]] <- data.frame(
        Category = grp_name,
        Variable = names(loadings),
        PC1_Loading = round(loadings, 3),
        PC1_Variance_Explained = paste0(round(var_exp, 1), "%")
      )
    } else {
      cat("  [跳过] 组别", grp_name, "有效变量不足 2 个，无法进行 PCA。\n")
    }
  }
}

# 4. 合并结果
if(length(pc_interpretation_list) > 0) {
  final_pc_table <- do.call(rbind, pc_interpretation_list)
  row.names(final_pc_table) <- NULL
  
  print(head(final_pc_table))
  write.csv(final_pc_table, "SI_Table_PCA_Loadings_Fixed.csv", row.names = FALSE)
  cat("\n成功！结果已保存为 SI_Table_PCA_Loadings_Fixed.csv\n")
} else {
  cat("\n没有生成任何结果。\n")
}

# 使用 sapply 批量应用转换函数
final_pc_table$Description <- sapply(final_pc_table$Variable, get_pretty_name)

# ==============================================================================
# 3. 整理表格顺序 (美化)
# ==============================================================================
# 把列的顺序调整一下，把 Description 放在 Variable 后面，方便阅读
final_pc_table_polished <- final_pc_table %>%
  select(
    Category,
    Variable,
    Description,          # 新加的解释列放在中间
    PC1_Loading,
    PC1_Variance_Explained
  )

# ==============================================================================
# 4. 查看并保存
# ==============================================================================

cat("=== 最终 PCA 载荷表预览 ===\n")
print(head(final_pc_table_polished))

write.csv(final_pc_table_polished, publication_path("LOCAL_ANALYSIS_ROOT", "figure/gam_draft/SI_Table_PCA_Loadings_Final.csv"), row.names = FALSE)

city_df_res <- city_df_clean %>%
  mutate(
    res_nonsp = residuals(m_idx_rclass, type = "pearson"),
    res_sp    = residuals(m_spatial,    type = "pearson")
  )
coords <- cbind(city_df_res$Longitude, city_df_res$Latitude)

# k 最近邻（比如 k = 4 或 6 都可以）
nb <- knearneigh(coords, k = 4)
nb <- knn2nb(nb)

lw <- nb2listw(nb, style = "W", zero.policy = TRUE)
mi_nonsp <- moran.test(city_df_res$res_nonsp, lw, zero.policy = TRUE)
mi_sp    <- moran.test(city_df_res$res_sp,    lw, zero.policy = TRUE)

mi_nonsp
mi_sp
# correlogram：按照邻接阶数（1-6 阶邻居）看 Moran's I
cor_nonsp <- sp.correlogram(
  nb, city_df_res$res_nonsp,
  order = 6, method = "I",
  style = "W", zero.policy = TRUE
)

cor_sp <- sp.correlogram(
  nb, city_df_res$res_sp,
  order = 6, method = "I",
  style = "W", zero.policy = TRUE
)
# helper，把 sp.correlogram 的结果转成 data.frame
cor2df <- function(cor_obj) {
  # 有些版本里 cor_obj$res 是 matrix/data.frame，有些是 list
  res_obj <- cor_obj$res
  
  if (is.matrix(res_obj) || is.data.frame(res_obj)) {
    # 常见结构：列名包括 "I" 和 "VarI"
    I_vals   <- res_obj[, "I"]
    # 有的版本叫 "VarI"，有的叫 "VI"，做个兜底
    var_name <- intersect(c("VarI", "VI", "varI"), colnames(res_obj))[1]
    var_vals <- res_obj[, var_name]
  } else if (is.list(res_obj)) {
    # 保险写法：每一阶是一个 list，里面有 I 和 VarI
    I_vals   <- sapply(res_obj, function(x) x[["I"]])
    var_vals <- sapply(res_obj, function(x) x[["VarI"]])
  } else {
    stop("Unknown structure for cor_obj$res")
  }
  
  se_vals <- sqrt(var_vals)
  
  dplyr::tibble(
    order = seq_along(I_vals),
    I     = I_vals,
    se    = se_vals,
    lower = I - 1.96 * se,
    upper = I + 1.96 * se
  )
}
df_nonsp <- cor2df(cor_nonsp) %>% dplyr::mutate(model = "Non-spatial")
df_sp    <- cor2df(cor_sp)    %>% dplyr::mutate(model = "Spatial")

df_cor <- dplyr::bind_rows(df_nonsp, df_sp)
par(mfrow = c(1, 2))

plot(cor_nonsp,
     main = "Non-spatial model residuals",
     xlab = "Neighbourhood order",
     ylab = "Moran's I")

plot(cor_sp,
     main = "Spatial model residuals",
     xlab = "Neighbourhood order",
     ylab = "Moran's I")

par(mfrow = c(1, 1))
windows()
png(
  filename = "Supp_Fig_Sy_MoranI_correlogram.png", 
  width = 8,       # 宽 (英寸)
  height = 6,      # 高 (英寸)
  units = "in",    # 单位
  res = 300        # 分辨率 (这里要用 res，不能用 dpi)
)
par(mfrow = c(1, 2))
plot(cor_nonsp,
     main = "Non-spatial model residuals",
     xlab = "Neighbourhood order",
     ylab = "Moran's I")
plot(cor_sp,
     main = "Spatial model residuals",
     xlab = "Neighbourhood order",
     ylab = "Moran's I")
par(mfrow = c(1, 1))
dev.off()
