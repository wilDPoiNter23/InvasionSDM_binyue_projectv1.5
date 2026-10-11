# ARCHIVE: historical reference; symbolic paths are intentionally not runnable.
# Original SHA256: 2e54f8fd57db3b96c7ead92c48b603ae28124ba1425591ec2b905a64e0ab720f
library(randomForest)
library(terra)
library(ranger)
library(pdp)
library(partykit)
library(data.table)
library(xgboost)
library(pdp)
library(DALEX)
library(patchwork)
library(ggplot2)
library(lavaan)
library(semPlot)
library(mgcv)
library(piecewiseSEM)
library(sf)
library(dplyr)
library(osmdata)
library(psych)
library(ggfortify)
r <- terra::rast("${LOCAL_DATABASE_ROOT}/evaluations/hotspot_mean_suitability_normalized2.0.tif") # or .tif
reclamation_city_shp <- st_read("${LOCAL_ANALYSIS_ROOT}/data/data/108finalCities.shp")
cityindex <- st_read("${LOCAL_ANALYSIS_ROOT}/data/data/geoBoundariesCGAZ_ADM1/geoBoundariesCGAZ_ADM1.shp")
city_means <- fread("${LOCAL_ANALYSIS_ROOT}/data/10_03_108city_mean_variables.csv")
more_cities <- fread("${LOCAL_ANALYSIS_ROOT}/data/02_01_reclaimedcities.csv")
reclaimeddata <- fread("${LOCAL_ANALYSIS_ROOT}/data/02_01_reclaimedcities.csv")
typedata <- fread("${LOCAL_ANALYSIS_ROOT}/data/City_Only_Land_Use_Types.csv")



setnames(reclaimeddata, "City", "city")
reclaimeddata$city <- tolower(reclaimeddata$city)

more_cities$City <- tolower(more_cities$City)
typedata$city <- tolower(typedata$city)
vars_to_remove <- c(
  "wc2.1_2.5m_bio_11",
  "wc2.1_2.5m_bio_6",
  "temperature_mean",
  "wc2.1_2.5m_bio_5",
  "wc2.1_2.5m_bio_12",
  "wc2.1_2.5m_bio_13",
  "wc2.1_2.5m_bio_14",
  "wc2.1_2.5m_bio_7",
  "temperature_range"
)


for (i in seq_along(cities)) {
  city_name <- cities[i]
  cat(sprintf("[%02d/%02d] Processing: %s\n", i, length(cities), city_name))

  # Extract current city polygon
  one_city <- city_vect[city_vect$shapeName == city_name, ]

  if (nrow(one_city) == 0) {
    cat("No geometry found for city:", city_name, "\n")
    failed_cities <- c(failed_cities, city_name)
    next
  }

  # Crop raster data
  city_stack <- tryCatch(
    {
      crop(full_stack, one_city)
    },
    error = function(e) {
      cat("Failed to crop for", city_name, "\n")
      return(NULL)
    }
  )

  if (is.null(city_stack)) {
    failed_cities <- c(failed_cities, city_name)
    next
  }

  # Extract all raster values
  all_vals <- tryCatch(
    {
      terra::values(city_stack)
    },
    error = function(e) {
      cat("Failed to extract values for", city_name, "\n")
      return(NULL)
    }
  )

  if (is.null(all_vals)) {
    failed_cities <- c(failed_cities, city_name)
    next
  }

  # Find rows where the first column (usually risk) is not NA
  if (ncol(all_vals) > 0) {
    non_na_idx <- !is.na(all_vals[, 1])
    n_valid <- sum(non_na_idx)

    # Extract if there are valid pixels
    if (n_valid > 0) {
      # Extract valid data
      filtered_vals <- all_vals[non_na_idx, , drop = FALSE]

      # Extract corresponding coordinates
      coords <- tryCatch(
        {
          as.data.frame(xyFromCell(city_stack, which(non_na_idx)))
        },
        error = function(e) {
          cat("Failed to extract coordinates for", city_name, "\n")
          return(NULL)
        }
      )

      if (!is.null(coords) && nrow(coords) > 0) {
        # Merge coordinates and data
        sample_df <- cbind(coords, filtered_vals)

        # Add city information
        sample_df$city <- city_name
        sample_df$city_id <- i
        sample_df$extraction_date <- Sys.Date()

        # Save results
        all_samples[[city_name]] <- sample_df
        success_count <- success_count + 1

        cat("✅ Success:", city_name, "- extracted", nrow(sample_df), "pixels\n")
      } else {
        cat("Failed to get coordinates for", city_name, "\n")
        failed_cities <- c(failed_cities, city_name)
      }
    } else {
      cat("No valid pixels for", city_name, "\n")
      failed_cities <- c(failed_cities, city_name)
    }
  } else {
    cat("No data columns for", city_name, "\n")
    failed_cities <- c(failed_cities, city_name)
  }

  # Clean memory
  rm(city_stack, one_city)
  if (i %% 5 == 0) gc(verbose = FALSE)

  cat("Progress:", success_count, "successful,", length(failed_cities), "failed\n")
  cat("---\n")
}
# Merge all city data
merged_data <- tryCatch(
  {
    # Use rbindlist (if data.table package exists) or rbind to merge
    if (require(data.table, quietly = TRUE)) {
      rbindlist(all_samples, fill = TRUE) # fill=TRUE handles different column counts
    } else {
      do.call(rbind, all_samples)
    }
  },
  error = function(e) {
    cat("⚠️ Direct merge failed, trying stepwise merge...\n")

    # Stepwise merge, handling potential column name inconsistencies
    result <- all_samples[[1]]
    for (i in 2:length(all_samples)) {
      current_data <- all_samples[[i]]

      # Ensure column names are consistent
      common_cols <- intersect(names(result), names(current_data))
      if (length(common_cols) < ncol(result) - 3) { # Allow minor column name differences
        cat("⚠️ City ", names(all_samples)[i], " column names are not entirely consistent with previous ones\n")
      }

      # Merge data
      result <- rbind(result[, common_cols], current_data[, common_cols])
    }
    return(result)
  }
)
names(merged_data)

merged_data %>%
  distinct(city) %>%
  count()
merged_data %>%
  distinct(city)
write.csv(merged_data, "${LOCAL_ANALYSIS_ROOT}/data/pixelfrom108city.csv")


numeric_vars <- setdiff(
  names(merged_data)[sapply(merged_data, is.numeric)],
  c("x", "y", "city_id")
)

# Group by city, calculate mean for each variable
city_means <- merged_data[, lapply(.SD, function(x) mean(x, na.rm = TRUE)),
  by = city, .SDcols = numeric_vars
]

city_means %>%
  distinct(city) %>%
  count()

city_means$city <- tolower(city_means$city)

city_means$city


# Use city as key, left join to replace reclamation_raster
city_means <- merge(
  city_means, # Remove original fields
  reclaimeddata[, .(city, Regions, Latitude, Longitude)], # Keep only necessary fields
  by = "city",
  all.x = TRUE
)


city_means <- merge(
  city_means, # Main dataset (left table)
  typedata[, .(city, Land_Use_Types)], # Select only columns to merge
  by = "city",
  all.x = TRUE
)

city_means <- city_means %>%
  mutate(
    risk_class = case_when(
      risk <= 0.28 ~ 1,
      risk <= 0.62 ~ 2,
      risk <= 0.97 ~ 3,
      TRUE ~ NA_integer_ # Handle out of range or NA
    )
  )

city_means <- city_means %>%
  mutate(
    abs_lat = abs(Latitude),
    climate_zone = case_when(
      abs_lat <= 23.5 ~ "Tropical",
      abs_lat <= 35 ~ "Subtropical",
      abs_lat <= 66.5 ~ "Temperate",
      abs_lat > 66.5 ~ "Polar",
      TRUE ~ NA_character_
    )
  )
city_means$climate_zone
fwrite(city_means, "${LOCAL_ANALYSIS_ROOT}/data/10_03_108city_mean_variables.csv")

# Calculate quantiles
thresholds <- city_mean %>%
  summarise(
    u75 = quantile(urban_dev, 0.75),
    r75 = quantile(reclamation, 0.75),
    r25 = quantile(risk, 0.25),
    r75_risk = quantile(risk, 0.75),
    median_urban = median(urban_dev),
    median_recl = median(reclamation)
  )




# 数据已经加载为 merged_data.  初始化结果表
results <- data.table(
  Variable = character(),
  Breakpoint = numeric(),
  Slope_low = numeric(),
  Slope_high = numeric(),
  Delta_Slope = numeric(),
  R_squared = numeric(),
  AIC = numeric(),
  Valid_points = integer()
)

# 定义需要分析的变量
var_names <- setdiff(
  names(merged_data),
  c("x", "y", "risk", "city", "city_id", "extraction_date")
)

# 批量逐变量分析
for (var in var_names) {
  cat("🔍 正在分析变量:", var, "\n")

  # 提取当前变量和风险值，单独去除NA
  temp_data <- merged_data[, .(risk, current_var = get(var))]
  temp_data <- na.omit(temp_data)

  # 检查有效数据点数目，过少则跳过
  if (nrow(temp_data) < 50) { # 可以根据需求修改阈值
    cat("⚠️", var, "有效数据点过少，跳过。\n")
    next
  }

  # 拟合 piecewise 回归树
  formula <- risk ~ current_var
  ctree_fit <- ctree(formula, data = temp_data, control = ctree_control(maxdepth = 2))

  # 提取断点
  split_info <- partykit:::.list.rules.party(ctree_fit)

  if (length(split_info) == 1) {
    breakpoint <- NA
    slope_low <- NA
    slope_high <- NA
    delta_slope <- NA
  } else {
    breakpoint <- as.numeric(gsub(".*<= (.*)", "\\1", split_info[1]))

    # 根据断点分数据计算斜率
    temp_data[, segment := ifelse(current_var <= breakpoint, "low", "high")]
    slope_low <- coef(lm(risk ~ current_var, data = temp_data[segment == "low"]))[2]
    slope_high <- coef(lm(risk ~ current_var, data = temp_data[segment == "high"]))[2]
    delta_slope <- slope_high - slope_low
  }

  # 全局模型的R²与AIC
  lm_global <- lm(risk ~ current_var, data = temp_data)
  r_squared <- summary(lm_global)$r.squared
  aic_val <- AIC(lm_global)

  # 存储结果
  results <- rbind(results, data.table(
    Variable = var,
    Breakpoint = breakpoint,
    Slope_low = slope_low,
    Slope_high = slope_high,
    Delta_Slope = delta_slope,
    R_squared = r_squared,
    AIC = aic_val,
    Valid_points = nrow(temp_data)
  ))
}

# 保存分析结果
fwrite(results, "${LOCAL_ANALYSIS_ROOT}/data/piecewise_108cities_results.csv")

# 定义显著非线性标准
nonlinear_vars <- results[
  !is.na(Breakpoint) & # 存在断点
    abs(Delta_Slope) >= 0.01 & # 斜率变化显著
    Breakpoint > quantile(results$Breakpoint, 0.01, na.rm = TRUE) &
    Breakpoint < quantile(results$Breakpoint, 0.99, na.rm = TRUE) & # 断点位置合理
    R_squared >= 0.1 & # 有较强的解释能力
    Valid_points > 5000 # 数据点足够多
]

print(nonlinear_vars[order(-abs(Delta_Slope))])
selected_vars <- nonlinear_vars$Variable
# 仅选取risk及selected_vars，并去掉缺失值
selected_vars
model_data <- merged_data[, c("risk", selected_vars), with = FALSE]
model_data <- na.omit(model_data)
model_data

# 设定XGBoost参数（示例参数）
params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  max_depth = 6,
  eta = 0.1
)

features <- as.data.frame(model_data[, -"risk", with = FALSE])
labels <- model_data$risk

dtrain <- xgb.DMatrix(data = as.matrix(features), label = labels)
# 明确告诉 DALEX 如何对新数据做预测
predict_xgb <- function(model, newdata) {
  newdata <- xgb.DMatrix(data = as.matrix(newdata))
  predict(model, newdata)
}
xgb_model <- xgb.train(
  params = list(objective = "reg:squarederror", eta = 0.1, max_depth = 6),
  data = dtrain,
  nrounds = 200
)


# 创建 explainer 时添加 predict_function 参数
explainer <- DALEX::explain(
  model = xgb_model,
  data = features,
  y = labels,
  predict_function = predict_xgb,
  label = "XGBoost",
  type = "regression"
)

pdp_plot_list <- list()

for (var in selected_vars) {
  # 提取断点
  bp <- results[Variable == var, Breakpoint]

  # 安全检查
  if (length(bp) == 0 || is.na(bp)) next

  # 计算 PDP
  pdp_data <- partial(
    object = xgb_model,
    pred.var = var,
    train = as.matrix(features),
    grid.resolution = 100,
    plot = FALSE
  )

  # 构建图形
  p <- ggplot(pdp_data, aes_string(x = var, y = "yhat")) +
    geom_line(size = 1.2, color = "#0072B2") +
    # geom_vline(xintercept = bp, linetype = "dashed", color = "red", linewidth = 0.8) +
    # annotate("text",
    #          x = bp, y = max(pdp_data$yhat, na.rm = TRUE),
    #          label = paste0("Breakpoint = ", round(bp, 2)),
    #          hjust = -0.1, vjust = 0.5, size = 4, color = "red") +
    labs(
      x = var,
      y = "Partial dependence"
      # title = paste("PDP:", var)
    ) +
    theme(
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
      axis.line = element_line(color = "black"),
      axis.ticks = element_line(color = "black"),
      text = element_text(size = 13)
    )

  pdp_plot_list[[var]] <- p
}
wrap_plots(pdp_plot_list, ncol = 3)
ggsave("${LOCAL_DATABASE_ROOT}/evaluations/PDP_108cities_plot.pdf", width = 8, height = 12)

# =====SEM BUDILD=======


city_mean_table_scaled <- city_means
city_mean_table_scaled$reclamation_area <- as.numeric(city_mean_table_scaled$reclamation_area)
num_cols <- names(which(sapply(city_mean_table_scaled, is.numeric)))

scaled_data <- as.data.table(scale(city_mean_table_scaled[, ..num_cols]))
names(city_mean_table_scaled)

# Replace original data
city_mean_table_scaled[, (num_cols) := scaled_data]
names_clean <- trimws(names(city_mean_table_scaled))

library(corrplot)
num_data <- city_mean_table_scaled[, .SD, .SDcols = is.numeric]

cor_matrix <- cor(num_data, use = "pairwise.complete.obs")
corrplot(cor(num_data, use = "pairwise.complete.obs"))
high_cor_pairs <- which(abs(cor_matrix) > 0.9 & abs(cor_matrix) < 1, arr.ind = TRUE)
# Assuming you have vector of variable names (which variables cor_matrix was calculated for)
var_names <- colnames(cor_matrix)
var_names
high_cor_pairs
# Map row/col indices to variable names
high_cor_pairs_named <- data.frame(
  Var1 = var_names[high_cor_pairs[, 1]],
  Var2 = var_names[high_cor_pairs[, 2]]
)
high_cor_pairs_named$Correlation <- mapply(
  function(i, j) cor_matrix[i, j],
  high_cor_pairs[, 1], high_cor_pairs[, 2]
)

fwrite(high_cor_pairs_named, "${LOCAL_ANALYSIS_ROOT}/data/high_cor_pairs_108cities.csv")
high_cor_pairs_named_unique <- high_cor_pairs_named %>%
  mutate(
    Var_min = pmin(Var1, Var2),
    Var_max = pmax(Var1, Var2)
  ) %>%
  distinct(Var_min, Var_max, .keep_all = TRUE)
high_cor_pairs_named_unique

vars_clean <- trimws(vars_to_remove)

names(city_mean_table_scaled) <- trimws(names(city_mean_table_scaled))
# Remove high collinearity variables again
cols_to_keep <- setdiff(names(city_mean_table_scaled), vars_to_remove)

city_mean_table_filtered <- city_mean_table_scaled[, ..cols_to_keep]


city_mean_table_scaled <- city_mean_table_filtered
names(city_mean_table_scaled)
model3 <- "
  BUILT_S_E2025_2.5min ~ reclamation_area
  salinity_mean ~ reclamation_area
  risk ~ BUILT_S_E2025_2.5min + salinity_mean
"
fit <- sem(model3, data = city_mean_table_scaled)


summary(fit, standardized = TRUE, fit.measures = TRUE)

env_vars <- city_mean_table_scaled[, c(
  "wc2.1_2.5m_bio_1", "wc2.1_2.5m_bio_10", "wc2.1_2.5m_bio_15", "wc2.1_2.5m_bio_16",
  "wc2.1_2.5m_bio_17", "wc2.1_2.5m_bio_18", "wc2.1_2.5m_bio_19", "wc2.1_2.5m_bio_2",
  "wc2.1_2.5m_bio_3", "wc2.1_2.5m_bio_4", "wc2.1_2.5m_bio_8", "wc2.1_2.5m_bio_9"
)]

pca_result <- prcomp(env_vars, center = TRUE, scale. = TRUE)
summary(pca_result)
climate_PC <- as.data.frame(pca_result$x[, 1:4])
colnames(climate_PC) <- c("climate_PC1", "climate_PC2", "climate_PC3", "climate_PC4")

city_mean_table_scaled <- cbind(city_mean_table_scaled, climate_PC)

fa(env_vars, nfactors = 2, rotate = "oblimin")



model <- "
  # Latent variable definition
  Climate =~ wc2.1_2.5m_bio_1
  Soil =~ soc_mean_2024_2_5min_aligned
  Anthropogenic =~ BUILT_S_E2025_2.5min + GDP2020_2.5min + Pop2020_2.5min

  # Manifest variable paths
  salinity_mean ~ a1*Climate + a2*Anthropogenic + a3*reclamation_area
  Soil ~ b1*Climate + b2*Anthropogenic + b3*reclamation_area
  risk ~ c1*salinity_mean + c2*Soil + c3*Anthropogenic + c4*reclamation_area

 # Climate
ind_climate1 := a1 * c1     # Climate -> salinity -> risk
ind_climate2 := b1 * c2     # Climate -> soil -> risk
total_climate := ind_climate1 + ind_climate2

# Anthropogenic
ind_anthro1 := a2 * c1
ind_anthro2 := b2 * c2
total_anthro := ind_anthro1 + ind_anthro2 + c3

# Reclamation
ind_reclaim1 := a3 * c1     # Via changing salinity
ind_reclaim2 := b3 * c2     # Via changing soil
total_reclaim := ind_reclaim1 + ind_reclaim2 + c4

"
fit <- lavaan::sem(model, data = city_mean_table_scaled, std.lv = TRUE, missing = "fiml")
summary(fit, fit.measures = TRUE, standardized = TRUE)
semPaths(fit, whatLabels = "std", layout = "tree", edge.label.cex = 1.2, fade = FALSE)
