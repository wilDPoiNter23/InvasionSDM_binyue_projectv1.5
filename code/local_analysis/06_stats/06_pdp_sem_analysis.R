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
library(gratia)
library(ggeffects)

.libPaths(c("D:/R-4.4.2/library", .libPaths()))
unlink("D:/R-4.4.2/library/scales", recursive = TRUE)
install.packages("scales", lib = "D:/R-4.4.2/library", dependencies = TRUE)
find.package("ggplot2")
find.package("scales")
packageVersion("ggplot2")
packageVersion("scales")
remove.packages("scales", lib = "D:/R-4.4.2/library")
install.packages("scales", lib = "D:/R-4.4.2/library", type = "binary")
install.packages("httpgd")
library(scales, lib.loc = "D:/R-4.4.2/library")

# ==== 1. Load response variable (richness / risk map) =======
r <- terra::rast("E:/RcodeforTest/database/evaluations/hotspot_mean_suitability_normalized2.0.tif") # or .tif
reclamation_city_shp <- st_read("E:/k_PhD_NKU/202407biomod/data/data/reclaimed43cities.shp")

vars_to_remove <- c(
  "Pop2020_2.5min",
  "temperature_mean",
  "wc2.1_2.5m_bio_11",
  "wc2.1_2.5m_bio_16",
  "wc2.1_2.5m_bio_17",
  "wc2.1_2.5m_bio_4",
  "wc2.1_2.5m_bio_5",
  "wc2.1_2.5m_bio_6",
  "wc2.1_2.5m_bio_7",
  "wc2.1_2.5m_bio_9",
  "wc2.1_2.5m_elev"
)
city_mean_table <- fread("E:/k_PhD_NKU/202407biomod/data/10_01city_variable_means_updated.csv")

# ====== 2. Load explanatory variables (multi-layer factor raster) =======
factor_stack <- rast("E:/RcodeforTest/full_stack.tif")

full_stack <- c(r, factor_stack)

names(full_stack)

names(full_stack)[1] <- "risk"

city_sf <- st_make_valid(reclamation_city_shp)
city_vect <- vect(city_sf)

city_vect <- project(city_vect, full_stack)
names(city_vect)

crs(city_vect)
crs(full_stack)

cities <- unique(city_vect$shapeName)
cities
# ======= 3. Loop through each city region =======
all_samples <- list()
cities <- unique(city_vect$shapeName)

# Create progress tracking
start_time <- Sys.time()
failed_cities <- character(0)
success_count <- 0
sample_df
city_name <- cities[1]

one_city <- city_vect[city_vect$shapeName == city_name, ]
one_city
plot(one_city)
city_stack <- tryCatch(
  {
    # First crop, then mask
    cropped <- crop(full_stack, one_city)
    cropped
  },
  error = function(e) {
    cat("❌ Failed to crop/mask for", city_name, ":", e$message, "\n")
    return(NULL)
  }
)

cropped <- crop(full_stack, one_city)
plot(cropped)
print(cropped)
n_valid <- global(!is.na(cropped[[1]]), "sum", na.rm = TRUE)
print(n_valid)
all_vals <- terra::values(cropped)

# Only keep rows where risk is not NA
non_na_idx <- !is.na(all_vals[, "risk"])
filtered_vals <- all_vals[non_na_idx, ]

# Extract coordinates
coords <- as.data.frame(xyFromCell(cropped, which(non_na_idx)))
sample_df <- cbind(coords, filtered_vals)
sample_df

sample_df <- sample_df[, colSums(!is.na(sample_df)) > 0]
sample_df
names(sample_df)
sample_df[3]


# Improved city raster data extraction loop
for (i in seq_along(cities)) {
  city_name <- cities[i]
  cat(sprintf("[%02d/%02d] 🚧 Processing: %s\n", i, length(cities), city_name))

  # Extract current city polygon
  one_city <- city_vect[city_vect$shapeName == city_name, ]

  if (nrow(one_city) == 0) {
    cat("⚠️ No geometry found for city:", city_name, "\n")
    failed_cities <- c(failed_cities, city_name)
    next
  }

  # Crop & mask - Fix: add mask operation
  city_stack <- tryCatch(
    {
      # First crop, then mask
      cropped <- crop(full_stack, one_city)
      cropped
    },
    error = function(e) {
      cat("❌ Failed to crop/mask for", city_name, ":", e$message, "\n")
      return(NULL)
    }
  )

  if (is.null(city_stack)) {
    failed_cities <- c(failed_cities, city_name)
    next
  }

  # Check if raster data is valid
  if (all(is.na(values(city_stack[[1]], na.rm = FALSE)))) {
    cat("⚠️ All values are NA for", city_name, "\n")
    failed_cities <- c(failed_cities, city_name)
    next
  }

  # Check number of valid cells and data quality
  n_cell <- tryCatch(
    {
      # Fix: use more robust counting method
      sum(!is.na(values(city_stack[[1]])))
    },
    error = function(e) {
      cat("⚠️ Error counting cells for", city_name, ":", e$message, "\n")
      return(0)
    }
  )

  if (is.na(n_cell) || n_cell < 10) {
    cat("⚠️ Too few non-NA pixels (", n_cell, ") for:", city_name, "\n")
    failed_cities <- c(failed_cities, city_name)
    next
  }

  # Additional data quality checks
  cat("🔍 Checking data quality for", city_name, "...\n")
  value_range <- tryCatch(
    {
      range(values(city_stack), na.rm = TRUE)
    },
    error = function(e) {
      cat("⚠️ Error checking value range:", e$message, "\n")
      return(c(NA, NA))
    }
  )

  if (any(is.na(value_range))) {
    cat("⚠️ Cannot determine value range for", city_name, "\n")
  } else {
    cat("📊 Value range:", round(value_range[1], 4), "to", round(value_range[2], 4), "\n")
  }

  # Check if all values are the same (this would cause sampling issues)
  unique_values <- tryCatch(
    {
      unique_vals <- unique(values(city_stack[[1]]), na.rm = TRUE)
      if (length(unique_vals) > 100) unique_vals[1:100] else unique_vals # Limit check quantity
    },
    error = function(e) {
      return(NULL)
    }
  )

  if (!is.null(unique_values) && length(unique_values) <= 1) {
    cat("⚠️ All pixels have the same value (", unique_values[1], ") for", city_name, "\n")
  }

  # Extract all valid raster points (improved extraction method)
  cat("📊 Available pixels:", n_cell, ", extracting all pixels\n")

  # Improved data extraction method
  sample_df <- NULL

  cat("🔄 Extracting all pixels using improved method...\n")

  all_vals <- terra::values(city_stack)

  # Only keep rows where risk is not NA
  non_na_idx <- !is.na(all_vals[, "risk"])
  filtered_vals <- all_vals[non_na_idx, ]

  # Extract coordinates
  coords <- as.data.frame(xyFromCell(city_stack, which(non_na_idx)))
  sample_df <- cbind(coords, filtered_vals)

  sample_df <- sample_df[, colSums(!is.na(sample_df)) > 0]

  if (!is.null(sample_df) && nrow(sample_df) > 0) {
    # Add city identifier and other metadata
    sample_df$city <- city_name
    sample_df$city_id <- i
    sample_df$extraction_date <- Sys.Date()

    # Data quality check (modified: check validity of each variable instead of complete cases)
    numeric_cols <- sapply(sample_df, is.numeric)
    numeric_cols[c("city_id")] <- FALSE # Exclude ID columns

    if (sum(numeric_cols) > 0) {
      # Check valid data proportion for each variable
      var_completeness <- sapply(
        sample_df[, numeric_cols, drop = FALSE],
        function(x) sum(!is.na(x)) / length(x)
      )

      cat("📊 Variable completeness rates:\n")
      for (var_name in names(var_completeness)) {
        cat(sprintf(
          "  - %s: %.1f%% (%d/%d valid)\n",
          var_name,
          var_completeness[var_name] * 100,
          sum(!is.na(sample_df[[var_name]])),
          nrow(sample_df)
        ))
      }

      # Check if risk variable exists, if so, ensure it is 100% valid
      if (!is.null(risk_col) && risk_col %in% names(sample_df)) {
        risk_completeness <- var_completeness[risk_col]
        if (risk_completeness < 1.0) {
          cat("❌ Risk variable has missing values, this should not happen!\n")
          failed_cities <- c(failed_cities, city_name)
          next
        }
        cat("✅ Risk variable is 100% complete as expected\n")
      }

      # Check if too many variables have high missing rates
      high_missing_vars <- names(var_completeness)[var_completeness < 0.3]
      if (length(high_missing_vars) > length(var_completeness) * 0.7) {
        cat("⚠️ Warning: Many variables have high missing rates in", city_name, "\n")
        cat("Variables with >70% missing:", paste(high_missing_vars, collapse = ", "), "\n")
      }
    }

    # Check if data range is reasonable (check each variable separately)
    for (col in names(sample_df)[numeric_cols]) {
      valid_values <- sample_df[[col]][!is.na(sample_df[[col]])]
      if (length(valid_values) > 0) {
        col_range <- range(valid_values)
        if (diff(col_range) == 0) {
          cat(
            "⚠️ Warning: Variable", col, "has no variation in", city_name,
            "(value:", col_range[1], ")\n"
          )
        } else {
          cat(
            "📈 Variable", col, "range:",
            round(col_range[1], 4), "to", round(col_range[2], 4),
            sprintf("(%d valid values)\n", length(valid_values))
          )
        }
      } else {
        cat("❌ Warning: Variable", col, "has no valid values in", city_name, "\n")
      }
    }

    all_samples[[city_name]] <- sample_df
    success_count <- success_count + 1
    cat("✅ Successfully processed", city_name, "with", nrow(sample_df), "pixels\n")

    # Display memory usage (when dataset is large)
    if (nrow(sample_df) > 10000) {
      cat("📊 Large dataset (", nrow(sample_df), " rows). Current memory usage:\n")
      print(gc())
    }
  } else {
    cat("❌ No valid pixels extracted from", city_name, "\n")
    failed_cities <- c(failed_cities, city_name)
  }

  # Clean up temporary variables
  rm(city_stack, one_city)

  # Periodic memory cleanup
  if (i %% 3 == 0) {
    gc(verbose = FALSE)
    cat("🧹 Memory cleanup completed\n")
  }

  cat(sprintf(
    "Progress: %d/%d cities completed, %d successful, %d failed\n",
    i, length(cities), success_count, length(failed_cities)
  ))
  cat(paste(rep("-", 60), collapse = ""), "\n\n")
}


# Merge all city data
merged_data <- tryCatch(
  {
    # Use rbindlist (if data.table package available) or rbind to merge
    if (require(data.table, quietly = TRUE)) {
      rbindlist(all_samples, fill = TRUE) # fill=TRUE handles different column counts
    } else {
      do.call(rbind, all_samples)
    }
  },
  error = function(e) {
    cat("⚠️ Direct merge failed, trying step-by-step merge...\n")

    # Step-by-step merge, handling possible column name inconsistencies
    result <- all_samples[[1]]
    for (i in 2:length(all_samples)) {
      current_data <- all_samples[[i]]

      # Ensure column names are consistent
      common_cols <- intersect(names(result), names(current_data))
      if (length(common_cols) < ncol(result) - 3) { # Allow minor column name differences
        cat("⚠️ City", names(all_samples)[i], "has inconsistent column names\n")
      }

      # Merge data
      result <- rbind(result[, common_cols], current_data[, common_cols])
    }
    return(result)
  }
)

cat("✅ Data merge complete! Total samples:", nrow(merged_data), "pixels\n")
cat("📋 Number of variables:", ncol(merged_data), "\n")

write.csv(merged_data, "E:/k_PhD_NKU/202407biomod/data/pixelfromcity.csv")
str(merged_data)
risk_vars <- grep("risk", names(merged_data), ignore.case = TRUE, value = TRUE)
risk_vars <- risk_vars[!grepl("city|id|date", risk_vars, ignore.case = TRUE)] # Exclude auxiliary variables
target_var <- risk_vars[1]
# Identify predictor variables (exclude coordinates, city identifiers, etc.)
exclude_vars <- c("x", "y", "city", "city_id", "extraction_date", target_var)
predictor_vars <- names(merged_data)[!names(merged_data) %in% exclude_vars]
cat("📈 Predictor variables (", length(predictor_vars), "):", paste(predictor_vars, collapse = ", "), "\n")

# 2.3 Data quality check
cat("\n🔍 Data quality check:\n")

# Check target variable
if (!is.null(target_var)) {
  target_na <- sum(is.na(merged_data[[target_var]]))
  cat("🎯 Target variable missing:", target_na, "(", round(target_na / nrow(merged_data) * 100, 2), "%)\n")

  if (target_na > 0) {
    cat("⚠️ Warning: Target variable has missing values, removing these observations\n")
    merged_data <- merged_data[!is.na(merged_data[[target_var]]), ]
    cat("✅ Sample size after removal:", nrow(merged_data), "\n")
  }
}

# Check missing patterns in predictor variables
cat("\n📋 Predictor variable missingness:\n")
missing_summary <- sapply(merged_data[predictor_vars], function(x) {
  c(
    n_missing = sum(is.na(x)),
    pct_missing = round(sum(is.na(x)) / length(x) * 100, 2),
    n_valid = sum(!is.na(x))
  )
})

missing_df <- as.data.frame(t(missing_summary))
missing_df <- missing_df[order(missing_df$pct_missing, decreasing = TRUE), ]
print(missing_df)

# Identify high-missingness variables
high_missing_vars <- rownames(missing_df)[missing_df$pct_missing > 30]
if (length(high_missing_vars) > 0) {
  cat("\n⚠️ High-missingness variables (>30%):", paste(high_missing_vars, collapse = ", "), "\n")
  cat("Suggestion: Consider removing or special handling for these variables\n")
}

# 3. Prepare datasets for different analyses
# ================================================================================

cat("\n📊 Preparing analysis datasets...\n")

# 3.1 PDP analysis dataset (requires complete cases or imputation)
cat("🔄 Preparing PDP analysis dataset...\n")

# Select variables with low missingness for PDP
low_missing_vars <- rownames(missing_df)[missing_df$pct_missing <= 20] # Missingness <=20%
pdp_predictors <- intersect(predictor_vars, low_missing_vars)

if (!is.null(target_var)) {
  pdp_data <- merged_data[, c(target_var, pdp_predictors, "city", "x", "y")]
} else {
  pdp_data <- merged_data[, c(pdp_predictors, "city", "x", "y")]
}

# Remove completely missing observations (for PDP analysis)
if (!is.null(target_var)) {
  complete_cases_pdp <- complete.cases(pdp_data[, c(target_var, pdp_predictors)])
  pdp_data_complete <- pdp_data[complete_cases_pdp, ]

  cat("✅ PDP dataset:\n")
  cat("   - Number of variables:", length(pdp_predictors), "predictor variables\n")
  cat(
    "   - Complete cases:", nrow(pdp_data_complete), "(",
    round(nrow(pdp_data_complete) / nrow(merged_data) * 100, 2), "% of total)\n"
  )
  cat("   - Number of cities:", length(unique(pdp_data_complete$city)), "\n")
}

# 3.2 SEM analysis dataset (can retain more missing values, handle with FIML later)
cat("\n🔄 Preparing SEM analysis dataset...\n")

# SEM can handle missing values, retain more variables
moderate_missing_vars <- rownames(missing_df)[missing_df$pct_missing <= 50] # Missingness <=50%
sem_predictors <- intersect(predictor_vars, moderate_missing_vars)

if (!is.null(target_var)) {
  sem_data <- merged_data[, c(target_var, sem_predictors, "city", "x", "y")]
} else {
  sem_data <- merged_data[, c(sem_predictors, "city", "x", "y")]
}

cat("✅ SEM dataset:\n")
cat("   - Number of variables:", length(sem_predictors), "predictor variables\n")
cat("   - Total samples:", nrow(sem_data), "\n")
cat("   - Number of cities:", length(unique(sem_data$city)), "\n")

# 4. Descriptive Statistics
# ================================================================================

cat("\n📈 Generating descriptive statistics...\n")

# 4.1 Target variable statistics
if (!is.null(target_var) && target_var %in% names(merged_data)) {
  target_stats <- summary(merged_data[[target_var]])
  cat("🎯 Target variable", target_var, "statistics:\n")
  print(target_stats)

  # Check distribution
  cat("📊 Target variable distribution info:\n")
  cat("   - Skewness:", round(moments::skewness(merged_data[[target_var]], na.rm = TRUE), 3), "\n")
  cat("   - Kurtosis:", round(moments::kurtosis(merged_data[[target_var]], na.rm = TRUE), 3), "\n")
}

# 4.2 City sample size statistics
city_counts <- table(merged_data$city)
cat("\n🏙️ Sample size per city:\n")
city_summary <- data.frame(
  city = names(city_counts),
  n_pixels = as.numeric(city_counts),
  percentage = round(as.numeric(city_counts) / sum(city_counts) * 100, 2)
)
city_summary <- city_summary[order(city_summary$n_pixels, decreasing = TRUE), ]
print(city_summary)

# 5. Save results and generate report
# ================================================================================

cat("\n💾 Saving analysis datasets...\n")

# Save as RDS format (preserve data types)
if (!is.null(target_var)) {
  saveRDS(pdp_data_complete, "pdp_analysis_data.rds")
  cat("✅ PDP analysis data saved: pdp_analysis_data.rds\n")
}

saveRDS(sem_data, "sem_analysis_data.rds")
cat("✅ SEM analysis data saved: sem_analysis_data.rds\n")

saveRDS(merged_data, "merged_all_data.rds")
cat("✅ Complete merged data saved: merged_all_data.rds\n")

# Can also save as CSV format
if (!is.null(target_var)) {
  write.csv(pdp_data_complete, "pdp_analysis_data.csv", row.names = FALSE)
}
write.csv(sem_data, "sem_analysis_data.csv", row.names = FALSE)

# 6. Generate data quality report
# ================================================================================

cat("\n📋 Generating data quality report...\n")

# Create report
report <- list(
  summary = list(
    total_pixels = nrow(merged_data),
    total_cities = length(unique(merged_data$city)),
    total_variables = ncol(merged_data),
    target_variable = target_var,
    predictor_variables = length(predictor_vars)
  ),
  data_quality = list(
    missing_summary = missing_df,
    high_missing_vars = high_missing_vars,
    city_distribution = city_summary
  ),
  datasets = list(
    pdp_data_size = if (!is.null(target_var)) nrow(pdp_data_complete) else NA,
    pdp_variables = length(pdp_predictors),
    sem_data_size = nrow(sem_data),
    sem_variables = length(sem_predictors)
  )
)

saveRDS(report, "data_quality_report.rds")
cat("✅ Data quality report saved: data_quality_report.rds\n")

# 7. Output final summary
# ================================================================================

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("🎉 Data merge and preprocessing complete!\n")
cat(paste(rep("=", 80), collapse = ""), "\n")

cat("📊 Data Summary:\n")
cat("   - Total sample size:", nrow(merged_data), "pixels\n")
cat("   - Number of cities:", length(unique(merged_data$city)), "\n")
cat("   - Total variables:", ncol(merged_data), "\n")
if (!is.null(target_var)) {
  cat("   - Target variable:", target_var, "\n")
  cat("   - PDP analysis available:", nrow(pdp_data_complete), "samples\n")
}
cat("   - SEM analysis available:", nrow(sem_data), "samples\n")

cat("\n🎯 Recommendations for follow-up analysis:\n")
cat("   1. PDP Analysis: Use pdp_analysis_data.rds\n")
cat("   2. SEM Analysis: Use sem_analysis_data.rds (Can use FIML for missing values)\n")
cat("   3. Check if high-missingness variables need special handling\n")
cat("   4. Consider variable transformation to improve distribution\n")

cat("\n🔗 Suggested Analysis Workflow:\n")
cat("   PDP Analysis → Identify important variables → SEM Modeling → GLM Verification\n")

# Display data objects currently usable
cat("\n💡 Data objects in current environment:\n")
cat("   - merged_data: Complete merged data\n")
if (!is.null(target_var)) {
  cat("   - pdp_data_complete: Complete data for PDP analysis\n")
}
cat("   - sem_data: Data for SEM analysis\n")
cat("   - report: Data quality report\n")

cat("\n✨ Ready to start PDP and SEM analysis!\n")

results <- fread("E:/k_PhD_NKU/202407biomod/data/piecewise_analysis_city_results.csv")
merged_data <- fread("E:/k_PhD_NKU/202407biomod/data/pixelfromcity.csv")
# Define significant nonlinearity criteria
nonlinear_vars <- results[
  !is.na(Breakpoint) & # Breakpoint exists
    abs(Delta_Slope) >= 0.01 & # Significant slope change
    Breakpoint > quantile(results$Breakpoint, 0.01, na.rm = TRUE) &
    Breakpoint < quantile(results$Breakpoint, 0.99, na.rm = TRUE) & # Breakpoint position reasonable
    R_squared >= 0.1 & # Strong explanatory power
    Valid_points > 5000 # Sufficient data points
]

print(nonlinear_vars[order(-abs(Delta_Slope))])
selected_vars <- nonlinear_vars$Variable
# Select only risk and selected_vars, and remove missing values

model_data <- merged_data[, c("risk", selected_vars), with = FALSE]
model_data <- na.omit(model_data)
model_data

# Set XGBoost parameters (example parameters)
params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  max_depth = 6,
  eta = 0.1
)



plots <- list()
features <- as.data.frame(model_data[, -"risk", with = FALSE])
labels <- model_data$risk

dtrain <- xgb.DMatrix(data = as.matrix(features), label = labels)

xgb_model <- xgb.train(
  params = list(objective = "reg:squarederror", eta = 0.1, max_depth = 6),
  data = dtrain,
  nrounds = 200
)


# Explicitly tell DALEX how to predict on new data
predict_xgb <- function(model, newdata) {
  newdata <- xgb.DMatrix(data = as.matrix(newdata))
  predict(model, newdata)
}

# Add predict_function parameter when creating explainer
explainer <- explain(
  model = xgb_model,
  data = features,
  y = labels,
  predict_function = predict_xgb,
  label = "XGBoost"
)


pdp_plot_list <- list()

for (var in selected_vars) {
  # Extract breakpoint
  bp <- results[Variable == var, Breakpoint]

  # Safety check
  if (length(bp) == 0 || is.na(bp)) next

  # Calculate PDP
  pdp_data <- partial(
    object = xgb_model,
    pred.var = var,
    train = as.matrix(features),
    grid.resolution = 100,
    plot = FALSE
  )

  # Construct plot
  p <- ggplot(pdp_data, aes_string(x = var, y = "yhat")) +
    geom_line(size = 1.2, color = "#0072B2") +
    geom_vline(xintercept = bp, linetype = "dashed", color = "red", linewidth = 0.8) +
    annotate("text",
      x = bp, y = max(pdp_data$yhat, na.rm = TRUE),
      label = paste0("Breakpoint = ", round(bp, 2)),
      hjust = -0.1, vjust = 0.5, size = 4, color = "red"
    ) +
    labs(
      x = var,
      y = "Partial dependence",
      title = paste("PDP:", var)
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
ggsave("E:/RcodeforTest/database/evaluations/PDP_panel_plot.pdf", width = 8, height = 12)

lm_fit <- lm(risk ~ reclamation_area + GDP2020_2.5min + Pop2020_2.5min + BUILT_S_E2025_2.5min, data = city_mean_table)
summary(lm_fit)

city_mean_table

city_mean_table_scaled <- city_mean_table
city_mean_table_scaled$reclamation_area <- as.numeric(city_mean_table_scaled$reclamation_area)
num_cols <- names(which(sapply(city_mean_table_scaled, is.numeric)))

scaled_data <- as.data.table(scale(city_mean_table_scaled[, ..num_cols]))
names(city_mean_table_scaled)

# Replace original data
city_mean_table_scaled[, (num_cols) := scaled_data]
names_clean <- trimws(names(city_mean_table_scaled))
vars_clean <- trimws(vars_to_remove)

names(city_mean_table_scaled) <- trimws(names(city_mean_table_scaled))
# Delete high collinearity variables again
cols_to_keep <- setdiff(names(city_mean_table_scaled), vars_to_remove)

city_mean_table_filtered <- city_mean_table_scaled[, ..cols_to_keep]


city_mean_table_scaled <- city_mean_table_filtered

names(city_mean_table_scaled)
# Example simplified model (customizable)
# Model 2
model2 <- "
 	reclamation_area ➝ BUILT
risk ~ BUILT
"

# Re-fit
fit <- sem(model2, data = city_mean_table_scaled, missing = "fiml")
summary(fit, standardized = TRUE, fit.measures = TRUE)

semPaths(fit, whatLabels = "std", layout = "tree", edge.label.cex = 1.2, fade = FALSE)


model3 <- "
  BUILT_S_E2025_2.5min ~ reclamation_area
  salinity_mean ~ reclamation_area
  risk ~ BUILT_S_E2025_2.5min + salinity_mean
"
fit <- sem(model3, data = city_mean_table_scaled)


summary(fit, standardized = TRUE, fit.measures = TRUE)
semPaths(fit, whatLabels = "std", layout = "tree", edge.label.cex = 1.2, fade = FALSE)



model <- "

# Latent variable definition
urban_dev =~ BUILT_S_E2025_2.5min + GDP2020_2.5min + Pop2020_2.5min
disturbance =~ reclamation_area + VIIRS_2023_2.5min
climate =~ wc2.1_2.5m_bio_1 + wc2.1_2.5m_bio_12 + wc2.1_2.5m_bio_15
soil =~ sand_mean_2024_2_5min_aligned + phh2o_mean_2024_2_5min_aligned

# Urbanization and disturbance intersect
urban_dev ~~ disturbance

# Urbanization/disturbance affects climate and soil
climate ~ urban_dev + disturbance
soil ~ urban_dev + disturbance

# Risk is affected by urbanization, environmental factors comprehensively
risk ~ urban_dev + disturbance + climate + soil
"

gam_model <- gam(risk ~ s(reclamation_area), data = city_mean_table_scaled, method = "REML")
summary(gam_model)
plot(gam_model, shade = TRUE, seWithMean = TRUE)

library(corrplot)
num_data <- city_mean_table_scaled[, .SD, .SDcols = is.numeric]

cor_matrix <- cor(num_data, use = "pairwise.complete.obs")
corrplot(cor(num_data, use = "pairwise.complete.obs"))

cor_matrix

names(num_data)

num_data_clean <- city_mean_table_scaled
names(city_mean_table_scaled)
library(piecewiseSEM)


summary(model4)
model_env_mediation <- psem(
  lm(salinity_mean ~ reclamation_area, data = num_data_clean),
  lm(temperature_mean ~ reclamation_area, data = num_data_clean),
  lm(risk ~ salinity_mean + temperature_mean + reclamation_area, data = num_data_clean)
)
summary(model_env_mediation)



model_psem <- psem(
  lm(salinity_mean ~ reclamation_area, data = num_data_clean),
  lm(temperature_mean ~ reclamation_area, data = num_data_clean),
  lm(VIIRS_2023_2.5min ~ reclamation_area, data = num_data_clean),
  lm(urban_index ~ BUILT_S_E2025_2.5min + GDP2020_2.5min + Pop2020_2.5min, data = num_data_clean), # Construct factor score
  lm(risk ~ salinity_mean + temperature_mean + VIIRS_2023_2.5min + urban_index + climate_index, data = num_data_clean)
)
summary(model_psem)


model_B <- "
  # Structural paths between environmental drivers (if necessary)
  salinity_mean ~ wc2.1_2.5m_bio_1 + soc_mean_2024_2_5min_aligned

  # Risk regression path
  risk ~ salinity_mean + wc2.1_2.5m_bio_1 + wc2.1_2.5m_bio_12 + soc_mean_2024_2_5min_aligned + sand_mean_2024_2_5min_aligned
"
library(psych)
fa(city_mean_table_scaled[, c("BUILT_S_E2025_2.5min", "GDP2020_2.5min", "reclamation_area")], nfactors = 1, rotate = "none")
fa(city_mean_table_scaled[, c("wc2.1_2.5m_bio_1", "wc2.1_2.5m_bio_12", "wc2.1_2.5m_bio_15")], nfactors = 1)
fa(city_mean_table_scaled[, c("salinity_mean", "salinity_range")], nfactors = 1, rotate = "none")
# Fit
fit <- sem(model_B, data = city_mean_table_scaled, missing = "fiml")

# Output results
summary(fit, standardized = TRUE, fit.measures = TRUE)

model_piecewise <- psem(
  # Environmental response driven jointly by urban and disturbance
  lm(salinity_mean ~ reclamation_area + BUILT_S_E2025_2.5min + VIIRS_2023_2.5min, data = df),
  lm(temperature_mean ~ reclamation_area + BUILT_S_E2025_2.5min + VIIRS_2023_2.5min, data = df),

  # Risk model
  lm(risk ~ salinity_mean + temperature_mean + BUILT_S_E2025_2.5min + VIIRS_2023_2.5min + reclamation_area, data = df)
)


env_vars <- c(
  "wc2.1_2.5m_bio_1", "wc2.1_2.5m_bio_12", "wc2.1_2.5m_bio_13",
  "wc2.1_2.5m_bio_14", "wc2.1_2.5m_bio_15", "wc2.1_2.5m_bio_18"
)

# Construct formula
formula <- as.formula(paste("risk ~", paste(env_vars, collapse = " + ")))

# Fit model
fit_env <- lm(formula, data = city_mean_table_scaled)

# 输出回归结果
summary(fit_env)


soil_vars <- city_mean_table_scaled[, c(
  "bdod_mean_2024_2_5min_aligned",
  "cfvo_mean_2024_2_5min_aligned",
  "nitrogen_mean_2024_2_5min_aligned",
  "ocd_mean_2024_2_5min_aligned",
  "phh2o_mean_2024_2_5min_aligned",
  "sand_mean_2024_2_5min_aligned",
  "silt_mean_2024_2_5min_aligned",
  "soc_mean_2024_2_5min_aligned"
)]

fa_soil <- fa(soil_vars, nfactors = 1, rotate = "none")
print(fa_soil)

clim_vars <- city_mean_table_scaled[, c(
  "wc2.1_2.5m_bio_1", "wc2.1_2.5m_bio_12", "wc2.1_2.5m_bio_13",
  "wc2.1_2.5m_bio_14", "wc2.1_2.5m_bio_15", "wc2.1_2.5m_bio_18"
)]
fa(clim_vars, nfactors = 1, rotate = "none", fm = "minres")




soil_model <- "
soil_condition =~ bdod_mean_2024_2_5min_aligned +
                    nitrogen_mean_2024_2_5min_aligned +
                    ocd_mean_2024_2_5min_aligned +
                    phh2o_mean_2024_2_5min_aligned +
                    soc_mean_2024_2_5min_aligned
urban_dev =~ BUILT_S_E2025_2.5min
risk ~ soil_condition + urban_dev
"

soil_model <- "
soil_condition =~ bdod_mean_2024_2_5min_aligned +
                    nitrogen_mean_2024_2_5min_aligned +
                    ocd_mean_2024_2_5min_aligned +
                    phh2o_mean_2024_2_5min_aligned +
                    soc_mean_2024_2_5min_aligned
climate =~ wc2.1_2.5m_bio_1 + wc2.1_2.5m_bio_15

soil_condition ~ a * climate
risk ~ b * soil_condition + c_prime * climate
indirect := a * b
total := indirect + c_prime

"
fit <- sem(soil_model, data = city_mean_table_scaled, missing = "fiml")
summary(fit, standardized = TRUE, fit.measures = TRUE)

model <- "
  # 潜变量构建
  soil_condition =~ bdod_mean_2024_2_5min_aligned +
                    nitrogen_mean_2024_2_5min_aligned +
                    ocd_mean_2024_2_5min_aligned +
                    phh2o_mean_2024_2_5min_aligned +
                    soc_mean_2024_2_5min_aligned
  urban_dev =~ BUILT_S_E2025_2.5min
  climate =~ wc2.1_2.5m_bio_1

  # 显变量路径，并为路径加标签
  salinity_mean ~ a1*climate + a2*urban_dev
  soil_condition ~ b1*climate + b2*urban_dev
  risk ~ c1*soil_condition + c2*salinity_mean + c3*climate

  # 间接效应定义（基于路径标签）
  indirect1 := b1 * c1         # climate → soil_condition → risk
  indirect2 := a1 * c2         # climate → salinity_mean → risk
  indirect3 := a1  * c1    # climate → salinity_mean → soil_condition → risk

  total := indirect1 + indirect2 + indirect3
"
fit <- sem(model, data = city_mean_table_scaled, missing = "fiml")
summary(fit, standardized = TRUE, fit.measures = TRUE)
modindices(fit)



model <- "

  # 潜变量构建（latent variables）
  climate        =~ wc2.1_2.5m_bio_1 + wc2.1_2.5m_bio_12
  soil_condition =~ bdod_mean_2024_2_5min_aligned +
                    nitrogen_mean_2024_2_5min_aligned +
                    ocd_mean_2024_2_5min_aligned +
                    phh2o_mean_2024_2_5min_aligned +
                    soc_mean_2024_2_5min_aligned
  urban_dev      =~ BUILT_S_E2025_2.5min

  # 显变量回归路径（带label）
  salinity_mean ~ a1*urban_dev + a2*reclamation_area + a3*climate
  climate       ~ b1*reclamation_area
  soil_condition ~ c1*climate + c2*salinity_mean + c3*urban_dev
  risk          ~ d1*soil_condition + d2*urban_dev + d3*salinity_mean + d4*climate

  # 间接效应定义（indirect effects）
  # reclaim → climate → soil → risk
  ind_reclaim_climate_soil_risk := b1 * c1 * d1

  # reclaim → salinity → soil → risk
  ind_reclaim_salinity_soil_risk := a2 * c2 * d1

  # climate → soil → risk
  ind_climate_soil_risk := c1 * d1

  # salinity → soil → risk
  ind_salinity_soil_risk := c2 * d1

  # 总间接效应（reclaim 到 risk）
  total_indirect_reclaim := ind_reclaim_climate_soil_risk + ind_reclaim_salinity_soil_risk

  # 总效应（reclaim 到 risk）
  total_reclaim := total_indirect_reclaim  # 目前无直接路径，如有可加 e1*reclaim_area 再加 e1

"

model <- "
  # 潜变量构建（latent variables）
  climate        =~ wc2.1_2.5m_bio_1 + wc2.1_2.5m_bio_13+  wc2.1_2.5m_bio_18
  soil_condition =~ bdod_mean_2024_2_5min_aligned +
                    nitrogen_mean_2024_2_5min_aligned +
                    ocd_mean_2024_2_5min_aligned +
                    phh2o_mean_2024_2_5min_aligned +
                    soc_mean_2024_2_5min_aligned
  urban_dev      =~ BUILT_S_E2025_2.5min
 salinity =~ salinity_mean

  # 显变量回归路径（带 label）
  salinity       ~ a1*urban_dev + a2*reclamation_area + a3*climate
  climate        ~ b1*reclamation_area
  soil_condition ~ c1*climate + c2*salinity + c3*urban_dev
  risk           ~ d1*soil_condition + d2*urban_dev + d3*salinity + d4*climate

  # 间接效应（indirect effects）
  ind_reclaim_climate_soil_risk := b1 * c1 * d1
  ind_reclaim_salinity_soil_risk := a2 * c2 * d1
  ind_climate_soil_risk := c1 * d1
  ind_salinity_soil_risk := c2 * d1

  # 总间接效应（reclaim 到 risk）
  total_indirect_reclaim := ind_reclaim_climate_soil_risk + ind_reclaim_salinity_soil_risk

  # Total effect (reclaim to risk)
  total_reclaim := total_indirect_reclaim  # If adding direct path, add e1*reclamation_area then add e1
"
parameterEstimates(fit) %>%
  filter(op == "~~" & est < 0)
fa(clim_vars[, c("wc2.1_2.5m_bio_1", "wc2.1_2.5m_bio_13", "wc2.1_2.5m_bio_15", "wc2.1_2.5m_bio_18")], nfactors = 1)

# ===== Check for curvature/saturation/inverted U (nonlinear) =====
city_df <- csv_2

# 1) Basic GAM: Check for nonlinearity (REML is more stable)
city_df$Regions <- as.factor(city_df$Regions)
city_df$Land_Use_Types <- as.factor(city_df$Land_Use_Types)
city_df$climate_zone <- as.factor(city_df$climate_zone)
city_df$risk_class <- as.factor(city_df$risk_class)
city_df$reclamation_level_manual <- as.factor(city_df$reclamation_level_manual)

city_df$recl_frac <- with(city_df, pmin(reclamation_area / area_km2, 1))
city_df$log_recl <- log1p(city_df$reclamation_area)
# Key derived variables: Reclamation Fraction & Log Transformation
# 1) Basic GAM: Test nonlinear response of Reclamation (controlling for climate/human intensity/salinity/city area)
m1 <- gam(
  risk ~ s(recl_frac, k = 5) + # Reclamation nonlinearity
    s(`BUILT_S_E2025_2.5min`, k = 5) + # Built-up intensity
    s(`wc2.1_2.5m_bio_1`, k = 5) + # Annual mean temperature
    s(`wc2.1_2.5m_bio_15`, k = 5) + # Precipitation seasonality
    s(salinity_mean, k = 5) +
    s(phh2o_mean_2024_2_5min_aligned, k = 5) +
    s(soc_mean_2024_2_5min_aligned, k = 5) +
    s(area_km2, k = 5) + # City area control
    s(Regions, bs = "re"), # Continent/Region random effect
  data = city_df, method = "REML"
)
gam.check(m1)
summary(m1) # Check edf and p-value of s(log_recl) to judge if there is nonlinearity
p5 <- plot(m1, pages = 1, shade = TRUE)
save(p5, "E:/k_PhD_NKU/202407biomod/figure/gam_draft/gam_draft1.jpg")
# 2) Compare with linear term: Is nonlinear better (AIC/test)
m0 <- gam(
  risk ~ log_recl +
    `BUILT_S_E2025_2.5min` +
    `wc2.1_2.5m_bio_1` + `wc2.1_2.5m_bio_15` +
    salinity_mean + phh2o_mean_2024_2_5min_aligned + soc_mean_2024_2_5min_aligned +
    area_km2 + s(Regions, bs = "re"),
  data = city_df, method = "REML"
)
AIC(m0, m1) # Does nonlinearity improve fit

m_fs <- gam(
  risk ~ s(recl_frac, recla_level_byfraction, bs = "fs", k = 5) + # One smooth per risk_class
    s(`BUILT_S_E2025_2.5min`, k = 5) + # Built-up intensity
    s(`wc2.1_2.5m_bio_1`, k = 5) + # Annual mean temperature
    s(`wc2.1_2.5m_bio_15`, k = 5) + # Precipitation seasonality
    s(salinity_mean, k = 5) +
    s(phh2o_mean_2024_2_5min_aligned, k = 5) +
    s(soc_mean_2024_2_5min_aligned, k = 5) +
    s(area_km2, k = 5) + # City area control
    s(Regions, bs = "re"),
  data = city_df, method = "REML"
)
gam.check(m_fs)
# Compare with common curve
AIC(m1, m_fs)

# Or LRT under ML
anova(update(m1, method = "ML"),
  update(m_fs, method = "ML"),
  test = "Chisq"
)

summary(m_fs)
library(gratia)
# fs basis generates a smooth per group, just plot directly
draw(m_fs, parametric = FALSE) # Or:
draw(m_fs, select = "s(recl_frac):risk_classlow")
draw(m_fs, select = "s(recl_frac):risk_class2")
plot(m_fs, pages = 1, shade = TRUE)

install.packages("gratia")
m_lin <- lm(risk ~ log_recl + `BUILT_S_E2025_2.5min` +
  `wc2.1_2.5m_bio_1` + `wc2.1_2.5m_bio_15` +
  salinity_mean + area_km2, data = city_df)
m_seg <- segmented(m_lin, seg.Z = ~log_recl, psi = list(log_recl = 1))
summary(m_seg)
