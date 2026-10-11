# ARCHIVE: historical reference; symbolic paths are intentionally not runnable.
# Original SHA256: 5b766977e15290597a738721cf93a9300b6ccdd95d6046bcd2e87b15d6287130
# ============================================================================
# Model evaluation reports summary for 800+ Species
# Exprcially fot supplymentary figure
# Author: Binyue Kang 02/12/2025
# A major issue was discovered: among the selected optimal SDMs, nearly 500+ models have empty validation columns. For this situation, the principle is: although the Full Model has no validation score, it was selected from previous RUN1, RUN2, RUN3... Therefore, find the validation scores of those RUNs that participated in building this Full Model and calculate the average.
# ============================================================================
library(dplyr)
library(readr)
library(stringr)
library(purrr)

# 1. read the data
df_all <- read.csv("${LOCAL_ANALYSIS_ROOT}/data/organized_table/03_All_Models_evaluationCombined.csv")
df_best <- read.csv("${LOCAL_ANALYSIS_ROOT}/data/organized_table/04_All_Best_Models_Combined.csv")
alg_TSS <- df_all %>%
  filter(
    Type == "individual",
    metric.eval == "TSS",
    !is.na(validation)
  ) %>%
  group_by(algo) %>%
  summarise(
    n = n(),
    median_TSS = median(validation),
    IQR_low = quantile(validation, 0.25),
    IQR_high = quantile(validation, 0.75)
  )

alg_ROC <- df_all %>%
  filter(
    Type == "individual",
    metric.eval == "ROC",
    !is.na(validation)
  ) %>%
  group_by(algo) %>%
  summarise(
    n = n(),
    median_AUC = median(validation),
    IQR_low = quantile(validation, 0.25),
    IQR_high = quantile(validation, 0.75)
  )

alg_TSS
alg_ROC
# --- 2. Plot A: Algorithm comparison (using df_all) ---
# Clean data: extract required algorithm columns
plot_data_A <- df_all %>%
  filter(metric.eval %in% c("TSS", "ROC")) %>%
  # Here validation is the validation set score, most important
  mutate(
    Score = as.numeric(validation),
    # Simplify algorithm names, group EMmean, EMmedian etc. as Ensemble
    Algorithm = case_when(
      grepl("EM", algo) ~ "Ensemble",
      algo == "RFd" ~ "Random Forest",
      algo == "MAXNET" ~ "MaxEnt",
      algo == "GLM" ~ "GLM",
      TRUE ~ "Other"
    )
  ) %>%
  filter(!is.na(Score))

p_algo <- ggplot(plot_data_A, aes(x = reorder(Algorithm, Score, median), y = Score, fill = Algorithm)) +
  geom_boxplot(outlier.size = 0.2, outlier.alpha = 0.3, width = 0.6, alpha = 0.8) +
  facet_wrap(~metric.eval, scales = "free_y") + # Facet display for TSS and ROC
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "a | Performance comparison across modeling algorithms",
    y = "Evaluation Score (Validation)",
    x = NULL
  ) +
  theme_minimal(base_size = 20) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.border = element_rect(color = "black", fill = NA, size = 0.8),
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
p_algo

ggsave(
  filename = "${LOCAL_ANALYSIS_ROOT}/figure/sdm_evalueation/03_All_Models_evaluationCombined.png",
  plot = p_algo,
  width = 7,
  height = 6,
  device = grDevices::png
)
ggsave(
  filename = "${LOCAL_ANALYSIS_ROOT}/figure/sdm_evalueation/03_All_Models_evaluationCombined.pdf",
  plot     = p_algo,
  width    = 7,
  height   = 6
)
# --- 3. Plot B: Final model quality distribution (using df_best) ---
# Strategy: do not distinguish Type, collectively called "Final Models"

best_TSS_val <- df_best %>%
  filter(
    metric.eval == "TSS",
    !is.na(validation)
  )

best_ROC_val <- df_best %>%
  filter(
    metric.eval == "ROC",
    !is.na(validation)
  )

summary_TSS_val <- best_TSS_val %>%
  summarise(
    n_models = n(),
    median_TSS = median(validation),
    IQR_TSS_low = quantile(validation, 0.25),
    IQR_TSS_high = quantile(validation, 0.75)
  )

summary_ROC_val <- best_ROC_val %>%
  summarise(
    n_models = n(),
    median_AUC = median(validation),
    IQR_AUC_low = quantile(validation, 0.25),
    IQR_AUC_high = quantile(validation, 0.75)
  )

summary_TSS_val
summary_ROC_val

best_TSS_cal_only <- df_best %>%
  filter(
    metric.eval == "TSS",
    is.na(validation),
    !is.na(calibration)
  )

summary_TSS_cal_only <- best_TSS_cal_only %>%
  summarise(
    n_models = n(),
    median_TSS = median(calibration),
    IQR_TSS_low = quantile(calibration, 0.25),
    IQR_TSS_high = quantile(calibration, 0.75)
  )

best_ROC_cal_only <- df_best %>%
  filter(
    metric.eval == "ROC",
    is.na(validation),
    !is.na(calibration)
  )

summary_ROC_cal_only <- best_ROC_cal_only %>%
  summarise(
    n_models = n(),
    median_TSS = median(calibration),
    IQR_TSS_low = quantile(calibration, 0.25),
    IQR_TSS_high = quantile(calibration, 0.75)
  )
summary_TSS_cal_only
summary_ROC_cal_only

# ==============================================================================
# Step 2: Calculate "Proxy Scores" from full record table
# ==============================================================================
# We need to calculate the average performance of each species and each metric in cross-validation (RUN1, RUN2...)
# Use this average to represent those Final Models without validation scores
df_proxy <- df_all %>%
  # 1. Exclude full data runs (allRun / allData), only keep cross-validation runs
  filter(!run %in% c("allRun", "allData")) %>%
  # 2. Filter required metrics
  filter(metric.eval %in% c("TSS", "ROC")) %>%
  # 3. Ensure scores are numeric
  mutate(validation = as.numeric(validation)) %>%
  filter(!is.na(validation)) %>%
  # 4. Group and calculate average scores
  # Note: Here we group by Species and metric.
  # Logic: Regardless of which algorithm was finally selected, the species' average performance in cross-validation is the best estimate of its predictability.
  group_by(Species, metric.eval) %>%
  summarise(Mean_CV_Score = mean(validation, na.rm = TRUE), .groups = "drop")

# ==============================================================================
# Step 3: Clean final model table and fill gaps
# ==============================================================================

plot_data <- df_best %>%
  # 1. Filter metrics
  filter(metric.eval %in% c("TSS", "ROC")) %>%
  # 2. Join proxy score table
  left_join(df_proxy, by = c("Species", "metric.eval")) %>%
  mutate(
    # 3. Core logic: prioritize existing validation, if NA, use Mean_CV_Score as substitute
    # as.numeric is important here to prevent character data in original
    Real_Validation = as.numeric(validation),
    Final_Score = case_when(
      !is.na(Real_Validation) ~ Real_Validation, # If already has validation score (those 271), use original
      is.na(Real_Validation) & !is.na(Mean_CV_Score) ~ Mean_CV_Score, # If NA (those 598), use proxy
      TRUE ~ as.numeric(calibration) # In rare case proxy also missing, use calibration as fallback to prevent error
    )
  ) %>%
  # 4. Filter again to ensure no NA (double insurance)
  filter(!is.na(Final_Score))

# --- Check current statistics (for your own assurance) ---
summary_stats <- plot_data %>%
  group_by(metric.eval) %>%
  summarise(
    Min = min(Final_Score),
    Median = median(Final_Score),
    Mean = mean(Final_Score),
    Max = max(Final_Score)
  )
plot_data_clean <- plot_data %>%
  filter(Final_Score > 0) # Exclude outliers of 0 and -1

# Calculate median again
final_stats <- plot_data_clean %>%
  group_by(metric.eval) %>%
  summarise(
    n = n(),
    Median = median(Final_Score),
    Mean = mean(Final_Score),
    IQR_25 = quantile(Final_Score, 0.25),
    IQR_75 = quantile(Final_Score, 0.75)
  )

print(final_stats)
# plot_data_B <- df_best %>%
#   filter(metric.eval %in% c("TSS", "ROC")) %>%
#   # Note: Need to confirm whether to use calibration or validation from the Best table
#   # Usually Best Model reports calibration (full data training) or validation (mean CV)
#   # Assuming we focus on validation or calibration here, or take the mean of both
#   mutate(Score = as.numeric(calibration)) # Assuming using calibration to show final fit, or validation to show predictive power

# 2. Define Nature-style color scheme (advanced deep blue and dark red/orange)
nature_colors <- c("ROC" = "#377EB8", "TSS" = "#E41A1C")
# Or blue-green scheme: c("ROC" = "#0072B2", "TSS" = "#009E73")

# 3. Plotting
p_performance <- ggplot(plot_data_clean, aes(x = metric.eval, y = Final_Score, fill = metric.eval, color = metric.eval)) +

  # 1. Jitter points: show data volume, increase credibility
  # Set alpha low to make points look like clouds
  geom_jitter(position = position_jitter(width = 0.25), size = 0.6, alpha = 0.15) +

  # 2. Violin plot: show distribution density
  # alpha=0.5 allows background points to show through slightly
  geom_violin(trim = FALSE, alpha = 0.5, color = NA, width = 0.6) +

  # 3. Boxplot: show statistics
  # Place on top layer, white fill, black thin border
  geom_boxplot(
    width = 0.1, fill = "white", color = "black",
    outlier.shape = NA, # Outliers already shown by jitter, turn off here
    alpha = 0.3, size = 0.4
  ) +

  # 4. Colors and scales
  scale_fill_manual(values = nature_colors) +
  scale_color_manual(values = nature_colors) +
  scale_y_continuous(limits = c(0, 1.05), breaks = seq(0, 1, 0.2), expand = c(0, 0)) +

  # 5. Title and labels (conform to journal Supplementary Figure format)
  labs(
    title = "b | Predictive performance of final selected models",
    # Automatically calculate species count for subtitle
    subtitle = paste0("Evaluated via cross-validation metrics across ", length(unique(plot_data$Species)), " species"),
    y = "Evaluation Score",
    x = NULL
  ) +

  # 6. Transparent theme
  theme_minimal(base_size = 24) +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.border = element_rect(color = "black", fill = NA, size = 2),

    # --- Modified here: remove all grid lines ---
    panel.grid = element_blank(),
    # ------------------------------------

    legend.position = "none",
    text = element_text(color = "black"),
    axis.text = element_text(color = "black", size = 22),
    plot.title = element_text(face = "bold", size = 14)
  )

# Preview
print(p_performance)
ggsave(
  filename = "${LOCAL_ANALYSIS_ROOT}/figure/sdm_evalueation/03.b_Best_Models_evaluationCombined.png",
  plot = p_performance,
  width = 7,
  height = 6,
  device = grDevices::png
)
ggsave(
  filename = "${LOCAL_ANALYSIS_ROOT}/figure/sdm_evalueation/03.b_Best_Models_evaluationCombined.pdf",
  plot     = p_performance,
  width    = 7,
  height   = 6
)
# 2. Extract cross-validated TSS:
#    - individual models
#    - Use TSS metric
#    - Use calibration column (you used it before to summarize performance)
#    - Select one record representing allRun (here simply filter using grepl("allRun", full.name))
tss_alg <- df_all %>%
  filter(
    Type == "individual",
    metric.eval == "TSS",
    !is.na(calibration),
    grepl("allRun", full.name)
  ) %>%
  group_by(Species, algo) %>%
  summarise(
    TSS = mean(calibration), # In case there are multiple allRun records, take average
    .groups = "drop"
  )

# 3. Normalize TSS by species to get weights (truncate negatives to 0)
weights_raw <- tss_alg %>%
  group_by(Species) %>%
  mutate(
    TSS_pos = pmax(TSS, 0),
    w = ifelse(sum(TSS_pos) > 0, TSS_pos / sum(TSS_pos), NA_real_)
  ) %>%
  ungroup()
weights_raw
# 4. Keep only true "ensemble" species: at least two algorithms have positive weights
weights_multi <- weights_raw %>%
  group_by(Species) %>%
  filter(sum(!is.na(w) & w > 0) >= 2) %>%
  ungroup()
alg_w_summary <- weights_multi %>%
  group_by(algo) %>%
  summarise(
    n_species = n_distinct(Species),
    median_w = median(w, na.rm = TRUE),
    IQR_low = quantile(w, 0.25, na.rm = TRUE),
    IQR_high = quantile(w, 0.75, na.rm = TRUE)
  )

alg_w_summary
# 5. Plot panel a: weight distribution by algorithm
p_weights <- ggplot(weights_multi, aes(x = algo, y = w, fill = algo)) +
  geom_violin(trim = TRUE, alpha = 0.6, colour = NA) +
  geom_boxplot(width = 0.15, outlier.size = 0.4, alpha = 0.9) +
  scale_y_continuous("Ensemble weight", limits = c(0.2, 0.6)) +
  scale_x_discrete("Algorithm") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal(base_size = 24) +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.border = element_rect(color = "black", fill = NA, size = 2),

    # --- Modified here: remove all grid lines ---
    panel.grid = element_blank(),
    # ------------------------------------

    legend.position = "none",
    text = element_text(color = "black"),
    axis.text = element_text(color = "black", size = 22),
    plot.title = element_text(face = "bold", size = 14)
  )

p_weights
ggsave(
  filename = "${LOCAL_ANALYSIS_ROOT}/figure/sdm_evalueation/Ensemble_weight.png",
  plot = p_weights,
  width = 7,
  height = 6,
  device = grDevices::png
)
ggsave(
  filename = "${LOCAL_ANALYSIS_ROOT}/figure/sdm_evalueation/Ensemble_weight.pdf",
  plot     = p_weights,
  width    = 7,
  height   = 6
)
