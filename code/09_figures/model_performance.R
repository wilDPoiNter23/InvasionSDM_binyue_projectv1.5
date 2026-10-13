# Model-performance figures
# Input: Combined model and selected-model evaluation tables
# Output: Performance and ensemble-weight plots

source(file.path(Sys.getenv("INVASIONSDM_PROJECT_ROOT", "."), "config", "bootstrap.R"))
# ============================================================================
# Model evaluation reports summary for 800+ Species
# Summary plots for supplementary figures.
# Many selected SDMs (approximately 500 or more) lack validation scores. For full-data models, calculate mean validation scores from the preceding RUN1, RUN2, RUN3 and other runs used in selection.
# ============================================================================
library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(purrr)

# 1. read the data
 df_all <- read.csv(publication_path("MODEL_EVALUATIONS"))
df_best <- read.csv(publication_path("SELECTED_MODEL_EVALUATIONS"))
alg_TSS <- df_all  %>%
  filter(Type == "individual",
         metric.eval == "TSS",
         !is.na(validation)) %>%
  group_by(algo) %>%
  summarise(
    n = n(),
    median_TSS = median(validation),
    IQR_low  = quantile(validation, 0.25),
    IQR_high = quantile(validation, 0.75)
  )

alg_ROC <- df_all  %>%
  filter(Type == "individual",
         metric.eval == "ROC",
         !is.na(validation)) %>%
  group_by(algo) %>%
  summarise(
    n = n(),
    median_AUC = median(validation),
    IQR_low  = quantile(validation, 0.25),
    IQR_high = quantile(validation, 0.75)
  )

alg_TSS
alg_ROC
# 2. Figure A: algorithm performance using df_all.
# Extract the required algorithm columns.
plot_data_A <- df_all %>%
  filter(metric.eval %in% c("TSS", "ROC")) %>%
  # validation contains evaluation scores from the validation set.
  mutate(Score = as.numeric(validation),
         # Group EMmean, EMmedian and related methods under Ensemble.
         Algorithm = case_when(
           grepl("EM", algo) ~ "Ensemble",
           algo == "RFd" ~ "Random Forest",
           algo == "MAXNET" ~ "MaxEnt",
           algo == "GLM" ~ "GLM",
           TRUE ~ "Other"
         )) %>%
  filter(!is.na(Score))

p_algo <- ggplot(plot_data_A, aes(x = reorder(Algorithm, Score, median), y = Score, fill = Algorithm)) +
  geom_boxplot(outlier.size = 0.2, outlier.alpha = 0.3, width = 0.6, alpha = 0.8) +
  facet_wrap(~metric.eval, scales = "free_y") + # Facet TSS and ROC.
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
  filename = publication_path("OUTPUT_ROOT", "figures/sdm_evalueation/03_All_Models_evaluationCombined.png"),
  plot     = p_algo ,
  width    = 7,
  height   = 6,
  device = grDevices::png
)
 ggsave(
  filename = publication_path("OUTPUT_ROOT", "figures/sdm_evalueation/03_All_Models_evaluationCombined.pdf"),
  plot     = p_algo ,
  width    = 7,
  height   = 6)
# 3. Figure B: selected-model score distribution using df_best.
# Combine Type categories under Final Models.

best_TSS_val <- df_best %>%
  filter(metric.eval == "TSS",
         !is.na(validation))

best_ROC_val <- df_best %>%
  filter(metric.eval == "ROC",
         !is.na(validation))

summary_TSS_val <- best_TSS_val %>%
  summarise(
    n_models   = n(),
    median_TSS = median(validation),
    IQR_TSS_low  = quantile(validation, 0.25),
    IQR_TSS_high = quantile(validation, 0.75)
  )

summary_ROC_val <- best_ROC_val %>%
  summarise(
    n_models   = n(),
    median_AUC = median(validation),
    IQR_AUC_low  = quantile(validation, 0.25),
    IQR_AUC_high = quantile(validation, 0.75)
  )

summary_TSS_val
summary_ROC_val

best_TSS_cal_only <- df_best %>%
  filter(metric.eval == "TSS",
         is.na(validation),
         !is.na(calibration))

summary_TSS_cal_only <- best_TSS_cal_only %>%
  summarise(
    n_models   = n(),
    median_TSS = median(calibration),
    IQR_TSS_low  = quantile(calibration, 0.25),
    IQR_TSS_high = quantile(calibration, 0.75)
  )

best_ROC_cal_only <- df_best %>%
  filter(metric.eval == "ROC",
         is.na(validation),
         !is.na(calibration))

summary_ROC_cal_only <- best_ROC_cal_only %>%
  summarise(
    n_models   = n(),
    median_TSS = median(calibration),
    IQR_TSS_low  = quantile(calibration, 0.25),
    IQR_TSS_high = quantile(calibration, 0.75)
  )
summary_TSS_cal_only
summary_ROC_cal_only

# ==============================================================================
# Step 2: calculate proxy scores from the full records table.
# ==============================================================================
# Calculate mean cross-validation performance (RUN1, RUN2 and other runs) for each species and metric.
# Use this mean as a proxy for final models without validation scores.
df_proxy <- df_all %>%
  # 1. Exclude full-data allRun/allData records and retain cross-validation runs.
  filter(!run %in% c("allRun", "allData")) %>%
  # 2. Select the required metrics.
  filter(metric.eval %in% c("TSS", "ROC")) %>%
  # 3. Ensure numeric scores.
  mutate(validation = as.numeric(validation)) %>%
  filter(!is.na(validation)) %>%
  # 4. Calculate grouped mean scores.
  # Group by Species and metric.
  # The proxy averages cross-validation scores by species and metric across algorithms, irrespective of the selected final algorithm.
  group_by(Species, metric.eval) %>%
  summarise(Mean_CV_Score = mean(validation, na.rm = TRUE), .groups = "drop")

# ==============================================================================
# Step 3: clean the final-model table and fill missing scores.
# ==============================================================================

plot_data <- df_best %>%
  # 1. Select metrics.
  filter(metric.eval %in% c("TSS", "ROC")) %>%
  # 2. Join the proxy-score table.
  left_join(df_proxy, by = c("Species", "metric.eval")) %>%
  mutate(
    # 3. Use validation when available; replace NA with Mean_CV_Score.
    # Use as.numeric to convert character values.
    Real_Validation = as.numeric(validation),

    Final_Score = case_when(
      !is.na(Real_Validation) ~ Real_Validation, # Retain existing validation scores for the 271 available records.
      is.na(Real_Validation) & !is.na(Mean_CV_Score) ~ Mean_CV_Score, # Use proxy scores for the 598 records with NA validation.
      TRUE ~ as.numeric(calibration) # If no proxy score is available, use calibration as the fallback.
    )
  ) %>%
  # 4. Remove remaining NA values.
  filter(!is.na(Final_Score))

# Inspect the current summary statistics.
summary_stats <- plot_data %>%
  group_by(metric.eval) %>%
  summarise(
    Min = min(Final_Score),
    Median = median(Final_Score),
    Mean = mean(Final_Score),
    Max = max(Final_Score)
  )
  plot_data_clean <- plot_data %>%
  filter(Final_Score > 0)  # Exclude scores of 0 and -1.

# Recalculate the median.
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
# Confirm whether calibration or validation is appropriate in the Best table.
# Distinguish calibration (full-data fitting) from validation (mean cross-validation performance).
# Specify whether the displayed score uses validation, calibration or their mean.
# mutate(Score = as.numeric(calibration)) ; calibration describes fit, whereas validation describes predictive evaluation.

# 2. Define publication colours using dark blue and dark red/orange.
nature_colors <- c("ROC" = "#377EB8", "TSS" = "#E41A1C")
# Alternative blue-green palette: c("ROC" = "#0072B2", "TSS" = "#009E73").

# 3. Plot.
p_performance <- ggplot(plot_data_clean, aes(x = metric.eval, y = Final_Score, fill = metric.eval, color = metric.eval)) +

  # 1. Jittered observations show individual scores and sample size.
  # Use low alpha to show overlapping points.
  geom_jitter(position = position_jitter(width = 0.25), size = 0.6, alpha = 0.15) +

  # 2. Violin plots show distribution density.
  # alpha = 0.5 keeps underlying observations visible.
  geom_violin(trim = FALSE, alpha = 0.5, color = NA, width = 0.6) +

  # 3. Boxplots show summary statistics.
  # Overlay white-filled boxes with thin black borders.
  geom_boxplot(width = 0.1, fill = "white", color = "black",
               outlier.shape = NA, # Suppress boxplot outliers because jitter already displays the observations.
               alpha = 0.3, size = 0.4) +

  # 4. Colours and scales.
  scale_fill_manual(values = nature_colors) +
  scale_color_manual(values = nature_colors) +
  scale_y_continuous(limits = c(0, 1.05), breaks = seq(0, 1, 0.2), expand = c(0, 0)) +

  # 5. Titles and labels for a supplementary figure.
  labs(
    title = "b | Predictive performance of final selected models",
    # Calculate the species count for the subtitle.
    subtitle = paste0("Evaluated via cross-validation metrics across ", length(unique(plot_data$Species)), " species"),
    y = "Evaluation Score",
    x = NULL
  ) +

  # 6. Transparent theme.
theme_minimal(base_size = 24) +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.border = element_rect(color = "black", fill = NA, size = 2),

    # Remove all grid lines.
    panel.grid = element_blank(),
    # ------------------------------------

    legend.position = "none",

    text = element_text(color = "black"),
    axis.text = element_text(color = "black", size = 22),
    plot.title = element_text(face = "bold", size = 14)
  )

# Preview.
print(p_performance)
 ggsave(
  filename = publication_path("OUTPUT_ROOT", "figures/sdm_evalueation/03.b_Best_Models_evaluationCombined.png"),
  plot     = p_performance ,
  width    = 7,
  height   = 6,
  device = grDevices::png
)
 ggsave(
  filename = publication_path("OUTPUT_ROOT", "figures/sdm_evalueation/03.b_Best_Models_evaluationCombined.pdf"),
  plot     = p_performance ,
  width    = 7,
  height   = 6)
# 2. Extract TSS scores for the following model records:
# Individual models.
# Use the TSS metric.
# Use the calibration column for this performance summary.
# Select allRun records using grepl("allRun", full.name).
tss_alg <-  df_all %>%
  filter(Type == "individual",
         metric.eval == "TSS",
         !is.na(calibration),
         grepl("allRun", full.name)) %>%
  group_by(Species, algo) %>%
  summarise(
    TSS = mean(calibration),  # Average multiple allRun records if present.
    .groups = "drop"
  )

# 3. Normalise TSS within species to obtain weights, truncating negative scores to 0.
weights_raw <- tss_alg %>%
  group_by(Species) %>%
  mutate(
    TSS_pos = pmax(TSS, 0),
    w = ifelse(sum(TSS_pos) > 0, TSS_pos / sum(TSS_pos), NA_real_)
  ) %>%
  ungroup()
weights_raw
# 4. Retain species with at least two algorithms having positive ensemble weights.
weights_multi <- weights_raw %>%
  group_by(Species) %>%
  filter(sum(!is.na(w) & w > 0) >= 2) %>%
  ungroup()
alg_w_summary <- weights_multi %>%
  group_by(algo) %>%
  summarise(
    n_species = n_distinct(Species),
    median_w = median(w, na.rm = TRUE),
    IQR_low  = quantile(w, 0.25, na.rm = TRUE),
    IQR_high = quantile(w, 0.75, na.rm = TRUE)
  )

alg_w_summary
# 5. Panel a: distributions of algorithm weights.
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

    # Remove all grid lines.
    panel.grid = element_blank(),
    # ------------------------------------

    legend.position = "none",

    text = element_text(color = "black"),
    axis.text = element_text(color = "black", size = 22),
    plot.title = element_text(face = "bold", size = 14)
  )

p_weights
 ggsave(
  filename = publication_path("OUTPUT_ROOT", "figures/sdm_evalueation/Ensemble_weight.png"),
  plot     = p_weights,
  width    = 7,
  height   = 6,
  device = grDevices::png
)
 ggsave(
  filename = publication_path("OUTPUT_ROOT", "figures/sdm_evalueation/Ensemble_weight.pdf"),
  plot     = p_weights,
  width    = 7,
  height   = 6)
