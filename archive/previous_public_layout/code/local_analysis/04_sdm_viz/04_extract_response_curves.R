# ARCHIVE: historical reference; symbolic paths are intentionally not runnable.
# Original SHA256: 926e43951741dcdcf6f178a0ebb0964b8d2cb4f7d44ed3b361eb562e6170db53
################################################################################
# TASK: (Deep Diagnostic)
# ------------------------------------------------------------------------------
# Goal:  Extract and plot response curves for 800+ species without re-running models.
# Author: Binyue Kang
# Date:   December 2025
# ==============================================================================
#
# DESCRIPTION:
# This script generates an aggregated "response curve cloud" to visualize the
# general ecological response of [800+] invasive species to a dominant
# environmental driver (e.g., Built-up Area / Temperature).
#
# METHODOLOGY:
# 1. Background Sampling: Instead of loading full environmental rasters, we use
#    'spatSample' to extract 5,000 random background points, significantly
#    optimizing computation time.
# 2. Data Extraction: Iterates through pre-calibrated biomod2 ensemble models
#    (.out objects) to calculate marginal response curves for each species.
# 3. Visualization: Uses ggplot2 to create a "spaghetti plot" (semi-transparent
#    individual curves) overlaid with a global GAM trend line, highlighting
#    macro-ecological patterns amidst species-specific idiosyncrasies.
#
# INPUTS:
# - biomod2 ensemble model objects (.models.out)
# - Environmental raster stack (.tif)
#
# OUTPUTS:
# - Figure: SI_Fig_Response_Cloud.png (Nature-style visualization)
# - Data: Combined response table (optional)
# ==============================================================================

library(biomod2)
library(terra)
library(dplyr)
library(ggplot2)
library(stringr)
# 1. Preparation
setwd("${WORK_ROOT}/models")
env_stack <- rast("${HPC_INPUT_ROOT}/climate/full_stack.tif")
target_var <- "GDP2020_2.5min"
cat("=== Optimization Step 1: Generate background sampling data (one-time only) ===\n")
# 【Key Optimization】
# Don't read TIF every time. We randomly sample 1000 points to represent the environmental background of the study area.
# This way biomod2 can calculate medians instantly without reading from disk.
set.seed(123) # Ensure reproducibility
cat("=== Optimization Step 1: Generate background sampling data (fixed version) ===\n")

# Approach A: Use regular sampling
# This lays a grid on the map, always able to capture cells with data
# Set size larger (e.g., 50000), after na.rm the remaining is what we need
bg_data <- spatSample(env_stack, size = 50000, method = "regular", na.rm = TRUE, as.df = TRUE)
names(bg_data) <- names(env_stack)
# Check how many rows we have now
cat("Fixed background data size:", nrow(bg_data), "rows.\n")

# If still 0 or very few (<100), your TIF might have issues
if (nrow(bg_data) < 100) {
  cat("Warning: Still too few sampling points! Attempting brute-force full median calculation...\n")

  # Approach B: Fallback method (if sampling fails, directly calculate median of entire image to construct one row of data)
  # This will be slightly slower, but absolutely reliable
  env_medians <- global(env_stack, median, na.rm = TRUE)
  # Construct a data frame with only 1 row, all medians
  bg_data <- as.data.frame(t(env_medians[, 1]))
  # Replicate to 100 rows to fool the function
  bg_data <- bg_data[rep(1, 100), ]
  cat("Background mean data generated through full calculation.\n")
}

# Confirm again
if (nrow(bg_data) > 0) {
  cat("Background data prepared successfully! Ready to enter loop.\n")
} else {
  stop("Fatal error: Cannot extract any valid values from environmental layers! Please check if your TIF file is corrupted or contains all null values.")
}

cat("Background data preparation complete, size:", nrow(bg_data), "rows. Subsequent calculations will be very fast.\n")


# 2. Test which step causes error - get first model file
test_file_list <- list.files(
  path = "${WORK_ROOT}/models",
  pattern = "\\.models\\.out$",
  recursive = TRUE,
  full.names = TRUE
)
# To improve hit rate, we find a file containing 'ensemble' to test
ensemble_files <- grep("ensemble", test_file_list, value = TRUE)
if (length(ensemble_files) > 0) {
  target_file <- ensemble_files[1]
} else {
  target_file <- test_file_list[1]
}

cat("=== Diagnosing file ===\n")
cat("Path:", target_file, "\n")

# 3. Load model
my_model_out <- get(load(target_file))
sp_name <- my_model_out@sp.name
cat("Species name:", sp_name, "\n")

# ------------------------------------------------------------------------------
# Diagnostic A: Are variable names completely consistent?
# ------------------------------------------------------------------------------
cat("\n>>> [Diagnostic A] Variable name comparison <<<\n")
vars_in_model <- my_model_out@expl.var.names
vars_in_data <- names(bg_data)

cat("Variables required by model (first 5): ", paste(head(vars_in_model, 5), collapse = ", "), "\n")
cat("Variables provided by background data (first 5): ", paste(head(vars_in_data, 5), collapse = ", "), "\n")

# Find differences
missing_in_data <- setdiff(vars_in_model, vars_in_data)
if (length(missing_in_data) > 0) {
  cat("\n❌ Fatal error: Background data missing the following variables (required by model but not in data):\n")
  print(missing_in_data)
} else {
  cat("\n✅ Variable name check passed! All variables required by model are present.\n")
}

# ------------------------------------------------------------------------------
# Diagnostic B: Is model selection logic valid?
# ------------------------------------------------------------------------------
cat("\n>>> [Diagnostic B] Model selection logic <<<\n")
all_built_models <- get_built_models(my_model_out)
cat("Number of models in this object:", length(all_built_models), "\n")
cat("Model name examples (first 3):\n")
print(head(all_built_models, 3))

# Test selection logic
chosen_model <- NULL
if (any(grepl("EMmean|EMwmean", all_built_models))) {
  chosen_model <- grep("EMmean|EMwmean", all_built_models, value = TRUE)[1]
  cat("Logic decision: Selected ensemble model ->", chosen_model, "\n")
} else if (any(grepl("Full|AllData", all_built_models))) {
  chosen_model <- grep("Full|AllData", all_built_models, value = TRUE)[1]
  cat("Logic decision: Selected full data model ->", chosen_model, "\n")
} else {
  chosen_model <- all_built_models[1]
  cat("Logic decision: Selected first model (Fallback) ->", chosen_model, "\n")
}

if (is.null(chosen_model) || is.na(chosen_model)) {
  cat("\n❌ Fatal error: Cannot select any model! Please check grep keywords.\n")
} else {
  cat("\n✅ Model selection successful.\n")
}

# ------------------------------------------------------------------------------
# Diagnostic C: Force run function (without tryCatch)
# ------------------------------------------------------------------------------
cat("\n>>> [Diagnostic C] Attempting to extract data <<<\n")

# If this step errors, it will directly show the specific error message
rp_dat <- bm_PlotResponseCurves(
  bm.out = my_model_out,
  models.chosen = chosen_model,
  new.env = bg_data,
  fixed.var = "median",
  do.plot = FALSE
)

cat("Function execution completed.\n")
cat("Return result type:", class(rp_dat), "\n")
cat("Return data row count:", nrow(rp_dat), "\n")

if (nrow(rp_dat) > 0) {
  cat("\n🎉 Diagnostic successful! Data extraction normal.\n")
  print(head(rp_dat))
} else {
  cat("\nLog: Warning: Function didn't error, but returned empty table.\n")
}
# # 4. Merge and Plot
# cat("\nExtraction complete, plotting...\n")
# big_response_df <- do.call(rbind, response_data_list)
# write.csv(big_response_df, '${WORK_ROOT}/results/Response_Curves_Plots/GDP2020_2.5min_Response.csv')


# # Normalization
# if(max(big_response_df$Probability, na.rm=T) > 1.5) {
#   big_response_df$Probability <- big_response_df$Probability / 1000
# }

# p_cloud <- ggplot(big_response_df, aes(x = Env_Value, y = Probability)) +
#   geom_line(aes(group = Species), color = "#377EB8", alpha = 0.03, linewidth = 0.3) +
#   geom_smooth(method = "gam", color = "#D55E00", fill = "#D55E00",
#               alpha = 0.3, linewidth = 1.5) +
#   labs(title = "c | Macro-ecological response pattern",
#        subtitle = paste0("Response to ", target_var, " (n=", length(unique(big_response_df$Species)), ")"),
#        x = target_var, y = "Probability") +
#  theme(
#     plot.background = element_rect(fill = "transparent", color = NA),
#     panel.background = element_rect(fill = "transparent", color = NA),
#     panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank()
#   )

# ggsave("${WORK_ROOT}/results/Response_Curves_Plots/GDP2020_2.5min_Response.png", p_cloud, width = 8, height = 6, bg = "white")
# cat("Success! Image saved.\n")
