# Species-model response curves
# Input: Fitted species models and predictor metadata
# Output: Numeric response curves and plots

source(file.path(Sys.getenv("INVASIONSDM_PROJECT_ROOT", "."), "config", "bootstrap.R"))
# ==============================================================================
# Project: Global Invasion SDM Response Curves (Batch Processing)
# Environment: Server / High Performance Computing
# ==============================================================================
# Load packages.
library(biomod2)
library(terra)
library(dplyr)
library(ggplot2)
library(stringr)
library(data.table) # Process large tables efficiently.

# ==============================================================================
# Configure paths and the environment.
# ==============================================================================
# Configure input paths.
model_dir <- publication_path("WORK_ROOT", "models")
env_tif_path <- publication_path("HPC_INPUT_ROOT", "climate/full_stack.tif")
out_dir_csv <- publication_path("WORK_ROOT", "results/Response_Curves_Plots/Response_Data_CSVs") # Set the CSV output directory.
out_dir_img <- publication_path("WORK_ROOT", "results/Response_Curves_Plots/Response_Curves_Plots") # Set the figure output directory.
cat("Output directory ready:\n")
cat("CSV:", out_dir_csv, "\n")
cat("IMG:", out_dir_img, "\n")
# Create output directories.
if(!dir.exists(out_dir_csv)) dir.create(out_dir_csv)
if(!dir.exists(out_dir_img)) dir.create(out_dir_img)

setwd(model_dir)

# ==============================================================================
# Prepare background data with the Global method to avoid missing values.
# ==============================================================================
cat("Loading predictor rasters...\n")
env_stack <- rast(env_tif_path)
cat("Calculating global statistics (Min, Max, Median)...\n")
env_min <- global(env_stack, min, na.rm = TRUE)
env_max <- global(env_stack, max, na.rm = TRUE)
env_med <- global(env_stack, median, na.rm = TRUE)

# Transpose data and correct column names.
df_min <- as.data.frame(t(env_min[,1]))
names(df_min) <- names(env_stack)

df_max <- as.data.frame(t(env_max[,1]))
names(df_max) <- names(env_stack)

df_med <- as.data.frame(t(env_med[,1]))
names(df_med) <- names(env_stack)

# Combine background data.
# Row 1 contains minimum values to set lower bounds.
# Row 2 contains maximum values to set upper bounds.
# Rows 3-1000 contain medians to hold background values constant.
bg_med_rep <- df_med[rep(1, 998), ]

bg_data <- rbind(df_min, df_max, bg_med_rep)

# Check and fill missing values.
# Use 0 for all-NA edge layers to allow calculation to proceed.
if(any(is.na(bg_data))) {
  bg_data[is.na(bg_data)] <- 0
  cat("Warning: NAs found in stats and filled with 0. (Check your TIF if this happens often)\n")
}

cat("Background data ready.\n")
cat("Rows:", nrow(bg_data), "\n")

# Inspect the first variable range to confirm it is not constant.
check_var <- names(bg_data)[1]
cat("Check Range for", check_var, ":\n")
cat("  Min:", min(bg_data[[check_var]]), "\n")
cat("  Max:", max(bg_data[[check_var]]), "\n")

if(min(bg_data[[check_var]]) == max(bg_data[[check_var]])) {
  stop("Warning: minimum and maximum are equal; check the predictor raster.")
} else {
  cat("Background values contain a nonzero range.\n")
}

# ==============================================================================
# List and filter model files.
# ==============================================================================
cat(">>> [Step 2/4] listing model files...\n")

raw_file_list <- list.files(pattern = "\\.models\\.out$", recursive = TRUE, full.names = TRUE)

# Remove duplicates, preferring Ensemble, then Full/AllData.
file_df <- data.frame(fullpath = raw_file_list) %>%
  mutate(
    folder_path = dirname(fullpath),
    filename = basename(fullpath),
    Priority = case_when(
      str_detect(filename, "ensemble") ~ 1,
      str_detect(filename, "Full|AllData") ~ 2,
      TRUE ~ 3
    )
  ) %>%
  group_by(folder_path) %>%
  arrange(Priority, filename) %>%
  slice(1) %>%
  ungroup()

file_list <- file_df$fullpath
cat("Target species count:", length(file_list), "\n")

# ==============================================================================
# Extract response data for all variables in one loop.
# ==============================================================================
cat(">>> [Step 3/4] Extracting response data for ALL variables...\n")

# Initialize the results list.
response_list_all <- list()
counter <- 0

for (f_path in file_list) {
  counter <- counter + 1

  tryCatch({
    # Load the model.
    capture.output({
      my_model_out <- get(load(f_path))
    })
    sp_name <- my_model_out@sp.name

    # Select models.
    all_models <- get_built_models(my_model_out)
    chosen_model <- NULL
    if (any(grepl("EMmean|EMwmean", all_models))) {
      chosen_model <- grep("EMmean|EMwmean", all_models, value = TRUE)[1]
    } else if (any(grepl("Full|AllData", all_models))) {
      chosen_model <- grep("Full|AllData", all_models, value = TRUE)[1]
    } else {
      chosen_model <- all_models[1]
    }

    # Calculate response curves quietly.
    capture.output({
      rp_res <- bm_PlotResponseCurves(
        bm.out = my_model_out,
        models.chosen = chosen_model,
        new.env = bg_data,
        fixed.var = 'median',
        do.plot = FALSE
      )
    })

    # Extract the data frame from the returned list.
    # Handle differences between biomod2 return structures.
    rp_dat <- NULL

    if (is.data.frame(rp_res)) {
      rp_dat <- rp_res # Older versions return a data frame directly.
    } else if (is.list(rp_res) && "tab" %in% names(rp_res)) {
      rp_dat <- rp_res$tab # Newer versions store the table in $tab.
    } else {
      # Try direct conversion or the first list element.
      try(rp_dat <- as.data.frame(rp_res), silent=TRUE)
    }

    # Append results to the list.
    if (!is.null(rp_dat) && nrow(rp_dat) > 0) {
      # Standardize biomod2 columns: expl.name, expl.val and pred.val.
      clean_dat <- rp_dat %>%
        select(Variable = expl.name, Env_Value = expl.val, Probability = pred.val) %>%
        mutate(Species = sp_name)

      response_list_all[[sp_name]] <- clean_dat
    }

    if(counter %% 10 == 0) cat(".")
    if(counter %% 500 == 0) cat("\n")

  }, error = function(e) {
    # Continue past failed calculations.
  })
}

cat("\nMerging dataframes...\n")
# Combine results into one data frame, potentially containing millions of rows.
master_df <- rbindlist(response_list_all)

cat("Total rows extracted:", nrow(master_df), "\n")
cat("Variables found in models:", paste(unique(master_df$Variable), collapse=", "), "\n")

# ==============================================================================
# Save CSV files and plots separately for each variable.
# ==============================================================================
cat(">>> [Step 4/4] Splitting by variable, saving CSVs and plotting...\n")

# List variables with extracted response data.
available_vars <- unique(master_df$Variable)

for (var_name in available_vars) {

  cat("Processing variable:", var_name, "...\n")

  # Select data for this variable.
  plot_df <- master_df[Variable == var_name]

  # Normalize predictions from 0-1000 to 0-1.
  if(max(plot_df$Probability, na.rm=T) > 1.5) {
    plot_df$Probability <- plot_df$Probability / 1000
  }

  # Save the CSV table.
  csv_name <- file.path(out_dir_csv, paste0("Response_Data_", var_name, ".csv"))
  fwrite(plot_df, csv_name)

  # Plot response curves with a cloud background.
  # Draw a cloud plot only when enough species are present.
  n_sp <- length(unique(plot_df$Species))

  p <- ggplot(plot_df, aes(x = Env_Value, y = Probability)) +
    # Use a blue background cloud.
    geom_line(aes(group = Species), color = "#377EB8", alpha = 0.02, linewidth = 0.2) +
    # Use a red trend line.
    geom_smooth(method = "gam", color = "#D55E00", fill = "#D55E00",
                alpha = 0.3, linewidth = 1.2) +
    labs(
      title = paste("Global Response:", var_name),
      subtitle = paste0("Aggregated response (n=", n_sp, " species)"),
      x = var_name,
      y = "Probability of Occurrence"
    ) +
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    theme_minimal(base_size = 14) +
    theme(
      plot.background = element_rect(fill = "white", color = NA), # Use a white background for saved figures.
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
      panel.grid.major = element_line(color = "grey90"),
      panel.grid.minor = element_blank()
    )

  # Save the figure.
  ggsave(filename = file.path(out_dir_img, paste0("Response_Curve_", var_name, ".png")),
         plot = p, width = 8, height = 6, dpi = 300)
}

cat(">>> ALL DONE. Check folders:", out_dir_csv, "and", out_dir_img, "\n")
