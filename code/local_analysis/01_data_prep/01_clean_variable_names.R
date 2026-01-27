# ==============================================================================
# Environmental Variable Metadata and Visualization Functions
# ==============================================================================
#
# Purpose:
#   Define metadata for all environmental variables used in SDM modeling,
#   including human-readable labels, categories, and color palettes.
#   Provides functions for creating publication-quality variable importance
#   plots and environmental layer visualizations.
#
# Manuscript Section:
#   - Methods: Environmental Variables (Table 1)
#   - Results: Variable Importance Analysis (Figure 3)
#   - Supplementary: Environmental Layer Maps (Figure S1-S5)
#
# Inputs:
#   - Variable importance summary: results/evaluations/importance_summary_cleaned.csv
#   - Environmental raster layers: data/environmental/*.tif
#
# Outputs:
#   - Figure 3: Top 25 environmental drivers (boxplot)
#   - Figure 4: Variable importance by category (violin plot)
#   - Figure 5: Ensemble vs individual model comparison
#   - Figure 6: Frequency of top drivers across species
#   - Supplementary: Global environmental layer maps
#
# Dependencies:
#   - dplyr, ggplot2, terra, viridis, tidyterra, forcats
#
# Author: Binyue Kang
# Date Created: 2025-XX-XX
# Last Modified: 2026-01-23
# ==============================================================================

# Load required packages
library(dplyr)
library(stringr)
library(viridis)
library(terra)
library(ggplot2)
library(tidyterra)
library(forcats)

# Load configuration (if available)
if (file.exists("code_clean/00_setup/00_config.R")) {
  source("code_clean/00_setup/00_config.R")
}

# ==============================================================================
# Data Loading
# ==============================================================================

# Load variable importance data
# TODO: Update these paths to use config$results_evaluations
data <- read.csv("E:/RcodeforTest/database/evaluations/importance/importance_summary_cleaned.csv")
# Alternative path (update as needed)
# data <- read.csv('E:/k_PhD_NKU/202407biomod/data/organized_table/02_importance_summary_cleaned.csv')

# ==============================================================================
# Color Palettes for Different Variable Types
# ==============================================================================

# Temperature variables: Blue -> White -> Red (diverging, no green)
pal_temp <- colorRampPalette(c("#2166AC", "#F7F7F7", "#B2182B"))

# Intensity/density variables (e.g., built-up, GDP): Light yellow -> Orange -> Dark brown
pal_intensity <- colorRampPalette(c("#fff7bc00", "#FEC44F", "#D95F0E", "#8C2D04"))

# Soil / SOC variables: Light gray-green -> Dark brown-green
pal_soil <- colorRampPalette(c("#F7FCF5", "#C7E9C0", "#74C476", "#238B45", "#00441B"))

# Population density: Light blue -> Deep blue -> Purple gradient
pal_pop <- colorRampPalette(c(
  "#F7FBFF", # Very light blue
  "#51a3e6", # Light blue
  "#2d64da", # Medium blue
  "#b81c7c", # Deep purple-blue
  "#be0a0a" # Deep red
))

# Default palette: Viridis (if unsure which palette to use)
pal_viridis <- function(n) {
  viridis::viridis(n, option = "E")
}

# ==============================================================================
# Variable Category Color Palette (for publication figures)
# ==============================================================================

# Nature-inspired color scheme for variable categories
# Used in Figures 3-6 for consistent visual identity
nature_palette <- c(
  "Climate"        = "#61998B", # Teal (climate/temperature)
  "Soil"           = "#d58d3a", # Earth brown (soil)
  "Topography"     = "#EDE6C1", # Rock gray (mountains/terrain)
  "Marine"         = "#A4D1DB", # Ocean blue (water)
  "Anthropogenic"  = "#F06256" # Brick red (human/urban)
)

# ==============================================================================
# Variable Metadata Table
# ==============================================================================
#
# This table maps raw variable names to human-readable labels and categories.
# All variables used in the manuscript should be documented here.
#
# Data sources:
# - Climate: WorldClim 2.1 (Fick & Hijmans 2017)
# - Soil: SoilGrids 2024 (Poggio et al. 2021)
# - Marine: Bio-ORACLE (Assis et al. 2018)
# - Anthropogenic: Multiple sources (see Methods)
# - Topography: Derived from WorldClim elevation

var_info <- tibble::tribble(
  ~raw, ~label, ~category,

  # ============================================================================
  # Climate Variables (WorldClim 2.1, 2.5 arcmin resolution)
  # ============================================================================
  "wc2.1_2.5m_bio_1", "Annual mean temperature (°C)", "Climate",
  "wc2.1_2.5m_bio_2", "Mean diurnal temperature range (°C)", "Climate",
  "wc2.1_2.5m_bio_3", "Isothermality (%)", "Climate",
  "wc2.1_2.5m_bio_4", "Temperature seasonality (SD × 100)", "Climate",
  "wc2.1_2.5m_bio_5", "Max temperature of warmest month (°C)", "Climate",
  "wc2.1_2.5m_bio_6", "Min temperature of coldest month (°C)", "Climate",
  "wc2.1_2.5m_bio_7", "Temperature annual range (°C)", "Climate",
  "wc2.1_2.5m_bio_8", "Mean temperature of wettest quarter (°C)", "Climate",
  "wc2.1_2.5m_bio_9", "Mean temperature of driest quarter (°C)", "Climate",
  "wc2.1_2.5m_bio_10", "Mean temperature of warmest quarter (°C)", "Climate",
  "wc2.1_2.5m_bio_11", "Mean temperature of coldest quarter (°C)", "Climate",
  "wc2.1_2.5m_bio_12", "Annual precipitation (mm)", "Climate",
  "wc2.1_2.5m_bio_13", "Precipitation of wettest month (mm)", "Climate",
  "wc2.1_2.5m_bio_14", "Precipitation of driest month (mm)", "Climate",
  "wc2.1_2.5m_bio_15", "Precipitation seasonality (CV)", "Climate",
  "wc2.1_2.5m_bio_16", "Precipitation of wettest quarter (mm)", "Climate",
  "wc2.1_2.5m_bio_17", "Precipitation of driest quarter (mm)", "Climate",
  "wc2.1_2.5m_bio_18", "Precipitation of warmest quarter (mm)", "Climate",
  "wc2.1_2.5m_bio_19", "Precipitation of coldest quarter (mm)", "Climate",

  # ============================================================================
  # Soil Variables (SoilGrids 2024, 2.5 arcmin aligned)
  # ============================================================================
  "bdod_mean_2024_2_5min_aligned", "Soil bulk density (g cm⁻³)", "Soil",
  "cfvo_mean_2024_2_5min_aligned", "Coarse fragments volume (%)", "Soil",
  "clay_mean_2024_2_5min_aligned", "Clay content (%)", "Soil",
  "sand_mean_2024_2_5min_aligned", "Sand content (%)", "Soil",
  "silt_mean_2024_2_5min_aligned", "Silt content (%)", "Soil",
  "nitrogen_mean_2024_2_5min_aligned", "Soil nitrogen (g kg⁻¹)", "Soil",
  "ocd_mean_2024_2_5min_aligned", "SOC density (kg m⁻²)", "Soil",
  "soc_mean_2024_2_5min_aligned", "SOC content (g kg⁻¹)", "Soil",
  "phh2o_mean_2024_2_5min_aligned", "Soil pH (H₂O)", "Soil",

  # ============================================================================
  # Anthropogenic Variables (Multiple sources, 2.5 arcmin)
  # ============================================================================
  "BUILT_S_E2025_2.5min", "Built-up fraction", "Anthropogenic",
  "GDP2020_2.5min", "GDP density", "Anthropogenic",
  "Pop2020_2.5min", "Population density", "Anthropogenic",
  "VIIRS_2023_2.5min", "Night-time lights", "Anthropogenic",
  "reclamation_raster", "Reclamation area", "Anthropogenic",

  # ============================================================================
  # Topographic Variables (Derived from WorldClim elevation)
  # ============================================================================
  "wc2.1_2.5m_elev", "Elevation (m)", "Topography",
  "slope", "Slope (°)", "Topography",
  "aspect", "Aspect (°)", "Topography",

  # ============================================================================
  # Marine Variables (Bio-ORACLE, 2.5 arcmin)
  # ============================================================================
  "salinity_mean", "Mean sea surface salinity", "Marine",
  "salinity_range", "Sea surface salinity range", "Marine",
  "temperature_mean", "Mean sea surface temperature (°C)", "Marine",
  "temperature_range", "Sea surface temperature range (°C)", "Marine"
)

# ==============================================================================
# Helper Functions for Variable Name Conversion
# ==============================================================================

# Convert raw variable names to human-readable labels
# Used in all plots to ensure consistent labeling
pretty_label <- function(x) {
  out <- var_info$label[match(x, var_info$raw)]
  out[is.na(out)] <- x[is.na(out)] # Keep original name if no match found
  out
}

# Get variable category (Climate, Soil, Marine, etc.)
get_category <- function(x) {
  out <- var_info$category[match(x, var_info$raw)]
  out[is.na(out)] <- "Other" # Assign "Other" if category not found
  out
}

# ==============================================================================
# Custom ggplot2 Theme for Publication Figures
# ==============================================================================

# Transparent background with black border
# Used for all main figures to ensure consistency
theme_transparent_box <- function() {
  theme_minimal(base_size = 14) +
    theme(
      # Transparent backgrounds for overlay on presentations/posters
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA),
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.box.background = element_rect(fill = "transparent", color = NA),

      # Black border around plot panel
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1.2),

      # Remove grid lines for cleaner appearance
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),

      # Ensure text is visible on any background
      text = element_text(color = "black"),
      axis.text = element_text(color = "black"),

      # Legend positioning
      legend.position = "bottom",
      legend.title = element_text(face = "bold")
    )
}

# ==============================================================================
# Data Preparation for Plotting
# ==============================================================================

# Clean and transform importance data
plot_data <- data %>%
  mutate(
    # Convert raw variable names to readable labels
    Label = pretty_label(Variable),
    Category = get_category(Variable)
  ) %>%
  # Remove rows with missing importance values
  filter(!is.na(Importance))

# Verify data transformation
cat("\nVariable importance data summary:\n")
cat("Total records:", nrow(plot_data), "\n")
cat("Unique variables:", length(unique(plot_data$Variable)), "\n")
cat("Categories:", paste(unique(plot_data$Category), collapse = ", "), "\n\n")
head(plot_data)


# ==============================================================================
# Global Raster Visualization Functions
# ==============================================================================

# Basic function for most environmental layers
plot_global_raster_basic <- function(
    tif_path,
    var_raw_name, # e.g., "wc2.1_2.5m_bio_1"
    palette_fun,
    ncol = 256,
    agg_factor = 4 # Aggregation factor (4, 8, 12...) to reduce resolution
    ) {
  # Load raster file
  r <- terra::rast(tif_path)

  # Reduce resolution to speed up ggplot rendering
  if (!is.null(agg_factor) && agg_factor > 1) {
    r <- terra::aggregate(r, fact = agg_factor, fun = "mean", na.rm = TRUE)
  }

  # Convert to data.frame for ggplot
  df <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
  colname <- names(df)[3]

  # Get human-readable label
  lab <- pretty_label(var_raw_name)

  # Generate color palette
  pal <- palette_fun(ncol)

  ggplot(df) +
    geom_raster(aes(x = x, y = y, fill = .data[[colname]])) +
    scale_fill_gradientn(
      colours = pal, name = lab,
      na.value = "transparent", trans = "log1p"
    ) +
    coord_equal() +
    theme_minimal(base_size = 11) +
    theme(
      axis.title = element_blank(),
      panel.grid = element_blank(),
      legend.position = "right",
      panel.background = element_blank(),
      plot.background = element_blank()
    )
}

# Specialized function for night-time lights (VIIRS) with outlier handling
# Uses 99th percentile clipping to prevent extreme values from dominating color scale
plot_global_raster_viirs <- function(
    tif_path,
    var_raw_name,
    palette_fun,
    ncol = 256,
    agg_factor = 4,
    quantile_clip = 0.99 # Clip values above this quantile
    ) {
  r <- terra::rast(tif_path)

  if (!is.null(agg_factor) && agg_factor > 1) {
    r <- terra::aggregate(r, fact = agg_factor, fun = "mean", na.rm = TRUE)
  }

  df <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
  colname <- names(df)[3]

  # Step 1: Remove zero values (background)
  df <- df[df[[colname]] > 0, ]

  # Step 2: Calculate 99th percentile and clip extreme values
  # This prevents a few extremely bright pixels from dominating the color scale
  limit_high <- quantile(df[[colname]], quantile_clip, na.rm = TRUE)
  df[[colname]][df[[colname]] > limit_high] <- limit_high

  lab <- pretty_label(var_raw_name)
  pal <- palette_fun(ncol)

  ggplot(df) +
    geom_raster(aes(x = x, y = y, fill = .data[[colname]])) +
    scale_fill_gradientn(
      colours = pal,
      name = lab,
      na.value = "transparent",
      trans = "sqrt", # Square root transformation for better visualization
      labels = scales::label_number(accuracy = 1)
    ) +
    coord_equal() +
    theme_minimal(base_size = 11) +
    theme(
      axis.title = element_blank(),
      panel.grid = element_blank(),
      legend.position = "right",
      panel.background = element_rect(fill = "black", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    )
}
# ==============================================================================
# Example: Create elevation map for presentation
# ==============================================================================
# TODO: Update paths to use config
p_elevation <- plot_global_raster_basic(
  tif_path = "E:/k_PhD_NKU/202407biomod/data/data/Topo/wc2.1_2.5m_elev.tif",
  var_raw_name = "wc2.1_2.5m_elev",
  palette_fun = terrain.colors,
  agg_factor = 4 # Adjust based on computer performance
)
p_elevation
# ggsave("E:/k_PhD_NKU/ppt/svg-pic/elev_forppt.pdf", p_elevation,
#        width = 8, height = 6, dpi = 600)

# ==============================================================================
# FIGURE 3: Top 25 Environmental Drivers (Boxplot)
# ==============================================================================
# Manuscript reference: Results section, Variable Importance Analysis
# Identify top 25 variables by median importance across all species
var_order <- plot_data %>%
  group_by(Label) %>%
  summarise(median_imp = median(Importance)) %>%
  arrange(desc(median_imp)) %>%
  head(25) %>%
  pull(Label)

# Create boxplot showing importance distribution for top 25 variables
p1 <- plot_data %>%
  filter(Label %in% var_order) %>%
  ggplot(aes(x = factor(Label, levels = rev(var_order)), y = Importance)) +

  # Boxplot colored by variable category
  geom_boxplot(
    aes(fill = Category),
    outlier.size = 0.8,
    outlier.alpha = 0.6,
    alpha = 0.8, # Transparency for softer colors
    width = 0.7,
    color = "black" # Black borders for clarity
  ) +
  coord_flip() + # Horizontal layout for better label readability

  # Apply nature-inspired color palette
  scale_fill_manual(values = nature_palette) +
  labs(
    title = "Top 25 Environmental Drivers",
    subtitle = "Ranked by median importance across all species",
    x = NULL,
    y = "Importance Score"
  ) +
  theme_transparent_box()

# Display and save figure
# windows()  # Uncomment to display in new window
p1

# TODO: Update paths to use config$results_figures
# ggsave(file.path(config$results_figures, "plot1_importance.png"),
#        plot = p1, width = 10, height = 8, bg = "transparent", dpi = 300)
# ggsave(file.path(config$results_figures, "plot1_importance.svg"),
#        plot = p1, width = 10, height = 8, bg = "transparent", dpi = 300)
# ==============================================================================
# FIGURE 4: Variable Importance by Category (Violin Plot)
# ==============================================================================
# Purpose: Compare importance distributions across variable categories
# (Climate, Soil, Marine, Anthropogenic, Topography)

p2 <- ggplot(plot_data, aes(x = Category, y = Importance, fill = Category)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  labs(
    title = "Importance Distribution by Variable Category",
    subtitle = "Comparison of Anthropogenic vs. Natural drivers",
    x = "Category",
    y = "Importance Score"
  ) +
  theme_bw() +
  theme(legend.position = "none")
# p2  # Uncomment to display
# print(p2)

# ==============================================================================
# FIGURE 5: Ensemble vs Individual Model Comparison
# ==============================================================================
# Purpose: Examine whether ensemble models assign different importance weights
# compared to individual models

# Calculate mean importance by category and model source
# Note: Requires 'Source' column in data (ensemble vs individual)
if ("Source" %in% names(plot_data)) {
  source_comp <- plot_data %>%
    group_by(Category, Source) %>%
    summarise(Mean_Importance = mean(Importance), .groups = "drop")

  # Calculate statistical summaries for each category and source
  # Metrics:
  # - Median: Central tendency
  # - IQR: Variability/uncertainty (larger = more variable)
  # - Mean and Max: Additional distribution characteristics

  stats_summary <- plot_data %>%
    group_by(Category, Source) %>%
    summarise(
      Median_Imp = median(Importance),
      IQR_Imp = IQR(Importance), # Interquartile range
      Mean_Imp = mean(Importance),
      Max_Imp = max(Importance),
      .groups = "drop"
    ) %>%
    arrange(Category, Source)

  print(as.data.frame(stats_summary))
}


# Create dodged boxplot comparing ensemble vs individual model importance
# Note: Split violin plots would require additional helper functions
if ("Source" %in% names(plot_data)) {
  p3 <- ggplot(plot_data, aes(x = Category, y = Importance, fill = Source)) +
    geom_boxplot(
      position = position_dodge(width = 0.8),
      width = 0.6,
      outlier.size = 0.2
    ) +
    labs(
      title = "Variable Importance: Ensemble vs. Individual Models",
      x = "Variable Category",
      y = "Importance Score"
    ) +
    scale_fill_manual(values = c("ensemble" = "#D4AD93", "individual" = "#957EAC")) +
    theme_minimal(base_size = 24) +
    theme(
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 2),

      # Remove all grid lines for cleaner appearance
      panel.grid = element_blank(),
      legend.position = "bottom",
      text = element_text(color = "black"),
      axis.text = element_text(color = "black", size = 22),
      plot.title = element_text(face = "bold", size = 14)
    )

  # p3  # Uncomment to display
  # print(p3)

  # TODO: Update paths to use config$results_figures
  # ggsave(file.path(config$results_figures, "plot3_Ensemble_vs_Individual.png"),
  #        plot = p3, width = 10, height = 8, bg = "transparent", dpi = 300)
  # ggsave(file.path(config$results_figures, "plot3_Ensemble_vs_Individual.pdf"),
  #        plot = p3, width = 10, height = 8, bg = "transparent", dpi = 300)
}
# ==============================================================================
# FIGURE 6: Frequency of Top Drivers Across Species
# ==============================================================================
# Purpose: Identify which variables most frequently appear in the top 5
# most important predictors across all species

# For each species, identify top 5 most important variables
# Then count how many species have each variable in their top 5
if ("Species" %in% names(plot_data)) {
  top_drivers_data <- plot_data %>%
    group_by(Species) %>%
    arrange(desc(Importance)) %>%
    slice_head(n = 5) %>% # Take top 5 per species
    ungroup() %>%
    count(Label, Category, name = "Frequency") %>% # Count occurrences
    arrange(desc(Frequency)) %>%
    head(20) # Show top 20 to avoid overcrowding

  # Create horizontal bar chart showing frequency
  p_freq <- ggplot(
    top_drivers_data,
    aes(x = reorder(Label, Frequency), y = Frequency, fill = Category)
  ) +

    # Bar chart
    geom_bar(
      stat = "identity",
      width = 0.7,
      alpha = 0.8, # Slight transparency
      color = "black", # Black borders
      linewidth = 0.6
    ) +

    # Flip coordinates for better label readability
    coord_flip() +

    # Apply nature-inspired color palette
    scale_fill_manual(values = nature_palette) +

    # Expand Y axis to prevent labels from touching plot border
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    labs(
      title = "Frequency of Top 5 Drivers Across Species",
      subtitle = "Number of species where the variable ranks in the top 5 importance",
      x = NULL,
      y = "Count of Species"
    ) +
    theme_transparent_box() +

    # Additional theme adjustments for bar chart
    theme(
      panel.grid.major.y = element_blank(), # Remove horizontal grid lines
      legend.position = "bottom"
    )

  # p_freq  # Uncomment to display
  # windows()  # Uncomment to open new window

  # TODO: Update paths to use config$results_figures
  # ggsave(file.path(config$results_figures, "plot4_Frequency.png"),
  #        plot = p_freq, width = 10, height = 8, bg = "transparent", dpi = 300)
  # ggsave(file.path(config$results_figures, "plot4_Frequency.svg"),
  #        plot = p_freq, width = 10, height = 8, bg = "transparent", dpi = 300)
}

# ==============================================================================
# End of Script
# ==============================================================================
# All figures created. Review and uncomment ggsave() calls to save outputs.
# Remember to update file paths to use config$results_figures for portability.
