# Predictor response curves
# Input: Response-curve CSV files
# Output: Predictor response plots

source(file.path(Sys.getenv("INVASIONSDM_PROJECT_ROOT", "."), "config", "bootstrap.R"))
library(ggplot2)
library(dplyr)
library(stringr)
library(tools)
# Check the response data; reuse big_response_df if extraction is complete.
while(!is.null(dev.list())) dev.off()
csv_folder <- publication_path("LOCAL_AUX_ROOT", "Response_Curves_Plots/Response_Data_CSVs/")
file_list <- list.files(path= csv_folder, pattern = "\\.csv$", full.names = TRUE)
file_list
big_response_df <- read.csv(publication_path("LOCAL_AUX_ROOT", "Response_Curves_Plots/Response_Data_CSVs/Response_Data_Pop2020_2.5min.csv"))
scientifi_10 <- function(x) {
  func <- function(y) {
    # Return NA when the input is NA.
    if (is.na(y)) return(NA)
    # Check for 0 next.
    if (y == 0) return("0")
    # Apply the usual conversion.
    parse(text=gsub("e\\+?", " %*% 10^", scales::scientific_format()(y)))
  }
  return(lapply(x, func))
}
bg_color <- "#113A42"
# Set the trend color, for example orange #E64B35 or red #D55E00.
trend_color <- "#A13C46"

# Define variable information.
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

# Define the label lookup function.
# Return a display label for an input filename.
get_pretty_name <- function(raw_name) {
  # Find the corresponding label in var_info.
  match_idx <- match(raw_name, var_info$raw)

  if (!is.na(match_idx)) {
    return(var_info$label[match_idx]) # Return the label when a match exists.
  } else {
    return(raw_name) # Otherwise retain the original name.
  }
}
for (f_path in file_list) {
  tryCatch({
    # Prepare identifiers.
    raw_filename <- file_path_sans_ext(basename(f_path))
      var_id <- str_remove(raw_filename, "^Response_Data_")

    # Look up display labels.
    x_label_text <- get_pretty_name(var_id)

    cat("Processing:", var_id, "Display name:", x_label_text, "\n")

    # Read data.
    plot_data <- read.csv(f_path)

   # Check the number of columns to reject empty files.
    if(ncol(plot_data) < 4) {
      stop("Expected at least four data columns.")
    }

    # Extract columns 4, 2 and 3.
    plot_data <- plot_data[, c(4, 2, 3)]

    # Rename columns for plotting.
    colnames(plot_data) <- c("Species", "Env_Value", "Probability")

    # Clean the data.
    # Convert Species to a factor.
    plot_data$Species <- as.factor(plot_data$Species)

    # Convert numeric columns to numeric values.
    plot_data$Env_Value <- as.numeric(plot_data$Env_Value)
    plot_data$Probability <- as.numeric(plot_data$Probability)

    # Divide by 1000 when predictions use the 0-1000 scale.
    if(max(plot_data$Probability, na.rm=TRUE) > 1.5) {
      plot_data$Probability <- plot_data$Probability / 1000
    }
    # Draw the plot.
 p_classic <- ggplot(plot_data , aes(x = Env_Value, y = Probability)) +

  # Layer 1: background response curves.
  # Use grey30 with low opacity (0.01).
  # Overlapping lines become darker and show the density of curves.
  geom_line(aes(group = Species), color = "#255DAA", alpha = 0.1, linewidth = 0.2) +

  # Layer 2: trend line.
  # Use a prominent color, for example orange #E64B35 or green #00A087.
  geom_smooth(method = "gam",
              color = "#DE525E", # Use bright orange-red.
              fill = "#DE525E",  # Match the confidence interval fill to the line color.
              alpha = 0.2,       # Use a translucent confidence interval.
              linewidth = 1.9) + # Increase line width.

  # Layer 3: English labels.
  labs(
     x = x_label_text,
    y = "Probability of Occurrence"
  ) +

  # Layer 4: axis scales.
  # Fix the y-axis to 0-1 and remove x-axis padding.
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0), breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(expand = c(0, 0)) +

  # Layer 5: figure theme.
  theme_minimal(base_size = 30) +
  theme(
    # Use the generic sans font for consistent text rendering.
    text = element_text(family = "sans", color = "black"),

    # Use a transparent background.
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),

    # Add a black border.
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),

    # Keep only faint grid lines or remove them.
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),

    # Set title placement.
    plot.title = element_text(face = "bold", margin = ggplot2::margin(b = 10)),
    axis.title.x = element_text(margin = ggplot2::margin(t = 10)),
    axis.title.y = element_text(margin = ggplot2::margin(r = 10))
  )

 out_name <- paste0(publication_path("LOCAL_AUX_ROOT", "Response_Curves_Plots/Response_Curves_Plots/newResponse_"), var_id, ".png")
      # Open the graphics device with a transparent background.
    png(filename = out_name,
        width = 2400, height = 1800, res = 300, # Use an 8x6-inch canvas.
        bg = "transparent")

    # Print the plot to the device.
    print(p_classic)

    # Close the device to save the file.
    dev.off()

    cat("Finished.\n")}, error = function(e) {
    cat("Skipped after error:", conditionMessage(e), "\n")
    try(dev.off(), silent = TRUE)
  })
}

cat("Finished all predictors.\n")

p_classic <- ggplot(big_response_df , aes(x = Env_Value, y = Probability)) +

  # Layer 1: background response curves.
  # Use grey30 with low opacity (0.01).
  # Overlapping lines become darker and show the density of curves.
  geom_line(aes(group = Species), color = "#255DAA", alpha = 0.1, linewidth = 0.2) +

  # Layer 2: trend line.
  # Use a prominent color, for example orange #E64B35 or green #00A087.
  geom_smooth(method = "gam",
              color = "#DE525E", # Use bright orange-red.
              fill = "#DE525E",  # Match the confidence interval fill to the line color.
              alpha = 0.2,       # Use a translucent confidence interval.
              linewidth = 1.5) + # Increase line width.

  # Layer 3: English labels.
  labs(
     x = 'test',
    y = "Probability of Occurrence"
  ) +

  # Layer 4: axis scales.
  # Fix the y-axis to 0-1 and remove x-axis padding.
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0), breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(expand = c(0, 0)) +

  # Layer 5: figure theme.
  theme_minimal(base_size = 34) +
  theme(
    # Use the generic sans font for consistent text rendering.
    text = element_text(family = "sans", color = "black"),

    # Use a transparent background.
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),

    # Add a black border.
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),

    # Keep only faint grid lines or remove them.
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),

    # Set title placement.
    plot.title = element_text(face = "bold", margin = ggplot2::margin(b = 10)),
    axis.title.x = element_text(margin = ggplot2::margin(t = 10)),
    axis.title.y = element_text(margin = ggplot2::margin(r = 10))
  )
p_classic
