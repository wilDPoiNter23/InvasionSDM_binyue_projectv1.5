# ARCHIVE: historical reference; symbolic paths are intentionally not runnable.
# Original SHA256: 00420c129e160dda96ee68a5382f5d961c8ad0c6d5512c88bb5291d0fb620b1d
library(ggplot2)
library(dplyr)
library(stringr)
library(tools)
# 1. Ensure data is correct (if extraction code was run before, use big_response_df directly)
getwd()
while(!is.null(dev.list())) dev.off()
csv_folder <- "${PRIVATE_EXTERNAL_PATH}" 
file_list <- list.files(path= csv_folder, pattern = "\\.csv$", full.names = TRUE)
file_list
big_response_df <- read.csv("${PRIVATE_EXTERNAL_PATH}")
scientifi_10 <- function(x) {
  func <- function(y) {
    # 【Fix point】 First check if it's NA, if so return NA directly
    if (is.na(y)) return(NA)
    # Then check if it's 0
    if (y == 0) return("0")
    # Normal conversion
    parse(text=gsub("e\\+?", " %*% 10^", scales::scientific_format()(y)))
  }
  return(lapply(x, func))
}
bg_color <- "#113A42" 
# Trend line color (recommended: Nature orange #E64B35 or bright red #D55E00)
trend_color <- "#A13C46" 

# --- 1. Define variable information table (your latest version) ---
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

# --- 2. Define lookup helper (Helper Function) ---
# Give it a raw filename, it returns a pretty label
get_pretty_name <- function(raw_name) {
  # Look for corresponding label in var_info
  match_idx <- match(raw_name, var_info$raw)
  
  if (!is.na(match_idx)) {
    return(var_info$label[match_idx]) # Found, return pretty name
  } else {
    return(raw_name) # Not found, return original name
  }
}
for (f_path in file_list) {
  tryCatch({
    # 1. Prepare ID
    raw_filename <- file_path_sans_ext(basename(f_path))
      var_id <- str_remove(raw_filename, "^Response_Data_")
    
    # 2. 【Key】Use lookup table to get pretty name
    x_label_text <- get_pretty_name(var_id)
    
    cat("Processing:", var_id, "--> Display as:", x_label_text, "\n")
    
    # 3. Read data
    plot_data <- read.csv(f_path)
    
   # Check if column count is sufficient (prevent empty files)
    if(ncol(plot_data) < 4) {
      stop("Data has fewer than 4 columns, cannot parse!")
    }
    
    # Extract columns 4, 2, 3
    plot_data <- plot_data[, c(4, 2, 3)]
    
    # Force rename (ensure ggplot can recognize)
    colnames(plot_data) <- c("Species", "Env_Value", "Probability")
    
    # --- Data cleaning ---
    # Ensure Species is a factor
    plot_data$Species <- as.factor(plot_data$Species)
    
    # Ensure numeric columns are numbers
    plot_data$Env_Value <- as.numeric(plot_data$Env_Value)
    plot_data$Probability <- as.numeric(plot_data$Probability)
    
    # Normalize: if data is 0-1000, divide by 1000
    if(max(plot_data$Probability, na.rm=TRUE) > 1.5) {
      plot_data$Probability <- plot_data$Probability / 1000
    }
    # 4. Plot
 p_classic <- ggplot(plot_data , aes(x = Env_Value, y = Probability)) +
  
  # --- Layer 1: Background lines (The Cloud) ---
  # Key change: Use deep gray (grey30), very low transparency (0.01)
  # This way overlapping areas naturally darken, showing data "density"
  geom_line(aes(group = Species), color = "#255DAA", alpha = 0.1, linewidth = 0.2) +
  
  # --- Layer 2: Trend line (The Signal) ---
  # Key change: Use eye-catching color (e.g., Nature orange #E64B35 or teal #00A087)
  geom_smooth(method = "gam", 
              color = "#DE525E", # Bright orange-red
              fill = "#DE525E",  # Confidence interval fill same color
              alpha = 0.2,       # Confidence interval semi-transparent
              linewidth = 1.9) + # Thicker line
  
  # --- Layer 3: Beautify labels (all English) ---
  labs(
     x = x_label_text, 
    y = "Probability of Occurrence"
  ) +
  
  # --- Layer 4: Scale control ---
  # Force Y-axis 0-1, X-axis tight
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0), breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(expand = c(0, 0)) +
  
  # --- Layer 5: Nature style theme ---
  theme_minimal(base_size = 30) +
  theme(
    # Font settings (key to solving garbled text, use universal sans font)
    text = element_text(family = "sans", color = "black"),
    
    # Background fully transparent
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    
    # Border: add black frame
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    
    # Grid lines: keep only very faint grid, or remove completely
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    # Title position
    plot.title = element_text(face = "bold", margin = ggplot2::margin(b = 10)),
    axis.title.x = element_text(margin = ggplot2::margin(t = 10)),
    axis.title.y = element_text(margin = ggplot2::margin(r = 10))
  )

 out_name <- paste0("${PRIVATE_EXTERNAL_PATH}", var_id, ".png")
      # Open graphics device (specify transparent background)
    png(filename = out_name, 
        width = 2400, height = 1800, res = 300, # Corresponds to 8x6 inch
        bg = "transparent") 
    
    # Print plot to device
    print(p_classic)
    
    # Close device (this step actually saves the file)
    dev.off()
    
    cat("Success!\n")}, error = function(e) {
    cat("  [Skipped] Error:", conditionMessage(e), "\n")
    try(dev.off(), silent = TRUE)
  })
}

cat("All complete!\n")

p_classic <- ggplot(big_response_df , aes(x = Env_Value, y = Probability)) +
  
  # --- Layer 1: Background lines (The Cloud) ---
  # Key change: Use deep gray (grey30), very low transparency (0.01)
  # This way overlapping areas naturally darken, showing data "density"
  geom_line(aes(group = Species), color = "#255DAA", alpha = 0.1, linewidth = 0.2) +
  
  # --- Layer 2: Trend line (The Signal) ---
  # Key change: Use eye-catching color (e.g., Nature orange #E64B35 or teal #00A087)
  geom_smooth(method = "gam", 
              color = "#DE525E", # Bright orange-red
              fill = "#DE525E",  # Confidence interval fill same color
              alpha = 0.2,       # Confidence interval semi-transparent
              linewidth = 1.5) + # Thicker line
  
  # --- Layer 3: Beautify labels (all English) ---
  labs(
     x = 'test', 
    y = "Probability of Occurrence"
  ) +
  
  # --- Layer 4: Scale control ---
  # Force Y-axis 0-1, X-axis tight
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0), breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(expand = c(0, 0)) +
  
  # --- Layer 5: Nature style theme ---
  theme_minimal(base_size = 34) +
  theme(
    # Font settings (key to solving garbled text, use universal sans font)
    text = element_text(family = "sans", color = "black"),
    
    # Background fully transparent
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    
    # Border: add black frame
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    
    # Grid lines: keep only very faint grid, or remove completely
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    # Title position
    plot.title = element_text(face = "bold", margin = ggplot2::margin(b = 10)),
    axis.title.x = element_text(margin = ggplot2::margin(t = 10)),
    axis.title.y = element_text(margin = ggplot2::margin(r = 10))
  )
p_classic
windows()
