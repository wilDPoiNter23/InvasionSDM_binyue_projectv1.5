# Predictor importance and selection frequency
# Input: Variable-importance summary and predictor rasters
# Output: Importance and frequency plots

source(file.path(Sys.getenv("INVASIONSDM_PROJECT_ROOT", "."), "config", "bootstrap.R"))
library(dplyr)
library(stringr)
library(viridis)
library(terra)
library(ggplot2)
library(tidyterra)
library(forcats)
data <- read.csv(publication_path("PREDICTOR_IMPORTANCE"))
# Temperature variables: blue to white to red, without green.
pal_temp <- colorRampPalette(c("#2166AC", "#F7F7F7", "#B2182B"))

# Intensity or density variables, such as built-up and GDP: pale yellow to orange to dark brown.
pal_intensity <- colorRampPalette(c("#fff7bc00", "#FEC44F", "#D95F0E", "#8C2D04"))

# Soil / SOC: pale grey-green to dark brown-green.
pal_soil <- colorRampPalette(c("#F7FCF5", "#C7E9C0", "#74C476", "#238B45", "#00441B"))
pal_pop <- colorRampPalette(c(
  "#F7FBFF",  # very light blue
  "#51a3e6",  # light blue
  "#2d64da",  # medium blue
  "#b81c7c",  # deep blue
  "#be0a0a"   # deep purple
))
# Use viridis as an alternative palette.
pal_viridis <- function(n) {
  viridis::viridis(n, option="E")
}
# 1. Create a variable-information table.
nature_palette <- c(
  "Climate"        = "#61998B", # Warm orange: climate/temperature.
  "Soil"           = "#d58d3a", # Earth brown: soil.
  "Topography"     = "#EDE6C1", # Slate teal: mountains/topography.
  "Marine"         = "#A4D1DB", # Ocean blue: water bodies.
  "Anthropogenic" = "#F06256" # Brick red: human/urban predictors.

)
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
  "GDP2020_2.5min",         "GDP density",          "Anthropogenic",
  "Pop2020_2.5min",         "Population density","Anthropogenic",
  "VIIRS_2023_2.5min",      "Night-time lights",       "Anthropogenic",
  "reclamation_raster",     "Reclamation area",                      "Anthropogenic",
  # Topography
  "wc2.1_2.5m_elev", "Elevation (m)", "Topography",
  "slope",           "Slope (°)",     "Topography",
  "aspect",          "Aspect (°)",    "Topography",
  # Marine
  "salinity_mean",   "Mean sea surface salinity",     "Marine",
  "salinity_range",  "Sea surface salinity range",    "Marine",
  "temperature_mean","Mean sea surface temperature (°C)", "Marine",
  "temperature_range","Sea surface temperature range (°C)","Marine"
)

# 2. Define helper functions.
pretty_label <- function(x) {
  out <- var_info$label[match(x, var_info$raw)]
  out[is.na(out)] <- x[is.na(out)] # Retain the original name if no matching label is found.
  out
}

get_category <- function(x) {
  out <- var_info$category[match(x, var_info$raw)]
  out[is.na(out)] <- "Other" # Assign unmatched categories to Other.
  out
}

# 3. Define a custom theme with transparent backgrounds and borders.
theme_transparent_box <- function() {
  theme_minimal(base_size = 14) +
    theme(
      # 1. Transparent backgrounds.
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA),
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.box.background = element_rect(fill = "transparent", color = NA),

      # 2. Panel borders on all sides.
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1.2),

      # 3. Optionally remove grid lines.
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),

      # 4. Set readable text colours across backgrounds.
      text = element_text(color = "black"),
      axis.text = element_text(color = "black"),

      # Legend.
      legend.position = "bottom",
      legend.title = element_text(face = "bold")
    )
}
# Clean and rename data.
plot_data <- data %>%
  mutate(
    # Apply the transformation function.
    Label = pretty_label(Variable),
    Category = get_category(Variable)
  ) %>%
  # Remove rows with NA Importance values.
  filter(!is.na(Importance))

# Check the transformed results.


#---------for importance value plot----------------
var_order <- plot_data %>%
  group_by(Label) %>%
  summarise(median_imp = median(Importance)) %>%
  arrange(desc(median_imp)) %>%
  pull(Label)

# Plot.
p1 <- plot_data %>%
  filter(Label %in% var_order) %>%
  ggplot(aes(x = factor(Label, levels = rev(var_order)), y = Importance)) +

  # Boxplots: map Category to fill.
  geom_boxplot(aes(fill = Category),
               outlier.size = 0.8,
               outlier.alpha = 0.6,
               alpha = 0.8,          # Use transparency to soften colours.
               width = 0.7,
               color = "black") +    # Black boxplot borders.

  coord_flip() +

  # Apply custom colours with scale_fill_manual.
  scale_fill_manual(values = nature_palette) +

  labs(
    title = "Top 25 Environmental Drivers",
    subtitle = "Ranked by median importance across all species",
    x = NULL,
    y = "Importance Score"
  ) +
theme_transparent_box()
p1
ggsave(publication_path("OUTPUT_ROOT", "figures/sdm_evalueation/plot1_importance.png"), plot = p1, width = 10, height = 8, bg = "transparent", dpi = 300)
ggsave(publication_path("OUTPUT_ROOT", "figures/sdm_evalueation/plot1_importance.svg"), plot = p1, width = 10, height = 8, bg = "transparent", dpi = 300)
# -------------------------------------------------------------------------
# Figure 2: violin plots comparing importance across predictor categories.
# Compare importance among climate, human, soil and other predictor groups.
# -------------------------------------------------------------------------

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
p2
print(p2)

# -------------------------------------------------------------------------
# Figure 3: compare Ensemble and Individual model sources.
# Examine whether ensemble models assign different importance to particular variables.
# -------------------------------------------------------------------------

# Calculate mean importance for each variable by model source.
source_comp <- plot_data %>%
  group_by(Category, Source) %>%
  summarise(Mean_Importance = mean(Importance), .groups = 'drop')
  # 2. Calculate statistical differences.
# Summarise each Source within each Category:
# 1. Median: central tendency.
# 2. Interquartile range (IQR): dispersion of importance values.
# 3. Extreme-value proportion: importance values > 0.5.

stats_summary <- plot_data %>%
  group_by(Category, Source) %>%
  summarise(
    Median_Imp = median(Importance),
    IQR_Imp = IQR(Importance), # Larger interquartile ranges indicate greater dispersion.
    Mean_Imp = mean(Importance),
    Max_Imp = max(Importance),
    .groups = 'drop'
  ) %>%
  arrange(Category, Source)
print(as.data.frame(stats_summary))


p3 <- ggplot(plot_data, aes(x = Category, y = Importance, fill = Source)) + # Use dodged boxplots if geom_split_violin is unavailable.
  # Use standard boxplots because split violins require an additional helper function.
  geom_boxplot(position = position_dodge(width = 0.8), width = 0.6, outlier.size = 0.2) +
  labs(
    title = "Variable Importance: Ensemble vs. Individual Models",
    x = "Variable Category",
    y = "Importance Score"
  ) +
  scale_fill_manual(values = c("ensemble" = "#D4AD93", "individual" = "#957EAC")) +
  theme_light()+theme_minimal(base_size = 24) +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.border = element_rect(color = "black", fill = NA, size = 2),

    # Remove all grid lines.
    panel.grid = element_blank(),
    # ------------------------------------

    legend.position = "bottom",

    text = element_text(color = "black"),
    axis.text = element_text(color = "black", size = 22),
    plot.title = element_text(face = "bold", size = 14)
  )
p3
print(p3)
ggsave(publication_path("OUTPUT_ROOT", "figures/sdm_evalueation/plot3_Ensemble_vs_Individual.png"), plot = p3, width = 10, height = 8, bg = "transparent", dpi = 300,
  device = grDevices::png  )
ggsave(publication_path("OUTPUT_ROOT", "figures/sdm_evalueation/plot3_Ensemble_vs_Individual.pdf"), plot = p3, width = 10, height = 8, bg = "transparent", dpi = 300)
# -------------------------------------------------------------------------
# p4: frequency bar chart counting only the top 5 variables by importance for each species.
top_drivers_data <- plot_data %>%
  group_by(Species) %>%
  arrange(desc(Importance)) %>%
  slice_head(n = 5) %>% # Select the top 5.
  ungroup() %>%
  # Count occurrences of each variable.
  count(Label, Category, name = "Frequency") %>%
  # Display the 20 most frequent variables to reduce crowding.
  arrange(desc(Frequency))

# -------------------------------------------------------------------------
# Plot a publication-style frequency bar chart.
# -------------------------------------------------------------------------

p_freq <- ggplot(top_drivers_data, aes(x = reorder(Label, Frequency), y = Frequency, fill = Category)) +

  # Bar chart.
  geom_bar(stat = "identity",
           width = 0.7,
           alpha = 0.8,       # Slight transparency.
           color = "black",   # Black borders.
           size = 0.6) +      # Border width.

  # Flip coordinates for readable long labels.
  coord_flip() +

  # Apply the publication colour palette.
  scale_fill_manual(values = nature_palette) +

  # Expand the y-axis to prevent labels touching the border.
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +

  labs(
    title = "Frequency of Top 5 Drivers Across Species",
    subtitle = "Number of species where the variable ranks in the top 5 importance",
    x = NULL,
    y = "Count of Species"
  ) +
theme_transparent_box() +

  # Adjust bar-chart formatting.
  theme(
    panel.grid.major.y = element_blank(), # Remove horizontal grid lines, which become vertical after coordinate flipping.
    legend.position = "bottom"
  )
p_freq
ggsave(publication_path("OUTPUT_ROOT", "figures/sdm_evalueation/plot4_Frequency.png"), plot = p_freq, width = 10, height = 8, bg = "transparent", dpi = 300)
ggsave(publication_path("OUTPUT_ROOT", "figures/sdm_evalueation/plot4_Frequency.svg"), plot = p_freq, width = 10, height = 8, bg = "transparent", dpi = 300)
