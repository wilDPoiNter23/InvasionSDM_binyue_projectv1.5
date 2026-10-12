# Climate context of coastal cities
# Input: city climate and reclamation table
# Output: Whittaker biome panel

source(file.path(Sys.getenv("INVASIONSDM_PROJECT_ROOT", "."), "config", "bootstrap.R"))
library(plotbiomes)
library(ggplot2)
library(dplyr)
library(scales)
library(scico)
library(ggnewscale)
library(grid)
city_df <- read.csv(publication_path("GAM_INPUT"), check.names=FALSE)
df <- city_df %>%
  mutate(
    temperature = wc2.1_2.5m_bio_1,
    precipitation = wc2.1_2.5m_bio_12 / 10,  # Convert to cm.
    reclamation_area = reclamation_area
  )

# Define approximate biome backgrounds.

# Draw the plot.
# Prepare the required variables.
df$MAT <- df$`wc2.1_2.5m_bio_1`  # Mean Annual Temperature
df$MAP <- df$`wc2.1_2.5m_bio_12` / 10  # Annual Precipitation (mm -> cm)
df$reclamation_area <- df$reclamation_area
max(df$reclamation_area)
# Set a color gradient by reclamation area.
fill_col <- scales::col_numeric(
  palette = c("#b460b4", "#fd8d3c", "#bd0026"),
  domain = df$reclamation_area
)
# Set log10 axis breaks.
rng   <- range(df$reclamation_area, na.rm = TRUE)
rng
brks  <- log_breaks(base = 10, n = 6)(rng)  # Use approximately six powers-of-ten breaks.
labsF <- label_number(accuracy = 1, scale_cut = cut_si("k"))

p <- whittaker_base_plot() + guides(fill = "none") +                # Hide the background biome fill legend.
  ggnewscale::new_scale_fill() +         # Start a new fill scale for points.
  geom_point(
    data = df,
    aes(x = MAT, y = MAP, fill = reclamation_area),
    shape = 21, size = 5.8, colour = "black", stroke = 0.30, alpha = 0.9
  ) +
  # Choose a continuous fill scale.
  # Option A: use scico palettes such as batlow or hawaii.
  scale_fill_scico(
    palette = "vik",    # Alternative palettes include hawaii and roma.
    trans   = "log10",
    breaks  = brks,
    labels  = label_number(accuracy = 1, big.mark = ""),  # Format as 1000 without a thousands separator or decimal places.
    name    = "Reclamation area (km²)"
  )+
  labs(x = "Temperature (°C)", y = "Precipitation (cm)") +
  # Set the theme.
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(fill = NA),
    axis.title = element_text(size = 16, face = "bold", colour = "black"),
    axis.text = element_text(size = 14, colour = "black"),
    legend.position.inside = c(0.02, 0.85),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = alpha("white", 0.7)),
    legend.key.height = unit(0.6, "cm"),
    legend.key.width = unit(0.4, "cm")
  ) +
  labs(
    x = "Mean Annual Temperature (°C)",
    y = "Annual Precipitation (cm)",
    title = "Distribution of Coastal Reclamation Cities in Climate Biomes"
  )
p
# Export figures.
ggsave(publication_path("OUTPUT_ROOT", "figures/biome_context.png"), p, width = 8, height = 6, dpi = 600)
