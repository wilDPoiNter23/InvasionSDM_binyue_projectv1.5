# Regional and city-level effect panels
# Input: archived coefficient-based city table
# Output: regional-effect and city-attribution panels

source(file.path(Sys.getenv("INVASIONSDM_PROJECT_ROOT", "."), "config", "bootstrap.R"))
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggtext)
library(ggrepel)
library(patchwork)
library(stringr)
df <- read.csv(publication_path("CITY_EFFECT_TABLE"), check.names=FALSE)
df$city <- df$City
df$reclamation_area <- df$Reclamation_area
continent_data <- df %>%
  # mutate(
  # Define continent assignments.
  #   Regions = case_when(
  #     str_detect(Regions, "Asia") ~ "Asia",
  #     str_detect(Regions, "Europe") ~ "Europe",
  #     str_detect(Regions, "Africa") ~ "Africa",
  #     str_detect(Regions, "America") ~ "America",
  #     str_detect(Regions, "Oceania") ~ "Oceania",
  # TRUE ~ "Other" # Catch unmatched regions.
  #   ),

  # Calculate city effects from unstandardized coefficients.
  # Normalize reclamation intensity to 0-1.
  #   Recl_Norm = (reclamation_area - min(reclamation_area, na.rm=TRUE)) /
  #               (max(reclamation_area, na.rm=TRUE) - min(reclamation_area, na.rm=TRUE)),

  # Calculate the three components.
  # Eff_Direct = Recl_Norm * (-0.562),     # Physical inhibition (Cost).
  # Eff_Socio  = Recl_Norm * (0.971),      # Socioeconomic contribution (Benefit 1).
  # Eff_Soil   = Recl_Norm * (0.193),      # Soil contribution (Benefit 2).
  # Eff_Net    = Eff_Direct + Eff_Socio + Eff_Soil # Net effect.
  # ) %>%

  # Calculate mean effects by continent.
  group_by(Regions) %>%
  summarise(
    Mean_Direct = mean(Direct_Effect, na.rm=TRUE),
    Mean_Socio  = mean(Ind_Socio, na.rm=TRUE),
    Mean_Soil   = mean(Ind_Soil, na.rm=TRUE),
    Mean_Net    = mean(Total_Effect, na.rm=TRUE),
    n_cities    = n() # Count cities in each continent.
  ) %>%
  # Exclude Other and missing values.
  filter(Regions != "Other") %>%
  # Reshape to long format for stacked bars.
  pivot_longer(cols = c(Mean_Direct, Mean_Socio, Mean_Soil),
               names_to = "Component", values_to = "Value") %>%
  mutate(
    # Stack Soil above Socio, with Direct separately on the left.
    Component = factor(Component, levels = c("Mean_Soil", "Mean_Socio", "Mean_Direct"))
  )

# Extract Net values for points.
net_points_cont <- continent_data %>%
  select(Regions, Mean_Net) %>%
  distinct()

continent_data <- df %>%
  # Calculate median effects by continent.
  group_by(Regions) %>%
  summarise(
    Mean_Direct = median(Direct_Effect, na.rm=TRUE),
    Mean_Socio  = median(Ind_Socio, na.rm=TRUE),
    Mean_Soil   = median(Ind_Soil, na.rm=TRUE),
    Mean_Net    = median(Total_Effect, na.rm=TRUE),
    n_cities    = n() # Count cities in each continent.
  ) %>%
  # Exclude Other and missing values.
  filter(Regions != "Other") %>%
  # Reshape to long format for stacked bars.
  pivot_longer(cols = c(Mean_Direct, Mean_Socio, Mean_Soil),
               names_to = "Component", values_to = "Value") %>%
  mutate(
    # Stack Soil above Socio, with Direct separately on the left.
    Component = factor(Component, levels = c("Mean_Soil", "Mean_Socio", "Mean_Direct"))
  )

# Extract Net values for points.
net_points_cont <- continent_data %>%
  select(Regions, Mean_Net) %>%
  distinct()
# ==============================================================================
# Plot asymmetric stacked effects by continent.
# ==============================================================================

# Define colors.
cols <- c("Mean_Direct" = "#E64B35", # Red: direct cost.
          "Mean_Socio" = "#3C5488",  # Dark blue: socioeconomic component.
          "Mean_Soil" = "#4DBBD5")   # Light blue: soil component.

p_continent <- ggplot() +

  # Draw the zero line.
  geom_vline(xintercept = 0, color = "black", linewidth = 0.6) +

  # Draw bars.
  geom_col(data = continent_data,
           aes(x = Value, y = reorder(Regions, Mean_Net), fill = Component),
           width = 0.5, alpha = 0.9) +

  # Draw net-effect points.
  geom_point(data = net_points_cont,
             aes(x = Mean_Net, y = Regions),
             shape = 18, size = 6, color = "black") + # Use large black diamonds.

  # Set colors.
  scale_fill_manual(
    values = cols,
    labels = c("Soil Facilitation (+)", "Socioeconomic Facilitation (+)", "Physical Constraint (-)")
  ) +

  # Add annotations.
  annotate("text", x = min(continent_data$Value)*0.9, y = 0.6,
           label = "Ecological Cost", fontface = "bold", color = "#E64B35", size = 4) +
  annotate("text", x = max(continent_data$Value)*0.9, y = 0.6,
           label = "Cumulative Benefit", fontface = "bold", color = "#3C5488", size = 4) +

  # Set axes and title.
  labs(
    title = "<b>Regionsal Breakdown of Reclamation Mechanisms</b>",
    subtitle = "Interplay of <span style='color:#E64B35;'>Direct Constraints</span> vs. <span style='color:#3C5488;'>Socioeconomic</span> & <span style='color:#4DBBD5;'>Soil</span> Facilitation.<br>♦ = Net Risk Outcome.",
    x = "Mean Standardized Effect Magnitude",
    y = NULL,
    fill = NULL
  ) +

  # Set the theme.
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_markdown(face = "bold", size = 16),
    plot.subtitle = element_markdown(size = 12, color = "grey30"),

    # Format axis text.
    axis.text.y = element_text(size = 12, face = "bold", color = "black"),
    axis.text.x = element_text(size = 10),

    # Remove unnecessary grid lines.
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),

    # Add a black frame.
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.2),

    # Set the legend.
    legend.position = "top",
    legend.justification = "right",
    legend.box = "horizontal",
    legend.margin = margin(b = -10)
  )

# Print and save the figure.
print(p_continent)

continent_data


plot_data_attrib <- df %>%
  mutate(
    # Recalculate the net effect using unstandardized coefficients.
    Recl_Norm = (reclamation_area - min(reclamation_area, na.rm=TRUE)) /
                (max(reclamation_area, na.rm=TRUE) - min(reclamation_area, na.rm=TRUE)),
    Effect_Net_Real = Recl_Norm * 0.602, # Use the net coefficient.

    # Calculate the dependency ratio.
    # Cap dependency at 100% when E_net > Risk because the model is nonlinear.
    # A value of 100% assigns all current risk to reclamation within this calculation.
    Dependency_Pct = (Effect_Net_Real / risk) * 100,
    Dependency_Pct = ifelse(Dependency_Pct > 100, 100, Dependency_Pct)
  ) %>%
  # Define categories for colors or shapes.
  mutate(
    City_Type = case_when(
      risk > median(risk) & Dependency_Pct > 50 ~ "Man-made Hotspot",  # High risk with a large reclamation contribution.
      risk > median(risk) & Dependency_Pct <= 50 ~ "Natural Hotspot",   # High risk with a large background contribution.
      TRUE ~ "Low Risk / Latent"
    )
  )

# Select labels.
label_data <- plot_data_attrib %>%
  filter(risk > quantile(risk, 0.85) | Dependency_Pct > 80)
label_data
# Draw the plot.
p_attrib_scatter <- ggplot(plot_data_attrib, aes(x = risk, y = Dependency_Pct)) +

  # Optionally divide background regions with dashed lines.
  geom_hline(yintercept = 50, linetype = "dashed", color = "grey80") +
  geom_vline(xintercept = median(plot_data_attrib$risk), linetype = "dashed", color = "grey80") +

  # Draw the scatter plot.
  geom_point(aes(fill = City_Type, size = reclamation_area),
             shape = 21, color = "white", stroke = 0.5, alpha = 0.85) +

  # Add labels.
  geom_text_repel(data = label_data, aes(label = city),
                  size = 8, box.padding = 0.5, max.overlaps = 20) +

  # Annotate regions.
  # annotate("text", x = max(df$risk), y = 100, label = "Man-made\nHotspots",
  #          color = "#E64B35", fontface = "bold", hjust = 1, vjust = 1, size = 9) +
  # annotate("text", x = max(df$risk), y = 0, label = "Natural/Background\nHotspots",
  #          color = "#3C5488", fontface = "bold", hjust = 1, vjust = 0, size = 9) +

  # Set colors and scales.
  scale_fill_manual(values = c(
    "Man-made Hotspot" = "#E64B35", # Use red for the high-reclamation group.
    "Natural Hotspot" = "#3C5488",  # Use blue for the background group.
    "Low Risk / Latent" = "grey70"  # Use grey.
  ), name = "City Typology") +

  scale_size_continuous(range = c(2, 10), guide = "none") +

  # Set title and coordinates.
  labs(
    title = "<b>Who is responsible for the risk?</b>",
    subtitle = "Differentiating <span style='color:#E64B35;'>Reclamation-Driven</span> vs. <span style='color:#3C5488;'>Background-Driven</span> invasion risks.",
    x = "Total Invasion Risk (Current Status)",
    y = "Reclamation Dependency (% of Risk Attributed to Reclamation)"
  ) +

  theme_minimal(base_size = 24) +
  theme(
    plot.title = element_markdown(face = "bold"),
    plot.subtitle = element_markdown(color = "grey30"),
       panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.grid = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top"
  )

print(p_attrib_scatter)
ggsave( publication_path("OUTPUT_ROOT", "figures/gam_draft/plot5.2_reclamationDependency.png"), plot = p_attrib_scatter, width = 10, height = 12, bg = "transparent", dpi = 300)

plot_data_main <- continent_data %>%
  # Check the Component factor order.
  mutate(Component = factor(Component, levels = c("Mean_Soil", "Mean_Socio", "Mean_Direct")))

# Define colors.
cols <- c("Mean_Direct" = "#4C6E72", "Mean_Socio" = "#D2807D", "Mean_Soil" = "#EADDC9")

# Draw the main plot for all continents.
p_main <- ggplot() +
  geom_vline(xintercept = 0, color = "black", linewidth = 1) +

  # Draw bars.
  geom_col(data = plot_data_main,
           aes(x = Value, y = reorder(Regions, Mean_Net), fill = Component),
           width = 0.8, alpha = 0.9) +

  # Draw net-effect points.
  geom_point(data = net_points_cont,
             aes(x = Mean_Net, y = Regions),
             shape = 18, size = 12, color = "black") +

  scale_fill_manual(values = cols) +

  labs(
    title = "<b>b | Regional Asymmetry in Reclamation Impacts</b>",
    subtitle = "East Asia dominates global risk, overshadowing other regions.",
    x = "Mean Standardized Effect Magnitude", y = NULL
  ) +

  theme_minimal(base_size = 25) +
  theme(
    plot.title = element_markdown(face = "bold"),
    legend.position = "top",
       panel.grid = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.5)
  )

# Draw an inset excluding East Asia/Asia.
# Identify the continent with the largest value, assumed here to be Asia.
max_continent <- "Asia" # Alternatively determine it from the data.

p_inset <- ggplot() +
  geom_vline(xintercept = 0, color = "black", linewidth = 1) +
  # Exclude the largest continent.
  geom_col(data = plot_data_main %>% filter(Regions != max_continent),
           aes(x = Value, y = reorder(Regions, Mean_Net), fill = Component),
           width = 0.8, alpha = 0.9) +

  geom_point(data = net_points_cont %>% filter(Regions != max_continent),
             aes(x = Mean_Net, y = Regions),
             shape = 18, size = 5, color = "black") +

  scale_fill_manual(values = cols) +

  labs(title = "Other Regions", x = NULL, y = NULL,size = 15) +

  theme_minimal(base_size = 25) +
  theme(
    legend.position = "none", # Hide the inset legend.
     panel.grid = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "grey50", fill = NA, linewidth = 1.5), # Add a grey inset frame.
    plot.background = element_rect(fill = "white", color = NA) # Use a white inset background to cover the main grid.
  )

# Place the inset in an empty part of the main plot.
# inset_element(plot, left, bottom, right, top) uses coordinates from 0-1.
# A long East Asia bar usually leaves room at the lower or upper right.
final_plot <- p_main + inset_element(p_inset, 0.4, 0.05, 0.95, 0.55)
print(p_main)
ggsave( publication_path("OUTPUT_ROOT", "figures/gam_draft/plot5.1_Mediannetscore_regional.png"), plot = p_main, width = 14, height = 13, bg = "transparent", dpi = 900)
