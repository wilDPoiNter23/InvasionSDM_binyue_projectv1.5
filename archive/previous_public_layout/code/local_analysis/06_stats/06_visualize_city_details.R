# ARCHIVE: historical reference; symbolic paths are intentionally not runnable.
# Original SHA256: 6c82a42a09502298afeaf70af4f06adde8c752f88caf540275e67578274ba66a
# ====== Load Packages ======
library(plotbiomes)
library(ggplot2)
library(dplyr)
library(readr)
library(scales)
library(scico)
library(viridis)
library(tidyr)
library(ggtext) # For advanced text rendering
library(patchwork)
library(ggrepel)
library(grid)
library(stringr)
library(ggbreak)
install.packages("ggbreak")
devtools::install_github("valentinitnelav/plotbiomes")
# ====== Get Whittaker Biome Data ======

df <- read.csv("${LOCAL_ANALYSIS_ROOT}/data/organized_table/0502_risk_city_netscoreStdVersion.csv")

# 1) Construct City-level Analysis Table
df <- df %>% # Each row = city
  mutate(
    diff = mean_risk_reclamation - mean_risk_non_reclamation,
    recl_pct = reclamation_percentage / 100
  ) %>%
  select(city, diff, recl_pct, latitude, pop, gdp, climate_code) # Example

num_vars <- c("diff", "recl_pct", "latitude", "pop", "gdp", "climate_code")

# 2) Correlation matrix (right heatmap)
cor_mat <- cor(df[, num_vars], use = "pairwise.complete.obs")
cor_long <- as.data.frame(as.table(cor_mat)) %>%
  rename(var1 = Var1, var2 = Var2, r = Freq)

p_cor <- ggplot(cor_long, aes(var1, var2, fill = r)) +
  geom_tile() +
  scale_fill_gradient2(limits = c(-1, 1)) +
  coord_equal() +
  theme_minimal() +
  theme(axis.title = element_blank())

# 3) Standardized regression (left arrows)
std <- function(x) as.numeric(scale(x))
fit <- lm(std(diff) ~ std(recl_pct) + std(latitude) + std(pop) + std(gdp) + std(climate_code),
  data = df
)
coef_tab <- tidy(fit) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    beta = estimate,
    p = p.value,
    sig = ifelse(p < 0.05, "<0.05", ">=0.05"),
    sign = ifelse(beta >= 0, "pos", "neg")
  )

# Manually place node coordinates
nodes <- tibble::tibble(
  term = c("std(recl_pct)", "std(latitude)", "std(pop)", "std(gdp)", "std(climate_code)"),
  x = 0, y = c(5, 4, 3, 2, 1)
)
target <- tibble::tibble(label = "diff", x = 4, y = 3)

edges <- coef_tab %>%
  left_join(nodes, by = c("term")) %>%
  mutate(xend = target$x, yend = target$y)

p_paths <- ggplot() +
  geom_point(data = nodes, aes(x, y), size = 2) +
  geom_point(data = target, aes(x, y), size = 3) +
  geom_curve(
    data = edges,
    aes(
      x = x, y = y, xend = xend, yend = yend,
      size = abs(beta),
      colour = sign,
      linetype = sig
    ),
    curvature = 0.2, arrow = arrow(length = unit(2, "mm"))
  ) +
  scale_size(range = c(0.3, 1.6), guide = "none") +
  scale_colour_manual(values = c(pos = "#1f78b4", neg = "#e66101"), guide = "none") +
  scale_linetype_manual(values = c("<0.05" = "solid", ">=0.05" = "dashed"), guide = "none") +
  theme_minimal() +
  labs(x = NULL, y = NULL)

# Combine: Left paths + Right correlation heatmap (Use patchwork or cowplot)
library(patchwork)
p_a_like <- p_paths + p_cor + plot_layout(widths = c(1, 1.2))



# ====== Construct Main Variables ======
df <- city_df %>%
  mutate(
    temperature = wc2.1_2.5m_bio_1,
    precipitation = wc2.1_2.5m_bio_12 / 10, # Convert to cm
    reclamation_area = reclamation_area
  )

# ====== Biome Background Data (Approximate Definition) ======

# ====== Plotting ======
# ==== Prepare Variables ====
df$MAT <- df$`wc2.1_2.5m_bio_1` # Mean Annual Temperature
df$MAP <- df$`wc2.1_2.5m_bio_12` / 10 # Annual Precipitation (mm -> cm)
df$reclamation_area <- df$reclamation_area
max(df$reclamation_area)
# ==== Set Color Gradient (Based on Reclamation Area) ====
fill_col <- scales::col_numeric(
  palette = c("#b460b4", "#fd8d3c", "#bd0026"),
  domain = df$reclamation_area
)
# Log Scale: Automatically generate nice log10 breaks
rng <- range(df$reclamation_area, na.rm = TRUE)
rng
brks <- log_breaks(base = 10, n = 6)(rng) # About 6 integer order magnitude breaks
labsF <- label_number(accuracy = 1, scale_cut = cut_si("k"))

p <- whittaker_base_plot() + guides(fill = "none") + # Hide background biome fill legend
  ggnewscale::new_scale_fill() + # Enable new fill scale, exclusively for points
  geom_point(
    data = df,
    aes(x = MAT, y = MAP, fill = reclamation_area),
    shape = 21, size = 5.8, colour = "black", stroke = 0.30, alpha = 0.9
  ) +
  # ---- Key: Continuous Fill Scale (Choose One) ----
  # Option A: scico advanced palette (recommend "batlow" or "hawaii")
  scale_fill_scico(
    palette = "vik", # Can change to "hawaii" / "roma"
    trans   = "log10",
    breaks  = brks,
    labels  = label_number(accuracy = 1, big.mark = ""), # 1000 instead of 1,000; no decimals
    name    = "Reclamation area (km²)"
  ) +
  labs(x = "Temperature (°C)", y = "Precipitation (cm)") +
  # ==== Theme Settings ====
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
# ==== Export Image ====
ggsave("Fig1_Whittaker_Biome_Cities.png", p, width = 8, height = 6, dpi = 600)

# -------------------------------------------------------------------------
# Data Preprocessing: Core Logic is Creating "Facet" Variable
# -------------------------------------------------------------------------
plot_data_prep <- df %>%
  # A. Sort by Risk descending
  arrange(desc(risk)) %>%
  # B. Create rank index (1, 2, 3...)
  mutate(rank_idx = row_number()) %>%
  # C. Create facet groups (Assume 3 facets, each with total/3 cities)
  mutate(
    # n() is total count, ceiling rounds up
    # This creates groups like "1-30", "31-60", "61-90"
    facet_group = ceiling(rank_idx / (n() / 3))
  ) %>%
  # D. Convert city to factor, ensuring correct plot order
  mutate(city = reorder(city, risk))
plot_data_prep
# E. Convert to long format for bar chart (only direct and indirect)
plot_data_long <- plot_data_prep %>%
  pivot_longer(
    cols = c(Effect_Direct, Ind_Socio),
    names_to = "Effect_Type",
    values_to = "Effect_Value"
  )
plot_data_long
# -------------------------------------------------------------------------
# Aesthetic Definitions
# -------------------------------------------------------------------------
# Colors: Direct(Red), Indirect(Blue), Net(Black Point), Risk(Fill Color)
col_direct <- "#E64B35"
col_indirect <- "#4DBBD5"
col_net <- "black"

# -------------------------------------------------------------------------
# Plotting
# -------------------------------------------------------------------------
p <- ggplot() +

  # --- 1. Zero Line ---
  geom_vline(xintercept = 0, color = "grey60", linetype = "dotted", linewidth = 0.4) +

  # --- 2. Bar Chart (Direct & Indirect) ---
  # Use geom_segment
  geom_segment(
    data = plot_data_long,
    aes(y = city, yend = city, x = 0, xend = Effect_Value, color = Effect_Type),
    linewidth = 3, alpha = 0.7
  ) + # linewidth controls bar width

  # --- 3. Net Effect Point Plot (Net Effect) ---
  # Placed on top of bar chart
  geom_point(
    data = plot_data_prep,
    aes(y = city, x = Effect_Indirect_Total, fill = risk),
    shape = 21, # Solid circle with border
    color = "black", # Border color
    stroke = 0.4, # Border width
    size = 2.5
  ) + # Point size

  # --- 4. Faceting (Key) ---
  # scales="free_y" makes each facet only show its own cities
  facet_wrap(~facet_group, scales = "free_y", ncol = 3) +

  # --- 5. Color mapping ---
  # Bar color
  scale_color_manual(
    # [Modification point] Keys must exactly match column names before pivot_longer!
    values = c("Effect_Direct" = col_direct, "Ind_Socio" = col_indirect),

    # Labels can be customized, this is the text shown on legend
    labels = c("Direct Effect", "Indirect Effect"),
    name = NULL
  ) +
  # Risk color (Point fill)
  scale_fill_gradientn(
    colors = c("#fcfdbf", "#fc8961", "#b73779", "#51127c"), # Magma style
    name = "Total Risk"
  ) +

  # --- 6. Labels and Coordinates ---
  labs(
    title = "<b>Risk Decomposition: Direct, Indirect, and Net Effects</b>",
    subtitle = "Cities are ordered by total invasion risk (high to low). <span style='color:#E64B35;'>Red bars</span>: Direct effects; <span style='color:#4DBBD5;'>Blue bars</span>: Indirect effects; <b>Circles</b>: Net outcome.",
    x = "Standardized Effect Magnitude",
    y = NULL
  ) +

  # --- 7. Theme settings ---
  theme_minimal(base_size = 11) +
  theme(
    # Fonts
    text = element_text(color = "black", family = "sans"),

    # Title rich text
    plot.title = element_markdown(face = "bold", size = 14),
    plot.subtitle = element_markdown(size = 10, color = "grey30", margin = margin(b = 10)),

    # Facet title hidden (we don't need to show 1, 2, 3)
    strip.text = element_blank(),

    # Grid optimization
    panel.grid.major.y = element_line(color = "grey95", linewidth = 0.2),
    panel.grid.major.x = element_line(color = "grey90", linewidth = 0.2),
    panel.grid.minor = element_blank(),

    # Adjust spacing
    panel.spacing.x = unit(1, "cm"), # Distance between facets

    # Legend
    legend.position = "top",
    legend.box = "horizontal",
    legend.margin = margin(b = -5),

    # Axis text
    axis.text.y = element_text(size = 8, hjust = 1)
  )
windows()
p
# 2. Data cleaning and preprocessing
plot_data <- df %>%
  # Filter Top 20 cities with significant reclamation effects (otherwise 80+ are crowded)
  # Sort by Indirect_Total
  arrange(desc(Effect_Indirect_Total)) %>%
  slice_head(n = 20) %>%
  # Convert to long format for stacking
  pivot_longer(
    cols = c(Ind_Socio, Ind_Soil, Effect_Direct),
    names_to = "Component", values_to = "Value"
  ) %>%
  mutate(
    # Set factor levels to ensure plotting order
    Component = factor(Component, levels = c("Ind_Socio", "Ind_Soil", "Effect_Direct")),
    # Order cities by total effect
    city = reorder(city, abs(Value))
  )

# ==============================================================================
# Fig 1: Mechanism Decomposition Lollipop Plot (Top 20 Cities)
# ==============================================================================
# Define colors: Direct(Red), Socio(Dark Blue), Soil(Light Blue)
cols <- c("Effect_Direct" = "#E64B35", "Ind_Socio" = "#3C5488", "Ind_Soil" = "#4DBBD5")

p1 <- ggplot(plot_data, aes(y = city, x = Value, fill = Component)) +
  # Zero line
  geom_vline(xintercept = 0, color = "grey60", linetype = "dashed") +

  # Stacked bar plot (Direct is negative, Ind is positive, will separate automatically)
  geom_bar(stat = "identity", width = 0.6, alpha = 0.9) +

  # Mark Net Effect point (Net Effect)
  # Need to take Net Effect (Direct + Ind_Total) for these 20 cities from original df
  geom_point(
    data = df %>% arrange(desc(Effect_Indirect_Total)) %>% slice_head(n = 20),
    aes(
      y = reorder(city, Effect_Indirect_Total),
      x = Effect_Direct + Effect_Indirect_Total,
      fill = NULL
    ),
    color = "black", size = 3, shape = 18
  ) + # Diamond represents Net Effect

  scale_fill_manual(
    values = cols,
    labels = c("Direct (Habitat Loss)", "Indirect (Socioeconomic)", "Indirect (Soil)")
  ) +
  labs(
    title = "a | Mechanistic decomposition of reclamation impacts",
    subtitle = "Top 20 cities ranked by reclamation intensity. ♦ = Net Effect.",
    x = "Standardized Effect Magnitude", y = NULL, fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold")
  )

# ==============================================================================
# Fig 2: Regional Disparity
# ==============================================================================
# Use full data
p2 <- ggplot(df, aes(x = Regions, y = Effect_Indirect_Total, fill = Regions)) +
  geom_jitter(width = 0.2, alpha = 0.3, color = "grey50") +
  geom_boxplot(width = 0.5, outlier.shape = NA, alpha = 0.8) +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "b | Regional disparity in reclamation-induced risk",
    y = "Total Indirect Effect (Facilitation)", x = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )
p2
# ==============================================================================
# Combine plots
# ==============================================================================
# Use patchwork: Left is large plot (p1), Right is small plot (p2)
final_plot <- p1 + p2 + plot_layout(widths = c(1.5, 1))
final_plot


explore_data <- df %>%
  mutate(
    # Calculate Net Reclamation Effect
    Net_Recl_Effect = Effect_Direct + Effect_Indirect_Total,

    # Calculate "contribution rate" of reclamation to total risk (Note: risk is probability, Net is effect value, dimensions differ, mainly looking at relative magnitude)
    # For visualization, we look at the ratio of the two
    Recl_Contribution = Net_Recl_Effect / risk
  )

# ==============================================================================
# Fig A: Reclamation Driver Analysis (Scatter Plot)
# X-axis: Net Reclamation Effect
# Y-axis: Total Invasion Risk
# ==============================================================================

p_driver <- ggplot(explore_data, aes(x = Net_Recl_Effect, y = risk)) +
  # Auxiliary line
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +

  # Scatter points
  geom_point(aes(fill = Regions, size = reclamation_area),
    shape = 21, color = "white", alpha = 0.8
  ) +

  # Labels (Only label those deviating from diagonal, or particularly extreme ones)
  geom_text_repel(
    data = filter(explore_data, Net_Recl_Effect > 0.1 | risk > 0.4),
    aes(label = city), size = 3
  ) +
  scale_size_continuous(range = c(2, 8), guide = "none") +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "Is Risk Driven by Reclamation?",
    subtitle = "Cities above the line have risks driven by other factors (Climate etc.)\nCities on the right are heavily driven by Reclamation.",
    x = "Net Effect of Reclamation (Model Estimated)",
    y = "Total Invasion Risk"
  ) +
  theme_minimal()
p_driver
print(p_driver)
p_region_contrib <- ggplot(explore_data, aes(
  x = reorder(Regions, Net_Recl_Effect, median),
  y = Net_Recl_Effect,
  fill = Regions
)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.5, size = 1) +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "Regional Disparity in Reclamation Impact",
    subtitle = "East Asia is the hotspot where reclamation actively drives invasion risk",
    x = NULL,
    y = "Net Reclamation Effect (Risk Increase)"
  ) +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

print(p_region_contrib)
# 1. Data preparation (Normalize + Unstandardized coefficients)
# Assume b_direct = -0.562, b_indirect = 0.971 (Socioeconomic) + 0.193 (Soil) = 1.164
b_direct <- -0.562
b_indirect <- 1.164

plot_data_vector <- df %>%
  mutate(
    # Normalize reclamation (0-1)
    Recl_Norm = (reclamation_area - min(reclamation_area, na.rm = TRUE)) /
      (max(reclamation_area, na.rm = TRUE) - min(reclamation_area, na.rm = TRUE)),
    # Calculate effects
    Direct = Recl_Norm * b_direct,
    Indirect = Recl_Norm * b_indirect,
    Net = Direct + Indirect
  ) %>%
  # Label Top 10 cities and a few typical non-East Asia cities
  mutate(Label = ifelse(rank(desc(Recl_Norm)) <= 10 | city %in% c("Dubai", "Singapore", "Dar Es Salaam"), city, NA))

# 2. Plotting
p_vector <- ggplot(plot_data_vector) +

  # --- Background regions ---
  # Green zone: Net < 0 (Suppression zone, but doesn't exist with your coefficients)
  # Red zone: Net > 0 (Promotion zone)
  geom_abline(intercept = 0, slope = abs(b_direct) / b_indirect, linetype = "dotted", color = "grey") +

  # --- Vector arrows (Core) ---
  geom_segment(aes(x = 0, y = 0, xend = Direct, yend = Indirect, color = Regions),
    arrow = arrow(length = unit(0.2, "cm")), size = 0.8, alpha = 0.7
  ) +

  # --- Endpoint scatter ---
  geom_point(aes(x = Direct, y = Indirect, fill = Regions, size = risk),
    shape = 21, color = "white", stroke = 0.5
  ) +

  # --- Labels ---
  geom_text_repel(aes(x = Direct, y = Indirect, label = Label),
    size = 3, fontface = "italic", box.padding = 0.5, max.overlaps = 20
  ) +

  # --- Auxiliary explanation ---
  annotate("text",
    x = -0.1, y = 0.8, label = "Net Risk Amplification\n(Indirect > |Direct|)",
    color = "grey50", fontface = "bold", hjust = 0.5
  ) +

  # --- Scale and Color ---
  scale_color_brewer(palette = "Set1", name = "Region") +
  scale_fill_brewer(palette = "Set1", name = "Region") +
  scale_size_continuous(range = c(1, 6), name = "Total Risk") +

  # --- Coordinates and Theme ---
  labs(
    title = "<b>Mechanistic Vector of Reclamation Impacts</b>",
    subtitle = "Reclamation exerts a consistent 'pull' towards risk amplification driven by socioeconomic facilitation.",
    x = "Ecological Suppression (Habitat Loss) [-]",
    y = "Socioeconomic Facilitation (Propagule Pressure) [+]"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_markdown(face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.grid.minor = element_blank()
  )

print(p_vector)
# 1. Data selection (Select Top 25 reclamation cities)
plot_data_bar <- plot_data_vector %>%
  arrange(desc(Recl_Norm)) %>%
  slice_head(n = 25) %>%
  mutate(city = reorder(city, Recl_Norm))

# 2. Plotting
p_balance <- ggplot(plot_data_bar) +
  # 0 line
  geom_hline(yintercept = 0, color = "black") +

  # Indirect effect (Benefit, upward)
  geom_col(aes(x = city, y = Indirect), fill = "#4DBBD5", alpha = 0.8, width = 0.6) +

  # Direct effect (Cost, downward)
  geom_col(aes(x = city, y = Direct), fill = "#E64B35", alpha = 0.8, width = 0.6) +

  # Net effect (Point)
  geom_point(aes(x = city, y = Net), shape = 18, size = 4, color = "black") +

  # Connecting line (Optional, increases visual coherence)
  geom_segment(aes(x = city, xend = city, y = Direct, yend = Indirect),
    color = "grey30", size = 0.2, alpha = 0.5
  ) +
  coord_flip() +
  labs(
    title = "<b>The Ecological Balance Sheet of Reclamation</b>",
    subtitle = "<span style='color:#4DBBD5;'><b>Socioeconomic Benefit</b></span> vs. <span style='color:#E64B35;'><b>Ecological Cost</b></span>. <br>Diamond (♦) indicates Net Invasion Pressure.",
    y = "Effect Magnitude", x = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_markdown(face = "bold"),
    plot.subtitle = element_markdown(),
    panel.grid.major.y = element_blank()
  )
print(p_balance)

# ==============================================================================
# 1. Data preparation: Regional aggregation and effect calculation
# ==============================================================================
names(df)
continent_data <- df %>%
  # mutate(
  #   # --- A. Define Continent logic ---
  #   Continent = case_when(
  #     str_detect(Regions, "Asia") ~ "Asia",
  #     str_detect(Regions, "Europe") ~ "Europe",
  #     str_detect(Regions, "Africa") ~ "Africa",
  #     str_detect(Regions, "America") ~ "America",
  #     str_detect(Regions, "Oceania") ~ "Oceania",
  #     TRUE ~ "Other" # Prevent missing cases
  #   ),

  #   # --- B. Calculate effect for each city (based on unstandardized coefficients) ---
  #   # Normalize reclamation intensity (0-1)
  #   Recl_Norm = (reclamation_area - min(reclamation_area, na.rm=TRUE)) /
  #               (max(reclamation_area, na.rm=TRUE) - min(reclamation_area, na.rm=TRUE)),

  #   # Calculate three forces
  #   Eff_Direct = Recl_Norm * (-0.562),     # Physical suppression (Cost)
  #   Eff_Socio  = Recl_Norm * (0.971),      # Economic facilitation (Benefit 1)
  #   Eff_Soil   = Recl_Norm * (0.193),      # Soil facilitation (Benefit 2)
  #   Eff_Net    = Eff_Direct + Eff_Socio + Eff_Soil # Net effect
  # ) %>%

  # --- C. Aggregate by region (Mean) ---
  group_by(Regions) %>%
  summarise(
    Mean_Direct = mean(Direct_Effect, na.rm = TRUE),
    Mean_Socio  = mean(Ind_Socio, na.rm = TRUE),
    Mean_Soil   = mean(Ind_Soil, na.rm = TRUE),
    Mean_Net    = mean(Total_Effect, na.rm = TRUE),
    n_cities    = n() # Count cities per region
  ) %>%
  # Filter out "Other" or NA
  filter(Regions != "Other") %>%
  # Convert to long format for stacking
  pivot_longer(
    cols = c(Mean_Direct, Mean_Socio, Mean_Soil),
    names_to = "Component", values_to = "Value"
  ) %>%
  mutate(
    # Set stack order: Soil on top, Socio in middle, Direct alone on left
    Component = factor(Component, levels = c("Mean_Soil", "Mean_Socio", "Mean_Direct"))
  )

# Extract Net data for plotting points
net_points_cont <- continent_data %>%
  select(Regions, Mean_Net) %>%
  distinct()

continent_data <- df %>%
  # --- C. Aggregate by region (Median) ---
  group_by(Regions) %>%
  summarise(
    Mean_Direct = median(Direct_Effect, na.rm = TRUE),
    Mean_Socio  = median(Ind_Socio, na.rm = TRUE),
    Mean_Soil   = median(Ind_Soil, na.rm = TRUE),
    Mean_Net    = median(Total_Effect, na.rm = TRUE),
    n_cities    = n() # Count cities per region
  ) %>%
  # Filter out "Other" or NA
  filter(Regions != "Other") %>%
  # Convert to long format for stacking
  pivot_longer(
    cols = c(Mean_Direct, Mean_Socio, Mean_Soil),
    names_to = "Component", values_to = "Value"
  ) %>%
  mutate(
    # Set stack order: Soil on top, Socio in middle, Direct alone on left
    Component = factor(Component, levels = c("Mean_Soil", "Mean_Socio", "Mean_Direct"))
  )

# Extract Net data for plotting points
net_points_cont <- continent_data %>%
  select(Regions, Mean_Net) %>%
  distinct()
# ==============================================================================
# 2. Plotting: Continental Asymmetric Stacked Chart
# ==============================================================================

# Define advanced colors
cols <- c(
  "Mean_Direct" = "#E64B35", # Red (Cost)
  "Mean_Socio" = "#3C5488", # Dark Blue (Economic)
  "Mean_Soil" = "#4DBBD5" # Light Blue (Soil)
)

# --- 0 line ---
p_continent <- ggplot() +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.6) +
  # --- 1. Bar chart ---
  geom_col(
    data = continent_data,
    aes(x = Value, y = reorder(Regions, Mean_Net), fill = Component),
    width = 0.5, alpha = 0.9
  ) +
  # --- 2. Net Effect Point ---
  geom_point(
    data = net_points_cont,
    aes(x = Mean_Net, y = Continent),
    shape = 18, size = 6, color = "black"
  ) + # Large black diamond
  # --- 3. Color Settings ---
  scale_fill_manual(
    values = cols,
    labels = c("Soil Facilitation (+)", "Socioeconomic Facilitation (+)", "Physical Constraint (-)")
  ) +
  # --- 4. Auxiliary Labels ---
  annotate("text",
    x = min(continent_data$Value) * 0.9, y = 0.6,
    label = "Ecological Cost", fontface = "bold", color = "#E64B35", size = 4
  ) +
  annotate("text",
    x = max(continent_data$Value) * 0.9, y = 0.6,
    label = "Cumulative Benefit", fontface = "bold", color = "#3C5488", size = 4
  ) +
  # --- 5. Axes and Title ---
  labs(
    title = "<b>Continental Breakdown of Reclamation Mechanisms</b>",
    subtitle = "Interplay of <span style='color:#E64B35;'>Direct Constraints</span> vs. <span style='color:#3C5488;'>Socioeconomic</span> & <span style='color:#4DBBD5;'>Soil</span> Facilitation.<br>♦ = Net Risk Outcome.",
    x = "Mean Standardized Effect Magnitude",
    y = NULL,
    fill = NULL
  ) +
  # --- 6. Theme ---
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_markdown(face = "bold", size = 16),
    plot.subtitle = element_markdown(size = 12, color = "grey30"),
    # Axis text
    axis.text.y = element_text(size = 12, face = "bold", color = "black"),
    axis.text.x = element_text(size = 10),
    # Remove extra grid lines
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    # Add black border
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.2),
    # Legend settings
    legend.position = "top",
    legend.justification = "right",
    legend.box = "horizontal",
    legend.margin = margin(b = -10)
  )

# Print and Save
print(p_continent)

continent_data


plot_data_attrib <- df %>%
  mutate(
    # A. Recalculate net effect (unstandardized, ensure consistency)
    Recl_Norm = (reclamation_area - min(reclamation_area, na.rm = TRUE)) /
      (max(reclamation_area, na.rm = TRUE) - min(reclamation_area, na.rm = TRUE)),
    Effect_Net_Real = Recl_Norm * 0.602, # Your real net coefficient

    # B. Calculate attribution ratio (Dependency)
    # Note: If E_net > Risk (due to model nonlinearity), truncate to 100%, which is reasonable for explaining "responsibility"
    # Meaning: Current risk is completely (100%) explained by reclamation
    Dependency_Pct = (Effect_Net_Real / risk) * 100,
    Dependency_Pct = ifelse(Dependency_Pct > 100, 100, Dependency_Pct)
  ) %>%
  # Define classification (for plotting color or shape)
  mutate(
    City_Type = case_when(
      risk > median(risk) & Dependency_Pct > 50 ~ "Man-made Hotspot", # High risk, mainly due to reclamation
      risk > median(risk) & Dependency_Pct <= 50 ~ "Natural Hotspot", # High risk, mainly due to background
      TRUE ~ "Low Risk / Latent"
    )
  )

# Filter labels
label_data <- plot_data_attrib %>%
  filter(risk > quantile(risk, 0.85) | Dependency_Pct > 80)
label_data
# 2. Plotting
p_attrib_scatter <- ggplot(plot_data_attrib, aes(x = risk, y = Dependency_Pct)) +

  # --- Background partitioning (Optional, dashed line) ---
  geom_hline(yintercept = 50, linetype = "dashed", color = "grey80") +
  geom_vline(xintercept = median(plot_data_attrib$risk), linetype = "dashed", color = "grey80") +

  # --- Scatter points ---
  geom_point(aes(fill = City_Type, size = reclamation_area),
    shape = 21, color = "white", stroke = 0.5, alpha = 0.85
  ) +

  # --- Labels ---
  geom_text_repel(
    data = label_data, aes(label = city),
    size = 8, box.padding = 0.5, max.overlaps = 20
  ) +

  # # --- Region annotations ---
  # annotate("text", x = max(df$risk), y = 100, label = "Man-made\nHotspots",
  #          color = "#E64B35", fontface = "bold", hjust = 1, vjust = 1, size = 9) +
  # annotate("text", x = max(df$risk), y = 0, label = "Natural/Background\nHotspots",
  #          color = "#3C5488", fontface = "bold", hjust = 1, vjust = 0, size = 9) +

  # --- Color and scale ---
  scale_fill_manual(values = c(
    "Man-made Hotspot" = "#E64B35", # Red alert
    "Natural Hotspot" = "#3C5488", # Blue background
    "Low Risk / Latent" = "grey70" # Grey
  ), name = "City Typology") +
  scale_size_continuous(range = c(2, 10), guide = "none") +

  # --- Title and axes ---
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
ggsave("${LOCAL_ANALYSIS_ROOT}/figure/gam_draft/plot5.2_reclamationDependency.png", plot = p_attrib_scatter, width = 10, height = 12, bg = "transparent", dpi = 300)

plot_data_main <- continent_data %>%
  # Ensure Component factor order is correct
  mutate(Component = factor(Component, levels = c("Mean_Soil", "Mean_Socio", "Mean_Direct")))

# 2. Define colors
cols <- c("Mean_Direct" = "#4C6E72", "Mean_Socio" = "#D2807D", "Mean_Soil" = "#EADDC9")

# 3. Plot Main Chart (Include all continents)
p_main <- ggplot() +
  geom_vline(xintercept = 0, color = "black", linewidth = 1) +

  # Bars
  geom_col(
    data = plot_data_main,
    aes(x = Value, y = reorder(Regions, Mean_Net), fill = Component),
    width = 0.8, alpha = 0.9
  ) +

  # Net Effect Point
  geom_point(
    data = net_points_cont,
    aes(x = Mean_Net, y = Regions),
    shape = 18, size = 12, color = "black"
  ) +
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

# 4. Plot Inset (Exclude East Asia/Asia)
# Find the continent with the largest value (Assume it is Asia)
max_continent <- "Asia" # Or determine automatically from data

p_inset <- ggplot() +
  geom_vline(xintercept = 0, color = "black", linewidth = 1) +
  # Filter out the largest continent
  geom_col(
    data = plot_data_main %>% filter(Regions != max_continent),
    aes(x = Value, y = reorder(Regions, Mean_Net), fill = Component),
    width = 0.8, alpha = 0.9
  ) +
  geom_point(
    data = net_points_cont %>% filter(Regions != max_continent),
    aes(x = Mean_Net, y = Regions),
    shape = 18, size = 5, color = "black"
  ) +
  scale_fill_manual(values = cols) +
  labs(title = "Other Regions", x = NULL, y = NULL, size = 15) +
  theme_minimal(base_size = 25) +
  theme(
    legend.position = "none", # Inset plot no legend
    panel.grid = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "grey50", fill = NA, linewidth = 1.5), # Inset plot add a grey frame
    plot.background = element_rect(fill = "white", color = NA) # Inset plot background set to white, cover main plot grid
  )

# 5. Combine (put the inset plot in the blank space of the main plot)
# inset_element(plot, left, bottom, right, top) range is 0-1
# Depending on your data distribution, the East Asia bar is very long, so bottom right or top right is usually empty
final_plot <- p_main + inset_element(p_inset, 0.4, 0.05, 0.95, 0.55)
windows()
print(p_main)
ggsave("${LOCAL_ANALYSIS_ROOT}/figure/gam_draft/plot5.1_Mediannetscore_regional.png", plot = p_main, width = 14, height = 13, bg = "transparent", dpi = 900)

p_break <- ggplot() +
  geom_vline(xintercept = 0, color = "black") +
  geom_col(
    data = plot_data_main,
    aes(x = Value, y = reorder(Continent, Mean_Net), fill = Component),
    width = 0.6, alpha = 0.9
  ) +
  geom_point(
    data = net_points_cont,
    aes(x = Mean_Net, y = Continent),
    shape = 18, size = 5, color = "black"
  ) +
  scale_fill_manual(values = cols) +

  # --- Core: Set breakpoint ---
  # Assume other continents are between -0.2 and 0.2, only Asia reaches 1.0
  # We cut out the 0.3 to 0.8 segment (adjust according to your data)
  scale_x_break(c(0.13, 1.0), scale = 0.5) +
  labs(
    title = "Regional Mechanism Breakdown (Broken Axis)",
    x = "Effect Magnitude", y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    panel.grid.major.y = element_blank()
  )

print(p_break)
