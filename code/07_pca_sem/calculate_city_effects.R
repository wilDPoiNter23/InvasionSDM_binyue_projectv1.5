# City-level SEM contributions
# Input: SEM composite table and coefficient-based city table
# Output: Direct, indirect and net-effect data

source(file.path(Sys.getenv("INVASIONSDM_PROJECT_ROOT", "."), "config", "bootstrap.R"))
dir.create(publication_path("SEM_OUTPUT"), recursive=TRUE, showWarnings=FALSE)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(ggtext)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(biscale)
library(cowplot)
library(readxl)
library(ggplot2, lib.loc = .libPaths()[1])  # Load from the main library.
df <-read_excel(publication_path("LOCAL_ANALYSIS_ROOT", "data/organized_table/05_effectScore_Qudurat.xlsx"))
dt1 <- read.csv(publication_path("SEM_INPUT"))
# 1. Use unstandardised coefficients (Estimate).
b_direct      <- -0.562
b_path_socio  <- 1.074 * 0.904  # = 0.971
b_path_soil   <- 0.674 * 0.287  # = 0.193
b_ind_total   <- b_path_socio + b_path_soil  # = 1.164
b_total       <- b_direct + b_ind_total      # = 0.602
# Estimates from Risk ~ Socioeconomic + Climate + Soil.
b_soc  <- 0.904
b_cli  <- 0.661
b_soil <- 0.287
# 2. Use the composites constructed before psem, already present in df, without PCA.
# df must contain reclamation, socioeconomic, Climate, Soil and risk.
# reclamation = beta_reclamation_area*reclamation_area + beta_area_ratio*area_ratio
# socioeconomic, Climate and Soil must match the composites constructed with risk-regression weights.

df_updated <- dt1 %>%
  mutate(
    # Use the reclamation mechanism variable in the SEM rather than reclamation_area.
    X_recl = reclamation,

    # Use the same outcome as the model, including any centring or relative-risk transformation.
    Y = risk,

    # Direct, indirect and total reclamation contributions on the same Y scale.
    Direct_Effect   = b_direct     * X_recl,
    Ind_Socio       = b_path_socio * X_recl,
    Ind_Soil        = b_path_soil  * X_recl,
    Indirect_Effect = Ind_Socio + Ind_Soil,
    Total_Effect    = b_total      * X_recl,

    # For other predictors, use composites from the SEM risk equation without PCA.
    OtherDrivers = b_soc  * socioeconomic +
                   b_cli  * Climate +
                   b_soil * Soil,

    # Mechanism share: reclamation mechanism versus other mechanisms.
    Dependency_mech = abs(Total_Effect) /
      (abs(Total_Effect) + abs(OtherDrivers)) * 100,

    # Optional magnitude share: reclamation-effect magnitude relative to outcome magnitude; distinct from mechanism share.
    Dependency_intensity = abs(Total_Effect) /
      (abs(Total_Effect) + abs(Y)) * 100
  )
  df_updated
  df_updated_2 <- df %>% mutate(


    # E. Quadrant classification.
    x_med = median(Pressure, na.rm = TRUE),
    y_thr = 50,
    Quadrant_Num = case_when(
      Pressure >= x_med & Dependency_mech  >= y_thr ~ 1,
      Pressure <  x_med & Dependency_mech >= y_thr ~ 2,
      Pressure <  x_med & Dependency_mech <  y_thr ~ 3,
      Pressure >= x_med & Dependency_mech  <  y_thr ~ 4
    ),
    Quadrant_Label = case_when(
      Quadrant_Num == 1 ~ "Q1: Amplified Hotspots",
      Quadrant_Num == 2 ~ "Q2: Latent Risk",
      Quadrant_Num == 3 ~ "Q3: Safe Coldspots",
      Quadrant_Num == 4 ~ "Q4: Natural Hotspots"
    ),
     Quadrant_intensity_Num = case_when(
      Pressure >= x_med & Dependency_intensity  >= y_thr ~ 1,
      Pressure <  x_med & Dependency_intensity >= y_thr ~ 2,
      Pressure <  x_med & Dependency_intensity <  y_thr ~ 3,
      Pressure >= x_med & Dependency_intensity  <  y_thr ~ 4
    ),
    Quadrant_intensity_Label = case_when(
      Quadrant_intensity_Num == 1 ~ "Q1: Amplified Hotspots",
      Quadrant_intensity_Num == 2 ~ "Q2: Latent Risk",
      Quadrant_intensity_Num == 3 ~ "Q3: Safe Coldspots",
      Quadrant_intensity_Num == 4 ~ "Q4: Natural Hotspots"
    )
    )
df_updated_2
# 4. Inspect results.
df_updated_2$Quadrant_Label
write.csv(df_updated_2,publication_path("LOCAL_ANALYSIS_ROOT", "data/organized_table/0501_risk_city_netscorewithQ.csv"))

# 1. Calculate effects from the fitted coefficients, including Soil.
plot_data_real <- df %>%
  mutate(
    # Normalise reclamation area to 0-1.
    Recl_Rescaled = (reclamation_area - min(reclamation_area, na.rm=TRUE)) /
                    (max(reclamation_area, na.rm=TRUE) - min(reclamation_area, na.rm=TRUE)),

    # A. Direct effect (Cost).
    Effect_Direct = Recl_Rescaled * (-0.562),

    # B. Indirect effect (Benefit/Facilitation).
    # Path 1: Socioeconomic.
    Ind_Socio = Recl_Rescaled * (1.074 * 0.904), # = 0.971 * A
    # Path 2: Soil.
    Ind_Soil  = Recl_Rescaled * (0.674 * 0.287), # = 0.193 * A

    # Sum the positive contributions to the total indirect effect.
    Effect_Indirect_Total = Ind_Socio + Ind_Soil
  )
plot_data_real
write.csv(plot_data_real, publication_path("OUTPUT_ROOT", "figures/gam_draft/netscore_updated.csv"), row.names = FALSE)
# 2. Plot Direct on the x-axis and Total Indirect on the y-axis.
p_cost_benefit_v2 <- ggplot(plot_data_real, aes(x = Effect_Direct, y = Effect_Indirect_Total)) +

  # Reference lines.
  geom_hline(yintercept = 0, color = "grey80") +
  geom_vline(xintercept = 0, color = "grey80") +
  # 1:1 diagonal.
  geom_abline(intercept = 0, slope = -1, linetype = "dashed", color = "grey60") +

  # Scatter points.
  geom_point(aes(fill = risk, size = Recl_Rescaled),
             shape = 21, color = "white", stroke = 0.4, alpha = 0.9) +

  # Label selected points.
  geom_text_repel(data = filter(plot_data_real, risk > quantile(risk, 0.95) | Recl_Rescaled > 0.5),
                  aes(label = city), size = 3, fontface = "italic", box.padding = 0.5) +

  scale_fill_viridis_c(option = "magma", direction = -1, name = "Invasion Risk") +
  scale_size_continuous(range = c(2, 6), guide = "none") +

  # Titles and axis descriptions.
  labs(
    title = "<b>Cumulative Cost-Benefit Trade-off of Reclamation</b>",
    subtitle = "Direct suppression vs. Cumulative indirect facilitation (Socioeconomic + Soil)",
    x = "Direct Ecological Effect (Habitat Suppression) [-]",
    y = "Total Indirect Effect (Facilitation) [+]"
  ) +

  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_markdown(face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.grid = element_blank(),
    legend.position = c(0.85, 0.25)
  )

print(p_cost_benefit_v2)
ggsave("Cost_Benefit_Total_Indirect.pdf", width = 7, height = 7)


# 1. Prepare the basemap.
world <- ne_countries(scale = "medium", returnclass = "sf")

# 2. Prepare data and calculate net effect.
map_data_bi <- df %>%
  mutate(
    # Normalise reclamation intensity.
    Recl_Norm = (reclamation_area - min(reclamation_area, na.rm=TRUE)) /
                (max(reclamation_area, na.rm=TRUE) - min(reclamation_area, na.rm=TRUE)),
    # Calculate net effect using fitted coefficients.
    # Example coefficients: Direct (-0.562) + Indirect (1.164) = 0.602.
    Net_Effect = Recl_Norm * 0.602
  )
# ==============================================================================
# 2. Calculate quantile breaks for numerical legend labels.
# ==============================================================================
# Use 3 x 3 quantile classes.
# Calculate x-axis Net Effect quantiles at 0%, 33%, 66% and 100%.
x_breaks <- quantile(abs(df_updated$Total_Effect), probs = c(0, 1/4, 2/4,3/4, 1), na.rm = TRUE)
x_breaks
# Calculate y-axis Total Risk quantiles at 0%, 33%, 66% and 100%.
y_breaks <- quantile(df_updated$Pressure, probs = c(0, 1/4, 2/4,3/4, 1), na.rm = TRUE)
y_breaks
x_breaks
# Format numbers to 2-3 decimal places.
x_labs <- sprintf("%.3f", x_breaks)
y_labs <- sprintf("%.3f", y_breaks)
x_labs
y_labs
# Create a bivariate classification column.
df_updated <- df_updated %>%
  mutate(Total_Effect_abs = abs(Total_Effect))
data_bi <- bi_class(df_updated, x = Total_Effect_abs , y = Pressure, style = "quantile", dim = 4)
data_bi
# ==============================================================================

map <- ggplot() +
  geom_sf(data = world, fill =  "#eeede7", color = "#727583", size = 0.1) +
   geom_point(data = data_bi,
             aes(x = Longitude, y = Latitude, fill = bi_class), # 1. Map values to fill.
             shape = 21,       # 2. Use shape 21, which supports separate fill and border colours.
             color = " grey30",  # 3. Set the border colour, such as white or grey30.
             stroke = 0.4,     # 4. Set border width, for example 0.3-0.5.
             size = 4,       # Use larger points to distinguish fill colours.
             alpha = 0.7) +

  # Use bi_scale_fill to match the fill aesthetic.
  bi_scale_fill("DkBlue2", dim = 4) +
  coord_sf(crs = sf::st_crs(4326), xlim = c(-180, 180), ylim = c(-90, 90), expand = FALSE) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_markdown(face = "bold"),
    plot.subtitle = element_markdown(size = 10, color = "grey30"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    panel.background = element_rect(fill = '#cde0f4', color = NA) # Light blue ocean background.
  )


# ==============================================================================
# 4. Draw a custom legend with numerical ticks.
# ==============================================================================
legend <- bi_legend(pal = "DkBlue2", dim = 4, xlab = "Reclamation-Induced Pressure", ylab = "Invasion Pressure Index")
# 5. Combine panels.
final_plot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  # Adjust legend position and dimensions (x, y, width, height).
  # Place the legend in the lower-left ocean area.
  draw_plot(legend, 0.05, 0.1, 0.25, 0.25)

print(final_plot)
ggsave( publication_path("OUTPUT_ROOT", "figures/gam_draft/plot5_netscore_bicovirate.png"), plot = final_plot, width = 10, height = 12, bg = "transparent", dpi = 1200, ,device = grDevices::png)


map_CN <- ggplot() +
  geom_sf(data = world, fill =  "#eeede7", color = "#727583", size = 0.1) +
   geom_point(data = data_bi,
             aes(x = Longitude, y = Latitude, fill = bi_class), # 1. Map values to fill.
             shape = 21,       # 2. Use shape 21, which supports separate fill and border colours.
             color = " grey50",  # 3. Set the border colour, such as white or grey30.
             stroke = 0.55,     # 4. Set border width, for example 0.3-0.5.
             size = 15,       # Use larger points to distinguish fill colours.
             alpha = 0.7) +

  # Use bi_scale_fill to match the fill aesthetic.
  bi_scale_fill("DkBlue2", dim = 4) +
  coord_sf(crs = sf::st_crs(4326), xlim = c(105, 130), ylim = c(15, 45), expand = FALSE)  +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_markdown(face = "bold"),
    plot.subtitle = element_markdown(size = 10, color = "grey30"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    panel.background = element_rect(fill = '#cde0f4', color = NA) # Light blue ocean background.
  )
  map_CN
ggsave( publication_path("OUTPUT_ROOT", "figures/gam_draft/CN_plot5_netscore_bicovirate.png"), plot = map_CN, width = 10, height = 12, bg = "transparent", dpi = 1200, device = grDevices::png)

write.csv(df_updated, publication_path("SEM_OUTPUT", "city_sem_contributions.csv"), row.names=FALSE)
