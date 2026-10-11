# ARCHIVE: historical reference; symbolic paths are intentionally not runnable.
# Original SHA256: fc7f06c8e788097aef8b0698027343d6c21a2b0073a140065d441a85be5c7cdb
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
install.packages("readxl")
install.packages("biscale")
detach("package:ggtext", unload = TRUE)
library(ggplot2, lib.loc = .libPaths()[1]) # Load from main library
df <- read.csv("${LOCAL_ANALYSIS_ROOT}/figure/gam_draft/gamtest_withpca.csv")
df <- read_excel("${LOCAL_ANALYSIS_ROOT}/data/organized_table/05_effectScore_Qudurat.xlsx")
dt1 <- read.csv("${LOCAL_ANALYSIS_ROOT}/figure/gam_draft/dt1_psem_preprocessed_scaled.csv")
head(df)
# --- 1) Use unstandardized coefficients (Estimate) ---
b_direct <- -0.562
b_path_socio <- 1.074 * 0.904 # = 0.971
b_path_soil <- 0.674 * 0.287 # = 0.193
b_ind_total <- b_path_socio + b_path_soil # = 1.164
b_total <- b_direct + b_ind_total # = 0.602
# Coefficients (Estimate) for Risk ~ Socioeconomic + Climate + Soil
b_soc <- 0.904
b_cli <- 0.661
b_soil <- 0.287
# --- 2) No PCA: Use the composite constructed before psem (must exist in df) ---
# df must contain these columns: reclamation, socioeconomic, Climate, Soil, risk
# reclamation = beta_reclamation_area*reclamation_area + beta_area_ratio*area_ratio
# socioeconomic / Climate / Soil should also be composite variables constructed using risk regression weights

df_updated <- dt1 %>%
  mutate(
    # This is the true "reclamation mechanism variable" in your SEM (not reclamation_area)
    X_recl = reclamation,

    # This is your outcome variable (Note: if risk is centered/relative in your model, use the same one)
    Y = risk,

    # --- Direct / indirect / total contribution of reclamation (all on the same scale as Y) ---
    Direct_Effect = b_direct * X_recl,
    Ind_Socio = b_path_socio * X_recl,
    Ind_Soil = b_path_soil * X_recl,
    Indirect_Effect = Ind_Socio + Ind_Soil,
    Total_Effect = b_total * X_recl,

    # --- Other drivers: Use composite from SEM risk equation (no PCA) ---
    OtherDrivers = b_soc * socioeconomic +
      b_cli * Climate +
      b_soil * Soil,

    # ✅ Mechanism share: Reclamation mechanism vs Other mechanisms
    Dependency_mech = abs(Total_Effect) /
      (abs(Total_Effect) + abs(OtherDrivers)) * 100,

    # (Optional) Intensity share: Reclamation effect magnitude vs Outcome magnitude (not mechanism share)
    Dependency_intensity = abs(Total_Effect) /
      (abs(Total_Effect) + abs(Y)) * 100
  )
df_updated
df_updated_2 <- df %>% mutate(


  # --- E. Quadrant (Your original logic) ---
  x_med = median(Pressure, na.rm = TRUE),
  y_thr = 50,
  Quadrant_Num = case_when(
    Pressure >= x_med & Dependency_mech >= y_thr ~ 1,
    Pressure < x_med & Dependency_mech >= y_thr ~ 2,
    Pressure < x_med & Dependency_mech < y_thr ~ 3,
    Pressure >= x_med & Dependency_mech < y_thr ~ 4
  ),
  Quadrant_Label = case_when(
    Quadrant_Num == 1 ~ "Q1: Amplified Hotspots",
    Quadrant_Num == 2 ~ "Q2: Latent Risk",
    Quadrant_Num == 3 ~ "Q3: Safe Coldspots",
    Quadrant_Num == 4 ~ "Q4: Natural Hotspots"
  ),
  Quadrant_intensity_Num = case_when(
    Pressure >= x_med & Dependency_intensity >= y_thr ~ 1,
    Pressure < x_med & Dependency_intensity >= y_thr ~ 2,
    Pressure < x_med & Dependency_intensity < y_thr ~ 3,
    Pressure >= x_med & Dependency_intensity < y_thr ~ 4
  ),
  Quadrant_intensity_Label = case_when(
    Quadrant_intensity_Num == 1 ~ "Q1: Amplified Hotspots",
    Quadrant_intensity_Num == 2 ~ "Q2: Latent Risk",
    Quadrant_intensity_Num == 3 ~ "Q3: Safe Coldspots",
    Quadrant_intensity_Num == 4 ~ "Q4: Natural Hotspots"
  )
)
df_updated_2
# 4. Check results
df_updated_2$Quadrant_Label
head(df_updated %>% select(City, Pressure, Net, Dependency, Quadrant_Label))
write.csv(df_analysis, "${LOCAL_ANALYSIS_ROOT}/data/organized_table/0502_risk_city_netscoreStdVersion.csv", row.names = FALSE)
write.csv(df_updated_2, "${LOCAL_ANALYSIS_ROOT}/data/organized_table/0501_risk_city_netscorewithQ.csv")
df_updated <- read.csv("${LOCAL_ANALYSIS_ROOT}/data/organized_table/0502_risk_city_netscoreStdVersion.csv")
# 1. Prepare data: Calculate effects based on real coefficients (including Soil)
plot_data_real <- df %>%
  mutate(
    # Normalize reclamation area (0 to 1)
    Recl_Rescaled = (reclamation_area - min(reclamation_area, na.rm = TRUE)) /
      (max(reclamation_area, na.rm = TRUE) - min(reclamation_area, na.rm = TRUE)),

    # --- A. Direct Effect (Cost) ---
    Effect_Direct = Recl_Rescaled * (-0.562),

    # --- B. Indirect Effect (Benefit/Facilitation) ---
    # Path 1: Socioeconomic
    Ind_Socio = Recl_Rescaled * (1.074 * 0.904), # = 0.971 * A
    # Path 2: Soil (New!)
    Ind_Soil = Recl_Rescaled * (0.674 * 0.287), # = 0.193 * A

    # Total Indirect Effect (Sum of all positive ones)
    Effect_Indirect_Total = Ind_Socio + Ind_Soil
  )
plot_data_real
write.csv(plot_data_real, "${LOCAL_ANALYSIS_ROOT}/figure/gam_draft/netscore_updated.csv", row.names = FALSE)
# 2. Plotting (X-axis: Direct, Y-axis: Total Indirect)
p_cost_benefit_v2 <- ggplot(plot_data_real, aes(x = Effect_Direct, y = Effect_Indirect_Total)) +

  # Auxiliary lines
  geom_hline(yintercept = 0, color = "grey80") +
  geom_vline(xintercept = 0, color = "grey80") +
  # 1:1 Diagonal line
  geom_abline(intercept = 0, slope = -1, linetype = "dashed", color = "grey60") +

  # Scatter points
  geom_point(aes(fill = risk, size = Recl_Rescaled),
    shape = 21, color = "white", stroke = 0.4, alpha = 0.9
  ) +

  # Labels (Only highlight key points)
  geom_text_repel(
    data = filter(plot_data_real, risk > quantile(risk, 0.95) | Recl_Rescaled > 0.5),
    aes(label = city), size = 3, fontface = "italic", box.padding = 0.5
  ) +
  scale_fill_viridis_c(option = "magma", direction = -1, name = "Invasion Risk") +
  scale_size_continuous(range = c(2, 6), guide = "none") +

  # Title and axes (Update description)
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
windows()
ggsave("Cost_Benefit_Total_Indirect.pdf", width = 7, height = 7)


# 1. Prepare base map
world <- ne_countries(scale = "medium", returnclass = "sf")

# 2. Prepare data (Calculate net effect)
map_data_bi <- df %>%
  mutate(
    # Normalize reclamation intensity
    Recl_Norm = (reclamation_area - min(reclamation_area, na.rm = TRUE)) /
      (max(reclamation_area, na.rm = TRUE) - min(reclamation_area, na.rm = TRUE)),
    # Calculate net effect (Use your real coefficients)
    # Assumed coefficients: Direct(-0.562) + Indirect(1.164) = 0.602
    Net_Effect = Recl_Norm * 0.602
  )
head(df_updated)
# ==============================================================================
# 2. Calculate quantile breakpoints (Critical step: to show values on legend)
# ==============================================================================
# We use 3x3 quantile classification
# Calculate X-axis (Net Effect) 0%, 33%, 66%, 100% quantiles
x_breaks <- quantile(abs(df_updated$Total_Effect), probs = c(0, 1 / 4, 2 / 4, 3 / 4, 1), na.rm = TRUE)
x_breaks
# Calculate Y-axis (Total Risk) 0%, 33%, 66%, 100% quantiles
y_breaks <- quantile(df_updated$Pressure, probs = c(0, 1 / 4, 2 / 4, 3 / 4, 1), na.rm = TRUE)
y_breaks
x_breaks
# Format values (Keep 2-3 decimal places)
x_labs <- sprintf("%.3f", x_breaks)
y_labs <- sprintf("%.3f", y_breaks)
x_labs
y_labs
# Create bivariate classification column
df_updated <- df_updated %>%
  mutate(Total_Effect_abs = abs(Total_Effect))
data_bi <- bi_class(df_updated, x = Total_Effect_abs, y = Pressure, style = "quantile", dim = 4)
data_bi
# ==============================================================================

map <- ggplot() +
  geom_sf(data = world, fill = "#eeede7", color = "#727583", size = 0.1) +
  geom_point(
    data = data_bi,
    aes(x = Longitude, y = Latitude, fill = bi_class), # 1. Change to fill here
    shape = 21, # 2. Key: Use shape 21 which supports border
    color = " grey30", # 3. Set border color here (recommend white or grey30)
    stroke = 0.4, # 4. Set border width here (0.3 - 0.5 looks refined)
    size = 4, # Points slightly larger to see colors clearly
    alpha = 0.7
  ) +

  # 【关键配套修改】因为上面用了 fill，这里必须用 bi_scale_fill
  bi_scale_fill("DkBlue2", dim = 4) +
  coord_sf(crs = sf::st_crs(4326), xlim = c(-180, 180), ylim = c(-90, 90), expand = FALSE) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_markdown(face = "bold"),
    plot.subtitle = element_markdown(size = 10, color = "grey30"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    panel.background = element_rect(fill = "#cde0f4", color = NA) # 淡蓝色海洋背景
  )


# ==============================================================================
# 4. 【核心】手绘带数值刻度的图例
# ==============================================================================
legend <- bi_legend(pal = "DkBlue2", dim = 4, xlab = "Reclamation-Induced Pressure", ylab = "Invasion Pressure Index")
# 5. 组合
final_plot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  # 调整图例的位置 (x, y, width, height)
  # 放在左下角海洋处
  draw_plot(legend, 0.05, 0.1, 0.25, 0.25)

print(final_plot)
ggsave("${LOCAL_ANALYSIS_ROOT}/figure/gam_draft/plot5_netscore_bicovirate.png", plot = final_plot, width = 10, height = 12, bg = "transparent", dpi = 1200, , device = grDevices::png)


map_CN <- ggplot() +
  geom_sf(data = world, fill = "#eeede7", color = "#727583", size = 0.1) +
  geom_point(
    data = data_bi,
    aes(x = Longitude, y = Latitude, fill = bi_class), # 1. 这里改成 fill
    shape = 21, # 2. 关键：使用支持边框的形状 21
    color = " grey50", # 3. 这里设置边框颜色 (推荐白色或 grey30)
    stroke = 0.55, # 4. 这里设置边框粗细 (0.3 - 0.5 比较精致)
    size = 15, # 点稍微大一点，方便看清颜色
    alpha = 0.7
  ) +

  # [Key complimentary change] Since fill was used above, must use bi_scale_fill here
  bi_scale_fill("DkBlue2", dim = 4) +
  coord_sf(crs = sf::st_crs(4326), xlim = c(105, 130), ylim = c(15, 45), expand = FALSE) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_markdown(face = "bold"),
    plot.subtitle = element_markdown(size = 10, color = "grey30"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    panel.background = element_rect(fill = "#cde0f4", color = NA) # Pale blue ocean background
  )
map_CN
ggsave("${LOCAL_ANALYSIS_ROOT}/figure/gam_draft/CN_plot5_netscore_bicovirate.png", plot = map_CN, width = 10, height = 12, bg = "transparent", dpi = 1200, device = grDevices::png)
