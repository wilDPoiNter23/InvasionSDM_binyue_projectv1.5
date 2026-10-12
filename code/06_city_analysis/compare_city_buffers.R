# City and surrounding-land suitability
# Input: City boundaries and suitability raster
# Output: City-background tables and buffer-sensitivity figures

source(file.path(Sys.getenv("INVASIONSDM_PROJECT_ROOT", "."), "config", "bootstrap.R"))
library(terra)
library(sf)
library(ggplot2)
library(patchwork)
library(viridis)
library(scales)
library(ggnewscale)
library(rnaturalearth)
library(tidyr)
# 1. Read the invasion pressure raster (IRI / pressure surface).
# Use an equal-area or metre-based CRS; reproject geographic coordinates before buffering.
press <- rast(publication_path("MAIN_RESPONSE_RASTER"))

# 2. Read city boundaries (108 city polygons).
cities <- vect(publication_path("CITY_BOUNDARIES_ORIGINAL")) # A city identifier such as city_id or name is required.
cities_sf <- st_read(publication_path("CITY_BOUNDARIES_ORIGINAL"))
# Check geometry validity (TRUE/FALSE).
is_ok <- is.valid(cities)
table(is_ok, useNA = "ifany")

# Identify cities with invalid geometries using city_id, if available.
bad <- cities[!is_ok, ]
bad
cities_v <- makeValid(cities)
# Ensure consistent CRS.
press_m <- project(press, "EPSG:6933")
cities_m <- project(cities_v, "EPSG:6933")

cities_m$shapeName
# 3. Specify buffer distance in metres: 50 km = 50000 m.
buf_m <- 50000
# 4. Calculate mean pressure within each city.
city_stat <- extract(press_m, cities_m, fun = mean, na.rm = TRUE)
# Method 1: use the surrounding buffer region as background.
# Create buffers.
cities_buf <- buffer(cities_m, width = buf_m)

# Calculate pressure within buffers, including cities.
buffer_stat <- extract(press_m, cities_buf, fun = mean, na.rm = TRUE)

# Method 2: use buffer minus city polygons as background rings.
# Calculate pressure within the rings.
cities_buf_ring <- erase(cities_buf, cities_m) # Ring = buffer minus city interior.
background_stat <- extract(press_m, cities_buf_ring, fun = mean, na.rm = TRUE)

# ============================================================
# Part 2: assemble the data frame.
# ============================================================

# Use a city identifier field, such as city_id or name.
# Use the first field of cities_m as an example.
city_ids <- values(cities_m)[, 1] # Adjust to the actual field name.
city_ids
# Create the analysis data frame.
df_analysis <- data.frame(
  city_id = city_ids,

  # City-specific pressure within city boundaries.
  city_pressure = city_stat[, 2], # The second extract() column contains the summary statistic.

  # Background pressure within the surrounding ring.
  background_pressure = background_stat[, 2],

  # Add reclamation net-effect data.
  # Join the reclamation data frame.
  # reclamation_effect = ... ; obtain values from the reclamation analysis.

  stringsAsFactors = FALSE
)
df_analysis
# ============================================================
# Part 3: calculate DeltaPressure.
# ============================================================
city_pressure <- city_stat[, 2]
background_pressure <- background_stat[, 2]

df_delta <- data.frame(
  city_id = city_ids,
  city_pressure = city_stat[, 2],
  background_pressure = background_stat[, 2],

  # City pressure relative to the surrounding background.
  delta_pressure = city_pressure - background_pressure,

  # Standardised relative deviation.
  relative_deviation = (city_pressure - background_pressure) /
    background_pressure
)
df_delta

df_clean <- df_delta %>%
  #  %>%
  #   select(
  #     city_id,
  #     city_pressure,
  #     background_pressure,
  # delta_pressure = delta_pressure.Abutilon.grandifolium_PA1_RUN1_GLM ; select the appropriate column.
  #     relative_deviation = relative_deviation.Abutilon.grandifolium_PA1_RUN1_GLM
  #   ) %>%
  mutate(
    # Capitalise city names.
    City = tools::toTitleCase(tolower(city_id)),

    # Calculate city roles.
    City_Role = case_when(
      delta_pressure > 0.1 ~ "Amplifier",
      delta_pressure < -0.1 ~ "Sink",
      TRUE ~ "Neutral"
    ),

    # Round to suitable decimal precision.
    City_Pressure = round(city_pressure, 3),
    Background_Pressure = round(background_pressure, 3),
    Delta_Pressure = round(delta_pressure, 3),
    Relative_Deviation_Pct = round(relative_deviation * 100, 1) # Convert to percentages.
  ) %>%
  select(
    City,
    City_Pressure,
    Background_Pressure,
    Delta_Pressure,
    Relative_Deviation_Pct,
    City_Role
  ) %>%
  arrange(desc(Delta_Pressure)) # Sort Delta in descending order.


summary_table <- df_clean %>%
  group_by(City_Role) %>%
  summarise(
    N_Cities = n(),
    Mean_City_Pressure = round(mean(City_Pressure), 3),
    Mean_Background = round(mean(Background_Pressure), 3),
    Mean_Delta = round(mean(Delta_Pressure), 3),
    Median_Delta = round(median(Delta_Pressure), 3),
    SD_Delta = round(sd(Delta_Pressure), 3),
    Max_Deviation_Pct = round(max(Relative_Deviation_Pct), 1),
    .groups = "drop"
  ) %>%
  arrange(desc(Mean_Delta))

print(summary_table)

df_supplement <- df_clean %>%
  mutate(
    # Add ranks.
    Rank_Delta = dense_rank(desc(Delta_Pressure)),

    # Add explanatory labels.
    Deviation_Category = case_when(
      Relative_Deviation_Pct > 200 ~ "Very High (>200%)",
      Relative_Deviation_Pct > 100 ~ "High (100-200%)",
      Relative_Deviation_Pct > 50 ~ "Moderate (50-100%)",
      Relative_Deviation_Pct > 0 ~ "Low (0-50%)",
      TRUE ~ "Negative"
    )
  ) %>%
  select(
    Rank_Delta,
    City,
    City_Pressure,
    Background_Pressure,
    Delta_Pressure,
    Relative_Deviation_Pct,
    City_Role,
    Deviation_Category
  )

df_supplement
write.csv(df_supplement, publication_path("LOCAL_ANALYSIS_ROOT", "data/organized_table/0701_50kmbufferclassified.csv"), row.names = FALSE)
write.csv(summary_table, publication_path("LOCAL_ANALYSIS_ROOT", "data/organized_table/0702_50kmbuffersummary.csv"), row.names = FALSE)
# ============================================================
# Method 7: create a compact three-column grouped table.
# ============================================================

# Divide cities into three groups by role.
amplifiers_list <- df_clean %>%
  filter(City_Role == "Amplifier") %>%
  pull(City)

neutral_list <- df_clean %>%
  filter(City_Role == "Neutral") %>%
  pull(City)

sinks_list <- df_clean %>%
  filter(City_Role == "Sink") %>%
  pull(City)

# Create grouped summaries.
role_summary <- data.frame(
  Category = c("Amplifiers", "Neutral", "Sinks"),
  Count = c(
    length(amplifiers_list),
    length(neutral_list),
    length(sinks_list)
  ),
  Percentage = round(c(
    length(amplifiers_list) / nrow(df_clean) * 100,
    length(neutral_list) / nrow(df_clean) * 100,
    length(sinks_list) / nrow(df_clean) * 100
  ), 1),
  Example_Cities = c(
    paste(head(amplifiers_list, 5), collapse = ", "),
    paste(head(neutral_list, 5), collapse = ", "),
    paste(head(sinks_list, 5), collapse = ", ")
  )
)

print(role_summary)


p_delta <- ggplot(df_clean, aes(x = Delta_Pressure, fill = City_Role)) +
  geom_histogram(bins = 30, color = "white", alpha = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1) +
  scale_fill_manual(values = c(
    "Amplifier" = "#E41A1C",
    "Neutral" = "#999999",
    "Sink/Filter" = "#377EB8"
  )) +
  labs(
    x = "ΔPressure (City − Background)",
    y = "Number of Cities",
    title = "City-level Deviation from Coastal Baseline",
    subtitle = "Positive values indicate invasion amplification",
    fill = "City Role"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "top"
  )

print(p_delta)
names(df_clean)
city_results <- df_clean %>%
  select(City, City_Pressure, Background_Pressure, Delta_Pressure, City_Role) %>%
  mutate(
    # Subdivide city roles.
    Role_Detail = case_when(
      Delta_Pressure > 0.2 ~ "Strong Amplifier",
      Delta_Pressure > 0.1 ~ "Moderate Amplifier",
      Delta_Pressure > 0 ~ "Weak Amplifier",
      Delta_Pressure > -0.1 ~ "Neutral"
    ),

    # Ratio relative to background.
    Pressure_Ratio = City_Pressure / (Background_Pressure + 1e-6),

    # Identify elevated city pressure within high-background areas.
    Hotspot_Type = case_when(
      Background_Pressure > 0.3 & Delta_Pressure > 0.1 ~ "Hotspot Amplifier",
      Background_Pressure > 0.3 & Delta_Pressure <= 0.1 ~ "Embedded Hotspot",
      Background_Pressure <= 0.3 & Delta_Pressure > 0.1 ~ "Isolated Hotspot",
      TRUE ~ "Low Background"
    )
  )
write.csv(city_results, publication_path("LOCAL_ANALYSIS_ROOT", "data/organized_table/0703_city_results.csv"), row.names = FALSE)
city_results <- read.csv(publication_path("LOCAL_ANALYSIS_ROOT", "data/organized_table/0703_city_results.csv"))
cities_sf_match <- cities_sf %>%
  mutate(
    # Create a matching column while preserving shapeName for display labels.
    join_key = tolower(shapeName)
  )
city_results_match <- city_results %>%
  mutate(
    # Apply the same transformation to City.
    join_key = tolower(City)
  )
cities_final <- cities_sf_match %>%
  left_join(city_results_match, by = "join_key")
na_count <- sum(is.na(cities_final$Delta_Pressure))
na_count
# ============================================================
# Main figure option 1: global map with city deviations.
# ============================================================

# Convert the raster to a data.frame for plotting.
press_df <- as.data.frame(press, xy = TRUE)
colnames(press_df) <- c("x", "y", "pressure")

col_fun <- colorRampPalette(c("white", "#4CAF50", "#FFD700", "#FF8C00", "#B22222"))
my_colors <- col_fun(100) # Create 100 interpolated colours.

# -------------------------------------------------------------------------
# 2. Prepare the cleaned data.
# -------------------------------------------------------------------------
# Remove missing city records and extract longitude and latitude.
cities_clean <- cities_final %>%
  filter(!is.na(Delta_Pressure))
names(cities_final)
# 1. Calculate polygon centroids.
# Represent each city polygon by its centroid.
sf_use_s2(FALSE)
cities_centroids <- st_centroid(cities_clean)

# 2. Extract centroid coordinates.
# city_coords and cities_clean should have the same number of rows (59).
city_coords <- st_coordinates(cities_centroids)

# 3. Assign coordinates.
cities_clean$lon <- city_coords[, 1]
cities_clean$lat <- city_coords[, 2]

# Obtain world boundaries.
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
world_map <- map_data("world")
# Main plot.
p_main <- ggplot() +
  # 1. Global invasion pressure raster as background.
  # geom_raster(data = press_df,
  #             aes(x = x, y = y, fill = pressure),
  #             alpha = 0.8) +
  # scale_fill_gradientn(
  #  colors = my_colors,
  #   name = "Global Invasion\nPressure",
  #   na.value = "transparent",
  #   limits = c(0, 0.7),
  #   guide = guide_colorbar(
  #     title.position = "top",
  #     barwidth = 15,
  #     barheight = 0.5,
  #     order = 1
  #   )
  # ) +

  # 2. World boundaries.
  geom_sf(data = world, fill = NA, color = "gray50", linewidth = 0.5) +
  new_scale_fill() + # Start a separate fill scale for the city symbols.

  geom_point(
    data = cities_clean,
    aes(
      x = lon, y = lat,
      fill = Delta_Pressure,
      size = abs(Delta_Pressure)
    ),
    shape = 21, # Open circles.
    color = "black", # Black borders.
    stroke = 0.4, # Thin symbol borders.
    alpha = 1
  ) + # Opaque city symbols for visibility.

  # Red-blue colour scale for city points.
  scale_fill_gradient2(
    low = "#2166AC", # Blue: Sink.
    mid = "#F7F7F7", # White: Neutral.
    high = "#B2182B", # Red: Amplifier.
    midpoint = 0,
    name = "ΔPressure\n(City - Background)",
    limits = c(-0.3, 0.5), # Adjust to the data range.
    guide = guide_colorbar(
      title.position = "top",
      barwidth = 15,
      barheight = 0.5,
      order = 2
    )
  ) +

  # Size scale for city points.
  scale_size_continuous(
    range = c(1, 7), # Adjust bubble sizes.
    name = "|ΔPressure|",
    guide = guide_legend(order = 3, nrow = 1)
  ) +
  # 4. Theme.
  coord_sf(expand = FALSE) +
  labs(
    title = "Global Invasion Pressure and City-level Deviations",
    subtitle = "Cities embedded in varying invasion backgrounds reshape local pressure patterns",
    caption = "Point color: deviation direction | Point size: deviation magnitude"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    legend.position = "bottom",
    legend.box = "horizontal",
    # Transparent panel background.
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.grid = element_blank(), # Remove all grid lines.
    panel.grid.major = element_blank(), # Apply an additional formatting check.
    panel.grid.minor = element_blank(), # Apply an additional formatting check.
    # Transparent plot background, including title and legend areas.
    plot.background = element_rect(fill = "transparent", color = NA),

    # Transparent legend background.
    legend.background = element_rect(fill = "transparent"),

    # Transparent legend border background.
    legend.box.background = element_rect(fill = "transparent")
  )

print(p_main)
ggsave(publication_path("OUTPUT_ROOT", "figures/study_area_draft/city_bufferonly.png"), bg = "transparent", p_main, width = 10, height = 8.5, dpi = 1200, device = grDevices::png)

# ============================================================
# Statistical tests of the scientific questions.
# ============================================================

# Question 1: do cities systematically deviate from their backgrounds?
t_test_deviation <- t.test(city_results$Delta_Pressure, mu = 0)
cat("\nCity and background comparison\n")

cat("Mean pressure difference:", round(mean(city_results$Delta_Pressure), 4), "\n")
cat("t-test p-value:", format.pval(t_test_deviation$p.value), "\n")
cat("Result:", ifelse(t_test_deviation$p.value < 0.05,
  "Cities have higher predicted suitability than their surroundings",
  "No significant city-background difference"
), "\n\n")
print(t_test_deviation)
# Question 2: do city deviations differ among background pressure levels?
kruskal_test <- kruskal.test(Delta_Pressure ~ BG_Group, data = city_results_grouped)
print(kruskal_test)
cat("City differences across background-suitability groups\n")
cat("Kruskal-Wallis p-value:", format.pval(kruskal_test$p.value), "\n")

# Post hoc tests.
pairwise_result <- pairwise.wilcox.test(
  city_results_grouped$Delta_Pressure,
  city_results_grouped$BG_Group,
  p.adjust.method = "bonferroni"
)
cat("Post-hoc tests with Bonferroni adjustment:\n")
print(pairwise_result$p.value)

# Question 3: does background pressure predict city deviation?
correlation_test <- cor.test(city_results$Background_Pressure, city_results$Delta_Pressure)
cat("\nBackground suitability and city-background differences\n")
cat("Pearson r:", round(correlation_test$estimate, 3), "\n")
cat("p-value:", format.pval(correlation_test$p.value), "\n")

# Linear regression.
lm_model <- lm(Delta_Pressure ~ Background_Pressure, data = city_results)
cat("\nLinear regression:\n")
print(summary(lm_model))
results_summary <- city_results %>%
  summarise(
    N_cities = n(),
    N_amplifiers = sum(Delta_Pressure > 0.1),
    Pct_amplifiers = round(100 * N_amplifiers / N_cities, 1),
    N_strong_amplifiers = sum(Delta_Pressure > 0.2),
    Mean_DeltaP = round(mean(Delta_Pressure), 4),
    Median_DeltaP = round(median(Delta_Pressure), 4),
    SD_DeltaP = round(sd(Delta_Pressure), 4),
    SD_background = round(sd(Background_Pressure), 4),
    Mean_city_pressure = round(mean(City_Pressure), 3),
    Mean_bg_pressure = round(mean(Background_Pressure), 3),

    # Identify the most extreme examples.
    Max_amplifier = City[which.max(Delta_Pressure)],
    Max_amplifier_delta = round(max(Delta_Pressure), 3),
    Max_sink = City[which.min(Delta_Pressure)],
    Max_sink_delta = round(min(Delta_Pressure), 3)
  )

print("\nSummary statistics")
print(results_summary)

# ============================================================
# Main figure option 3: grouped boxplots comparing city roles.
# ============================================================

# Divide background pressure into three groups.
city_results_grouped <- city_results %>%
  mutate(
    BG_Group = cut(
      Background_Pressure,
      breaks = quantile(Background_Pressure, probs = c(0, 0.33, 0.67, 1)),
      labels = c("Low\nBackground", "Medium\nBackground", "High\nBackground"),
      include.lowest = TRUE
    )
  )

p_grouped <- ggplot(
  city_results_grouped,
  aes(x = BG_Group, y = Delta_Pressure, fill = BG_Group)
) +

  # Boxplots.
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +

  # Add jittered observations.
  geom_jitter(width = 0.2, alpha = 0.4, size = 2) +

  # Zero line.
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  scale_fill_manual(
    values = c("#E8F4F8", "#A1D5E8", "#4A90A4")
  ) +
  labs(
    x = "Background Invasion Pressure Level",
    y = "ΔPressure (City - Background)",
    title = "Do Cities Amplify More in High vs Low Background Regions?",
    subtitle = "Positive values indicate cities amplify invasion pressure beyond regional baseline"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "none",
    axis.text.x = element_text(size = 11)
  ) +

  # Add statistical annotations.
  stat_summary(
    fun = median,
    geom = "text",
    aes(label = sprintf("Median: %.3f", after_stat(y))),
    vjust = -1,
    size = 3.5,
    color = "darkred"
  )

print(p_grouped)
# ==============sensitive test for buffer zone =========================
calc_city_bg <- function(press_m, cities_m, r_km = 50,
                         id_field = "shapeName",
                         fun_stat = mean,
                         na_rm = TRUE) {
  # --- CRS check (avoid extract warnings) ---
  if (!same.crs(press_m, cities_m)) {
    cities_m <- project(cities_m, crs(press_m))
  }

  # --- geometry cleanup to avoid TopologyException ---
  cities_v <- makeValid(cities_m)
  cities_v <- buffer(cities_v, 0) # classic topology clean

  # 1) City pressure (within city polygon)
  city_stat <- extract(press_m, cities_v, fun = fun_stat, na.rm = na_rm)
  colnames(city_stat)[2] <- "P_city"

  # 2) Buffer polygon (includes city)
  r_m <- r_km * 1000
  cities_buf <- buffer(cities_v, width = r_m)
  cities_buf <- makeValid(cities_buf)
  cities_buf <- buffer(cities_buf, 0)

  buffer_stat <- extract(press_m, cities_buf, fun = fun_stat, na.rm = na_rm)
  colnames(buffer_stat)[2] <- "P_buffer"

  # 3) Ring polygon = buffer - city (pure background)
  cities_ring <- erase(cities_buf, cities_v)
  cities_ring <- makeValid(cities_ring)
  cities_ring <- buffer(cities_ring, 0)

  bg_stat <- extract(press_m, cities_ring, fun = fun_stat, na.rm = na_rm)
  colnames(bg_stat)[2] <- "P_bg"

  # 4) Assemble output
  out <- data.frame(
    buffer_km = r_km,
    city_id   = cities_v[[id_field]],
    P_city    = city_stat$P_city,
    P_buffer  = buffer_stat$P_buffer,
    P_bg      = bg_stat$P_bg
  )

  out$DeltaP <- out$P_city - out$P_bg
  out$RelDelta <- out$DeltaP / out$P_bg

  out
}

buffers <- c(10, 50, 100)

res_all <- do.call(rbind, lapply(buffers, function(bk) {
  calc_city_bg(
    press_m  = press_m,
    cities_m = cities_m,
    r_km     = bk,
    id_field = "shapeName",
    fun_stat = mean # Use mean if a mean summary is required.
  )
}))
names(res_all)
write.csv(res_all, publication_path("OUTPUT_ROOT", "tables/city_vs_background_buffer_sensitivity.csv"), row.names = FALSE)
wide <- reshape(res_all[, c("shapeName", "buffer_km", "DeltaP")],
  idvar = "shapeName", # City name is the unique identifier.
  timevar = "buffer_km", # Use buffer distances as column headings.
  direction = "wide"
)

rho_10_50 <- cor(wide$DeltaP.10, wide$DeltaP.50, method = "spearman", use = "complete.obs")
rho_50_100 <- cor(wide$DeltaP.50, wide$DeltaP.100, method = "spearman", use = "complete.obs")
rho_10_100 <- cor(wide$DeltaP.10, wide$DeltaP.100, method = "spearman", use = "complete.obs")

c(rho_10_50 = rho_10_50, rho_50_100 = rho_50_100, rho_10_100 = rho_10_100)
sign_consistent <- mean(
  sign(wide$DeltaP.10) == sign(wide$DeltaP.50) &
    sign(wide$DeltaP.50) == sign(wide$DeltaP.100),
  na.rm = TRUE
)
sign_consistent
# 1. Define a reusable plotting function.
plot_corr <- function(data, x_col, y_col, x_lab, y_lab, rho_val) {
  ggplot(data, aes_string(x = x_col, y = y_col)) +
    # Add scatter points.
    geom_point(alpha = 0.6, color = "#2e63d6", size = 2) +
    # Add a fitted linear regression line.
    geom_smooth(method = "lm", color = "#dd6e70", fill = "#dd6e70", alpha = 0.2) +
    # Add a dashed y = x reference line.
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
    # Annotate the correlation coefficient.
    annotate("text",
      x = -Inf, y = Inf,
      label = paste0("Spearman's rho = ", round(rho_val, 2)),
      hjust = -0.2, vjust = 2, size = 4, fontface = "bold"
    ) +
    labs(x = x_lab, y = y_lab) +
    theme_bw() +
    theme(
      panel.background = element_rect(fill = "transparent", color = NA), # Transparent panel background.
      plot.background = element_rect(fill = "transparent", color = NA), # Transparent overall plot background.
      panel.grid = element_blank(), aspect.ratio = 1
    ) # Maintain a square aspect ratio.
}

# 2. Create three plots.
p1 <- plot_corr(
  wide, "DeltaP.10", "DeltaP.50",
  "DeltaP (10 km buffer)", "DeltaP (50 km buffer)", 0.79
)

p2 <- plot_corr(
  wide, "DeltaP.50", "DeltaP.100",
  "DeltaP (50 km buffer)", "DeltaP (100 km buffer)", 0.93
)

p3 <- plot_corr(
  wide, "DeltaP.10", "DeltaP.100",
  "DeltaP (10 km buffer)", "DeltaP (100 km buffer)", 0.63
)

# 3. Combine plots using patchwork.
# Install patchwork if required: install.packages("patchwork").
final_plot <- p1 + p2 + p3 +
  plot_layout(ncol = 3) +
  plot_annotation(tag_levels = "a") # Add panel labels a, b and c automatically.
p_d <- ggplot(res_all, aes(x = buffer_km_factor, y = DeltaP)) +
  geom_boxplot(fill = "#A0CBE8", alpha = 0.8, outlier.shape = 21, outlier.fill = "white", outlier.size = 1.5, width = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#d73027", linewidth = 0.8) +
  labs(
    x = "Buffer Radius",
    y = expression(paste(Delta, "Pressure (City - Background)"))
  ) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA)
  )
final_plot <- (p1 + p2 + p3) / p_d +
  plot_layout(heights = c(1, 1)) +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(size = 18, face = "bold"))

final_plot
print(final_plot)
ggsave(publication_path("OUTPUT_ROOT", "figures/Fig.S/buffer_sensitive.jpg"), final_plot, width = 10, height = 8.5, dpi = 1200)
# ============================================================
# Visualise buffer sensitivity.
# ============================================================
res_all_final <- res_all %>%
  mutate(
    # Absolute change, defined for all background values.
    AbsDelta = DeltaP,

    # Relative change, defined only when bg > 0.
    RelDelta_valid = ifelse(P_bg > 0.001, RelDelta, NA),

    # Cap the standardised relative change within specified bounds.
    RelDelta_capped = case_when(
      is.na(RelDelta_valid) ~ NA_real_,
      RelDelta_valid > 1000 ~ 1000, # Upper bound: 1000%.
      RelDelta_valid < -100 ~ -100, # Lower bound: -100%.
      TRUE ~ RelDelta_valid
    ),

    # Classify city roles using absolute differences.
    City_Role = case_when(
      AbsDelta > 0.1 ~ "Strong Amplifier",
      AbsDelta > 0.05 ~ "Moderate Amplifier",
      AbsDelta > -0.05 ~ "Neutral",
      AbsDelta > -0.1 ~ "Moderate Sink",
      TRUE ~ "Strong Sink"
    )
  )

# Figure 1: city-level DeltaP across buffer distances (absolute difference).
p1 <- res_all_final %>%
  ggplot(aes(x = factor(buffer_km), y = AbsDelta, group = shapeName)) +
  geom_line(alpha = 0.3, color = "gray50") +
  geom_point(aes(color = City_Role), alpha = 0.6, size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  scale_color_manual(values = c(
    "Strong Amplifier" = "#D73027",
    "Moderate Amplifier" = "#FC8D59",
    "Neutral" = "#999999",
    "Moderate Sink" = "#91BFDB",
    "Strong Sink" = "#4575B4"
  )) +
  labs(
    x = "Buffer Distance (km)",
    y = "ΔPressure (City - Background)",
    title = "Sensitivity of City-Background Deviation to Buffer Size",
    color = "City Role"
  ) +
  theme_classic() +
  theme(legend.position = "bottom")

print(p1)

# Figure 2: boxplots comparing buffer distances.
p2 <- res_all_final %>%
  ggplot(aes(x = factor(buffer_km), y = AbsDelta)) +
  geom_boxplot(fill = "steelblue", alpha = 0.6, outlier.alpha = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    x = "Buffer Distance (km)",
    y = "ΔPressure (City - Background)",
    title = "Distribution of City Deviation Across Buffer Sizes"
  ) +
  theme_classic()

print(p2)

# Figure 3: relative changes using valid values only.
p3 <- res_all_final %>%
  filter(!is.na(RelDelta_capped)) %>% # Retain valid values only.
  ggplot(aes(x = factor(buffer_km), y = RelDelta_capped)) +
  geom_boxplot(fill = "coral", alpha = 0.6, outlier.shape = NA) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_cartesian(ylim = c(-100, 500)) + # Restrict the y-axis range.
  labs(
    x = "Buffer Distance (km)",
    y = "Relative Deviation (%, capped at ±1000%)",
    title = "Relative City Deviation (valid cases only)",
    subtitle = paste(
      "Excluded", sum(is.na(res_all_final$RelDelta_capped)),
      "cases with invalid background pressure"
    )
  ) +
  theme_classic()

print(p3)

# ============================================================
# Analyse sensitivity to buffer selection.
# ============================================================

buffer_sensitivity <- res_all_final %>%
  group_by(buffer_km) %>%
  summarise(
    N_cities = n(),
    N_valid_bg = sum(P_bg > 0.001),
    Pct_valid = round(100 * N_valid_bg / N_cities, 1),

    # Summarise absolute differences.
    Mean_AbsDelta = round(mean(AbsDelta, na.rm = TRUE), 4),
    Median_AbsDelta = round(median(AbsDelta, na.rm = TRUE), 4),
    SD_AbsDelta = round(sd(AbsDelta, na.rm = TRUE), 4),

    # Summarise relative differences using valid values only.
    Mean_RelDelta = round(mean(RelDelta_capped, na.rm = TRUE), 2),
    Median_RelDelta = round(median(RelDelta_capped, na.rm = TRUE), 2),

    # Distribution of city roles.
    N_Amplifiers = sum(AbsDelta > 0.05),
    N_Sinks = sum(AbsDelta < -0.05),
    N_Neutral = sum(abs(AbsDelta) <= 0.05),
    .groups = "drop"
  )

print("\nBuffer sensitivity statistics")
print(buffer_sensitivity)
best_buffer <- buffer_sensitivity %>%
  filter(Pct_valid >= 90) %>% # At least 90% valid values.
  slice_min(SD_AbsDelta, n = 1)

cat("Selected buffer:", best_buffer$buffer_km, "km\n")
cat("Valid coverage:", best_buffer$Pct_valid, "%, standard deviation:", best_buffer$SD_AbsDelta, "\n")
