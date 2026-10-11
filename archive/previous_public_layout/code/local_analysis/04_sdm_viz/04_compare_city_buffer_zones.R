# ARCHIVE: historical reference; symbolic paths are intentionally not runnable.
# Original SHA256: 475ff9fa1eb893a80575da096c9769bbb93c9d7b8c61edf591e275c4588d70ba
library(terra)
library(sf)
library(ggplot2)
library(patchwork)
library(viridis)
library(scales)
library(ggnewscale)
library(rnaturalearth)
install.packages('rnaturalearth')
# 1. Save list of all installed packages to a file
packages <- installed.packages()[,"Package"]
packages
packages_to_install <- readRDS("${PRIVATE_EXTERNAL_PATH}")

# Find packages not yet installed in new R
newly_installed <- installed.packages()[,"Package"]
for(p in packages_to_install){
  if(!p %in% newly_installed){
    install.packages(p)
  }
}
saveRDS(packages, "${PRIVATE_EXTERNAL_PATH}") 
# Save file to a location you can find
my_lib_path <- "${PRIVATE_EXTERNAL_PATH}"
.libPaths()
.libPaths("${PRIVATE_EXTERNAL_PATH}")
install.packages(c("rlang", "ggplot2", "patchwork", "scales", "tibble", "dplyr"), 

                 dependencies = TRUE)
R.version.string
# 1) Read invasion pressure raster (your IRI / pressure surface)
# Assume it's already in equal-area projection or meter units; if in lat/lon, project first (explained below)
press <- rast("${LOCAL_ANALYSIS_ROOT}/data/data/hotspot_mean_suitability_normalized2.0.tif")

# 2) Read city boundaries (108 city polygons)
cities <- vect("${LOCAL_ANALYSIS_ROOT}/data/data/108finalCities.shp")  # Must have a city ID field, e.g., "city_id" or "name"
cities_sf <- st_read("${LOCAL_ANALYSIS_ROOT}/data/data/108finalCities.shp")
# Check if geometry is valid (TRUE/FALSE)
is_ok <- is.valid(cities)
table(is_ok, useNA = "ifany")

# See which cities have problems (if you have city_id field)
bad <- cities[!is_ok, ]
bad
cities_v <- makeValid(cities)
# Ensure CRS consistency
press_m  <- project(press, "EPSG:6933")
cities_m <- project(cities_v, "EPSG:6933")

cities_m$shapeName
# 3) Set buffer distance (unit: meters; 50 km = 50000)
buf_m <- 50000
# 4) Calculate average pressure within cities
city_stat <- extract(press_m, cities_m, fun=mean, na.rm=TRUE)
# Method 1: Use buffer outer area as background
# Create buffer
cities_buf <- buffer(cities_m, width=buf_m)

# Calculate pressure within buffer (including city)
buffer_stat <- extract(press_m, cities_buf, fun=mean, na.rm=TRUE)

# Method 2 (recommended): Use ring area (buffer - city) as pure background
# Calculate ring area pressure
cities_buf_ring <- erase(cities_buf, cities_m)  # Ring = buffer - city interior
background_stat <- extract(press_m, cities_buf_ring, fun=mean, na.rm=TRUE)

# ============================================================
# Part 2: Integrate data frame
# ============================================================

# Assume your city data has a city ID field (e.g., "city_id" or "name")
# Here using the first field of cities_m as example
city_ids <- values(cities_m)[, 1]  # Adjust according to actual field name
city_ids
# Create analysis data frame
df_analysis <- data.frame(
  city_id = city_ids,
  
  # City-specific pressure (within city)
  city_pressure = city_stat[, 2],  # Column 2 of extract() is the statistic value
  
  # Background pressure (ring area)
  background_pressure = background_stat[, 2],
  
  # You need to add reclamation net effect data
  # Here assume you have a reclamation data frame, need to merge
  # reclamation_effect = ...  # Obtain from your reclamation analysis
  
  stringsAsFactors = FALSE
)
df_analysis
# ============================================================
# Part 3: Calculate ΔPressure (key metric)
# ============================================================
city_pressure = city_stat[, 2]
  background_pressure = background_stat[, 2]
  
df_delta <- data.frame(
  city_id = city_ids,
  city_pressure = city_stat[, 2],
  background_pressure = background_stat[, 2],
  
  # ⭐ Core metric: city deviation from background
  delta_pressure = city_pressure - background_pressure,
  
  # Relative deviation (normalized version)
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
#     delta_pressure = delta_pressure.Abutilon.grandifolium_PA1_RUN1_GLM,  # Use correct column
#     relative_deviation = relative_deviation.Abutilon.grandifolium_PA1_RUN1_GLM
#   ) %>%
  mutate(
    # Rename city names (capitalize first letter)
    City = tools::toTitleCase(tolower(city_id)),
    
    # Calculate city role
    City_Role = case_when(
      delta_pressure > 0.1 ~ "Amplifier",
      delta_pressure < -0.1 ~ "Sink",
      TRUE ~ "Neutral"
    ),
    
    # Round to appropriate decimal places
    City_Pressure = round(city_pressure, 3),
    Background_Pressure = round(background_pressure, 3),
    Delta_Pressure = round(delta_pressure, 3),
    Relative_Deviation_Pct = round(relative_deviation * 100, 1)  # Convert to percentage
  ) %>%
  select(
    City,
    City_Pressure,
    Background_Pressure,
    Delta_Pressure,
    Relative_Deviation_Pct,
    City_Role
  ) %>%
  arrange(desc(Delta_Pressure))  # Sort by Delta descending

  
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
    # Add ranking
    Rank_Delta = dense_rank(desc(Delta_Pressure)),
    
    # Add explanatory labels
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
write.csv(df_supplement, "${LOCAL_ANALYSIS_ROOT}/data/organized_table/0701_50kmbufferclassified.csv", row.names = FALSE)
write.csv(summary_table,"${LOCAL_ANALYSIS_ROOT}/data/organized_table/0702_50kmbuffersummary.csv", row.names = FALSE)
# ============================================================
# Method 7: Create three-column grouped table (compact display)
# ============================================================

# Divide into three groups by city role
amplifiers_list <- df_clean %>% 
  filter(City_Role == "Amplifier") %>% 
  pull(City)

neutral_list <- df_clean %>% 
  filter(City_Role == "Neutral") %>% 
  pull(City)

sinks_list <- df_clean %>% 
  filter(City_Role == "Sink") %>% 
  pull(City)

# Create grouped summary
role_summary <- data.frame(
  Category = c("Amplifiers", "Neutral", "Sinks"),
  Count = c(length(amplifiers_list), 
            length(neutral_list), 
            length(sinks_list)),
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
  scale_fill_manual(values = c("Amplifier" = "#E41A1C", 
                                "Neutral" = "#999999",
                                "Sink/Filter" = "#377EB8")) +
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
    # City role subdivision
    Role_Detail = case_when(
      Delta_Pressure > 0.2 ~ "Strong Amplifier",
      Delta_Pressure > 0.1 ~ "Moderate Amplifier",
       Delta_Pressure > 0 ~ "Weak Amplifier",
      Delta_Pressure > -0.1 ~ "Neutral"
    ),
    
    # Ratio relative to background
    Pressure_Ratio = City_Pressure / (Background_Pressure + 1e-6),
    
    # Whether it's a hotspot within hotspot
    Hotspot_Type = case_when(
     Background_Pressure > 0.3 & Delta_Pressure > 0.1 ~ "Hotspot Amplifier",
    Background_Pressure > 0.3 & Delta_Pressure <= 0.1 ~ "Embedded Hotspot",
      Background_Pressure<= 0.3 & Delta_Pressure > 0.1 ~ "Isolated Hotspot",
      TRUE ~ "Low Background"
    )
  )
write.csv(city_results,"${LOCAL_ANALYSIS_ROOT}/data/organized_table/0703_city_results.csv", row.names = FALSE)
cities_sf_match <- cities_sf %>%
  mutate(
    # Create a new column specifically for matching, without breaking the original shapeName (as the original may be used for labels)
    join_key = tolower(shapeName) 
  )
city_results_match <- city_results %>%
  mutate(
    # Process City column similarly
    join_key = tolower(City)
  )
cities_final <- cities_sf_match %>%
  left_join(city_results_match, by = "join_key")
na_count <- sum(is.na(cities_final$Delta_Pressure))
na_count
# ============================================================
# Main plot option 1: Global map + city deviation (strongly recommended)
# ============================================================

# Convert raster to data.frame for plotting
press_df <- as.data.frame(press, xy = TRUE)
colnames(press_df) <- c("x", "y", "pressure")

col_fun <- colorRampPalette(c("white", "#4CAF50", "#FFD700", "#FF8C00", "#B22222"))
my_colors <- col_fun(100) # Generate 100 transition colors

# -------------------------------------------------------------------------
# 2. Prepare data (ensure previously cleaned)
# -------------------------------------------------------------------------
# Ensure city data has no null values and extracted lat/lon (to avoid previous errors)
cities_clean <- cities_final %>%
  filter(!is.na(Delta_Pressure))
names(cities_final)
# 1. Calculate polygon centroid (Centroid)
# This step converts each city's shape to a center point, warning messages can be ignored
sf_use_s2(FALSE)
cities_centroids <- st_centroid(cities_clean)

# 2. Extract coordinates from center points
# Now city_coords rows will match cities_clean (both 59 rows)
city_coords <- st_coordinates(cities_centroids)

# 3. Assign values
cities_clean$lon <- city_coords[, 1]
cities_clean$lat <- city_coords[, 2]

# Get world boundaries
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
world_map <- map_data("world")
# Main plot
p_main <- ggplot() +
  # # 1) Global invasion pressure basemap (background)
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
  
  # 2) World boundaries
  geom_sf(data = world, fill = NA, color = "gray50", linewidth = 0.5) +
  
new_scale_fill() +  # Enable new fill scale to avoid conflict with basemap colors
  
  geom_point(data = cities_clean, 
             aes(x = lon, y = lat, 
                 fill = Delta_Pressure, 
                 size = abs(Delta_Pressure)),
             shape = 21,       # Hollow circle
             color = "black",  # Black border
             stroke = 0.4,     # Thinner border for more refinement
             alpha = 1) +      # City points opaque, highlight display
  
  # City point color scale (red-blue scheme)
  scale_fill_gradient2(
    low = "#2166AC",      # Blue Sink
    mid = "#F7F7F7",      # White Neutral
    high = "#B2182B",     # Red Amplifier
    midpoint = 0,
    name = "ΔPressure\n(City - Background)",
    limits = c(-0.3, 0.5), # Adjust according to your data range
    guide = guide_colorbar(
      title.position = "top", 
      barwidth = 15, 
      barheight = 0.5, 
      order = 2
    )
  ) +
  
  # City point size scale
  scale_size_continuous(
    range = c(1, 7),      # Adjust bubble size
    name = "|ΔPressure|",
    guide = guide_legend(order = 3, nrow = 1)
  ) +
  # 4) Theme
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
 # Plot area background (area enclosed by axes) -> transparent
    panel.background = element_rect(fill = "transparent", color = NA), 
    panel.grid = element_blank(),       # Remove all grid lines
    panel.grid.major = element_blank(), # Double insurance
    panel.grid.minor = element_blank(), # Double insurance
    # Entire plot background (including area around title and legend) -> transparent
    plot.background = element_rect(fill = "transparent", color = NA), 
    
    # Legend background -> transparent (prevent legend with white box)
    legend.background = element_rect(fill = "transparent"), 
    
    # Legend box background -> transparent
    legend.box.background = element_rect(fill = "transparent")
  )

print(p_main)
ggsave("${LOCAL_ANALYSIS_ROOT}/figure/study_area_draft/city_bufferonly.png", bg ='transparent' ,p_main, width = 10, height = 8.5, dpi = 1200,device = grDevices::png)

# ============================================================
# Statistical testing: Answer key scientific questions
# ============================================================

# Question 1: Do cities systematically deviate from background?
t_test_deviation <- t.test(city_results$Delta_Pressure, mu = 0)
cat("\n=== Question 1: Do cities systematically deviate from background? ===\n")
cat("Mean ΔPressure:", round(mean(city_results$Delta_Pressure), 4), "\n")
cat("t-test p-value:", format.pval(t_test_deviation$p.value), "\n")
cat("Conclusion:", ifelse(t_test_deviation$p.value < 0.05, 
                  "Yes, cities are systematically higher than background", 
                  "No, cities show no significant difference from background"), "\n\n")
print(t_test_deviation)
# Question 2: Do cities in different background levels show different deviations?
kruskal_test <- kruskal.test(Delta_Pressure ~ BG_Group, data = city_results_grouped)
print(kruskal_test)
cat("=== Question 2: Do cities in high/medium/low background regions differ in deviation? ===\n")
cat("Kruskal-Wallis p-value:", format.pval(kruskal_test$p.value), "\n")

# Post-hoc test
pairwise_result <- pairwise.wilcox.test(
  city_results_grouped$Delta_Pressure, 
  city_results_grouped$BG_Group,
  p.adjust.method = "bonferroni"
)
cat("Post-hoc test (Bonferroni correction):\n")
print(pairwise_result$p.value)

# Question 3: Can background pressure predict city deviation?
correlation_test <- cor.test(city_results$Background_Pressure, city_results$Delta_Pressure)
cat("\n=== Question 3: Correlation between background pressure and city deviation ===\n")
cat("Pearson r:", round(correlation_test$estimate, 3), "\n")
cat("p-value:", format.pval(correlation_test$p.value), "\n")

# Linear regression
lm_model <- lm(Delta_Pressure ~ Background_Pressure, data = city_results)
cat("\nLinear regression results:\n")
print(summary(lm_model))
results_summary <- city_results %>%
  summarise(
    N_cities = n(),
    N_amplifiers = sum( Delta_Pressure > 0.1),
    Pct_amplifiers = round(100 * N_amplifiers / N_cities, 1),
    N_strong_amplifiers = sum( Delta_Pressure > 0.2),
    
    Mean_DeltaP = round(mean( Delta_Pressure), 4),
    Median_DeltaP = round(median( Delta_Pressure), 4),
    SD_DeltaP = round(sd( Delta_Pressure), 4),
    SD_background = round(sd( Background_Pressure), 4),
    Mean_city_pressure = round(mean(City_Pressure), 3),
    Mean_bg_pressure = round(mean(Background_Pressure), 3),
    
    # Most extreme examples
    Max_amplifier = City[which.max(Delta_Pressure)],
    Max_amplifier_delta = round(max(Delta_Pressure), 3),
    
    Max_sink = City[which.min(Delta_Pressure)],
    Max_sink_delta = round(min(Delta_Pressure), 3)
  )

print("\n=== Key statistics for Results ===")
print(results_summary)

# ============================================================
# Main plot option 3: Grouped boxplot (clearly show role differences)
# ============================================================

# Divide background pressure into three groups
city_results_grouped <- city_results %>%
  mutate(
    BG_Group = cut(
      Background_Pressure,
      breaks = quantile(Background_Pressure, probs = c(0, 0.33, 0.67, 1)),
      labels = c("Low\nBackground", "Medium\nBackground", "High\nBackground"),
      include.lowest = TRUE
    )
  )

p_grouped <- ggplot(city_results_grouped, 
                    aes(x = BG_Group, y = Delta_Pressure, fill = BG_Group)) +
  
  # Boxplot
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
  
  # Add jitter points
  geom_jitter(width = 0.2, alpha = 0.4, size = 2) +
  
  # Zero line
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
  
  # Add statistical annotation
  stat_summary(
    fun = median, 
    geom = "text", 
    aes(label = sprintf("Median: %.3f", after_stat(y))),
    vjust = -1, 
    size = 3.5,
    color = "darkred"
  )

print(p_grouped)
#==============sensitive test for buffer zone =========================
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
  cities_v <- buffer(cities_v, 0)  # classic topology clean

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

  out$DeltaP   <- out$P_city - out$P_bg
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
    fun_stat = mean   # Use mean if you want mean
  )
}))
names(res_all)
write.csv(res_all, "${LOCAL_ANALYSIS_ROOT}/data/organized_table/city_vs_background_buffer_sensitivity.csv", row.names = FALSE)
res_all <- read.csv("${LOCAL_ANALYSIS_ROOT}/data/organized_table/city_vs_background_buffer_sensitivity.csv")
wide <- reshape(res_all[, c("shapeName", "buffer_km", "DeltaP")],
                idvar = "shapeName",    # Unique identifier is city name
                timevar = "buffer_km",  # Column headers will be buffer distances
                direction = "wide")

rho_10_50   <- cor(wide$DeltaP.10,  wide$DeltaP.50,  method="spearman", use="complete.obs")
rho_50_100  <- cor(wide$DeltaP.50,  wide$DeltaP.100, method="spearman", use="complete.obs")
rho_10_100  <- cor(wide$DeltaP.10,  wide$DeltaP.100, method="spearman", use="complete.obs")

c(rho_10_50=rho_10_50, rho_50_100=rho_50_100, rho_10_100=rho_10_100)
sign_consistent <- mean(sign(wide$DeltaP.10)==sign(wide$DeltaP.50) &
                        sign(wide$DeltaP.50)==sign(wide$DeltaP.100),
                        na.rm = TRUE)
sign_consistent
# 1. Define a plotting function to avoid code repetition
plot_corr <- function(data, x_col, y_col, x_lab, y_lab, rho_val) {
  ggplot(data, aes_string(x = x_col, y = y_col)) +
    # Add scatter points
    geom_point(alpha = 0.6, color = "#2e63d6", size = 2) +
    # Add fitted line (linear regression)
    geom_smooth(method = "lm", color = "#dd6e70", fill = "#dd6e70", alpha = 0.2) +
    # Add diagonal dashed line (y=x reference line)
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
    # Annotate correlation coefficient on plot
    annotate("text", x = -Inf, y = Inf, 
             label = paste0("Spearman's rho = ", round(rho_val, 2)),
             hjust = -0.2, vjust = 2, size = 4, fontface = "bold") +
    labs(x = x_lab, y = y_lab) +
    theme_bw() +
    theme(panel.background = element_rect(fill = "transparent", color = NA),   # Plot area transparent
  plot.background  = element_rect(fill = "transparent", color = NA),   # Entire plot background transparent
  panel.grid       = element_blank(),   aspect.ratio = 1) # Maintain square aspect ratio
}

# 2. Generate three plots
p1 <- plot_corr(wide, "DeltaP.10", "DeltaP.50", 
                "DeltaP (10 km buffer)", "DeltaP (50 km buffer)", 0.79)

p2 <- plot_corr(wide, "DeltaP.50", "DeltaP.100", 
                "DeltaP (50 km buffer)", "DeltaP (100 km buffer)", 0.93)

p3 <- plot_corr(wide, "DeltaP.10", "DeltaP.100", 
                "DeltaP (10 km buffer)", "DeltaP (100 km buffer)", 0.63)

# 3. Combine plots (requires patchwork package)
# If not installed: install.packages("patchwork")
final_plot <- p1 + p2 + p3 + 
  plot_layout(ncol = 3) +
  plot_annotation(tag_levels = 'a') # Automatically add a, b, c labels

print(final_plot)
ggsave("${LOCAL_ANALYSIS_ROOT}/figure/Fig.S/buffer_sensitive.png", final_plot, width = 10, height = 8.5, dpi = 1200,device = grDevices::png)
# ============================================================
# Visualization: Buffer zone sensitivity analysis
# ============================================================
res_all_final <- res_all %>%
  mutate(
    # Absolute change (always valid)
    AbsDelta = DeltaP,
    
    # Relative change (only valid when bg > 0)
    RelDelta_valid = ifelse(P_bg > 0.001, RelDelta, NA),
    
    # Standardized relative change (capped at reasonable range)
    RelDelta_capped = case_when(
      is.na(RelDelta_valid) ~ NA_real_,
      RelDelta_valid > 1000 ~ 1000,  # Upper limit 1000%
      RelDelta_valid < -100 ~ -100,   # Lower limit -100%
      TRUE ~ RelDelta_valid
    ),
    
    # City role classification (based on absolute difference)
    City_Role = case_when(
      AbsDelta > 0.1 ~ "Strong Amplifier",
      AbsDelta > 0.05 ~ "Moderate Amplifier",
      AbsDelta > -0.05 ~ "Neutral",
      AbsDelta > -0.1 ~ "Moderate Sink",
      TRUE ~ "Strong Sink"
    )
  )

# Plot 1: DeltaP change for each city across different buffer zones (absolute values)
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

# Plot 2: Boxplot comparing different buffer zones
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

# Plot 3: Relative change (show valid values only)
p3 <- res_all_final %>%
  filter(!is.na(RelDelta_capped)) %>%  # Keep valid values only
  ggplot(aes(x = factor(buffer_km), y = RelDelta_capped)) +
  geom_boxplot(fill = "coral", alpha = 0.6, outlier.shape = NA) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_cartesian(ylim = c(-100, 500)) +  # Limit y-axis range
  labs(
    x = "Buffer Distance (km)",
    y = "Relative Deviation (%, capped at ±1000%)",
    title = "Relative City Deviation (valid cases only)",
    subtitle = paste("Excluded", sum(is.na(res_all_final$RelDelta_capped)), 
                     "cases with invalid background pressure")
  ) +
  theme_classic()

print(p3)

# ============================================================
# Statistical analysis: Impact of buffer zone selection
# ============================================================

buffer_sensitivity <- res_all_final %>%
  group_by(buffer_km) %>%
  summarise(
    N_cities = n(),
    N_valid_bg = sum(P_bg > 0.001),
    Pct_valid = round(100 * N_valid_bg / N_cities, 1),
    
    # Absolute difference statistics
    Mean_AbsDelta = round(mean(AbsDelta, na.rm=TRUE), 4),
    Median_AbsDelta = round(median(AbsDelta, na.rm=TRUE), 4),
    SD_AbsDelta = round(sd(AbsDelta, na.rm=TRUE), 4),
    
    # Relative difference statistics (valid values only)
    Mean_RelDelta = round(mean(RelDelta_capped, na.rm=TRUE), 2),
    Median_RelDelta = round(median(RelDelta_capped, na.rm=TRUE), 2),
    
    # City role distribution
    N_Amplifiers = sum(AbsDelta > 0.05),
    N_Sinks = sum(AbsDelta < -0.05),
    N_Neutral = sum(abs(AbsDelta) <= 0.05),
    
    .groups = "drop"
  )

print("\n=== Buffer zone sensitivity statistics ===")
print(buffer_sensitivity)
best_buffer <- buffer_sensitivity %>%
  filter(Pct_valid >= 90) %>%  # At least 90% valid
  slice_min(SD_AbsDelta, n = 1)

cat("Recommended buffer zone:", best_buffer$buffer_km, "km\n")
cat("Rationale: validity rate", best_buffer$Pct_valid, "%, standard deviation", best_buffer$SD_AbsDelta, "\n")