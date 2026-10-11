# ARCHIVE: historical reference; symbolic paths are intentionally not runnable.
# Original SHA256: 3d2899c3bec52b7dff1bd5e9ea4baeebbeae67e087e899efc6c477d9a7732129
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggtext)
library(dunn.test) # For non-parametric multiple comparisons
library(rstatix)   # For convenient statistical annotations
library(multcompView)
library(stringr)
library(patchwork)
library(ggsci)
library(forcats)
library(openxlsx)
library(tibble)
target_lib <- "${PRIVATE_EXTERNAL_PATH}"
install.packages(c("cli", "rlang", "vctrs", "lifecycle"), lib = target_lib)
install.packages(c("dplyr", "tidyr", "tibble", "pillar"), lib = target_lib)
packageVersion("ggplot2")
install.packages(c("dplyr", "tidyr", "tibble"))
update.packages('ggplot2')
remotes::install_github("nanxstats/ggsci")
df_analysis <- read.csv("${LOCAL_ANALYSIS_ROOT}/data/organized_table/0502_risk_city_netscoreStdVersion.csv",
               stringsAsFactors = FALSE)
head(df)
# ---- 1) Change to your actual column names ----
# Example: Assuming your column names are these; replace the right side with your column names if different
df <- df %>%
  rename(
    City      = City,
    Continent = Continent,     # Or Region
    Direct    = Direct_Effect,
    Indirect  = Indirect_Effect,
    Net       = Total_Effect,
    Pressure  = Pressure,
    Dependency = Dependency_mech
    # If you don't have this column, see next section
  )

# If Dependency doesn't exist, generate it
if(!("Dependency" %in% names(df))){
  df <- df %>% mutate(Dependency = Net / Pressure)
}
median(  df$Pressure )
# ---- 2) Set thresholds for Fig.5b (must match your plot) ----
thr_pressure <- median(df$Pressure)  # Example: replace with your own
thr_dep      <- 0.5  # Example: replace with your own

# ---- 3) Global: direct/indirect/net ----
direct_neg_pct <- 100 * mean(df$Direct < 0, na.rm = TRUE)
direct_med <- median(df$Direct, na.rm = TRUE)
direct_q1  <- quantile(df$Direct, 0.25, na.rm = TRUE)
direct_q3  <- quantile(df$Direct, 0.75, na.rm = TRUE)

ind_pos_pct <- 100 * mean(df$Indirect > 0, na.rm = TRUE)
ind_mag_med <- median(abs(df$Indirect), na.rm = TRUE)
ind_mag_q1  <- quantile(abs(df$Indirect), 0.25, na.rm = TRUE)
ind_mag_q3  <- quantile(abs(df$Indirect), 0.75, na.rm = TRUE)

net_pos_n   <- sum(df$Net > 0, na.rm = TRUE)
net_pos_pct <- 100 * mean(df$Net > 0, na.rm = TRUE)

# ---- 4) Continental comparison ----
cont <- df %>%
  group_by(Continent) %>%
  summarise(
    n = n(),
    med_direct = median(Direct, na.rm = TRUE),
    med_indirect = median(Indirect, na.rm = TRUE),
    med_net = median(Net, na.rm = TRUE),
    net_sum = sum(Net, na.rm = TRUE),
    prop_net_pos = mean(Net > 0, na.rm = TRUE)
  ) %>% ungroup()

asia <- cont %>% filter(grepl("Asia", Continent, ignore.case = TRUE))
others <- cont %>% filter(!grepl("Asia", Continent, ignore.case = TRUE))

# "factor ~X–X": Asia median net / max of other continents' median net (approach 1)
factor1_low <- as.numeric(asia$med_net) / max(others$med_net, na.rm = TRUE)

# Or use net_sum (approach 2)
factor2_low <- as.numeric(asia$net_sum) / max(others$net_sum, na.rm = TRUE)

# Range of other continents' median net (for "clustered near zero")
others_med_net_min <- min(others$med_net, na.rm = TRUE)
others_med_net_max <- max(others$med_net, na.rm = TRUE)

# "fewer than X% strongly positive": define strong positive threshold
thr_strong_net <- quantile(df$Net, 0.75, na.rm = TRUE)
others_strong_pos_pct <- 100 * mean(df$Net > thr_strong_net & !grepl("Asia", df$Continent, ignore.case=TRUE), na.rm = TRUE)

# ---- 5) Dependency framework: four quadrants and key proportions ----
low_dep_pct <- 100 * mean(df$Dependency < thr_dep, na.rm = TRUE)

recl_sensitive_n <- sum(df$Dependency >= thr_dep & df$Pressure < thr_pressure, na.rm = TRUE)

dep_range_sensitive <- df %>%
  filter(Dependency >= thr_dep, Pressure < thr_pressure) %>%
  summarise(
    dep_pct_q1 = quantile(Dependency, 0.25, na.rm = TRUE) * 100,
    dep_pct_q3 = quantile(Dependency, 0.75, na.rm = TRUE) * 100,
    dep_pct_min = min(Dependency, na.rm = TRUE) * 100,
    dep_pct_max = max(Dependency, na.rm = TRUE) * 100
  )

# ---- 6) Shanghai specific analysis ----
sh <- df %>% filter(grepl("^Shanghai$", City, ignore.case = TRUE))

sh_pressure_pctile <- ecdf(df$Pressure)(sh$Pressure)    # 0-1
sh_top_pct <- (1 - sh_pressure_pctile) * 100
sh_dep_pct <- sh$Dependency * 100

hi_hi <- df %>% filter(Pressure >= thr_pressure, Dependency >= thr_dep)

# ---- Output ----
cat("\nDIRECT negative %:", round(direct_neg_pct, 1),
    " median:", round(direct_med, 3),
    " IQR:", round(direct_q1, 3), "to", round(direct_q3, 3), "\n")

cat("INDIRECT positive %:", round(ind_pos_pct, 1),
    " |magnitude| median:", round(ind_mag_med, 3),
    " IQR:", round(ind_mag_q1, 3), "to", round(ind_mag_q3, 3), "\n")

cat("NET positive:", net_pos_n, "cities (", round(net_pos_pct, 1), "%)\n")

cat("\nCONTINENT summary:\n"); print(cont)

cat("\nAsia median direct:", round(asia$med_direct, 3),
    " median indirect:", round(asia$med_indirect, 3),
    " median net:", round(asia$med_net, 3), "\n")

cat("Factor (median net Asia / max median net others):", round(factor1_low, 2), "\n")
cat("Factor (net sum Asia / max net sum others):", round(factor2_low, 2), "\n")

cat("Others median net range:", round(others_med_net_min, 3), "to", round(others_med_net_max, 3), "\n")
cat("Others strongly positive net % (Net > global 75th):", round(others_strong_pos_pct, 1), "\n")

cat("\nLow dependency %:", round(low_dep_pct, 1), "\n")
cat("Reclamation-sensitive cities (high dep, lower pressure):", recl_sensitive_n, "\n")
print(dep_range_sensitive)

cat("\nHigh-pressure & high-dependency cities n:", nrow(hi_hi), "\n")
print(hi_hi[, c("City","Continent","Pressure","Dependency","Net","Direct","Indirect")])

cat("\nShanghai top % (by Pressure):", round(sh_top_pct, 1),
    " Shanghai dependency %:", round(sh_dep_pct, 1), "\n")
# 3. Define Thresholds
# X-axis threshold: median of Invasion Pressure
x_threshold <- median(df_analysis$Pressure, na.rm = TRUE)

# Y-axis threshold: Dependency = 0.5 (i.e., 50%)
# [Auto-correction logic]: prevent mismatch when data is in percentage (0-100) but threshold is decimal (0.5)
# If data max > 1, it's in percentage scale, set threshold to 50; otherwise set to 0.5
y_threshold <- if(max(df_analysis$Dependency_mech, na.rm=TRUE) > 1.5) 50 else 0.5

# 4. Add Quadrant Column
df_final <- df %>%
  mutate(
    # Add quadrant labels (detailed descriptions)
    Quadrant_Label = case_when(
      #  (Upper right): High risk + High dependency (Reclamation is the main driver)
      Pressure >= x_threshold & Dependency >= y_threshold ~ "Q1: Amplified Hotspots",
      
      #  (Upper left): Low risk + High dependency (Latent risk/Reclamation-dominated)
      Pressure < x_threshold & Dependency >= y_threshold  ~ "Q2: Latent Risk",
      
      #  (Lower left): Low risk + Low dependency (Safe zone/Background-dominated)
      Pressure < x_threshold & Dependency < y_threshold   ~ "Q3: Safe Coldspots",
      
      #  (Lower right): High risk + Low dependency (High risk due to favorable climate, etc.)
      Pressure >= x_threshold & Dependency < y_threshold  ~ "Q4: Natural Hotspots"
    ),
    
    # Add simple quadrant numbers (1, 2, 3, 4) for easier plotting
    Quadrant_Num = case_when(
      Pressure >= x_threshold & Dependency >= y_threshold ~ 1,
      Pressure < x_threshold & Dependency >= y_threshold  ~ 2,
      Pressure < x_threshold & Dependency < y_threshold   ~ 3,
      Pressure >= x_threshold & Dependency < y_threshold  ~ 4
    )
  )

# 5. View distribution of results
cat("\nCity count by quadrant:\n")
print(table(df_analysis$Quadrant_Label))

head(df_final)
df_analysis <- df_analysis %>%
  mutate(N_dir = ifelse(Total_Effect >= 0, "Amplifying (N>0)", "Suppressive (N<0)"))
p <- ggplot(df_analysis, aes(x = Pressure, y = Dependency_mech)) +
  
  # --- A. Draw quadrant dividing lines (bottom layer) ---
  # Use dashed lines, lighter color, don't steal visual focus
  geom_vline(xintercept = x_threshold, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  geom_hline(yintercept = 50 , linetype = "dashed", color = "gray50", linewidth = 0.5) +
  
  # --- B. Draw bubbles (core layer) ---
  geom_point(aes(size = Reclamation_area,   
                 fill = N_dir,     # Fill color mapping
                 color = N_dir),    # Color mapping: quadrant labels                   # Shape 21: supports fill+border             # White border (high contrast)
             stroke = 0.6, 
             shape=21,                 # Border thickness
             alpha = 0.85) +                # Slight transparency
  
  # --- C. Smart city name labeling ---
  # # Strategy: label points with larger area or extreme positions
  # geom_text_repel(
  #   data = subset(df_analysis, Reclamation_area > mean(Reclamation_area, na.rm=TRUE)), 
  #   aes(label = City, color = N_dir),
  #   size = 3,                 # Moderate font size
  #   fontface = "bold",        # Bold font
  #   bg.color = "white",       # White halo around text to prevent illegibility
  #   bg.r = 0.15,              # Halo radius
  #   box.padding = 0.5,        # Text box spacing
  #   max.overlaps = 30,        # Maximum number of allowed overlaps
  #   show.legend = FALSE       # Labels not shown in legend
  # ) +
  
  # --- D. Scale adjustments ---
  
#  scale_fill_manual(name = "Quadrant", values = c("#7B5C90", "#5D7894", "#A2B7A8", "#C0485A")) +
#   scale_color_manual(name = "Quadrant", values = c("#7B5C90", "#5D7894", "#A2B7A8", "#C0485A")) +
   scale_fill_manual(name = "Net reclamation effect", values = c("#5D7894", "#C0485A")) +
  scale_color_manual(name = "Net reclamation effect", values = c("#5D7894",  "#C0485A")) +
  # Size: scale by area (critical!)
  scale_size_area(
   range = c(4, 16),                # Control max bubble visual size (fine-tune based on canvas size)
    breaks = c(100, 1000, 10000, 100000), # Legend ticks: manually adjust based on your data range
    name = "Reclamation\nArea (ha)"
  ) +

  # Axes: appropriately expand boundaries to prevent largest bubbles from being cut off
  scale_x_continuous(expand = expansion(mult = 0.02)) + 
  # scale_y_continuous(expand = expansion(mult = 0.1)) +

  # --- E. Theme beautification ---
  theme_classic() +  # Classic black-white theme, most suitable for academic standards
  theme(
    # Unified font
    text = element_text(family = "sans", color = "black"),
    
    # Axis titles and ticks
    axis.title = element_text(size = 22, face = "bold"),
    axis.text = element_text(size = 22, color = "black"),
    
    # Legend settings (placed on right side)
    legend.position = "right", 
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 5),
    legend.background = element_blank(),
    
    # Border
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  ) +
  
  # --- F. Labels ---
  labs(
    x = "Pressure",    # Remember to clearly state X-axis meaning
    y = "Dependency Index (Socio-economic)",     # Remember to clearly state Y-axis meaning
    title = "Socio-ecological Trade-offs"        # Title
  )

# 3. Output preview
print(p)
windows()
ggsave("${LOCAL_ANALYSIS_ROOT}/figure/gam_draft/2colors_mech.png", p, width = 10, height = 8.5, dpi = 900,device = grDevices::png)



write.csv(df_analysis,'${LOCAL_ANALYSIS_ROOT}/data/organized_table/0501_risk_city_netscorewithQ.csv')
quadrant_stats <- df_final %>%
  # Ensure classification is a factor with reasonable order (for plot stacking order)
  mutate(Risk_Typology = factor(Quadrant_Label, levels = c(
    "Q1: Amplified Hotspots", # Upper right (most red)
    "Q4: Natural Hotspots",  # Lower right (deep blue)
    "Q2: Latent Risk", # Upper left (light red/orange)
    "Q3: Safe Coldspots"                 # Lower left (light gray/light blue)
  ))) %>%
  # Group statistics
  group_by(Continent, Risk_Typology) %>%
  summarise(Count = n(), .groups = "drop_last") %>%
  # Calculate percentages
  mutate(Percentage = Count / sum(Count)) %>%
  ungroup()

# View calculation results table
print(quadrant_stats)
# Define colors for four quadrants (consistent with scatter plot)
type_colors <- c(
  "Q1: Amplified Hotspots" = "#E64B35", # Red (anthropogenic hotspots)
  "Q2: Natural Hotspots"  = "#3C5488", # Deep blue (natural hotspots)
  "Q3: Latent Risk" = "#F39B7F", # Light red (latent risk)
  "Q4: Safe Coldspots"                = "#91D1C2"  # Light green/gray (safe zone)
)

p_composition <- ggplot(quadrant_stats, aes(x = Continent, y = Percentage, fill = Risk_Typology)) +
  
  # Stacked bar chart
  geom_bar(stat = "identity", width = 0.7, color = "white") +
  
  # Add percentage number labels (only show > 5% to avoid crowding)
  geom_text(aes(label = ifelse(Percentage > 0.05, scales::percent(Percentage, accuracy = 1), "")),
            position = position_stack(vjust = 0.5), size = 3, color = "white", fontface = "bold") +
  
  # Flip coordinates (easier to read continent names)
  coord_flip() +
  
  # Color mapping
  scale_fill_manual(values = type_colors, name = NULL) +
  
  # Y-axis percentage format
  scale_y_continuous(labels = scales::percent, expand = c(0, 0)) +
  
  # Title and theme
  labs(
    title = "<b>Regional Composition of Invasion Risk Types</b>",
    subtitle = "Proportion of cities in each quadrant of the risk-attribution typology.",
    x = NULL, y = "Percentage of Cities"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_markdown(face = "bold"),
    legend.position = "top",
    legend.text = element_text(size = 9),
    #legend.key.size = unit(0.4, "cm"),
    legend.box = "vertical", # Legend wraps if too long
    panel.grid.major.y = element_blank()
  )


# ==============================================================================
# Step 1: Merge into seven continents (Data Preparation)
# ==============================================================================
# Assume df_final is your original data containing Regions column
df_total_dummy <- df_analysis %>%
  mutate(Regions= "Total")

df_analysis <- df_updated_2 %>%
  mutate(
    Regions = case_when(
      # Asia
      str_detect(Regions, "Asia") ~ "Asia",
      # Europe
      str_detect(Regions, "Europe") ~ "Europe",
      # Africa
      str_detect(Regions, "Africa") ~ "Africa",
      # North America (including Central America)
      str_detect(Regions, "North America|Central America") ~ "North America",
      # South America
      str_detect(Regions, "South America") ~ "South America",
      # Oceania
      str_detect(Regions, "Oceania") ~ "Oceania",
      TRUE ~ "Other"
    )
  ) %>%
  # Filter out groups with too little data for statistics (e.g., Other)
  filter(Regions != "Other")

# ==============================================================================
# 2. Define general statistics function (The Magic Function)
# ==============================================================================
# This function automatically: calculates median -> difference test -> generates letters -> outputs formatted column
get_stats_column <- function(data, var_name, display_name) {
  
  # A. Extract data
  sub_df <- data %>% select(Regions, Value = all_of(var_name))
  
  # B. Statistical test (Kruskal-Wallis + Dunn)
  # If all groups have the same values, or only one group, skip test
  if (n_distinct(sub_df$Regions) > 1) {
    dunn_res <- dunn_test(sub_df, Value ~ Regions, p.adjust.method = "bonferroni")
    
    # Convert to letters
    p_vec <- dunn_res$p.adj
    names(p_vec) <- paste(dunn_res$group1, dunn_res$group2, sep = "-")
    letters <- multcompLetters(p_vec)$Letters
    
    letter_df <- data.frame(Regions = names(letters), Letter = letters)
  } else {
    letter_df <- data.frame(Regions = unique(sub_df$Regions), Letter = "")
  }
  
  # C. Calculate summary statistics
  stats <- sub_df %>%
    group_by(Regions) %>%
    summarise(
      Median = median(Value, na.rm = TRUE),
      IQR_25 = quantile(Value, 0.25, na.rm = TRUE),
      IQR_75 = quantile(Value, 0.75, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(letter_df, by = "Regions") %>%
    # Format: "0.12 (0.05-0.20) a"
    mutate(
      !!display_name := paste0(
  sprintf("%.3f", Median), 
  " (", sprintf("%.3f", IQR_25), "-", sprintf("%.3f", IQR_75), ")",
  ifelse(Letter != "", paste0(" ", Letter), "")
)
    ) %>%
    select(Regions, !!display_name)
  
  return(stats)
}

# ==============================================================================
# 3. Batch execution and merge tables
# ==============================================================================
df_analysis_all <- bind_rows(df_analysis, df_total_dummy)

# (Optional) Set factor order to ensure "Total" always appears in the last row of the table
df_analysis_all$Regions<- fct_relevel(as.factor(df_analysis_all$Regions), "Total", after = Inf)

# ==========================================================
# 2. The following code hardly needs modification, just replace input data with df_analysis_all
# ==========================================================

# Calculate N (sample size)
base_table <- df_analysis_all %>%
  group_by(Regions) %>%
  summarise(N = n()) %>%
  arrange(Regions) # Ensure sorting

# List of variables to analyze
target_vars <- c(
  "Reclamation_area" = "Reclamation Area (km2)",
  "Pressure"         = "Total Invasion Risk",
  "Direct_Effect"           = "Direct Effect (Cost)",
  "Indirect_Effect"         = "Indirect Effect (Benefit)",
  "Total_Effect"              = "Net Effect"
)

# Loop merge
final_si_table <- base_table

for (v in names(target_vars)) {
  cat("Analyzing:", target_vars[v], "...\n")
  
  # Note: Input here is the full dataset df_analysis_all containing Total
  # As long as your get_stats_column function internally uses group_by(Continent),
  # it will automatically calculate the "Total" group as well
  temp_res <- get_stats_column(df_analysis_all, v, target_vars[v])
  
  final_si_table <- left_join(final_si_table, temp_res, by = "Regions")
}

# View results
print(final_si_table)
# ==============================================================================
# 4. View and save
# ==============================================================================
print(head(final_si_table))
final_si_table
write.xlsx(final_si_table, "${LOCAL_ANALYSIS_ROOT}/data/organized_table/06SI_Table_Comprehensive_Regional_Stats.xlsx")
plot_long <- df_analysis %>%
  select(Continent, Reclamation_area, Pressure, Direct, Indirect, Net) %>%
  pivot_longer(cols = -Continent, names_to = "Metric", values_to = "Value") %>%
  mutate(
    Metric = factor(Metric, 
                    levels = c("reclamation_area", "Pressure", "Direct", "Indirect", "Net"),
                    labels = c("a | Reclamation Area", "b | Total Risk", 
                               "c | Direct Effect", "d | Indirect Effect", "e | Net Effect"))
  )
final_si_table
p_multi <- ggplot(plot_long, aes(x = reorder(Continent, Value, median), y = Value, fill = Continent)) +
  
  # 1. Jitter points (show sample size imbalance)
  geom_jitter(width = 0.2, alpha = 0.3, color = "grey40", size = 0.8) +
  
  # 2. Boxplot
  geom_boxplot(width = 0.6, alpha = 0.8, outlier.shape = NA) +
  
  # 3. Facet display
  facet_wrap(~Metric, scales = "free", ncol = 3) +
  
  # 4. Beautification
  scale_fill_brewer(palette = "Set3") +
  coord_flip() + # Flip coordinates, continent names written horizontally
  labs(x = NULL, y = "Value (Units vary by metric)") +
  
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 11, hjust = 0),
    panel.border = element_rect(color = "grey", fill = NA)
  )

print(p_multi)
windows()
