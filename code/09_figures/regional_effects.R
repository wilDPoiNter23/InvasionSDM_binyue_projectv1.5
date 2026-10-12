# City typology and regional summary table
# Input: archived coefficient-based city table
# Output: typology panel and regional statistics

source(file.path(Sys.getenv("INVASIONSDM_PROJECT_ROOT", "."), "config", "bootstrap.R"))
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(rstatix)
library(multcompView)
library(forcats)
library(openxlsx)
df_analysis <- read.csv(publication_path("CITY_EFFECT_TABLE"), check.names=FALSE)
df_updated_2 <- df_analysis
df <- df_analysis
df$Dependency <- df$Dependency_mech
x_threshold <- median(df_analysis$Pressure, na.rm = TRUE)

# Y-axis threshold: Dependency = 0.5 (50%).
# Handle percentage inputs on a 0-100 scale when the threshold is 0.5.
# Use threshold 50 when the maximum exceeds 1; otherwise use 0.5.
y_threshold <- if(max(df_analysis$Dependency_mech, na.rm=TRUE) > 1.5) 50 else 0.5

# Assign quadrants.
df_final <- df %>%
  mutate(
    # Add descriptive quadrant labels.
    Quadrant_Label = case_when(
      # Upper right: high risk and high dependency, with a large reclamation contribution.
      Pressure >= x_threshold & Dependency >= y_threshold ~ "Q1: Amplified Hotspots",

      # Upper left: low risk and high dependency, with reclamation dominance.
      Pressure < x_threshold & Dependency >= y_threshold  ~ "Q2: Latent Risk",

      # Lower left: low risk and low dependency, with background dominance.
      Pressure < x_threshold & Dependency < y_threshold   ~ "Q3: Safe Coldspots",

      # Lower right: high risk and low dependency, potentially reflecting favorable climatic conditions.
      Pressure >= x_threshold & Dependency < y_threshold  ~ "Q4: Natural Hotspots"
    ),

    # Add quadrant numbers 1, 2, 3 and 4 for plotting.
    Quadrant_Num = case_when(
      Pressure >= x_threshold & Dependency >= y_threshold ~ 1,
      Pressure < x_threshold & Dependency >= y_threshold  ~ 2,
      Pressure < x_threshold & Dependency < y_threshold   ~ 3,
      Pressure >= x_threshold & Dependency < y_threshold  ~ 4
    )
  )

# Inspect the distribution of categories.
cat("\nCity counts by quadrant:\n")
print(table(df_analysis$Quadrant_Label))

df_analysis <- df_analysis %>%
  mutate(N_dir = ifelse(Total_Effect >= 0, "Amplifying (N>0)", "Suppressive (N<0)"))
p <- ggplot(df_analysis, aes(x = Pressure, y = Dependency_mech)) +

  # Layer A: quadrant dividing lines beneath other layers.
  # Use faint dashed lines.
  geom_vline(xintercept = x_threshold, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  geom_hline(yintercept = 50 , linetype = "dashed", color = "gray50", linewidth = 0.5) +

  # Layer B: bubbles.
  geom_point(aes(size = Reclamation_area,
                 fill = N_dir,     # Map fill colors.
                 color = N_dir),    # Map color to quadrant labels; shape 21 supports fill and borders, with white borders.
             stroke = 0.6,
             shape=21,                 # Border width.
             alpha = 0.85) +                # Use slight transparency.

  # Layer C: city labels.
  # Label points with large areas or extreme positions.
  # geom_text_repel(
  #   data = subset(df_analysis, Reclamation_area > mean(Reclamation_area, na.rm=TRUE)),
  #   aes(label = City, color = N_dir),
  # size = 3,                 # Set label size.
  # fontface = "bold",        # Use bold text.
  # bg.color = "white",       # Use a white text halo for readability.
  # bg.r = 0.15,              # Set halo radius.
  # box.padding = 0.5,        # Set label box spacing.
  # max.overlaps = 30,        # Set the maximum permitted overlaps.
  # show.legend = FALSE       # Exclude labels from the legend.
  # ) +

  # Layer D: scales.

#  scale_fill_manual(name = "Quadrant", values = c("#7B5C90", "#5D7894", "#A2B7A8", "#C0485A")) +
#   scale_color_manual(name = "Quadrant", values = c("#7B5C90", "#5D7894", "#A2B7A8", "#C0485A")) +
   scale_fill_manual(name = "Net reclamation effect", values = c("#5D7894", "#C0485A")) +
  scale_color_manual(name = "Net reclamation effect", values = c("#5D7894",  "#C0485A")) +
  # Scale bubble size by area.
  scale_size_area(
   range = c(4, 16),                # Adjust the maximum bubble size to the canvas.
    breaks = c(100, 1000, 10000, 100000), # Choose legend breaks for the data range.
    name = "Reclamation\nArea (ha)"
  ) +

  # Expand axis limits to avoid clipping large bubbles.
  scale_x_continuous(expand = expansion(mult = 0.02)) +
  # scale_y_continuous(expand = expansion(mult = 0.1)) +

  # Layer E: theme.
  theme_classic() +  # Use a classic black-and-white theme.
  theme(
    # Use a consistent font.
    text = element_text(family = "sans", color = "black"),

    # Format axis titles and ticks.
    axis.title = element_text(size = 22, face = "bold"),
    axis.text = element_text(size = 22, color = "black"),

    # Place the legend on the right.
    legend.position = "right",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 5),
    legend.background = element_blank(),

    # Set the border.
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  ) +

  # Layer F: labels.
  labs(
    x = "Pressure",    # Describe the x-axis variable.
    y = "Dependency Index (Socio-economic)",     # Describe the y-axis variable.
    title = "Socio-ecological Trade-offs"        # Set the title.
  )

# Preview the output.
print(p)
ggsave(publication_path("OUTPUT_ROOT", "figures/gam_draft/2colors_mech.png"), p, width = 10, height = 8.5, dpi = 900,device = grDevices::png)



write.csv(df_analysis,publication_path("LOCAL_ANALYSIS_ROOT", "data/organized_table/0501_risk_city_netscorewithQ.csv"))
df_total_dummy <- df_analysis %>%
  mutate(Regions= "Total")

df_analysis <- df_updated_2 %>%
  mutate(
    Regions = case_when(
      # Asia.
      str_detect(Regions, "Asia") ~ "Asia",
      # Europe.
      str_detect(Regions, "Europe") ~ "Europe",
      # Africa.
      str_detect(Regions, "Africa") ~ "Africa",
      # North America, including Central America.
      str_detect(Regions, "North America|Central America") ~ "North America",
      # South America.
      str_detect(Regions, "South America") ~ "South America",
      # Oceania.
      str_detect(Regions, "Oceania") ~ "Oceania",
      TRUE ~ "Other"
    )
  ) %>%
  # Exclude groups with insufficient data, such as Other.
  filter(Regions != "Other")

# ==============================================================================
# Define a reusable statistical summary function.
# ==============================================================================
# Calculate medians, test differences, assign letters and format output columns.
get_stats_column <- function(data, var_name, display_name) {

  # Extract data.
  sub_df <- data %>% select(Regions, Value = all_of(var_name))

  # Use Kruskal-Wallis and Dunn tests.
  # Skip testing when all values are identical or only one group is present.
  if (n_distinct(sub_df$Regions) > 1) {
    dunn_res <- dunn_test(sub_df, Value ~ Regions, p.adjust.method = "bonferroni")

    # Convert comparisons to letter groups.
    p_vec <- dunn_res$p.adj
    names(p_vec) <- paste(dunn_res$group1, dunn_res$group2, sep = "-")
    letters <- multcompLetters(p_vec)$Letters

    letter_df <- data.frame(Regions = names(letters), Letter = letters)
  } else {
    letter_df <- data.frame(Regions = unique(sub_df$Regions), Letter = "")
  }

  # Calculate summary statistics.
  stats <- sub_df %>%
    group_by(Regions) %>%
    summarise(
      Median = median(Value, na.rm = TRUE),
      IQR_25 = quantile(Value, 0.25, na.rm = TRUE),
      IQR_75 = quantile(Value, 0.75, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(letter_df, by = "Regions") %>%
    # Format as "0.12 (0.05-0.20) a".
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
# Apply the function to all variables and combine results.
# ==============================================================================
df_analysis_all <- bind_rows(df_analysis, df_total_dummy)

# Optionally set factor levels so Total is the final row.
df_analysis_all$Regions<- fct_relevel(as.factor(df_analysis_all$Regions), "Total", after = Inf)

# ==========================================================
# Use df_analysis_all as the input to the following code.
# ==========================================================

# Calculate sample size N.
base_table <- df_analysis_all %>%
  group_by(Regions) %>%
  summarise(N = n()) %>%
  arrange(Regions) # Check the ordering.

# List variables for analysis.
target_vars <- c(
  "Reclamation_area" = "Reclamation Area (km2)",
  "Pressure"         = "Total Invasion Risk",
  "Direct_Effect"           = "Direct Effect (Cost)",
  "Indirect_Effect"         = "Indirect Effect (Benefit)",
  "Total_Effect"              = "Net Effect"
)

# Combine results in a loop.
final_si_table <- base_table

for (v in names(target_vars)) {
  cat("Analysing:", target_vars[v], "...\n")

  # Pass the complete df_analysis_all dataset, including Total.
  # get_stats_column groups by Continent.
  # The function also calculates the Total group.
  temp_res <- get_stats_column(df_analysis_all, v, target_vars[v])

  final_si_table <- left_join(final_si_table, temp_res, by = "Regions")
}

# Inspect the results.
print(final_si_table)
# ==============================================================================
# 4. Inspect and save.
# ==============================================================================
print(head(final_si_table))
final_si_table
write.xlsx(final_si_table, publication_path("LOCAL_ANALYSIS_ROOT", "data/organized_table/06SI_Table_Comprehensive_Regional_Stats.xlsx"))
