# Original source: extract_buffer_vs_scale_regression.R
# Original source location: <LOCAL_CODE_ROOT>/extract_buffer_vs_scale_regression.R
# Original source SHA-256: 92be7de25ab5ccd37de970f2b71f0a3657b9aae36452943520716aa6fa63ebe1
# Public-safe original snapshot: archive/original_sources/local/extract_buffer_vs_scale_regression.R
# Classification: EXPLORATORY
# Publication modifications:
# - machine-specific absolute path literals replaced by configured publication_path aliases
# - local encoding decoded to UTF-8; source bytes hashed above
# - comments and historical private machine paths sanitized
# - working-directory calls retained with configured paths; no analyses executed
# Scientific modification: NONE
# This is full historical source, not a certified unattended/reproducible run.
# Snapshot for inspection; path placeholders are not executable locations.
library(dplyr)
library(ggplot2)

setwd("<LOCAL_ANALYSIS_ROOT>/data/organized_table")

# 1. Load the buffer data (Delta Pressure)
buffer_data <- read.csv("0701_50kmbufferclassified.csv", stringsAsFactors=FALSE, fileEncoding="UTF-8-BOM")
if (!"City" %in% names(buffer_data)) names(buffer_data)[2] <- "City" # Enforce mapping if BOM persists

# 2. Load the main GAM dataset containing reclamation fraction
gam_data <- read.csv("<LOCAL_ANALYSIS_ROOT>/figure/gam_draft/gamtest_withpca.csv", stringsAsFactors=FALSE)

# Process GAM data to calculate fraction
gam_data$reclamation_area <- gam_data$reclamation_area + 1e-6
gam_data$area_km2 <- gam_data$area_km2 + 1e-6
gam_data$recl_frac <- gam_data$reclamation_area / gam_data$area_km2
gam_data$log_recl_frac <- log1p(gam_data$recl_frac)

# Standardize names for joining
buffer_data$join_name <- trimws(tolower(buffer_data$City))
gam_data$join_name <- trimws(tolower(gam_data$city))

# 3. Join the datasets
merged_df <- inner_join(buffer_data, gam_data, by = "join_name")

cat("Successfully matched", nrow(merged_df), "cities out of 108.\n\n")

# 4. Run the linear regression
cat("=== Linear Regression: Delta_Pressure ~ log_recl_frac ===\n")
model <- lm(Delta_Pressure ~ log_recl_frac, data = merged_df)
print(summary(model))

# Run a Spearman correlation as a non-parametric check just in case
cor_test <- cor.test(merged_df$log_recl_frac, merged_df$Delta_Pressure, method = "spearman")
cat("\n=== Spearman Rank Correlation ===\n")
print(cor_test)

# 5. Create a visualization scatterplot
p_scatter <- ggplot(merged_df, aes(x = log_recl_frac, y = Delta_Pressure)) +
  geom_point(aes(color = City_Role.x, size = area_km2), alpha = 0.7) +
  geom_smooth(method = "lm", color = "black", fill = "grey80") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_classic(base_size = 14) +
  scale_color_manual(values = c("Amplifier" = "#d73027", "Neutral" = "#999999", "Sink" = "#4575b4")) +
  labs(
    title = "Does Reclamation Intensity Drive Invasion Amplification?",
    x = "Log Reclamation Fraction",
    y = "Delta Pressure (City - 50km Background)",
    color = "City Role",
    size = "City Area (km²)"
  )

ggsave("<LOCAL_ANALYSIS_ROOT>/figure/gam_draft/buffer_vs_reclamation.png", p_scatter, width = 8, height = 6, dpi=300)
cat("\nPlot saved to <LOCAL_ANALYSIS_ROOT>/figure/gam_draft/buffer_vs_reclamation.png\n")
