# Predictor correlation matrix
# Input: Predictor correlation table
# Output: Correlation matrix figure

source(file.path(Sys.getenv("INVASIONSDM_PROJECT_ROOT", "."), "config", "bootstrap.R"))
library(terra)
library(dplyr)
library(stats)
library(tidyr)
library(ggplot2)
library(ggcorrplot)
library(tibble)
library(pheatmap)

library(igraph)
cor_mat <- read.csv(publication_path("PREDICTOR_CORRELATIONS"))
# 1. Read the multilayer raster.
r_stack <- terra::rast(publication_path("PREDICTOR_STACK"))
var_info <- tibble::tribble(
  ~raw,                           ~label,                                      ~category,
  # Climate
  "wc2.1_2.5m_bio_1",  "Annual mean temperature (°C)",              "Climate",
  "wc2.1_2.5m_bio_2",  "Mean diurnal temperature range (°C)",       "Climate",
  "wc2.1_2.5m_bio_3",  "Isothermality (%)",                         "Climate",
  "wc2.1_2.5m_bio_4",  "Temperature seasonality (SD × 100)",        "Climate",
  "wc2.1_2.5m_bio_5",  "Max temperature of warmest month (°C)",     "Climate",
  "wc2.1_2.5m_bio_6",  "Min temperature of coldest month (°C)",     "Climate",
  "wc2.1_2.5m_bio_7",  "Temperature annual range (°C)",             "Climate",
  "wc2.1_2.5m_bio_8",  "Mean temperature of wettest quarter (°C)",  "Climate",
  "wc2.1_2.5m_bio_9",  "Mean temperature of driest quarter (°C)",   "Climate",
  "wc2.1_2.5m_bio_10", "Mean temperature of warmest quarter (°C)",  "Climate",
  "wc2.1_2.5m_bio_11", "Mean temperature of coldest quarter (°C)",  "Climate",
  "wc2.1_2.5m_bio_12", "Annual precipitation (mm)",                 "Climate",
  "wc2.1_2.5m_bio_13", "Precipitation of wettest month (mm)",       "Climate",
  "wc2.1_2.5m_bio_14", "Precipitation of driest month (mm)",        "Climate",
  "wc2.1_2.5m_bio_15", "Precipitation seasonality (CV)",            "Climate",
  "wc2.1_2.5m_bio_16", "Precipitation of wettest quarter (mm)",     "Climate",
  "wc2.1_2.5m_bio_17", "Precipitation of driest quarter (mm)",      "Climate",
  "wc2.1_2.5m_bio_18", "Precipitation of warmest quarter (mm)",     "Climate",
  "wc2.1_2.5m_bio_19", "Precipitation of coldest quarter (mm)",     "Climate",
  # Soil
  "bdod_mean_2024_2_5min_aligned",      "Soil bulk density (g cm⁻³)",       "Soil",
  "cfvo_mean_2024_2_5min_aligned",      "Coarse fragments volume (%)",      "Soil",
  "clay_mean_2024_2_5min_aligned",      "Clay content (%)",                 "Soil",
  "sand_mean_2024_2_5min_aligned",      "Sand content (%)",                 "Soil",
  "silt_mean_2024_2_5min_aligned",      "Silt content (%)",                 "Soil",
  "nitrogen_mean_2024_2_5min_aligned",  "Soil nitrogen (g kg⁻¹)",           "Soil",
  "ocd_mean_2024_2_5min_aligned",       "SOC density (kg m⁻²)",             "Soil",
  "soc_mean_2024_2_5min_aligned",       "SOC content (g kg⁻¹)",             "Soil",
  "phh2o_mean_2024_2_5min_aligned",     "Soil pH (H₂O)",                    "Soil",
  # Socio-economic
  "BUILT_S_E2025_2.5min",   "Built-up fraction",           "Anthropogenic",
  "GDP2020_2.5min",         "GDP density",                 "Anthropogenic",
  "Pop2020_2.5min",         "Population density",          "Anthropogenic",
  "VIIRS_2023_2.5min",      "Night-time lights",           "Anthropogenic",
  "reclamation_raster",     "Reclamation area",            "Anthropogenic",
  # Topography
  "wc2.1_2.5m_elev", "Elevation (m)", "Topography",
  "slope",           "Slope (°)",     "Topography",
  "aspect",          "Aspect (°)",    "Topography",
  # Marine
  "salinity_mean",     "Mean sea surface salinity",          "Marine",
  "salinity_range",    "Sea surface salinity range",         "Marine",
  "temperature_mean",  "Mean sea surface temperature (°C)",  "Marine",
  "temperature_range", "Sea surface temperature range (°C)", "Marine"
)

pretty_label <- function(x) {
  out <- var_info$label[match(x, var_info$raw)]
  out[is.na(out)] <- x[is.na(out)]
  out
}
# Inspect variable names and confirm the 40 candidate predictors.
# names(r_stack)
# set.seed(123)

# 2. Randomly sample cells from the global calibration domain.
# r_sample <- terra::spatSample(
#   r_stack,
# size = 50000 ; adjust the sample size to computing capacity.
#   method = "random",
#   na.rm  = TRUE,
#   as.points = FALSE
# )

# Optionally remove columns with many missing values; spatSample usually handles these.
# vals <- vals %>% select(where(~ sum(!is.na(.)) > 1000))
# str(vals)
# Calculate the Pearson correlation matrix.
# cor_mat <- cor(vals, use = "pairwise.complete.obs", method = "pearson")



# Plot correlations with hierarchical clustering of variables.
# Complete symmetric pairs where only A-B is present.
# Missing symmetric pairs produce NA entries and can prevent clustering.
df_mirror <- cor_mat%>%
  rename(var1 = var2, var2 = var1) %>% # Swap column names.
  select(var1, var2, r)

df_full <- bind_rows(cor_mat %>% select(var1, var2, r), df_mirror) %>%
  distinct(var1, var2, .keep_all = TRUE) # Remove duplicate rows.

# Set missing self-correlations to 1.
vars <- unique(c(df_full$var1, df_full$var2))
df_diag <- data.frame(var1 = vars, var2 = vars, r = 1)
df_final <- bind_rows(df_full, df_diag) %>% distinct(var1, var2, .keep_all = TRUE)

# Convert to a matrix.
cor_matrix <- df_final %>%
  pivot_wider(names_from = var2, values_from = r, values_fill = 0) %>%
  column_to_rownames("var1") %>%
  as.matrix()
dim(cor_matrix)

dist_mat <- as.dist(1 - abs(cor_matrix))

# Perform hierarchical clustering.
hc <- hclust(dist_mat, method = "complete")

# Extract the ordered variable names.
ordered_vars <- rownames(cor_matrix)[hc$order]


# Set factor levels in the original data.
# Factor levels determine the plotting order.
df_full$var1 <- factor(df_full$var1, levels = ordered_vars)
df_full$var2 <- factor(df_full$var2, levels = ordered_vars)

# Draw the plot.
p_cor <- ggplot(df_full, aes(x = var1, y = var2, fill = r)) +
  geom_tile(color = "white", size = 0.05) + # Use thin white cell borders.

  # Use a dark blue-white-dark red palette.
  scale_fill_gradient2(
    low = "#2166AC", mid = "white", high = "#B2182B",
    midpoint = 0, limit = c(-1, 1),
    name = "Pearson\nCorr"
  ) +

  coord_fixed() + # Keep cells square.

  theme_minimal() +
  theme(
    # Rotate x-axis labels 90 degrees and reduce their size for 40 variables.
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 7),
    # Format y-axis labels.
    axis.text.y = element_text(size = 7),

    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right"
  )

pheatmap(cor_matrix ,
         # Set colors.
         color = colorRampPalette(c("#2166AC", "white", "#B2182B"))(100),

         # Cluster variables automatically.
         cluster_rows = TRUE,
         cluster_cols = TRUE,

         # Split clusters into groups, for example five blocks.
         cutree_rows = 3,
         cutree_cols = 4,

         # Format the plot.
         show_rownames = TRUE,
         show_colnames = TRUE,
         fontsize = 8,
         border_color = NA,  # Remove cell borders for continuous color blocks.
         treeheight_row = 20,
         treeheight_col = 20,

         main = "Cluster Heatmap with Gaps"
)

  ggsave(
  filename = publication_path("OUTPUT_ROOT", "figures/sdm_evalueation/Supp_Fig_S4_cor_matrix.png"),
  plot     = p_cor,
  width    = 7,
  height   = 6,
device = grDevices::png
)
# Keep strong correlations, for example |r| > 0.7.
threshold <- 0.8
cor_filter <- cor_matrix
cor_filter[abs(cor_filter) < threshold] <- 0 # Set weak correlations to 0.
cor_filter[cor_filter == 1] <- 0             # Set diagonal entries to 0.

# Build the network.
net <- graph_from_adjacency_matrix(cor_filter, mode = "undirected", weighted = TRUE, diag = FALSE)

# Plot the network.
# Remove isolated variables without strong collinearity.
net_clean <- delete_vertices(net, V(net)[degree(net) == 0])

net <- plot(net_clean,
     layout = layout_with_fr(net_clean), # Use a force-directed layout.
     vertex.size = 5,                    # Set node size.
     vertex.label.cex = 0.7,             # Set label size.
     vertex.color = "orange",            # Set node color.
     edge.width = E(net_clean)$weight * 3, # Thicker edges indicate stronger correlations.
     main = "High Collinearity Network (|r| > 0.7)"
)

  ggsave(
  filename = publication_path("OUTPUT_ROOT", "figures/sdm_evalueation/Supp_Fig_net.pdf"),
  plot     = net,
  width    = 7,
 height   = 6

# device = grDevices::png
)
corrplot(cor_matrix,
         method = "square",       # Fill each cell with a square.
         order = "hclust",        # Order variables by hierarchical clustering.
         hclust.method = "ward.D2",

         # Set colors.
         col = colorRampPalette(c("#4477AA", "white", "#BB4444"))(200),

         # Use a minimal theme.
         tl.col = "black",        # Set text color.
         tl.cex = 0.6,            # Use small text, for example 0.6 or 0.5.
         tl.srt = 90,             # Rotate labels vertically to save space.

         cl.cex = 0.8,            # Set legend text size.

         # Remove the diagonal.
         diag = FALSE,
         grid = FALSE             # Remove background grid lines.
)
