# City-variable PCA loadings
# Input: City-level PCA analysis table
# Output: PC1 loadings and variance explained

source(file.path(Sys.getenv("INVASIONSDM_PROJECT_ROOT", "."), "config", "bootstrap.R"))
library(dplyr)
library(tidyr)
library(mgcv)
library(spdep)
coords <- cbind(city_df_res$Longitude, city_df_res$Latitude)

# 1. Read the data.
df_city <- read.csv(publication_path("PCA_LOADING_INPUT"))
# 2. Define variable groups identical to those used in the PCA.
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
  "BUILT_S_E2025_2.5min",   "Built-up fraction",              "Anthropogenic",
  "GDP2020_2.5min",         "GDP density",                    "Anthropogenic",
  "Pop2020_2.5min",         "Population density",             "Anthropogenic",
  "VIIRS_2023_2.5min",      "Night-time lights",              "Anthropogenic",
  "reclamation_raster",     "Reclamation area",               "Anthropogenic",
  # Topography
  "wc2.1_2.5m_elev", "Elevation (m)", "Topography",
  "slope",           "Slope (°)",     "Topography",
  "aspect",          "Aspect (°)",    "Topography",
  # Marine
  "salinity_mean",   "Mean sea surface salinity",             "Marine",
  "salinity_range",  "Sea surface salinity range",            "Marine",
  "temperature_mean","Mean sea surface temperature (°C)",     "Marine",
  "temperature_range","Sea surface temperature range (°C)",   "Marine"
)
get_pretty_name <- function(raw_name) {
  # Look up the corresponding label in var_info.
  match_idx <- match(raw_name, var_info$raw)

  if (!is.na(match_idx)) {
    return(var_info$label[match_idx]) # Return Label when a match is found.
  } else {
    return(raw_name) # Otherwise retain the original name.
  }
}
# Match variable names to the CSV column names.
group_list <- list(
  Climate = paste0("wc2.1_2.5m_bio_", 1:19),

  Anthropogenic = c("BUILT_S_E2025_2.5min", "GDP2020_2.5min",
                    "Pop2020_2.5min", "VIIRS_2023_2.5min", "reclamation_area"), # Check this variable for potential issues.

  Soil = c("bdod_mean_2024_2_5min_aligned", "cfvo_mean_2024_2_5min_aligned",
           "clay_mean_2024_2_5min_aligned", "nitrogen_mean_2024_2_5min_aligned",
           "ocd_mean_2024_2_5min_aligned", "phh2o_mean_2024_2_5min_aligned",
           "sand_mean_2024_2_5min_aligned", "silt_mean_2024_2_5min_aligned",
           "soc_mean_2024_2_5min_aligned"),

  Marine = c("salinity_mean", "salinity_range", "temperature_mean", "temperature_range"),

  Geomorphic = c("wc2.1_2.5m_elev", "slope", "aspect")
)

# 3. Iterate with variance checks.
pc_interpretation_list <- list()

cat("PCA loading calculation\n")

for (grp_name in names(group_list)) {

  vars <- group_list[[grp_name]]

  # A. Check that variables exist.
  valid_vars <- vars[vars %in% names(df_city)]

  if(length(valid_vars) > 1) {
    # Extract the data matrix.
    data_subset <- df_city[, valid_vars]

    # B. Replace NA values with column means.
    for(i in 1:ncol(data_subset)) {
      if(any(is.na(data_subset[,i]))) {
        data_subset[is.na(data_subset[,i]), i] <- mean(data_subset[,i], na.rm = TRUE)
      }
    }

    # C. Remove zero-variance variables.
    # Calculate variance for each column.
    col_vars <- apply(data_subset, 2, var)

    # Identify columns with variance 0 or NA, including entirely missing columns.
    bad_cols <- names(col_vars)[col_vars == 0 | is.na(col_vars)]

    if(length(bad_cols) > 0) {
      cat("Warning: group ", grp_name, " has constant variables removed:", paste(bad_cols, collapse=", "), "\n")
      # Remove these columns from the data.
      data_subset <- data_subset[, !names(data_subset) %in% bad_cols, drop = FALSE]
    }

    # D. Check remaining variables: PCA requires at least 2.
    if(ncol(data_subset) > 1) {
      # Run PCA.
      pca_res <- prcomp(data_subset, center = TRUE, scale. = TRUE)

      # Extract results.
      loadings <- pca_res$rotation[, 1]
      var_exp <- summary(pca_res)$importance[2, 1] * 100

      # Save the table.
      pc_interpretation_list[[grp_name]] <- data.frame(
        Category = grp_name,
        Variable = names(loadings),
        PC1_Loading = round(loadings, 3),
        PC1_Variance_Explained = paste0(round(var_exp, 1), "%")
      )
    } else {
      cat("Skipping group ", grp_name, "fewer than two usable variables for PCA.\n")
    }
  }
}

# 4. Combine results.
if(length(pc_interpretation_list) > 0) {
  final_pc_table <- do.call(rbind, pc_interpretation_list)
  row.names(final_pc_table) <- NULL

  print(head(final_pc_table))
  write.csv(final_pc_table, "SI_Table_PCA_Loadings_Fixed.csv", row.names = FALSE)
  cat("Results saved to SI_Table_PCA_Loadings_Fixed.csv\n")
} else {
  cat("No results were written.\n")
}

# Apply the transformation function with sapply.
final_pc_table$Description <- sapply(final_pc_table$Variable, get_pretty_name)

# ==============================================================================
# 3. Arrange table columns.
# ==============================================================================
# Place Description immediately after Variable for readability.
final_pc_table_polished <- final_pc_table %>%
  select(
    Category,
    Variable,
    Description,          # Place the explanatory column among the central columns.
    PC1_Loading,
    PC1_Variance_Explained
  )

# ==============================================================================
# 4. Inspect and save.
# ==============================================================================

cat("PCA loading table\n")
print(head(final_pc_table_polished))

write.csv(final_pc_table_polished, publication_path("OUTPUT_ROOT", "figures/gam_draft/SI_Table_PCA_Loadings_Final.csv"), row.names = FALSE)
