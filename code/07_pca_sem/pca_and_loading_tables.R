# Publication integration: pca_and_loading_tables.R
# Role: PCA/reference loading tables
# Inputs: City table; pre-existing city_df_res with coordinates
# Outputs: PCA/loadings and diagnostic statistics
# Source SHA256: 62e1c4d3b00f31ce853185fec29b89eeaa114dcce362ab996a65a877563e5112
# Scope: implementation reference; submitted-use certainty is in metadata.
# Scientific expressions/settings retained. Only paths and setup were cleaned.
# Load config/bootstrap.R before sourcing, or use code/run_stage.R.
if (!exists("publication_path", mode = "function")) stop("Load config/bootstrap.R first")
publication_require_workspace(c("city_df_res"))
library(dplyr)
library(tidyr)
library(mgcv)
library(spdep)
# Publication portability: environment setup omitted.
coords <- cbind(city_df_res$Longitude, city_df_res$Latitude)

# 1. Read data
df_city <- read.csv(publication_path("LOCAL_ANALYSIS_ROOT", "figure/gam_draft/gamtest_withpca.csv"))
# 2. Define variable groups (must match exactly with your original PCA)
var_info <- tibble::tribble(
  ~raw, ~label, ~category,
  # Climate
  "wc2.1_2.5m_bio_1", "Annual mean temperature (°C)", "Climate",
  "wc2.1_2.5m_bio_2", "Mean diurnal temperature range (°C)", "Climate",
  "wc2.1_2.5m_bio_3", "Isothermality (%)", "Climate",
  "wc2.1_2.5m_bio_4", "Temperature seasonality (SD × 100)", "Climate",
  "wc2.1_2.5m_bio_5", "Max temperature of warmest month (°C)", "Climate",
  "wc2.1_2.5m_bio_6", "Min temperature of coldest month (°C)", "Climate",
  "wc2.1_2.5m_bio_7", "Temperature annual range (°C)", "Climate",
  "wc2.1_2.5m_bio_8", "Mean temperature of wettest quarter (°C)", "Climate",
  "wc2.1_2.5m_bio_9", "Mean temperature of driest quarter (°C)", "Climate",
  "wc2.1_2.5m_bio_10", "Mean temperature of warmest quarter (°C)", "Climate",
  "wc2.1_2.5m_bio_11", "Mean temperature of coldest quarter (°C)", "Climate",
  "wc2.1_2.5m_bio_12", "Annual precipitation (mm)", "Climate",
  "wc2.1_2.5m_bio_13", "Precipitation of wettest month (mm)", "Climate",
  "wc2.1_2.5m_bio_14", "Precipitation of driest month (mm)", "Climate",
  "wc2.1_2.5m_bio_15", "Precipitation seasonality (CV)", "Climate",
  "wc2.1_2.5m_bio_16", "Precipitation of wettest quarter (mm)", "Climate",
  "wc2.1_2.5m_bio_17", "Precipitation of driest quarter (mm)", "Climate",
  "wc2.1_2.5m_bio_18", "Precipitation of warmest quarter (mm)", "Climate",
  "wc2.1_2.5m_bio_19", "Precipitation of coldest quarter (mm)", "Climate",
  # Soil
  "bdod_mean_2024_2_5min_aligned", "Soil bulk density (g cm⁻³)", "Soil",
  "cfvo_mean_2024_2_5min_aligned", "Coarse fragments volume (%)", "Soil",
  "clay_mean_2024_2_5min_aligned", "Clay content (%)", "Soil",
  "sand_mean_2024_2_5min_aligned", "Sand content (%)", "Soil",
  "silt_mean_2024_2_5min_aligned", "Silt content (%)", "Soil",
  "nitrogen_mean_2024_2_5min_aligned", "Soil nitrogen (g kg⁻¹)", "Soil",
  "ocd_mean_2024_2_5min_aligned", "SOC density (kg m⁻²)", "Soil",
  "soc_mean_2024_2_5min_aligned", "SOC content (g kg⁻¹)", "Soil",
  "phh2o_mean_2024_2_5min_aligned", "Soil pH (H₂O)", "Soil",
  # Socio-economic
  "BUILT_S_E2025_2.5min", "Built-up fraction", "Anthropogenic",
  "GDP2020_2.5min", "GDP density", "Anthropogenic",
  "Pop2020_2.5min", "Population density", "Anthropogenic",
  "VIIRS_2023_2.5min", "Night-time lights", "Anthropogenic",
  "reclamation_raster", "Reclamation area", "Anthropogenic",
  # Topography
  "wc2.1_2.5m_elev", "Elevation (m)", "Topography",
  "slope", "Slope (°)", "Topography",
  "aspect", "Aspect (°)", "Topography",
  # Marine
  "salinity_mean", "Mean sea surface salinity", "Marine",
  "salinity_range", "Sea surface salinity range", "Marine",
  "temperature_mean", "Mean sea surface temperature (°C)", "Marine",
  "temperature_range", "Sea surface temperature range (°C)", "Marine"
)
get_pretty_name <- function(raw_name) {
  # Look up corresponding label in var_info table
  match_idx <- match(raw_name, var_info$raw)

  if (!is.na(match_idx)) {
    return(var_info$label[match_idx]) # Found, return Label
  } else {
    return(raw_name) # Not found, return original name
  }
}
# Based on the CSV column names you provided, I've organized the corresponding variable names
group_list <- list(
  Climate = paste0("wc2.1_2.5m_bio_", 1:19),
  Anthropogenic = c(
    "BUILT_S_E2025_2.5min", "GDP2020_2.5min",
    "Pop2020_2.5min", "VIIRS_2023_2.5min", "reclamation_area"
  ), # This item is likely the suspect

  Soil = c(
    "bdod_mean_2024_2_5min_aligned", "cfvo_mean_2024_2_5min_aligned",
    "clay_mean_2024_2_5min_aligned", "nitrogen_mean_2024_2_5min_aligned",
    "ocd_mean_2024_2_5min_aligned", "phh2o_mean_2024_2_5min_aligned",
    "sand_mean_2024_2_5min_aligned", "silt_mean_2024_2_5min_aligned",
    "soc_mean_2024_2_5min_aligned"
  ),
  Marine = c("salinity_mean", "salinity_range", "temperature_mean", "temperature_range"),
  Geomorphic = c("wc2.1_2.5m_elev", "slope", "aspect")
)

# 3. Loop calculation (with variance detection)
pc_interpretation_list <- list()

cat("=== Starting PCA loading extraction ===\n")

for (grp_name in names(group_list)) {
  vars <- group_list[[grp_name]]

  # A. Check if variables exist
  valid_vars <- vars[vars %in% names(df_city)]

  if (length(valid_vars) > 1) {
    # Extract data matrix
    data_subset <- df_city[, valid_vars]

    # B. Fill NA (fill with mean)
    for (i in 1:ncol(data_subset)) {
      if (any(is.na(data_subset[, i]))) {
        data_subset[is.na(data_subset[, i]), i] <- mean(data_subset[, i], na.rm = TRUE)
      }
    }

    # --- C. [Core fix] Remove zero-variance variables ---
    # Calculate variance for each column
    col_vars <- apply(data_subset, 2, var)

    # Find columns with zero variance (or NA, meaning all NA)
    bad_cols <- names(col_vars)[col_vars == 0 | is.na(col_vars)]

    if (length(bad_cols) > 0) {
      cat("  [Warning] Group", grp_name, "removing invariant variables:", paste(bad_cols, collapse = ", "), "\n")
      # Remove these columns from data
      data_subset <- data_subset[, !names(data_subset) %in% bad_cols, drop = FALSE]
    }

    # D. Check remaining variable count again (PCA needs at least 2 variables)
    if (ncol(data_subset) > 1) {
      # Execute PCA
      pca_res <- prcomp(data_subset, center = TRUE, scale. = TRUE)

      # Extract information
      loadings <- pca_res$rotation[, 1]
      var_exp <- summary(pca_res)$importance[2, 1] * 100

      # Save table
      pc_interpretation_list[[grp_name]] <- data.frame(
        Category = grp_name,
        Variable = names(loadings),
        PC1_Loading = round(loadings, 3),
        PC1_Variance_Explained = paste0(round(var_exp, 1), "%")
      )
    } else {
      cat("  [Skip] Group", grp_name, "has fewer than 2 valid variables, cannot perform PCA.\n")
    }
  }
}

# 4. Merge results
if (length(pc_interpretation_list) > 0) {
  final_pc_table <- do.call(rbind, pc_interpretation_list)
  row.names(final_pc_table) <- NULL

  print(head(final_pc_table))
  write.csv(final_pc_table, "SI_Table_PCA_Loadings_Fixed.csv", row.names = FALSE)
  cat("\nSuccess! Results saved as SI_Table_PCA_Loadings_Fixed.csv\n")
} else {
  cat("\nNo results generated.\n")
}

# Use sapply to batch apply conversion function
final_pc_table$Description <- sapply(final_pc_table$Variable, get_pretty_name)

# ==============================================================================
# 3. Organize table order (beautify)
# ==============================================================================
# Adjust column order, put Description after Variable for easier reading
final_pc_table_polished <- final_pc_table %>%
  select(
    Category,
    Variable,
    Description, # New explanation column in the middle
    PC1_Loading,
    PC1_Variance_Explained
  )

# ==============================================================================
# 4. View and save
# ==============================================================================

cat("=== Final PCA loading table preview ===\n")
print(head(final_pc_table_polished))

write.csv(final_pc_table_polished, publication_path("LOCAL_ANALYSIS_ROOT", "figure/gam_draft/SI_Table_PCA_Loadings_Final.csv"), row.names = FALSE)

city_df_res <- city_df_clean %>%
  mutate(
    res_nonsp = residuals(m_idx_rclass, type = "pearson"),
    res_sp    = residuals(m_spatial, type = "pearson")
  )
coords <- cbind(city_df_res$Longitude, city_df_res$Latitude)

# k nearest neighbors (e.g., k = 4 or 6 both work)
nb <- knearneigh(coords, k = 4)
nb <- knn2nb(nb)

lw <- nb2listw(nb, style = "W", zero.policy = TRUE)
mi_nonsp <- moran.test(city_df_res$res_nonsp, lw, zero.policy = TRUE)
mi_sp <- moran.test(city_df_res$res_sp, lw, zero.policy = TRUE)

mi_nonsp
mi_sp
# correlogram: view Moran's I by neighborhood order (1-6 order neighbors)
cor_nonsp <- sp.correlogram(
  nb, city_df_res$res_nonsp,
  order = 6, method = "I",
  style = "W", zero.policy = TRUE
)

cor_sp <- sp.correlogram(
  nb, city_df_res$res_sp,
  order = 6, method = "I",
  style = "W", zero.policy = TRUE
)
# helper function to convert sp.correlogram results to data.frame
cor2df <- function(cor_obj) {
  # In some versions cor_obj$res is matrix/data.frame, in others it's a list
  res_obj <- cor_obj$res

  if (is.matrix(res_obj) || is.data.frame(res_obj)) {
    # Common structure: column names include "I" and "VarI"
    I_vals <- res_obj[, "I"]
    # Some versions call it "VarI", others "VI", use fallback
    var_name <- intersect(c("VarI", "VI", "varI"), colnames(res_obj))[1]
    var_vals <- res_obj[, var_name]
  } else if (is.list(res_obj)) {
    # Safe approach: each order is a list containing I and VarI
    I_vals <- sapply(res_obj, function(x) x[["I"]])
    var_vals <- sapply(res_obj, function(x) x[["VarI"]])
  } else {
    stop("Unknown structure for cor_obj$res")
  }

  se_vals <- sqrt(var_vals)

  dplyr::tibble(
    order = seq_along(I_vals),
    I     = I_vals,
    se    = se_vals,
    lower = I - 1.96 * se,
    upper = I + 1.96 * se
  )
}
df_nonsp <- cor2df(cor_nonsp) %>% dplyr::mutate(model = "Non-spatial")
df_sp <- cor2df(cor_sp) %>% dplyr::mutate(model = "Spatial")

df_cor <- dplyr::bind_rows(df_nonsp, df_sp)
par(mfrow = c(1, 2))

plot(cor_nonsp,
  main = "Non-spatial model residuals",
  xlab = "Neighbourhood order",
  ylab = "Moran's I"
)

plot(cor_sp,
  main = "Spatial model residuals",
  xlab = "Neighbourhood order",
  ylab = "Moran's I"
)

par(mfrow = c(1, 1))
# Publication portability: environment setup omitted.
png(
  filename = "Supp_Fig_Sy_MoranI_correlogram.png",
  width = 8, # Width (inches)
  height = 6, # Height (inches)
  units = "in", # Units
  res = 300 # Resolution (use res here, not dpi)
)
par(mfrow = c(1, 2))
plot(cor_nonsp,
  main = "Non-spatial model residuals",
  xlab = "Neighbourhood order",
  ylab = "Moran's I"
)
plot(cor_sp,
  main = "Spatial model residuals",
  xlab = "Neighbourhood order",
  ylab = "Moran's I"
)
par(mfrow = c(1, 1))
dev.off()
