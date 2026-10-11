# ARCHIVE: historical reference; symbolic paths are intentionally not runnable.
# Original SHA256: 07aa358a10016525e8746bb05ec6fd57121092a6c618adb49277f33ae57a1a53
# ==============================================================================
# Project Configuration File for InvasionSDM
# ==============================================================================
#
# Purpose:
#   Centralized configuration for paths, parameters, and project settings.
#   Source this file at the beginning of each script to ensure consistent
#   paths and settings across the entire workflow.
#
# Usage:
#   source("code_clean/00_setup/00_config.R")
#   data <- read.csv(file.path(config$data_processed, "myfile.csv"))
#
# Author: Binyue Kang
# Date Created: 2026-01-23
# ==============================================================================

# Load required packages
if (!require("here")) install.packages("here")
library(here)

# ==============================================================================
# Directory Configuration
# ==============================================================================

config <- list(
    # Project root (automatically detected)
    project_root = here::here(),

    # Data directories
    data_raw = here::here("data", "raw"),
    data_processed = here::here("data", "processed"),
    data_species = here::here("data", "species_occ"),
    data_environmental = here::here("data", "environmental"),
    data_species_formatted = here::here("data", "species_formatted"),

    # Results directories
    results_models = here::here("results", "individual_models"),
    results_ensemble = here::here("results", "ensemble_models"),
    results_projections = here::here("results", "projections"),
    results_evaluations = here::here("results", "evaluations"),
    results_figures = here::here("results", "figures"),
    results_tables = here::here("results", "tables"),
    results_variable = here::here("results", "variable"),

    # Log directory
    logs = here::here("logs"),

    # Temporary files
    temp = here::here("temp")
)

# ==============================================================================
# Modeling Parameters (for consistency across scripts)
# ==============================================================================

config$modeling <- list(
    # SDM algorithms
    algorithms = c("GLM", "RFd", "MAXNET"),

    # Cross-validation settings
    cv_strategy = "block",
    cv_k = 3,
    cv_perc = 0.8,
    cv_rep = 1,

    # Ensemble settings
    ensemble_methods = c("EMmean", "EMmedian"),

    # Model selection thresholds
    tss_primary = 0.8,
    roc_primary = 0.8,
    tss_fallback = 0.7,
    roc_fallback = 0.7,

    # Data filtering
    min_presence_points = 100,
    max_presence_points = 1000,
    thinning_distance_km = 10,
    na_threshold = 0.1,

    # Pseudo-absence settings
    n_pseudo_absences = 1000,
    pa_strategy = "random",

    # Variable selection
    correlation_threshold = 0.8,
    vif_threshold = 10,
    priority_vars = c(
        "reclamation_raster", "VIIRS_2023_2.5min",
        "GDP2020_2.5min", "BUILT_S_E2025_2.5min",
        "Pop2020_2.5min"
    ),

    # Random seed for reproducibility
    seed = 42,

    # Computational settings
    n_cores = 2
)

# ==============================================================================
# Create Directories if They Don't Exist
# ==============================================================================

create_directories <- function(config) {
    # Extract all directory paths from config
    dirs_to_create <- unlist(config[!names(config) %in% c("modeling", "project_root")])

    for (dir_path in dirs_to_create) {
        if (!dir.exists(dir_path)) {
            dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
            message("Created directory: ", dir_path)
        }
    }
}

# Create directories
create_directories(config)

# ==============================================================================
# Helper Functions
# ==============================================================================

# Function to get full path
get_path <- function(...) {
    file.path(...)
}

# Function to check if file exists and warn if not
check_file <- function(filepath, stop_on_missing = FALSE) {
    if (!file.exists(filepath)) {
        msg <- paste("File not found:", filepath)
        if (stop_on_missing) {
            stop(msg)
        } else {
            warning(msg)
            return(FALSE)
        }
    }
    return(TRUE)
}

# ==============================================================================
# Export Configuration
# ==============================================================================

# Save config for reference
saveRDS(config, here::here("config.rds"))

# Print configuration summary
cat("\n=== InvasionSDM Configuration Loaded ===\n")
cat("Project root:", config$project_root, "\n")
cat("Modeling algorithms:", paste(config$modeling$algorithms, collapse = ", "), "\n")
cat(
    "TSS threshold:", config$modeling$tss_primary, "(primary),",
    config$modeling$tss_fallback, "(fallback)\n"
)
cat("Random seed:", config$modeling$seed, "\n")
cat("========================================\n\n")
