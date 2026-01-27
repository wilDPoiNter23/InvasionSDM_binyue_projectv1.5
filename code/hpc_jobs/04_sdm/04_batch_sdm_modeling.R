# ==============================================================================
# Batch Species Distribution Modeling for 800+ Invasive Plant Species
# ==============================================================================
#
# Purpose:
#   Train ensemble Species Distribution Models (SDMs) for 800+ invasive plant
#   species using biomod2 framework. Implements complete SDM workflow including:
#   - Data loading and quality control
#   - Variable selection (correlation + VIF filtering)
#   - Spatial thinning and pseudo-absence generation
#   - Individual model training (GLM, Random Forest, MaxEnt)
#   - Ensemble model creation (EMmean, EMmedian)
#   - Model evaluation and projection
#
# Manuscript Section:
#   - Methods: Species Distribution Modeling Framework (main text)
#   - Methods: Model Evaluation and Selection (main text)
#   - Methods: Variable Selection (main text)
#   - Supplementary Methods: Computational Implementation
#   - Supplementary Table 2: Environmental variables used
#
# Inputs:
#   - Species occurrence data: data/species_occ/{species}_occurrences.csv
#   - Environmental stack: data/environmental/full_stack.tif (40 variables)
#   - Species list: data/script/output/species_list.csv (800+ species)
#   - Batch ID: Command line argument for parallel processing
#
# Outputs:
#   - Individual models: results/individual_models/{species}_individual.rds
#   - Ensemble models: results/ensemble_models/{species}_ensemble.rds
#   - Best ensemble: results/ensemble_models/{species}_best_ensemble.rds
#   - Projections: results/projections/{species}_ensemble_projection.rds
#   - Evaluation reports: results/evaluations/{batch_id}_evaluationTotalReport.csv
#   - Variable selection: results/variable/select/{species}_selection_summary.csv
#   - Correlation matrices: results/variable/correlation/{species}_correlation_matrix.csv
#
# Key Parameters (Manuscript Methods):
#   **Algorithms:**
#   - GLM: Generalized Linear Model
#   - RFd: Random Forest (default settings, ntree=500)
#   - MAXNET: MaxEnt implementation via maxnet package
#
#   **Cross-Validation:**
#   - Strategy: Block CV (spatially structured)
#   - k-folds: 3
#   - Training proportion: 80%
#   - Repetitions: 1
#
#   **Ensemble Methods:**
#   - EMmean: Weighted mean ensemble
#   - EMmedian: Weighted median ensemble
#   - Weighting: TSS-based (models with TSS > 0.8)
#
#   **Model Selection Thresholds:**
#   - Primary: TSS > 0.8 AND ROC > 0.8 (excellent performance)
#   - Fallback: TSS > 0.7 AND ROC > 0.7 (good performance)
#   - Reference: Allouche et al. (2006) for threshold justification
#
#   **Variable Selection:**
#   - Correlation threshold: |r| > 0.8 (pairwise removal)
#   - VIF threshold: VIF > 10 (stepwise removal)
#   - Priority variables (always retained):
#     * reclamation_raster (key predictor)
#     * VIIRS_2023_2.5min (urbanization proxy)
#     * GDP2020_2.5min (economic development)
#     * BUILT_S_E2025_2.5min (urban extent)
#     * Pop2020_2.5min (human pressure)
#
#   **Data Filtering:**
#   - Minimum presence points: 100 (after NA removal)
#   - Maximum presence points: 1,000 (after spatial thinning)
#   - Spatial thinning distance: 10 km (using spThin package)
#   - NA threshold: 10% maximum missing values per variable
#
#   **Pseudo-Absences:**
#   - Number: 1,000 per species
#   - Strategy: Random sampling from study area
#   - Repetitions: 1
#
#   **Computational Settings:**
#   - Parallel cores: 2 per species
#   - Batch size: 29 species per SLURM job
#   - Random seed: 42 (for reproducibility)
#
# HPC Configuration:
#   - Platform: SLURM cluster
#   - Array jobs: Batch processing of species
#   - Memory: 64GB per node recommended
#   - Runtime: ~2-3 weeks for 800+ species
#
# Dependencies:
#   - biomod2 (>= 4.2) - SDM framework
#   - terra (>= 1.7) - Raster operations
#   - dplyr, tidyr - Data manipulation
#   - usdm - VIF calculation
#   - spThin - Spatial thinning
#   - caret - Correlation filtering
#   - parallel, foreach, doParallel - Parallel processing
#
# Author: Binyue Kang
# Institution: [Your Institution]
# Date Created: 2025-02-06
# Last Modified: 2026-01-23
# ==============================================================================

# Load required packages
library(biomod2)
library(dplyr)
library(parallel)
library(foreach)
library(doParallel)
library(terra)
library(tidyr)
library(caret) # For findCorrelation()
library(corrplot) # For correlation visualization
library(usdm) # For VIF calculation
library(DBI)
library(duckdb)
library(spThin) # For spatial thinning
library(pryr) # For memory monitoring
library(ENMeval)

# ==============================================================================
# Configuration and Setup
# ==============================================================================

# Main configuration
# TODO: Update to use config from 00_config.R
config <- list(
  base_dir = "/thfs1/home/qs_songsj1/invasionSDM_binyue",
  save_intermediate = TRUE,
  cleanup_temp = TRUE
)

# Create directory structure for outputs
setup_directories <- function(base_dir) {
  dirs <- c(
    "data/species_occ", # Species occurrence records
    "data/species_formatted", # Biomod2 formatted data objects
    "data/environmental", # Environmental variable stacks
    "results/individual_models", # Individual algorithm results
    "results/ensemble_models", # Ensemble model results
    "results/projections", # Spatial projections
    "results/evaluations", # Model evaluation metrics
    "logs", # Log files
    "temp" # Temporary files
  )

  for (dir in dirs) {
    dir.create(file.path(base_dir, dir), recursive = TRUE, showWarnings = FALSE)
  }

  return(file.path(base_dir, dirs))
}

# Load environmental variable stack (40 variables)
# See DATA_SOURCES.md for complete variable documentation
full_stack <- terra::rast("/thfs1/home/qs_songsj1/data/climate/full_stack.tif")
base_dir <- "/thfs1/home/qs_songsj1/invasionSDM_binyue"

# ==============================================================================
# SLURM Array Job Configuration
# ==============================================================================

# Get batch ID from command line argument
# Usage: Rscript 04_batch_sdm_modeling.R <batch_id>
args <- commandArgs(trailingOnly = TRUE)
batch_id <- as.numeric(args[1])
slurm_array_id <- Sys.getenv("SLURM_ARRAY_TASK_ID", unset = NA)

# Load species list and determine batch range
species_list <- read.csv("/thfs1/home/qs_songsj1/data/script/output/species_list.csv")
species_list <- species_list$species
total_species <- length(species_list)

# Batch configuration: 29 species per job
species_per_batch <- 29
start_idx <- (batch_id - 1) * species_per_batch + 1
end_idx <- min(batch_id * species_per_batch, total_species)

# ==============================================================================
# Logging Functions
# ==============================================================================

# Setup logging system for tracking progress and errors
setup_logging <- function(log_dir) {
  log_file <- file.path(log_dir, paste0(Sys.Date(), "_modeling_Batch_", batch_id, ".log"))

  log_message <- function(..., level = "INFO") {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    msg <- paste(...)
    log_entry <- paste0("[", timestamp, "] [", level, "] ", msg)
    cat(log_entry, "\n")
    cat(log_entry, "\n", file = log_file, append = TRUE)
  }

  return(log_message)
}

# Log detailed status for each species and processing step
log_status <- function(batch_id, species_name, step, status, message) {
  status_log_path <- file.path(base_dir, "logs", "modeling_status_log.csv")
  slurm_job_id <- Sys.getenv("SLURM_JOB_ID", unset = NA)
  slurm_array_id <- Sys.getenv("SLURM_ARRAY_TASK_ID", unset = NA)

  # Create header if file doesn't exist
  if (!file.exists(status_log_path)) {
    write.table(
      data.frame(
        JobID = character(), ArrayID = character(),
        BatchID = character(), Species = character(), Step = character(),
        Status = character(), Message = character(), Timestamp = character()
      ),
      file = status_log_path, sep = ",", row.names = FALSE, col.names = TRUE
    )
  }

  # Append status entry
  write.table(
    data.frame(
      JobID     = slurm_job_id,
      ArrayID   = slurm_array_id,
      BatchID   = batch_id,
      Species   = species_name,
      Step      = step,
      Status    = status,
      Message   = message,
      Timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    ),
    file = status_log_path, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE
  )
}

# Initialize directories and logging
dirs <- setup_directories(config$base_dir)
log_msg <- setup_logging(dirs[8]) # logs directory

# ==============================================================================
# Species Batch Processing Setup
# ==============================================================================

log_msg("Processing batch:", batch_id)
current_species <- species_list[start_idx:end_idx]
log_msg("Batch", batch_id, "processing species indices:", start_idx, "to", end_idx)
log_msg("Current batch species:", paste(current_species, collapse = ", "))

# Set working directory for model outputs
setwd("/thfs1/home/qs_songsj1/invasionSDM_binyue/models")

# ==============================================================================
# Data Quality Control Functions
# ==============================================================================

# Filter environmental variables and presence points based on NA values
# Removes variables with >10% missing data and points with any NA values
# Manuscript reference: Methods - Data Quality Control
filter_env_vars_and_points <- function(resp_xy, env_stack, species_name,
                                       na_threshold = 0.1, min_points = 100) {
  env_vals <- terra::extract(env_stack, resp_xy)

  # Calculate proportion of NA values for each environmental layer
  na_prop <- colMeans(is.na(env_vals))
  drop_layers <- names(na_prop[na_prop > na_threshold])

  if (length(drop_layers) > 0) {
    message(
      "Species ", species_name, " - Removing variables with high NA proportion: ",
      paste(drop_layers, collapse = ", ")
    )
    env_stack <- env_stack[[setdiff(names(env_stack), drop_layers)]]
    env_vals <- terra::extract(env_stack, resp_xy) # Re-extract with filtered stack
  }

  # Remove presence points with any NA values
  valid_idx <- complete.cases(env_vals)
  valid_points <- sum(valid_idx)

  # Check if sufficient valid points remain
  if (valid_points < min_points) {
    message(
      "Species ", species_name, " has only ", valid_points,
      " valid presence points. Skipping modeling (minimum required: ", min_points, ")"
    )
    return(NULL) # Insufficient data for modeling
  }

  list(
    env_stack = env_stack,
    resp_xy = resp_xy[valid_idx, , drop = FALSE],
    env_vals = env_vals[valid_idx, , drop = FALSE]
  )
}


# Function to process presence points (limit to 10000 and thin)
process_presence_points <- function(species_name, resp_xy) {
  original_n <- nrow(resp_xy)
  log_msg("Original presence points for", species_name, ":", original_n)

  # Standardize column names
  if (all(c("decimalLongitude", "decimalLatitude") %in% names(resp_xy))) {
    names(resp_xy) <- c("x", "y")
  }

  # Step 0: If presence points > 10000, pre-sample to 10000
  if (original_n > 10000) {
    log_msg("Pre-sampling to 10000 to prevent memory issues in thinning...")
    set.seed(42)
    sampled_idx <- sample(seq_len(original_n), size = 10000)
    resp_xy <- resp_xy[sampled_idx, ]
    original_n <- 10000
  }

  # Step 1: If original points <= 1000, return directly
  if (original_n <= 1000) {
    resp_var <- rep(1, original_n)
    return(list(resp_xy = resp_xy, resp_var = resp_var))
  }


  # Step 2: Apply thinning (10km)
  log_msg("Applying thinning for", species_name, "...")
  thin_result <- tryCatch(
    {
      thin(
        loc.data = data.frame(Species = species_name, Longitude = resp_xy$x, Latitude = resp_xy$y),
        lat.col = "Latitude", long.col = "Longitude", spec.col = "Species",
        thin.par = 10, # Unit: km
        reps = 1,
        locs.thinned.list.return = TRUE,
        write.files = FALSE
      )
    },
    error = function(e) {
      log_msg("Thinning failed for", species_name, ":", e$message)
      return(NULL)
    }
  )

  # Step 3: Check thinning results
  if (is.null(thin_result) || length(thin_result) == 0 || nrow(thin_result[[1]]) == 0) {
    log_msg("No points left after thinning for", species_name, ", using original data")
    # Fallback: use original data but sample if > 1000
    if (original_n > 1000) {
      set.seed(42)
      sampled_idx <- sample(seq_len(original_n), size = 1000)
      resp_xy <- resp_xy[sampled_idx, ]
    }
    resp_var <- rep(1, nrow(resp_xy))
    return(list(resp_xy = resp_xy, resp_var = resp_var))
  }

  thinned_coords <- thin_result[[1]][, c("Longitude", "Latitude")]
  names(thinned_coords) <- c("x", "y")

  # Step 4: If still > 10000, random sample
  final_n <- nrow(thinned_coords)
  if (final_n > 1000) {
    set.seed(42)
    sampled_idx <- sample(seq_len(final_n), size = 1000)
    thinned_coords <- thinned_coords[sampled_idx, ]
  }

  # Step 5: Return results
  resp_var <- rep(1, nrow(thinned_coords))
  log_msg("Final presence points used for", species_name, ":", nrow(thinned_coords))

  return(list(resp_xy = thinned_coords, resp_var = resp_var))
}
for (i in seq_along(current_species)) {
  species_name <- current_species[i]
  log_msg("Modeling species (", i, "/", length(current_species), "):", species_name, "\n")
  log_msg(
    paste0(
      "Memory before modeling ", species_name, ": ",
      round(pryr::mem_used() / 1024^2, 1), " Mb"
    )
  )
  projection_file <- file.path(base_dir, "results/projections", paste0(species_name, "_ensemble_projection.rds"))

  if (file.exists(projection_file)) {
    log_msg(paste0("Batch ", batch_id, ": Species ", species_name, " - Projection already exists. Skipping modeling..."))
    log_status(batch_id, species_name, "projection_check", "Success", "Projection exists, modeling skipped.")
    next # Skip current iteration, process next species
  }

  tryCatch(
    {
      # Step 2: Load Species Data
      tryCatch(
        {
          log_msg(paste0("Batch ", batch_id, "Step 2: Loading species data..."))
          # Clean species name (remove/replace unsafe characters for file paths)
          clean_name <- gsub("[^A-Za-z0-9_]", "_", species_name)
          # Build full path to CSV file
          species_file <- file.path(base_dir, "data/species_occ", clean_name, paste0(clean_name, "_occurrences.csv"))
          output_directory <- "/thfs1/home/qs_songsj1/invasionSDM_binyue/models"
          species_data <- tryCatch(
            {
              read.csv(species_file)
            },
            error = function(e) {
              warning("Failed to read ", species_file, ": ", e$message)
              return(NULL)
            }
          )

          resp_var <- rep(1, nrow(species_data))
          resp_xy <- species_data[, c("decimalLongitude", "decimalLatitude")]
          # formatted_data <- readRDS('/thfs1/home/qs_songsj1/data/test/result/formatted_data.rds')
          log_msg(paste0("Batch ", batch_id, ": Species ", species_name, "Species data loaded."))
        },
        error = function(e) {
          log_msg(paste0("Batch ", batch_id, ": Species ", species_name, "Error in loading species data:", e$message))
          stop(e)
        }
      )

      # Step 3: Correlation analysis among predictors at species locations
      # Generate pseudo-absence points
      log_msg(paste0("Batch ", batch_id, ": Species ", species_name, "Generating pseudo-absence points..."))

      env_final_path <- file.path(base_dir, "data/environmental", paste0(species_name, "_env_stack_final.rds"))

      if (file.exists(env_final_path)) {
        log_msg(paste0("Batch ", batch_id, ": Species ", species_name, " - env stacks already exist. Skipping selecting."))
        env_stack_final <- readRDS(env_final_path)
      } else {
        log_msg(paste0("Batch ", batch_id, ": Species ", species_name, " - Training individual models..."))


        n_pseudo <- 1000
        pseudo_absence <- spatSample(full_stack[[1]], size = n_pseudo, method = "random", na.rm = TRUE, xy = TRUE)
        colnames(pseudo_absence) <- c("x", "y")

        # # Combine presence + pseudo-absence
        all_points <- rbind(
          data.frame(x = resp_xy$decimalLongitude, y = resp_xy$decimalLatitude, type = "presence"),
          data.frame(x = pseudo_absence$x, y = pseudo_absence$y, type = "pseudoabsence")
        )

        # # Extract environmental values
        log_msg(paste0("Batch ", batch_id, ": Species ", species_name, "Extracting environmental values at presence + pseudo-absence points..."))
        env_vals <- terra::extract(full_stack, all_points[, c("x", "y")])
        env_df <- as.data.frame(env_vals)[, -1] # drop ID column
        priority_vars <- c("reclamation_raster", "VIIRS_2023_2.5min", "GDP2020_2.5min", "BUILT_S_E2025_2.5min", "Pop2020_2.5min")
        orig_vars <- names(full_stack)
        # Pre‐processing to remove any zero‐variance (constant) predictors before correlation
        ## ---------------------------------------------------------------------------
        ##  STEP 1 – remove columns that are entirely NA or perfectly constant
        ## ---------------------------------------------------------------------------
        all_NA <- colSums(!is.na(env_df)) == 0
        zero_SD <- sapply(env_df, function(x) sd(x, na.rm = TRUE) == 0)

        constant_vars <- names(env_df)[all_NA | zero_SD]
        if (length(constant_vars)) {
          log_msg(paste0(
            "Batch ", batch_id, ": Species ", species_name, "Removed all-NA / zero-variance layers: ",
            paste(constant_vars, collapse = ", ")
          ))
          env_df <- env_df[, !(names(env_df) %in% constant_vars), drop = FALSE]
          full_stack <- full_stack[[setdiff(names(full_stack), constant_vars)]]
        }
        # ##------------------------------------------------------------
        # ##  STEP 2 – remove columns with *too few* non-NA samples
        # ##           (e.g. rasters covering a tiny coastal strip)
        # ##------------------------------------------------------------
        min_obs <- 20 # Can be adjusted to 30, 50, etc.
        obs_cnt <- colSums(!is.na(env_df))

        few_obs_vars <- names(obs_cnt)[obs_cnt < min_obs]
        if (length(few_obs_vars)) {
          log_msg(paste0(
            "Batch ", batch_id, ": Species ", species_name, "Removed vars with <", min_obs,
            " non-NA samples: ", paste(few_obs_vars, collapse = ", ")
          ))
          env_df <- env_df[, !(names(env_df) %in% few_obs_vars), drop = FALSE]
          full_stack <- full_stack[[setdiff(names(full_stack), few_obs_vars)]]
        }

        # ## ---------------------------------------------------------------------------
        # ##  STEP 3 – build the correlation matrix and detect any remaining NA columns
        # ## ---------------------------------------------------------------------------
        corr_mat <- stats::cor(env_df, use = "pairwise.complete.obs")
        bad_cols <- character(0) # Initialize bad_cols as empty vector before the conditional check
        if (anyNA(corr_mat)) {
          # identify every column/row that still generates an NA correlation
          bad_cols <- unique(c(
            rownames(corr_mat)[rowSums(is.na(corr_mat)) > 0],
            colnames(corr_mat)[colSums(is.na(corr_mat)) > 0]
          ))

          #   message("Removed layers that still produced NA correlations: ",
          #           paste(bad_cols, collapse = ", "))

          env_df <- env_df[, !(names(env_df) %in% bad_cols), drop = FALSE]
          full_stack <- full_stack[[setdiff(names(full_stack), bad_cols)]]

          ## rebuild the matrix – now guaranteed to be NA-free
          corr_mat <- stats::cor(env_df, use = "pairwise.complete.obs")
        }

        stopifnot(!anyNA(corr_mat)) # safety fuse: will stop if NA sneaks back in


        ## Save the full correlation matrix to CSV
        corr_path <- file.path(base_dir, "results/variable/correlation", paste0(species_name, "PriorityVariablecorrelation_matrix.csv"))
        write.csv(corr_mat, file = corr_path, row.names = TRUE)
        log_msg(paste0("Batch ", batch_id, ": Species ", species_name, paste("Saved correlation matrix to", corr_path)))

        ## find and drop highly correlated variables
        highCorrIdx <- findCorrelation(corr_mat, cutoff = 0.8)
        highCorrVars <- colnames(corr_mat)[highCorrIdx]

        # log_msg(paste("Variables with |r| >= 0.8:", paste(highCorrVars, collapse = ", ")))

        highCorrVars_filtered <- setdiff(highCorrVars, priority_vars)
        env_stack_corrOK <- full_stack[[setdiff(names(full_stack), highCorrVars_filtered)]]

        # log_msg(paste("Remaining variables after correlation filter:", paste(env_stack_corrOK, collapse = ", ")))
        # # ──────────────────────────────────────────────────────────────────────────────
        # # Step 4: VIF screening to further remove multicollinearity
        # # ──────────────────────────────────────────────────────────────────────────────
        log_msg(paste0("Batch ", batch_id, ": Species ", species_name, "Running VIF step (threshold = 10)…"))
        vif_sel <- vifstep(env_stack_corrOK, th = 10)

        # str(vif_sel)
        ## results table
        vif_results <- vif_sel@results

        ## identify dropped and kept
        excluded_vars <- vif_sel@excluded
        dropped_vif <- setdiff(excluded_vars, priority_vars)
        # log_msg(paste("Dropped by VIF:", paste(dropped_vif, collapse = ", ")))
        kept_vif <- union(setdiff(vif_results$Variables, dropped_vif), intersect(excluded_vars, priority_vars))

        # log_msg(paste("Kept after VIF:", paste(kept_vif, collapse = ", ")))
        # # build final stack
        env_stack_final <- env_stack_corrOK[[kept_vif]]

        ## Prepare summary of variable selection
        kept_final <- names(env_stack_final)

        ## Build a data.frame with Variable and Status
        var_status <- data.frame(
          Variable = orig_vars,
          Status = vapply(orig_vars, function(v) {
            if (v %in% constant_vars) {
              "Removed_all_NA_or_zero_variance"
            } else if (v %in% few_obs_vars) {
              "Removed_too_few_samples"
            } else if (v %in% bad_cols) {
              "Removed_NA_in_correlation"
            } else if (v %in% highCorrVars) {
              "Removed_by_pairwise_correlation"
            } else if (v %in% dropped_vif) {
              "Removed_by_VIF"
            } else if (v %in% names(env_stack_final)) {
              "Kept"
            } else {
              "Other"
            }
          }, character(1)),
          stringsAsFactors = FALSE
        )

        ## 3. Save the summary table
        summary_path <- file.path(base_dir, "results/variable/select", paste0(species_name, "PriorityVariable_selection_summary.csv"))
        write.csv(var_status, file = summary_path, row.names = FALSE)
        log_msg(paste0("Batch ", batch_id, ": Species ", species_name, paste("Saved variable selection summary to", summary_path)))

        saveRDS(env_stack_final, file.path(base_dir, "data/environmental", paste0(species_name, "_env_stack_final.rds")))
      }


      rm(env_vals, env_df, all_points)

      ##### filter NA layer

      filter_result <- filter_env_vars_and_points(resp_xy, env_stack_final, species_name)

      if (is.null(filter_result)) {
        log_msg(paste("Batch", batch_id, "Species", species_name, "too few valid points, skipping"))
        log_status(batch_id, species_name, "DataCheck", "TooFewValidPoints", "Valid points below threshold")
        next
      }

      # Update env_stack_final and resp_xy (with NA filtering)
      env_stack_final <- filter_result$env_stack

      ##### check formatted data if exist

      formatted_data_path <- file.path(base_dir, "data/species_formatted", paste0(species_name, "_formatted.rds"))
      if (file.exists(formatted_data_path)) {
        log_msg(paste0("Batch ", batch_id, ": Species ", species_name, " - formatted data already exist. Skipping generating."))
        formatted_data <- readRDS(formatted_data_path)
      } else {
        log_msg(paste0("Batch ", batch_id, ": Species ", species_name, " - generating formatted data..."))
        resp_xy <- filter_result$resp_xy


        #### thin the formatted data (no more than 10000)
        result <- process_presence_points(species_name, resp_xy)

        resp_xy <- result$resp_xy
        resp_var <- result$resp_var



        validate_biomod_inputs <- function(resp_var, resp_xy, expl_var) {
          if (length(resp_var) != nrow(resp_xy)) {
            stop("Error: resp_var and resp_xy have inconsistent row counts")
          }

          extracted <- terra::extract(expl_var, resp_xy)
          bad_rows <- which(rowSums(is.na(extracted)) > 0)

          if (length(bad_rows) > 0) {
            warning(paste("Found", length(bad_rows), "presence points in NA regions, removing them"))
            resp_xy <- resp_xy[-bad_rows, , drop = FALSE]
            resp_var <- resp_var[-bad_rows]
          }

          if (nrow(resp_xy) < 10) {
            stop("Too few valid presence points for modeling")
          }

          return(list(resp_var = resp_var, resp_xy = resp_xy))
        }
        validated <- validate_biomod_inputs(resp_var, resp_xy, env_stack_final)
        resp_var <- validated$resp_var
        resp_xy <- validated$resp_xy

        formatted_data <- tryCatch(
          {
            # if (file.exists(formatted_data_path)) {
            #   log_msg(paste0("Batch ", batch_id, ": Species ", species_name, " - Formatted data already exists. Loading from disk..."))
            #   readRDS(formatted_data_path)
            # } else {
            log_msg(paste0("Batch ", batch_id, ": Species ", species_name, " - Formatting data for BIOMOD2..."))

            formatted_data <- BIOMOD_FormatingData(
              resp.var = resp_var,
              expl.var = env_stack_final,
              resp.xy = resp_xy,
              resp.name = species_name,
              PA.nb.rep = 1,
              PA.nb.absences = 1000,
              PA.strategy = "random",
              # PA.dist.min = 20000,
              # PA.dist.max = 50000,
              na.rm = TRUE
            )

            saveRDS(formatted_data, file = formatted_data_path)
            log_msg(paste0("Batch ", batch_id, ": Species ", species_name, " - Data formatted and saved."))
            formatted_data
            # }
          },
          error = function(e) {
            log_msg(paste0("Batch ", batch_id, ": Species ", species_name, " - Error in formatting or loading data: ", e$message))
            stop(e)
          }
        )
      }

      # # ──────────────────────────────────────────────────────────────────────────────
      # # 5. Modelling!!!
      # # ──────────────────────────────────────────────────────────────────────────────

      log_msg(paste0("Batch ", batch_id, ": Species ", species_name, "Step 5: Setting up modeling options with default parameters"))
      log_msg(
        paste0(
          "Memory before remove ", species_name, ": ",
          round(pryr::mem_used() / 1024^2, 1), " Mb"
        )
      )
      rm(list = c("resp_xy", "resp_var"))
      terra::tmpFiles(remove = TRUE)
      gc()

      # Set default options for all models
      modeling_options <- tryCatch(
        {
          bm_ModelingOptions(
            data.type = "binary",
            models = c("GLM", "RFd", "MAXNET"),
            strategy = "default" # Use default parameters
          )
        },
        error = function(e) {
          log_msg(paste("Error in modeling options:", e$message))
          return(NULL)
        }
      )

      if (is.null(modeling_options)) {
        stop("Failed to create modeling options")
      }
      log_msg("Modeling options created successfully")
      # ============================================================================
      # 5.1 Train individual models
      # ============================================================================
      individual_model_path <- file.path(base_dir, "results/individual_models", paste0(species_name, "_individual.rds"))

      individual_models <- tryCatch(
        {
          if (file.exists(individual_model_path)) {
            log_msg(paste0("Batch ", batch_id, ": Species ", species_name, " - Individual models already exist. Loading from disk..."))
            individual_models <- readRDS(individual_model_path)
          } else {
            log_msg(paste0("Batch ", batch_id, ": Species ", species_name, " - Step 5.1: Training individual models"))
            log_msg(
              paste0(
                "Memory before modeling ", species_name, ": ",
                round(pryr::mem_used() / 1024^2, 1), " Mb"
              )
            )

            individual_models <- BIOMOD_Modeling(
              bm.format = formatted_data,
              modeling.id = paste0(species_name, "MultiModel_Ensemble"),
              models = c("GLM", "RFd", "MAXNET"),
              bm.options = modeling_options,
              CV.strategy = "block",
              CV.nb.rep = 1,
              CV.k = 3,
              CV.perc = 0.8,
              OPT.strategy = "bigboss",
              var.import = 2,
              metric.eval = c("TSS", "ROC"),
              do.full.models = TRUE,
              seed.val = 42,
              nb.cpu = 2
            )

            saveRDS(individual_models, file = individual_model_path)
            log_msg(paste0("Batch ", batch_id, ": Species ", species_name, " - Individual models trained and saved."))
            individual_models
          }
        },
        error = function(e) {
          log_msg(paste0("Batch ", batch_id, ": Species ", species_name, " - Error in individual modeling: ", e$message))
          return(NULL)
        }
      )

      if (is.null(individual_models)) {
        stop("Failed to train individual models")
      }

      individual_evaluation <- get_evaluations(individual_models)

      log_msg(paste0("Batch ", batch_id, ": Species ", species_name, "Individual models trained successfully"))

      # ============================================================================
      # Step 5.2: Evaluate individual model performance
      # ============================================================================
      log_msg(paste0("Batch ", batch_id, ": Species ", species_name, "Step 5.2  Starting model selection based on TSS scores... and algo"))
      log_msg(
        paste0(
          "Memory after modeling ", species_name, ": ",
          round(pryr::mem_used() / 1024^2, 1), " Mb"
        )
      )
      # Get model evaluation results

      # Step 1: keep TSS AND ROC
      tss_roc_df <- individual_evaluation %>%
        filter(metric.eval %in% c("TSS", "ROC")) %>%
        select(full.name, algo, PA, run, metric.eval, validation)

      # # Step 2: reshape table
      tss_roc_summary <- tss_roc_df %>%
        pivot_wider(names_from = metric.eval, values_from = validation) %>%
        arrange(desc(TSS))

      good_models <- tss_roc_summary %>%
        filter(TSS > 0.8, ROC > 0.8)

      if (nrow(good_models) == 0) {
        good_models <- tss_roc_summary %>%
          filter(TSS > 0.7, ROC > 0.7)

        # If still no suitable models, stop workflow and log failure
        if (nrow(good_models) == 0) {
          log_msg(paste0(
            "Batch ", batch_id, ": Species ", species_name,
            " - Modeling failed: No model meets TSS > 0.7 and ROC > 0.7 criteria."
          ))
          log_status(batch_id, species_name, "evaluation", "Failed", "No model meets TSS > 0.7 and ROC > 0.7")
          stop("No valid models for ensemble building.")
        } else {
          log_msg(paste0(
            "Batch ", batch_id, ": Species ", species_name,
            " - Using models with TSS > 0.7 and ROC > 0.7 (fallback)."
          ))
        }
      } else {
        log_msg(paste0(
          "Batch ", batch_id, ": Species ", species_name,
          " - Using models with TSS > 0.8 and ROC > 0.8."
        ))
      }

      selected_models <- good_models$full.name

      log_msg(paste("Final selected models:", length(selected_models)))
      # log_msg(paste("Memory reduction: from", length(get_built_models(individual_models)),
      #                  "to", length(selected_models), "models"))
      #    log_msg("Best performing models for each metric:")
      #   print(selected_models)
      log_msg(
        paste0(
          "Memory after modeling ", species_name, ": ",
          round(pryr::mem_used() / 1024^2, 1), " Mb"
        )
      )
      # ============================================================================
      # Step 5.3: Create ensemble models
      # ============================================================================

      log_msg(paste0("Batch ", batch_id, ": Species ", species_name, "Step 5.3 :Starting ensemble modeling with selected models..."))

      bestensembal_model_path <- file.path(base_dir, "results/ensemble_models", paste0(species_name, "_best_ensemble.rds"))
      ensembal_model_path <- file.path(base_dir, "results/ensemble_models", paste0(species_name, "_ensemble.rds"))

      if (file.exists(bestensembal_model_path)) {
        log_msg(paste0("Batch ", batch_id, ": Species ", species_name, " - bestensembal_model models already exist. Loading from disk..."))
        best_ensemble_model <- readRDS(bestensembal_model_path)
        ensemble_models <- readRDS(ensembal_model_path)
      } else {
        ## Set ensemble modeling options
        ensemble_options <- tryCatch(
          {
            bm_ModelingOptions(
              data.type = "binary",
              models = "EMmean", # Mean ensemble
              strategy = "default"
            )
          },
          error = function(e) {
            log_msg(paste0("Batch ", batch_id, ": Species ", species_name, paste("Error in ensemble options:", e$message)))
            return(NULL)
          }
        )

        ## Create ensemble models
        ensemble_models <- tryCatch(
          {
            BIOMOD_EnsembleModeling(
              bm.mod = individual_models,
              models.chosen = selected_models, # Use tss and roc both over 0.8 models
              em.by = "all", # Ensemble method
              em.algo = c("EMmean", "EMmedian"), # Multiple ensemble algorithms
              metric.select = c("TSS"), # Metric for model selection
              metric.select.thresh = c(0.8), # TSS threshold
              metric.eval = c("TSS", "ROC", "KAPPA"),
              var.import = 3,
              EMci.alpha = 0.05,
              EMwmean.decay = "proportional"
            )
          },
          error = function(e) {
            log_msg(paste0("Batch ", batch_id, ": Species ", species_name, paste("Error in ensemble modeling:", e$message)))
            return(NULL)
          }
        )

        if (!is.null(ensemble_models)) {
          log_msg(paste0("Batch ", batch_id, ": Species ", species_name, "Ensemble models created successfully"))
          log_msg(paste0("Batch ", batch_id, ": Species ", species_name, paste("Built ensemble models:", length(get_built_models(ensemble_models)))))
          saveRDS(ensemble_models, file = file.path(base_dir, "results/ensemble_models", paste0(species_name, "_ensemble.rds")))
        }

        # =============================================================================
        # Step 5.4: Model projection (only best ensemble model)
        # =============================================================================

        log_msg(paste0("Batch ", batch_id, ": Species ", species_name, "Step 5.4: Model projection"))

        # Check if projection_data (a raster stack of environmental variables) is defined
        if (exists("env_stack_final") && !is.null(env_stack_final)) {
          log_msg(paste0("Batch ", batch_id, ": Species ", species_name, "Projection data detected. Starting best ensemble model projection..."))

          # ---------------------------------------------------------------------------
          # Step 5.5: Select the best ensemble model based on composite score
          # ---------------------------------------------------------------------------
          if (!is.null(ensemble_models)) {
            log_msg(paste0("Batch ", batch_id, ": Species ", species_name, "5.5 Evaluating ensemble models to identify the best..."))

            # Extract evaluation results (as a data.frame)
            eval_df <- tryCatch(
              {
                get_evaluations(ensemble_models)
              },
              error = function(e) {
                log_msg(paste0("Batch ", batch_id, ": Species ", species_name, " - Error getting evaluations: ", e$message))
                data.frame() # Return empty dataframe
              }
            )

            if (nrow(eval_df) == 0) {
              best_ensemble_model <- selected_models
              log_msg(paste("No ensemble evaluation results, using best individual model:", best_ensemble_model))
              log_msg(paste0("Batch ", batch_id, ": Species ", species_name, paste("Best ensemble model selected:", best_ensemble_model)))
            } else {
              # If it's already a data.frame (which it should be), proceed
              if (is.data.frame(eval_df)) {
                # Make sure required columns exist
                if (all(c("full.name", "sensitivity", "specificity", "calibration") %in% colnames(eval_df))) {
                  # Convert columns to numeric (just in case)
                  eval_df$sensitivity <- as.numeric(eval_df$sensitivity)
                  eval_df$specificity <- as.numeric(eval_df$specificity)
                  eval_df$calibration <- as.numeric(eval_df$calibration)

                  # Compute composite score
                  eval_df$composite_score <- rowMeans(
                    eval_df[, c("sensitivity", "specificity", "calibration")],
                    na.rm = TRUE
                  )

                  # Identify best ensemble model
                  best_row <- eval_df[which.max(eval_df$composite_score), ]
                  best_ensemble_model <- best_row$full.name
                } else {
                  stop(" Required metrics (sensitivity, specificity, calibration) not found in evaluation output.")
                }
              } else {
                stop(" get_evaluations() did not return a data.frame. Please inspect manually.")
              }
            }
          }
        }

        saveRDS(best_ensemble_model, file = file.path(base_dir, "results/ensemble_models", paste0(species_name, "_best_ensemble.rds")))
        log_msg(paste0("Batch ", batch_id, ": Species ", species_name, paste("Best ensemble model selected:", best_ensemble_model)))
      }

      # ============================================================================
      # 6: Generate comprehensive evaluation report
      # ============================================================================

      log_msg(paste0("Batch ", batch_id, ": Species ", species_name, "6: Generating comprehensive evaluation report"))

      eval_df <- tryCatch(
        {
          get_evaluations(ensemble_models)
        },
        error = function(e) {
          log_msg(paste0("Batch ", batch_id, ": Species ", species_name, " - Error getting evaluations: ", e$message))
          data.frame() # Return empty data frame
        }
      )


      # Helper function
      # Initialize master results CSV file
      initialize_master_results <- function(output_dir) {
        # Check if output_dir exists and is a directory
        if (!dir.exists(output_dir)) {
          dir.create(output_dir, recursive = TRUE)
          log_msg(paste("Created output directory:", output_dir))
        }

        # Ensure we're working with a directory, not a file
        if (file.exists(output_dir) && !dir.exists(output_dir)) {
          stop(paste("Output path exists but is not a directory:", output_dir))
        }
        master_results_file <- file.path(output_dir, paste0(batch_id, "_evaluationTotalReport.csv"))
        # Create header if file doesn't exist
        if (!file.exists(master_results_file)) {
          # Create empty dataframe with exact column structure
          master_df <- data.frame(
            Species = character(),
            Type = character(),
            full.name = character(),
            PA = character(),
            run = character(),
            algo = character(),
            metric.eval = character(),
            cutoff = numeric(),
            sensitivity = numeric(),
            specificity = numeric(),
            calibration = numeric(),
            validation = numeric(),
            evaluation = numeric(),
            merged.by.PA = character(),
            merged.by.run = character(),
            merged.by.algo = character(),
            filtered.by = character(),
            stringsAsFactors = FALSE
          )
          write.csv(master_df, master_results_file, row.names = FALSE)
          log_msg(paste("Initialized master results file:", master_results_file))
        }

        return(master_results_file)
      }
      format_biomod_evaluation <- function(individual_evaluation,
                                           eval_df = NULL,
                                           species_name = NULL,
                                           individual_models = NULL) {
        # Extract species name
        log_msg(paste0("Formatting evaluation for species: ", species_name))
        results_list <- list()

        # Process individual model evaluations
        if (!is.null(individual_evaluation) && nrow(individual_evaluation) > 0) {
          log_msg(paste0("Processing ", nrow(individual_evaluation), " individual evaluation records"))
          # Add Species and Type columns
          individual_df <- individual_evaluation %>%
            mutate(
              Species = species_name,
              Type = "individual"
            )

          # Ensure all required columns exist, add missing ones with NA
          required_cols <- c(
            "Species", "Type", "full.name", "PA", "run", "algo",
            "metric.eval", "cutoff", "sensitivity", "specificity",
            "calibration", "validation", "evaluation", "merged.by.PA",
            "merged.by.run", "merged.by.algo", "filtered.by"
          )

          for (col in required_cols) {
            if (!col %in% colnames(individual_df)) {
              individual_df[[col]] <- NA
            }
          }

          # Reorder columns to match your desired structure
          individual_df <- individual_df %>%
            select(all_of(required_cols))

          results_list[["individual"]] <- individual_df
        }

        # Process ensemble model evaluations
        if (!is.null(eval_df) && nrow(eval_df) > 0) {
          log_msg(paste0("Processing ", nrow(eval_df), " ensemble evaluation records"))
          # Add Species and Type columns
          ensemble_df <- eval_df %>%
            mutate(
              Species = species_name,
              Type = "ensemble"
            )

          # Ensure all required columns exist
          required_cols <- c(
            "Species", "Type", "full.name", "PA", "run", "algo",
            "metric.eval", "cutoff", "sensitivity", "specificity",
            "calibration", "validation", "evaluation", "merged.by.PA",
            "merged.by.run", "merged.by.algo", "filtered.by"
          )

          for (col in required_cols) {
            if (!col %in% colnames(ensemble_df)) {
              ensemble_df[[col]] <- NA
            }
          }

          # Reorder columns
          ensemble_df <- ensemble_df %>%
            select(all_of(required_cols))

          results_list[["ensemble"]] <- ensemble_df
        }

        # Combine all results
        if (length(results_list) > 0) {
          final_df <- bind_rows(results_list)
          return(final_df)
        } else {
          return(NULL)
        }
      }

      # Function to append results to master CSV file
      append_species_direct <- function(species_evaluation_df, master_file_path) {
        if (is.null(species_evaluation_df) || nrow(species_evaluation_df) == 0) {
          log_msg("No evaluation data to append")
          return()
        }

        tryCatch(
          {
            # Check if file exists
            if (file.exists(master_file_path)) {
              # Append without column names
              write.table(species_evaluation_df, master_file_path,
                sep = ",", row.names = FALSE, col.names = FALSE,
                append = TRUE, na = "NA"
              )
            } else {
              # Create new file with column names
              write.csv(species_evaluation_df, master_file_path, row.names = FALSE, na = "NA")
            }

            log_msg(paste0("Batch ", batch_id, ": Species ", species_name, paste("Results appended directly. Species rows:", nrow(species_evaluation_df))))
          },
          error = function(e) {
            log_msg(paste0("Batch ", batch_id, ": Species ", species_name, paste("Error in direct append:", e$message)))

            # Fallback
            backup_file <- gsub("\\.csv$", paste0("_", species_name, "_backup.csv"), "/thfs1/home/qs_songsj1/invasionSDM_binyue/temp")
            write.csv(species_evaluation_df, backup_file, row.names = FALSE)
            log_msg(paste("Saved species backup file:", backup_file))
          }
        )
      }

      # Initialize master results file (run once at the start of your entire workflow)
      master_file <- initialize_master_results("/thfs1/home/qs_songsj1/invasionSDM_binyue/results/evaluations/comprehensive")

      # Extract species name from your formatted data
      # Adjust this line based on how you extract species name from formatteddata
      species_name <- individual_models@sp.name # or however you get the species name

      # Format evaluation data to match your desired table structure
      formatted_evaluation <- format_biomod_evaluation(
        individual_evaluation = individual_evaluation,
        eval_df = eval_df, # your ensemble evaluation data
        species_name = species_name,
        individual_models = individual_models
      )

      append_species_direct(formatted_evaluation, master_file)

      # Print concise summary
      log_msg(paste0("Batch ", batch_id, ": Species ", species_name, "Workflow completed successfully!"))
      log_msg(paste0("Batch ", batch_id, ": Species ", species_name, paste("Results saved in:", "results/evaluations/comprehensive")))

      # ============================================================================
      # 7: Variable importance for best ensemble model
      # ============================================================================
      extract_variable_importance_any <- function(individual_models,
                                                  ensemble_models,
                                                  best_ensemble_model,
                                                  species_name,
                                                  batch_id,
                                                  base_dir) {
        log_msg(paste0("Batch ", batch_id, ": Species ", species_name, " - Extracting variable importance..."))

        # Helper: Extract and process variable importance
        get_model_importance <- function(bm_object, source_label) {
          var_imp <- tryCatch(
            {
              get_variables_importance(bm_object)
            },
            error = function(e) {
              log_msg(paste0("Batch ", batch_id, ": ", species_name, " - Failed to get var imp from ", source_label, ": ", e$message))
              return(NULL)
            }
          )

          if (is.null(var_imp)) {
            return(NULL)
          }

          if (!("full.name" %in% colnames(var_imp))) {
            log_msg(paste0("Batch ", batch_id, ": ", species_name, " - No full.name column in var_imp from ", source_label))
            return(NULL)
          }

          if (!(best_ensemble_model %in% var_imp$full.name)) {
            log_msg(paste0("Batch ", batch_id, ": ", species_name, " - best_ensemble_model not in var_imp full.name from ", source_label))
            return(NULL)
          }

          imp_best <- var_imp %>%
            dplyr::filter(full.name == best_ensemble_model)

          imp_summary <- imp_best %>%
            dplyr::group_by(expl.var) %>%
            dplyr::summarise(
              MeanImportance = mean(var.imp, na.rm = TRUE),
              .groups = "drop"
            )

          importance_df <- data.frame(
            Species = species_name,
            Variable = imp_summary$expl.var,
            Importance = as.numeric(imp_summary$MeanImportance),
            Source = source_label,
            stringsAsFactors = FALSE
          )

          return(importance_df)
        }

        # Determine which model type it belongs to
        ensemble_model_names <- tryCatch(get_built_models(ensemble_models), error = function(e) character(0))
        individual_model_names <- tryCatch(get_built_models(individual_models), error = function(e) character(0))

        importance_df <- NULL

        if (best_ensemble_model %in% ensemble_model_names) {
          importance_df <- get_model_importance(ensemble_models, "ensemble")
        } else if (best_ensemble_model %in% individual_model_names) {
          importance_df <- get_model_importance(individual_models, "individual")
        } else {
          log_msg(paste0("Batch ", batch_id, ": ", species_name, " - best_ensemble_model not found in any model set"))
          return(invisible(NULL))
        }

        if (is.null(importance_df)) {
          log_msg(paste0("Batch ", batch_id, ": ", species_name, " - Variable importance extraction returned NULL"))
          return(invisible(NULL))
        }

        # Sort by importance
        importance_df <- importance_df[order(importance_df$Importance, decreasing = TRUE), ]

        # Save path
        output_file <- file.path(base_dir, "results/evaluations/importance", paste0(batch_id, "_importanceTotalReport.csv"))

        # Write to file
        if (!file.exists(output_file)) {
          write.csv(importance_df, output_file, row.names = FALSE)
          log_msg(paste("Created new variable importance file:", output_file))
        } else {
          write.table(importance_df, output_file,
            sep = ",",
            append = TRUE, row.names = FALSE, col.names = FALSE
          )
          log_msg(paste0(
            "Batch ", batch_id, ": ", species_name,
            " - Appended variable importance to file"
          ))
        }

        return(invisible(importance_df))
      }

      extract_variable_importance_any(
        individual_models = individual_models,
        ensemble_models = ensemble_models,
        best_ensemble_model = best_ensemble_model,
        species_name = species_name,
        batch_id = batch_id,
        base_dir = base_dir
      )



      # -------------------------------------------------------------------------
      # Step 5.6: Run projection for the best ensemble model
      # -------------------------------------------------------------------------
      log_msg(paste0("Batch ", batch_id, ": Species ", species_name, "Step 5.6: Run projection for the best ensemble model"))

      rm(list = c(
        "resp_xy", "resp_var", "formatted_data", "individual_evaluation",
        "ensemble_projections",
        "tss_roc_df", "tss_roc_summary", "good_models", "selected_models"
      ))

      gc()
      # Run projection if valid data and model exist
      if (!is.null(env_stack_final) && inherits(env_stack_final, c("RasterStack", "SpatRaster")) &&
        !is.null(best_ensemble_model) && length(best_ensemble_model) > 0 && nzchar(best_ensemble_model)) {
        if (nrow(eval_df) == 0) {
          fallback_proj <- BIOMOD_Projection(
            bm.mod = individual_models,
            proj.name = "fallback_projection",
            new.env = env_stack_final,
            models.chosen = best_ensemble_model,
            build.clamping.mask = FALSE,
            do.stack = FALSE,
            keep.in.memory = FALSE
          )
          saveRDS(fallback_proj, file = file.path(base_dir, "results/projections", paste0(species_name, "_fallback_projection.rds")))
          log_msg(paste0("Batch ", batch_id, ": Species ", species_name, " Projection for best model completed successfully."))
          log_status(batch_id, species_name, "FALLBACK", "successd!", "fallback projection successfully")
          #  --- Binary Prediction after fallback ---
          cutoffs <- get_evaluations(individual_models)["TSS", , best_ensemble_model]

          # Build path to fallback projection folder
          proj_dir <- file.path(base_dir, "models", species_name, "proj_fallback_projection", "individual_projections")

          # Find prediction tif file (with model name)
          tif_path <- list.files(
            path = proj_dir,
            pattern = paste0(".*", best_ensemble_model, "\\.tif$"),
            full.names = TRUE
          )
          if (length(tif_path) == 1 && !is.na(cutoffs)) {
            fallback_raster <- terra::rast(tif_path)
            binary_raster <- fallback_raster > cutoffs
            terra::writeRaster(
              binary_raster,
              filename = sub("\\.tif$", "_bin.tif", tif_path),
              overwrite = TRUE
            )
            log_msg(paste0("Binary fallback prediction saved at: ", sub("\\.tif$", "_bin.tif", tif_path)))
          } else {
            log_msg("Fallback projection file or cutoff missing, skipping binary output.")
          }
        } else {
          ensemble_projections <- tryCatch(
            {
              BIOMOD_EnsembleForecasting(
                bm.em = ensemble_models,
                proj.name = "ensemble_projection",
                new.env = env_stack_final,
                models.chosen = best_ensemble_model,
                compress = TRUE,
                metric.binary = "TSS",
                metric.filter = "TSS",
                build.clamping.mask = FALSE,
                do.stack = TRUE,
                keep.in.memory = FALSE,
                output.format = ".grd"
              )
            },
            error = function(e) {
              log_msg(paste0("Batch ", batch_id, ": Species ", species_name, paste("Error in ensemble projection:", e$message)))
              return(NULL)
            }
          )
        }

        if (!is.null(ensemble_projections)) {
          saveRDS(ensemble_projections, file = file.path(base_dir, "results/projections", paste0(species_name, "_ensemble_projection.rds")))
          log_msg(paste0("Batch ", batch_id, ": Species ", species_name, " Projection for best ensemble model completed successfully."))
          log_status(batch_id, species_name, "SDM", "successd!", "Projection for best ensemble model completed successfully")
        } else {
          log_msg(paste0("Batch ", batch_id, ": Species ", species_name, " Projection failed. No RDS file saved."))
          log_status(batch_id, species_name, "SDM", "Failed", e$message)
        }
      } else {
        log_msg(paste0("Batch ", batch_id, ": Species ", species_name, " Projection skipped: Missing or invalid projection data or ensemble model."))
      }


      log_msg("species", species_name, "modeling completed\n")
    },
    error = function(e) {
      log_msg("species", species_name, "modeling failed:", e$message, "\n")
    }
  )
  log_msg(
    paste0(
      "Memory after modeling ", species_name, ": ",
      round(pryr::mem_used() / 1024^2, 1), " Mb"
    )
  )

  # Memory cleanup between chunks
  rm(list = c("ensemble_projections", "best_ensemble_model", "env_stack_final"))
  terra::tmpFiles(remove = TRUE)
  gc()
}

log_msg("batch", batch_id, "All complete! Processed", length(current_species), "species\n")
