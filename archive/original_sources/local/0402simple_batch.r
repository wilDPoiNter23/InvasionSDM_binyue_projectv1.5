# Original source: 0402simple_batch.r
# Original source location: <LOCAL_CODE_ROOT>/0402simple_batch.r
# Original source SHA-256: 094577875ecc7e183d247bceaeb1be6c05c12310408b36a612844f46ba9aebdc
# Public-safe original snapshot: archive/original_sources/local/0402simple_batch.r
# Classification: SUPPORTING
# Publication modifications:
# - machine-specific absolute path literals replaced by configured publication_path aliases
# - local encoding decoded to UTF-8; source bytes hashed above
# - comments and historical private machine paths sanitized
# - working-directory calls retained with configured paths; no analyses executed
# Scientific modification: NONE
# This is full historical source, not a certified unattended/reproducible run.
# Snapshot for inspection; path placeholders are not executable locations.
# ============================================================================
# Batch Species Distribution Modeling (simple ONLY!!) for 800+ Species
# Optimized for High Performance Computing - FIXED VERSION
# Author: Binyue Kang 03/06/2025
# 1000 records and EMmean only
# ============================================================================

library(biomod2)
library(dplyr)
library(parallel)
library(foreach)
library(doParallel)
library(terra)
library(tidyr)
library(caret)      # for findCorrelation()
library(corrplot)   # for corrplot()
library(usdm)
library(DBI)
library(duckdb)
library(spThin)

# ============================================================================
# Configuration and Setup
# ============================================================================

# Main configuration
config <- list(
  base_dir = "<HISTORICAL_PATH_005>",
  save_intermediate = TRUE,
  cleanup_temp = TRUE
)

# Create directory structure
setup_directories <- function(base_dir) {
  dirs <- c(
    "data/species_occ",
    "data/species_formatted",      # Individual species biomod2 data
    "data/environmental",          # Environmental variables
    "results/individual_models",   # Individual model results
    "results/ensemble_models",     # Ensemble model results
    "results/projections",         # Projection results
    "results/evaluations",         # Evaluation summaries
    "results/evaluations/comprehensive", # Comprehensive evaluations
    "results/evaluations/importance",    # Variable importance
    "results/variable/correlation",      # Correlation matrices
    "results/variable/select",           # Variable selection
    "logs",                        # Log files
    "temp",                         # Temporary files
    'models'
  )

  for (dir in dirs) {
    dir.create(file.path(base_dir, dir), recursive = TRUE, showWarnings = FALSE)
  }
  
  return(file.path(base_dir, dirs))
}

# Load environmental data ONCE at the beginning
full_stack_path <- "<HPC_INPUT_ROOT>/climate/full_stack.tif"
if (!file.exists(full_stack_path)) {
  stop("Environmental data file not found: ", full_stack_path)
}

# Load and validate environmental data
full_stack <- tryCatch({
  terra::rast(full_stack_path)
}, error = function(e) {
  stop("Failed to load environmental data: ", e$message)
})

# Validate raster stack
if (is.null(full_stack) || terra::nlyr(full_stack) == 0) {
  stop("Environmental raster stack is empty or invalid")
}

base_dir <- "<HISTORICAL_PATH_005>"

# Set up the batch id
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  batch_id <- 1  # Default for testing
  warning("No batch_id provided, using default: 1")
} else {
  batch_id <- as.numeric(args[1])
  if (is.na(batch_id)) {
    stop("batch_id must be a number")
  }
}

# Load species list
species_list_path <- "<HPC_INPUT_ROOT>/script/output/species_list.csv"
if (!file.exists(species_list_path)) {
  stop("Species list file not found: ", species_list_path)
}

species_df <- read.csv(species_list_path)
if (!"species" %in% names(species_df)) {
  stop("Species list must have a 'species' column")
}

species_list <- species_df$species
total_species <- length(species_list)

species_per_batch <- 28
start_idx <- (batch_id - 1) * species_per_batch + 1
end_idx <- min(batch_id * species_per_batch, total_species)

# Validate indices
if (start_idx > total_species) {
  stop("Batch ", batch_id, " exceeds total species count (", total_species, ")")
}

# Setup logging
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

# Initialize directories and logging
dirs <- setup_directories(config$base_dir)
log_msg <- setup_logging(file.path(config$base_dir, "logs"))

# ============================================================================
# Species List Management
# ============================================================================

log_msg("Processing batch:", batch_id)
current_species <- species_list[start_idx:end_idx]
log_msg("Batch", batch_id, "processing species indices:", start_idx, "to", end_idx)
log_msg("Current batch species:", paste(current_species, collapse = ", "))

setwd(file.path(config$base_dir, "models"))
if (!dir.exists(getwd())) {
  dir.create(getwd(), recursive = TRUE)
}

# ============================================================================
# Helper Functions
# ============================================================================

# Function to process presence points (limit to 10000 and thin)
process_presence_points <- function(species_name, resp_xy) {
  original_n <- nrow(resp_xy)
  log_msg("Original presence points for", species_name, ":", original_n)

  # Standardize column names
  if (all(c("decimalLongitude", "decimalLatitude") %in% names(resp_xy))) {
    names(resp_xy) <- c("x", "y")
  }

  # Step 0: If presence points > 20000, pre-sample to 20000
  if (original_n > 30000) {
    log_msg("Pre-sampling to 20000 to prevent memory issues in thinning...")
    set.seed(42)
    sampled_idx <- sample(seq_len(original_n), size = 30000)
    resp_xy <- resp_xy[sampled_idx, ]
    original_n <- 30000
  }


  # Step 1: If original points <= 10000, return directly
  if (original_n <= 10000) {
    resp_var <- rep(1, original_n)
    return(list(resp_xy = resp_xy, resp_var = resp_var))
  }

  # Step 2: Apply thinning (10km)
  log_msg("Applying thinning for", species_name, "...")
  thin_result <- tryCatch({
    thin(
      loc.data = data.frame(Species = species_name, Longitude = resp_xy$x, Latitude = resp_xy$y),
      lat.col = "Latitude", long.col = "Longitude", spec.col = "Species",
      thin.par = 10,     # Unit: km
      reps = 1,
      locs.thinned.list.return = TRUE,
      write.files = FALSE
    )
  }, error = function(e) {
    log_msg("Thinning failed for", species_name, ":", e$message)
    return(NULL)
  })

  # Step 3: Check thinning results
  if (is.null(thin_result) || length(thin_result) == 0 || nrow(thin_result[[1]]) == 0) {
    log_msg("No points left after thinning for", species_name, ", using original data")
    # Fallback: use original data but sample if > 10000
    if (original_n > 10000) {
      set.seed(42)
      sampled_idx <- sample(seq_len(original_n), size = 10000)
      resp_xy <- resp_xy[sampled_idx, ]
    }
    resp_var <- rep(1, nrow(resp_xy))
    return(list(resp_xy = resp_xy, resp_var = resp_var))
  }

  thinned_coords <- thin_result[[1]][, c("Longitude", "Latitude")]
  names(thinned_coords) <- c("x", "y")

  # Step 4: If still > 10000, random sample
  final_n <- nrow(thinned_coords)
  if (final_n > 10000) {
    set.seed(42)
    sampled_idx <- sample(seq_len(final_n), size = 1000)
    thinned_coords <- thinned_coords[sampled_idx, ]
  }

  # Step 5: Return results
  resp_var <- rep(1, nrow(thinned_coords))
  log_msg("Final presence points used for", species_name, ":", nrow(thinned_coords))

  return(list(resp_xy = thinned_coords, resp_var = resp_var))
}

# Memory-efficient environmental variable processing
process_environmental_vars <- function(species_name, resp_xy, full_stack, base_dir, log_msg) {
  
  env_final_path <- file.path(base_dir, "data/environmental", paste0(species_name, "_env_stack_final.rds"))
  
  if (file.exists(env_final_path)) {
    log_msg("Environmental stack already exists for", species_name, "- loading from file")
    return(readRDS(env_final_path))
  }
  
  log_msg("Processing environmental variables for", species_name)
  
  # Generate pseudo-absence points
  n_pseudo <- min(1000, nrow(resp_xy) * 2)  # Adaptive pseudo-absence count
  
  pseudo_absence <- tryCatch({
    terra::spatSample(full_stack[[1]], size = n_pseudo, method = "random", na.rm = TRUE, xy = TRUE)
  }, error = function(e) {
    log_msg("Error generating pseudo-absence points:", e$message)
    return(NULL)
  })
  
  if (is.null(pseudo_absence)) {
    stop("Failed to generate pseudo-absence points for ", species_name)
  }
  
  names(pseudo_absence) <- c("x", "y")
  
  # Combine presence + pseudo-absence
  all_points <- rbind(
    data.frame(x = resp_xy$x, y = resp_xy$y, type = "presence"),
    data.frame(x = pseudo_absence$x, y = pseudo_absence$y, type = "pseudoabsence")
  )
  
  # Extract environmental values
  log_msg("Extracting environmental values for", species_name)
  env_vals <- tryCatch({
    terra::extract(full_stack, all_points[, c("x", "y")])
  }, error = function(e) {
    log_msg("Error extracting environmental values:", e$message)
    return(NULL)
  })
  
  if (is.null(env_vals)) {
    stop("Failed to extract environmental values for ", species_name)
  }
  
  env_df <- as.data.frame(env_vals)[, -1, drop = FALSE]  # Remove ID column
  
  # Priority variables
  priority_vars <- c("reclamation_raster", "VIIRS_2023_2.5min", "GDP2020_2.5min", 
                     "BUILT_S_E2025_2.5min", "Pop2020_2.5min")
  
  # Variable selection process
  log_msg("Starting variable selection for", species_name)
  
  # Step 1: Remove constant/NA variables
  all_NA <- colSums(!is.na(env_df)) == 0
  zero_SD <- sapply(env_df, function(x) sd(x, na.rm = TRUE) == 0)
  
  constant_vars <- names(env_df)[all_NA | zero_SD]
  if (length(constant_vars) > 0) {
    log_msg("Removed constant/NA variables for", species_name, ":", paste(constant_vars, collapse = ", "))
    env_df <- env_df[, !names(env_df) %in% constant_vars, drop = FALSE]
    full_stack_filtered <- full_stack[[setdiff(names(full_stack), constant_vars)]]
  } else {
    full_stack_filtered <- full_stack
  }
  
  # Step 2: Remove variables with too few observations
  min_obs <- 20
  obs_cnt <- colSums(!is.na(env_df))
  few_obs_vars <- names(obs_cnt)[obs_cnt < min_obs]
  
  if (length(few_obs_vars) > 0) {
    log_msg("Removed variables with <", min_obs, "observations for", species_name, ":", 
            paste(few_obs_vars, collapse = ", "))
    env_df <- env_df[, !names(env_df) %in% few_obs_vars, drop = FALSE]
    full_stack_filtered <- full_stack_filtered[[setdiff(names(full_stack_filtered), few_obs_vars)]]
  }
  
  # Step 3: Correlation analysis
  if (ncol(env_df) < 2) {
    log_msg("Warning: Less than 2 variables remaining for", species_name)
    env_stack_final <- full_stack_filtered
  } else {
    corr_mat <- tryCatch({
      cor(env_df, use = "pairwise.complete.obs")
    }, error = function(e) {
      log_msg("Error in correlation calculation for", species_name, ":", e$message)
      return(NULL)
    })
    
    if (is.null(corr_mat) || anyNA(corr_mat)) {
      log_msg("Warning: Correlation matrix has issues for", species_name, "- skipping correlation filter")
      env_stack_final <- full_stack_filtered
    } else {
      # Remove highly correlated variables
      highCorrIdx <- tryCatch({
        findCorrelation(corr_mat, cutoff = 0.8)
      }, error = function(e) {
        log_msg("Error in findCorrelation for", species_name, ":", e$message)
        return(numeric(0))
      })
      
      if (length(highCorrIdx) > 0) {
        highCorrVars <- colnames(corr_mat)[highCorrIdx]
        highCorrVars_filtered <- setdiff(highCorrVars, priority_vars)
        
        if (length(highCorrVars_filtered) > 0) {
          env_stack_corrOK <- full_stack_filtered[[setdiff(names(full_stack_filtered), highCorrVars_filtered)]]
        } else {
          env_stack_corrOK <- full_stack_filtered
        }
      } else {
        env_stack_corrOK <- full_stack_filtered
      }
      
      # Step 4: VIF analysis
      if (terra::nlyr(env_stack_corrOK) > 2) {
        vif_sel <- tryCatch({
          vifstep(env_stack_corrOK, th = 10)
        }, error = function(e) {
          log_msg("VIF analysis failed for", species_name, ":", e$message)
          return(NULL)
        })
        
        if (!is.null(vif_sel)) {
          excluded_vars <- vif_sel@excluded
          dropped_vif <- setdiff(excluded_vars, priority_vars)
          
          if (length(dropped_vif) > 0) {
            kept_vars <- setdiff(names(env_stack_corrOK), dropped_vif)
            env_stack_final <- env_stack_corrOK[[kept_vars]]
          } else {
            env_stack_final <- env_stack_corrOK
          }
        } else {
          env_stack_final <- env_stack_corrOK
        }
      } else {
        env_stack_final <- env_stack_corrOK
      }
    }
  }
  
  # Save final environmental stack
  saveRDS(env_stack_final, env_final_path)
  log_msg("Environmental processing completed for", species_name, "- Variables:", terra::nlyr(env_stack_final))
  
  return(env_stack_final)
}

# Initialize master results file
initialize_master_results <- function(output_dir) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  master_results_file <- file.path(output_dir, paste0("batch_", batch_id, "_evaluationTotalReport.csv"))
  
  if (!file.exists(master_results_file)) {
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
    log_msg("Initialized master results file:", master_results_file)
  }
  
  return(master_results_file)
}

# ============================================================================
# Main Species Processing Loop
# ============================================================================

# Initialize master results file
master_file <- initialize_master_results(file.path(config$base_dir, 'results/evaluations/comprehensive'))

# Process each species
for (i in seq_along(current_species)) {
  species_name <- current_species[i]
  log_msg("Processing species (", i, "/", length(current_species), "):", species_name)
  
  # Wrap each species in try-catch for error handling
  tryCatch({
    
    # Step 1: Load species data
    clean_name <- gsub("[^A-Za-z0-9_]", "_", species_name)
    species_file <- file.path("<WORK_ROOT>/data/species_occ", clean_name, paste0(clean_name, "_occurrences.csv"))
    
    if (!file.exists(species_file)) {
      stop("Species file not found: ", species_file)
    }
    
    species_data <- read.csv(species_file)
    
    if (nrow(species_data) == 0) {
      stop("No occurrence data for species: ", species_name)
    }
    
    # Validate required columns
    required_cols <- c("decimalLongitude", "decimalLatitude")
    if (!all(required_cols %in% names(species_data))) {
      stop("Missing required columns in species data: ", paste(setdiff(required_cols, names(species_data)), collapse = ", "))
    }
    
    resp_xy <- species_data[, c("decimalLongitude", "decimalLatitude")]
    
    # Remove rows with NA coordinates
    complete_rows <- complete.cases(resp_xy)
    if (sum(complete_rows) == 0) {
      stop("No valid coordinates for species: ", species_name)
    }
    
    if (sum(complete_rows) < nrow(resp_xy)) {
      log_msg("Removed", nrow(resp_xy) - sum(complete_rows), "rows with NA coordinates for", species_name)
      resp_xy <- resp_xy[complete_rows, ]
    }
    
    log_msg("Species data loaded for", species_name, "- Records:", nrow(resp_xy))
    
    # Step 2: Process presence points
    result <- process_presence_points(species_name, resp_xy)
    resp_xy <- result$resp_xy
    resp_var <- result$resp_var
    
    # Step 3: Process environmental variables
    env_stack_final <- process_environmental_vars(species_name, resp_xy, full_stack, base_dir, log_msg)
    
    # Step 4: Format data for BIOMOD2
    log_msg("Formatting data for BIOMOD2 for", species_name)
    formatted_data <- tryCatch({
      BIOMOD_FormatingData(
        resp.var = resp_var,
        expl.var = env_stack_final,
        resp.xy = resp_xy,
        resp.name = species_name,
        PA.nb.rep = 1,
        PA.nb.absences = min(1000, nrow(resp_xy) * 2),
        PA.strategy = "disk",
        PA.dist.min = 20000, # 20 km
        PA.dist.max = 50000,
        na.rm = TRUE
      )
    }, error = function(e) {
      log_msg("Error in BIOMOD_FormatingData for", species_name, ":", e$message)
      return(NULL)
    })
    
    if (is.null(formatted_data)) {
      stop("Failed to format data for BIOMOD2")
    }
    
    # Save formatted data
    formatted_file <- file.path(base_dir, 'data/species_formatted', paste0(species_name, "_formatted.rds"))
    saveRDS(formatted_data, formatted_file)
    log_msg("Data formatted and saved for", species_name)
    
    # Step 5: Set up modeling options
    modeling_options <- tryCatch({
      bm_ModelingOptions(
        data.type = 'binary',
        models = c("GLM", "GAM", "GBM", "RFd", "MAXNET"),
        strategy = 'default'
      )
    }, error = function(e) {
      log_msg("Error creating modeling options for", species_name, ":", e$message)
      return(NULL)
    })
    
    if (is.null(modeling_options)) {
      stop("Failed to create modeling options")
    }
    
    # Step 6: Train individual models
    log_msg("Training individual models for", species_name)
    individual_models <- tryCatch({
      BIOMOD_Modeling(
        bm.format = formatted_data,
        modeling.id = paste0(species_name, '_MultiModel_Ensemble'),
        models = c("GLM", "GAM", "GBM", "RFd", "MAXNET"),
        bm.options = modeling_options,
        CV.strategy = 'kfold',
        CV.nb.rep = 2,
        CV.k = 5,
        CV.perc = 0.8,
        OPT.strategy = 'bigboss',
        var.import = 2,
        metric.eval = c('TSS', 'ROC', 'KAPPA', 'ACCURACY'),
        do.full.models = TRUE,
        seed.val = 42,
        nb.cpu = 8
      )
    }, error = function(e) {
      log_msg("Error in individual modeling for", species_name, ":", e$message)
      return(NULL)
    })
    
    if (is.null(individual_models)) {
      stop("Failed to train individual models")
    }
    
    # Save individual models
    individual_file <- file.path(base_dir, 'results/individual_models', paste0(species_name, '_individual.rds'))
    saveRDS(individual_models, individual_file)
    
    # Get individual model evaluations
    individual_evaluation <- get_evaluations(individual_models)
    log_msg("Individual models trained successfully for", species_name)
    
    # Step 7: Model selection
    log_msg("Starting model selection for", species_name)
    
    tss_roc_df <- individual_evaluation %>%
      filter(metric.eval %in% c("TSS", "ROC")) %>%
      select(full.name, algo, PA, run, metric.eval, validation)
    
    tss_roc_summary <- tss_roc_df %>%
      pivot_wider(names_from = metric.eval, values_from = validation) %>%
      arrange(desc(TSS))
    
    # Select good models
    good_models <- tss_roc_summary %>%
      filter(TSS > 0.8, ROC > 0.8)
    
    if (nrow(good_models) == 0) {
      good_models <- tss_roc_summary %>%
        filter(TSS > 0.7, ROC > 0.7)
      
      if (nrow(good_models) == 0) {
        stop("No models meet minimum performance criteria (TSS > 0.7, ROC > 0.7)")
      } else {
        log_msg("Using fallback criteria (TSS > 0.7, ROC > 0.7) for", species_name)
      }
    }
    
    selected_models <- good_models$full.name
    log_msg("Selected", length(selected_models), "models for ensemble for", species_name)
    
    # Step 8: Create ensemble models
    log_msg("Creating ensemble models for", species_name)
    
    ensemble_models <- tryCatch({
      BIOMOD_EnsembleModeling(
        bm.mod = individual_models,
        models.chosen = selected_models,
        em.by = 'all',
        em.algo = 'EMmean',
        metric.select = c('TSS'),
        metric.select.thresh = c(0.8),
        metric.eval = c('TSS', 'ROC', 'KAPPA'),
        var.import = 3,
        EMci.alpha = 0.05,
        EMwmean.decay = 'proportional'
      )
    }, error = function(e) {
      log_msg("Error in ensemble modeling for", species_name, ":", e$message)
      return(NULL)
    })
    
    if (!is.null(ensemble_models)) {
      # Save ensemble models
      ensemble_file <- file.path(base_dir, 'results/ensemble_models', paste0(species_name, '_ensemble.rds'))
      saveRDS(ensemble_models, ensemble_file)
      
      # Get ensemble evaluations
      eval_df <- get_evaluations(ensemble_models)
      
      # Select best ensemble model
      if (nrow(eval_df) > 0) {
        eval_df$composite_score <- rowMeans(
          eval_df[, c("sensitivity", "specificity", "calibration"), drop = FALSE],
          na.rm = TRUE
        )
        best_ensemble_model <- eval_df$full.name[which.max(eval_df$composite_score)]
        
        # Save best model info
        best_model_file <- file.path(base_dir, 'results/ensemble_models', paste0(species_name, '_best_ensemble.rds'))
        saveRDS(best_ensemble_model, best_model_file)
        
        log_msg("Best ensemble model for", species_name, ":", best_ensemble_model)
      }
      
      log_msg("Ensemble models created successfully for", species_name)
    }
    
    # Step 9: Save evaluation results
    log_msg("Saving evaluation results for", species_name)
    
    # Format evaluation data
    results_list <- list()
    
    # Individual model results
    if (!is.null(individual_evaluation) && nrow(individual_evaluation) > 0) {
      individual_df <- individual_evaluation %>%
        mutate(Species = species_name, Type = "individual")
      results_list[["individual"]] <- individual_df
    }
    
    # Ensemble model results
    if (exists("eval_df") && !is.null(eval_df) && nrow(eval_df) > 0) {
      ensemble_df <- eval_df %>%
        mutate(Species = species_name, Type = "ensemble")
      results_list[["ensemble"]] <- ensemble_df
    }
    
    # Combine and save results
    if (length(results_list) > 0) {
      combined_results <- bind_rows(results_list)
      
      # Ensure all required columns exist
      required_cols <- c("Species", "Type", "full.name", "PA", "run", "algo", 
                        "metric.eval", "cutoff", "sensitivity", "specificity", 
                        "calibration", "validation", "evaluation", "merged.by.PA", 
                        "merged.by.run", "merged.by.algo", "filtered.by")
      
      for (col in required_cols) {
        if (!col %in% names(combined_results)) {
          combined_results[[col]] <- NA
        }
      }
      
      combined_results <- combined_results %>%
        select(all_of(required_cols))
      
      # Append to master file
      write.table(combined_results, master_file, 
                  sep = ",", row.names = FALSE, col.names = FALSE, 
                  append = TRUE, na = "NA")
      
      log_msg("Evaluation results saved for", species_name)
    }
    
    log_msg("Species", species_name, "completed successfully")
    
    # Clean up memory
    rm(list = c("species_data", "resp_xy", "resp_var", "env_stack_final", 
                "formatted_data", "individual_models", "individual_evaluation"))
    if (exists("ensemble_models")) rm(ensemble_models)
    if (exists("eval_df")) rm(eval_df)
    gc()
    
  }, error = function(e) {
    log_msg("Species", species_name, "failed:", e$message, level = "ERROR")
    
    # Clean up memory even on error
    rm(list = ls(pattern = paste0("^", gsub("[^A-Za-z0-9_]", "_", species_name))))
    gc()
  })
}

log_msg("Batch", batch_id, "completed! Processed", length(current_species), "species")

# Final cleanup
rm(full_stack)
gc()

log_msg("Script execution completed for batch", batch_id)