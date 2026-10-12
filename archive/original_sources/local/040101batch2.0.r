# Original source: 040101batch2.0.r
# Original source location: <LOCAL_CODE_ROOT>/040101batch2.0.r
# Original source SHA-256: a21808e0a26f0017fe5384def24b76f4afa812b3bddbbaee1e8d6a6186efa9f8
# Public-safe original snapshot: archive/original_sources/local/040101batch2.0.r
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
# Batch Species Distribution Modeling for 800+ Species
# Optimized for High Performance Computing (simple version)
# Author: Binyue Kang 16/06/2025
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
library(pryr)
library(ENMeval)
# ============================================================================
# Configuration and Setup
# ============================================================================

# Main configuration
config <- list(
  base_dir = "<WORK_ROOT>",
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
    "logs",                        # Log files
    "temp"                         # Temporary files
  )

   for (dir in dirs) {
    dir.create(file.path(base_dir, dir), recursive = TRUE, showWarnings = FALSE)
  }

  return(file.path(base_dir, dirs))
}
full_stack <- terra::rast( "<HPC_INPUT_ROOT>/climate/full_stack.tif")
base_dir <- "<WORK_ROOT>"
# set up the batch id
args <- commandArgs(trailingOnly = TRUE)
batch_id <- as.numeric(args[1])
slurm_array_id <- Sys.getenv("SLURM_ARRAY_TASK_ID", unset = NA)

species_list <- read.csv("<HPC_INPUT_ROOT>/script/output/species_list.csv")
species_list <- species_list$species
total_species <- nrow(species_list)

species_per_batch <- 29
start_idx <- (batch_id - 1) * species_per_batch + 1
end_idx <- min(batch_id * species_per_batch, total_species)

# Setup logging
setup_logging <- function(log_dir) {
  log_file <- file.path(log_dir, paste0(Sys.Date(),"modeling_","Batch_", batch_id, ".log"))
  log_message <- function(..., level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  msg <- paste(...)
  log_entry <- paste0("[", timestamp, "] [", level, "] ", msg)
  cat(log_entry, "\n")
  cat(log_entry, "\n", file = log_file, append = TRUE)
}
  return(log_message)
}

log_status <- function(batch_id, species_name, step, status, message) {
  status_log_path <- file.path(base_dir, "logs", "modeling_status_log.csv")
  slurm_job_id    <- Sys.getenv("SLURM_JOB_ID", unset = NA)
  slurm_array_id  <- Sys.getenv("SLURM_ARRAY_TASK_ID", unset = NA)

  if (!file.exists(status_log_path)) {
    write.table(
      data.frame(JobID = character(), ArrayID = character(),
                 BatchID = character(), Species = character(), Step = character(), 
                 Status = character(), Message = character(), Timestamp = character()),
      file = status_log_path, sep = ",", row.names = FALSE, col.names = TRUE
    )
  }

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


# Initialize
 dirs <- setup_directories(config$base_dir)
 log_msg <- setup_logging(dirs[8])  # logs directory

filter_env_vars_and_points <- function(resp_xy, env_stack, species_name, na_threshold = 0.1, min_points = 100) {
  env_vals <- terra::extract(env_stack, resp_xy)

  # 计算每层变量 NA 比例
  na_prop <- colMeans(is.na(env_vals))
  drop_layers <- names(na_prop[na_prop > na_threshold])
  
  if (length(drop_layers) > 0) {
    message("物种 ", species_name, " - 移除高 NA 比例变量: ", paste(drop_layers, collapse = ", "))
    env_stack <- env_stack[[setdiff(names(env_stack), drop_layers)]]
    env_vals <- terra::extract(env_stack, resp_xy)  # 重新提取
  }

  valid_idx <- complete.cases(env_vals)
  valid_points <- sum(valid_idx)

  if (valid_points < min_points) {
    message("物种 ", species_name, " 有效 presence 点数仅 ", valid_points, "，跳过建模")
    return(NULL)  # 表示数据太少不建模
  }

  list(
    env_stack = env_stack,
    resp_xy = resp_xy[valid_idx, , drop = FALSE],
    env_vals = env_vals[valid_idx, , drop = FALSE]
  )
}
get_best_individual_model <- function(individual_models, metric = "TSS") {
  if (is.null(individual_evaluation) || length(individual_evaluation) == 0) {
    return(NULL)
  }

  model_names <- get_built_models(individual_models)

  # 计算 metric 排名
  scores <- sapply(model_names, function(m) {
    score <- tryCatch({
      individual_evaluation[metric, "Testing.data", m]
    }, error = function(e) NA)
    if (is.na(score)) return(-1) else return(score)
  })

  best_model <- model_names[which.max(scores)]
  return(best_model)
}

# ============================================================================
# Species List Management
# ============================================================================

log_msg("处理批次:", batch_id, "\n")

current_species <- species_list[start_idx:end_idx]

log_msg("批次", batch_id, "处理物种索引:", start_idx, "到", end_idx, "\n")
log_msg("当前批次物种:", paste(current_species, collapse = ", "), "\n")

setwd("<WORK_ROOT>/models")

for(i in seq_along(current_species)) {
species_name <- current_species[i]
  log_msg("正在建模物种 (", i, "/", length(current_species), "):", species_name, "\n")
  log_msg(
  paste0(
    "Memory before modeling ", species_name, ": ",
    round(pryr::mem_used()/1024^2, 1), " Mb"
  )
)
projection_file <- file.path(base_dir,'results/projections', paste0(species_name, '_ensemble_projection.rds'))

tryCatch({
# Step 2: Load Species Data
  tryCatch({
  log_msg(paste0("Batch ", batch_id, "Step 2: Loading species data..."))
  # Clean species name (remove/replace unsafe characters for file paths)
  clean_name <- gsub("[^A-Za-z0-9_]", "_", species_name)
   # Build full path to CSV file
  species_file <- file.path(base_dir,'data/species_occ', clean_name, paste0(clean_name, "_occurrences.csv"))
  output_directory <- "<WORK_ROOT>/models"
   species_data <- tryCatch({
      read.csv(species_file)
    }, error = function(e) {
      warning("Failed to read ", species_file, ": ", e$message)
      return(NULL)
    })
  resp_var <- rep(1, nrow(species_data))
  resp_xy <- species_data[, c("decimalLongitude", "decimalLatitude")]
  log_msg(paste0("Batch ", batch_id, ": Species ", species_name, "Species data loaded."))
}, error = function(e) {
  log_msg(paste0("Batch ", batch_id, ": Species ", species_name,"Error in loading species data:", e$message))
  stop(e)
})
# Step 3: Correlation analysis among predictors at species locations
# Generate pseudo-absence points
log_msg(paste0("Batch ", batch_id, ": Species ", species_name,"Generating pseudo-absence points..."))

env_final_path <- file.path(base_dir,"data/environmental", paste0(species_name, "_env_stack_final.rds"))
log_msg(paste0("Batch ", batch_id, ": Species ", species_name, " - env stacks already exist. Skipping selecting."))
env_stack_final <- readRDS(env_final_path)

filter_result <- filter_env_vars_and_points(resp_xy, env_stack_final, species_name)

if (is.null(filter_result)) {
  log_msg(paste("Batch", batch_id, "Species", species_name, "有效点数太少，跳过"))
  log_status(batch_id, species_name, "DataCheck", "TooFewValidPoints", "有效点数小于设定阈值")
  next
}
# 更新 env_stack_final 和 resp_xy（带 NA 清洗）
env_stack_final <- filter_result$env_stack

formatted_data_path <- file.path(base_dir, "data/species_formatted", paste0(species_name, "_formatted.rds"))
formatted_data <- readRDS(formatted_data_path)

log_msg(paste0("Batch ", batch_id, ": Species ", species_name,"Step 5: Setting up modeling options with default parameters"))
   log_msg(
  paste0(
    "Memory before remove ", species_name, ": ",
    round(pryr::mem_used()/1024^2, 1), " Mb"
  ))
rm(list = c('resp_xy','resp_var','filter_result'))
terra::tmpFiles(remove = TRUE)
gc()

# Set default options for all models
modeling_options <- tryCatch({
  bm_ModelingOptions(
    data.type = 'binary',
    models = c("GLM", "RFd", "MAXNET"),
    strategy = 'default'  # Use default parameters
  )
}, error = function(e) {
  log_msg(paste("Error in modeling options:", e$message))
  return(NULL)
})

# ============================================================================
# 5.1 Train individual models
# ============================================================================
individual_model_path <- file.path(base_dir, "results/individual_models", paste0(species_name, "_individual.rds"))

individual_models <- tryCatch({
  if (file.exists(individual_model_path)) {
    log_msg(paste0("Batch ", batch_id, ": Species ", species_name, " - Individual models already exist. Loading from disk..."))
    readRDS(individual_model_path)
  } else {
    log_msg(paste0("Batch ", batch_id, ": Species ", species_name, " - Step 5.1: Training individual models"))
    log_msg(
  paste0(
    "Memory before modeling ", species_name, ": ",
    round(pryr::mem_used()/1024^2, 1), " Mb"
  )
)

    individual_models <- BIOMOD_Modeling(
      bm.format = formatted_data,
      modeling.id = paste0(species_name, 'MultiModel_Ensemble'),
      models = c("GLM", "RFd", "MAXNET"),
      bm.options = modeling_options,
      CV.strategy = 'block',
      CV.nb.rep = 1,
      CV.k = 3,
      CV.perc = 0.8,
      OPT.strategy = 'bigboss',
      var.import = 2,
      metric.eval = c('TSS', 'ROC'),
      do.full.models = TRUE,
      seed.val = 42,
      nb.cpu = 2
    )

    saveRDS(individual_models, file = individual_model_path)
    log_msg(paste0("Batch ", batch_id, ": Species ", species_name, " - Individual models trained and saved."))
  }
}, error = function(e) {
  log_msg(paste0("Batch ", batch_id, ": Species ", species_name, " - Error in individual modeling: ", e$message))
  return(NULL)
})

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
    round(pryr::mem_used()/1024^2, 1), " Mb"
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
  
  if (nrow(good_models) == 0) {

    log_msg(paste0("Batch ", batch_id, ": Species ", species_name, 
                   " FALLBACK -  No model meets TSS > 0.7 and ROC > 0.7 criteria."))
    log_status(batch_id, species_name, "evaluation", "Fallback", "No model meets TSS > 0.7 and ROC > 0.7")
    selected_models <- get_best_individual_model(individual_models)
  } else {
     selected_models <- good_models$full.name
     log_msg(paste0("Batch ", batch_id, ": Species ", species_name, 
                   " - Using",length(selected_models)," models with TSS > 0.7 and ROC > 0.7 (fallback)."))
  }
} else {
  log_msg(paste0("Batch ", batch_id, ": Species ", species_name, 
                 " - Using models with TSS > 0.8 and ROC > 0.8."))
    selected_models <- good_models$full.name
}

log_msg(paste("Final selected models:", length(selected_models)))

# ============================================================================
# Step 5.3: Create ensemble models
# ============================================================================

log_msg(paste0("Batch ", batch_id, ": Species ", species_name, "Step 5.3 :Starting ensemble modeling with selected models..."))

best_ensemble_model <- NULL
ensemble_models <- NULL
if (is.null(selected_models) || length(selected_models) < 2)  {
  # 启动 fallback 模式：选择 best individual model
  best_ensemble_model <- selected_models

  log_msg(paste0("Batch ", batch_id, ": Species ", species_name, 
                 " fallback using best individual model: ", best_ensemble_model))

} else {
    ensemble_model_path <-  file.path(base_dir, 'results/ensemble_models', paste0(species_name,'_ensemble.rds'))
    if(file.exists(ensemble_model_path)){
    ensemble_models <- readRDS(ensemble_model_path)
        eval_df <- tryCatch({
        get_evaluations(ensemble_models)
      }, error = function(e) {
        log_msg(paste0("Batch ", batch_id, ": Species ", species_name, " - Error getting evaluations: ", e$message))
        data.frame()  # 返回空数据框
      })
        if (is.null(eval_df) || nrow(eval_df) == 0) {
             log_msg(paste("Species", species_name, ": Ensemble evaluation missing or invalid. Rebuilding ensemble with threshold 0.7..."))
             ## Set ensemble modeling options
        ensemble_options <- tryCatch({
          bm_ModelingOptions(
            data.type = 'binary',
            models = 'EMmean',  # Mean ensemble
            strategy = 'default'
          )
        }, error = function(e) {
          log_msg(paste0("Batch ", batch_id, ": Species ", species_name, paste("Error in ensemble options:", e$message)))
          return(NULL)
        })
       # 重新训练 ensemble 模型
        ensemble_models <- tryCatch({
          BIOMOD_EnsembleModeling(
            bm.mod = individual_models,
            models.chosen = selected_models,
            em.by = "all",
            em.algo = c("EMmean", "EMmedian"),
            metric.select = c("TSS"),
            metric.select.thresh = c(0.7),  #  修改阈值为 0.7
            metric.eval = c("TSS", "ROC", "KAPPA"),
            var.import = 3,
            EMci.alpha = 0.05,
            EMwmean.decay = "proportional"
          )
        }, error = function(e) {
          log_msg(paste("Species", species_name, ": Ensemble retraining failed:", e$message))
          return(NULL)
        })
        if (!is.null(ensemble_models)) {
        log_msg(paste0("Batch ", batch_id, ": Species ", species_name,"Ensemble models recreated successfully"))
        log_msg(paste0("Batch ", batch_id, ": Species ", species_name, paste("REBuilt ensemble models:", length(get_built_models(ensemble_models)))))
        saveRDS(ensemble_models, file = file.path(base_dir, 'results/ensemble_models', paste0(species_name,'_ensemble.rds')))
         }
        }else{
        log_msg(paste("Species", species_name, ": Ensemble evaluation is valid. No need to retrain."))
        }
    }else{
        log_msg(paste("Species", species_name, ": No ensemble model found. Proceeding with modeling..."))
        ## Set ensemble modeling options
          ensemble_options <- tryCatch({
            bm_ModelingOptions(
              data.type = 'binary',
              models = 'EMmean',  # Mean ensemble
              strategy = 'default'
            )
          }, error = function(e) {
            log_msg(paste0("Batch ", batch_id, ": Species ", species_name, paste("Error in ensemble options:", e$message)))
            return(NULL)
          })
        ## Create ensemble models
        ensemble_models <- tryCatch({
        BIOMOD_EnsembleModeling(
        bm.mod = individual_models,
        models.chosen = selected_models,  # Use tss and roc both over 0.8 models
        em.by = 'all',          # Ensemble method
        em.algo = c('EMmean','EMmedian'),  # Multiple ensemble algorithms
        metric.select = c('TSS'),  # Metric for model selection
        metric.select.thresh = c(0.7),  # TSS threshold
        metric.eval = c('TSS', 'ROC', 'KAPPA'),
        var.import = 3,
        EMci.alpha = 0.05,
        EMwmean.decay = 'proportional'
  )
}, error = function(e) {
  log_msg(paste0("Batch ", batch_id, ": Species ", species_name,paste("Error in ensemble modeling:", e$message)))
  return(NULL)
})
    if (!is.null(ensemble_models)) {
        log_msg(paste0("Batch ", batch_id, ": Species ", species_name,"Ensemble models created successfully"))
        log_msg(paste0("Batch ", batch_id, ": Species ", species_name, paste("Built ensemble models:", length(get_built_models(ensemble_models)))))
        saveRDS(ensemble_models, file = file.path(base_dir, 'results/ensemble_models', paste0(species_name,'_ensemble.rds')))
         }
    
    }
}
# ---------------------------------------------------------------------------
# Step 5.5: Select the best ensemble model based on composite score
# ---------------------------------------------------------------------------
# Extract evaluation results (as a data.frame)
eval_df <- tryCatch({
  get_evaluations(ensemble_models)
}, error = function(e) {
  log_msg(paste0("Batch ", batch_id, ": Species ", species_name, " - Error getting evaluations: ", e$message))
  data.frame()  # 返回空数据框
})

if (nrow(eval_df) == 0) {
    #FALLBACK
    ind_eval <- tryCatch({
    get_evaluations(individual_models)
  }, error = function(e) {
    log_msg(paste("无法从 individual_models 获取评估:", e$message))
    return(NULL)
  })
    best_ensemble_model <- get_best_individual_model(
      individual_models = individual_models,
      metric = "TSS"
    )
    log_msg(paste("没有 ensemble 评估结果，使用 best individual model:", best_ensemble_model))
    log_msg(paste0("Batch ", batch_id, ": Species ", species_name, " Best ensemble model selected: ", best_ensemble_model))
}else{
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
  }else{
    stop(" Required metrics (sensitivity, specificity, calibration) not found in evaluation output.")
  }
}
saveRDS(best_ensemble_model, file = file.path(base_dir, 'results/ensemble_models', paste0(species_name,'_best_ensemble.rds')))
log_msg(paste0("Batch ", batch_id, ": Species ", species_name, paste("Best ensemble model selected:", best_ensemble_model)))
# ============================================================================
# 6: Generate comprehensive evaluation report
# ============================================================================
log_msg(paste0("Batch ", batch_id, ": Species ", species_name,"6: Generating comprehensive evaluation report"))

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
    required_cols <- c("Species", "Type", "full.name", "PA", "run", "algo", 
                      "metric.eval", "cutoff", "sensitivity", "specificity", 
                      "calibration", "validation", "evaluation", "merged.by.PA", 
                      "merged.by.run", "merged.by.algo", "filtered.by")
    
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
    required_cols <- c("Species", "Type", "full.name", "PA", "run", "algo", 
                      "metric.eval", "cutoff", "sensitivity", "specificity", 
                      "calibration", "validation", "evaluation", "merged.by.PA", 
                      "merged.by.run", "merged.by.algo", "filtered.by")
    
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
  
  tryCatch({
    # Check if file exists
    if (file.exists(master_file_path)) {
      # Append without column names
      write.table(species_evaluation_df, master_file_path, 
                  sep = ",", row.names = FALSE, col.names = FALSE, 
                  append = TRUE, na = "NA")
    } else {
      # Create new file with column names
      write.csv(species_evaluation_df, master_file_path, row.names = FALSE, na = "NA")
    }
    
    log_msg(paste0("Batch ", batch_id, ": Species ", species_name, paste("Results appended directly. Species rows:", nrow(species_evaluation_df))))
    
  }, error = function(e) {
    log_msg(paste0("Batch ", batch_id, ": Species ", species_name, paste("Error in direct append:", e$message)))
    
    # Fallback
    backup_file <- gsub("\\.csv$", paste0("_", species_name, "_backup.csv"), "<WORK_ROOT>/temp")
    write.csv(species_evaluation_df, backup_file, row.names = FALSE)
    log_msg(paste("Saved species backup file:", backup_file))
  })
}

# Initialize master results file (run once at the start of your entire workflow)
master_file <- initialize_master_results("<WORK_ROOT>/results/evaluations/comprehensive")
# Format evaluation data to match your desired table structure
formatted_evaluation <- format_biomod_evaluation(
  individual_evaluation = individual_evaluation,
  eval_df = eval_df,  # your ensemble evaluation data
  species_name = species_name,
  individual_models = individual_models
)

append_species_direct(formatted_evaluation, master_file )

# Print concise summary
log_msg(paste0("Batch ", batch_id, ": Species ", species_name, "Workflow completed successfully!"))
log_msg(paste0("Batch ", batch_id, ": Species ", species_name, paste("Results saved in:", 'results/evaluations/comprehensive')))


initialize_best_model_results <- function(output_dir, batch_id) {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  best_file <- file.path(output_dir, paste0(batch_id, "_bestModelEvalReport.csv"))
  if (!file.exists(best_file)) {
    header_df <- data.frame(
      Species = character(),
      Type = character(),
      full.name = character(),
      metric.eval = character(),
      cutoff = numeric(),
      sensitivity = numeric(),
      specificity = numeric(),
      calibration = numeric(),
      validation = numeric(),
      evaluation = numeric(),
      stringsAsFactors = FALSE
    )
    write.csv(header_df, best_file, row.names = FALSE)
  }
  return(best_file)
}

format_best_evaluation <- function(eval_df, best_model_name, species_name, type) {
  if (is.null(eval_df) || !best_model_name %in% eval_df$full.name) return(NULL)
  best_rows <- subset(eval_df, full.name == best_model_name)
  best_rows$Species <- species_name
  best_rows$Type    <- type
  col_order <- c("Species", "Type", "full.name", "metric.eval",
                 "cutoff", "sensitivity", "specificity", "calibration",
                 "validation", "evaluation")
  for (col in col_order) {
    if (!col %in% colnames(best_rows)) best_rows[[col]] <- NA
  }
  best_rows <- best_rows[, col_order]
  return(best_rows)
}

# 得到模型名
ensemble_names <- tryCatch(get_built_models(ensemble_models), error = function(e) character(0))
individual_names <- tryCatch(get_built_models(individual_models), error = function(e) character(0))
# 得到评估数据框
individual_eval_df <- tryCatch(get_evaluations(individual_models), error=function(e) NULL)
ensemble_eval_df   <- tryCatch(get_evaluations(ensemble_models),   error=function(e) NULL)

# 判断模型来源，并格式化best评估
if (best_ensemble_model %in% ensemble_names) {
  best_eval <- format_best_evaluation(ensemble_eval_df, best_ensemble_model, species_name, type = "ensemble")
  best_model_type <- "ensemble"
} else if (best_ensemble_model %in% individual_names) {
  best_eval <- format_best_evaluation(individual_eval_df, best_ensemble_model, species_name, type = "individual")
  best_model_type <- "individual"
} else {
  best_eval <- NULL
  best_model_type <- NA
}

# 写入文件
best_file <- initialize_best_model_results("<WORK_ROOT>/results/evaluations/bestEvaluation", batch_id)
if (!is.null(best_eval) && nrow(best_eval) > 0) {
  write.table(best_eval, file = best_file, sep = ",", append = TRUE,
              row.names = FALSE, col.names = FALSE, na = "NA")
  log_msg(paste0("Best model evaluation appended for ", species_name, " (", best_model_type, ") in ", best_file))
} else {
  log_msg(paste0("No best model evaluation found for ", species_name, "."))
}

# 日志
if (!is.null(best_eval)) {
  log_msg(paste0("Batch ", batch_id, ": Species ", species_name, " best model summary (", best_model_type, ") completed successfully!"))
  log_msg(paste0("Batch ", batch_id, ": Species ", species_name, " Results saved in: results/evaluations/bestEvaluation"))
} else {
  log_msg(paste0("Batch ", batch_id, ": Species ", species_name, " best model summary failed or not found."))
}
# ============================================================================
#7: Variable importance for best ensemble model
# ============================================================================
extract_variable_importance_any <- function(individual_models,
                                            ensemble_models,
                                            best_ensemble_model,
                                            species_name,
                                            batch_id,
                                            base_dir) {
  log_msg(paste0("Batch ", batch_id, ": Species ", species_name, " - Extracting variable importance..."))

  get_model_importance <- function(bm_object, source_label) {
    var_imp <- tryCatch({
      get_variables_importance(bm_object)
    }, error = function(e) {
      log_msg(paste0("Batch ", batch_id, ": ", species_name, 
                     " - Failed to get var importance from ", source_label, ": ", e$message))
      return(NULL)
    })

    if (is.null(var_imp) || length(var_imp) == 0) {
      log_msg(paste0("Batch ", batch_id, ": ", species_name, 
                     " - Variable importance is NULL or empty from ", source_label))
      return(NULL)
    }

    # Fallback 情况 - matrix
    if (is.matrix(var_imp)) {
      if (!is.character(best_ensemble_model) || !(best_ensemble_model %in% colnames(var_imp))) {
        log_msg(paste0("Batch ", batch_id, ": ", species_name, 
                       " - best model not found in importance matrix"))
        return(NULL)
      }

      imp_vector <- var_imp[, best_ensemble_model]
      imp_df <- data.frame(
        expl.var = rownames(var_imp),
        var.imp = as.numeric(imp_vector),
        stringsAsFactors = FALSE
      )
    }

    # Ensemble 情况 - data.frame
    else if (is.data.frame(var_imp) && "full.name" %in% names(var_imp)) {
      if (!is.character(best_ensemble_model) || !(best_ensemble_model %in% var_imp$full.name)) {
        log_msg(paste0("Batch ", batch_id, ": ", species_name, 
                       " - best model not found in var_imp$full.name"))
        return(NULL)
      }

      imp_df <- var_imp %>%
        dplyr::filter(full.name == best_ensemble_model) %>%
        dplyr::select(expl.var, var.imp)
    }

    # 未识别结构
    else {
      log_msg(paste0("Batch ", batch_id, ": ", species_name, 
                     " - Unrecognized var_imp structure (", class(var_imp)[1], ")"))
      return(NULL)
    }

    # 汇总变量重要性
    imp_summary <- imp_df %>%
      dplyr::group_by(expl.var) %>%
      dplyr::summarise(MeanImportance = mean(var.imp, na.rm = TRUE), .groups = "drop")

    importance_df <- data.frame(
      Species = species_name,
      Variable = imp_summary$expl.var,
      Importance = as.numeric(imp_summary$MeanImportance),
      Source = source_label,
      stringsAsFactors = FALSE
    )

    return(importance_df)
  }

  ensemble_model_names <- tryCatch(get_built_models(ensemble_models), error = function(e) character(0))
  individual_model_names <- tryCatch(get_built_models(individual_models), error = function(e) character(0))

  importance_df <- NULL

  # 决定使用哪个对象提取
  if (!is.null(best_ensemble_model) && best_ensemble_model %in% ensemble_model_names) {
    importance_df <- get_model_importance(ensemble_models, "ensemble")
  } else if (!is.null(best_ensemble_model) && best_ensemble_model %in% individual_model_names) {
    importance_df <- get_model_importance(individual_models, "individual")
  } else {
    log_msg(paste0("Batch ", batch_id, ": ", species_name, 
                   " - best_ensemble_model not found in model sets"))
    return(invisible(NULL))
  }

  if (is.null(importance_df)) {
    log_msg(paste0("Batch ", batch_id, ": ", species_name, 
                   " - No variable importance extracted"))
    return(invisible(NULL))
  }

  importance_df <- importance_df[order(importance_df$Importance, decreasing = TRUE), ]

  output_file <- file.path(base_dir, "results/evaluations/importance", paste0(batch_id, "_importanceTotalReport.csv"))

  if (!file.exists(output_file)) {
    write.csv(importance_df, output_file, row.names = FALSE)
    log_msg(paste("Created new variable importance file:", output_file))
  } else {
    write.table(importance_df, output_file, sep = ",", append = TRUE,
                row.names = FALSE, col.names = FALSE)
    log_msg(paste("Appended variable importance to file:", output_file))
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
log_msg(paste0("Batch ", batch_id, ": Species ", species_name,"Step 5.6: Run projection for the best ensemble model"))

  rm(list = c( "resp_xy", "resp_var", "formatted_data", "individual_evaluation",
                "ensemble_projections",
                "tss_roc_df", "tss_roc_summary", "good_models", "selected_models"))

gc()
unlink(list.files(tempdir(), full.names = TRUE), recursive = TRUE)

tmp_proj_dirs <- list.dirs(file.path(base_dir, "models", species_name), full.names = TRUE, recursive = FALSE)
tmp_proj_dirs <- tmp_proj_dirs[grepl("^proj_Tmp", basename(tmp_proj_dirs))]

for (tmp in tmp_proj_dirs) {
  unlink(tmp, recursive = TRUE, force = TRUE)
  log_msg(paste("Deleted temp folder:", tmp))
}

if (file.exists(projection_file)) {
  log_msg(paste0("Batch ", batch_id, ": Species ", species_name, " - Projection already exists. Skipping modeling..."))
  log_status(batch_id, species_name, "projection_check", "Success", "Projection exists, modeling skipped.")
  next  # 跳过本轮循环，处理下一个物种
}
# fallback_proj <- file.path(base_dir,'results/projections', paste0(species_name, '_fallback_projection.rds'))

# if (file.exists(fallback_proj)) {
#   log_msg(paste0("Batch ", batch_id, ": Species ", species_name, " - FALLBACK Projection already exists. Skipping modeling..."))
#   log_status(batch_id, species_name, "projection_check", "FALL BACK Success", "Projection exists, modeling skipped.")
#   next  # 跳过本轮循环，处理下一个物种
# }
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
        keep.in.memory = FALSE)
        saveRDS(fallback_proj, file = file.path(base_dir,'results/projections', paste0(species_name, '_fallback_projection.rds')))
        log_msg(paste0("Batch ", batch_id, ": Species ", species_name," Projection for best model completed successfully."))
        log_status(batch_id, species_name, "FALLBACK", "successd!", "fallback projection successfully")
         #  --- Binary Prediction after fallback ---
        cutoffs <- get_evaluations(individual_models)["TSS", , best_ensemble_model]

        # 构建路径到 fallback 投影文件夹
        proj_dir <- file.path(base_dir, "models", species_name, "proj_fallback_projection", "individual_projections")

        # 查找预测 tif 文件（带模型名的）
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
             }else{
                ensemble_projections <- tryCatch({
                    BIOMOD_EnsembleForecasting(
                    bm.em = ensemble_models,
                    proj.name = "ensemble_projection",
                    new.env = env_stack_final,
                    models.chosen = best_ensemble_model,
                    compress = TRUE,
                    metric.binary = "TSS",
                    metric.filter = "TSS",
                    build.clamping.mask = FALSE,
                    do.stack = FALSE,
                    keep.in.memory = FALSE,
                    output.format = '.grd'
                    )
                }, error = function(e) {
                    log_msg(paste0("Batch ", batch_id, ": Species ", species_name, paste("Error in ensemble projection:", e$message)))
                    return(NULL)
                })
                 if (!is.null(ensemble_projections)) {
                    saveRDS(ensemble_projections, file = file.path(base_dir,'results/projections', paste0(species_name, '_ensemble_projection.rds')))
                    log_msg(paste0("Batch ", batch_id, ": Species ", species_name," Projection for best ensemble model completed successfully."))
                    log_status(batch_id, species_name, "SDM", "successd!", "Projection for best ensemble model completed successfully")
                  } else {
                    log_msg(paste0("Batch ", batch_id, ": Species ", species_name," Projection failed. No RDS file saved."))
                    log_status(batch_id, species_name, "SDM", "Failed", e$message)
                  }
             }
    }else{
        log_msg(paste0("Batch ", batch_id, ": Species ", species_name," Projection skipped: Missing or invalid projection data or ensemble model."))
    }

 log_msg("species", species_name, "建模完成\n")

}, error = function(e) {
   log_msg("species", species_name, "建模失败:", e$message, "\n")
  })
  log_msg(
  paste0(
    "Memory after modeling ", species_name, ": ",
    round(pryr::mem_used()/1024^2, 1), " Mb"
  )
)

     # Memory cleanup between chunks
  rm(list = c( "ensemble_projections", "best_ensemble_model",'env_stack_final', 'ensemble_models','individual_models'))
  terra::tmpFiles(remove = TRUE)
    gc()

}
log_msg("batch", batch_id, "全部完成！处理了", length(current_species), "个物种\n")