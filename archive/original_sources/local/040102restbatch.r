# Original source: 040102restbatch.r
# Original source location: <LOCAL_CODE_ROOT>/040102restbatch.r
# Original source SHA-256: 26e9f605727fdee242505e305e250a90325376f52414c2aeeddd1e9f11c88b6b
# Public-safe original snapshot: archive/original_sources/local/040102restbatch.r
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
# Batch Species Distribution Modeling for rest Species
# Optimized for High Performance Computing (simple version)
# Author: Binyue Kang 18/06/2025
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


species_list <- read.csv("<HPC_INPUT_ROOT>/script/output/species_list.csv")$species
proj_df <- read.csv("<WORK_ROOT>/results/evaluations/projection_summary.csv")

rest_one <- setdiff(species_list, unique(proj_df$species))
species_list <- rest_one
cat(length(species_list), "species left!\n")


# set up the batch id
args <- commandArgs(trailingOnly = TRUE)
batch_id <- as.numeric(args[1])
slurm_array_id <- Sys.getenv("SLURM_ARRAY_TASK_ID", unset = NA)

total_species <- nrow(species_list)

species_per_batch <- 8
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

filter_env_vars_and_points <- function(resp_xy, env_stack, species_name, na_threshold = 0.1, min_points = 50) {
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
get_best_individual_model <- function(individual_models, individual_evaluation = NULL, metric = "TSS") {
  # FIX 3: Get evaluation if not provided
  if (is.null(individual_evaluation)) {
    individual_evaluation <- tryCatch({
      get_evaluations(individual_models)
    }, error = function(e) {
      message("Error getting evaluations: ", e$message)
      return(NULL)
    })
  }
  
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
log_msg("rest left:", length(species_list), "\n")

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
species_dir_name <- gsub(" ", ".", species_name)
model_dir <- file.path(base_dir, "models", species_dir_name)
if (dir.exists(model_dir)) {
  rm(list = ls(pattern = species_dir_name))
  gc()
  terra::tmpFiles(remove = TRUE)
  system(paste("rm -rf", shQuote(model_dir)))
  if (!dir.exists(model_dir)) {
    log_msg(paste0("Successfully deleted: ", species_name))
  } else {
    log_msg(paste0("Failed to delete: ", species_name))
  }
}
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

n_pseudo <- 1000
pseudo_absence <- spatSample(full_stack[[1]], size = n_pseudo, method = "random", na.rm = TRUE, xy = TRUE)
colnames(pseudo_absence) <- c("x", "y")


# # Combine presence + pseudo-absence
all_points <- rbind(
  data.frame(x = resp_xy$decimalLongitude, y = resp_xy$decimalLatitude, type = "presence"),
  data.frame(x = pseudo_absence$x, y = pseudo_absence$y, type = "pseudoabsence")
)

# # Extract environmental values
log_msg(paste0("Batch ", batch_id, ": Species ", species_name,"Extracting environmental values at presence + pseudo-absence points..."))
env_vals <- terra::extract(full_stack, all_points[, c("x", "y")])
env_df   <- as.data.frame(env_vals)[, -1]    # drop ID column
orig_vars   <- names(full_stack)

# Pre‐processing to remove any zero‐variance (constant) predictors before correlation
## ---------------------------------------------------------------------------
##  STEP 1 – remove columns that are entirely NA or perfectly constant
## ---------------------------------------------------------------------------
all_NA  <- colSums(!is.na(env_df)) == 0
zero_SD <- sapply(env_df, function(x) sd(x, na.rm = TRUE) == 0)

constant_vars <- names(env_df)[all_NA | zero_SD]
if (length(constant_vars)) {
log_msg(paste0("Batch ", batch_id, ": Species ", species_name,"Removed all-NA / zero-variance layers: ",
        paste(constant_vars, collapse = ", ")))
env_df    <- env_df[ , !(names(env_df) %in% constant_vars), drop = FALSE]
  full_stack <- full_stack[[ setdiff(names(full_stack), constant_vars) ]]
}
# ##------------------------------------------------------------
# ##  STEP 2 – remove columns with *too few* non-NA samples
# ##           (e.g. rasters covering a tiny coastal strip)
# ##------------------------------------------------------------
min_obs <- 20                          # 可以调成 30、50……
obs_cnt <- colSums(!is.na(env_df))

few_obs_vars <- names(obs_cnt)[obs_cnt < min_obs]
if (length(few_obs_vars)) {
  log_msg(paste0("Batch ", batch_id, ": Species ", species_name, "Removed vars with <", min_obs,
                     " non-NA samples: ", paste(few_obs_vars, collapse = ", ")))
  env_df <- env_df[ , !(names(env_df) %in% few_obs_vars), drop = FALSE]
  full_stack <- full_stack[[ setdiff(names(full_stack), few_obs_vars) ]]
}

# ## ---------------------------------------------------------------------------
# ##  STEP 3 – build the correlation matrix and detect any remaining NA columns
# ## ---------------------------------------------------------------------------
corr_mat <- stats::cor(env_df, use = "pairwise.complete.obs")
bad_cols <- character(0)   # Initialize bad_cols as empty vector before the conditional check
if (anyNA(corr_mat)) {
  # identify every column/row that still generates an NA correlation
  bad_cols <- unique(c(rownames(corr_mat)[rowSums(is.na(corr_mat)) > 0],
                       colnames(corr_mat)[colSums(is.na(corr_mat)) > 0]))

#   message("Removed layers that still produced NA correlations: ",
#           paste(bad_cols, collapse = ", "))

  env_df    <- env_df[ , !(names(env_df) %in% bad_cols), drop = FALSE]
  full_stack <- full_stack[[ setdiff(names(full_stack), bad_cols) ]]

## rebuild the matrix – now guaranteed to be NA-free
  corr_mat <- stats::cor(env_df, use = "pairwise.complete.obs")
}

stopifnot(!anyNA(corr_mat))   # safety fuse: will stop if NA sneaks back in


## Save the full correlation matrix to CSV
corr_path <- file.path(base_dir,'results/variable/correlation', paste0(species_name, "PriorityVariablecorrelation_matrix.csv"))
write.csv(corr_mat, file = corr_path, row.names = TRUE)
log_msg(paste0("Batch ", batch_id, ": Species ", species_name, paste("Saved correlation matrix to", corr_path)))

## find and drop highly correlated variables
highCorrIdx <- findCorrelation(corr_mat, cutoff = 0.8)
highCorrVars <- colnames(corr_mat)[highCorrIdx]

# log_msg(paste("Variables with |r| >= 0.8:", paste(highCorrVars, collapse = ", ")))

highCorrVars_filtered <- highCorrVars
env_stack_corrOK <- full_stack[[setdiff(names(full_stack), highCorrVars_filtered)]]

# log_msg(paste("Remaining variables after correlation filter:", paste(env_stack_corrOK, collapse = ", ")))
# # ──────────────────────────────────────────────────────────────────────────────
# # Step 4: VIF screening to further remove multicollinearity
# # ──────────────────────────────────────────────────────────────────────────────
log_msg(paste0("Batch ", batch_id, ": Species ", species_name,"Running VIF step (threshold = 10)…"))
vif_sel <- vifstep(env_stack_corrOK, th = 10)

# str(vif_sel)
## results table
vif_results <- vif_sel@results

## identify dropped and kept
excluded_vars <- vif_sel@excluded
kept_vif <- vif_sel@results$Variables
# log_msg(paste("Dropped by VIF:", paste(dropped_vif, collapse = ", ")))

# log_msg(paste("Kept after VIF:", paste(kept_vif, collapse = ", ")))
# # build final stack
env_stack_final <- env_stack_corrOK[[ kept_vif ]] 

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
      
    } else if (v %in% excluded_vars) {
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
summary_path <- file.path(base_dir,'results/variable/select', paste0(species_name, "PriorityVariable_selection_summary.csv"))
write.csv(var_status, file = summary_path, row.names = FALSE)
log_msg(paste0("Batch ", batch_id, ": Species ", species_name, paste("Saved variable selection summary to", summary_path)))

saveRDS(env_stack_final, file.path(base_dir,'data/environmental', paste0(species_name, "_env_stack_final.rds")))

# filter_result <- filter_env_vars_and_points(resp_xy, env_stack_final, species_name)

# if (is.null(filter_result)) {
#   log_msg(paste("Batch", batch_id, "Species", species_name, "有效点数太少，跳过"))
#   log_status(batch_id, species_name, "DataCheck", "TooFewValidPoints", "有效点数小于设定阈值")
#   next
# }
# # 更新 env_stack_final 和 resp_xy（带 NA 清洗）
# env_stack_final <- filter_result$env_stack

formatted_data_path <- file.path(base_dir, "data/species_formatted", paste0(species_name, "_formatted.rds"))
 log_msg(paste0("Batch ", batch_id, ": Species ", species_name, " - Formatting data for BIOMOD2..."))
    if (file.exists(formatted_data_path)) {
  unlink(formatted_data_path)
  log_msg(paste0("Deleted existing formatted data for: ", species_name))
}
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
    selected_models <- get_best_individual_model(
          individual_models = individual_models,
          individual_evaluation = individual_evaluation,
          metric = "TSS"
        )
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
          individual_evaluation = individual_evaluation,
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

auto_fill_env_stack_verbose <- function(env_stack_final, required_vars) {
  # 确保是 terra::SpatRaster
  if (!inherits(env_stack_final, "SpatRaster")) {
    stop("env_stack_final 不是 SpatRaster 类型")
  }

  current_vars <- names(env_stack_final)
  missing_vars <- setdiff(required_vars, current_vars)

  log_msg(paste0("当前变量数量:", length(current_vars)))
  log_msg(paste0("所需变量数量:", length(required_vars)))
  log_msg(paste0("缺失变量数量:", length(missing_vars)))

  if (length(missing_vars) > 0) {
    log_msg("缺失的变量:")
    for (v in missing_vars) log_msg(paste0("  - ", v))

    log_msg(paste("Auto-filling missing variables:", paste(missing_vars, collapse = ", ")))

    # 使用第一个图层作为模板
    template <- env_stack_final[[1]]
    for (v in missing_vars) {
      log_msg(paste0("创建NA栅格: ", v))
      na_rast <- terra::rast(template)
      terra::values(na_rast) <- NA
      names(na_rast) <- v
      env_stack_final <- c(env_stack_final, na_rast)
    }
  }

  # 保证顺序完全按照 required_vars
  all_vars <- names(env_stack_final)
  if (!all(required_vars %in% all_vars)) {
    stop("填充后仍有变量缺失！")
  }

  log_msg("重新排序变量...")
  reordered_stack <- env_stack_final[[required_vars]]

  log_msg(paste0("最终变量数量:", length(names(reordered_stack))))
  log_msg(paste0("变量名匹配:", identical(names(reordered_stack), required_vars)))

  return(reordered_stack)
}



# fallback_proj <- file.path(base_dir,'results/projections', paste0(species_name, '_fallback_projection.rds'))

# if (file.exists(fallback_proj)) {
#   log_msg(paste0("Batch ", batch_id, ": Species ", species_name, " - FALLBACK Projection already exists. Skipping modeling..."))
#   log_status(batch_id, species_name, "projection_check", "FALL BACK Success", "Projection exists, modeling skipped.")
#   next  # 跳过本轮循环，处理下一个物种
# }
# Run projection if valid data and model exist

required_vars <- if (nrow(eval_df) == 0) {
  individual_models@expl.var.names
} else {
  ensemble_models@expl.var.names
}

env_stack_filled <- auto_fill_env_stack_verbose(env_stack_final, required_vars)
rm(env_stack_final)
gc()
if (!is.null(env_stack_filled) && inherits(env_stack_filled, c("RasterStack", "SpatRaster")) &&
    !is.null(best_ensemble_model) && length(best_ensemble_model) > 0 && nzchar(best_ensemble_model)) {
        if (nrow(eval_df) == 0) {
        fallback_proj <- BIOMOD_Projection(
        bm.mod = individual_models,
        proj.name = "fallback_projection",
        new.env = env_stack_filled,
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
                    new.env = env_stack_filled,
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