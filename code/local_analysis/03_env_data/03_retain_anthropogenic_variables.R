library(terra)      # for rast() & extract()
library(caret)      # for findCorrelation()
library(corrplot)   # for corrplot()
library(usdm)

library(biomod2)



start_time <- Sys.time()            # Timer
set.seed(2025)                      # Ensure reproducibility

# Define paths
# full_stack <- "/thfs1/home/qs_songsj1/data/climate/full_stack.tif"
species_file <- "/thfs1/home/qs_songsj1/data/Spartina_alterniflora_gbif.csv"
output_dir <- "/thfs1/home/qs_songsj1/data/test/result"
env_stack_final <- readRDS('/thfs1/home/qs_songsj1/data/test/result/env_stack_final.rds')
formatted_data <- readRDS('/thfs1/home/qs_songsj1/data/test/result/formatted_data.rds')
# if (!dir.exists(output_dir)) dir.create(output_dir)

# Log file for progress
log_file <- file.path(output_dir, "biomod2_process.log")
log_msg <- function(message) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  full_message <- paste0("[", timestamp, "] ", message)
  cat(full_message, "\n", file = log_file, append = TRUE)
  print(full_message)
}

# Step 1: Load all factor stack
# tryCatch({
#   log_msg("Loading factor stack...")
#   env_stack <- rast(full_stack)
 
#   log_msg("factor stack loaded and saved.")
# }, error = function(e) {
#   log_msg(paste("Error in loading factor stack:", e$message))
#   stop(e)
# })
# orig_vars   <- names(env_stack)
# print(orig_vars)

# priority_vars <- c("reclamation_raster", "VIIRS_2023_2.5min", 
#                    "GDP2020_2.5min", "BUILT_S_E2025_2.5min", "Pop2020_2.5min")

# Step 2: Load Species Data
# tryCatch({
#   log_msg("Loading species data...")
#   species_data <- read.csv(species_file)
#   resp_var <- rep(1, nrow(species_data))
#   resp_xy <- species_data[, c("decimalLongitude", "decimalLatitude")]
#   # formatted_data <- readRDS('/thfs1/home/qs_songsj1/data/test/result/formatted_data.rds')
#   log_msg("Species data loaded.")
# }, error = function(e) {
#   log_msg(paste("Error in loading species data:", e$message))
#   stop(e)
# })
# Step 3: Correlation analysis among predictors at species locations

# Generate pseudo-absence points
# log_msg("Generating pseudo-absence points...")
# n_pseudo <- 1000
# pseudo_absence <- spatSample(env_stack[[1]], size = n_pseudo, method = "random", na.rm = TRUE, xy = TRUE)
# colnames(pseudo_absence) <- c("x", "y")

# # Combine presence + pseudo-absence
# all_points <- rbind(
#   data.frame(x = resp_xy$decimalLongitude, y = resp_xy$decimalLatitude, type = "presence"),
#   data.frame(x = pseudo_absence$x, y = pseudo_absence$y, type = "pseudoabsence")
# )

# # Extract environmental values
# log_msg("Extracting environmental values at presence + pseudo-absence points...")
# env_vals <- terra::extract(env_stack, all_points[, c("x", "y")])
# env_df   <- as.data.frame(env_vals)[, -1]    # drop ID column

# Pre‐processing to remove any zero‐variance (constant) predictors before correlation
## ---------------------------------------------------------------------------
##  STEP 1 – remove columns that are entirely NA or perfectly constant
## ---------------------------------------------------------------------------
# all_NA  <- colSums(!is.na(env_df)) == 0
# zero_SD <- sapply(env_df, function(x) sd(x, na.rm = TRUE) == 0)

# constant_vars <- names(env_df)[all_NA | zero_SD]
# if (length(constant_vars)) {
#   message("Removed all-NA / zero-variance layers: ",
#           paste(constant_vars, collapse = ", "))
#   env_df    <- env_df[ , !(names(env_df) %in% constant_vars), drop = FALSE]
#   env_stack <- env_stack[[ setdiff(names(env_stack), constant_vars) ]]
# }
# ##------------------------------------------------------------
# ##  STEP 2 – remove columns with *too few* non-NA samples
# ##           (e.g. rasters covering a tiny coastal strip)
# ##------------------------------------------------------------
# min_obs <- 20                          # You can adjust to 30, 50...
# obs_cnt <- colSums(!is.na(env_df))

# few_obs_vars <- names(obs_cnt)[obs_cnt < min_obs]
# if (length(few_obs_vars)) {
#   log_msg(paste0("Removed vars with <", min_obs,
#                      " non-NA samples: ", paste(few_obs_vars, collapse = ", ")))
#   env_df    <- env_df[ , !(names(env_df) %in% few_obs_vars), drop = FALSE]
#   env_stack <- env_stack[[ setdiff(names(env_stack), few_obs_vars) ]]
# }

# ## ---------------------------------------------------------------------------
# ##  STEP 3 – build the correlation matrix and detect any remaining NA columns
# ## ---------------------------------------------------------------------------
# corr_mat <- stats::cor(env_df, use = "pairwise.complete.obs")

# if (anyNA(corr_mat)) {
#   # identify every column/row that still generates an NA correlation
#   bad_cols <- unique(c(rownames(corr_mat)[rowSums(is.na(corr_mat)) > 0],
#                        colnames(corr_mat)[colSums(is.na(corr_mat)) > 0]))

#   message("Removed layers that still produced NA correlations: ",
#           paste(bad_cols, collapse = ", "))

#   env_df    <- env_df[ , !(names(env_df) %in% bad_cols), drop = FALSE]
#   env_stack <- env_stack[[ setdiff(names(env_stack), bad_cols) ]]

#   # rebuild the matrix – now guaranteed to be NA-free
#   corr_mat <- stats::cor(env_df, use = "pairwise.complete.obs")
# }

# stopifnot(!anyNA(corr_mat))   # safety fuse: will stop if NA sneaks back in


# # Save the full correlation matrix to CSV
# corr_path <- file.path(output_dir, "PriorityVariablecorrelation_matrix.csv")
# write.csv(corr_mat, file = corr_path, row.names = TRUE)
# log_msg(paste("Saved correlation matrix to", corr_path))

# # visualize
# log_msg("Plotting correlation matrix…")
# corrplot(corr_mat, method = "color", type = "upper", tl.cex = 0.6, 
#          title = "Pairwise correlation (>0.8 will be removed)", mar = c(0,0,1,0))



# # find and drop highly correlated variables
# highCorrIdx <- findCorrelation(corr_mat, cutoff = 0.8)
# highCorrVars <- colnames(corr_mat)[highCorrIdx]

# log_msg(paste("Variables with |r| >= 0.8:", paste(highCorrVars, collapse = ", ")))

# highCorrVars_filtered <- setdiff(highCorrVars, priority_vars)


# env_stack_corrOK <- env_stack[[setdiff(names(env_stack), highCorrVars_filtered)]]

# log_msg(paste("Remaining variables after correlation filter:", paste(env_stack_corrOK, collapse = ", ")))

# # ──────────────────────────────────────────────────────────────────────────────
# # Step 4: VIF screening to further remove multicollinearity
# # ──────────────────────────────────────────────────────────────────────────────
# log_msg("Running VIF step (threshold = 10)…")
# vif_sel <- vifstep(env_stack_corrOK, th = 10)


# str(vif_sel)

# # results table
# vif_results <- vif_sel@results
# print(vif_results)
# log_msg("VIF results printed.")

# # identify dropped and kept


# excluded_vars <- vif_sel@excluded

# dropped_vif <- setdiff(excluded_vars, priority_vars)
# log_msg(paste("Dropped by VIF:", paste(dropped_vif, collapse = ", ")))
# kept_vif <- union(setdiff(vif_results$Variables, dropped_vif), intersect(excluded_vars, priority_vars))

# log_msg(paste("Kept after VIF:", paste(kept_vif, collapse = ", ")))

# # build final stack
# env_stack_final <- env_stack_corrOK[[ kept_vif ]] 
# log_msg("Final environmental stack ready with variables:")
# log_msg(paste(names(env_stack_final), collapse = ", "))

# # Prepare summary of variable selection
# kept_final     <- names(env_stack_final)

# # Build a data.frame with Variable and Status
# var_status <- data.frame(
#   Variable = orig_vars,
#   Status = vapply(orig_vars, function(v) {
    
#     if (v %in% constant_vars) {
#       "Removed_all_NA_or_zero_variance"
      
#     } else if (v %in% few_obs_vars) {
#       "Removed_too_few_samples"
      
#     } else if (v %in% bad_cols) {
#       "Removed_NA_in_correlation"
      
#     } else if (v %in% highCorrVars) {
#       "Removed_by_pairwise_correlation"
      
#     } else if (v %in% dropped_vif) {
#       "Removed_by_VIF"
      
#     } else if (v %in% names(env_stack_final)) {
#       "Kept"
      
#     } else {
#       "Other"
#     }
#   }, character(1)),
#   stringsAsFactors = FALSE
# )

# # 3. Save the summary table
# summary_path <- file.path(output_dir, "PriorityVariable_selection_summary.csv")
# write.csv(var_status, file = summary_path, row.names = FALSE)
# log_msg(paste("Saved variable selection summary to", summary_path))


# saveRDS(env_stack_final, file.path(output_dir, "env_stack_final.rds"))



############ Format Data for BIOMOD2
#  tryCatch({
#   log_msg("Formatting data for BIOMOD2...")
#   formatted_data <- BIOMOD_FormatingData(
#     resp.var = resp_var,
#     expl.var = env_stack_final,
#     resp.xy = resp_xy,
#     resp.name = "Spartina_alterniflora",
#     PA.nb.rep = 1,
#     PA.nb.absences = 1000,
#     PA.strategy = "disk",
#     PA.dist.min = 20000, # 20 km
#     PA.dist.max = 50000,
#     na.rm = TRUE

#   )
#  saveRDS(formatted_data, file = file.path(output_dir, "formatted_data.rds"))
#  log_msg("Data formatted and saved.")
# }, error = function(e) {
#   log_msg(paste("Error in formatting data for BIOMOD2:", e$message))
#   stop(e)
# })

############ Step 4: Set Model Options
  # ------------ 1. GLM model tuning ------------
  log_msg("Starting GLM parameter tuning")
# Set modeling options for GLM
  opt_glm <- tryCatch({
    bm_ModelingOptions(
      data.type = 'binary',
      models = "GLM",
      strategy = 'default'
    )
  }, error = function(e) {
    log_msg(paste("Error in GLM modeling options:", e$message))
    return(NULL)
  })
  
  if (!is.null(opt_glm)) {
    # Create cross-validation settings
    cv_kfold_glm <- tryCatch({
      bm_CrossValidation(
       formatted_data,
        strategy = 'kfold',
        nb.rep = 1,
        k = 10,
        do.full.models = TRUE
      )
    }, error = function(e) {
      log_msg(paste("Error in GLM cross-validation:", e$message))
      return(NULL)
    })
    
    if (!is.null(cv_kfold_glm)) {
      # GLM parameter tuning
      tuned_glm <- tryCatch({
        bm_Tuning(
          model = "GLM",
          tuning.fun = "glm", 
          bm.options = opt_glm@options$GLM.binary.glm.glm,
          do.formula = TRUE,
          bm.format = formatted_data,
          calib.lines = cv_kfold_glm
        )
      }, error = function(e) {
        log_msg(paste("Error in GLM tuning:", e$message))
        return(NULL)
      })
      
      if (!is.null(tuned_glm)) {
        log_msg("GLM tuning completed successfully")
        saveRDS(tuned_glm,  file = file.path(output_dir, 'tuned_parameters_GLM.rds'))
      }
    }
  }
 # ------------ 2. GAM model tuning ------------
  log_msg("Starting GAM parameter tuning")
  
  # Set modeling options for GAM
  opt_gam <- tryCatch({
    bm_ModelingOptions(
      data.type = 'binary',
      models = "GAM",
      strategy = 'default'
    )
  }, error = function(e) {
    log_msg(paste("Error in GAM modeling options:", e$message))
    return(NULL)
  })
  
  if (!is.null(opt_gam)) {
    # Create cross-validation settings
    cv_kfold_gam <- tryCatch({
      bm_CrossValidation(
       formatted_data,
        strategy = 'kfold',
        nb.rep = 1,
        k = 10,
        do.full.models = TRUE
      )
    }, error = function(e) {
      log_msg(paste("Error in GAM cross-validation:", e$message))
      return(NULL)
    })
    
    if (!is.null(cv_kfold_gam)) {
      # GAM parameter tuning
      tuned_gam <- tryCatch({
        bm_Tuning(
          model = "GAM",
          tuning.fun = "gam", 
          bm.options = opt_gam@options$GAM.binary.gam.gam,
          do.formula = TRUE,
          bm.format =formatted_data,
          calib.lines = cv_kfold_gam
        )
      }, error = function(e) {
        log_msg(paste("Error in GAM tuning:", e$message))
        return(NULL)
      })
      
      if (!is.null(tuned_gam)) {
        log_msg("GAM tuning completed successfully")
        saveRDS(tuned_gam, file = file.path(output_dir, 'tuned_parameters_GAM.rds'))
      }
    }
  }

  # ------------ 3. GBM model tuning ------------
  log_msg("Starting GBM parameter tuning")
  
  # Set modeling options for GBM
  opt_gbm <- tryCatch({
    bm_ModelingOptions(
      data.type = 'binary',
      models = "GBM",
      strategy = 'default'
    )
  }, error = function(e) {
    log_msg(paste("Error in GBM modeling options:", e$message))
    return(NULL)
  })
  
  if (!is.null(opt_gbm)) {
    # Create cross-validation settings
    cv_kfold_gbm <- tryCatch({
      bm_CrossValidation(
       formatted_data,
        strategy = 'kfold',
        nb.rep = 1,
        k = 10,
        do.full.models = TRUE
      )
    }, error = function(e) {
      log_msg(paste("Error in GBM cross-validation:", e$message))
      return(NULL)
    })
    
    if (!is.null(cv_kfold_gbm)) {
      # GBM parameter tuning
      tuned_gbm <- tryCatch({
        bm_Tuning(
          model = "GBM",
          tuning.fun = "gbm", 
          bm.options = opt_gbm@options$GBM.binary.gbm.gbm,
          do.formula = FALSE,
          bm.format =formatted_data,
          calib.lines = cv_kfold_gbm
        )
      }, error = function(e) {
        log_msg(paste("Error in GBM tuning:", e$message))
        return(NULL)
      })
      
      if (!is.null(tuned_gbm)) {
        log_msg("GBM tuning completed successfully")
        saveRDS(tuned_gbm, file = file.path(output_dir, 'tuned_parameters_GBM.rds'))
      }
    }
  }

  # ------------ 4. RF model tuning ------------
  log_msg("Starting RF parameter tuning")
  
  # Set modeling options for RF
  opt_rf <- tryCatch({
    bm_ModelingOptions(
      data.type = 'binary',
      models = "RF",
      strategy = 'default'
    )
  }, error = function(e) {
    log_msg(paste("Error in RF modeling options:", e$message))
    return(NULL)
  })
  
  if (!is.null(opt_rf)) {
    # Create cross-validation settings
    cv_kfold_rf <- tryCatch({
      bm_CrossValidation(
       formatted_data,
        strategy = 'kfold',
        nb.rep = 1,
        k = 10,
        do.full.models = TRUE
      )
    }, error = function(e) {
      log_msg(paste("Error in RF cross-validation:", e$message))
      return(NULL)
    })
    
    if (!is.null(cv_kfold_rf)) 
    {
      # RF parameter tuning
      tuned_rf <- tryCatch({
        bm_Tuning(
          model = "RF",
          tuning.fun = "randomForest", 
          bm.options = opt_rf@options$RF.binary.randomForest.randomForest,
          do.formula = FALSE,
          bm.format = formatted_data,
          calib.lines = cv_kfold_rf
        )
      }, error = function(e) {
        log_msg(paste("Error in RF tuning:", e$message))
        return(NULL)
      })
      
      if (!is.null(tuned_rf)) {
        log_msg("RF tuning completed successfully")
        saveRDS(tuned_rf, file = file.path(output_dir, 'tuned_parameters_RF.rds'))
      }
    }
  }

  # ------------ 5. MAXENT model tuning ------------
  log_msg("Starting MAXENT parameter tuning")
  
  # Set modeling options for MAXENT
  opt_maxent <- tryCatch({
    bm_ModelingOptions(
      data.type = 'binary',
      models = "MAXENT.Phillips",
      strategy = 'default'
    )
  }, error = function(e) {
    log_msg(paste("Error in MAXENT modeling options:", e$message))
    return(NULL)
  })
  
  if (!is.null(opt_maxent)) {
    # Create cross-validation settings
    cv_kfold_maxent <- tryCatch({
      bm_CrossValidation(
       formatted_data,
        strategy = 'kfold',
        nb.rep = 1,
        k = 10,
        do.full.models = TRUE
      )
    }, error = function(e) {
      log_msg(paste("Error in MAXENT cross-validation:", e$message))
      return(NULL)
    })
    
    if (!is.null(cv_kfold_maxent)) {
      # MAXENT parameter tuning - Note: MAXENT may require special handling
      tuned_maxent <- tryCatch({
        bm_Tuning(
          model = "MAXENT.Phillips",
          tuning.fun = "maxent", 
          bm.options = opt_maxent@options$MAXENT.Phillips.binary.maxent.maxent,
          do.formula = FALSE,
          bm.format =formatted_data,
          calib.lines = cv_kfold_maxent
        )
      }, error = function(e) {
        log_msg(paste("Error in MAXENT tuning:", e$message))
        return(NULL)
      })
      
      if (!is.null(tuned_maxent)) {
        log_msg("MAXENT tuning completed successfully")
        saveRDS(tuned_maxent, file = file.path(output_dir, 'tuned_parameters_MAXENT.rds'))
      }
    }
  }


 # ------------ 6. Model building with tuned parameters ------------
  log_msg("Starting model building with tuned parameters")
  
  # Check which tuned models are available
  available_models <- c()
  model_tuning_results <- list()
  
  if (exists("tuned_glm") && !is.null(tuned_glm)) {
    available_models <- c(available_models, "GLM")
    model_tuning_results$GLM.binary.glm.glm <- tuned_glm
  }
  
  if (exists("tuned_gam") && !is.null(tuned_gam)) {
    available_models <- c(available_models, "GAM")
    model_tuning_results$GAM.binary.gam.gam <- tuned_gam
  }
  
  if (exists("tuned_gbm") && !is.null(tuned_gbm)) {
    available_models <- c(available_models, "GBM")
    model_tuning_results$GBM.binary.gbm.gbm <- tuned_gbm
  }
  
  if (exists("tuned_rf") && !is.null(tuned_rf)) {
    available_models <- c(available_models, "RF")
    model_tuning_results$RF.binary.randomForest.randomForest <- tuned_rf
  }
  
  if (exists("tuned_maxent") && !is.null(tuned_maxent)) {
    available_models <- c(available_models, "MAXENT.Phillips")
    model_tuning_results$MAXENT.Phillips.binary.maxent.maxent <- tuned_maxent
  }
  
  if (length(available_models) > 0) {
    log_msg(paste("Models available for ensemble:", paste(available_models, collapse = ", ")))
    
    # Create user-defined modeling options
    opt_user <- tryCatch({
      bm_ModelingOptions(
        data.type = 'binary',
        models = available_models,
        strategy = "user.defined",
        user.val = model_tuning_results,
        user.base = "bigboss",
        bm.format =formatted_data
      )
    }, error = function(e) {
      log_msg(paste("Error in creating user-defined modeling options:", e$message))
      return(NULL)
    })
    
    if (!is.null(opt_user)) {
      # Choose an appropriate CV set based on available models
      cv_to_use <- NULL
      if (exists("cv_kfold_glm") && !is.null(cv_kfold_glm)) {
        cv_to_use <- cv_kfold_glm
      } else if (exists("cv_kfold_gbm") && !is.null(cv_kfold_gbm)) {
        cv_to_use <- cv_kfold_gbm
      } else if (exists("cv_kfold_rf") && !is.null(cv_kfold_rf)) {
        cv_to_use <- cv_kfold_rf
      } else if (exists("cv_kfold_gam") && !is.null(cv_kfold_gam)) {
        cv_to_use <- cv_kfold_gam
      } else if (exists("cv_kfold_maxent") && !is.null(cv_kfold_maxent)) {
        cv_to_use <- cv_kfold_maxent
      }
      
      if (!is.null(cv_to_use)) {
        # Model building with tuned parameters
        biomod_models <- tryCatch({
          BIOMOD_Modeling(
           formatted_data,
            modeling.id = "All_Models_10Kfold_Tuned",
            models = available_models,
            CV.strategy = "user.defined",
            CV.user.table = cv_to_use,
            OPT.strategy = "user.defined",
            OPT.user = opt_user,
            metric.eval = c('TSS', 'ROC'),
            var.import = 3,
            seed.val = 42
          )
        }, error = function(e) {
          log_msg(paste("Error in model building:", e$message))
          return(NULL)
        })
        
        if (!is.null(biomod_models)) {
          log_msg("All models built successfully")
          saveRDS(biomod_models, file = file.path(output_dir, 'biomod_models_tuned.rds'))
  
  # ------------ 7. Ensemble modeling ------------
  log_msg("Creating ensemble models...")
  
 biomod_ensemble <- tryCatch({
            BIOMOD_EnsembleModeling(
              modeling.output = biomod_models,
              chosen.models = 'all',
              em.by = 'all',
              eval.metric = c('TSS', 'ROC'),
              eval.metric.quality.threshold = c(0.7, 0.8),
              prob.mean = TRUE,
              prob.cv = TRUE,
              prob.ci = TRUE,
              prob.ci.alpha = 0.05,
              committee.averaging = TRUE,
              prob.mean.weight = TRUE,
              prob.mean.weight.decay = 'proportional'
            )
          }, error = function(e) {
            log_msg(paste("Error in ensemble modeling:", e$message))
            return(NULL)
          })
          
          if (!is.null(biomod_ensemble)) {
            log_msg("Ensemble modeling completed successfully")
    # Save ensemble model results
    saveRDS(biomod_ensemble, file.path(output_dir, "biomod_ensemble.rds"))
    log_msg("Ensemble models created successfully.")
       # ------------ 8. Model projection ------------
            log_msg("Starting model projection")
            
            # Check if new environmental data exists
            if (exists("new_env_data")) {
              # Individual model projection
              biomod_projection <- tryCatch({
                BIOMOD_Projection(
                  modeling.output = biomod_models,
                  new.env = env_stack_final,
                  proj.name = 'current',
                  selected.models = 'all',
                  binary.meth = c('TSS', 'ROC'),
                  compress = TRUE,
                  build.clamping.mask = TRUE,
                  output.format = '.tif'
                )
              }, error = function(e) {
                log_msg(paste("Error in model projection:", e$message))
                return(NULL)
              })
              
              if (!is.null(biomod_projection)) {
                log_msg("Model projection completed successfully")
                saveRDS(biomod_projection, file.path(output_dir, 'biomod_projection_tuned.rds'))

            # ------------ 8. Model projection ------------
            log_msg("Starting model projection")
            
            # Check if new environmental data exists
            if (exists("new_env_data")) {
              # Individual model projection
              biomod_projection <- tryCatch({
                BIOMOD_Projection(
                  modeling.output = biomod_models,
                  new.env = new_env_data,
                  proj.name = 'current',
                  selected.models = 'all',
                  binary.meth = c('TSS', 'ROC'),
                  compress = TRUE,
                  build.clamping.mask = TRUE,
                  output.format = '.tif'
                )
              }, error = function(e) {
                log_msg(paste("Error in model projection:", e$message))
                return(NULL)
              })
              
              if (!is.null(biomod_projection)) {
                log_msg("Model projection completed successfully")
                saveRDS(biomod_projection, file.path(output_dir, 'biomod_projection_tuned.rds'))
                
                # Ensemble model projection
                biomod_ensemble_projection <- tryCatch({
                  BIOMOD_EnsembleForecasting(
                    EM.output = biomod_ensemble,
                    projection.output = biomod_projection,
                    binary.meth = c('TSS', 'ROC'),
                    filtered.meth = c('TSS', 'ROC'),
                    compress = TRUE,
                    output.format = ".tif"
                  )
                }, error = function(e) {
                  log_msg(paste("Error in ensemble projection:", e$message))
                  return(NULL)
                })
                
                if (!is.null(biomod_ensemble_projection)) {
                  log_msg("Ensemble projection completed successfully")
                  saveRDS(biomod_ensemble_projection, file.path(output_dir, 'biomod_ensemble_projection_tuned.rds'))
                }
              }
            } else {
              log_msg("Warning: No new_env_data found for projection. Skipping projection steps.")
            }
          }
        }
      } else {
        log_msg("Error: No valid cross-validation set found for modeling")
      }
    }
  } else {
    log_msg("Error: No tuned models available for ensemble modeling")
  }
}, error = function(e) {
  log_msg(paste("Fatal error in workflow:", e$message))
}}

# Track end time
end_time <- Sys.time()
log_msg(paste("Script finished. Total time:", round(difftime(end_time, start_time, units = "mins"), 2), "minutes."))

