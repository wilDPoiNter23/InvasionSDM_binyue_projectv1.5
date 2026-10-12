# Original source: 0304VariableSelect.r
# Original source location: <LOCAL_CODE_ROOT>/0304VariableSelect.r
# Original source SHA-256: 9c6da1236e5a776e56acf93536272497f3f451e22830572e3a2dbdbe509745b6
# Public-safe original snapshot: archive/original_sources/local/0304VariableSelect.r
# Classification: SUPPORTING
# Publication modifications:
# - machine-specific absolute path literals replaced by configured publication_path aliases
# - local encoding decoded to UTF-8; source bytes hashed above
# - comments and historical private machine paths sanitized
# - working-directory calls retained with configured paths; no analyses executed
# Scientific modification: NONE
# This is full historical source, not a certified unattended/reproducible run.
if (!exists("publication_path", mode="function")) stop("Load config/bootstrap.R first")
library(terra)      # for rast() & extract()
library(caret)      # for findCorrelation()
library(corrplot)   # for corrplot()
library(usdm)

start_time <- Sys.time()            # 计时器
set.seed(2025)                      # 保证可重复

# Define paths
full_stack <- publication_path("HPC_INPUT_ROOT", "climate/full_stack.tif")
species_file <- publication_path("HPC_INPUT_ROOT", "Spartina_alterniflora_gbif.csv")
output_dir <- publication_path("HPC_INPUT_ROOT", "test/result")
# if (!dir.exists(output_dir)) dir.create(output_dir)

# Log file for progress
log_file <- file.path(output_dir, "biomod2_process.log")
log_msg <- function(msg) {
  cat(msg, "\n", file = log_file, append = TRUE)
  cat(msg, "\n")
}
log_msg("Script started.")

# Step 1: Load all factor stack
tryCatch({
  log_msg("Loading factor stack...")
  env_stack <- rast(full_stack)
 
  log_msg("factor stack loaded and saved.")
}, error = function(e) {
  log_msg(paste("Error in loading factor stack:", e$message))
  stop(e)
})
orig_vars   <- names(env_stack)
print(orig_vars)
# Step 2: Load Species Data
tryCatch({
  log_msg("Loading species data...")
  species_data <- read.csv(species_file)
  resp_var <- rep(1, nrow(species_data))
  resp_xy <- species_data[, c("decimalLongitude", "decimalLatitude")]
  # formatted_data <- readRDS('<HPC_INPUT_ROOT>/test/result/formatted_data.rds')
  log_msg("Species data loaded.")
}, error = function(e) {
  log_msg(paste("Error in loading species data:", e$message))
  stop(e)
})
# Step 3: Correlation analysis among predictors at species locations

# Generate pseudo-absence points
log_msg("Generating pseudo-absence points...")
n_pseudo <- 1000
pseudo_absence <- spatSample(env_stack[[1]], size = n_pseudo, method = "random", na.rm = TRUE, xy = TRUE)
colnames(pseudo_absence) <- c("x", "y")

# Combine presence + pseudo-absence
all_points <- rbind(
  data.frame(x = resp_xy$decimalLongitude, y = resp_xy$decimalLatitude, type = "presence"),
  data.frame(x = pseudo_absence$x, y = pseudo_absence$y, type = "pseudoabsence")
)

# Extract environmental values
log_msg("Extracting environmental values at presence + pseudo-absence points...")
env_vals <- terra::extract(env_stack, all_points[, c("x", "y")])
env_df   <- as.data.frame(env_vals)[, -1]    # drop ID column

# Pre‐processing to remove any zero‐variance (constant) predictors before correlation
## ---------------------------------------------------------------------------
##  STEP 1 – remove columns that are entirely NA or perfectly constant
## ---------------------------------------------------------------------------
all_NA  <- colSums(!is.na(env_df)) == 0
zero_SD <- sapply(env_df, function(x) sd(x, na.rm = TRUE) == 0)

constant_vars <- names(env_df)[all_NA | zero_SD]
if (length(constant_vars)) {
  message("Removed all-NA / zero-variance layers: ",
          paste(constant_vars, collapse = ", "))
  env_df    <- env_df[ , !(names(env_df) %in% constant_vars), drop = FALSE]
  env_stack <- env_stack[[ setdiff(names(env_stack), constant_vars) ]]
}
##------------------------------------------------------------
##  STEP 2 – remove columns with *too few* non-NA samples
##           (e.g. rasters covering a tiny coastal strip)
##------------------------------------------------------------
min_obs <- 20                          # 你可以调成 30、50……
obs_cnt <- colSums(!is.na(env_df))

few_obs_vars <- names(obs_cnt)[obs_cnt < min_obs]
if (length(few_obs_vars)) {
  log_msg(paste0("Removed vars with <", min_obs,
                     " non-NA samples: ", paste(few_obs_vars, collapse = ", ")))
  env_df    <- env_df[ , !(names(env_df) %in% few_obs_vars), drop = FALSE]
  env_stack <- env_stack[[ setdiff(names(env_stack), few_obs_vars) ]]
}

## ---------------------------------------------------------------------------
##  STEP 3 – build the correlation matrix and detect any remaining NA columns
## ---------------------------------------------------------------------------
corr_mat <- stats::cor(env_df, use = "pairwise.complete.obs")

if (anyNA(corr_mat)) {
  # identify every column/row that still generates an NA correlation
  bad_cols <- unique(c(rownames(corr_mat)[rowSums(is.na(corr_mat)) > 0],
                       colnames(corr_mat)[colSums(is.na(corr_mat)) > 0]))

  message("Removed layers that still produced NA correlations: ",
          paste(bad_cols, collapse = ", "))

  env_df    <- env_df[ , !(names(env_df) %in% bad_cols), drop = FALSE]
  env_stack <- env_stack[[ setdiff(names(env_stack), bad_cols) ]]

  # rebuild the matrix – now guaranteed to be NA-free
  corr_mat <- stats::cor(env_df, use = "pairwise.complete.obs")
}

stopifnot(!anyNA(corr_mat))   # safety fuse: will stop if NA sneaks back in


# Save the full correlation matrix to CSV
corr_path <- file.path(output_dir, "correlation_matrix.csv")
write.csv(corr_mat, file = corr_path, row.names = TRUE)
log_msg(paste("Saved correlation matrix to", corr_path))

# visualize
log_msg("Plotting correlation matrix…")
corrplot(corr_mat, method = "color", type = "upper", tl.cex = 0.6, 
         title = "Pairwise correlation (>0.8 will be removed)", mar = c(0,0,1,0))



# find and drop highly correlated variables
highCorrIdx <- findCorrelation(corr_mat, cutoff = 0.8)
highCorrVars <- colnames(corr_mat)[highCorrIdx]
log_msg(paste("Variables with |r| >= 0.75:", paste(highCorrVars, collapse = ", ")))


env_stack_corrOK <- env_stack[[ setdiff(names(env_stack), highCorrVars) ]]
log_msg(paste("Remaining variables after correlation filter:", paste(env_stack_corrOK, collapse = ", ")))

# ──────────────────────────────────────────────────────────────────────────────
# Step 4: VIF screening to further remove multicollinearity
# ──────────────────────────────────────────────────────────────────────────────
log_msg("Running VIF step (threshold = 10)…")
vif_sel <- vifstep(env_stack_corrOK, th = 10)

# results table
vif_results <- vif_sel@results
print(vif_results)
log_msg("VIF results printed.")




# identify dropped and kept
dropped_vif <- vif_sel@excluded              # variables removed by the algorithm

log_msg(paste("Dropped by VIF:", paste(dropped_vif, collapse = ", ")))

kept_vif <- vif_results$Variables

log_msg(paste("Kept after VIF:", paste(kept_vif, collapse = ", ")))

# build final stack
env_stack_final <- env_stack_corrOK[[ kept_vif ]] 
log_msg("Final environmental stack ready with variables:")
log_msg(paste(names(env_stack_final), collapse = ", "))

# Prepare summary of variable selection
kept_final     <- names(env_stack_final)

# Build a data.frame with Variable and Status
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

# 3. Save the summary table
summary_path <- file.path(output_dir, "variable_selection_summary.csv")
write.csv(var_status, file = summary_path, row.names = FALSE)
log_msg(paste("Saved variable selection summary to", summary_path))

# Track end time
end_time <- Sys.time()
log_msg(paste("Script finished. Total time:", round(difftime(end_time, start_time, units = "mins"), 2), "minutes."))