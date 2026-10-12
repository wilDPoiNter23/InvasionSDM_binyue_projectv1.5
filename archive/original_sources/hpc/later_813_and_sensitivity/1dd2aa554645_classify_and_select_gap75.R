# Original source: classify_and_select_gap75.R
# Original source location: ${WORK_ROOT}/publication_release_v1/scripts/classify_and_select_gap75.R
# Original source SHA-256: 1dd2aa5546457c1efed5a018cd7071906c0d3425f015dbeb442c3fce00d6a667
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
suppressMessages({ library(biomod2); library(terra) })
BASE <- "<WORK_ROOT>"
AUDIT <- file.path(BASE, "publication_release_v1/audits")

# Get master species list and old projections
master <- read.csv("<HPC_INPUT_ROOT>/script/output/species_list.csv", stringsAsFactors=FALSE)$species
old <- read.csv(file.path(BASE, "results/evaluations/projection_summary.csv"), stringsAsFactors=FALSE)$species
rep_mani <- read.csv(file.path(BASE, "publication_release_v1/manifests/final_repaired_projection_manifest_v2.csv"), stringsAsFactors=FALSE)$species

# Gap species
gap_sp <- setdiff(master, old)
gap_sp <- setdiff(gap_sp, rep_mani)
gap_sp <- gap_sp[gap_sp != "Diodia teres"]
gap_sp <- c(gap_sp, "Opuntia ficus-indica")
cat("Total gap species:", length(gap_sp), "\n")

# Current stack predictor names
cur_names <- names(rast(file.path(BASE, "data/predictors/full_stack_terrestrial_primary_v1.tif")))

results <- data.frame(
  species = gap_sp,
  model_predictors = character(length(gap_sp)),
  missing_predictors = character(length(gap_sp)),
  classification = character(length(gap_sp)),
  n_models_built = integer(length(gap_sp)),
  n_primary_qualified = integer(length(gap_sp)),
  n_fallback_qualified = integer(length(gap_sp)),
  best_model = character(length(gap_sp)),
  best_TSS = numeric(length(gap_sp)),
  best_ROC = numeric(length(gap_sp)),
  stringsAsFactors = FALSE
)

for (i in seq_along(gap_sp)) {
  sp <- gap_sp[i]
  spd <- gsub(" ", ".", sp)
  mf <- list.files(file.path(BASE, "models", spd), pattern="\\.models\\.out$")
  # Only exclude .ensemble.models.out (ensemble model objects), 
  # NOT the main MultiModel_Ensemble.models.out containers
  mf <- mf[!grepl("\\.ensemble\\.models\\.out$", mf, ignore.case=TRUE)]
  
  if (length(mf) == 0) {
    results$classification[i] <- "NO_MODEL_FILE"
    if (i %% 10 == 0) cat(sprintf("  %d/%d (NO_FILE)\n", i, length(gap_sp)))
    next
  }
  
  # Load first model to get predictor names
  m <- tryCatch(get(load(file.path(BASE, "models", spd, mf[1]))), error=function(e) NULL)
  if (is.null(m)) {
    results$classification[i] <- "MODEL_LOAD_FAILED"
    if (i %% 10 == 0) cat(sprintf("  %d/%d (LOAD_FAIL)\n", i, length(gap_sp)))
    next
  }
  
  mod_preds <- BIOMOD_LOADING_NAMES(m)$expl_var
  results$model_predictors[i] <- paste(mod_preds, collapse=";")
  
  missing <- setdiff(mod_preds, cur_names)
  has_viirs <- "VIIRS_2023_2.5min" %in% missing
  has_hycom <- any(c("salinity_mean","salinity_range","temperature_mean","temperature_range") %in% missing)
  has_clay <- "clay_mean" %in% missing
  has_other <- length(setdiff(missing, c("VIIRS_2023_2.5min","salinity_mean","salinity_range","temperature_mean","temperature_range","clay_mean"))) > 0
  
  results$missing_predictors[i] <- paste(missing, collapse=";")
  
  if (length(missing) == 0) {
    results$classification[i] <- "DIRECT_34STACK"
  } else if (has_hycom && has_viirs) {
    results$classification[i] <- "MIXED_HYCOM_VIIRS"
  } else if (has_hycom) {
    results$classification[i] <- "HYCOM_ONLY"
  } else if (has_viirs) {
    results$classification[i] <- "VIIRS_ONLY"
  } else if (has_clay) {
    results$classification[i] <- "CLAY"
  } else {
    results$classification[i] <- "OTHER"
  }
  
  # Model selection across ALL model files (not just first)
  all_tss <- c(); all_roc <- c(); all_names <- c()
  for (f in mf) {
    m2 <- tryCatch(get(load(file.path(BASE, "models", spd, f))), error=function(e) NULL)
    if (is.null(m2)) next
    ev <- tryCatch(get_evaluations(m2), error=function(e) NULL)
    if (is.null(ev)) next
    mn <- BIOMOD_LOADING_NAMES(m2)$mod_names
    for (mn_i in mn) {
      tss_vals <- tryCatch(ev["TSS","Testing.data", mn_i,,drop=FALSE], error=function(e) NULL)
      roc_vals <- tryCatch(ev["ROC","Testing.data", mn_i,,drop=FALSE], error=function(e) NULL)
      if (!is.null(tss_vals) && !is.null(roc_vals)) {
        tss_max <- max(tss_vals, na.rm=TRUE)
        roc_max <- max(roc_vals, na.rm=TRUE)
        if (is.finite(tss_max) && is.finite(roc_max)) {
          all_tss <- c(all_tss, tss_max)
          all_roc <- c(all_roc, roc_max)
          all_names <- c(all_names, mn_i)
        }
      }
    }
  }
  
  results$n_models_built[i] <- length(all_tss)
  
  if (length(all_tss) > 0) {
    primary <- which(all_tss > 0.8 & all_roc > 0.8)
    fallback <- which(all_tss > 0.7 & all_roc > 0.7)
    results$n_primary_qualified[i] <- length(primary)
    results$n_fallback_qualified[i] <- length(fallback)
    
    if (length(primary) > 0) {
      best_idx <- primary[which.max(all_tss[primary])]
      results$best_model[i] <- all_names[best_idx]
      results$best_TSS[i] <- all_tss[best_idx]
      results$best_ROC[i] <- all_roc[best_idx]
    } else if (length(fallback) > 0) {
      best_idx <- fallback[which.max(all_tss[fallback])]
      results$best_model[i] <- all_names[best_idx]
      results$best_TSS[i] <- all_tss[best_idx]
      results$best_ROC[i] <- all_roc[best_idx]
    }
  }
  
  if (i %% 10 == 0) cat(sprintf("  %d/%d done\n", i, length(gap_sp)))
}

cat("  All", length(gap_sp), "done\n\n")

# Summary
cat("=== Classification Summary ===\n")
print(table(results$classification))
cat("\n")

# Write
write.csv(results, file.path(AUDIT, "gap75_full_classification_and_selection.csv"), row.names=FALSE)

# Write separate lists for each workstream
direct8 <- results[results$classification == "DIRECT_34STACK",]
viirs57 <- results[results$classification == "VIIRS_ONLY",]
hycom10 <- results[results$classification %in% c("MIXED_HYCOM_VIIRS","HYCOM_ONLY"),]

cat(sprintf("\nDIRECT_34STACK: %d\n", nrow(direct8)))
cat(sprintf("VIIRS_ONLY: %d\n", nrow(viirs57)))
cat(sprintf("HYCOM: %d\n", nrow(hycom10)))

write.csv(direct8, file.path(AUDIT, "gap75_workstream_A_direct8.csv"), row.names=FALSE)
write.csv(viirs57, file.path(AUDIT, "gap75_workstream_B_viirs57.csv"), row.names=FALSE)
write.csv(hycom10, file.path(AUDIT, "gap75_workstream_C_hycom10.csv"), row.names=FALSE)

# Model selection summary
cat("\n=== Model Selection Summary ===\n")
for (cl in unique(results$classification)) {
  sub <- results[results$classification == cl,]
  has_model <- sum(sub$n_primary_qualified > 0)
  has_fallback <- sum(sub$n_fallback_qualified > 0 & sub$n_primary_qualified == 0)
  no_model <- sum(sub$n_fallback_qualified == 0)
  cat(sprintf("%s: primary=%d fallback=%d none=%d (total=%d)\n", 
      cl, has_model, has_fallback, no_model, nrow(sub)))
}

cat("\nReady to project (has qualified models):\n")
ready <- results[results$n_fallback_qualified > 0,]
for (cl in c("VIIRS_ONLY","DIRECT_34STACK","HYCOM_ONLY","MIXED_HYCOM_VIIRS")) {
  total_cl <- sum(results$classification == cl)
  ready_cl <- sum(ready$classification == cl)
  cat(sprintf("  %s: %d / %d ready\n", cl, ready_cl, total_cl))
}
