# Original source: classify_and_select_gap75_v2.R
# Original source location: ${WORK_ROOT}/publication_release_v1/scripts/classify_and_select_gap75_v2.R
# Original source SHA-256: 6aae2a264743e9f4e08a4dfcd253845d7f2b8ea05b3519f8bccee35d4ea9fb90
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
suppressMessages({ library(biomod2); library(terra) })
BASE <- "<WORK_ROOT>"
AUDIT <- file.path(BASE, "publication_release_v1/audits")

# ── Gap species list ──
master <- read.csv("<HPC_INPUT_ROOT>/script/output/species_list.csv", stringsAsFactors=FALSE)$species
old <- read.csv(file.path(BASE, "results/evaluations/projection_summary.csv"), stringsAsFactors=FALSE)$species
rep_mani <- read.csv(file.path(BASE, "publication_release_v1/manifests/final_repaired_projection_manifest_v2.csv"), stringsAsFactors=FALSE)$species

gap_sp <- setdiff(master, old)
gap_sp <- setdiff(gap_sp, rep_mani)
gap_sp <- gap_sp[gap_sp != "Diodia teres"]
gap_sp <- c(gap_sp, "Opuntia ficus-indica")
cat("Total gap species:", length(gap_sp), "\n\n")

# ── Current stack predictor names ──
cur_names <- names(rast(file.path(BASE, "data/predictors/full_stack_terrestrial_primary_v1.tif")))

# ── Results container ──
results <- data.frame(
  species = gap_sp,
  model_predictors = character(length(gap_sp)),
  n_model_predictors = integer(length(gap_sp)),
  missing_predictors = character(length(gap_sp)),
  classification = character(length(gap_sp)),
  n_model_files = integer(length(gap_sp)),
  n_models_built = integer(length(gap_sp)),
  n_primary_qualified = integer(length(gap_sp)),
  n_fallback_qualified = integer(length(gap_sp)),
  best_model = character(length(gap_sp)),
  best_TSS = numeric(length(gap_sp)),
  best_ROC = numeric(length(gap_sp)),
  best_tier = character(length(gap_sp)),
  stringsAsFactors = FALSE
)

for (i in seq_along(gap_sp)) {
  sp <- gap_sp[i]
  spd <- gsub(" ", ".", sp)
  
  # Find model files (exclude .ensemble.models.out which are ensemble model objects)
  mf <- list.files(file.path(BASE, "models", spd), pattern="\\.models\\.out$")
  mf <- mf[!grepl("\\.ensemble\\.models\\.out$", mf, ignore.case=TRUE)]
  results$n_model_files[i] <- length(mf)
  
  if (length(mf) == 0) {
    results$classification[i] <- "NO_MODEL_FILE"
    if (i %% 10 == 0) cat(sprintf("  [%d/%d] %s: NO_MODEL_FILE\n", i, length(gap_sp), sp))
    next
  }
  
  # Load first model to get predictor names (S4 slot access, no formal data loading needed)
  m <- tryCatch(get(load(file.path(BASE, "models", spd, mf[1]))), error=function(e) NULL)
  if (is.null(m)) {
    results$classification[i] <- "MODEL_LOAD_FAILED"
    if (i %% 10 == 0) cat(sprintf("  [%d/%d] %s: LOAD_FAILED\n", i, length(gap_sp), sp))
    next
  }
  
  mod_preds <- m@expl.var.names
  results$model_predictors[i] <- paste(mod_preds, collapse=";")
  results$n_model_predictors[i] <- length(mod_preds)
  
  # Check missing from 34-stack
  missing <- setdiff(mod_preds, cur_names)
  has_viirs <- "VIIRS_2023_2.5min" %in% missing
  has_hycom <- any(c("salinity_mean","salinity_range","temperature_mean","temperature_range") %in% missing)
  has_clay <- "clay_mean" %in% missing
  
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
  
  # ── Model selection with frozen thresholds ──
  all_tss <- c(); all_roc <- c(); all_names <- c()
  n_warned <- 0
  for (f in mf) {
    m2 <- tryCatch(get(load(file.path(BASE, "models", spd, f))), error=function(e) NULL)
    if (is.null(m2)) next
    ev <- tryCatch(m2@models.evaluation@val, error=function(e) NULL)
    if (is.null(ev) || nrow(ev) == 0) next
    
    mod_names <- unique(ev$full.name)
    for (mn in mod_names) {
      sub <- ev[ev$full.name == mn,]
      tss_row <- sub[sub$metric.eval == "TSS",]
      roc_row <- sub[sub$metric.eval == "ROC",]
      
      if (nrow(tss_row) > 0 && nrow(roc_row) > 0) {
        tss_val <- suppressWarnings(max(tss_row$validation, na.rm=TRUE))
        roc_val <- suppressWarnings(max(roc_row$validation, na.rm=TRUE))
        if (is.finite(tss_val) && is.finite(roc_val)) {
          all_tss <- c(all_tss, tss_val)
          all_roc <- c(all_roc, roc_val)
          all_names <- c(all_names, mn)
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
      results$best_tier[i] <- "PRIMARY"
    } else if (length(fallback) > 0) {
      best_idx <- fallback[which.max(all_tss[fallback])]
      results$best_model[i] <- all_names[best_idx]
      results$best_TSS[i] <- all_tss[best_idx]
      results$best_ROC[i] <- all_roc[best_idx]
      results$best_tier[i] <- "FALLBACK"
    } else {
      results$best_tier[i] <- "NONE"
    }
  }
  
  if (i %% 5 == 0 || i == length(gap_sp)) {
    cat(sprintf("  [%d/%d] %s: class=%s built=%d prim=%d fallb=%d tier=%s\n", 
        i, length(gap_sp), sp, results$classification[i],
        results$n_models_built[i], results$n_primary_qualified[i],
        results$n_fallback_qualified[i], results$best_tier[i]))
  }
}

cat("\n=== Classification Summary ===\n")
print(table(results$classification))

# ── Write outputs ──
write.csv(results, file.path(AUDIT, "gap75_full_classification_and_selection.csv"), row.names=FALSE)

# Workstream splits
direct8  <- results[results$classification == "DIRECT_34STACK",]
viirs57  <- results[results$classification == "VIIRS_ONLY",]
hycom10  <- results[results$classification %in% c("MIXED_HYCOM_VIIRS","HYCOM_ONLY"),]

cat(sprintf("\nDIRECT_34STACK: %d\n", nrow(direct8)))
cat(sprintf("VIIRS_ONLY: %d\n", nrow(viirs57)))
cat(sprintf("HYCOM: %d\n", nrow(hycom10)))

write.csv(direct8,  file.path(AUDIT, "gap75_workstream_A_direct8.csv"),  row.names=FALSE)
write.csv(viirs57,  file.path(AUDIT, "gap75_workstream_B_viirs57.csv"),  row.names=FALSE)
write.csv(hycom10,  file.path(AUDIT, "gap75_workstream_C_hycom10.csv"),  row.names=FALSE)

# ── Model selection summary ──
cat("\n=== Model Selection Summary ===\n")
for (cl in unique(results$classification)) {
  sub <- results[results$classification == cl,]
  prim <- sum(sub$n_primary_qualified > 0)
  fall <- sum(sub$n_fallback_qualified > 0 & sub$n_primary_qualified == 0)
  none <- sum(sub$n_fallback_qualified == 0)
  cat(sprintf("  %s: primary=%d fallback=%d none=%d (total=%d)\n", cl, prim, fall, none, nrow(sub)))
}

# ── Action determination ──
# VIIRS species: can project on 35-stack if they have qualified models
viirs_ready <- viirs57[viirs57$n_fallback_qualified > 0,]
viirs_need_repair <- viirs57[viirs57$n_fallback_qualified == 0,]
cat(sprintf("\nVIIRS_ONLY: %d ready to project, %d need repair\n", nrow(viirs_ready), nrow(viirs_need_repair)))

# DIRECT species: project on 34-stack if qualified, else repair
direct_ready <- direct8[direct8$n_fallback_qualified > 0,]
direct_need_repair <- direct8[direct8$n_fallback_qualified == 0,]
cat(sprintf("DIRECT_34STACK: %d ready to project, %d need repair\n", nrow(direct_ready), nrow(direct_need_repair)))

# HYCOM species: always need repair (stack missing HYCOM predictors)
hycom_ready <- hycom10[hycom10$n_fallback_qualified > 0,]
hycom_need_repair <- hycom10[hycom10$n_fallback_qualified == 0,]
cat(sprintf("HYCOM: %d ready (but need repair stack), %d need repair + model fix\n", nrow(hycom_ready), nrow(hycom_need_repair)))

# Total ready to project vs need repair
total_ready <- nrow(viirs_ready) + nrow(direct_ready)
total_repair <- nrow(viirs_need_repair) + nrow(direct_need_repair) + nrow(hycom10)
cat(sprintf("\nTOTAL: %d ready to project, %d need model repair\n", total_ready, total_repair))
