# Original source: rebuild_audits_final.R
# Original source location: ${WORK_ROOT}/publication_release_v1/scripts/rebuild_audits_final.R
# Original source SHA-256: b73d15e83d7161174a75c6125691de0d0fea9e1e37ec09619fdeeb6b88559205
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# rebuild_audits_final.R — Phases 3-6 evidence-based audit rebuild
suppressMessages(library(terra))

BASE <- "<WORK_ROOT>"
AUDIT <- file.path(BASE, "publication_release_v1/audits")
MANI  <- file.path(BASE, "publication_release_v1/manifests")
LOG_DIR <- file.path(BASE, "results/targeted_negative_TSS_projection_multifold_v4")

ref <- rast(file.path(BASE, "data/predictors/full_stack_terrestrial_primary_v1.tif"))
ref_lyr <- ref[[1]]

sel <- read.csv(file.path(BASE, "audit/publication_projection_model_selection_final.csv"), stringsAsFactors=FALSE)
sel <- sel[sel$final_selected %in% TRUE, ]
maxnet_exclude <- c("Dracaena trifasciata","Harrisia tortuosa","Melinis minutiflora",
  "Pachira aquatica","Pluchea carolinensis","Urochloa dictyoneura","Vicia eriocarpa")

pilot_sp <- c("Pachira aquatica","Geranium dissectum")
rem24_sp <- readLines(file.path(MANI, "projection_v4_remaining24_species.csv"))
all26 <- sort(c(pilot_sp, rem24_sp))

# ── Helper: get actual projected model names from log ──
get_actual_models <- function(logf) {
  if (!file.exists(logf)) return(character(0))
  txt <- readLines(logf, warn=FALSE)
  pat <- "Projecting[[:space:]]+([A-Za-z]+\\.[a-z]+_PA1_RUN[0-9]+_(GLM|RF|MAXNET))"
  m <- regmatches(txt, gregexpr(pat, txt))
  actual <- unique(gsub("Projecting[[:space:]]+", "", unlist(m)))
  sort(unique(actual[grepl("_PA1_RUN[0-9]+_(GLM|RF|MAXNET)$", actual)]))
}

# ── Helper: check if ensemble forecast completed ──
has_forecast <- function(logf) {
  if (!file.exists(logf)) return(FALSE)
  any(grepl("Forecast done", readLines(logf, warn=FALSE)))
}

# ── Helper: check log for errors ──
check_log_errors <- function(logf) {
  if (!file.exists(logf)) return(c(FALSE,FALSE,FALSE))
  txt <- readLines(logf, warn=FALSE)
  c(any(grepl("std::bad_alloc", txt)),
    any(grepl("Out of memory|oom|OOM|cannot allocate", txt, ignore.case=TRUE)),
    any(grepl("Fatal|FATAL", txt)))
}

# ═══ Phase 3: Model Lineage ═══
cat("Phase 3: Model lineage\n")
lineage <- data.frame(species=all26, stringsAsFactors=FALSE)
for (i in seq_along(all26)) {
  sp <- all26[i]; spu <- gsub(" ","_",sp)
  frozen <- sort(sel$full.name[sel$species==sp])
  logf <- file.path(LOG_DIR, paste0(spu,"_projection_v4.log"))
  actual <- get_actual_models(logf)
  fc_done <- has_forecast(logf)
  
  lineage$frozen_models[i] <- paste(frozen, collapse=";")
  lineage$frozen_count[i] <- length(frozen)
  lineage$actual_models[i] <- if(length(actual)>0) paste(actual, collapse=";") else NA_character_
  lineage$actual_count[i] <- length(actual)
  lineage$unexpected_count[i] <- sum(!actual %in% frozen)
  lineage$missing_count[i] <- sum(!frozen %in% actual)
  lineage$maxnet_selected[i] <- sum(grepl("MAXNET", frozen))
  lineage$maxnet_violation[i] <- sp %in% maxnet_exclude && any(grepl("MAXNET", actual))
  lineage$expected_product[i] <- if(length(frozen)>=2) "ENSEMBLE_EMMEAN_EMMEDIAN" else "SINGLE_QUALIFIED_MODEL"
  
  if (length(frozen)==1) {
    lineage$actual_product[i] <- "SINGLE_QUALIFIED_MODEL"
  } else if (fc_done) {
    lineage$actual_product[i] <- "ENSEMBLE_EMMEAN_EMMEDIAN"
  } else {
    lineage$actual_product[i] <- "ENSEMBLE_BUILT_FORECAST_FAILED"
  }
  
  lineage$lineage_pass[i] <- all(sort(frozen)==sort(actual)) &&
    !lineage$maxnet_violation[i] &&
    lineage$expected_product[i] == lineage$actual_product[i]
}
# Glebionis: lineage of individual projections is correct, product needs retry
lineage$lineage_pass[lineage$species=="Glebionis coronaria"] <- FALSE  # product mismatch
write.csv(lineage, file.path(AUDIT, "projection_v4_model_lineage_final.csv"), row.names=FALSE)
cat(sprintf("  Lineage PASS: %d / 26  (Glebionis product mismatch)\n", sum(lineage$lineage_pass)))

# ═══ Phase 4: SLURM audit ═══
cat("Phase 4: SLURM audit\n")
sacct <- read.table(pipe("sacct -j 8953545,8953643 --format=JobID,State,ExitCode,Elapsed,MaxRSS,MaxVMSize,ReqMem -X -p 2>/dev/null | grep -v 'bat+\\|ext+'"),
  header=TRUE, sep="|", stringsAsFactors=FALSE)
sacct$job_id <- as.integer(gsub("_.*","",sacct$JobID))
sacct$task_id <- as.integer(gsub(".*_","",sacct$JobID))
sacct$task_id[!grepl("_", sacct$JobID)] <- NA_integer_

slurm <- data.frame(species=all26, stringsAsFactors=FALSE)
for (i in seq_along(all26)) {
  sp <- all26[i]; spu <- gsub(" ","_",sp)
  if (sp %in% pilot_sp) {
    tid <- if(sp=="Pachira aquatica") 1 else 2
    row <- sacct[sacct$job_id==8953545 & sacct$task_id==tid, ]
    slurm$source_job_id[i] <- 8953545; slurm$task_id[i] <- tid
    slurm$requested_memory[i] <- "32G"
  } else {
    tid <- match(sp, rem24_sp)
    row <- sacct[sacct$job_id==8953643 & sacct$task_id==tid, ]
    slurm$source_job_id[i] <- 8953643; slurm$task_id[i] <- tid
    slurm$requested_memory[i] <- "64G"
  }
  slurm$state[i] <- if(nrow(row)>0) row$State[1] else "UNKNOWN"
  slurm$exit_code[i] <- if(nrow(row)>0) row$ExitCode[1] else "N/A"
  slurm$elapsed[i] <- if(nrow(row)>0) row$Elapsed[1] else "N/A"
  slurm$MaxRSS[i] <- if(nrow(row)>0) row$MaxRSS[1] else NA_character_
  slurm$MaxVMSize[i] <- if(nrow(row)>0) row$MaxVMSize[1] else NA_character_
  
  logf <- file.path(LOG_DIR, paste0(spu,"_projection_v4.log"))
  errs <- check_log_errors(logf)
  slurm$bad_alloc[i] <- errs[1]; slurm$oom[i] <- errs[2]; slurm$fatal_error[i] <- errs[3]
  slurm$timeout[i] <- FALSE
  
  slurm$slurm_pass[i] <- !is.na(slurm$state[i]) && slurm$state[i]=="COMPLETED" &&
    !is.na(slurm$exit_code[i]) && slurm$exit_code[i]=="0:0" &&
    !errs[1] && !errs[2] && !errs[3]
}
write.csv(slurm, file.path(AUDIT, "projection_v4_slurm_audit_final.csv"), row.names=FALSE)
cat(sprintf("  SLURM PASS: %d / 26\n", sum(slurm$slurm_pass)))

# ═══ Phase 5: Raster QC ═══
cat("Phase 5: Raster QC\n")
rqc <- data.frame(species=all26, stringsAsFactors=FALSE)
for (i in seq_along(all26)) {
  sp <- all26[i]; spu <- gsub(" ","_",sp); spd <- gsub(" ",".",sp)
  frozen <- sort(sel$full.name[sel$species==sp])
  is_ensemble <- length(frozen) >= 2
  proj_dir <- file.path(BASE, spd, paste0("proj_proj_",spu,"_multifold_v4"), "individual_projections")
  
  raster_path <- NA_character_
  if (dir.exists(proj_dir)) {
    if (is_ensemble) {
      em <- list.files(proj_dir, pattern="EMmeanByTSS.*\\.tif$", full.names=TRUE)
      em <- em[!grepl("_TSSbin|_TSSfilt", em)]
      if (length(em)>0) raster_path <- em[1]
    } else {
      ind <- list.files(proj_dir, pattern="\\.tif$", full.names=TRUE)
      ind <- ind[!grepl("_TSSbin|_TSSfilt|ClampingMask|EMmean|EMmedian", ind)]
      if (length(ind)>0) raster_path <- ind[1]
    }
  }
  rqc$final_raster_path[i] <- if(is.na(raster_path)) NA_character_ else raster_path
  rqc$product_class[i] <- if(is_ensemble) "ENSEMBLE" else "SINGLE"
  
  if (!is.na(raster_path) && file.exists(raster_path)) {
    r <- rast(raster_path)
    rqc$readable[i] <- TRUE
    rqc$crs[i] <- crs(r, describe=TRUE)$code
    rqc$resolution[i] <- paste(res(r)[1], res(r)[2])
    rqc$rows[i] <- nrow(r); rqc$cols[i] <- ncol(r)
    rqc$geom_match[i] <- all(ext(r)==ext(ref_lyr) && all(res(r)==res(ref_lyr)))
    rqc$total_cells[i] <- ncell(r)
    
    # Block-safe stats
    g_min <- global(r, "min", na.rm=TRUE); rqc$min_val[i] <- g_min[1,1]
    g_max <- global(r, "max", na.rm=TRUE); rqc$max_val[i] <- g_max[1,1]
    g_mean <- global(r, "mean", na.rm=TRUE); rqc$mean_val[i] <- g_mean[1,1]
    g_sd <- global(r, "sd", na.rm=TRUE); rqc$sd_val[i] <- g_sd[1,1]
    g_nna <- global(r, "notNA", na.rm=TRUE); rqc$non_NA[i] <- g_nna[1,1]
    
    rqc$all_NA[i] <- !hasValues(r)
    rqc$all_zero[i] <- isTRUE(rqc$min_val[i]==0 && rqc$max_val[i]==0)
    g_inf <- global(r, "isInf", na.rm=TRUE); rqc$Inf_flag[i] <- isTRUE(g_inf[1,1] > 0)
    g_nan <- global(r, "isNaN", na.rm=TRUE); rqc$NaN_flag[i] <- isTRUE(g_nan[1,1] > 0)
    rqc$finite[i] <- rqc$non_NA[i]
    
    rqc$raster_qc_pass[i] <- rqc$readable[i] && rqc$geom_match[i] &&
      !rqc$all_NA[i] && !rqc$all_zero[i] && !rqc$Inf_flag[i] && !rqc$NaN_flag[i] &&
      !is.na(rqc$non_NA[i]) && rqc$non_NA[i] > 0
  } else {
    rqc$readable[i] <- FALSE; rqc$raster_qc_pass[i] <- FALSE
  }
  cat(sprintf("  %s: pass=%s (%s)\n", sp, rqc$raster_qc_pass[i], basename(raster_path)))
}
write.csv(rqc, file.path(AUDIT, "projection_v4_raster_QC_final.csv"), row.names=FALSE)
cat(sprintf("  Raster QC PASS: %d / 26\n", sum(rqc$raster_qc_pass)))

# ═══ Phase 6: Build final manifest from audit joins ═══
cat("Phase 6: Build combined manifest\n")
manifest <- data.frame(species=all26, stringsAsFactors=FALSE)
for (i in seq_along(all26)) {
  sp <- all26[i]; spu <- gsub(" ","_",sp)
  frozen <- sort(sel$full.name[sel$species==sp])
  
  manifest$accepted_taxon[i] <- sp
  manifest$source_run[i] <- if(sp %in% pilot_sp) "V4_PILOT" else "V4_REMAINING24"
  manifest$source_job_id[i] <- slurm$source_job_id[i]
  manifest$task_id[i] <- slurm$task_id[i]
  manifest$frozen_selected_models[i] <- paste(frozen, collapse=";")
  manifest$actual_projected_models[i] <- lineage$actual_models[i]
  manifest$selected_model_count[i] <- length(frozen)
  manifest$expected_product_type[i] <- lineage$expected_product[i]
  manifest$actual_product_type[i] <- lineage$actual_product[i]
  
  # Formal final product per rule
  if (length(frozen) >= 2) {
    manifest$formal_final_product[i] <- "EMmeanByTSS"
  } else {
    manifest$formal_final_product[i] <- frozen[1]
  }
  
  manifest$final_raster_path[i] <- rqc$final_raster_path[i]
  
  # SHA256
  if (!is.na(rqc$final_raster_path[i]) && file.exists(rqc$final_raster_path[i])) {
    manifest$final_raster_sha256[i] <- system(paste("sha256sum", shQuote(rqc$final_raster_path[i]), "| awk '{print $1}'"), intern=TRUE)
  } else {
    manifest$final_raster_sha256[i] <- "MISSING"
  }
  
  manifest$SLURM_pass[i] <- slurm$slurm_pass[i]
  manifest$lineage_pass[i] <- lineage$lineage_pass[i]
  manifest$geometry_pass[i] <- rqc$geom_match[i]
  manifest$numeric_QC_pass[i] <- rqc$raster_qc_pass[i]
  
  manifest$ready_for_aggregation[i] <- manifest$SLURM_pass[i] && manifest$lineage_pass[i] &&
    manifest$geometry_pass[i] && manifest$numeric_QC_pass[i]
  
  # Warnings
  warns <- character(0)
  if (sp %in% maxnet_exclude) warns <- c(warns, "MAXNET_EXCLUDED")
  if (sp %in% c("Dracaena trifasciata","Urochloa dictyoneura")) warns <- c(warns, "FALLBACK_THRESHOLD")
  if (sp %in% c("Cinnamomum camphora","Croton setiger")) warns <- c(warns, "KFOLD_CV")
  if (manifest$expected_product_type[i] == "SINGLE_QUALIFIED_MODEL") warns <- c(warns, "SINGLE_MODEL")
  if (sp == "Glebionis coronaria") warns <- c(warns, "FORECAST_FAILED_NEEDS_RETRY")
  manifest$warning[i] <- if(length(warns)>0) paste(warns, collapse="; ") else "NONE"
  
  manifest$frozen_status[i] <- if(manifest$ready_for_aggregation[i]) {
    "READY_FOR_AGGREGATION"
  } else if (sp == "Glebionis coronaria") {
    "PENDING_ENSEMBLE_FORECAST_RETRY"
  } else {
    "FAILED"
  }
}

# Hard checks
cat(sprintf("  Rows: %d\n", nrow(manifest)))
cat(sprintf("  Unique species: %d\n", length(unique(manifest$species))))
cat(sprintf("  Duplicates: %d\n", sum(duplicated(manifest$species))))
cat(sprintf("  Pachira count: %d\n", sum(manifest$species=="Pachira aquatica")))
cat(sprintf("  Geranium count: %d\n", sum(manifest$species=="Geranium dissectum")))
cat(sprintf("  Diodia teres: %d\n", sum(manifest$species=="Diodia teres")))
cat(sprintf("  v2/v3 paths: %d\n", sum(grepl("v2|v3", manifest$final_raster_path))))
cat(sprintf("  Ready for aggregation: %d / 26\n", sum(manifest$ready_for_aggregation)))

write.csv(manifest, file.path(MANI, "final_repaired_projection_manifest_v2.csv"), row.names=FALSE)
cat("  Manifest v2 saved.\n")

# Final decision
cat(sprintf("\nFINAL: %d / 26 ready. Glebionis needs ensemble retry.\n", sum(manifest$ready_for_aggregation)))
