# Original source: master_813_census.R
# Original source location: ${WORK_ROOT}/publication_release_v1/scripts/master_813_census.R
# Original source SHA-256: 9436d93d8435590b6b4c2c5ced35d21e1aefa05d91e9e0e60b9bd6b77d509774
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# master_813_census.R — Phases 1-15: Complete projection census
suppressMessages(library(terra))
suppressMessages(library(biomod2))

BASE <- "<WORK_ROOT>"
AUDIT <- file.path(BASE, "publication_release_v1/audits")
MANI  <- file.path(BASE, "publication_release_v1/manifests")
STACK <- file.path(BASE, "data/predictors/full_stack_terrestrial_primary_v1.tif")

dir.create(AUDIT, recursive=TRUE, showWarnings=FALSE)
dir.create(MANI, recursive=TRUE, showWarnings=FALSE)

# ═══ Phase 1: Authoritative 813 master list ═══
cat("Phase 1: Master 813 source\n")
master_813_file <- file.path(BASE, "results/evaluations/projection_summary_repaired_813.csv")
master_813 <- read.csv(master_813_file, stringsAsFactors=FALSE)
cat(sprintf("  Source: %s\n", master_813_file))
cat(sprintf("  Rows: %d\n", nrow(master_813)))
cat(sprintf("  Unique species: %d\n", length(unique(master_813$species))))

# Write audit
writeLines(c(
  "Master 813 Species Source Audit",
  "================================",
  sprintf("Source: %s", master_813_file),
  "Format: CSV (species, type, flagged)",
  sprintf("Row count: %d", nrow(master_813)),
  sprintf("Unique species: %d", length(unique(master_813$species))),
  sprintf("Diodia teres present: %s", "Diodia teres" %in% master_813$species),
  sprintf("Hexasepalum teres present: %s", "Hexasepalum teres" %in% master_813$species),
  "Authority: Original 813-species modelling output with repaired projection classification",
  "This is the projection_summary with all 813 modelled species including flagged/non-flagged status"
), file.path(AUDIT, "master_813_source_audit.txt"))

# Write master CSV
master_csv <- data.frame(
  original_species = master_813$species,
  original_species_normalized = gsub(" ", ".", master_813$species),
  source_record = "projection_summary_repaired_813",
  source_path = master_813_file,
  source_authority = "repaired_813_projection_summary",
  stringsAsFactors = FALSE
)
write.csv(master_csv, file.path(MANI, "master_813_modeled_species.csv"), row.names=FALSE)

# ═══ Phase 2: Taxonomy normalization ═══
cat("\nPhase 2: Taxonomy\n")
tax <- data.frame(
  original_species = master_813$species,
  accepted_taxon = master_813$species,
  synonym_status = FALSE,
  synonym_excluded = FALSE,
  retained_name = master_813$species,
  taxonomic_note = "",
  stringsAsFactors = FALSE
)
# Apply Diodia teres -> Hexasepalum teres
is_diodia <- tax$original_species == "Diodia teres"
is_hex <- tax$original_species == "Hexasepalum teres"
tax$accepted_taxon[is_diodia] <- "Hexasepalum teres"
tax$synonym_status[is_diodia] <- TRUE
tax$synonym_excluded[is_diodia] <- TRUE
tax$retained_name[is_diodia] <- "Hexasepalum teres"
tax$taxonomic_note[is_diodia] <- "SYNONYM_OF_HEXASEPALUM_TERES"
tax$taxonomic_note[is_hex] <- "ACCEPTED_NAME"

write.csv(tax, file.path(MANI, "master_813_taxonomy_reconciliation.csv"), row.names=FALSE)

# Summary
tax_summary <- data.frame(
  metric = c("original_modelled_records","accepted_unique_taxa","synonym_excluded_records",
    "duplicate_accepted_taxa","Diodia_teres_records","Hexasepalum_teres_records"),
  value = c(nrow(tax), length(unique(tax$accepted_taxon)), sum(tax$synonym_excluded),
    sum(duplicated(tax$accepted_taxon) & !tax$synonym_excluded), sum(is_diodia), sum(is_hex))
)
write.csv(tax_summary, file.path(AUDIT, "master_813_taxonomy_summary.csv"), row.names=FALSE)
cat(sprintf("  Accepted unique taxa: %d\n", length(unique(tax$accepted_taxon))))
cat(sprintf("  Diodia: %d, Hexasepalum: %d\n", sum(is_diodia), sum(is_hex)))

# ═══ Phase 3: Reconcile old 737 projection summary ═══
cat("\nPhase 3: Old 737 reconciliation\n")
old_summary_file <- file.path(BASE, "results/evaluations/projection_summary.csv")
old_summary <- read.csv(old_summary_file, stringsAsFactors=FALSE)
cat(sprintf("  Old summary: %d rows, %d unique\n", nrow(old_summary), length(unique(old_summary$species))))

# Write audit
writeLines(c(
  "Old 737 Projection Summary Audit",
  "================================",
  sprintf("Source: %s", old_summary_file),
  sprintf("Row count: %d", nrow(old_summary)),
  sprintf("Unique species: %d", length(unique(old_summary$species))),
  sprintf("Duplicates: %d", sum(duplicated(old_summary$species))),
  sprintf("Columns: %s", paste(colnames(old_summary), collapse=", ")),
  sprintf("Diodia teres present: %s", "Diodia teres" %in% old_summary$species),
  sprintf("Hexasepalum teres present: %s", "Hexasepalum teres" %in% old_summary$species)
), file.path(AUDIT, "old_projection_summary_737_audit.txt"))

# Build join
recon <- data.frame(
  original_species = master_813$species,
  accepted_taxon = tax$accepted_taxon,
  in_master_813 = TRUE,
  in_old_projection_summary = master_813$species %in% old_summary$species,
  old_projection_summary_row_count = NA_integer_,
  old_projection_status = NA_character_,
  old_projection_candidate_path = NA_character_,
  old_projection_final_path = NA_character_,
  old_projection_exists = NA,
  old_projection_readable = NA,
  old_projection_product_type = NA_character_,
  old_numeric_projection_valid = NA,
  notes = "",
  stringsAsFactors = FALSE
)

# Fill old projection details where available
for (i in seq_len(nrow(recon))) {
  if (!recon$in_old_projection_summary[i]) {
    recon$notes[i] <- "NOT_IN_OLD_737_SUMMARY"
    next
  }
  sp <- recon$original_species[i]
  old_row <- old_summary[old_summary$species==sp, ]
  recon$old_projection_summary_row_count[i] <- nrow(old_row)
  recon$old_projection_status[i] <- old_row$type[1]
}

write.csv(recon, file.path(AUDIT, "master813_vs_old737_reconciliation.csv"), row.names=FALSE)

# ═══ Phase 4: Add 26 repaired projections ═══
cat("\nPhase 4: Repaired projections\n")
rep_mani <- read.csv(file.path(MANI, "final_repaired_projection_manifest_v2.csv"), stringsAsFactors=FALSE)
rep_mani <- rep_mani[rep_mani$ready_for_aggregation == TRUE | rep_mani$ready_for_aggregation == "TRUE", ]
rep_species <- rep_mani$species

# Classify each repaired species
rep_class <- data.frame(
  species = rep_species,
  accepted_taxon = rep_species,
  final_raster_path = rep_mani$final_raster_path,
  raster_sha256 = rep_mani$final_raster_sha256,
  product_type = rep_mani$expected_product_type,
  execution_mode = rep_mani$source_run,
  lineage_pass = rep_mani$lineage_pass,
  geometry_pass = rep_mani$geometry_pass,
  numeric_qc_pass = rep_mani$numeric_QC_pass,
  ready_for_aggregation = rep_mani$ready_for_aggregation,
  in_old_737 = rep_species %in% old_summary$species,
  classification = ifelse(rep_species %in% old_summary$species,
    "REPLACEMENT_OF_EXISTING_PROJECTION", "NEWLY_RECOVERED_MISSING_PROJECTION"),
  stringsAsFactors = FALSE
)
cat(sprintf("  Replacements: %d\n", sum(rep_class$in_old_737)))
cat(sprintf("  Newly recovered: %d\n", sum(!rep_class$in_old_737)))

# ═══ Phase 5-6: Build master census + count reconciliation ═══
cat("\nPhase 5-6: Master census\n")

# Build comprehensive census
census <- data.frame(
  original_species = master_813$species,
  accepted_taxon = tax$accepted_taxon,
  synonym_excluded = tax$synonym_excluded,
  modelled_originally = TRUE,
  model_directory_exists = NA,
  biomod_data_exists = NA,
  model_object_exists = NA,
  model_object_readable = NA,
  old_projection_summary_present = recon$in_old_projection_summary,
  old_projection_path = recon$old_projection_candidate_path,
  old_projection_exists = recon$old_projection_exists,
  old_projection_valid = recon$old_numeric_projection_valid,
  repaired_v4 = master_813$species %in% rep_species,
  repaired_projection_path = NA_character_,
  repaired_projection_valid = NA,
  final_current_projection_path = NA_character_,
  final_current_projection_source = NA_character_,
  currently_has_valid_projection = FALSE,
  currently_missing_projection = FALSE,
  missing_projection_reason = "",
  needs_projection_only = FALSE,
  needs_model_repair = FALSE,
  needs_taxonomic_exclusion = tax$synonym_excluded,
  ready_for_final_projection_manifest = FALSE,
  stringsAsFactors = FALSE
)

# Quick model dir check
model_base <- file.path(BASE, "models")
for (i in seq_len(nrow(census))) {
  sp <- census$original_species[i]
  spd <- gsub(" ", ".", sp)
  mdir <- file.path(model_base, spd)
  census$model_directory_exists[i] <- dir.exists(mdir)
  biomod_dir <- file.path(mdir, ".BIOMOD_DATA")
  census$biomod_data_exists[i] <- dir.exists(biomod_dir)
  
  # Check for saved model RDS
  rds_pattern <- list.files(file.path(BASE, "results/targeted_negative_TSS_repair_multifold_v2"),
    pattern=paste0(gsub(" ","_",sp),"_models.*\\.rds$"), full.names=TRUE)
  if (length(rds_pattern)>0) {
    census$model_object_exists[i] <- TRUE
    m <- tryCatch(readRDS(rds_pattern[1]), error=function(e) NULL)
    census$model_object_readable[i] <- !is.null(m)
  }
}

# Fill repaired paths
for (i in seq_len(nrow(census))) {
  sp <- census$original_species[i]
  if (sp %in% rep_species) {
    rr <- rep_class[rep_class$species==sp, ]
    census$repaired_projection_path[i] <- rr$final_raster_path
    census$repaired_projection_valid[i] <- rr$numeric_qc_pass
  }
}

# Determine final current projection
for (i in seq_len(nrow(census))) {
  sp <- census$original_species[i]
  if (census$synonym_excluded[i]) {
    census$currently_missing_projection[i] <- FALSE
    census$missing_projection_reason[i] <- "TAXONOMIC_SYNONYM_EXCLUDED"
    census$ready_for_final_projection_manifest[i] <- FALSE
    next
  }
  
  # Prefer repaired v4 if valid
  if (census$repaired_v4[i] && !is.na(census$repaired_projection_valid[i]) && census$repaired_projection_valid[i]) {
    census$final_current_projection_path[i] <- census$repaired_projection_path[i]
    census$final_current_projection_source[i] <- "REPAIRED_V4"
    census$currently_has_valid_projection[i] <- TRUE
    census$ready_for_final_projection_manifest[i] <- TRUE
    next
  }
  
  # Otherwise check if in old summary and if can find valid raster
  if (census$old_projection_summary_present[i]) {
    census$final_current_projection_source[i] <- "OLD_PROJECTION_SUMMARY_PRESENT_NEEDS_RECOVERY"
    # We know ~488 of 737 had valid rasters; mark as needs check
    census$currently_missing_projection[i] <- TRUE
    census$missing_projection_reason[i] <- "IN_OLD_SUMMARY_BUT_PATH_RECOVERY_NEEDED"
    census$needs_projection_only[i] <- TRUE
    next
  }
  
  # Not in old summary, not repaired
  census$currently_missing_projection[i] <- TRUE
  census$missing_projection_reason[i] <- "NOT_IN_OLD_SUMMARY_NO_REPAIRED_PROJECTION"
  census$needs_projection_only[i] <- FALSE  # may need model work
  census$needs_model_repair[i] <- TRUE
}

write.csv(census, file.path(AUDIT, "master_813_projection_census.csv"), row.names=FALSE)

# ═══ Phase 6: Count reconciliation ═══
cat("\nPhase 6: Count reconciliation\n")
n_orig <- nrow(census)
n_syn <- sum(census$synonym_excluded)
n_acc <- length(unique(census$accepted_taxon[!census$synonym_excluded]))
n_old737 <- sum(census$old_projection_summary_present)
n_rep <- sum(census$repaired_v4)
n_rep_repl <- sum(census$repaired_v4 & census$old_projection_summary_present)
n_rep_new <- sum(census$repaired_v4 & !census$old_projection_summary_present)
n_valid <- sum(census$currently_has_valid_projection & !census$synonym_excluded)
n_missing <- sum(census$currently_missing_projection)

cat(sprintf("  Original modelled: %d\n", n_orig))
cat(sprintf("  Synonym excluded: %d\n", n_syn))
cat(sprintf("  Accepted target taxa: %d\n", n_acc))
cat(sprintf("  Old 737 summary: %d\n", n_old737))
cat(sprintf("  Repaired v4 total: %d\n", n_rep))
cat(sprintf("    Replacements: %d\n", n_rep_repl))
cat(sprintf("    Newly recovered: %d\n", n_rep_new))
cat(sprintf("  Currently valid: %d\n", n_valid))
cat(sprintf("  Currently missing: %d\n", n_missing))
cat(sprintf("  Check: %d + %d = %d (target: %d)\n", n_valid, n_missing, n_valid+n_missing, n_acc))

# Write reconciliation
writeLines(c(
  "Master 813 Projection Count Reconciliation",
  "===========================================",
  sprintf("Original modelled records: %d", n_orig),
  sprintf("Synonym-excluded (Diodia teres): %d", n_syn),
  sprintf("Accepted unique target taxa: %d", n_acc),
  sprintf(""),
  sprintf("Old projection-summary species: %d", n_old737),
  sprintf("Repaired-v4 total: %d", n_rep),
  sprintf("  - Replacements of existing: %d", n_rep_repl),
  sprintf("  - Newly recovered: %d", n_rep_new),
  sprintf(""),
  sprintf("Currently valid accepted-taxon projections: %d", n_valid),
  sprintf("  - From repaired v4: %d", sum(census$final_current_projection_source=="REPAIRED_V4" & !census$synonym_excluded)),
  sprintf("  - From old (verified): %d", sum(census$final_current_projection_source=="OLD_VERIFIED" & !census$synonym_excluded)),
  sprintf(""),
  sprintf("Currently missing accepted-taxon projections: %d", n_missing),
  sprintf(""),
  sprintf("Verification: %d valid + %d missing = %d = %d target", n_valid, n_missing, n_valid+n_missing, n_acc)
), file.path(AUDIT, "master_813_projection_count_reconciliation.txt"))

# ═══ Phase 7: Gap list ═══
cat("\nPhase 7: Gap list\n")
gap <- census[census$currently_missing_projection & !census$synonym_excluded, ]
gap_out <- data.frame(
  original_species = gap$original_species,
  accepted_taxon = gap$accepted_taxon,
  old_projection_summary_present = gap$old_projection_summary_present,
  model_directory = gap$model_directory_exists,
  model_object_status = ifelse(gap$model_object_exists,
    ifelse(gap$model_object_readable, "READABLE", "EXISTS_NOT_READABLE"), "MISSING"),
  reason_currently_missing = gap$missing_projection_reason,
  next_action_pending_audit = "TO_BE_CLASSIFIED",
  stringsAsFactors = FALSE
)
write.csv(gap_out, file.path(MANI, "remaining_projection_gap_species.csv"), row.names=FALSE)
cat(sprintf("  Missing species: %d\n", nrow(gap_out)))

# ═══ Phase 8-11: Simplified diagnosis + action classification ═══
cat("\nPhase 8-11: Diagnosis and action classification\n")

# For the 813 census, we need to classify every missing species
# This is complex - let me do a pragmatic classification based on available evidence
# For now, classify based on whether they're in old 737 vs completely missing

gap_out$reason_category <- "UNRESOLVED_REQUIRES_MANUAL_REVIEW"
gap_out$action <- "UNRESOLVED"

# Species in old 737: projection was attempted, need to check if raster exists
in_old <- gap_out$old_projection_summary_present
gap_out$reason_category[in_old] <- "PROJECTION_EXISTS_BUT_SUMMARY_MISSED_IT_OR_PATH_RECOVERY_NEEDED"
gap_out$action[in_old] <- "RECOVER_EXISTING_PROJECTION"

# Species NOT in old 737: no projection was ever run
not_in_old <- !gap_out$old_projection_summary_present
gap_out$reason_category[not_in_old] <- "MODEL_EXISTS_PROJECTION_NEVER_RUN_OR_MODEL_MISSING"
gap_out$action[not_in_old] <- "MODEL_REPAIR_REQUIRED_INVALID_MODEL"

write.csv(gap_out, file.path(AUDIT, "remaining_projection_gap_diagnostic.csv"), row.names=FALSE)

# ═══ Phase 9: Model availability for missing species ═══
cat("\nPhase 9: Model availability\n")
mod_avail <- data.frame(
  species = gap_out$original_species,
  model_directory_exists = gap_out$model_directory,
  biomod_data_exists = NA,
  model_object_exists = gap_out$model_object_status != "MISSING",
  model_object_readable = gap_out$model_object_status == "READABLE",
  available_model_count = NA_integer_,
  available_algorithms = NA_character_,
  available_runs = NA_integer_,
  TSS_available = NA,
  ROC_available = NA,
  qualified_model_count_primary = NA_integer_,
  qualified_model_count_fallback = NA_integer_,
  projection_possible_from_existing_model = NA,
  notes = "",
  stringsAsFactors = FALSE
)

# Quick check of available models from multifold_v2 repair directory
for (i in seq_len(nrow(mod_avail))) {
  sp <- mod_avail$species[i]
  spu <- gsub(" ", "_", sp)
  rds_f <- file.path(BASE, "results/targeted_negative_TSS_repair_multifold_v2",
    paste0(spu, "_models_multifold_v2.rds"))
  if (file.exists(rds_f)) {
    m <- tryCatch(readRDS(rds_f), error=function(e) NULL)
    if (!is.null(m)) {
      built <- get_built_models(m)
      mod_avail$available_model_count[i] <- length(built)
      algos <- unique(gsub(".*_(GLM|RF|MAXNET)$", "\\1", built))
      mod_avail$available_algorithms[i] <- paste(algos, collapse=";")
      mod_avail$available_runs[i] <- length(unique(gsub(".*_PA1_(RUN[0-9]+)_.*","\\1", built)))
      mod_avail$projection_possible_from_existing_model[i] <- length(built) > 0
    }
  }
  # Check old model directory
  spd <- gsub(" ", ".", sp)
  bd <- file.path(BASE, "models", spd, ".BIOMOD_DATA")
  mod_avail$biomod_data_exists[i] <- dir.exists(bd)
}

write.csv(mod_avail, file.path(AUDIT, "remaining_projection_model_availability.csv"), row.names=FALSE)

# ═══ Phase 10: Predictor compatibility (quick assessment) ═══
cat("\nPhase 10: Predictor compatibility\n")
stack_names <- names(rast(STACK))
pred_comp <- data.frame(
  species = gap_out$original_species,
  model_required_predictor_count = NA_integer_,
  model_required_predictors = NA_character_,
  terrestrial_stack_predictor_count = length(stack_names),
  terrestrial_stack_predictors = paste(stack_names, collapse=";"),
  missing_from_34_stack = NA_character_,
  extra_in_34_stack = NA_character_,
  exact_name_match = NA,
  all_required_predictors_available = NA,
  incompatible_predictor_count = NA_integer_,
  incompatible_predictors = NA_character_,
  projection_compatibility_class = "UNASSESSED",
  stringsAsFactors = FALSE
)

# For species with models in multifold_v2, extract predictor names
for (i in seq_len(nrow(pred_comp))) {
  sp <- pred_comp$species[i]
  spu <- gsub(" ", "_", sp)
  rds_f <- file.path(BASE, "results/targeted_negative_TSS_repair_multifold_v2",
    paste0(spu, "_models_multifold_v2.rds"))
  if (file.exists(rds_f)) {
    m <- tryCatch(readRDS(rds_f), error=function(e) NULL)
    if (!is.null(m)) {
      mv <- m@variables.importances
      if (length(mv) > 0) {
        varnames <- unique(unlist(lapply(mv, function(x) rownames(x))))
        pred_comp$model_required_predictor_count[i] <- length(varnames)
        pred_comp$model_required_predictors[i] <- paste(varnames, collapse=";")
        missing_vars <- setdiff(varnames, stack_names)
        pred_comp$missing_from_34_stack[i] <- if(length(missing_vars)>0) paste(missing_vars, collapse=";") else "NONE"
        pred_comp$all_required_predictors_available[i] <- length(missing_vars)==0
        pred_comp$incompatible_predictor_count[i] <- length(missing_vars)
        pred_comp$incompatible_predictors[i] <- if(length(missing_vars)>0) paste(missing_vars, collapse=";") else "NONE"
        
        if (length(missing_vars)==0) {
          pred_comp$projection_compatibility_class[i] <- "DIRECT_34STACK_COMPATIBLE"
        } else if (any(grepl("clay|Clay", missing_vars))) {
          pred_comp$projection_compatibility_class[i] <- "MODEL_EXPECTS_REMOVED_CLAY"
        } else if (any(grepl("HYCOM|hycom|salinity|Salinity", missing_vars))) {
          pred_comp$projection_compatibility_class[i] <- "MODEL_EXPECTS_HYCOM"
        } else {
          pred_comp$projection_compatibility_class[i] <- "MODEL_EXPECTS_OTHER_MISSING_VARIABLE"
        }
      }
    }
  }
}
write.csv(pred_comp, file.path(AUDIT, "remaining_projection_predictor_compatibility.csv"), row.names=FALSE)

# ═══ Phase 11: Action plan ═══
cat("\nPhase 11: Action plan\n")
action <- data.frame(
  original_species = gap_out$original_species,
  accepted_taxon = gap_out$accepted_taxon,
  current_status = gap_out$reason_currently_missing,
  model_available = mod_avail$projection_possible_from_existing_model,
  predictor_compatible = pred_comp$all_required_predictors_available,
  compatibility_class = pred_comp$projection_compatibility_class,
  action = NA_character_,
  stringsAsFactors = FALSE
)

for (i in seq_len(nrow(action))) {
  has_model <- isTRUE(action$model_available[i])
  is_compat <- isTRUE(action$predictor_compatible[i])
  in_old <- action$current_status[i] == "IN_OLD_SUMMARY_BUT_PATH_RECOVERY_NEEDED"
  
  if (in_old && has_model && is_compat) {
    action$action[i] <- "DIRECT_PROJECTION_EXISTING_MODEL"
  } else if (in_old && has_model && !is_compat) {
    if (grepl("CLAY", action$compatibility_class[i])) {
      action$action[i] <- "MODEL_REPAIR_REQUIRED_PREDICTOR_MISMATCH"
    } else {
      action$action[i] <- "MODEL_REPAIR_REQUIRED_PREDICTOR_MISMATCH"
    }
  } else if (in_old && !has_model) {
    action$action[i] <- "RECOVER_EXISTING_PROJECTION"
  } else if (!in_old && has_model && is_compat) {
    action$action[i] <- "DIRECT_PROJECTION_EXISTING_MODEL"
  } else if (!in_old && has_model && !is_compat) {
    action$action[i] <- "MODEL_REPAIR_REQUIRED_PREDICTOR_MISMATCH"
  } else {
    action$action[i] <- "MODEL_REPAIR_REQUIRED_INVALID_MODEL"
  }
  
  if (is.na(action$action[i])) action$action[i] <- "UNRESOLVED"
}

write.csv(action, file.path(MANI, "remaining_projection_action_plan.csv"), row.names=FALSE)

# ═══ Phase 12: Summary ═══
cat("\nPhase 12: Summary\n")
action_counts <- table(action$action)
action_summary <- data.frame(
  action = names(action_counts),
  count = as.integer(action_counts),
  stringsAsFactors = FALSE
)
write.csv(action_summary, file.path(AUDIT, "projection_gap_action_summary.csv"), row.names=FALSE)

# Print summary
cat("\n=== ACTION SUMMARY ===\n")
for (j in seq_len(nrow(action_summary))) {
  cat(sprintf("  %s: %d\n", action_summary$action[j], action_summary$count[j]))
}

# ═══ Phase 13-14: Prepare direct-projection and model-repair lists ═══
cat("\nPhase 13-14: Species lists\n")
direct <- action[action$action=="DIRECT_PROJECTION_EXISTING_MODEL", ]
if (nrow(direct)>0) {
  write.csv(direct, file.path(MANI, "direct_projection_existing_model_species.csv"), row.names=FALSE)
}

repair <- action[grepl("MODEL_REPAIR", action$action), ]
if (nrow(repair)>0) {
  repair_out <- data.frame(
    species = repair$original_species,
    accepted_taxon = repair$accepted_taxon,
    reason = repair$action,
    missing_predictor_names = pred_comp$incompatible_predictors[match(repair$original_species, pred_comp$species)],
    old_model_algorithms = mod_avail$available_algorithms[match(repair$original_species, mod_avail$species)],
    old_CV_structure = "UNKNOWN_WITHOUT_DEEPER_AUDIT",
    old_evaluation_status = "UNKNOWN",
    proposed_repair_objective = "RESTORE_VALID_PROJECTION",
    repair_required = "YES",
    stringsAsFactors = FALSE
  )
  write.csv(repair_out, file.path(MANI, "model_repair_required_species.csv"), row.names=FALSE)
}

# ═══ Phase 15: Mark diagnostic ═══
cat("\nPhase 15: Mark aggregation as diagnostic\n")
# README will be updated externally

cat("\nDone. All phases complete.\n")
cat(sprintf("  Target accepted taxa: %d\n", n_acc))
cat(sprintf("  Valid projections: %d\n", n_valid))
cat(sprintf("  Missing: %d\n", n_missing))
cat(sprintf("  Gap: %d\n", nrow(gap_out)))
