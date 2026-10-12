# Original source: phases_1_6_gap.R
# Original source location: ${WORK_ROOT}/publication_release_v1/scripts/phases_1_6_gap.R
# Original source SHA-256: 2eb8062db70c4c2028996319e06df4d9e3f4d56413914683d3ef51621c426c4d
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# Phases 1-6: Census + gap + recovery + model audit + predictor + classify
suppressMessages({ library(biomod2); library(terra) })
BASE <- "<WORK_ROOT>"
AUDIT <- file.path(BASE, "publication_release_v1/audits")
MANI  <- file.path(BASE, "publication_release_v1/manifests")
STACK <- file.path(BASE, "data/predictors/full_stack_terrestrial_primary_v1.tif")
stack_names <- names(rast(STACK))
dir.create(AUDIT, recursive=TRUE, showWarnings=FALSE)
dir.create(MANI, recursive=TRUE, showWarnings=FALSE)

# ── Load sources ──
master <- read.csv("<HPC_INPUT_ROOT>/script/output/species_list.csv", stringsAsFactors=FALSE)$species
old <- read.csv(file.path(BASE, "results/evaluations/projection_summary.csv"), stringsAsFactors=FALSE)
rep_mani <- read.csv(file.path(MANI, "final_repaired_projection_manifest_v2.csv"), stringsAsFactors=FALSE)
rep_species <- rep_mani$species

# ── Improved raster finder ──
find_raster <- function(sp, spd, ptype) {
  rds_path <- file.path(BASE, "results/projections", paste0(sp, "_", ptype, "_projection.rds"))
  if (!file.exists(rds_path)) return(list(found=FALSE, path=NA, reason="RDS_NOT_FOUND", rds_path=rds_path))
  rds <- tryCatch(readRDS(rds_path), error=function(e) NULL)
  if (is.null(rds) || !inherits(rds, "BIOMOD.projection.out")) return(list(found=FALSE, path=NA, reason="RDS_NOT_READABLE", rds_path=rds_path))
  mname <- rds@models.projected[1]
  mdir <- file.path(BASE, "models", spd)
  # Strategy 1: exact model name
  tifs <- list.files(mdir, pattern=paste0(mname, ".*\\.(tif|grd)$"), full.names=TRUE, recursive=TRUE)
  if (length(tifs)>0 && file.exists(tifs[1])) return(list(found=TRUE, path=tifs[1], reason="MODEL_NAME", rds_path=rds_path))
  # Strategy 2: _ensemble.grd for ensemble type
  if (ptype=="ensemble") {
    ens <- list.files(mdir, pattern="_ensemble\\.(grd|tif)$", recursive=TRUE, full.names=TRUE)
    ens <- ens[!grepl("_TSSbin|_TSSfilt", ens)]
    if (length(ens)>0 && file.exists(ens[1])) return(list(found=TRUE, path=ens[1], reason="ENSEMBLE_GRD", rds_path=rds_path))
  }
  # Strategy 3: fallback individual_projection
  ind <- list.files(mdir, pattern="\\.(tif|grd)$", recursive=TRUE, full.names=TRUE)
  ind <- ind[!grepl("_TSSbin|_TSSfilt|ClampingMask", ind)]
  if (length(ind)>0 && file.exists(ind[1])) return(list(found=TRUE, path=ind[1], reason="FALLBACK", rds_path=rds_path))
  return(list(found=FALSE, path=NA, reason="NO_RASTER", rds_path=rds_path))
}

# ── Phase 1: Full 813 census ──
cat("Phase 1: Building 813 census...\n")
census <- data.frame(
  original_species = master,
  accepted_taxon = master,
  synonym_excluded = master=="Diodia teres",
  modelled_originally = TRUE,
  in_old_projection_summary = master %in% old$species,
  old_projection_type = NA_character_,
  old_projection_rds_path = NA_character_,
  old_projection_raster_path = NA_character_,
  old_projection_valid = FALSE,
  repaired_v4 = master %in% rep_species,
  repaired_projection_path = NA_character_,
  repaired_projection_valid = FALSE,
  current_final_projection_path = NA_character_,
  current_projection_source = NA_character_,
  currently_has_valid_projection = FALSE,
  currently_missing_projection = FALSE,
  missing_reason = NA_character_,
  action_required = NA_character_,
  stringsAsFactors = FALSE
)

# Fill old projection details
cat("  Checking old projections...\n")
for (i in seq_len(nrow(census))) {
  sp <- census$original_species[i]
  if (!census$in_old_projection_summary[i]) next
  spd <- gsub(" ", ".", sp)
  orow <- old[old$species==sp, ]
  census$old_projection_type[i] <- orow$type[1]
  res <- find_raster(sp, spd, orow$type[1])
  census$old_projection_rds_path[i] <- res$rds_path
  census$old_projection_raster_path[i] <- res$path
  census$old_projection_valid[i] <- res$found
}

# Fill repaired details
for (i in seq_len(nrow(census))) {
  sp <- census$original_species[i]
  if (!census$repaired_v4[i]) next
  rr <- rep_mani[rep_mani$species==sp, ]
  census$repaired_projection_path[i] <- rr$final_raster_path
  census$repaired_projection_valid[i] <- isTRUE(rr$ready_for_aggregation)
}

# Determine current state
for (i in seq_len(nrow(census))) {
  sp <- census$original_species[i]
  if (census$synonym_excluded[i]) {
    census$currently_missing_projection[i] <- FALSE
    census$missing_reason[i] <- "SYNONYM_EXCLUDED"
    next
  }
  # Prefer v4 repaired
  if (census$repaired_v4[i] && census$repaired_projection_valid[i]) {
    census$current_final_projection_path[i] <- census$repaired_projection_path[i]
    census$current_projection_source[i] <- "REPAIRED_V4"
    census$currently_has_valid_projection[i] <- TRUE
    next
  }
  # Then old valid
  if (census$old_projection_valid[i]) {
    census$current_final_projection_path[i] <- census$old_projection_raster_path[i]
    census$current_projection_source[i] <- "ORIGINAL_FROZEN"
    census$currently_has_valid_projection[i] <- TRUE
    next
  }
  # Missing
  census$currently_missing_projection[i] <- TRUE
  if (census$in_old_projection_summary[i]) {
    census$missing_reason[i] <- "OLD_SUMMARY_BUT_NO_VALID_RASTER"
  } else {
    census$missing_reason[i] <- "NEVER_PROJECTED"
  }
}
write.csv(census, file.path(AUDIT, "master_813_projection_census_final.csv"), row.names=FALSE)

n_valid <- sum(census$currently_has_valid_projection)
n_missing <- sum(census$currently_missing_projection)
cat(sprintf("  Valid: %d, Missing: %d, Target: 812\n", n_valid, n_missing))

# ── Phase 2: Gap list ──
cat("Phase 2: Gap list\n")
gap <- census[census$currently_missing_projection, ]
gap_out <- data.frame(
  original_species = gap$original_species,
  accepted_taxon = gap$accepted_taxon,
  old_summary_present = gap$in_old_projection_summary,
  existing_model_directory = dir.exists(file.path(BASE, "models", gsub(" ", ".", gap$original_species))),
  existing_model_object = NA,
  existing_projection_candidates = NA_character_,
  current_missing_reason = gap$missing_reason,
  audit_status = "PENDING",
  stringsAsFactors = FALSE
)
write.csv(gap_out, file.path(MANI, "final_remaining_projection_gap_74.csv"), row.names=FALSE)
cat(sprintf("  Gap: %d species\n", nrow(gap_out)))

# ── Phase 3: Search for recoverable projections ──
cat("Phase 3: Recoverable projections search\n")
gap_out$recoverable_projection_path <- NA_character_
gap_out$recoverable_projection_type <- NA_character_
gap_out$recovered <- FALSE
for (i in seq_len(nrow(gap_out))) {
  sp <- gap_out$original_species[i]; spd <- gsub(" ", ".", sp)
  mdir <- file.path(BASE, "models", spd)
  if (!dir.exists(mdir)) next
  # Search for any continuous raster
  candidates <- list.files(mdir, pattern="\\.(tif|grd)$", recursive=TRUE, full.names=TRUE)
  candidates <- candidates[!grepl("_TSSbin|_TSSfilt|ClampingMask", candidates)]
  if (length(candidates)>0) {
    gap_out$recoverable_projection_path[i] <- candidates[1]
    gap_out$existing_projection_candidates[i] <- paste(basename(candidates), collapse=";")
    # Quick validation
    r <- tryCatch(rast(candidates[1]), error=function(e) NULL)
    if (!is.null(r) && nrow(r)==4320 && ncol(r)==8640) {
      mm <- minmax(r)
      if (hasValues(r) && !any(is.infinite(mm)) && !any(is.nan(mm))) {
        gap_out$recovered[i] <- TRUE
        gap_out$recoverable_projection_type[i] <- "HISTORICAL_CONTINUOUS"
      }
    }
  }
}
n_recovered <- sum(gap_out$recovered)
cat(sprintf("  Recoverable: %d\n", n_recovered))
write.csv(gap_out, file.path(AUDIT, "remaining74_existing_projection_recovery.csv"), row.names=FALSE)

# ── Phase 4: Model availability for unresolved ──
cat("Phase 4: Model availability\n")
unresolved <- gap_out[!gap_out$recovered, ]
mod_avail <- data.frame(
  species = unresolved$original_species,
  accepted_taxon = unresolved$accepted_taxon,
  model_directory = file.path(BASE, "models", gsub(" ", ".", unresolved$original_species)),
  model_directory_exists = unresolved$existing_model_directory,
  biomod_data_exists = NA,
  modelling_object_path = NA_character_,
  modelling_object_exists = FALSE,
  modelling_object_readable = FALSE,
  available_model_names = NA_character_,
  available_model_count = 0,
  available_algorithms = NA_character_,
  available_PA = NA_integer_,
  available_runs = NA_integer_,
  evaluation_available = FALSE,
  TSS_available = FALSE,
  ROC_available = FALSE,
  primary_qualified_model_names = NA_character_,
  primary_qualified_count = 0,
  fallback_qualified_model_names = NA_character_,
  fallback_qualified_count = 0,
  final_selected_model_names = NA_character_,
  final_selected_model_count = 0,
  projection_from_existing_model_possible = FALSE,
  failure_reason = NA_character_,
  stringsAsFactors = FALSE
)

for (i in seq_len(nrow(mod_avail))) {
  sp <- mod_avail$species[i]; spu <- gsub(" ", "_", sp); spd <- gsub(" ", ".", sp)
  mdir <- mod_avail$model_directory[i]
  if (!dir.exists(mdir)) { mod_avail$failure_reason[i] <- "NO_MODEL_DIR"; next }
  
  biomod_dir <- file.path(mdir, ".BIOMOD_DATA")
  mod_avail$biomod_data_exists[i] <- dir.exists(biomod_dir)
  
  # Find saved model RDS
  rds_files <- list.files(file.path(BASE, "results/targeted_negative_TSS_repair_multifold_v2"),
    pattern=paste0(spu, "_models.*\\.rds$"), full.names=TRUE)
  if (length(rds_files)==0) {
    # Try old model location
    rds_files <- list.files(mdir, pattern="\\.rds$", recursive=TRUE, full.names=TRUE)
    rds_files <- rds_files[grepl("models|BIOMOD", rds_files)]
  }
  
  if (length(rds_files)>0) {
    mod_avail$modelling_object_path[i] <- rds_files[1]
    mod_avail$modelling_object_exists[i] <- TRUE
    m <- tryCatch(readRDS(rds_files[1]), error=function(e) NULL)
    mod_avail$modelling_object_readable[i] <- !is.null(m)
    
    if (!is.null(m)) {
      built <- get_built_models(m)
      mod_avail$available_model_names[i] <- paste(built, collapse=";")
      mod_avail$available_model_count[i] <- length(built)
      algos <- unique(gsub(".*_(GLM|RF|MAXNET)$", "\\1", built))
      mod_avail$available_algorithms[i] <- paste(algos, collapse=";")
      pas <- unique(gsub(".*_PA([0-9]+)_.*", "PA\\1", built))
      mod_avail$available_PA[i] <- length(pas)
      runs <- unique(gsub(".*_RUN([0-9]+)_.*", "RUN\\1", built))
      mod_avail$available_runs[i] <- length(runs)
      
      # Check evaluation
      ev <- get_evaluations(m)
      if (!is.null(ev) && nrow(ev)>0) {
        mod_avail$evaluation_available[i] <- TRUE
        mod_avail$TSS_available[i] <- "TSS" %in% colnames(ev)
        mod_avail$ROC_available[i] <- "ROC" %in% colnames(ev)
        
        # Filter to validation runs only (not allData, not full)
        ev_val <- ev[!grepl("allData|_allRun|Full", ev$full.name), ]
        if (nrow(ev_val)==0) ev_val <- ev
        
        # Primary threshold
        primary <- ev_val[ev_val$TSS > 0.8 & ev_val$ROC > 0.8, ]
        if (nrow(primary)>0) {
          mod_avail$primary_qualified_model_names[i] <- paste(unique(primary$full.name), collapse=";")
          mod_avail$primary_qualified_count[i] <- length(unique(primary$full.name))
        }
        # Fallback
        fallback <- ev_val[ev_val$TSS > 0.7 & ev_val$ROC > 0.7, ]
        if (nrow(fallback)>0 && nrow(primary)==0) {
          mod_avail$fallback_qualified_model_names[i] <- paste(unique(fallback$full.name), collapse=";")
          mod_avail$fallback_qualified_count[i] <- length(unique(fallback$full.name))
        }
      }
      
      # Determine selection
      if (mod_avail$primary_qualified_count[i] > 0) {
        sel_names <- unique(primary$full.name)
      } else if (mod_avail$fallback_qualified_count[i] > 0) {
        sel_names <- unique(fallback$full.name)
      } else {
        sel_names <- character(0)
      }
      mod_avail$final_selected_model_names[i] <- paste(sel_names, collapse=";")
      mod_avail$final_selected_model_count[i] <- length(sel_names)
      mod_avail$projection_from_existing_model_possible[i] <- length(sel_names) > 0
      if (!mod_avail$projection_from_existing_model_possible[i]) {
        mod_avail$failure_reason[i] <- "NO_QUALIFIED_MODELS"
      }
    } else {
      mod_avail$failure_reason[i] <- "MODEL_NOT_READABLE"
    }
  } else {
    mod_avail$failure_reason[i] <- "NO_MODELLING_OBJECT"
  }
}
write.csv(mod_avail, file.path(AUDIT, "remaining74_model_availability_final.csv"), row.names=FALSE)
cat(sprintf("  Models available: %d / %d\n", sum(mod_avail$modelling_object_readable), nrow(mod_avail)))
cat(sprintf("  Projection possible: %d\n", sum(mod_avail$projection_from_existing_model_possible)))

# ── Phase 5: Predictor compatibility ──
cat("Phase 5: Predictor compatibility\n")
pred_comp <- data.frame(
  species = mod_avail$species,
  model_required_predictor_count = NA_integer_,
  model_required_predictors = NA_character_,
  stack34_predictor_count = length(stack_names),
  stack34_predictors = paste(stack_names, collapse=";"),
  missing_from_stack34 = NA_character_,
  extra_in_stack34 = NA_character_,
  all_required_available = FALSE,
  exact_required_name_match = FALSE,
  requires_clay = FALSE,
  requires_HYCOM = FALSE,
  requires_other_missing_predictor = FALSE,
  projection_compatibility_class = "UNASSESSED",
  stringsAsFactors = FALSE
)

for (i in seq_len(nrow(pred_comp))) {
  if (!mod_avail$modelling_object_readable[i]) {
    pred_comp$projection_compatibility_class[i] <- "MODEL_METADATA_UNRESOLVED"
    next
  }
  m <- readRDS(mod_avail$modelling_object_path[i])
  mv <- m@variables.importances
  if (length(mv)==0) {
    pred_comp$projection_compatibility_class[i] <- "MODEL_METADATA_UNRESOLVED"
    next
  }
  varnames <- unique(unlist(lapply(mv, rownames)))
  pred_comp$model_required_predictor_count[i] <- length(varnames)
  pred_comp$model_required_predictors[i] <- paste(varnames, collapse=";")
  missing_vars <- setdiff(varnames, stack_names)
  pred_comp$missing_from_stack34[i] <- if(length(missing_vars)>0) paste(missing_vars, collapse=";") else "NONE"
  pred_comp$all_required_available[i] <- length(missing_vars)==0
  pred_comp$exact_required_name_match[i] <- length(missing_vars)==0
  
  if (any(grepl("clay|Clay", missing_vars))) pred_comp$requires_clay[i] <- TRUE
  if (any(grepl("HYCOM|hycom|salinity|Salinity", missing_vars))) pred_comp$requires_HYCOM[i] <- TRUE
  if (length(missing_vars)>0 && !pred_comp$requires_clay[i] && !pred_comp$requires_HYCOM[i]) {
    pred_comp$requires_other_missing_predictor[i] <- TRUE
  }
  
  if (length(missing_vars)==0) {
    pred_comp$projection_compatibility_class[i] <- "DIRECT_34STACK_COMPATIBLE"
  } else if (pred_comp$requires_clay[i]) {
    pred_comp$projection_compatibility_class[i] <- "MODEL_EXPECTS_REMOVED_CLAY"
  } else if (pred_comp$requires_HYCOM[i]) {
    pred_comp$projection_compatibility_class[i] <- "MODEL_EXPECTS_HYCOM"
  } else {
    pred_comp$projection_compatibility_class[i] <- "MODEL_EXPECTS_OTHER_MISSING_VARIABLE"
  }
}
write.csv(pred_comp, file.path(AUDIT, "remaining74_predictor_compatibility_final.csv"), row.names=FALSE)
cat(sprintf("  Directly compatible: %d\n", sum(pred_comp$projection_compatibility_class=="DIRECT_34STACK_COMPATIBLE")))
cat(sprintf("  Requires clay: %d\n", sum(pred_comp$requires_clay)))
cat(sprintf("  Requires HYCOM: %d\n", sum(pred_comp$requires_HYCOM)))

# ── Phase 6: Action classification ──
cat("Phase 6: Action classification\n")
action <- data.frame(
  original_species = gap_out$original_species,
  accepted_taxon = gap_out$accepted_taxon,
  action = NA_character_,
  stringsAsFactors = FALSE
)

for (i in seq_len(nrow(action))) {
  sp <- action$original_species[i]
  is_recovered <- gap_out$recovered[i]
  in_mod <- sp %in% mod_avail$species
  mi <- match(sp, mod_avail$species)
  pi <- match(sp, pred_comp$species)
  
  if (is_recovered) {
    action$action[i] <- "RECOVER_EXISTING_PROJECTION"
  } else if (in_mod && !is.na(mi)) {
    has_model <- mod_avail$modelling_object_readable[mi]
    can_project <- mod_avail$projection_from_existing_model_possible[mi]
    is_compat <- if(!is.na(pi)) pred_comp$all_required_available[pi] else FALSE
    is_clay <- if(!is.na(pi)) pred_comp$requires_clay[pi] else FALSE
    is_hycom <- if(!is.na(pi)) pred_comp$requires_HYCOM[pi] else FALSE
    
    if (has_model && can_project && is_compat) {
      action$action[i] <- "DIRECT_PROJECTION_EXISTING_MODEL"
    } else if (has_model && !is_compat && (is_clay || is_hycom)) {
      action$action[i] <- "MODEL_REPAIR_REQUIRED_PREDICTOR_MISMATCH"
    } else if (has_model && !can_project) {
      action$action[i] <- "MODEL_REPAIR_REQUIRED_NO_QUALIFIED_MODELS"
    } else if (!has_model) {
      action$action[i] <- "MODEL_REPAIR_REQUIRED_MODEL_INVALID"
    } else {
      action$action[i] <- "MANUAL_REVIEW_REQUIRED"
    }
  } else {
    action$action[i] <- "MODEL_REPAIR_REQUIRED_MODEL_INVALID"
  }
  if (is.na(action$action[i])) action$action[i] <- "MANUAL_REVIEW_REQUIRED"
}

write.csv(action, file.path(MANI, "remaining74_final_action_plan.csv"), row.names=FALSE)

# Summary
cat("\n=== ACTION SUMMARY ===\n")
print(table(action$action))
cat(sprintf("\nTotal classified: %d\n", nrow(action)))
