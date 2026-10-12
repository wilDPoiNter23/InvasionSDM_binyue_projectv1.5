# Original source: audit_813_predictor_contamination.R
# Original source location: ${WORK_ROOT}/publication_release_v1/scripts/audit_813_predictor_contamination.R
# Original source SHA-256: 1897ff6393f349f9c1ce87739678e52b1b8e85d6300123252a5e368b0ce31536
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
.libPaths(c(.libPaths(), "<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu"))
suppressPackageStartupMessages(library(biomod2))

# ============================================================================
# 813-TAXON PREDICTOR CONTAMINATION AUDIT
# ============================================================================

BASE <- "<WORK_ROOT>"
OUT <- file.path(BASE, "publication_release_v1/audits")

# Load scientific whitelist
wl <- readLines("/tmp/whitelist_all_sci.txt")
cat("Scientific whitelist:", length(wl), "predictors\n")

# Define technical blacklist
tech_blacklist <- c("ID", "id", "row", "rowid", "index", "cell", "cell_id", 
                     "cellid", "cell_index", "raster_cell", "X", "X.1",
                     "record_id", "point_id", "sample_id", "fid", "FID")
coord_blacklist <- c("lon", "lat", "longitude", "latitude", "x", "y", 
                      "coord_x", "coord_y", "Longitude", "Latitude")
response_blacklist <- c("species", "response", "presence", "absence", "PA", 
                         "fold", "RUN", "cv_fold")
all_blacklist <- c(tech_blacklist, coord_blacklist, response_blacklist)
cat("Blacklist entries:", length(all_blacklist), "\n")

# Load manifest
mani <- read.csv(file.path(BASE, "publication_release_v1/manifests/final_accepted_projection_manifest_813_v2.csv"), 
                 stringsAsFactors = FALSE)
cat("Manifest species:", nrow(mani), "\n")

# Load existing eval data to find model object paths
models_dir <- file.path(BASE, "models")

# Results containers
long_results <- data.frame()
summary_results <- data.frame()
errors <- data.frame()

# Process each taxon
for (i in 1:nrow(mani)) {
  taxon <- mani$accepted_taxon[i]
  gen <- mani$source_generation[i]
  mod_src <- mani$model_source[i]
  
  if (i %% 50 == 0) cat("Processing", i, "/ 813:", taxon, "\n")
  
  sp_dir_name <- gsub(" ", ".", taxon)
  sp_dir <- file.path(models_dir, sp_dir_name)
  
  # Find .models.out file (non-ensemble)
  all_mo <- list.files(sp_dir, pattern = "\\.models\\.out$", recursive = TRUE, full.names = TRUE)
  base_mo <- all_mo[!grepl("\\.ensemble\\.models\\.out$", all_mo)]
  
  pred_names <- NULL
  mo_file <- NA
  extraction_method <- NA
  
  if (length(base_mo) > 0) {
    mo_file <- base_mo[1]
    tryCatch({
      mo <- get(load(mo_file))
      pred_names <- mo@expl.var.names
      extraction_method <- "models.out@expl.var.names"
    }, error = function(e) {
      # Try reading eval directly
      eval_files <- list.files(file.path(sp_dir, ".BIOMOD_DATA"), 
                               pattern = "models\\.evaluation$", recursive = TRUE, full.names = TRUE)
      if (length(eval_files) > 0) {
        ev <- read.csv(eval_files[1], stringsAsFactors = FALSE)
        # Try to identify predictor columns
        known_non_pred <- c("Species", "Type", "full.name", "PA", "run", "algo", 
                            "metric.eval", "cutoff", "sensitivity", "specificity",
                            "calibration", "validation", "evaluation", 
                            "merged.by.PA", "merged.by.run", "merged.by.algo", "filtered.by")
        potential_preds <- setdiff(colnames(ev), known_non_pred)
        # Not reliable — skip
      }
    })
  }
  
  # For repair species, try RDS
  if (is.null(pred_names) && gen == "GAP13_REPAIR35_V1") {
    rds_file <- file.path(BASE, "results/gap13_model_repair_fullcoverage_v1", 
                          paste0(gsub(" ", "_", taxon), "_repair_models.rds"))
    if (file.exists(rds_file)) {
      tryCatch({
        mo <- readRDS(rds_file)
        if (inherits(mo, "BIOMOD.models.out")) {
          pred_names <- mo@expl.var.names
          mo_file <- rds_file
          extraction_method <- "repair_RDS@expl.var.names"
        }
      }, error = function(e) {})
    }
  }
  
  # For Spartina, use known location
  if (is.null(pred_names) && taxon == "Spartina alterniflora") {
    rds_file <- file.path(BASE, "results/gap13_model_repair_fullcoverage_v1", 
                          "Spartina_alterniflora_repair_models.rds")
    if (file.exists(rds_file)) {
      tryCatch({
        mo <- readRDS(rds_file)
        if (inherits(mo, "BIOMOD.models.out")) {
          pred_names <- mo@expl.var.names
          mo_file <- rds_file
          extraction_method <- "repair_RDS@expl.var.names"
        }
      }, error = function(e) {})
    }
  }
  
  # Classify
  if (is.null(pred_names)) {
    # Check if the species directory has any model artifacts
    summary_results <- rbind(summary_results, data.frame(
      accepted_taxon = taxon, source_generation = gen,
      authoritative_model_file = if(is.na(mo_file)) "NOT_FOUND" else mo_file,
      declared_predictor_count = NA, expected_predictor_count = NA,
      expected_predictor_set = NA, unexpected_predictors = "",
      missing_expected_predictors = "", technical_blacklist_hits = "",
      coordinate_blacklist_hits = "", ID_present = NA,
      cell_index_present = NA, lon_lat_present = NA,
      other_unexpected_present = NA, predictor_set_exact_match = NA,
      contamination_status = "MODEL_OBJECT_NOT_RECOVERABLE",
      action_required = "NEEDS_DIRECT_FILE_INSPECTION",
      stringsAsFactors = FALSE
    ))
    next
  }
  
  # Analyze predictor names
  pred_count <- length(pred_names)
  in_wl <- pred_names %in% wl
  unexpected <- pred_names[!in_wl]
  missing_wl <- wl[!(wl %in% pred_names)]
  
  tech_hits <- pred_names[pred_names %in% tech_blacklist | 
                           tolower(pred_names) %in% tolower(tech_blacklist)]
  coord_hits <- pred_names[pred_names %in% coord_blacklist | 
                            tolower(pred_names) %in% tolower(coord_blacklist)]
  
  id_present <- "ID" %in% pred_names
  cell_present <- any(grepl("cell|index", pred_names, ignore.case = TRUE))
  lonlat_present <- any(grepl("^lon$|^lat$|^longitude$|^latitude$|^x$|^y$", 
                              pred_names, ignore.case = TRUE))
  
  # Determine expected set
  expected_set <- if (gen == "GAP75_DIRECT34_V1") "primary34"
                   else if (gen %in% c("GAP75_VIIRS35_V1", "GAP13_REPAIR35_V1")) "viirs35"
                   else if (gen == "SPARTINA_FINAL_V1") "viirs35"
                   else if (gen == "ORIGINAL_FROZEN") "historical40_scientific_subset"
                   else "UNKNOWN"
  
  expected_count <- if (gen == "GAP75_DIRECT34_V1") 34
                    else if (gen %in% c("GAP75_VIIRS35_V1", "GAP13_REPAIR35_V1")) 35
                    else if (gen == "SPARTINA_FINAL_V1") 35
                    else NA
  
  exact_match <- if (!is.na(expected_count)) pred_count == expected_count && length(unexpected) == 0
                 else NA
  
  # Classify contamination
  if (length(unexpected) == 0 && length(tech_hits) == 0 && length(coord_hits) == 0) {
    if (exact_match %in% TRUE) {
      contam_status <- "PASS_EXACT_EXPECTED_SET"
    } else if (gen == "ORIGINAL_FROZEN") {
      contam_status <- "PASS_HISTORICAL_SCIENTIFIC_SET"
    } else {
      contam_status <- "PASS_EXACT_EXPECTED_SET"
    }
  } else if (length(tech_hits) > 0 || id_present || cell_present) {
    contam_status <- "CONTAMINATION_CONFIRMED"
  } else if (length(coord_hits) > 0) {
    contam_status <- "CONTAMINATION_CONFIRMED"
  } else if (length(unexpected) > 0) {
    contam_status <- "SUSPICIOUS_REQUIRES_ALGORITHM_AUDIT"
  } else {
    contam_status <- "SUSPICIOUS_REQUIRES_ALGORITHM_AUDIT"
  }
  
  # Summary row
  summary_results <- rbind(summary_results, data.frame(
    accepted_taxon = taxon, source_generation = gen,
    authoritative_model_file = mo_file,
    declared_predictor_count = pred_count,
    expected_predictor_count = expected_count,
    expected_predictor_set = expected_set,
    unexpected_predictors = paste(unexpected, collapse = ";"),
    missing_expected_predictors = paste(missing_wl[1:min(5, length(missing_wl))], collapse = ";"),
    technical_blacklist_hits = paste(tech_hits, collapse = ";"),
    coordinate_blacklist_hits = paste(coord_hits, collapse = ";"),
    ID_present = id_present,
    cell_index_present = cell_present,
    lon_lat_present = lonlat_present,
    other_unexpected_present = if(length(unexpected) > 0) paste(setdiff(unexpected, c(tech_hits, coord_hits)), collapse = ";") else "",
    predictor_set_exact_match = if(is.na(exact_match)) "NA_HISTORICAL" else as.character(exact_match),
    contamination_status = contam_status,
    action_required = if(contam_status == "CONTAMINATION_CONFIRMED") "REPAIR_REQUIRED"
                      else if(contam_status == "SUSPICIOUS_REQUIRES_ALGORITHM_AUDIT") "ALGORITHM_AUDIT_REQUIRED"
                      else "NONE",
    stringsAsFactors = FALSE
  ))
  
  # Long-format rows
  for (j in seq_along(pred_names)) {
    pn <- pred_names[j]
    long_results <- rbind(long_results, data.frame(
      accepted_taxon = taxon, source_generation = gen,
      model_object = basename(mo_file),
      predictor_name = pn, predictor_index = j,
      in_scientific_whitelist = pn %in% wl,
      technical_blacklist_hit = pn %in% tech_blacklist | tolower(pn) %in% tolower(tech_blacklist),
      coordinate_blacklist_hit = pn %in% coord_blacklist | tolower(pn) %in% tolower(coord_blacklist),
      unexpected_predictor = !(pn %in% wl),
      notes = "",
      stringsAsFactors = FALSE
    ))
  }
}

# Write outputs
write.csv(summary_results, file.path(OUT, "final_813_predictor_contamination_audit.csv"), row.names = FALSE)
write.csv(long_results, file.path(OUT, "final_813_declared_predictor_names_long.csv"), row.names = FALSE)

# Summary statistics
cat("\n=== CONTAMINATION CENSUS ===\n")
cat("Total audited:", nrow(summary_results), "\n")
cat("Model objects recovered:", sum(summary_results$contamination_status != "MODEL_OBJECT_NOT_RECOVERABLE"), "\n")
cat("Not recoverable:", sum(summary_results$contamination_status == "MODEL_OBJECT_NOT_RECOVERABLE"), "\n")
cat("\nStatus distribution:\n")
print(table(summary_results$contamination_status))

cat("\n=== CONTAMINATED TAXA ===\n")
contam <- summary_results[summary_results$contamination_status == "CONTAMINATION_CONFIRMED",]
cat("Count:", nrow(contam), "\n")
if (nrow(contam) > 0) {
  for (k in 1:nrow(contam)) {
    cat(sprintf("  %s | %s | %s\n", contam$accepted_taxon[k], contam$source_generation[k], contam$unexpected_predictors[k]))
  }
}

cat("\n=== SUSPICIOUS TAXA ===\n")
susp <- summary_results[summary_results$contamination_status == "SUSPICIOUS_REQUIRES_ALGORITHM_AUDIT",]
cat("Count:", nrow(susp), "\n")
if (nrow(susp) > 0) {
  for (k in 1:nrow(susp)) {
    cat(sprintf("  %s | %s | %s\n", susp$accepted_taxon[k], susp$source_generation[k], susp$unexpected_predictors[k]))
  }
}

cat("\n=== ID PRESENT ===\n")
id_taxa <- summary_results[summary_results$ID_present %in% TRUE,]
cat("Count:", nrow(id_taxa), "\n")
if (nrow(id_taxa) > 0) print(id_taxa[, c("accepted_taxon", "source_generation")])

cat("\n=== By source generation ===\n")
for (g in unique(summary_results$source_generation)) {
  sg <- summary_results[summary_results$source_generation == g,]
  cat(sprintf("%s: total=%d contam=%d suspicious=%d not_recoverable=%d pass=%d\n",
      g, nrow(sg), sum(sg$contamination_status == "CONTAMINATION_CONFIRMED"),
      sum(sg$contamination_status == "SUSPICIOUS_REQUIRES_ALGORITHM_AUDIT"),
      sum(sg$contamination_status == "MODEL_OBJECT_NOT_RECOVERABLE"),
      sum(sg$contamination_status %in% c("PASS_EXACT_EXPECTED_SET", "PASS_HISTORICAL_SCIENTIFIC_SET"))))
}

cat("\nDone.\n")
