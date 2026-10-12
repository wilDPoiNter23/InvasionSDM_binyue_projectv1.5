# Original source: phases_1_6_prep.R
# Original source location: ${WORK_ROOT}/publication_release_v1/scripts/phases_1_6_prep.R
# Original source SHA-256: f061617b3623f9be2a6076eb8d1f1dd596adbb1438a242742d004943f83df3a5
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# Phases 1-6: Aggregation preparation
suppressMessages(library(terra))

BASE <- "<WORK_ROOT>"
AUDIT <- file.path(BASE, "publication_release_v1/audits")
MANI  <- file.path(BASE, "publication_release_v1/manifests")
ORIG_DIR <- file.path(BASE, "results/aggregation_813_final_20260729")
ORIG_SCRIPT <- "<HPC_INPUT_ROOT>/script/0405Allstack.r"
STACK <- file.path(BASE, "data/predictors/full_stack_terrestrial_primary_v1.tif")

ref <- rast(STACK)
ref_lyr <- ref[[1]]

# ═══ Phase 1: Document original aggregation protocol ═══
cat("Phase 1: Original aggregation protocol\n")

protocol_txt <- file.path(AUDIT, "original_aggregation_protocol_frozen.txt")
writeLines(c(
  "Original Aggregation Protocol — Frozen",
  "=======================================",
  "Script: <HPC_INPUT_ROOT>/script/0405Allstack.r",
  "",
  "1. INPUT MANIFEST",
  "   File: results/evaluations/projection_summary.csv",
  "   Columns: species, type",
  "   type values: 'ensemble' (653 species), 'fallback' (84 species)",
  "",
  "2. PER-SPECIES PROJECTION SELECTION",
  "   For each species:",
  "   - Load RDS: results/projections/{species}_{type}_projection.rds",
  "   - Extract: model_name <- rds@models.projected[1]",
  "   - Search: models/{species_dot}/ recursively for {model_name}.*.(tif|grd)",
  "   - Use: tif_file[1] (first match)",
  "",
  "3. ENSEMBLE vs SINGLE-MODEL HANDLING",
  "   - type='ensemble': models.projected[1] returns ensemble model name (e.g. EMmeanByTSS)",
  "   - type='fallback': models.projected[1] returns individual model name",
  "   - No explicit EMmean/EMmedian selection — uses BIOMOD2 default order",
  "",
  "4. NORMALIZATION (per-species, before aggregation)",
  "   normalize_raster <- function(r) {",
  "     mm <- terra::minmax(r)",
  "     rmin <- mm[1]; rmax <- mm[2]",
  "     if (is.na(rmin) || is.na(rmax) || rmin == rmax) {",
  "       return(r * 0)",
  "     }",
  "     return((r - rmin) / (rmax - rmin))",
  "   }",
  "   Formula: (x - min) / (max - min)",
  "   Edge case (constant raster): return 0",
  "",
  "5. NA HANDLING",
  "   r <- terra::ifel(is.na(r), 0, r)",
  "   NA cells → 0 before accumulation",
  "",
  "6. ACCUMULATION",
  "   r_sum <- terra::cover(r_sum + r, r_sum)",
  "   NA-safe sum using cover()",
  "",
  "7. MEAN CALCULATION",
  "   r_mean <- r_sum / count",
  "   Denominator: count = number of successfully processed species",
  "   Equal weight per species (not per non-NA cell)",
  "",
  "8. RASTER ALIGNMENT",
  "   Implicit: all rasters must share CRS, extent, resolution",
  "   No explicit check; dimension mismatch caught by tryCatch",
  "",
  "9. OUTPUT",
  "   File: results/hotspot_mean_suitability_normalized.tif",
  "   Format: GeoTIFF",
  "",
  "10. FAILURE LOG",
  "    results/stack_failed_species_log_normalized.xlsx",
  ""
), protocol_txt)
cat("  Protocol saved.\n")

# ═══ Phase 2: Recover original frozen inputs ═══
cat("Phase 2: Original frozen input manifest\n")

summary_df <- read.csv(file.path(BASE, "results/evaluations/projection_summary.csv"), stringsAsFactors=FALSE)
cat("  Original species count:", nrow(summary_df), "\n")

orig_mani <- data.frame(
  original_species_name = summary_df$species,
  accepted_taxon = summary_df$species,
  original_product_type = summary_df$type,
  original_projection_path = NA_character_,
  original_projection_sha256 = NA_character_,
  included_in_original_aggregation = TRUE,
  source_rule = "projection_summary_csv",
  notes = "",
  stringsAsFactors = FALSE
)

# Reconstruct paths per original logic
for (i in seq_len(nrow(summary_df))) {
  sp <- summary_df$species[i]
  sp_dot <- gsub(" ", ".", sp)
  proj_type <- summary_df$type[i]
  rds_path <- file.path(BASE, "results/projections", paste0(sp, "_", proj_type, "_projection.rds"))
  
  if (file.exists(rds_path)) {
    rds <- tryCatch(readRDS(rds_path), error=function(e) NULL)
    if (!is.null(rds) && inherits(rds, "BIOMOD.projection.out")) {
      model_name <- rds@models.projected[1]
      tif_file <- list.files(
        file.path(BASE, "models", sp_dot),
        pattern = paste0(model_name, ".*\\.(tif|grd)$"),
        full.names = TRUE, recursive = TRUE
      )
      if (length(tif_file) > 0) {
        orig_mani$original_projection_path[i] <- tif_file[1]
        orig_mani$original_projection_sha256[i] <- system(paste("sha256sum", shQuote(tif_file[1]), "| awk '{print $1}'"), intern=TRUE)
      } else {
        orig_mani$notes[i] <- "PROJECTION_RASTER_NOT_FOUND"
      }
    } else {
      orig_mani$notes[i] <- "INVALID_RDS"
    }
  } else {
    orig_mani$notes[i] <- "RDS_NOT_FOUND"
  }
}

write.csv(orig_mani, file.path(MANI, "original_frozen_aggregation_input_manifest.csv"), row.names=FALSE)
cat("  Original manifest:", nrow(orig_mani), "rows\n")
cat("  Found rasters:", sum(!is.na(orig_mani$original_projection_path)), "\n")

# ═══ Phase 3: Apply taxonomic rule ═══
cat("Phase 3: Taxonomy audit\n")

tax_audit <- data.frame(
  metric = c("raw_original_candidate_count", "accepted_unique_taxon_count",
    "synonym_exclusion_count", "duplicate_accepted_taxa_count",
    "Diodia_teres_count", "Hexasepalum_teres_count", "final_included_taxon_count"),
  value = NA_integer_,
  stringsAsFactors = FALSE
)

tax_audit$value[tax_audit$metric=="raw_original_candidate_count"] <- nrow(summary_df)

# Apply Diodia teres → Hexasepalum teres rule
is_diodia <- summary_df$species == "Diodia teres"
is_hexasepalum <- summary_df$species == "Hexasepalum teres"

# Remove Diodia, keep Hexasepalum (already in summary)
accepted <- summary_df$species
# If Diodia exists and Hexasepalum exists, exclude Diodia
# If Diodia exists and Hexasepalum doesn't, rename Diodia to Hexasepalum
diodia_count <- sum(is_diodia)
hex_count <- sum(is_hexasepalum)
cat("  Diodia teres count:", diodia_count, "\n")
cat("  Hexasepalum teres count:", hex_count, "\n")

# For final: exclude Diodia from accepted list
final_accepted <- accepted[!is_diodia]
# Deduplicate
dup_count <- sum(duplicated(final_accepted))
final_accepted <- unique(final_accepted)

tax_audit$value[tax_audit$metric=="accepted_unique_taxon_count"] <- length(unique(accepted))
tax_audit$value[tax_audit$metric=="synonym_exclusion_count"] <- diodia_count
tax_audit$value[tax_audit$metric=="duplicate_accepted_taxa_count"] <- dup_count
tax_audit$value[tax_audit$metric=="Diodia_teres_count"] <- diodia_count
tax_audit$value[tax_audit$metric=="Hexasepalum_teres_count"] <- hex_count
tax_audit$value[tax_audit$metric=="final_included_taxon_count"] <- length(final_accepted)

write.csv(tax_audit, file.path(AUDIT, "final_aggregation_taxonomy_audit.csv"), row.names=FALSE)
cat("  Final accepted taxon count:", length(final_accepted), "\n")

# ═══ Phase 4: Build publication aggregation manifest ═══
cat("Phase 4: Publication aggregation manifest\n")

# Read repaired manifest
rep_mani <- read.csv(file.path(MANI, "final_repaired_projection_manifest_v2.csv"), stringsAsFactors=FALSE)
rep_species <- rep_mani$species
cat("  Repaired species:", length(rep_species), "\n")

# Build combined manifest
agg_mani <- orig_mani
agg_mani$source_generation <- "ORIGINAL_FROZEN"
agg_mani$final_projection_path <- agg_mani$original_projection_path
agg_mani$final_projection_sha256 <- agg_mani$original_projection_sha256
agg_mani$replaced_by_v4 <- FALSE
agg_mani$replacement_reason <- ""
agg_mani$projection_execution_mode <- ""
agg_mani$geometry_pass <- NA
agg_mani$numeric_QC_pass <- NA
agg_mani$include_in_aggregation <- TRUE
agg_mani$taxonomic_note <- ""

# Replace repaired species
for (i in seq_len(nrow(agg_mani))) {
  sp <- agg_mani$original_species_name[i]
  if (sp %in% rep_species) {
    rrow <- rep_mani[rep_mani$species==sp, ]
    if (nrow(rrow)==1 && rrow$ready_for_aggregation) {
      agg_mani$source_generation[i] <- "REPAIRED_MULTIFOLD_V4"
      agg_mani$final_projection_path[i] <- rrow$final_raster_path
      agg_mani$final_projection_sha256[i] <- rrow$final_raster_sha256
      agg_mani$replaced_by_v4[i] <- TRUE
      agg_mani$replacement_reason[i] <- "NEGATIVE_TSS_REPAIR"
      agg_mani$projection_execution_mode[i] <- rrow$source_run[1]
      agg_mani$geometry_pass[i] <- rrow$geometry_pass
      agg_mani$numeric_QC_pass[i] <- rrow$numeric_QC_pass
    }
  }
}

# Apply taxonomy: exclude Diodia teres
agg_mani$include_in_aggregation[agg_mani$original_species_name=="Diodia teres"] <- FALSE
agg_mani$taxonomic_note[agg_mani$original_species_name=="Diodia teres"] <- "SYNONYM_EXCLUDED_Diodia_teres_is_Hexasepalum_teres"
if (hex_count > 0) {
  agg_mani$taxonomic_note[agg_mani$original_species_name=="Hexasepalum teres"] <- "ACCEPTED_NAME"
}

# Write
write.csv(agg_mani, file.path(MANI, "final_aggregation_input_manifest_v1.csv"), row.names=FALSE)

n_orig <- sum(agg_mani$source_generation=="ORIGINAL_FROZEN")
n_rep <- sum(agg_mani$source_generation=="REPAIRED_MULTIFOLD_V4")
n_incl <- sum(agg_mani$include_in_aggregation)
cat("  Original-frozen:", n_orig, "\n")
cat("  Repaired-v4:", n_rep, "\n")
cat("  Included in aggregation:", n_incl, "\n")

# ═══ Phase 5: Replacement-only integrity pre-check ═══
cat("Phase 5: Replacement integrity pre-check\n")

int_check <- data.frame(
  accepted_taxon = agg_mani$original_species_name,
  original_path = agg_mani$original_projection_path,
  publication_path = agg_mani$final_projection_path,
  original_sha256 = agg_mani$original_projection_sha256,
  publication_sha256 = agg_mani$final_projection_sha256,
  path_changed = FALSE,
  checksum_changed = FALSE,
  expected_change = FALSE,
  reason = "",
  integrity_pass = TRUE,
  stringsAsFactors = FALSE
)

for (i in seq_len(nrow(int_check))) {
  op <- int_check$original_path[i]; pp <- int_check$publication_path[i]
  os <- int_check$original_sha256[i]; ps <- int_check$publication_sha256[i]
  
  int_check$path_changed[i] <- !is.na(op) && !is.na(pp) && op != pp
  int_check$checksum_changed[i] <- !is.na(os) && !is.na(ps) && os != ps
  
  is_repaired <- agg_mani$source_generation[i] == "REPAIRED_MULTIFOLD_V4"
  is_excluded <- !agg_mani$include_in_aggregation[i]
  int_check$expected_change[i] <- is_repaired || is_excluded
  
  if (is_repaired) {
    int_check$reason[i] <- "APPROVED_REPAIRED_V4_REPLACEMENT"
  } else if (is_excluded) {
    int_check$reason[i] <- "TAXONOMIC_SYNONYM_EXCLUSION"
  } else if (int_check$path_changed[i] || int_check$checksum_changed[i]) {
    int_check$reason[i] <- "UNEXPLAINED_CHANGE"
    int_check$integrity_pass[i] <- FALSE
  } else {
    int_check$reason[i] <- "IDENTICAL_UNCHANGED"
  }
}

write.csv(int_check, file.path(AUDIT, "replacement_only_integrity_preaggregation.csv"), row.names=FALSE)

unexplained <- sum(!int_check$integrity_pass)
cat("  Unaffected unchanged:", sum(int_check$reason=="IDENTICAL_UNCHANGED"), "\n")
cat("  Repaired replacements:", sum(int_check$reason=="APPROVED_REPAIRED_V4_REPLACEMENT"), "\n")
cat("  Unexplained changes:", unexplained, "\n")

if (unexplained > 0) {
  cat("  *** PUBLICATION_REPLACEMENT_ONLY_PRECHECK_FAIL ***\n")
  print(int_check[!int_check$integrity_pass, c("accepted_taxon","reason")])
} else {
  cat("  PUBLICATION_REPLACEMENT_ONLY_PRECHECK_PASS\n")
}

# ═══ Phase 6: Pre-aggregation raster QC ═══
cat("Phase 6: Pre-aggregation raster QC\n")

included <- agg_mani[agg_mani$include_in_aggregation, ]
qc_df <- data.frame(
  accepted_taxon = included$original_species_name,
  source_generation = included$source_generation,
  final_projection_path = included$final_projection_path,
  expected_sha256 = included$final_projection_sha256,
  actual_sha256 = NA_character_,
  checksum_match = FALSE,
  readable = FALSE,
  CRS = NA_character_,
  extent = NA_character_,
  resolution = NA_character_,
  rows = NA_integer_,
  columns = NA_integer_,
  geometry_match = FALSE,
  total_cell_count = NA_integer_,
  non_NA_cell_count = NA_integer_,
  finite_cell_count = NA_integer_,
  min_val = NA_real_,
  mean_val = NA_real_,
  median_val = NA_real_,
  max_val = NA_real_,
  sd_val = NA_real_,
  all_NA = FALSE,
  all_zero = FALSE,
  Inf_flag = FALSE,
  NaN_flag = FALSE,
  QC_pass = FALSE,
  stringsAsFactors = FALSE
)

for (i in seq_len(nrow(qc_df))) {
  fp <- qc_df$final_projection_path[i]
  if (is.na(fp) || !file.exists(fp)) next
  
  qc_df$actual_sha256[i] <- system(paste("sha256sum", shQuote(fp), "| awk '{print $1}'"), intern=TRUE)
  qc_df$checksum_match[i] <- !is.na(qc_df$expected_sha256[i]) && qc_df$actual_sha256[i] == qc_df$expected_sha256[i]
  
  r <- tryCatch(rast(fp), error=function(e) NULL)
  if (is.null(r)) next
  
  qc_df$readable[i] <- TRUE
  qc_df$CRS[i] <- crs(r, describe=TRUE)$code
  qc_df$extent[i] <- paste(ext(r)[1:4], collapse=" ")
  qc_df$resolution[i] <- paste(res(r)[1], res(r)[2])
  qc_df$rows[i] <- nrow(r); qc_df$columns[i] <- ncol(r)
  qc_df$geometry_match[i] <- all(ext(r)==ext(ref_lyr) && all(res(r)==res(ref_lyr)))
  qc_df$total_cell_count[i] <- ncell(r)
  
  g <- global(r, "notNA", na.rm=TRUE); qc_df$non_NA_cell_count[i] <- g[1,1]
  g <- global(r, "min", na.rm=TRUE); qc_df$min_val[i] <- g[1,1]
  g <- global(r, "max", na.rm=TRUE); qc_df$max_val[i] <- g[1,1]
  g <- global(r, "mean", na.rm=TRUE); qc_df$mean_val[i] <- g[1,1]
  g <- global(r, "sd", na.rm=TRUE); qc_df$sd_val[i] <- g[1,1]
  qc_df$finite_cell_count[i] <- qc_df$non_NA_cell_count[i]
  
  mm <- minmax(r)
  qc_df$all_NA[i] <- !hasValues(r)
  qc_df$all_zero[i] <- isTRUE(qc_df$min_val[i]==0 && qc_df$max_val[i]==0)
  qc_df$Inf_flag[i] <- any(is.infinite(mm))
  qc_df$NaN_flag[i] <- any(is.nan(mm))
  
  qc_df$QC_pass[i] <- qc_df$readable[i] && qc_df$checksum_match[i] &&
    qc_df$geometry_match[i] && !qc_df$all_NA[i] && !qc_df$all_zero[i] &&
    !qc_df$Inf_flag[i] && !qc_df$NaN_flag[i] && !is.na(qc_df$non_NA_cell_count[i]) && qc_df$non_NA_cell_count[i] > 0
}

write.csv(qc_df, file.path(AUDIT, "final_aggregation_input_QC.csv"), row.names=FALSE)

cat("  Included taxa:", nrow(qc_df), "\n")
cat("  QC pass:", sum(qc_df$QC_pass), "/", nrow(qc_df), "\n")
cat("  Checksum failures:", sum(!qc_df$checksum_match), "\n")
cat("  Geometry failures:", sum(!qc_df$geometry_match), "\n")
cat("  Missing rasters:", sum(!qc_df$readable), "\n")

if (all(qc_df$QC_pass)) {
  cat("  PUBLICATION_AGGREGATION_INPUT_QC_PASS\n")
} else {
  cat("  PUBLICATION_AGGREGATION_INPUT_QC_FAIL\n")
  fails <- qc_df[!qc_df$QC_pass, ]
  for (j in seq_len(min(5, nrow(fails)))) {
    cat("    FAIL:", fails$accepted_taxon[j], "- readable:", fails$readable[j], "checksum:", fails$checksum_match[j], "geom:", fails$geometry_match[j], "\n")
  }
}
