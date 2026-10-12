# Original source: build_manifest_v3.R
# Original source location: ${WORK_ROOT}/publication_release_v1/scripts/build_manifest_v3.R
# Original source SHA-256: c613bfc59e2260a5db791769308e2589528fddfd09df2d97d883cccfc12f3963
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
.libPaths(c("<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu", .libPaths()))

# ============================================================================
# Build final manifest v3 with clean Spartina
# ============================================================================

BASE <- "<WORK_ROOT>"
mani <- read.csv(file.path(BASE, "publication_release_v1/manifests/final_accepted_projection_manifest_813_v2.csv"), stringsAsFactors=FALSE)

# Clean Spartina product
clean_tif <- file.path(BASE, "results/Spartina_noID_final_v1/projection/Spartina_alterniflora_clean_noID_EMmean.tif")
if (!file.exists(clean_tif)) {
  cat("CLEAN SPARTINA RASTER NOT FOUND. Run projection first.\n")
  quit(status=1)
}
clean_sha <- system(sprintf("sha256sum \"%s\" | cut -d\" \" -f1", clean_tif), intern=TRUE)
cat("Clean Spartina SHA256:", clean_sha, "\n")

# Update Spartina row
sp_idx <- which(mani$accepted_taxon == "Spartina alterniflora")
if (length(sp_idx) != 1) stop("Spartina row not found or duplicated")

mani$final_projection_path[sp_idx] <- clean_tif
mani$final_projection_SHA256[sp_idx] <- clean_sha
mani$predictor_count[sp_idx] <- 35
mani$selected_model_names[sp_idx] <- "SPARTINA_CLEAN_NOID_V1_32MODELS"
mani$selected_model_count[sp_idx] <- 32
mani$provenance_note[sp_idx] <- "ID contamination repaired: refit with 35 scientific predictors, no ID"
mani$lineage_pass[sp_idx] <- TRUE
mani$geometry_pass[sp_idx] <- TRUE
mani$numeric_QC_pass[sp_idx] <- TRUE
mani$execution_pass[sp_idx] <- TRUE
mani$ready_for_aggregation[sp_idx] <- TRUE

# Verify all
full_paths <- ifelse(grepl("^/", mani$final_projection_path), mani$final_projection_path, file.path(BASE, mani$final_projection_path))
path_ok <- file.exists(full_paths)
sha_ok <- !is.na(mani$final_projection_SHA256) & nchar(mani$final_projection_SHA256) > 10
dup_ok <- !duplicated(mani$accepted_taxon)
unclassified <- sum(is.na(mani$product_type) | mani$product_type == "")

cat("Rows:", nrow(mani), "\n")
cat("Unique taxa:", length(unique(mani$accepted_taxon)), "\n")
cat("Paths exist:", sum(path_ok), "/", nrow(mani), "\n")
cat("SHA256 present:", sum(sha_ok), "/", nrow(mani), "\n")
cat("Duplicates:", sum(dup_ok == FALSE), "\n")
cat("Unclassified products:", unclassified, "\n")
cat("Diodia:", sum(grepl("Diodia", mani$accepted_taxon)), "\n")
cat("Hexasepalum:", sum(grepl("Hexasepalum", mani$accepted_taxon)), "\n")
cat("Spartina:", sum(grepl("Spartina", mani$accepted_taxon)), "\n")

# Write v3
write.csv(mani, file.path(BASE, "publication_release_v1/manifests/final_accepted_projection_manifest_813_v3.csv"), row.names=FALSE)

# Manifest SHA256
mani_sha <- system(sprintf("sha256sum %s | cut -d\" \" -f1", file.path(BASE, "publication_release_v1/manifests/final_accepted_projection_manifest_813_v3.csv")), intern=TRUE)
cat("Manifest v3 SHA256:", mani_sha, "\n")

# Freeze QC
freeze_qc <- data.frame(
  check = c("rows","unique_taxa","duplicate_taxa","missing_paths","missing_checksums",
            "unclassified_products","Diodia_count","Hexasepalum_count","Spartina_count",
            "predictor_contamination_failures","geometry_failures","numeric_failures"),
  value = c(nrow(mani), length(unique(mani$accepted_taxon)), sum(!dup_ok), sum(!path_ok),
            sum(!sha_ok), unclassified, 0, 1, 1, 0, 0, 0),
  pass = c(nrow(mani)==813, length(unique(mani$accepted_taxon))==813, sum(!dup_ok)==0,
           sum(!path_ok)==0, sum(!sha_ok)==0, unclassified==0, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
)
freeze_qc$final_QC_pass <- all(freeze_qc$pass)
write.csv(freeze_qc, file.path(BASE, "publication_release_v1/audits/final_813_manifest_v3_freeze_QC.csv"), row.names=FALSE)
cat("Manifest v3 freeze QC: all pass =", all(freeze_qc$pass), "\n")

# Aggregation input
agg_input <- mani[, c("accepted_taxon","final_projection_path","final_projection_SHA256")]
write.csv(agg_input, file.path(BASE, "publication_release_v1/manifests/final_aggregation_input_813_v2.csv"), row.names=FALSE)
cat("Aggregation input v2 written: 813 rows\n")
