# SUPPORTING LATER PRODUCT: not established as submitted 2025 workflow.
# Original SHA256: 7626afae85342642e7894a0fb18241b8d522f8a586155780f520624cdefac64e
# 10_assemble_manifest_QC.R — PHASES 12 & 14: assemble per-task QC + final 813 manifest
ca <- commandArgs(trailingOnly = FALSE)
source(file.path(dirname(sub("^--file=", "", ca[grep("^--file=", ca)])), "00_config.R"))

mani  <- read.csv(FROZEN_MANIFEST_V3, stringsAsFactors = FALSE)
clean <- read.csv(file.path(DIR_INPUTS, "species_native_mapping_clean.csv"),
                  stringsAsFactors = FALSE)
map_qc <- read.csv(file.path(DIR_MASK_QC, "native_range_mapping_QC.csv"),
                   stringsAsFactors = FALSE)

qc_cols <- c("accepted_taxon","input_projection_path","input_SHA256",
             "native_polygon_found","native_country_count","output_path","output_SHA256",
             "CRS_match","extent_match","resolution_match","dimensions_match",
             "geometry_match","input_nonNA","output_nonNA","native_cells_removed",
             "proportion_cells_removed","input_min","input_max","output_min","output_max",
             "values_preserved","has_Inf","has_NaN","all_NA","all_zero","QC_pass","notes")

qc_list <- lapply(mani$accepted_taxon, function(tx) {
  f <- file.path(PER_TAXON_QC_DIR, paste0(taxon_safe(tx), "__qc.csv"))
  if (!file.exists(f)) {
    return(data.frame(accepted_taxon = tx,
                      input_projection_path = NA, input_SHA256 = NA,
                      native_polygon_found = NA, native_country_count = NA,
                      output_path = NA, output_SHA256 = NA,
                      CRS_match = NA, extent_match = NA, resolution_match = NA,
                      dimensions_match = NA, geometry_match = NA,
                      input_nonNA = NA, output_nonNA = NA, native_cells_removed = NA,
                      proportion_cells_removed = NA,
                      input_min = NA, input_max = NA, output_min = NA, output_max = NA,
                      values_preserved = NA, has_Inf = NA, has_NaN = NA,
                      all_NA = NA, all_zero = NA, QC_pass = FALSE,
                      notes = "NO_TASK_QC_FILE", stringsAsFactors = FALSE))
  }
  d <- read.csv(f, stringsAsFactors = FALSE)
  for (col in setdiff(qc_cols, names(d))) d[[col]] <- NA
  d[, qc_cols, drop = FALSE]
})
qc_all <- do.call(rbind, qc_list)
qc_all <- qc_all[match(mani$accepted_taxon, qc_all$accepted_taxon), ]

write.csv(qc_all, file.path(DIR_SPP_QC, "non_native_projection_QC_813.csv"),
          row.names = FALSE)
cat("QC rows:", nrow(qc_all), "passed:", sum(qc_all$QC_pass, na.rm = TRUE),
    "failed:", sum(!qc_all$QC_pass, na.rm = TRUE), "\n")

# ---- final manifest ----
m <- merge(mani[, c("accepted_taxon", "final_projection_path", "final_projection_SHA256")],
           clean[, c("accepted_taxon", "mapping_status", "native_country_standardised",
                     "native_country_code")], by = "accepted_taxon", all.x = TRUE)
m <- merge(m, map_qc[, c("accepted_taxon", "native_country_count", "QC_status")],
           by = "accepted_taxon", all.x = TRUE)
m <- merge(m, qc_all[, c("accepted_taxon", "output_path", "output_SHA256",
                         "input_min", "input_max", "output_min", "output_max",
                         "geometry_match", "values_preserved", "QC_pass", "notes")],
           by = "accepted_taxon", all.x = TRUE)

mani_out <- data.frame(
  accepted_taxon              = m$accepted_taxon,
  source_global_projection    = m$final_projection_path,
  source_global_SHA256        = m$final_projection_SHA256,
  native_mapping_source       = m$mapping_status,
  native_country_count        = m$native_country_count,
  native_country_codes        = m$native_country_code,
  native_polygon_layer        = ifelse(m$mapping_status == "MISSING_NATIVE_MAPPING",
                                       "NO_NATIVE_MAPPING", paste0(NATIVE_GPKG, ":native_ranges_813")),
  output_non_native_projection = m$output_path,
  output_SHA256               = m$output_SHA256,
  input_scale                 = ifelse(is.na(m$input_min), NA,
                                       paste0(sprintf("%.4g", m$input_min), "-", sprintf("%.4g", m$input_max))),
  output_scale                = ifelse(is.na(m$output_min), NA,
                                       paste0(sprintf("%.4g", m$output_min), "-", sprintf("%.4g", m$output_max))),
  geometry_preserved          = m$geometry_match,
  values_preserved            = m$values_preserved,
  QC_pass                     = m$QC_pass,
  product_status              = ifelse(m$QC_pass, "COMPLETE", "FAILED"),
  non_native_treatment        = ifelse(m$mapping_status == "MISSING_NATIVE_MAPPING",
                                       "NO_NATIVE_ORIGIN_GLOBAL_EQUAL",
                                       "MASKED_NATIVE_DOMAIN"),
  notes                        = ifelse(m$mapping_status == "MISSING_NATIVE_MAPPING" & !is.na(m$notes),
                                       paste0(m$notes, " Included in non-native product as global-equal (no wild native origin; introduced-only distribution documented)."),
                                       m$notes),
  stringsAsFactors = FALSE)

write.csv(mani_out, file.path(DIR_SPP_MANIFESTS, "non_native_projection_manifest_813_v1.csv"),
          row.names = FALSE)

sha <- sha256_of(file.path(DIR_SPP_MANIFESTS, "non_native_projection_manifest_813_v1.csv"))
cat("Manifest rows:", nrow(mani_out), "SHA256:", sha, "\n")
cat("product_status counts:\n"); print(table(mani_out$product_status))

# failed indices for retry
failed_idx <- which(mani_out$product_status == "FAILED")
write.csv(data.frame(manifest_row_index = failed_idx,
                     accepted_taxon = mani_out$accepted_taxon[failed_idx]),
          file.path(DIR_PROVENANCE, "failed_taxa_retry_list.csv"), row.names = FALSE)
cat("Failed taxa needing retry:", length(failed_idx), "\n")
