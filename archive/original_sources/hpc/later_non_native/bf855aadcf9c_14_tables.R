# Original source: 14_tables.R
# Original source location: ${NON_NATIVE_ROOT}/02_derived_non_native_potential/07_scripts/14_tables.R
# Original source SHA-256: bf855aadcf9c557e7a909b6ed9ee51d2f9be43dfdc004cf75a648c3668eb3344
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_NON_NATIVE)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# 14_tables.R — PHASE 23: publication tables
ca <- commandArgs(trailingOnly = FALSE)
source(file.path(dirname(sub("^--file=", "", ca[grep("^--file=", ca)])), "00_config.R"))

mani  <- read.csv(file.path(DIR_SPP_MANIFESTS, "non_native_projection_manifest_813_v1.csv"),
                  stringsAsFactors = FALSE)
mapqc <- read.csv(file.path(DIR_MASK_QC, "native_range_mapping_QC.csv"),
                  stringsAsFactors = FALSE)
aggqc <- read.csv(file.path(DIR_AGG_QC, "non_native_aggregate_QC.csv"),
                  stringsAsFactors = FALSE)
cmp   <- read.csv(file.path(DIR_TABLES, "global_vs_non_native_aggregate_comparison.csv"),
                  stringsAsFactors = FALSE)

# 1. native-range mapping summary
t1 <- as.data.frame(table(mapqc$QC_status), stringsAsFactors = FALSE)
names(t1) <- c("QC_status", "n_taxa")
write.csv(t1, file.path(DIR_TABLES, "native_range_mapping_summary.csv"), row.names = FALSE)

# 2. species native-country counts (masked taxa; 3 global-equal taxa have NA)
t2 <- mani[mani$product_status == "COMPLETE" & mani$non_native_treatment == "MASKED_NATIVE_DOMAIN",
           c("accepted_taxon", "native_country_count", "native_country_codes")]
names(t2) <- c("accepted_taxon", "native_country_count", "native_country_codes_ISO3")
write.csv(t2, file.path(DIR_TABLES, "species_native_country_counts.csv"), row.names = FALSE)

t2b <- data.frame(
  statistic = c("n_taxa_with_native_mapping", "median_native_countries",
                "min_native_countries", "max_native_countries"),
  value = c(nrow(t2),
            median(t2$native_country_count, na.rm = TRUE),
            min(t2$native_country_count, na.rm = TRUE),
            max(t2$native_country_count, na.rm = TRUE)))
write.csv(t2b, file.path(DIR_TABLES, "species_native_country_counts_summary.csv"), row.names = FALSE)

# 3. projection QC summary
qc <- read.csv(file.path(DIR_SPP_QC, "non_native_projection_QC_813.csv"),
               stringsAsFactors = FALSE)
t3 <- data.frame(
  statistic = c("n_taxa_processed", "n_QC_pass", "n_QC_fail",
                "n_no_native_origin_global_equal",
                "n_values_not_preserved", "n_geometry_mismatch",
                "total_native_cells_removed",
                "median_proportion_cells_removed"),
  value = c(nrow(qc), sum(qc$QC_pass, na.rm = TRUE), sum(!qc$QC_pass, na.rm = TRUE),
            sum(mani$non_native_treatment == "NO_NATIVE_ORIGIN_GLOBAL_EQUAL"),
            sum(!qc$values_preserved, na.rm = TRUE),
            sum(!qc$geometry_match, na.rm = TRUE),
            sum(qc$native_cells_removed, na.rm = TRUE),
            median(qc$proportion_cells_removed, na.rm = TRUE)))
write.csv(t3, file.path(DIR_TABLES, "projection_QC_summary.csv"), row.names = FALSE)

# 4. aggregate QC summary
write.csv(aggqc, file.path(DIR_TABLES, "aggregate_QC_summary.csv"), row.names = FALSE)

# 5. unmatched / ambiguous / excluded taxa
t5 <- mani[mani$product_status != "COMPLETE",
           c("accepted_taxon", "native_mapping_source", "native_country_count",
             "product_status", "notes")]
write.csv(t5, file.path(DIR_TABLES, "excluded_or_failed_taxa.csv"), row.names = FALSE)

# 6. global-vs-non-native comparison
write.csv(cmp, file.path(DIR_TABLES, "global_vs_non_native_aggregate_comparison.csv"),
          row.names = FALSE)

cat("Tables written.\n"); print(t1); print(t3); print(t2b)
