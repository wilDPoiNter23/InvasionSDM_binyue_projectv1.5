# Original source: build_performance_table.R
# Original source location: ${NON_NATIVE_ROOT}/temp/build_performance_table.R
# Original source SHA-256: 5fc7c267aef074166728f2fd5fe5fe549a98d26dc4e8ffbdbdd55405046f193b
# Recovery: live read-only SSH, 2026-10-12
# Classification: SUPPORTING (HISTORICAL_SUPPORTING_AUDIT_LINEAGE_HELPER)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
.libPaths(c(.libPaths(), "<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu"))

# ============================================================
# Build complete species_model_performance_813.csv
# Merging: 784-eval + gap75-classification + repair-v2 + Spartina
# ============================================================

manifest <- read.csv("<WORK_ROOT>/publication_release_v1/manifests/final_accepted_projection_manifest_813_v1.csv", stringsAsFactors = FALSE)
eval_784 <- read.csv("<NON_NATIVE_ROOT>/temp/eval_TSS_ROC_784.csv", stringsAsFactors = FALSE)
gap75 <- read.csv("<WORK_ROOT>/publication_release_v1/audits/gap75_full_classification_and_selection.csv", stringsAsFactors = FALSE)
repair <- read.csv("<WORK_ROOT>/publication_release_v1/manifests/gap13_repair_projection_selection_final_v2.csv", stringsAsFactors = FALSE)

# Build master table row by row
master <- data.frame(stringsAsFactors = FALSE)

for (i in 1:nrow(manifest)) {
  taxon <- manifest$accepted_taxon[i]
  gen <- manifest$source_generation[i]
  ptype <- manifest$product_type[i]
  
  row <- data.frame(
    accepted_taxon = taxon,
    source_generation = gen,
    product_type = ptype,
    n_presence = NA,
    CV_strategy = "user_defined_random_kfold",
    CV_folds = NA,
    selected_model_count = NA,
    best_TSS = NA,
    best_ROC = NA,
    selection_tier = NA,
    data_source = NA,
    stringsAsFactors = FALSE
  )
  
  # Determine data source priority
  if (gen == "GAP13_REPAIR35_V1") {
    # Use repair selection v2
    r_row <- repair[repair$accepted_taxon == taxon, ]
    if (nrow(r_row) > 0) {
      row$n_presence <- r_row$n_presence[1]
      row$selected_model_count <- r_row$selected_model_count[1]
      row$best_TSS <- r_row$best_TSS[1]
      row$best_ROC <- r_row$best_ROC[1]
      row$selection_tier <- r_row$selection_tier[1]
      row$data_source <- "gap13_repair_selection_v2"
      # Adaptive CV folds for low-N species
      row$CV_folds <- ifelse(row$n_presence <= 25, max(2, floor(row$n_presence/4)), 4)
    }
  } else if (gen == "SPARTINA_FINAL_V1") {
    # Use Spartina model data (from prior extraction)
    row$n_presence <- 886
    row$selected_model_count <- 12
    row$best_TSS <- 0.950
    row$best_ROC <- 0.995
    row$selection_tier <- "PRIMARY"
    row$CV_folds <- 4
    row$data_source <- "Spartina_model_object"
  } else if (gen %in% c("GAP75_VIIRS35_V1", "GAP75_DIRECT34_V1")) {
    # Use gap75 classification CSV
    g_row <- gap75[gap75$species == taxon, ]
    if (nrow(g_row) > 0) {
      row$n_presence <- NA  # Not in this CSV
      row$selected_model_count <- g_row$n_models_built[1]
      row$best_TSS <- g_row$best_TSS[1]
      row$best_ROC <- g_row$best_ROC[1]
      row$selection_tier <- g_row$best_tier[1]
      row$CV_folds <- 4
      row$data_source <- "gap75_classification"
    }
  } else if (gen == "ORIGINAL_FROZEN") {
    # Use 784 eval CSV
    e_row <- eval_784[eval_784$accepted_taxon == taxon, ]
    if (nrow(e_row) > 0) {
      row$n_presence <- NA
      row$selected_model_count <- e_row$n_models[1]
      row$best_TSS <- e_row$best_TSS_val[1]
      row$best_ROC <- e_row$best_ROC_val[1]
      row$selection_tier <- e_row$selection_tier[1]
      row$CV_folds <- 4
      row$data_source <- "comprehensive_eval_CSVs"
    }
  }
  
  master <- rbind(master, row)
}

# Summary statistics
cat("=== Master table built ===\n")
cat("Total rows:", nrow(master), "\n")
cat("Species with TSS:", sum(!is.na(master$best_TSS)), "\n")
cat("Species with ROC:", sum(!is.na(master$best_ROC)), "\n")
cat("Species missing TSS:", sum(is.na(master$best_TSS)), "\n")

cat("\n=== Selection tier distribution ===\n")
print(table(master$selection_tier, useNA = "ifany"))

cat("\n=== Data source distribution ===\n")
print(table(master$data_source, useNA = "ifany"))

cat("\n=== TSS distribution ===\n")
tss <- master$best_TSS[!is.na(master$best_TSS)]
cat(sprintf("N=%d Mean=%.4f SD=%.4f Min=%.4f Q05=%.4f Q25=%.4f Median=%.4f Q75=%.4f Q95=%.4f Max=%.4f\n",
    length(tss), mean(tss), sd(tss), min(tss),
    quantile(tss, 0.05), quantile(tss, 0.25), median(tss),
    quantile(tss, 0.75), quantile(tss, 0.95), max(tss)))

cat("\n=== ROC distribution ===\n")
roc <- master$best_ROC[!is.na(master$best_ROC)]
cat(sprintf("N=%d Mean=%.4f SD=%.4f Min=%.4f Q05=%.4f Q25=%.4f Median=%.4f Q75=%.4f Q95=%.4f Max=%.4f\n",
    length(roc), mean(roc), sd(roc), min(roc),
    quantile(roc, 0.05), quantile(roc, 0.25), median(roc),
    quantile(roc, 0.75), quantile(roc, 0.95), max(roc)))

# Show missing
cat("\n=== Species with missing TSS/ROC ===\n")
missing_sp <- master$accepted_taxon[is.na(master$best_TSS)]
print(missing_sp)
cat("Count:", length(missing_sp), "\n")

# Write output
write.csv(master, "<NON_NATIVE_ROOT>/publication_tables/species_model_performance_813.csv", row.names = FALSE)
cat("\n=== Written to publication_tables/species_model_performance_813.csv ===\n")
