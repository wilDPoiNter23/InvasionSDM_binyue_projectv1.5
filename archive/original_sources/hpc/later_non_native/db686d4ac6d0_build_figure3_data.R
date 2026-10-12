# Original source: build_figure3_data.R
# Original source location: ${NON_NATIVE_ROOT}/temp/build_figure3_data.R
# Original source SHA-256: db686d4ac6d019bc5282a6c480798ccdae2f0a8ad4c5e22f4b92731971997bcf
# Recovery: live read-only SSH, 2026-10-12
# Classification: SUPPORTING (HISTORICAL_SUPPORTING_AUDIT_LINEAGE_HELPER)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
.libPaths(c("<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu", .libPaths()))

# ============================================================================
# Figure 3 — Model Technical Validation (813 taxa, final)
# ============================================================================

SD <- "<NON_NATIVE_ROOT>"
perf <- read.csv(file.path(SD, "publication_tables/species_model_performance_813_final.csv"), stringsAsFactors=FALSE)

# Prepare Figure 3 data
fig3 <- data.frame(
  accepted_taxon = perf$accepted_taxon,
  best_TSS = perf$best_TSS,
  best_ROC = perf$best_ROC,
  selection_tier = perf$selection_tier,
  selected_model_count = perf$selected_model_count,
  source_generation = perf$source_generation,
  stringsAsFactors = FALSE
)

dir.create(file.path(SD, "publication_figures"), showWarnings=FALSE)
write.csv(fig3, file.path(SD, "publication_figures/figure3_model_validation_data_v2.csv"), row.names=FALSE)

# Algorithm frequency (from selected model names in manifest)
mani <- read.csv("<WORK_ROOT>/publication_release_v1/manifests/final_accepted_projection_manifest_813_v3.csv", stringsAsFactors=FALSE)
algo_freq <- data.frame(
  algorithm = c("GLM","RF","MAXNET"),
  species_count = c(
    sum(grepl("GLM", mani$selected_model_names)),
    sum(grepl("RF", mani$selected_model_names)),
    sum(grepl("MAXNET", mani$selected_model_names))
  )
)
algo_freq$species_count[1] <- sum(grepl("GLM", mani$selected_model_names))
write.csv(algo_freq, file.path(SD, "publication_figures/figure3_algorithm_frequency.csv"), row.names=FALSE)

# Summary statistics for Figure 3 caption
tss <- fig3$best_TSS[!is.na(fig3$best_TSS)]
roc <- fig3$best_ROC[!is.na(fig3$best_ROC)]
cat(sprintf("TSS: N=%d mean=%.3f median=%.3f IQR=%.3f-%.3f\n", length(tss), mean(tss), median(tss), quantile(tss,0.25), quantile(tss,0.75)))
cat(sprintf("ROC: N=%d mean=%.3f median=%.3f IQR=%.3f-%.3f\n", length(roc), mean(roc), median(roc), quantile(roc,0.25), quantile(roc,0.75)))
cat("Tier distribution:\n")
print(table(fig3$selection_tier))
cat("Algorithm frequency:\n")
print(algo_freq)
cat("Figure 3 data written.\n")
