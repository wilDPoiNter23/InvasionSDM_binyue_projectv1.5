# Original source: fast_13sp_audit.r
# Original source location: ${WORK_ROOT}/archive/v1_deprecated/fast_13sp_audit.r
# Original source SHA-256: ccb3ca0c029f607c402013674b7e899dd6e09f7ed8e4c2bdeb3e6e1b164d0802
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# Fast 13-species audit — uses formatted BIOMOD data, no terra extraction
.libPaths(c("<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu", .libPaths()))
suppressMessages(library(biomod2))

species_13 <- c(
  "Adiantum latifolium", "Ageratum houstonianum", "Ailanthus altissima",
  "Cenchrus echinatus", "Diodia teres", "Geranium dissectum",
  "Glebionis coronaria", "Hibiscus trionum", "Tamarix aphylla",
  "Veronica arvensis", "Urochloa eminii", "Musa ornata", "Vicia eriocarpa"
)

base_dir <- "<WORK_ROOT>"
out <- file(file.path(base_dir, "audit/13_species_occurrence_retention_terrestrial_stack.csv"), "w")
cat("species,raw_records,valid_coords,has_HYCOM_columns,hycom_cols,total_rows,complete_with_HYCOM,HYCOM_NA_rows,complete_without_HYCOM,rows_recovered\n", file=out)

for (sp in species_13) {
  cat(sprintf("%s: ", sp))
  fmtd_path <- file.path(base_dir, "data/species_formatted", paste0(sp, "_formatted.rds"))
  
  if (!file.exists(fmtd_path)) {
    cat("NO_DATA\n")
    cat(sprintf("%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n", sp,NA,NA,NA,NA,NA,NA,NA,NA,NA), file=out)
    next
  }
  
  fmtd <- readRDS(fmtd_path)
  env <- fmtd@data.env.var  # species-specific env variables (subset of 40)
  coord <- fmtd@coord
  n_raw <- nrow(coord)
  
  valid <- !is.na(coord[,1]) & !is.na(coord[,2]) &
           coord[,1] >= -180 & coord[,1] <= 180 &
           coord[,2] >= -90 & coord[,2] <= 90
  n_valid <- sum(valid)
  
  hycom_cols <- grep("salinity|temperature", colnames(env), value=TRUE, ignore.case=TRUE)
  has_hycom <- length(hycom_cols) > 0
  hycom_str <- if(has_hycom) paste(hycom_cols, collapse="; ") else "none"
  
  n_cc_full <- sum(complete.cases(env))
  
  if (has_hycom) {
    hycom_na <- rowSums(is.na(env[, hycom_cols, drop=FALSE])) > 0
    non_hycom_na <- rowSums(is.na(env[, !colnames(env) %in% hycom_cols, drop=FALSE])) > 0
    n_hycom_na <- sum(hycom_na)
    n_cc_without <- sum(!non_hycom_na)
    recovered <- n_cc_without - n_cc_full
  } else {
    n_hycom_na <- 0
    n_cc_without <- n_cc_full
    recovered <- 0
  }
  
  cat(sprintf("total=%d cc=%d noHYCOM=%d rec=%d cols=%s\n", nrow(env), n_cc_full, n_cc_without, recovered, hycom_str))
  cat(sprintf("%s,%d,%d,%s,%s,%d,%d,%d,%d,%d\n", sp, n_raw, n_valid, has_hycom, hycom_str,
    nrow(env), n_cc_full, n_hycom_na, n_cc_without, recovered), file=out)
}

close(out)
cat("\nDone: audit/13_species_occurrence_retention_terrestrial_stack.csv\n")
