# Original source: spartina_id_audit_v4.R
# Original source location: ${NON_NATIVE_ROOT}/temp/spartina_id_audit_v4.R
# Original source SHA-256: 9b3df1e87202b714805487c0e28dbbbe599817ab8210274dac6d19cf896f649d
# Recovery: live read-only SSH, 2026-10-12
# Classification: SUPPORTING (HISTORICAL_SUPPORTING_AUDIT_LINEAGE_HELPER)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
.libPaths(c(.libPaths(), "<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu"))
SPM <- "<WORK_ROOT>/models/Spartina.alterniflora/models/1786382404"

# Quick check: does ID appear in the raw model files?
cat("=== Grep for ID in model files ===\n")
system(sprintf('grep -l "ID" %s/Spartina.alterniflora_allData_RUN1_GLM 2>/dev/null && echo "ID found in GLM file" || echo "ID NOT in GLM file"', SPM))
system(sprintf('grep -l "ID" %s/Spartina.alterniflora_allData_RUN1_RF 2>/dev/null && echo "ID found in RF file" || echo "ID NOT in RF file"', SPM))
system(sprintf('grep -l "ID" %s/Spartina.alterniflora_allData_RUN1_MAXNET 2>/dev/null && echo "ID found in MAXNET file" || echo "ID NOT in MAXNET file"', SPM))

# Try to load and use S4 methods
load(file.path(SPM, "Spartina.alterniflora_allData_RUN1_GLM"))
objname <- setdiff(ls(), c("SPM"))
gm <- get(objname)
cat("\n=== GLM S4 class:", class(gm)[1], "===\n")
cat("Slots:", paste(slotNames(gm), collapse=", "), "\n")
if("formula" %in% slotNames(gm)) {
  f <- gm@formula
  cat("Formula:", deparse(f), "\n")
  cat("ID in formula terms:", "ID" %in% attr(terms(f), "term.labels"), "\n")
}

rm(list=setdiff(ls(), c("SPM")))
load(file.path(SPM, "Spartina.alterniflora_allData_RUN1_RF"))
objname <- setdiff(ls(), c("SPM"))
gm <- get(objname)
cat("\n=== RF S4 class:", class(gm)[1], "===\n")
cat("Slots:", paste(slotNames(gm), collapse=", "), "\n")
# For RF it might have xNames or similar
if("xNames" %in% slotNames(gm)) {
  cat("xNames:", paste(gm@xNames[1:min(5,length(gm@xNames))], collapse=", "), "\n")
  cat("ID in xNames:", "ID" %in% gm@xNames, "\n")
}
# Also check the underlying randomForest object
if(".Model" %in% slotNames(gm)) {
  rf <- gm@.Model
  cat("Underlying RF class:", class(rf)[1], "\n")
  if(inherits(rf, "randomForest")) {
    cat("RF nvars:", ncol(rf$importance), "\n")
    cat("ID in RF:", "ID" %in% colnames(rf$importance), "\n")
  }
}

rm(list=setdiff(ls(), c("SPM")))
load(file.path(SPM, "Spartina.alterniflora_allData_RUN1_MAXNET"))
objname <- setdiff(ls(), c("SPM"))
gm <- get(objname)
cat("\n=== MAXNET S4 class:", class(gm)[1], "===\n")
cat("Slots:", paste(slotNames(gm), collapse=", "), "\n")
