# Original source: spartina_id_audit_v3.R
# Original source location: ${NON_NATIVE_ROOT}/temp/spartina_id_audit_v3.R
# Original source SHA-256: 4516f7d31930fa73eea3062e6719ea6df875c344d022a5187197041682eab0d6
# Recovery: live read-only SSH, 2026-10-12
# Classification: SUPPORTING (HISTORICAL_SUPPORTING_AUDIT_LINEAGE_HELPER)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
.libPaths(c(.libPaths(), "<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu"))
SPM <- "<WORK_ROOT>/models/Spartina.alterniflora/models/1786382404"

# Load and check GLM model
glm_file <- file.path(SPM, "Spartina.alterniflora_allData_RUN1_GLM")
load(glm_file)
gm <- get(ls()[ls() != "glm_file" & ls() != "SPM"])
cat("GLM class:", class(gm)[1], "\n")
cat("GLM formula:\n")
print(formula(gm))
tl <- attr(terms(gm), "term.labels")
cat("Total terms:", length(tl), "\n")
cat("ID in terms:", "ID" %in% tl, "\n")
cat("First 5 terms:", paste(tl[1:min(5,length(tl))], collapse=", "), "\n")

# Load and check RF model
rm(list=setdiff(ls(), c("SPM")))
rf_file <- file.path(SPM, "Spartina.alterniflora_allData_RUN1_RF")
load(rf_file)
gm <- get(ls()[!ls() %in% c("SPM", "glm_file", "rf_file")])
cat("\nRF class:", class(gm)[1], "\n")
if(inherits(gm, "randomForest")) {
  cat("RF pred names count:", length(colnames(gm$importance)), "\n")
  cat("ID in RF:", "ID" %in% colnames(gm$importance), "\n")
  cat("First 5 preds:", paste(colnames(gm$importance)[1:min(5,ncol(gm$importance))], collapse=", "), "\n")
}

# Load and check MAXNET model
rm(list=setdiff(ls(), c("SPM")))
mx_file <- file.path(SPM, "Spartina.alterniflora_allData_RUN1_MAXNET")
load(mx_file)
gm <- get(ls()[!ls() %in% c("SPM", "glm_file", "rf_file", "mx_file")])
cat("\nMAXNET class:", class(gm)[1], "\n")
if(inherits(gm, "maxnet")) {
  feats <- names(coef(gm))
  cat("MAXNET features count:", length(feats), "\n")
  cat("ID in MAXNET:", "ID" %in% feats, "\n")
  cat("First 5 features:", paste(feats[1:min(5,length(feats))], collapse=", "), "\n")
}
