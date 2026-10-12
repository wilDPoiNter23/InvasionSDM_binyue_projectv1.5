# Original source: spartina_id_audit_v5.R
# Original source location: ${NON_NATIVE_ROOT}/temp/spartina_id_audit_v5.R
# Original source SHA-256: 44a002d660c3ead0c9c6a5da520e7d4d1dfdbfd72caa0fb505e3b799eda63121
# Recovery: live read-only SSH, 2026-10-12
# Classification: SUPPORTING (HISTORICAL_SUPPORTING_AUDIT_LINEAGE_HELPER)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
.libPaths(c(.libPaths(), "<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu"))
SPM <- "<WORK_ROOT>/models/Spartina.alterniflora/models/1786382404"

load(file.path(SPM, "Spartina.alterniflora_allData_RUN1_GLM"))
gm <- get(setdiff(ls(), c("SPM")))
cat("GLM expl_var_names count:", length(gm@expl_var_names), "\n")
cat("ID in GLM expl_var_names:", "ID" %in% gm@expl_var_names, "\n")
cat("First 5:", paste(gm@expl_var_names[1:5], collapse=", "), "\n")

rm(list=setdiff(ls(), c("SPM")))
load(file.path(SPM, "Spartina.alterniflora_allData_RUN1_RF"))
gm <- get(setdiff(ls(), c("SPM")))
cat("\nRF expl_var_names count:", length(gm@expl_var_names), "\n")
cat("ID in RF expl_var_names:", "ID" %in% gm@expl_var_names, "\n")

rm(list=setdiff(ls(), c("SPM")))
load(file.path(SPM, "Spartina.alterniflora_allData_RUN1_MAXNET"))
gm <- get(setdiff(ls(), c("SPM")))
cat("\nMAXNET expl_var_names count:", length(gm@expl_var_names), "\n")
cat("ID in MAXNET expl_var_names:", "ID" %in% gm@expl_var_names, "\n")

# Now check the actual model slot for ID usage
rm(list=setdiff(ls(), c("SPM")))
load(file.path(SPM, "Spartina.alterniflora_allData_RUN1_GLM"))
gm <- get(setdiff(ls(), c("SPM")))
cat("\n=== GLM model slot class:", class(gm@model), "===\n")
if(inherits(gm@model, "glm")) {
  coefs <- coef(gm@model)
  cat("Number of coefficients:", length(coefs), "\n")
  cat("Has ID coefficient:", "ID" %in% names(coefs), "\n")
  cat("ID coefficient value:", coefs["ID"], "\n")
}

rm(list=setdiff(ls(), c("SPM")))
load(file.path(SPM, "Spartina.alterniflora_allData_RUN1_RF"))
gm <- get(setdiff(ls(), c("SPM")))
cat("\n=== RF model slot class:", class(gm@model)[1], "===\n")
if(inherits(gm@model, "randomForest")) {
  cat("RF importance dims:", paste(dim(gm@model$importance), collapse="x"), "\n")
  cat("ID in RF importance:", "ID" %in% rownames(gm@model$importance), "\n")
  if("ID" %in% rownames(gm@model$importance)) {
    cat("ID importance:", gm@model$importance["ID","MeanDecreaseGini"], "\n")
  }
}

rm(list=setdiff(ls(), c("SPM")))
load(file.path(SPM, "Spartina.alterniflora_allData_RUN1_MAXNET"))
gm <- get(setdiff(ls(), c("SPM")))
cat("\n=== MAXNET model slot class:", class(gm@model)[1], "===\n")
if(inherits(gm@model, "maxnet")) {
  feats <- names(coef(gm@model))
  cat("MAXNET feature count:", length(feats), "\n")
  cat("ID in MAXNET features:", "ID" %in% feats, "\n")
  if("ID" %in% feats) {
    cat("ID coefficient:", coef(gm@model)["ID"], "\n")
  }
}
