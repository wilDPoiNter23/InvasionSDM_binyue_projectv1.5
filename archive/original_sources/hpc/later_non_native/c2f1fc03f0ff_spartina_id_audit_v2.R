# Original source: spartina_id_audit_v2.R
# Original source location: ${NON_NATIVE_ROOT}/temp/spartina_id_audit_v2.R
# Original source SHA-256: c2f1fc03f0ffd6f196016497d721b7a4e65da1a3ba2060d594bdcfd765c0419e
# Recovery: live read-only SSH, 2026-10-12
# Classification: SUPPORTING (HISTORICAL_SUPPORTING_AUDIT_LINEAGE_HELPER)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
.libPaths(c(.libPaths(), "<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu"))
library(biomod2)
mo_file <- "<WORK_ROOT>/Spartina.alterniflora/Spartina.alterniflora.1786382404.models.out"
mo <- get(load(mo_file))

cat("=== expl.var.names ===\n")
print(mo@expl.var.names)
cat("ID present in expl.var.names:", "ID" %in% mo@expl.var.names, "\n")

# Get models via BIOMOD_LoadModels
cat("\n=== Loading individual models ===\n")
loaded <- BIOMOD_LoadModels(mo)
cat("Loaded models:\n")
print(loaded)

# Check first GLM
glm_idx <- grep("GLM", loaded)
if(length(glm_idx) > 0) {
  gm <- get(loaded[glm_idx[1]])
  cat("\nFirst GLM class:", class(gm)[1], "\n")
  cat("GLM formula:\n")
  print(formula(gm))
  tl <- attr(terms(gm), "term.labels")
  cat("ID in GLM terms:", "ID" %in% tl, "\n")
}

# Check first RF
rf_idx <- grep("RF", loaded)
if(length(rf_idx) > 0) {
  gm <- get(loaded[rf_idx[1]])
  cat("\nFirst RF class:", class(gm)[1], "\n")
  cat("RF predictor names:\n")
  print(head(colnames(gm$importance), 5))
  cat("ID in RF predictors:", "ID" %in% colnames(gm$importance), "\n")
}

# Check first MAXNET
mx_idx <- grep("MAXNET", loaded)
if(length(mx_idx) > 0) {
  gm <- get(loaded[mx_idx[1]])
  cat("\nFirst MAXNET class:", class(gm)[1], "\n")
  cat("MAXNET feature names:\n")
  print(head(names(coef(gm)), 5))
  cat("ID in MAXNET features:", "ID" %in% names(coef(gm)), "\n")
}

# Variable importance
cat("\n=== Variable importance ===\n")
vi <- get_variables_importance(mo)
cat("VI columns:", paste(head(colnames(vi), 5), collapse=", "), "...\n")
cat("ID in VI:", "ID" %in% colnames(vi), "\n")
if("ID" %in% colnames(vi)) {
  cat("ID mean importance:", mean(vi[,"ID"], na.rm=TRUE), "\n")
}
