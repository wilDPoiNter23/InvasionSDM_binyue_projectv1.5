# Original source: spartina_id_audit.R
# Original source location: ${NON_NATIVE_ROOT}/temp/spartina_id_audit.R
# Original source SHA-256: ebe4ae35256586b99d75af2298ae041ee02763ee59517e47d4e21c1a0e1cc91d
# Recovery: live read-only SSH, 2026-10-12
# Classification: SUPPORTING (HISTORICAL_SUPPORTING_AUDIT_LINEAGE_HELPER)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
.libPaths(c(.libPaths(), "<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu"))
library(biomod2)
mo_file <- "<WORK_ROOT>/Spartina.alterniflora/Spartina.alterniflora.1786382404.models.out"
mo <- get(load(mo_file))

glm_names <- mo@models.computed[grep("GLM", mo@models.computed)]
rf_names <- mo@models.computed[grep("RF", mo@models.computed)]
mx_names <- mo@models.computed[grep("MAXNET", mo@models.computed)]

cat("--- GLM: ID in formula ---\n")
for(m in glm_names) {
  gm <- get(m)
  has_id <- "ID" %in% attr(terms(gm), "term.labels")
  cat(if(has_id) "!!!USED " else "OK  ", m, "\n")
}

cat("--- RF: ID in predictor names ---\n")
for(m in rf_names) {
  gm <- get(m)
  has_id <- "ID" %in% colnames(gm$importance)
  cat(if(has_id) "!!!USED " else "OK  ", m, "\n")
}

cat("--- MAXNET: ID in feature names ---\n")
for(m in mx_names) {
  gm <- get(m)
  has_id <- "ID" %in% names(coef(gm))
  cat(if(has_id) "!!!USED " else "OK  ", m, "\n")
}

cat("--- Variable importance ---\n")
vi <- get_variables_importance(mo)
id_in_vi <- "ID" %in% colnames(vi)
cat("ID in VI:", id_in_vi, "\n")
if(id_in_vi) {
  cat("ID mean importance:", mean(vi[,"ID"], na.rm=TRUE), "\n")
  cat("ID VI values:\n")
  print(summary(vi[,"ID"]))
}
