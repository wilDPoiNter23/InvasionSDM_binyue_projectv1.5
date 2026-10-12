# Original source: read_recovery_metadata_v2.R
# Original source location: ${NON_NATIVE_ROOT}/audit/environment_only_sensitivity/review_triage/read_recovery_metadata_v2.R
# Original source SHA-256: 0127b756f1341c755e90418901ef0e02a3dee4972a949a18f79c2222010a2972
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_ENVIRONMENT_ONLY_SENSITIVITY)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
x<-read.delim(commandArgs(TRUE)[1],stringsAsFactors=FALSE); for(i in seq_len(nrow(x))){p<-file.path(x$root[i],"writer_contract.rds"); if(file.exists(p)){w<-readRDS(p); cat(x$key[i],as.character(w$chosen_ensemble),sep="\t");cat("\n")}}