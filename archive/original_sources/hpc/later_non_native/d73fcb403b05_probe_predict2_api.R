# Original source: probe_predict2_api.R
# Original source location: ${NON_NATIVE_ROOT}/audit/environment_only_sensitivity/probe_predict2_api.R
# Original source SHA-256: d73fcb403b05e3855d88245cff7afd58e07fb112cf4754d371c0d640717b4bf6
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_ENVIRONMENT_ONLY_SENSITIVITY)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
.libPaths(c('<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu',.libPaths()))
suppressPackageStartupMessages(library(biomod2));print(methods::selectMethod(biomod2:::predict2,c('MAXNET_biomod2_model','data.frame')));print(methods::selectMethod(biomod2:::predict2,c('MAXNET_biomod2_model','SpatRaster')))