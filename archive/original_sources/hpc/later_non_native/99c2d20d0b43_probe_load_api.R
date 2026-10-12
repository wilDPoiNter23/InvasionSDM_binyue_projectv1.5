# Original source: probe_load_api.R
# Original source location: ${NON_NATIVE_ROOT}/audit/environment_only_sensitivity/probe_load_api.R
# Original source SHA-256: 99c2d20d0b4390feb441d2f307083d3c17b2a60b2e4722559311cb4c8a6bf5e9
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_ENVIRONMENT_ONLY_SENSITIVITY)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
.libPaths(c('<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu',.libPaths()))
suppressPackageStartupMessages(library(biomod2));print(args(BIOMOD_LoadModels));print(BIOMOD_LoadModels)