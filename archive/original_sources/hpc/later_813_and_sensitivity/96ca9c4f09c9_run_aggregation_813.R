# Original source: run_aggregation_813.R
# Original source location: ${WORK_ROOT}/archive/v1_deprecated/run_aggregation_813.R
# Original source SHA-256: 96ca9c4f09c9a1f87f9a495e5875b19f49c76756e0cec17b9bb4caf562e20499
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/usr/bin/env Rscript
# Wrapper for 0405Allstack.r that sets correct library path
renv_lib <- "<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu"
.libPaths(c(renv_lib, .libPaths()))

cat("R library paths:\n")
for (p in .libPaths()) cat("  ", p, "\n")

# Run the original aggregation script
source("<HPC_INPUT_ROOT>/script/0405Allstack.r")
