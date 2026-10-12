# Original source: start_finish_e2e814.sh
# Original source location: ${NON_NATIVE_ROOT}/audit/environment_only_sensitivity/start_finish_e2e814.sh
# Original source SHA-256: 46e7bdaf6360a6f28bcbcca5aec45e4bb824e8c97266454164ab2297cb6bca91
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_ENVIRONMENT_ONLY_SENSITIVITY)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/bin/bash
set -euo pipefail
source /etc/profile.d/modules.sh_20220105
module add loginnode
export R_LIBS_USER=<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu
export R_LIBS_SITE="$R_LIBS_USER" R_LIBS="$R_LIBS_USER"
audit=<NON_NATIVE_ROOT>/audit/environment_only_sensitivity
nohup python3 "$audit/finish_e2e814.py" > "$audit/logs/e2e_finalizer.log" 2>&1 < /dev/null &
echo "E2E_FINALIZER_PID=$!"
