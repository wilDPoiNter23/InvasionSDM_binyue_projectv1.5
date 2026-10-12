# Original source: start_production814.sh
# Original source location: ${NON_NATIVE_ROOT}/audit/environment_only_sensitivity/start_production814.sh
# Original source SHA-256: 136db0c93f28a236e8fd6a95820efbc35bc293a670b1e07f7458234fcb2f045b
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
[[ ! -d "$audit/CONTROLLER_OWNER" ]]
nohup python3 "$audit/production_controller.py" run >> "$audit/logs/production_controller.log" 2>&1 < /dev/null &
echo "PRODUCTION_CONTROLLER_PID=$!"
