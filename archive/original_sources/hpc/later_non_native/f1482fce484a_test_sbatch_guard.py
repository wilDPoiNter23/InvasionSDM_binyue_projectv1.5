# Original source: test_sbatch_guard.py
# Original source location: ${NON_NATIVE_ROOT}/audit/environment_only_sensitivity/test_sbatch_guard.py
# Original source SHA-256: f1482fce484a83b21ab89b55de37e50666b7fb90db5153610a3c6f114ca356f5
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_ENVIRONMENT_ONLY_SENSITIVITY)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
from production_guards import parse_sbatch_output
assert parse_sbatch_output('module status\n9236200\n')=='9236200'
assert parse_sbatch_output('login note\n9236200;cluster0\n')=='9236200'
for bad in ['module status\n','9236200\n9236201\n']:
 try:parse_sbatch_output(bad);raise AssertionError('ambiguous output accepted')
 except ValueError:pass
print('SBATCH_PARSE_GUARD_PASS')
