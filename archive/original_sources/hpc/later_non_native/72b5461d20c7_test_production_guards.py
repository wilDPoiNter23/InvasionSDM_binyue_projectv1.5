# Original source: test_production_guards.py
# Original source location: ${NON_NATIVE_ROOT}/audit/environment_only_sensitivity/test_production_guards.py
# Original source SHA-256: 72b5461d20c703d4f49b43ef7be67f84994a34f59a11bb55907a92a32137cb4a
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_ENVIRONMENT_ONLY_SENSITIVITY)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
from pathlib import Path
from production_guards import strict_inventory, capacity
p=Path('bad_inventory_fixture.tsv');p.write_bytes(b'a\tb\r\n1\t2\r\n')
try:
 strict_inventory(p)
 raise AssertionError('CRLF accepted')
except ValueError: pass
p.write_bytes(b'a\tb\n1\t"2"\n')
try:
 strict_inventory(p)
 raise AssertionError('quotes accepted')
except ValueError: pass
p.write_bytes(b'a\tb\n1\t2\t3\n')
try:
 strict_inventory(p)
 raise AssertionError('wrong field count accepted')
except ValueError: pass
assert capacity(submitted=29,nodes=18,cpus=1152)==1
assert capacity(submitted=12,nodes=20,cpus=1280)==0
assert capacity(submitted=8,nodes=3,cpus=192)==17
p.unlink()
print('PRODUCTION_GUARD_TESTS_PASS')
