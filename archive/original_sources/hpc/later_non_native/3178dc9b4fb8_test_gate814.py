# Original source: test_gate814.py
# Original source location: ${NON_NATIVE_ROOT}/audit/environment_only_sensitivity/test_gate814.py
# Original source SHA-256: 3178dc9b4fb8ed0a7f36db0d3f183c61570b9193b6e069dccbd7a7c028a3b3ea
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_ENVIRONMENT_ONLY_SENSITIVITY)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
import tempfile,json,hashlib
from pathlib import Path
import production_controller as c
with tempfile.TemporaryDirectory() as scratch:
 c.A=Path(scratch)
 assert c.verify_gates() is False
 roots=[]
 for i in range(3):
  p=c.A/str(i);p.mkdir();(p/'VALIDATED.rds').write_bytes(b'receipt');roots.append(str(p))
 (c.A/'0'/'PAIRED_VALIDATED.rds').write_bytes(b'pair')
 for name in c.FROZEN_FILES:(c.A/name).write_text('frozen')
 from production_guards import write_inventory
 rows=[dict(accepted_taxon='Abutilon grandifolium',output_root=roots[0],clean_full_output_root='',clean_env_output_root=''),dict(accepted_taxon='Acer platanoides',output_root=roots[2],clean_full_output_root=roots[1],clean_env_output_root=roots[2])]+[dict(accepted_taxon='fixture '+str(i),output_root='',clean_full_output_root='',clean_env_output_root='') for i in range(812)]
 write_inventory(c.A/'sensitivity_inventory.tsv',rows)
 (c.A/'PAIRED_VALIDATED.rds').write_bytes(b'pair')
 gate=dict(scope='814_FINAL_ANALYSIS',species_count=814,fit_count=824,ordinary_route_pass=True,entrypoint_sha256={name:hashlib.sha256(b'frozen').hexdigest() for name in c.FROZEN_FILES},validated_roots=roots[:1])
 (c.A/'STANDARD_GATE_PASS.json').write_text(json.dumps(gate))
 assert c.verify_gates() is False
 (c.A/'aggregation_policy.json').write_text(json.dumps(dict(decision='DEFINED_NON_NA_MEAN',author_confirmed=True)))
 assert c.verify_gates() is True
 assert c.verify_gates('special') is False
 special=dict(gate,paired_route_pass=True,validated_roots=roots,paired_receipt=str(c.A))
 (c.A/'SPECIAL_GATE_PASS.json').write_text(json.dumps(special))
 assert c.verify_gates('special') is True
 (c.A/'run_sensitivity814.R').write_text('changed')
 try:c.verify_gates('special')
 except AssertionError:pass
 else:raise AssertionError('Changed frozen entrypoint accepted')
 (c.A/'run_sensitivity814.R').write_text('frozen')
 (c.A/'1'/'VALIDATED.rds').unlink()
 try:c.verify_gates('special')
 except AssertionError:pass
 else:raise AssertionError('Missing validated route receipt accepted')
 print('GATE814_MISSING_POLICY_HASH_DRIFT_RECEIPT_TESTS_PASS')
