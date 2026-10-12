# Original source: freeze_standard_start814.py
# Original source location: ${NON_NATIVE_ROOT}/audit/environment_only_sensitivity/freeze_standard_start814.py
# Original source SHA-256: a0af6193292f05c74a226a6714f4affcb5afd419516eba7c645953d65a7a6213
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_ENVIRONMENT_ONLY_SENSITIVITY)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
import csv,json,hashlib,time
from pathlib import Path
from production_guards import strict_inventory
from production_controller import A,FROZEN_FILES,verify_gates
rows=strict_inventory(A/'sensitivity_inventory.tsv');assert len(rows)==814 and len({r['accepted_taxon'] for r in rows})==814
assert sum(not r['clean_full_output_root'] for r in rows)==804 and sum(int(r['fitting_task_count']) for r in rows)==824
pre=list(csv.DictReader((A/'static_preflight_814.csv').open()));assert len(pre)==814 and all(r['static_pass']=='TRUE' for r in pre)
r=next(r for r in rows if r['accepted_taxon']=='Abutilon grandifolium');assert (Path(r['output_root'])/'VALIDATED.rds').exists()
author=A/'author_decision_20261009.txt';assert author.is_file() and 'AUTHOR DECISION CONFIRMED' in author.read_text()
policy=dict(decision='DEFINED_NON_NA_MEAN',author_confirmed=True,scope='814_FINAL_ANALYSIS',finite_values_only=True,denominator='nvalid_species',zero_valid_output='NA',legacy_products_unchanged=True,author_source=str(author),author_sha256=hashlib.sha256(author.read_bytes()).hexdigest(),recorded_at=time.strftime('%Y-%m-%dT%H:%M:%S'))
if (A/'aggregation_policy.json').exists():assert json.loads((A/'aggregation_policy.json').read_text())['author_sha256']==policy['author_sha256']
else:(A/'aggregation_policy.json').write_text(json.dumps(policy,indent=2))
agg=A/'aggregates_814_na_safe_v1';agg.mkdir(exist_ok=True)
for side in ['FULL','ENV']:(agg/side).mkdir(exist_ok=True)
(agg/'PRODUCT_PLAN.json').write_text(json.dumps(dict(scope='814_FINAL_ANALYSIS',status='NOT_GENERATED',definition='per pixel mean across finite non-NA species values; nvalid_species==0 -> NA',species_count=814,full_composition='804 accepted FULL + 10 new CLEAN_FULL',env_composition='804 new ENV + 10 new CLEAN_ENV',same_code_on_both_sides=True,required_products=['FULL/ensemble_mean_suitability_non_native_814.tif','FULL/nvalid_species_continuous_814.tif','ENV/ensemble_mean_suitability_non_native_814.tif','ENV/nvalid_species_continuous_814.tif','FULL/tss_binary_suitable_taxon_share_non_native_814.tif','FULL/nvalid_species_binary_814.tif','ENV/tss_binary_suitable_taxon_share_non_native_814.tif','ENV/nvalid_species_binary_814.tif']),indent=2))
mapping=json.loads((A/'E2E_ROUTE_JOBS.json').read_text())
gate=dict(scope='814_FINAL_ANALYSIS',species_count=814,fit_count=824,ordinary_route_pass=True,paired_route_pass=False,validated_roots=[r['output_root']],entrypoint_sha256={f:hashlib.sha256((A/f).read_bytes()).hexdigest() for f in FROZEN_FILES},canary_jobs={r['task_key']+'_ENV':mapping[r['accepted_taxon']]},author_standard_immediate_release=True,recorded_at=time.strftime('%Y-%m-%dT%H:%M:%S'))
assert not (A/'STANDARD_GATE_PASS.json').exists()
(A/'STANDARD_GATE_PASS.json').write_text(json.dumps(gate,indent=2));assert verify_gates()
print('STANDARD_GATE_OPEN 804; already validated1; remaining803; special paired gate separate; NA_SAFE_POLICY_FROZEN')
