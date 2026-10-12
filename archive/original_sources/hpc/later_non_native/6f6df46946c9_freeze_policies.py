# Original source: freeze_policies.py
# Original source location: ${NON_NATIVE_ROOT}/audit/environment_only_sensitivity/freeze_policies.py
# Original source SHA-256: 6f6df46946c9e92adb473076f6f2fefd8570e8d4ea101d37a4dac6d9a6a21bae
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_ENVIRONMENT_ONLY_SENSITIVITY)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
import csv,json,hashlib,os
from pathlib import Path
A=Path('<NON_NATIVE_ROOT>/audit/environment_only_sensitivity');B=Path('<WORK_ROOT>')
rows=list(csv.DictReader((A/'sensitivity_inventory.tsv').open(),delimiter='\t'))
policies={}
for src in {r['authoritative_baseline_source'] for r in rows}:
 if 'ORIGINAL_FROZEN' in src:files=[Path('<HPC_INPUT_ROOT>/script/0401batch_modeling_config.r')];kind='ORIGINAL_COMPOSITE_BEST_ENSEMBLE';min_models=1;single=False
 elif 'V4_MULTIFOLD' in src:files=[B/'publication_release_v1/scripts/project_26_repaired_v4.r'];kind='V4_EMMEAN_BY_TSS';min_models=1;single=False
 elif 'GAP75' in src:files=[B/'publication_release_v1/scripts/project_gap75_final.r'];kind='MANUAL_EMMEAN';min_models=1;single=False
 elif 'GAP13' in src:files=[B/'publication_release_v1/scripts/repair_gap13_v2.r',B/'publication_release_v1/scripts/project_gap13_repaired.r'];kind='MANUAL_EMMEAN';min_models=1;single=False
 elif 'SPARTINA' in src:files=[B/'publication_release_v1/scripts/refit_Spartina_noID_final_v1.R'];kind='MANUAL_EMMEAN';min_models=2;single=True
 elif 'SOLANUM' in src:files=[B/'publication_release_v1/scripts/refit_Solanum_rostratum_v1.R'];kind='MANUAL_EMMEAN';min_models=2;single=True
 else:raise RuntimeError('Unknown route '+src)
 assert all(f.is_file() for f in files),files
 policies[src]={'kind':kind,'primary_TSS_ROC_strict':.8,'fallback_TSS_ROC_strict':.7,'minimum_qualified_models':min_models,'best_TSS_single_fallback':single,'evidence_files':{str(f):hashlib.sha256(f.read_bytes()).hexdigest() for f in files}}
(A/'frozen_route_policies.json').write_text(json.dumps(policies,indent=2))
print('POLICIES',len(policies))
