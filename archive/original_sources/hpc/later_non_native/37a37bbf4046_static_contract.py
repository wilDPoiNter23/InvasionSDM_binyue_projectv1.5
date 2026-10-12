# Original source: static_contract.py
# Original source location: ${NON_NATIVE_ROOT}/audit/environment_only_sensitivity/static_contract.py
# Original source SHA-256: 37a37bbf404606ef07e03ca96768dda1d895720ba989f35fc4f6ef16fe0a872a
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_ENVIRONMENT_ONLY_SENSITIVITY)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
import csv,hashlib,json,sys
from pathlib import Path
from production_guards import strict_inventory
A=Path('<NON_NATIVE_ROOT>/audit/environment_only_sensitivity')
FIELDS=['task_key','accepted_taxon','scope','variant','authoritative_baseline_source','baseline_model_path','baseline_predictor_vector','environmental_predictor_vector','human_exclusion_set','formatted_input_path','calibration_path','algorithm_route','model_options_path','baseline_evaluation_artifact_path','baseline_global_projection_path','baseline_analysis_projection_path','output_root','clean_full_predictor_vector','clean_full_output_root','clean_env_predictor_vector','clean_env_output_root']
def sha(p):
 h=hashlib.sha256()
 with Path(p).open('rb') as f:
  for block in iter(lambda:f.read(1024*1024),b''):h.update(block)
 return h.hexdigest()
def raster_fingerprint(path):
 p=Path(path);components=[p]
 if p.suffix=='.grd':components.append(p.with_suffix('.gri'))
 for sidecar in [Path(str(p)+'.aux.xml')]:
  if sidecar.exists():components.append(sidecar)
 out={}
 for c in components:
  st=c.stat();h=hashlib.sha256()
  with c.open('rb') as f:
   for offset in sorted({0,max(0,st.st_size//2-65536),max(0,st.st_size-131072)}):
    f.seek(offset);h.update(f.read(131072))
  out[str(c)]={'size':st.st_size,'mtime_ns':st.st_mtime_ns,'sample_payload_sha256':h.hexdigest()}
 return out

def stack_source(r):
 b='<WORK_ROOT>';src=r['authoritative_baseline_source']
 if 'V4_MULTIFOLD' in src or 'DIRECT34' in src:return b+'/data/predictors/full_stack_terrestrial_primary_v1.tif'
 if any(k in src for k in ['GAP13','VIIRS35','SPARTINA','SOLANUM']):return b+'/data/predictors/full_stack_terrestrial_plus_VIIRS_v1.tif'
 return '<HPC_INPUT_ROOT>/climate/full_stack.tif'

def digest(r):return hashlib.sha256(json.dumps({k:r[k] for k in FIELDS},sort_keys=True).encode()).hexdigest()
def seal():
 rows=strict_inventory(A/'sensitivity_inventory.tsv');pre=list(csv.DictReader((A/'static_preflight_814.csv').open()));assert len(pre)==814 and all(z['static_pass']=='TRUE' for z in pre);assert {z['accepted_taxon'] for z in pre}=={r['accepted_taxon'] for r in rows}
 evidence={};contracts={}
 for r in rows:
  hashes={}
  for k in ['baseline_model_path','formatted_input_path','calibration_path','model_options_path','baseline_evaluation_artifact_path']:
   p=r[k]
   if p=='IN_MODEL_OBJECT':continue
   if p not in evidence:evidence[p]=sha(p)
   hashes[p]=evidence[p]
  iv=str(Path(r['formatted_input_path']).parent/'variables.importance')
  if Path(iv).is_file():hashes[iv]=sha(iv)
  projection=raster_fingerprint(r['baseline_global_projection_path']);stack=stack_source(r)
  contracts[r['accepted_taxon']]={'digest':digest(r),'artifacts':hashes,'domain_components':projection,'predictor_stack':stack,'predictor_components':raster_fingerprint(stack)}
 obj={'species_count':814,'fitting_tasks':824,'contracts':contracts,'policy_sha256':sha(A/'frozen_route_policies.json'),'preflight_sha256':sha(A/'static_preflight_814.csv')}
 (A/'static_preflight_contract.json').write_text(json.dumps(obj,indent=2));print('STATIC_CONTRACT_SEALED:814/824')
def verify(taxon):
 c=json.loads((A/'static_preflight_contract.json').read_text());rows=strict_inventory(A/'sensitivity_inventory.tsv');r=next(z for z in rows if z['accepted_taxon']==taxon);z=c['contracts'][taxon];assert digest(r)==z['digest'];assert sha(A/'frozen_route_policies.json')==c['policy_sha256'];assert sha(A/'static_preflight_814.csv')==c['preflight_sha256']
 for p,v in z['artifacts'].items():assert sha(p)==v,(p,'artifact changed since preflight')
 assert raster_fingerprint(r['baseline_global_projection_path'])==z['domain_components']
 assert raster_fingerprint(z['predictor_stack'])==z['predictor_components']
 assert stack_source(r)==z['predictor_stack']
 policies=json.loads((A/'frozen_route_policies.json').read_text())
 for p,h in policies[r['authoritative_baseline_source']]['evidence_files'].items():assert sha(p)==h
 print('ARTIFACT_CONTRACT_VERIFIED')
if __name__=='__main__':seal() if sys.argv[1]=='seal' else verify(sys.argv[2])
