# Original source: snapshot814.py
# Original source location: ${NON_NATIVE_ROOT}/audit/environment_only_sensitivity/snapshot814.py
# Original source SHA-256: 2e87aa7ce0ca4359aafcfe2620bd963dab645070f221df914164850df84357dc
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_ENVIRONMENT_ONLY_SENSITIVITY)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
import json,subprocess,time
from pathlib import Path
from production_guards import strict_inventory,write_inventory
A=Path('<NON_NATIVE_ROOT>/audit/environment_only_sensitivity')
rows=strict_inventory(A/'sensitivity_inventory.tsv')
raw=subprocess.run(['squeue','-r','-h','-A','HPC_ACCOUNT','-o','%i|%T|%D|%C|%j'],check=True,text=True,stdout=subprocess.PIPE).stdout
queue=[dict(zip(['job','state','nodes','cpus','name'],z.split('|'))) for z in raw.splitlines() if z];active={z['job']:z['state'] for z in queue}
jobs={'Abutilon grandifolium':'9236220_0','Acer platanoides':'9236220_1'}
if (A/'E2E_ROUTE_JOBS.json').exists():jobs=json.loads((A/'E2E_ROUTE_JOBS.json').read_text())
for r in rows:
 for k in ['env_fit_status','env_job_id','env_owner','clean_full_fit_status','clean_full_job_id','clean_full_owner']:r.setdefault(k,'')
 if r['accepted_taxon'] not in jobs:
  r['status']='READY_PREFLIGHT_PASSED';continue
 job=jobs[r['accepted_taxon']];root=Path(r['output_root']);special=bool(r['clean_full_output_root'])
 r['slurm_job_id']=job;r['owner']='s814_v2_e2e' if job in active else ''
 for prefix,path in [('env',root)]+([('clean_full',Path(r['clean_full_output_root']))] if special else []):
  status='TECHNICALLY_VALIDATED' if (path/'VALIDATED.rds').exists() else 'FIT_DESIGN_VALIDATED_QC_PENDING' if (path/'FIT_VALIDATED.rds').exists() else 'NOT_STARTED_WAITING_CLEAN_FULL' if special and prefix=='env' and not path.exists() else active.get(job,'REVIEW_REQUIRED')
  r[prefix+'_fit_status']=status;r[prefix+'_job_id']=job;r[prefix+'_owner']=r['owner']
  if prefix=='env' and (path/'FIT_VALIDATED.rds').exists():
   r['env_model_status']='FIT_DESIGN_VALIDATED';r['env_evaluation_status']='GENERATED';r['env_selection_status']='GENERATED' if (path/'selection.csv').exists() else 'PENDING';r['env_projection_status']='TECHNICALLY_VALIDATED' if (path/'VALIDATED.rds').exists() else 'GENERATED_PENDING_QC' if (path/'ensemble_suitability.tif').exists() else 'IN_PROGRESS'
 if (root/'VALIDATED.rds').exists() and not special:r['status']='E2E_TECHNICALLY_VALIDATED';r['env_state']='TECHNICALLY_VALIDATED'
 elif special and (Path(r['clean_full_output_root']).parent/'PAIRED_VALIDATED.rds').exists():r['status']='E2E_PAIRED_TECHNICALLY_VALIDATED'
 else:r['status']='E2E_'+active.get(job,'RECEIPT_REVIEW_REQUIRED')
write_inventory(A/'sensitivity_inventory.tsv',rows)
summary=dict(scope='814_FINAL_ANALYSIS',timestamp=time.strftime('%Y-%m-%dT%H:%M:%S'),species_count=len(rows),unique_species=len({r['accepted_taxon'] for r in rows}),ordinary_species=sum(not r['clean_full_output_root'] for r in rows),special_species=sum(bool(r['clean_full_output_root']) for r in rows),planned_species_level_fits=sum(int(r['fitting_task_count']) for r in rows),static_preflight='814_PASS_0_FAIL',canary_jobs=jobs,account_queue=queue,charged_nodes=sum(int(z['nodes']) for z in queue),charged_cpus=sum(int(z['cpus']) for z in queue),variant_validation_receipts=len(list((A/'production_v2').glob('*/*/VALIDATED.rds'))),paired_validation_receipts=len(list((A/'production_v2').glob('*/PAIRED_VALIDATED.rds'))),bulk_gate='CLOSED_PENDING_E2E_AND_AUTHOR_AGGREGATION_DECISION',bulk_submitted=False)
(A/'production_status_snapshot.json').write_text(json.dumps(summary,indent=2));print(json.dumps(summary,indent=2))
