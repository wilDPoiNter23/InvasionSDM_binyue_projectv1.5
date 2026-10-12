# Original source: resume_e2e_user_options.py
# Original source location: ${NON_NATIVE_ROOT}/audit/environment_only_sensitivity/resume_e2e_user_options.py
# Original source SHA-256: fad7d680e71dce113326d89893c341d1281f97b5a5feea4cfad3de85d1fd24f4
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_ENVIRONMENT_ONLY_SENSITIVITY)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
import json,time
from pathlib import Path
from production_guards import strict_inventory,write_inventory,parse_sbatch_output,frozen_submission_shell,capacity
from production_controller import queue,run
from static_contract import verify,digest
A=Path('<NON_NATIVE_ROOT>/audit/environment_only_sensitivity')
q=queue();assert all(not z['id'].startswith('9236205_') for z in q);acct=run(['sacct','-X','-j','9236205','-n','-P','--format=JobID,State']);assert '9236205_0|FAILED' in acct and '9236205_1|FAILED' in acct
verify('Abutilon grandifolium');verify('Acer platanoides');x=strict_inventory(A/'sensitivity_inventory.tsv')
for r in x:r.setdefault('rejected_env_attempt_path','')
changes=[]
for r in x:
 if r['accepted_taxon'] not in ['Abutilon grandifolium','Acer platanoides']:continue
 for d in [r['output_root'],r['clean_full_output_root']]:
  if not d:continue
  lock=Path(d)/'ACTIVE_OWNER'
  if lock.exists():
   owner=(lock/'owner.txt').read_text().splitlines();assert owner[0]==r['accepted_taxon'];(lock/'owner.txt').replace(Path(d)/'owner_failed_9236205.txt');lock.rmdir()
 if r['accepted_taxon']=='Abutilon grandifolium':
  old=r['output_root'];r['rejected_env_attempt_path']=old;r['output_root']=str(Path(old).parent/'ENV_user_options_v3')
  assert not Path(r['output_root']).exists();r['output_model_path']=r['output_root']+'/models.rds';r['output_evaluation_path']=r['output_root']+'/evaluation.csv';r['output_projection_path']=r['output_root']+'/ensemble_suitability.tif'
  (Path(old)/'STATUS.txt').write_text('REJECTED_UNACCEPTED_DEFAULT_OPTIONS_OVERRIDE\n');changes.append({'taxon':r['accepted_taxon'],'old_unaccepted_root':old,'new_root':r['output_root'],'reason':'Explicit OPT.user is required; deprecated bm.options was ignored under default strategy'})
 r['owner']='';r['status']='E2E_GUARD_REPAIRED_PENDING_RESUME'
write_inventory(A/'sensitivity_inventory.tsv',x);c=json.loads((A/'static_preflight_contract.json').read_text());r=next(r for r in x if r['accepted_taxon']=='Abutilon grandifolium');c['contracts'][r['accepted_taxon']]['digest']=digest(r);(A/'static_preflight_contract.json').write_text(json.dumps(c,indent=2));(A/'output_root_reconciliation_user_options_v3.json').write_text(json.dumps(changes,indent=2))
assert capacity(len(q),sum(int(z['nodes']) for z in q),sum(int(z['cpus']) for z in q))>=2
prior=A/'e2e_submission_intent.json';(A/'e2e_launch_9236205_failure.json').write_text(json.dumps({'previous_intent':json.loads(prior.read_text()),'accounting':acct,'ordinary':'OPTIONS_OVERRIDE_REJECTED_REQUIRES_NEW_FIT','paired':'CLEAN_FULL_PARAMETERS_EQUIVALENT_REUSE_SAVED_FIT','guard':'Explicit OPT.strategy=user.defined and OPT.user'},indent=2));prior.write_text(json.dumps({'name':'s814_v2_e2e','date':time.strftime('%Y-%m-%d'),'state':'USER_OPTIONS_REPAIRED_PRE_SUBMIT_INTENT'},indent=2))
cmd=frozen_submission_shell()+'sbatch --export=ALL --parsable --array=0-1%2 '+str(A/'slurm_e2e814.sh');job=parse_sbatch_output(run(['bash','-lc',cmd],timeout=60));prior.write_text(json.dumps({'name':'s814_v2_e2e','date':time.strftime('%Y-%m-%d'),'job_id':job,'state':'SUBMITTED','resumes_routes_from':'9236205'},indent=2))
for r in x:
 if r['accepted_taxon'] in ['Abutilon grandifolium','Acer platanoides']:r['owner']='s814_v2_e2e';r['slurm_job_id']=job+('_0' if r['accepted_taxon']=='Abutilon grandifolium' else '_1');r['status']='E2E_SUBMITTED'
write_inventory(A/'sensitivity_inventory.tsv',x);print('USER_OPTIONS_REPAIRED_E2E_SUBMITTED',job)
