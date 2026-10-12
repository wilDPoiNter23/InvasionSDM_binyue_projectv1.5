# Original source: recover_submit_e2e.py
# Original source location: ${NON_NATIVE_ROOT}/audit/environment_only_sensitivity/recover_submit_e2e.py
# Original source SHA-256: dbfbeb9ad8e46aac35625e72c02fa189204054151c2e4439eac3f256dcc72fd3
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_ENVIRONMENT_ONLY_SENSITIVITY)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
import json,time
from pathlib import Path
from production_guards import strict_inventory,write_inventory,parse_sbatch_output,frozen_submission_shell,capacity
from production_controller import queue,run
A=Path('<NON_NATIVE_ROOT>/audit/environment_only_sensitivity')
old=run(['sacct','-X','-j','9236200','-n','-P','--format=JobID,State']);assert '9236200_0|FAILED' in old and '9236200_1|FAILED' in old
q=queue();assert all(not z['id'].startswith('9236200_') for z in q);assert capacity(len(q),sum(int(z['nodes']) for z in q),sum(int(z['cpus']) for z in q))>=2
x=strict_inventory(A/'sensitivity_inventory.tsv');names=['Abutilon grandifolium','Acer platanoides'];selected=[r for r in x if r['accepted_taxon'] in names]
for r in selected:
 for d in [r['output_root'],r['clean_full_output_root']]:
  if d:assert not (Path(d)/'ACTIVE_OWNER').exists() and not (Path(d)/'models.rds').exists()
prior=A/'e2e_submission_intent.json';history=A/'e2e_launch_9236200_failure.json';assert not history.exists();history.write_text(json.dumps({'old_intent':json.loads(prior.read_text()),'accounting':old,'fingerprint':'COMPUTE_HOST_MODULE_FILE_MISSING','no_fitting_started':True,'repair':'Submit-side frozen module environment with exported loader fingerprint'},indent=2))
prior.write_text(json.dumps({'name':'s814_v2_e2e','date':time.strftime('%Y-%m-%d'),'state':'REPAIRED_PRE_SUBMIT_INTENT'},indent=2))
cmd=frozen_submission_shell()+'sbatch --export=ALL --parsable --array=0-1%2 '+str(A/'slurm_e2e814.sh')
job=parse_sbatch_output(run(['bash','-lc',cmd],timeout=60));prior.write_text(json.dumps({'name':'s814_v2_e2e','date':time.strftime('%Y-%m-%d'),'job_id':job,'state':'SUBMITTED','supersedes_launcher_only_failure':'9236200'},indent=2))
for r in selected:r['owner']='s814_v2_e2e';r['slurm_job_id']=job+('_0' if r['accepted_taxon']==names[0] else '_1');r['status']='E2E_SUBMITTED'
write_inventory(A/'sensitivity_inventory.tsv',x);print('REPAIRED_E2E_SUBMITTED',job)
