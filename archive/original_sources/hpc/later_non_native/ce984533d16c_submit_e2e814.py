# Original source: submit_e2e814.py
# Original source location: ${NON_NATIVE_ROOT}/audit/environment_only_sensitivity/submit_e2e814.py
# Original source SHA-256: ce984533d16cedc3a95d79fcf4142501916b032b687fb4951ba60f68c430685a
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_ENVIRONMENT_ONLY_SENSITIVITY)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
from pathlib import Path
import json,csv,hashlib,time
from production_guards import strict_inventory,capacity,write_inventory
from production_controller import queue,run
A=Path('<NON_NATIVE_ROOT>/audit/environment_only_sensitivity')
assert (A/'static_preflight_contract.json').exists()
x=strict_inventory(A/'sensitivity_inventory.tsv');names=['Abutilon grandifolium','Acer platanoides'];selected=[r for r in x if r['accepted_taxon'] in names];assert len(selected)==2
for r in selected:
 assert not r['owner'] and not r['slurm_job_id']
 for d in [r['output_root'],r['clean_full_output_root']]:
  if d:assert not (Path(d)/'ACTIVE_OWNER').exists() and not (Path(d)/'VALIDATED.rds').exists()
q=queue();assert capacity(len(q),sum(int(z['nodes']) for z in q),sum(int(z['cpus']) for z in q))>=2
intent=A/'e2e_submission_intent.json';assert not intent.exists(),'Reconcile prior submission intent before submitting'
intent.write_text(json.dumps({'name':'s814_v2_e2e','date':time.strftime('%Y-%m-%d'),'state':'PRE_SUBMIT_INTENT'},indent=2))
cmd='source /etc/profile.d/modules.sh_20220105; module add loginnode; export R_LIBS_USER=<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu; export R_LIBS_SITE="$R_LIBS_USER" R_LIBS="$R_LIBS_USER"; sbatch --export=ALL --parsable --array=0-1%2 '+str(A/'slurm_e2e814.sh')
job=run(['bash','-lc',cmd],timeout=60).split(';')[0];assert job.isdigit();intent.write_text(json.dumps({'name':'s814_v2_e2e','job_id':job,'state':'SUBMITTED'},indent=2))
for r in selected:
 r['owner']='s814_v2_e2e';r['slurm_job_id']=job+('_0' if r['accepted_taxon']==names[0] else '_1');r['status']='E2E_SUBMITTED'
write_inventory(A/'sensitivity_inventory.tsv',x)
print('E2E_SUBMITTED',job,'ordinary='+job+'_0','paired='+job+'_1','account_submitted_before='+str(len(q)))
