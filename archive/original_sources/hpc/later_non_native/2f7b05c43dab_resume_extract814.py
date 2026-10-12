# Original source: resume_extract814.py
# Original source location: ${NON_NATIVE_ROOT}/audit/environment_only_sensitivity/resume_extract814.py
# Original source SHA-256: 2f7b05c43dab5d8ed98189f6d209eb8612ec207edffb2abdca2ed0d325058e1b
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_ENVIRONMENT_ONLY_SENSITIVITY)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
import json,time
from pathlib import Path
from production_guards import strict_inventory,write_inventory,parse_sbatch_output,frozen_submission_shell,capacity
from production_controller import A,queue,run
q=queue();assert not any(z['id']=='9236220_0' for z in q)
acct=run(['sacct','-X','-j','9236220_0','-n','-P','--format=JobID,State']);assert '9236220_0|FAILED' in acct
x=strict_inventory(A/'sensitivity_inventory.tsv');r=next(r for r in x if r['accepted_taxon']=='Abutilon grandifolium');root=Path(r['output_root'])
assert all((root/f).exists() for f in ['models.rds','FIT_VALIDATED.rds','projection_object.rds','ensemble_suitability.tif'])
assert not (root/'VALIDATED.rds').exists()
lock=root/'ACTIVE_OWNER';owner=(lock/'owner.txt').read_text().splitlines();raw=run(['sacct','-X','-j','9236220_0','-n','-P','--format=JobIDRaw,State']);assert owner[0]==r['accepted_taxon'] and raw.split('|')[0]==owner[2]
(lock/'owner.txt').replace(root/'owner_failed_extract_9236220.txt');lock.rmdir()
assert capacity(len(q),sum(int(z['nodes']) for z in q),sum(int(z['cpus']) for z in q))>=1
intent=A/'extract_resume_submission_intent.json';assert not intent.exists()
intent.write_text(json.dumps({'state':'PRE_SUBMIT_INTENT','previous_job':'9236220_0','date':time.strftime('%Y-%m-%d'),'guard':'terra coordinate matrix extraction verified; reuse fit/projection checkpoints'},indent=2))
job=parse_sbatch_output(run(['bash','-lc',frozen_submission_shell()+'sbatch --export=ALL --parsable --array=0 '+str(A/'slurm_e2e814.sh')],timeout=60))
intent.write_text(json.dumps({'state':'SUBMITTED','job':job+'_0','previous_job':'9236220_0'},indent=2))
(A/'E2E_ROUTE_JOBS.json').write_text(json.dumps({'Abutilon grandifolium':job+'_0','Acer platanoides':'9236220_1'},indent=2))
r['slurm_job_id']=job+'_0';r['owner']='s814_v2_e2e';r['status']='E2E_QC_CHECKPOINT_RESUME_SUBMITTED';write_inventory(A/'sensitivity_inventory.tsv',x)
print('EXTRACTION_GUARD_CHECKPOINT_RESUME_SUBMITTED',job+'_0')
