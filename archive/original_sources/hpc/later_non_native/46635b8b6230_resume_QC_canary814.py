# Original source: resume_QC_canary814.py
# Original source location: ${NON_NATIVE_ROOT}/audit/environment_only_sensitivity/resume_QC_canary814.py
# Original source SHA-256: 46635b8b6230cfc02718713891088812b41688dd914b745aebe2db1977bf73f3
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_ENVIRONMENT_ONLY_SENSITIVITY)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
import sys,json,time,hashlib
from pathlib import Path
from production_guards import strict_inventory,write_inventory,parse_sbatch_output,frozen_submission_shell,capacity
from production_controller import A,queue,run
taxon=sys.argv[1];assert taxon in ['Abutilon grandifolium','Acer platanoides']
proof=json.loads((A/'QC_API_PROBE_PASS.json').read_text());assert proof['passed'] and proof['runner_sha256']==hashlib.sha256((A/'run_sensitivity814.R').read_bytes()).hexdigest()
mapping=json.loads((A/'E2E_ROUTE_JOBS.json').read_text());old=mapping[taxon];q=queue();assert not any(z['id']==old for z in q)
acct=run(['sacct','-X','-j',old,'-n','-P','--format=JobIDRaw,State']);raw,state=acct.splitlines()[0].split('|')[:2];assert state=='FAILED'
parent,index=old.split('_');err=(A/'logs'/('e2e_'+parent+'_'+index+'.err')).read_text()
assert 'unused argument (models = selected)' in err or 'unused argument (ID = FALSE)' in err or 'nrow(newdata) >= 10 is not TRUE' in err
rows=strict_inventory(A/'sensitivity_inventory.tsv');r=next(r for r in rows if r['accepted_taxon']==taxon);root=Path(r['output_root'] if index=='0' else r['clean_full_output_root'])
assert all((root/f).exists() for f in ['models.rds','FIT_VALIDATED.rds','projection_object.rds','ensemble_suitability.tif']) and not (root/'VALIDATED.rds').exists()
lock=root/'ACTIVE_OWNER';owner=(lock/'owner.txt').read_text().splitlines();assert owner[0]==taxon and owner[2]==raw
assert capacity(len(q),sum(int(z['nodes']) for z in q),sum(int(z['cpus']) for z in q))>=1
intent=A/('QC_resume_'+old+'.json');assert not intent.exists();intent.write_text(json.dumps(dict(state='PRE_SUBMIT_INTENT',old=old,taxon=taxon,guard_proof=proof,date=time.strftime('%Y-%m-%d')),indent=2))
(lock/'owner.txt').replace(root/('owner_failed_QC_'+old+'.txt'));lock.rmdir()
job=parse_sbatch_output(run(['bash','-lc',frozen_submission_shell()+'sbatch --export=ALL --parsable --array='+index+' '+str(A/'slurm_e2e814.sh')],timeout=60));mapping[taxon]=job+'_'+index
(A/'E2E_ROUTE_JOBS.json').write_text(json.dumps(mapping,indent=2));intent.write_text(json.dumps(dict(state='SUBMITTED',old=old,new=mapping[taxon],guard_proof=proof),indent=2))
r['slurm_job_id']=mapping[taxon];r['owner']='s814_v2_e2e';r['status']='E2E_QC_CHECKPOINT_RESUME_SUBMITTED';write_inventory(A/'sensitivity_inventory.tsv',rows);print('GUARDED_QC_RESUME',mapping[taxon])
