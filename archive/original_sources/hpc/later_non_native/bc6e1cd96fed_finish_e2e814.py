# Original source: finish_e2e814.py
# Original source location: ${NON_NATIVE_ROOT}/audit/environment_only_sensitivity/finish_e2e814.py
# Original source SHA-256: bc6e1cd96fed4d15864869196046ce414604a14ba8fe279c3c6581ad0b36e640
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_ENVIRONMENT_ONLY_SENSITIVITY)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
"""Record two-route validation completion; never infer success from Slurm alone."""
import hashlib,json,subprocess,time,fcntl,os
from pathlib import Path
from production_guards import strict_inventory,write_inventory
from production_controller import A,verify_gates
owner=(A/'E2E_FINALIZER_OWNER.lock').open('a+')
fcntl.flock(owner,fcntl.LOCK_EX|fcntl.LOCK_NB)
owner.seek(0);owner.truncate();owner.write(str(os.getpid()));owner.flush()
jobs={'Abutilon grandifolium':'9236220_0','Acer platanoides':'9236220_1'}
files=['run_sensitivity814.R','sensitivity_guards.R','validate_pair814.R','slurm_e2e814.sh','slurm_bulk814.sh','production_controller.py','production_guards.py','static_contract.py','frozen_route_policies.json','static_preflight_contract.json']
while True:
 if (A/'E2E_ROUTE_JOBS.json').exists():jobs=json.loads((A/'E2E_ROUTE_JOBS.json').read_text())
 rows=strict_inventory(A/'sensitivity_inventory.tsv');chosen={r['accepted_taxon']:r for r in rows if r['accepted_taxon'] in jobs}
 ordinary=Path(chosen['Abutilon grandifolium']['output_root']);special=chosen['Acer platanoides'];full=Path(special['clean_full_output_root']);env=Path(special['clean_env_output_root']);roots=[ordinary,full,env]
 if all((p/'VALIDATED.rds').is_file() for p in roots) and (full.parent/'PAIRED_VALIDATED.rds').is_file():
  gate=dict(scope='814_FINAL_ANALYSIS',species_count=814,ordinary_species_count=804,special_species_count=10,fit_count=824,ordinary_route_pass=True,paired_route_pass=True,validated_roots=[str(p) for p in roots],paired_receipt=str(full.parent),entrypoint_sha256={f:hashlib.sha256((A/f).read_bytes()).hexdigest() for f in files},canary_jobs={chosen['Abutilon grandifolium']['task_key']+'_ENV':jobs['Abutilon grandifolium'],special['task_key']+'_CLEAN_FULL':jobs['Acer platanoides'],special['task_key']+'_CLEAN_ENV':jobs['Acer platanoides']},recorded_at=time.strftime('%Y-%m-%dT%H:%M:%S'))
  temp=A/'E2E_GATE_PASS.json.new';temp.write_text(json.dumps(gate,indent=2));temp.replace(A/'E2E_GATE_PASS.json')
  print('BOTH_E2E_PASS_AND_ENTRYPOINTS_FROZEN',flush=True)
  if verify_gates():
   subprocess.run(['python3',str(A/'production_controller.py'),'run'],check=True)
  else:print('BULK_GATE_CLOSED_AUTHOR_AGGREGATION_DECISION_REQUIRED',flush=True)
  break
 text=subprocess.run(['sacct','-X','-n','-P','-j',','.join(jobs.values()),'--format=JobID,State,ExitCode'],check=True,text=True,stdout=subprocess.PIPE).stdout
 states={line.split('|')[0]:line.split('|')[1] for line in text.splitlines() if len(line.split('|'))>=3}
 terminal={'FAILED','CANCELLED','TIMEOUT','OUT_OF_MEMORY','NODE_FAIL','PREEMPTED'}
 if any(states.get(j,'').split(' ')[0] in terminal for j in jobs.values()) or all(states.get(j)=='COMPLETED' for j in jobs.values()):
  missing=[str(p) for p in roots if not (p/'VALIDATED.rds').is_file()]
  if missing:
   (A/'E2E_ATTENTION_REQUIRED.json').write_text(json.dumps(dict(jobs=states,missing_receipts=missing,action='Inspect logs and saved checkpoints; never automatically resubmit',recorded_at=time.strftime('%Y-%m-%dT%H:%M:%S')),indent=2))
   print('E2E_ATTENTION_REQUIRED_NO_AUTOMATIC_RESUBMISSION',flush=True);break
 time.sleep(45)
