# Original source: production_controller.pre_review_v2_20261010T195127Z.py
# Original source location: ${NON_NATIVE_ROOT}/audit/environment_only_sensitivity/review_triage/production_controller.pre_review_v2_20261010T195127Z.py
# Original source SHA-256: 09a9b22fdf09365e5217a0f8d057f728babcfc2f2d50584550af78c86a801152
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_ENVIRONMENT_ONLY_SENSITIVITY)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/usr/bin/env python3
"""One authoritative 814 species inventory; receipts describe 824 variant fits."""
import csv,hashlib,json,os,subprocess,sys,time
from pathlib import Path
from production_guards import strict_inventory,capacity,write_inventory,parse_sbatch_output,frozen_submission_shell
A=Path('<NON_NATIVE_ROOT>/audit/environment_only_sensitivity')
STATE=A/'controller_state.json'
FROZEN_FILES={'run_sensitivity814.R','sensitivity_guards.R','validate_pair814.R','slurm_e2e814.sh','slurm_bulk814.sh','production_controller.py','production_guards.py','static_contract.py','frozen_route_policies.json','static_preflight_contract.json'}

def run(cmd,**kw):return subprocess.run(cmd,text=True,stdout=subprocess.PIPE,stderr=subprocess.PIPE,check=True,**kw).stdout.strip()
def log(s):print(time.strftime('%Y-%m-%dT%H:%M:%S'),s,flush=True)
def queue():
 for attempt in range(4):
  try:
   lines=run(['squeue','-r','-h','-A','HPC_ACCOUNT','-o','%i|%T|%D|%C|%j'],timeout=45).splitlines();break
  except (subprocess.TimeoutExpired,subprocess.CalledProcessError):
   log('QUEUE_READ_UNAVAILABLE: no failure inference or new submission');time.sleep(10)
 else:raise RuntimeError('Scheduler read unavailable after bounded retries')
 return [dict(zip(['id','state','nodes','cpus','name'],line.split('|'))) for line in lines if line]
def tasks(rows):
 out=[]
 for r in rows:
  out.append(dict(species=r['accepted_taxon'],variant=r['variant'],root=r['output_root'],key=r['task_key']+'_'+r['variant']))
  if r['clean_full_output_root']:out.append(dict(species=r['accepted_taxon'],variant='CLEAN_FULL',root=r['clean_full_output_root'],key=r['task_key']+'_CLEAN_FULL'))
 assert len(rows)==814 and len({r['accepted_taxon'] for r in rows})==814 and len(out)==824
 assert len({t['root'] for t in out})==824
 return out

def validated(t):return (Path(t['root'])/'VALIDATED.rds').is_file()
def verify_gates(stage='standard'):
 assert stage in ('standard','special')
 gate_path=A/('STANDARD_GATE_PASS.json' if stage=='standard' else 'SPECIAL_GATE_PASS.json');policy_path=A/'aggregation_policy.json'
 if not gate_path.exists() or not policy_path.exists():return False
 gate=json.loads(gate_path.read_text());policy=json.loads(policy_path.read_text())
 assert gate['scope']=='814_FINAL_ANALYSIS' and gate['species_count']==814 and gate['fit_count']==824
 assert gate['ordinary_route_pass'] is True
 if stage=='special':assert gate['paired_route_pass'] is True
 assert policy['decision']=='DEFINED_NON_NA_MEAN' and policy['author_confirmed'] is True
 assert set(gate['entrypoint_sha256'])==FROZEN_FILES
 for name,digest in gate['entrypoint_sha256'].items():
  path=A/name;assert path.parent==A and hashlib.sha256(path.read_bytes()).hexdigest()==digest, 'Frozen entrypoint changed: '+name
 rows=strict_inventory(A/'sensitivity_inventory.tsv');assert len(rows)==814 and len({r['accepted_taxon'] for r in rows})==814
 canaries={r['accepted_taxon']:r for r in rows if r['accepted_taxon'] in ('Abutilon grandifolium','Acer platanoides')}
 assert len(canaries)==2
 ordinary=canaries['Abutilon grandifolium'];special=canaries['Acer platanoides']
 roots=[ordinary['output_root']]+([special['clean_full_output_root'],special['clean_env_output_root']] if stage=='special' else [])
 assert len(set(roots))==len(roots) and gate['validated_roots']==roots
 for root in roots:assert (Path(root)/'VALIDATED.rds').is_file()
 if stage=='special':
  assert Path(gate['paired_receipt'])==Path(special['clean_full_output_root']).parent
  assert (Path(gate['paired_receipt'])/'PAIRED_VALIDATED.rds').is_file()
 return True
def save(s):
 p=STATE.with_suffix('.json.new');p.write_text(json.dumps(s,indent=2));p.replace(STATE)

def checkpoint(tt,state,q,special_open):
 import statistics,math
 now=time.time();done=[t for t in tt if validated(t)];standard=[t for t in done if t['variant']=='ENV'];timings=[]
 for t in standard:
  path=Path(t['root'])/'stage_times.json'
  if path.exists():
   z=json.loads(path.read_text())
   if all(k in z for k in ['fit_started','fit_finished','e2e_started','complete']):timings.append(z)
 totals=sorted(z['complete']-z['e2e_started'] for z in timings);fits=[z['fit_finished']-z['fit_started'] for z in timings]
 recent=sum(z['complete']>=now-3600 for z in timings);elapsed=min(3600,max(1,now-state.setdefault('started_at',now)));rate=recent*3600/elapsed
 remaining=804-len(standard);eta=now+remaining/rate*3600 if len(timings)>=20 and rate>0 else None
 summary=dict(scope='814_FINAL_ANALYSIS',timestamp=time.strftime('%Y-%m-%dT%H:%M:%S'),complete_variants=len(done),total_variants=824,standard_complete=len(standard),standard_total=804,special_complete=len(done)-len(standard),special_total=20,active_tasks=sum(z['name'].startswith(('s814','sensitivity')) for z in q),running_nodes=sum(int(z['nodes']) for z in q if z['state']=='RUNNING'),account_submission_occupancy=len(q),node_limit=20,submit_limit=30,retry_or_isolation_count=len(state.get('isolated',{})),standard_gate='OPEN',special_gate='OPEN' if special_open else 'CLOSED_PENDING_PAIRED_QC',measured_standard_runtime_count=len(timings),median_fit_seconds=statistics.median(fits) if fits else None,median_e2e_seconds=statistics.median(totals) if totals else None,p90_e2e_seconds=totals[max(0,math.ceil(.9*len(totals))-1)] if totals else None,recent_standard_species_per_hour=rate if timings else None,projected_finish_unix=eta,eta_status='EVIDENCE_BASED' if eta else 'WAIT_FOR_20_MEASURED_STANDARD_COMPLETIONS',queue=q)
 path=A/'production_checkpoint.json';tmp=path.with_suffix('.json.new');tmp.write_text(json.dumps(summary,indent=2));tmp.replace(path)
def update_inventory(rows,tt,state,active):
 lookup={(t['species'],t['variant']):t for t in tt}
 for r in rows:
  statuses=[];jobs=[];owners=[]
  variants=[r['variant']]+(['CLEAN_FULL'] if r['clean_full_output_root'] else [])
  for v in variants:
   t=lookup[(r['accepted_taxon'],v)];j=state.get('jobs',{}).get(t['key'],'');root=Path(t['root'])
   status='TECHNICALLY_VALIDATED' if validated(t) else active.get(j,'SUBMITTED') if j and j in active else 'FAILED_REVIEW_REQUIRED' if j else 'READY_REFIT'
   prefix='clean_full' if v=='CLEAN_FULL' else 'env'
   r[prefix+'_fit_status']=status;r[prefix+'_job_id']=j;r[prefix+'_owner']='s814_v2_controller' if j in active else ''
   if prefix=='env' and status=='TECHNICALLY_VALIDATED':
    for phase in ['model','evaluation','selection','projection']: 
     if 'env_'+phase+'_status' in r:r['env_'+phase+'_status']='TECHNICALLY_VALIDATED'
   statuses.append(status)
   if j:jobs.append(v+':'+j)
   if j in active:owners.append(v+':s814_v2_controller')
  pair_ready=not r['clean_full_output_root'] or (Path(r['clean_full_output_root']).parent/'PAIRED_VALIDATED.rds').exists()
  r['owner']=';'.join(owners);r['slurm_job_id']=';'.join(jobs);r['status']=('TECHNICALLY_VALIDATED' if pair_ready else 'WAITING_PAIR_QC') if all(z=='TECHNICALLY_VALIDATED' for z in statuses) else 'ACTIVE' if owners else 'REVIEW_REQUIRED' if any('FAILED' in z for z in statuses) else 'READY_REFIT'
 write_inventory(A/'sensitivity_inventory.tsv',rows)

def paired(sp):
 rows=strict_inventory(A/'sensitivity_inventory.tsv');r=next(r for r in rows if r['accepted_taxon']==sp)
 if all((Path(r[k])/'VALIDATED.rds').exists() for k in ['clean_full_output_root','clean_env_output_root']):
  target=Path(r['clean_full_output_root']).parent
  lock=target/'PAIR_QC_OWNER'
  try:lock.mkdir()
  except FileExistsError:return
  try:
   if not (target/'PAIRED_VALIDATED.rds').exists():run(['<HPC_HOME>/R-4.4.1/bin/Rscript','--vanilla',str(A/'validate_pair814.R'),sp])
  finally:lock.rmdir()

def _controller_work():
 state=json.loads(STATE.read_text()) if STATE.exists() else {'jobs':{},'wave':0}
 rows=strict_inventory(A/'sensitivity_inventory.tsv');tt=tasks(rows)
 for gate_name in ['STANDARD_GATE_PASS.json','SPECIAL_GATE_PASS.json']:
  if (A/gate_name).exists():
   for k,v in json.loads((A/gate_name).read_text()).get('canary_jobs',{}).items():state['jobs'].setdefault(k,v)
 if state.get('submission_intent') or state.get('ambiguous_submission'):
  intent=state.get('submission_intent',state.get('ambiguous_submission'));name=intent['name'];found={z['id'].split('_')[0] for z in queue() if z['name']==name}
  if not found:
   accounting=run(['sacct','-X','-n','-P','-S',intent.get('date',time.strftime('%Y-%m-%d')),'--format=JobID,JobName,State'])
   found={line.split('|')[0].split('_')[0] for line in accounting.splitlines() if len(line.split('|'))>=3 and line.split('|')[1]==name}
  if len(found)!=1:log('UNRESOLVED_SUBMISSION_INTENT: stop; no task is ready for re-dispatch');return
  job=found.pop();wave=json.loads(Path(intent['manifest']).read_text())
  for i,t in enumerate(wave):state['jobs'][t['key']]=job+'_'+str(i)
  state.pop('submission_intent',None);state.pop('ambiguous_submission',None);save(state)
 for r in rows:
  for k in ['env_fit_status','env_job_id','env_owner','clean_full_fit_status','clean_full_job_id','clean_full_owner']:r.setdefault(k,'')
 while True:
  rows=strict_inventory(A/'sensitivity_inventory.tsv')
  mapping=json.loads((A/'E2E_ROUTE_JOBS.json').read_text())
  for r in rows:
   if r['accepted_taxon'] in mapping:
    state['jobs'][r['task_key']+'_'+r['variant']]=mapping[r['accepted_taxon']]
    if r['clean_full_output_root']:state['jobs'][r['task_key']+'_CLEAN_FULL']=mapping[r['accepted_taxon']]
  q=queue();active={z['id']:z['state'] for z in q}
  if not verify_gates():
   log('STANDARD_GATE_CLOSED: requires ordinary E2E and confirmed NA-safe aggregation');save(state);return
  special=next(r for r in rows if r['accepted_taxon']=='Acer platanoides')
  if not (A/'SPECIAL_GATE_PASS.json').exists() and (Path(special['clean_full_output_root']).parent/'PAIRED_VALIDATED.rds').exists():
   gate=json.loads((A/'STANDARD_GATE_PASS.json').read_text());gate.update(paired_route_pass=True,validated_roots=gate['validated_roots']+[special['clean_full_output_root'],special['clean_env_output_root']],paired_receipt=str(Path(special['clean_full_output_root']).parent))
   temp=A/'SPECIAL_GATE_PASS.json.new';temp.write_text(json.dumps(gate,indent=2));temp.replace(A/'SPECIAL_GATE_PASS.json');log('SPECIAL_PAIRED_GATE_FROZEN')
  special_open=verify_gates('special')
  unknown=[t for t in tt if (Path(t['root'])/'ACTIVE_OWNER').exists() and t['key'] not in state['jobs']]
  if unknown:raise RuntimeError('External ownership requires reconciliation; inventory not overwritten: '+','.join(t['key'] for t in unknown))
  update_inventory(rows,tt,state,active)
  checkpoint(tt,state,q,special_open)
  remaining=[t for t in tt if not validated(t)]
  if not remaining:log('824 variant fits technically validated; aggregate gate pending independent final assembly');save(state);return
  failed=[t for t in remaining if t['key'] in state['jobs'] and state['jobs'][t['key']] not in active]
  for t in failed:
   if t['key'] not in state.setdefault('isolated',{}):
    state['isolated'][t['key']]={'job':state['jobs'][t['key']],'reason':'Missing validation receipt after job left queue; preserve checkpoints; no blind refit','time':time.time()};log('ISOLATED '+t['key'])
  save(state)
  available=capacity(len(q),sum(int(z['nodes']) for z in q),sum(int(z['cpus']) for z in q))
  ready=[t for t in remaining if (t['variant']=='ENV' or special_open) and t['key'] not in state['jobs'] and not (Path(t['root'])/'ACTIVE_OWNER').exists()]
  wave=ready[:available]
  if wave:
   state['wave']+=1;name='s814v2b%04d'%state['wave'];manifest=A/('dispatch_%04d.json'%state['wave']);manifest.write_text(json.dumps(wave,indent=2))
   env=frozen_submission_shell()
   cmd=env+'sbatch --export=ALL --parsable --job-name='+name+' --array=0-'+str(len(wave)-1)+'%'+str(len(wave))+' '+str(A/'slurm_bulk814.sh')+' '+str(manifest)
   state['submission_intent']={'name':name,'manifest':str(manifest),'date':time.strftime('%Y-%m-%d'),'created_at':time.time()};save(state)
   try:job=parse_sbatch_output(run(['bash','-lc',cmd],timeout=60))
   except (subprocess.TimeoutExpired,subprocess.CalledProcessError) as error:
    found={z['id'].split('_')[0] for z in queue() if z['name']==name}
    if len(found)==1:job=found.pop()
    else:state['ambiguous_submission']={'name':name,'manifest':str(manifest),'reason':str(error)};save(state);log('AMBIGUOUS_SUBMISSION: stop and reconcile; never assume failed');return
   if not job.isdigit():raise RuntimeError('Malformed sbatch response '+repr(job))
   for i,t in enumerate(wave):state['jobs'][t['key']]=job+'_'+str(i)
   state.pop('submission_intent',None);state.pop('ambiguous_submission',None);save(state);log('SUBMITTED wave='+str(state['wave'])+' job='+job+' fits='+str(len(wave))+' account_occupancy_before='+str(len(q)))
  time.sleep(45)

def controller():
 lock=A/'CONTROLLER_OWNER'
 try:lock.mkdir()
 except FileExistsError:raise RuntimeError('A controller owner already exists; inspect its PID and submission intent before resuming')
 (lock/'pid').write_text(str(os.getpid()))
 try:_controller_work()
 finally:
  if (lock/'pid').read_text()==str(os.getpid()):(lock/'pid').unlink();lock.rmdir()

if __name__=='__main__':
 if sys.argv[1]=='resolve':
  item=json.loads(Path(sys.argv[2]).read_text())[int(sys.argv[3])];print(item['species']);print(item['variant'])
 elif sys.argv[1]=='pair-if-ready':paired(sys.argv[2])
 elif sys.argv[1]=='run':controller()
