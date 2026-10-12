# Original source: review_projection_recovery_controller_v1.py
# Original source location: ${NON_NATIVE_ROOT}/audit/environment_only_sensitivity/review_projection_recovery_controller_v1.py
# Original source SHA-256: ade58d4d06a1b0a3442588805b2bac460ac2b39717262f49d6cd05bf6724b9a9
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_ENVIRONMENT_ONLY_SENSITIVITY)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# Projection-only REVIEW recovery extension; imported by sole production controller.
import csv,hashlib,json,os,time
from pathlib import Path
A=Path('<NON_NATIVE_ROOT>/audit/environment_only_sensitivity')
T=A/'review_triage/814_review_recovery_137_v3.tsv'
S=A/'controller_state.json'
def _sha(p): return hashlib.sha256(Path(p).read_bytes()).hexdigest()
def _write_table(rows,path):
    fields=list(rows[0])
    for row in rows:
        for name in row:
            if name not in fields: fields.append(name)
    tmp=path.with_suffix('.tsv.new')
    with open(tmp,'w',newline='') as f:
        w=csv.DictWriter(f,fieldnames=fields,delimiter='\t',lineterminator='\n',quoting=csv.QUOTE_MINIMAL)
        w.writeheader();w.writerows(rows)
    tmp.replace(path)
def _update_row(key,**values):
    rows=list(csv.DictReader(open(T,newline=''),delimiter='\t'))
    hits=[r for r in rows if r['task_key']==key]
    if len(hits)!=1: raise RuntimeError('Review table key missing or duplicated: '+key)
    hits[0].update({k:str(v) for k,v in values.items()})
    _write_table(rows,T)
def _source_paths(invrow):
    src=invrow['authoritative_baseline_source']
    b=Path('<WORK_ROOT>')
    if any(x in src for x in ('V4_MULTIFOLD','DIRECT34')): stack=str(b/'data/predictors/full_stack_terrestrial_primary_v1.tif')
    elif any(x in src for x in ('GAP13','VIIRS35','SPARTINA','SOLANUM')): stack=str(b/'data/predictors/full_stack_terrestrial_plus_VIIRS_v1.tif')
    else: stack='<HPC_INPUT_ROOT>/climate/full_stack.tif'
    return src,stack,invrow['baseline_global_projection_path']
def _terminal(run,key):
    lines=run(['sacct','-X','-n','-P','-j',key,'-o','JobID%64,State'],timeout=45).splitlines()
    states=[z.split('|')[1].split()[0] for z in lines if len(z.split('|'))>=2 and z.split('|')[0]==key]
    terminal={'FAILED','COMPLETED','CANCELLED','TIMEOUT','OUT_OF_MEMORY','NODE_FAIL','PREEMPTED','BOOT_FAIL'}
    return len(states)==1 and states[0] in terminal
def tick_projection(state,tt,queue,run,save,log,capacity,frozen_submission_shell,parse_sbatch_output):
    assert (A/'CONTROLLER_OWNER/pid').read_text().strip()==str(os.getpid())
    records=state.setdefault('review_projection_recovery_v1',{})
    live=queue();active={x['id'] for x in live}
    # Reconcile only controller-submitted projection recoveries.
    for key,rec in list(records.items()):
        if rec.get('status')!='SUBMITTED' or rec.get('job') in active: continue
        root=Path(rec['root']);receipt=root/'PROJECTION_RECOVERY_PASS_V1.json'
        if (root/'VALIDATED.rds').is_file() and receipt.is_file():
            rec['status']='VALIDATED';rec['finished_at']=time.time();state.setdefault('isolated',{}).pop(key,None)
            _update_row(key,currently_isolated='NO',current_status='VALIDATED',projection_recovery_status='VALIDATED',projection_recovery_job=rec['job'],projection_recovery_receipt_sha256=_sha(receipt),projection_recovery_refit='NO',projection_recovery_reproject='YES')
            log('REVIEW_PROJECTION_RECOVERY_VALIDATED '+key+' job='+rec['job'])
        else:
            rec['status']='FAILED_REMAINS_ISOLATED';rec['finished_at']=time.time()
            err=A/'logs'/('review_projection_'+rec['job']+'.err')
            lines=err.read_text(errors='replace').splitlines() if err.exists() else []
            rec['failure_fingerprint']=next((x for x in reversed(lines) if x.startswith(('Error:','Error in','AssertionError:'))),'UNKNOWN_PROJECTION_RECOVERY_FAILURE')
            _update_row(key,currently_isolated='YES',current_status='REVIEW',projection_recovery_status='FAILED_REMAINS_ISOLATED',projection_recovery_job=rec['job'],projection_recovery_fingerprint=rec['failure_fingerprint'])
            log('REVIEW_PROJECTION_RECOVERY_FAILED '+key+' fingerprint='+rec['failure_fingerprint'][:180])
        lock=root/'REVIEW_RECOVERY_OWNER'
        if lock.exists():
            dest=root/('REVIEW_RECOVERY_OWNER.finished.projection_v1.'+rec['job'])
            if dest.exists(): raise RuntimeError('Recovery lock archive collision '+key)
            lock.rename(dest)
        save(state)
    if any(v.get('status')=='SUBMITTED' for v in records.values()): return
    # Do not reattempt any key; two identical recovery failures freeze this failure stream.
    failures=[v.get('failure_fingerprint') for v in records.values() if v.get('status')=='FAILED_REMAINS_ISOLATED']
    from collections import Counter
    repeated=Counter(failures)
    if any(n>=2 for n in repeated.values()):
        state['review_projection_recovery_v1_hold']='Repeated projection recovery fingerprint; preserve isolated artifacts and require focused fix'
        save(state);return
    if state.get('review_projection_recovery_v1_hold'): return
    free=capacity(len(live),sum(int(z['nodes']) for z in live),sum(int(z['cpus']) for z in live))
    if free<1: return
    table=list(csv.DictReader(open(T,newline=''),delimiter='\t'))
    inv={r['accepted_taxon']:r for r in csv.DictReader(open(A/'sensitivity_inventory.tsv',newline=''),delimiter='\t')}
    statejobs=state.get('jobs',{})
    for row in table:
        key=row['task_key']
        if row.get('projection_only_repair_candidate')!='YES' or key not in state.get('isolated',{}) or key in records: continue
        if row.get('currently_isolated')!='YES': continue
        species=row['species'];variant=row['variant'];ir=inv.get(species)
        if not ir or variant!='ENV': continue
        root=Path(ir['output_root'])
        if not str(root).startswith(str(A/'production_v2')) or (root/'VALIDATED.rds').exists(): continue
        if (root/'ACTIVE_OWNER').exists() and not (root/'REVIEW_RECOVERY_OWNER').exists():
            # Archive only after proving the original job is terminal below.
            pass
        needed=['FIT_VALIDATED.rds','fit_contract.rds','models.rds','evaluation.csv','selection.csv','projection_contract.rds']
        if any(not (root/n).is_file() for n in needed): continue
        # Existing ensemble objects are reused; a single saved selection can be projected alone.
        selected=list(csv.DictReader(open(root/'selection.csv',newline='')))
        ens=(root/'ensemble.rds').is_file()
        if not ens and len(selected)!=1: continue
        old=statejobs.get(key)
        if not old or old in active or not _terminal(run,old): continue
        src,stack,domain=_source_paths(ir)
        try: Path(stack).stat();Path(domain).stat()
        except OSError: continue
        # Refresh account occupancy immediately before claim and submission.
        live=queue();active={x['id'] for x in live}
        if old in active or capacity(len(live),sum(int(z['nodes']) for z in live),sum(int(z['cpus']) for z in live))<1: return
        lock=root/'REVIEW_RECOVERY_OWNER'
        try: lock.mkdir()
        except FileExistsError: continue
        rec={'key':key,'root':str(root),'species':species,'variant':variant,'status':'CLAIMED','old_job':old,'attempt':1,'claimed_at':time.time(),'controller_pid':os.getpid(),'runner_sha256':_sha(A/'review_triage/review_projection_recovery_v1.R')}
        records[key]=rec;save(state)
        owner=root/'ACTIVE_OWNER'
        if owner.exists():
            dest=root/('ACTIVE_OWNER.archived.projection_v1.'+old)
            if dest.exists(): raise RuntimeError('Original owner archival collision '+key)
            owner.rename(dest);rec['archived_owner']=str(dest);save(state)
        chosen=row.get('chosen_ensemble','')
        if chosen.startswith('NOT_') or ';' in chosen: chosen=''
        manifest={'key':key,'root':str(root),'species':species,'variant':variant,'audit_root':str(A),'baseline_source_route':src,'policy_path':str(A/'frozen_route_policies.json'),'stack':stack,'domain':domain,'chosen_ensemble':chosen,'review_table_sha256':_sha(T),'runner_sha256':rec['runner_sha256'],'old_job':old}
        mp=A/'review_triage'/('projection_recovery_'+key+'.json');mp.write_text(json.dumps(manifest,indent=2))
        name='s814prj'+key.split('_')[1][:10]
        state['submission_intent']={'name':name,'manifest':str(mp),'date':time.strftime('%Y-%m-%d'),'created_at':time.time(),'kind':'REVIEW_PROJECTION_ONLY_V1','key':key};save(state)
        cmd=frozen_submission_shell()+'sbatch --export=ALL --parsable --job-name='+name+' '+str(A/'review_triage/slurm_projection_recovery_v1.sh')+' '+str(mp)
        try: job=parse_sbatch_output(run(['bash','-lc',cmd],timeout=60))
        except Exception:
            log('AMBIGUOUS_PROJECTION_RECOVERY_SUBMISSION; preserve intent and stop controller for reconciliation');raise
        rec.update(status='SUBMITTED',job=job,submitted_at=time.time(),reserved_nodes=1,reserved_charged_cpus=64)
        state.setdefault('jobs',{})[key]=job;state.pop('submission_intent',None);save(state)
        _update_row(key,projection_recovery_status='SUBMITTED',projection_recovery_job=job,projection_recovery_refit='NO',projection_recovery_reproject='YES')
        log('REVIEW_PROJECTION_RECOVERY_SUBMITTED '+key+' job='+job+' reserved_nodes=1')
        return
