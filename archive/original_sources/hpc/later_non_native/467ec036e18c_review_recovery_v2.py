# Original source: review_recovery_v2.py
# Original source location: ${NON_NATIVE_ROOT}/audit/environment_only_sensitivity/review_recovery_v2.py
# Original source SHA-256: 467ec036e18c5a1d25f2dfe9d868ea7c825597a3d5ad90ac3e7f32b0cc6d392e
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_ENVIRONMENT_ONLY_SENSITIVITY)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
"""Called only inside the exclusive sensitivity production controller."""
import json,hashlib,os,time
from pathlib import Path
A=Path('<NON_NATIVE_ROOT>/audit/environment_only_sensitivity')
def tick(state,tt,queue,run,save,log,capacity,frozen_submission_shell,parse_sbatch_output):
    assert (A/'CONTROLLER_OWNER/pid').read_text().strip()==str(os.getpid())
    p=A/'review_triage/REVIEW_RECOVERY_PLAN_V2.json'
    if not p.exists(): return
    plan=json.loads(p.read_text()); records=state.setdefault('review_recovery_v2',{})
    # Only explicitly approved, already-fixed infrastructure retries; never blind refit.
    for key in plan.get('approved_infrastructure_retry_keys',[]):
        if key in records and records[key].get('status')=='QC_RECOVERY_FAILED_REQUIRES_FOCUSED_DIAGNOSIS':
            history=state.setdefault('review_recovery_v2_retry_history',{}).setdefault(key,[])
            if len(history)<1:history.append(records.pop(key));save(state)
    live=queue(); active={x['id'] for x in live}
    # Reconcile only explicit recovery jobs; failed QC never becomes a new fit.
    for key,rec in records.items():
        if rec.get('status')!='SUBMITTED' or rec['job'] in active: continue
        root=Path(rec['root'])
        if (root/'VALIDATED.rds').is_file() and (root/'QC_ONLY_PRESERVATION_PASS_V2.json').is_file():
            rec['status']='VALIDATED';state.setdefault('isolated',{}).pop(key,None)
            log('REVIEW_QC_ONLY_VALIDATED '+key)
        else:
            rec['status']='QC_RECOVERY_FAILED_REQUIRES_FOCUSED_DIAGNOSIS';rec['finished_at']=time.time()
            err=A/'logs'/('review_qc_'+rec['job']+'.err')
            lines=err.read_text(errors='replace').splitlines() if err.exists() else []
            rec['failure_fingerprint']=next((x for x in reversed(lines) if x.startswith(('Error:', 'AttributeError:', 'AssertionError:'))),'UNKNOWN_QC_RECOVERY_FAILURE')
            log('REVIEW_QC_ONLY_ISOLATED '+key)
        lock=root/'REVIEW_RECOVERY_OWNER'
        if lock.exists(): lock.rename(root/('REVIEW_RECOVERY_OWNER.finished.'+rec['job']))
        save(state)
    if any(v.get('status')=='SUBMITTED' for v in records.values()): return
    from collections import Counter
    repeated=Counter(v.get('failure_fingerprint') for v in records.values() if v.get('status')=='QC_RECOVERY_FAILED_REQUIRES_FOCUSED_DIAGNOSIS')
    if any(n>=2 for fp,n in repeated.items()):
        state['review_recovery_v2_hold']='Two focused recovery failures sharing a fingerprint; isolate stream pending recorded fix';save(state);return
    free=capacity(len(live),sum(int(z['nodes']) for z in live),sum(int(z['cpus']) for z in live))
    if free<1:return
    for t in plan['candidates']:
        key=t['key'];root=Path(t['root'])
        if key not in state.get('isolated',{}) or key in records or (root/'VALIDATED.rds').exists():continue
        old=state['jobs'].get(key)
        if not old or old in active:continue
        # Exact array task accounting must establish terminal status before old owner archival.
        lines=run(['sacct','-X','-n','-P','-j',old,'-o','JobID%64,State'],timeout=45).splitlines()
        terminal=[line.split('|')[1].split()[0] for line in lines if line.split('|')[0]==old]
        if len(terminal)!=1 or terminal[0] not in ('FAILED','COMPLETED','CANCELLED','TIMEOUT','OUT_OF_MEMORY','NODE_FAIL','PREEMPTED','BOOT_FAIL'):continue
        if hashlib.sha256((A/'review_triage/qc_only_recover_emmedian_v1.R').read_bytes()).hexdigest()!=plan['runner_sha256']:raise RuntimeError('QC runner hash changed')
        # Refresh occupancy immediately before claim and sbatch. This controller is the only submitter.
        live=queue();active={x['id'] for x in live}
        if old in active or capacity(len(live),sum(int(z['nodes']) for z in live),sum(int(z['cpus']) for z in live))<1:return
        lock=root/'REVIEW_RECOVERY_OWNER'
        try:lock.mkdir()
        except FileExistsError:continue
        rec=dict(t,status='CLAIMED',old_job=old,attempt=1,claimed_at=time.time(),controller_pid=os.getpid())
        records[key]=rec;save(state)
        owner=root/'ACTIVE_OWNER'
        if owner.exists():
            archive=root/('ACTIVE_OWNER.archived.qc_v2.'+old)
            if archive.exists():raise RuntimeError('Owner archival collision '+key)
            owner.rename(archive);rec['archived_owner']=str(archive);save(state)
        manifest=A/'review_triage'/('qc_recovery_'+key+'.json');manifest.write_text(json.dumps(t,indent=2))
        name='s814qc'+key.split('_')[1][:10]
        state['submission_intent']={'name':name,'manifest':str(manifest),'date':time.strftime('%Y-%m-%d'),'created_at':time.time(),'kind':'REVIEW_QC_ONLY_V2','key':key};save(state)
        try:
            output=run(['bash','-lc',frozen_submission_shell()+'sbatch --export=ALL --parsable --job-name='+name+' '+str(A/'review_triage/slurm_qc_recovery_v2.sh')+' '+str(manifest)],timeout=60)
            job=parse_sbatch_output(output)
        except Exception:
            log('REVIEW_QC_SUBMISSION_AMBIGUOUS; preserve intent; stop controller for reconciliation');raise
        rec.update(status='SUBMITTED',job=job,submitted_at=time.time(),reserved_nodes=1,reserved_charged_cpus=64)
        state['jobs'][key]=job;state.pop('submission_intent',None);save(state)
        log('REVIEW_QC_ONLY_SUBMITTED '+key+' job='+job+' reserved_nodes=1');return

# Versioned projection-only recovery extension; protected frozen entrypoint remains unchanged.
_qc_only_tick_v2=tick
from review_projection_recovery_controller_v1 import tick_projection as _tick_projection_recovery_v1
def tick(state,tt,queue,run,save,log,capacity,frozen_submission_shell,parse_sbatch_output):
    _qc_only_tick_v2(state,tt,queue,run,save,log,capacity,frozen_submission_shell,parse_sbatch_output)
    if any(v.get('status')=='SUBMITTED' for v in state.get('review_recovery_v2',{}).values()): return
    _tick_projection_recovery_v1(state,tt,queue,run,save,log,capacity,frozen_submission_shell,parse_sbatch_output)
