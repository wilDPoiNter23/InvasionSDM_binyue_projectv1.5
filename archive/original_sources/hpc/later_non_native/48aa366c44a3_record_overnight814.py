# Original source: record_overnight814.py
# Original source location: ${NON_NATIVE_ROOT}/audit/environment_only_sensitivity/record_overnight814.py
# Original source SHA-256: 48aa366c44a32ed693437d51d30b38ed61f29f71ce47caf4560e4c5302512f11
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_ENVIRONMENT_ONLY_SENSITIVITY)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
import json,time,os
from pathlib import Path
from production_controller import A,queue,verify_gates
from production_guards import strict_inventory
pid=int((A/'CONTROLLER_OWNER'/'pid').read_text());os.kill(pid,0);assert verify_gates()
q=queue();rows=strict_inventory(A/'sensitivity_inventory.tsv')
authority=dict(scope='environment_only_sensitivity',exclusive_tianjin_submission_authority=True,author_authorized_autonomous_overnight=True,controller_pid=pid,controller_owner=str(A/'CONTROLLER_OWNER'),other_workflows_must_not_submit_tianjin=True,unrelated_jobs_read_only=True,capacity_limits=dict(nodes=20,charged_cpus=1280,submit_jobs=30),heartbeat_automation_id='814',recorded_at_unix=time.time())
(A/'PRODUCTION_CONTROL_AUTHORITY.json').write_text(json.dumps(authority,indent=2))
snapshot=dict(complete_variants=sum((Path(r['output_root'])/'VALIDATED.rds').exists()+int(bool(r['clean_full_output_root']) and (Path(r['clean_full_output_root'])/'VALIDATED.rds').exists()) for r in rows),standard_complete=sum((Path(r['output_root'])/'VALIDATED.rds').exists() for r in rows if not r['clean_full_output_root']),special_complete=sum(int((Path(r['output_root'])/'VALIDATED.rds').exists())+int((Path(r['clean_full_output_root'])/'VALIDATED.rds').exists()) for r in rows if r['clean_full_output_root']),live_queue=q,running_nodes=sum(int(z['nodes']) for z in q if z['state']=='RUNNING'),account_submit_occupancy=len(q),running_charged_cpus=sum(int(z['cpus']) for z in q if z['state']=='RUNNING'),controller_pid=pid,heartbeat_automation_id='814',standard_gate='OPEN',special_gate='OPEN' if verify_gates('special') else 'PENDING_PAIRED_QC',recorded_at_unix=time.time())
(A/'overnight_checkpoint.json').write_text(json.dumps(snapshot,indent=2))
root=A.parent.parent
entry='\n\n## '+time.strftime('%Y-%m-%d %H:%M:%S')+' — Codex sole overnight sensitivity controller; author NA decision implemented\n'
entry+='- Author confirms finite/non-NA per-pixel mean; nvalid_species explicitly generated, zero valid -> NA. aggregation_policy.json binds author_decision_20261009.txt SHA. Legacy products unchanged. Final destination audit/environment_only_sensitivity/aggregates_814_na_safe_v1/FULL and ENV; directories and PRODUCT_PLAN exist, final aggregate TIFFs not yet generated.\n'
entry+='- Author explicitly releases804 standard gate independently of special. STANDARD_GATE_PASS.json frozen and verified; no new ordinary fit canary. First bulk wave9236407 has19 ordinary ENV tasks; special checkpoint recovery9236406_1. Unique controller PID'+str(pid)+' automatically refills live free capacity. Original accepted FULL and upstream PA/CV/predictor selection never rerun.\n'
entry+='- New sparse-QC sampling guard: 10k candidate regular cells, retain finite outputs and complete predictors, evenly retain up to200, require at least10. MAXNET installed dataframe prediction clamps while raster projection does not; independent QC now reassembles sampled attributes into a tiny raster to use matching native raster dispatch, no threshold relaxation. Saved Acer CLEAN_FULL probe200 coordinates max delta1.017253e-5; no fit/projection repeated. Shared entrypoint frozen after fix.\n'
entry+='- Changed/new files in '+str(A)+': run_sensitivity814.R (QC/timing), probe_saved_QC814.R, diagnose_saved_pair_QC.R, probe_predict2_api.R, production_controller.py (separate gates/refill/isolation/checkpoint metrics), test_gate814.py, resume_QC_canary814.py, freeze_standard_start814.py, start_production814.sh, aggregation_policy.json, author_decision_20261009.txt, STANDARD_GATE_PASS.json, PRODUCTION_CONTROL_AUTHORITY.json, overnight_checkpoint.json, aggregates_814_na_safe_v1/PRODUCT_PLAN.json, dispatch_0001.json, controller_state.json, production_checkpoint.json, logs and versioned variant branches.\n'
entry+='- Sole authority explicitly limited to environment_only_sensitivity; other live workflows consume capacity and are read-only. Nightly heartbeat automation814 ACTIVE every15 minutes in this chat; reuses existing healthy controller, no duplicate daemon/submission. Stop only user-defined no-runnable/all-required-complete/new-science/persistent-scheduler-unavailable conditions.\n'
entry+='- Snapshot: '+str(snapshot['complete_variants'])+'/824 variants; standard'+str(snapshot['standard_complete'])+'/804; special'+str(snapshot['special_complete'])+'/20; running nodes'+str(snapshot['running_nodes'])+'/20; submit occupancy'+str(snapshot['account_submit_occupancy'])+'/30. Median/p90/rate/ETA deferred until actual measured new standard completions, not inferred from24h requests. Special still awaiting actual paired receipt; auto gate will release after it exists. Final aggregate assembly remains downstream work, does not block fitting.\n'
entry+='- Next continuation: refresh all live ownership/accounting; reconcile exact validation receipts; inspect member vs job failures separately; resume smallest downstream stage only, isolate new same cause after two focused failures and refill other READY; generate paired NA-safe814 products only from fully validated per-species inputs.\n'
for name in ['HANDOFF_LOG.md','CONTINUATION_STATUS.md']:
 with (root/name).open('a') as f:f.write(entry)
print(json.dumps(snapshot,indent=2))
