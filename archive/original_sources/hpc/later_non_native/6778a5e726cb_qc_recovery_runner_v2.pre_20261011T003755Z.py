# Original source: qc_recovery_runner_v2.pre_20261011T003755Z.py
# Original source location: ${NON_NATIVE_ROOT}/audit/environment_only_sensitivity/review_triage/qc_recovery_runner_v2.pre_20261011T003755Z.py
# Original source SHA-256: 6778a5e726cb2e23d8c6baa2b0cfb42027799500989b74396ff113babd27b02a
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_ENVIRONMENT_ONLY_SENSITIVITY)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
import pathlib,json,hashlib,subprocess,sys,os
A=pathlib.Path('<NON_NATIVE_ROOT>/audit/environment_only_sensitivity')
t=json.loads(pathlib.Path(sys.argv[1]).read_text());root=pathlib.Path(t['root']);assert A/'production_v2' in root.parents
assert (root/'REVIEW_RECOVERY_OWNER').is_dir() and not (root/'ACTIVE_OWNER').exists() and not (root/'VALIDATED.rds').exists()
files=['FIT_VALIDATED.rds','models.rds','evaluation.csv','selection.csv','ensemble.rds','ensemble_contract.rds','projection_object.rds','projection_contract.rds','ensemble_suitability.tif','writer_contract.rds']
def hashes():
 out={}
 for n in files:
  h=hashlib.sha256()
  with (root/n).open('rb') as f:
   for b in iter(lambda:f.read(8*1024*1024),b''):h.update(b)
  out[n]=h.hexdigest()
 return out
before=hashes();log=root/'qc_only_recovery_v2_preservation.json';log.write_text(json.dumps({'before':before,'job':os.getenv('SLURM_JOB_ID')},indent=2))
p=subprocess.run(['<HPC_HOME>/R-4.4.1/bin/Rscript','--vanilla',str(A/'review_triage/qc_only_recover_emmedian_v1.R'),str(root),t['species'],t['variant']])
after=hashes();assert before==after,'Protected fit/projection artifacts changed during QC-only recovery'
if p.returncode==0:
 assert (root/'VALIDATED.rds').is_file()
 tmp=root/'QC_ONLY_PRESERVATION_PASS_V2.json.new';tmp.write_text(json.dumps({'job':os.getenv('SLURM_JOB_ID'),'before':before,'after':after,'no_refit':True,'no_reprojection':True},indent=2));tmp.replace(root/'QC_ONLY_PRESERVATION_PASS_V2.json')
else:sys.exit(p.returncode)
