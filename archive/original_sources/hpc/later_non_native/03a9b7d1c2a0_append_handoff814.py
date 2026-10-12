# Original source: append_handoff814.py
# Original source location: ${NON_NATIVE_ROOT}/audit/environment_only_sensitivity/append_handoff814.py
# Original source SHA-256: 03a9b7d1c2a07e41139bfd9107087e9c4d6153943a607e603a1ff29d41780f4e
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_ENVIRONMENT_ONLY_SENSITIVITY)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
import time
from pathlib import Path
A=Path('<NON_NATIVE_ROOT>/audit/environment_only_sensitivity');root=A.parent.parent
entry='''

## 2026-10-09 — Codex sensitivity814 implementation and two-route validation

- Authoritative scope: 814_FINAL_ANALYSIS, 814 unique taxa (813 + Solanum rostratum), 804 ordinary ENV, 10 CLEAN_ENV, 10 CLEAN_FULL = 824 planned variant fits. One canonical inventory only.
- Inspected: primary AGENTS.md and project state/readme/continuation/ledgers; later 36_aggregate_variants_814.R, 33/35 input writers and 813 manifest plus Solanum; actual saved model/formatted/PA/CV/options/evaluation artifacts; historical successful Slurm fit resources; current account queue/accounting and canary logs.
- Created/changed under <NON_NATIVE_ROOT>/audit/environment_only_sensitivity/: sensitivity_inventory.tsv, static_preflight_814.csv/R, static_preflight_contract.json, static_contract.py, frozen_route_policies.json, freeze_policies.py, run_sensitivity814.R, sensitivity_guards.R, validate_pair814.R, slurm_e2e814.sh, slurm_bulk814.sh, production_guards.py, production_controller.py, test_gate814.py, test_extract814.R, finish_e2e814.py, start_finish_e2e814.sh, snapshot814.py, E2E_ROUTE_JOBS.json, production_status_snapshot.json, sensitivity_814_status.md, aggregation_first_taxon_NA_diagnostic.csv, versioned production_v2 checkpoints and attempt/failure receipts. Originals untouched.
- Static preflight: 814 PASS, 0 FAIL. Solanum actual 35 predictors -> ENV30; saved 4 PA sets, 21 calibration columns, GLM/RF/MAXNET and accepted two-member projection route recovered. Historical failed postprocessing job is not evidence of missing fitted models.
- Only two unique canary species: ordinary Abutilon grandifolium and GAP13 Acer platanoides. Earlier implementation attempts preserved. Deprecated bm.options had ignored ordinary custom parameters; rejected unaccepted ordinary attempt was retained and explicit OPT.user version refitted. Actual current fit-options/PA/CV guards passed for ordinary ENV and special CLEAN_FULL.
- Latest: ordinary job9236220_0 finished projection then failed QC API coordinate extract ID=FALSE. Guard repaired and verified on installed terra; checkpoint-only resume9236225_0 submitted, no refit. Special9236220_1 still projects CLEAN_FULL. CLEAN_ENV and paired/final raster validation incomplete. Neither E2E declared passed; no bulk fits submitted.
- Resource policy recovered from successful fit jobs: one node, requested1CPU,64GB,24h; charged64CPU/node. Account limits20nodes1280CPU and30submit jobs; dynamic accountwide queue checks; unrelated jobs unchanged.
- Mechanical gate tests passed: absent policy, changed frozen entrypoint, missing validation receipt fail. Reviewer feedback added fixed required entrypoint set, exact designated canary roots, paired species status guard, and gate-before-inventory-update/external ownership reconciliation.
- Open author decision under AGENTS §13: later aggregate writer's first-taxon NA propagation contradicts stated non-NA denominator mean. Confirm either new paired FULL/ENV mean fixing initial NA (recommended; preserve old products), or strict legacy NA propagation. This affects publication-grade numeric coverage; no assumed approval. Required author question already sent. 814/824 counts themselves need no further permission.
- Gate: CLOSED until both actual route receipts and author aggregation policy. Background finalizer2066131 records pass/fail without blind resubmission; freeze only after three variant + paired receipts. Bulk controller has not been started. Final native/binary/continuous 814 aggregate assembly remains unfinished.
- Next: inspect checkpoint QC resume and special completion; recover only validated checkpoints for known API failure if it occurs in the already-running old interpreter; record author policy, freeze proven entrypoints, report gates/counts/resources, then guarded automatic bulk. Do not invent an E2E pass or final aggregate completion.
'''
with (root/'HANDOFF_LOG.md').open('a') as f:f.write(entry)
with (root/'CONTINUATION_STATUS.md').open('a') as f:f.write('\n\n## 2026-10-09 sensitivity814 continuation\n814 scope / 824 planned fits; static814 PASS. Two canaries incomplete; ordinary9236225_0 checkpoint QC resume, special9236220_1 CLEAN_FULL projection. Bulk gate closed: E2E receipts + author aggregation NA decision required. See audit/environment_only_sensitivity/sensitivity_814_status.md and latest HANDOFF_LOG entry. Preserve originals; no ordinary FULL reruns.\n')
print('APPEND_ONLY_HANDOFF_AND_CONTINUATION_UPDATED')
