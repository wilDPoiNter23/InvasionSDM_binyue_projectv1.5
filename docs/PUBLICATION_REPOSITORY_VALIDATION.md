# Publication repository validation

Validation date: 2026-10-11. Target repository: `wilDPoiNter23/InvasionSDM_binyue_projectv1.5`; working branch: `codex/publication-code-integration`. Integration base: `141bca2d60ee9bdc6f9572131d07a76debb0858b`. This is a publication integration candidate with explicit scientific blockers, not a certified end-to-end reproduction.

| Status | Check | Result and boundary |
|---|---|---|
| PASS | Correct repository/branch | Exact origin checked before editing; old differently named checkout left untouched; no main merge/history rewrite |
| PASS | Inventory before promotion | 44 original public R candidates, 263 scoped HPC scripts, 10 local scripts/notes inventoried before copying; former shell startup file separately preserved and recorded |
| PASS | Canonical R syntax | 12 scientific implementation/reference scripts plus driver, two config scripts and compatibility config: 16 R files parsed with R 4.5.1 |
| PASS | Scientific expression preservation | All 12 cleaned implementations compared against source R parse trees after normalizing only absolute-path strings, documented service-username configuration, omitted setup expressions and new preflight guards; all matched |
| PASS | Shell syntax | Slurm example passed remote `bash -n` with LF bytes on stdin; no job submitted |
| PASS | Cheap smoke tests | Stage listing succeeds without data; missing private config gives a clear expected error; isolated path/bootstrap checks below require no scientific inputs |
| PASS | Stage catalog | All 12 script entries exist; each has input/output descriptions; packages and I/O expressions have a static ledger |
| PASS | Publication hygiene | Canonical code, archive, docs, metadata and config scanned for private machine prefixes, original service username, common token/key patterns and disallowed binary extensions; no newly published matches |
| PASS | Small metadata | 814 unique species keys and 40 observed predictor names; product scope and evidence categories preserved; largest public-safe text file approximately 1.19 MB |
| PASS | Navigation | Non-archival workflow Markdown file links checked; historical audit references to external records are evidence locators, not distributed inputs |
| WARNING | R startup locale | Installed Windows R reports unavailable C.UTF-8 locale categories; parsing and cheap checks still succeeded |
| WARNING | Dependency reproducibility | No verified historical lockfile/session-version set; parse success does not prove package API compatibility |
| WARNING | Archives | Intentionally non-executable symbolic-path snapshots; source defects/fragment incompleteness retained. Original anthropogenic-variable script has a comma parse defect; GAM archive retains a console launcher |
| WARNING | Existing Git history | 43 pre-existing blobs contain machine-specific paths/service username; none of the reachable pre-integration blobs exceeds 1 MB. See metadata/preexisting_history_hygiene.csv. .gitignore does not erase this history; no rewrite performed |
| BLOCKER | Exact submitted raster lineage | normalized2.0 raster SHA is verified by prior audit, but exact 2025 writer, contributors, raster representation, failures/denominator and native masking remain unresolved |
| BLOCKER | Final SEM/PCA/GAM source chain | Interactive workspace dependencies and final model choice are unresolved; complete 060503 scale-dependent source was not found in authorized roots |
| BLOCKER | Result-sensitive source defects | Occurrence export/database use and undefined city-effect write objects were documented without scientific repairs |
| BLOCKER | Methods correspondence | CV/PA/presence thresholds, species membership, SEM approach and predictor reuse need author reconciliation; executable formulas/settings retained |
| BLOCKER | Release readiness | Dataset releases/DOIs/redistribution terms, author software licence confirmation and an approved citation release remain outstanding |

Commands run included `Rscript examples/validate_structure.R`, `Rscript code/run_stage.R --list`, the expected-failure `--check` configuration smoke, private source-versus-public parse-tree comparisons and `bash -n`. The private comparison baselines and source cache remain in ignored working/. No models were fitted, projected or aggregated; no scientific raster/table computation or manuscript edit was performed.

The convenience driver blocks unresolved reference stages under `--run`. Directly sourcing a reference is not evidence of acceptance and can write outputs; inspect inputs and blockers first. A supplied WORKSPACE_RDS must have independently documented provenance rather than arbitrary objects invented to satisfy guards.

The branch can be reviewed and pushed with these limitations visible. These blockers prevent an unqualified claim of full reproducibility or final publication readiness; they do not justify silently changing historical analyses.
