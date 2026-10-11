# HPC accepted-run lineage audit

Audit date: 2026-10-11. Read-only remote inspection; no model refitting, raster copying, code changes, manuscript edits, or job submission. Only the three requested audit files were written locally.

## Answers to the seven priority questions

1. **MOST LIKELY — model branch:** predominantly the historical batch/block family (`0401batch_modeling_config.r` / `040101batch2.0.r` / `040102restbatch.r`), with heterogeneous saved CV/PA settings, cached objects and subsequent projection repair. The saved pilot model has GLM/RFd/MAXNET and a four-way held-out partition, but the full census also contains three- and five-RUN objects: it does not support a single uniform branch. **UNCLEAR:** the exact script, revision and invocation that generated each historical species object. Original generating scripts are outside the two permitted roots and were not inspected. A script filename alone cannot resolve this.
2. **VERIFIED — handoff file identity; MOST LIKELY — submitted response:** `${WORK_ROOT}/results/hotspot_mean_suitability_normalized2.0.tif`, 47,256,803 bytes, mtime UTC `2025-07-10T17:17:37+00:00`, SHA-256 `e4f9011ac720471135e7f73f245558cc62522291ca664700180ebf4310e31680`. Its current HPC hash equals the recorded local-input hash in the in-scope source inventory (S013). The later 813 and native-masked products are different generations and are not established as replacements in the submitted city/SEM/GAM analyses.
3. **VERIFIED — reclamation is an actual fitted-model term; MOST LIKELY — contribution to the historical submitted response.** The saved selected Abutilon grandifolium PA1_RUN1_GLM includes `reclamation_raster` and `I(reclamation_raster^2)` in its fitted GLM terms/coefficients. Its current fallback projection object selects that model and the retrospective historical manifest includes its projection. Across the 737 historical-ledger species, 298 current saved model predictor sets include reclamation. This is stronger than a candidate-variable list. **UNCLEAR:** the exact 2025 contributing-species manifest/versions, so a mathematically conclusive count of reclamation-using contributors to the submitted raster cannot be claimed. The raster's recorded band description names this GLM but is stale metadata, not an independent writer ledger.
4. **VERIFIED — actual socioeconomic input names:** saved model metadata includes GDP, population, built-up, night-light variables and reclamation in species-specific subsets; these are enumerated in `hpc_actual_predictors.csv`. Direct fitted-GLM confirmation for Abutilon includes `GDP2020_2.5min`, `Pop2020_2.5min`, `BUILT_S_E2025_2.5min` and reclamation. There is no basis to describe the whole index as an SDM response independent of these human predictors. Full metadata-census counts are reported below, separately for historical-ledger candidates and later additions.
5. **VERIFIED — pilot realised settings:** one random PA replicate, 1,000 background/PA rows, 504 occurrence rows, four held-out RUN columns partitioning all 1,504 rows once, with additional allRun full-data columns. Occurrence coordinates have nearest-neighbour minimum 9.991725 km (spherical radius 6,371 km); four occurrence points are below exactly 10 km. This supports approximately 10-km thinning, not a universal strict >=10-km assertion. **MOST LIKELY:** spatial/block CV from fold geography. **VERIFIED heterogeneity:** 736 calibration matrices inspected: 715 have four RUNs, 10 have three, 11 have five; PA strategies are random (668) or disk (68), one replicate. Diodia teres calibration was not reread. RUN count alone does not prove a spatial CV strategy. **UNCLEAR:** exact strategy for every row and universal occurrence gate.
6. **UNCLEAR — range masking of the submitted 2025 raster.** No executed species-mask lineage was established for that hash-matched product. **VERIFIED — a separate later masking workflow exists:** August 2026 non-native manifest/products, inverse-mask script, input/output checksums and QC records. It is not evidence that the 2025 response was masked.
7. **UNCLEAR / CONFLICT — unresolved:** exact final writer invocation; 735 versus 737 effective inputs/denominator; historical immutable projection/model versions; 249 absent paths in a retrospective 737-row manifest; continuous versus TSS-binary layer selection; original script identity; and exact execution lineage from the matched local raster to the city extraction, reclaimed comparison, SEM and GAM tables/figures. These are provenance gaps, not reasons to rerun models in this audit.

## Evidence scope and interpretation

Permitted project roots:

- `${WORK_ROOT}`
- `${NON_NATIVE_ROOT}`

Project searches and scientific-object reads stayed within these roots, with path resolution rejecting external targets. SSH configuration and installed R/biomod2 dependencies were used solely to connect/read serialized objects; external project scripts/data were not audited. The local analyses and rasters outside the permitted roots were not re-opened: their paths and hashes below come from records already inside the permitted roots.

Status definitions: **VERIFIED** denotes direct current object/file evidence, or an explicitly described hash identity. **MOST LIKELY** denotes supported attribution without a complete immutable writer chain. **UNCLEAR** means the evidence does not establish the claim. **CONFLICT** means records/objects support incompatible versions/counts. A later manifest's `accepted`, `lineage_pass` or `QC_pass` label is not itself proof of submitted use.

## Backward chain from the response

### Hash-identified handoff

`${WORK_ROOT}/results/hotspot_mean_suitability_normalized2.0.tif`

→ recorded local copy `${LOCAL_ANALYSIS_ROOT}\data\data\hotspot_mean_suitability_normalized2.0.tif`

→ likely city/reclamation summaries

→ recorded city table `${LOCAL_ANALYSIS_ROOT}\FinalResults\data\10_03_108city_mean_variables_recheck.csv`

→ requested consumers `0602sem108cities.r`, `0605gam.r`, `060503scale_dependentEffect.r` (**UNCLEAR as an exact execution chain in this scoped audit**).

The current HPC raster was independently SHA-256 hashed. S013 in `${NON_NATIVE_ROOT}/00_audit/source_file_inventory.csv` records the identical local-copy hash and size. This verifies identity to that recorded snapshot, not a fresh check of the current local file. S013 records EPSG:4326, 8640 x 4320, 2.5 arcmin, Float32, all 37,324,800 cells finite and range 0–0.766795516. These raster statistics were not recomputed here.

S015/M022 record the 108-row, 53-column city table; its recorded SHA-256 is `83068AD78CC72C70B1524A077CAE2D3C032FE21497D2F58AE115B64DAE625501`. They do not establish the exact extraction code, polygon version, extracted columns or the execution that fed each final SEM/GAM/scale plot. Targeted searches of the in-root audit/analysis records did not locate the three named local consumer scripts. Therefore those links remain explicitly UNCLEAR.

### Historical aggregate -> species projections

Supporting in-scope records:

- `${NON_NATIVE_ROOT}/00_audit/model_output_inventory.csv` (M020)
- `${NON_NATIVE_ROOT}/00_audit/phase1b/final_run_candidates.csv` (C001/C002)
- `${WORK_ROOT}/publication_release_v1/audits/original_aggregation_protocol_frozen.txt`
- `${WORK_ROOT}/publication_release_v1/manifests/original_frozen_aggregation_input_manifest.csv`

M020/C001/C002 attribute normalized2.0 to `040501REsearch.r` plus `REcombine_job.out`, with 737 listed species, 735 apparent successes, two interleaved invocations and 61 duplicate processing lines. These are prior audit records; the underlying local log/script were not independently reread outside scope. The protocol instead names outside-root `0405Allstack.r` and output `hotspot_mean_suitability_normalized.tif` (without `2.0`). **CONFLICT:** that protocol cannot establish the exact normalized2.0 writer.

The current `${WORK_ROOT}/results/evaluations/projection_summary.csv` has 737 unique species (653 ensemble, 84 fallback), but mtime is `2026-08-01T16:39:18+00:00`, after the 2025 aggregate. The retrospective original manifest was written in August 2026. It has 737 rows, all marked included, but only 488 nonempty paths: 405 `.grd` paths containing `TSSbin`, 83 `.tif` paths, and **249 `NA` paths**. Its inclusion labels therefore do not constitute a recoverable exact 2025 input ledger. A direct `.grd` header check for historical Sporobolus alterniflorus reports INT2S, range 0–1 and 4320 x 8640. Its paired 74,649,600-byte `.gri` was sampled at 1,001 evenly spaced cells: 670 NoData (-9999), 319 zero, 12 one. This verifies binary sample values for this actual layer, not the full 405-layer inventory. `.grd` is a header with a paired `.gri` pixel payload: a checksum of the header alone does not fingerprint the complete raster. This audit did not hash every paired payload.

### Projection -> fitted model -> calibration: directly checked example

Species: Abutilon grandifolium.

- Current projection object: `${WORK_ROOT}/results/projections/Abutilon grandifolium_fallback_projection.rds`; `models.projected` selects `Abutilon.grandifolium_PA1_RUN1_GLM`.
- Projection listed in historical manifest: `${WORK_ROOT}/models/Abutilon.grandifolium/proj_fallback_projection/individual_projections/proj_fallback_projection_Abutilon.grandifolium_PA1_RUN1_GLM.tif`.
- Actual individual fitted object: `${WORK_ROOT}/models/Abutilon.grandifolium/models/Abutilon grandifoliumMultiModel_Ensemble/Abutilon.grandifolium_PA1_RUN1_GLM` (640,083 bytes; mtime UTC 2025-06-13T10:33:15).
- Parent fitted-model metadata: `${WORK_ROOT}/models/Abutilon.grandifolium/Abutilon.grandifolium.Abutilon grandifoliumMultiModel_Ensemble.models.out`.
- Saved calibration: `${WORK_ROOT}/models/Abutilon.grandifolium/.BIOMOD_DATA/Abutilon grandifoliumMultiModel_Ensemble/calib.lines`.
- Saved formatted input: same `.BIOMOD_DATA` directory, `formated.input.data`.

Actual predictors (13): `wc2.1_2.5m_bio_13;wc2.1_2.5m_bio_14;wc2.1_2.5m_bio_15;wc2.1_2.5m_bio_18;wc2.1_2.5m_bio_19;wc2.1_2.5m_bio_2;wc2.1_2.5m_bio_3;wc2.1_2.5m_bio_5;BUILT_S_E2025_2.5min;GDP2020_2.5min;Pop2020_2.5min;reclamation_raster;wc2.1_2.5m_elev`.

Both parent `expl.var.names` and fitted individual `expl_var_names` were read. The fitted GLM `terms` and coefficient names independently confirm reclamation, GDP, population and built-up, including their quadratic terms. No candidate stack or selection-summary label was used to establish membership.

Calibration columns: `_PA1_RUN1;_PA1_RUN2;_PA1_RUN3;_PA1_RUN4;_allData_allRun;_PA1_allRun`. The four RUN validation sizes are 139, 168, 347, 850; every row is held out once. The full-data allRun columns are not additional validation folds. Fold coordinate ranges show geographic quadrants rather than evidence for random five-fold CV.

PA table has one column with 1,504 selected rows, **including the 504 presences**. Thus 1,504 is not the PA count: actual background/PA rows = 1,000. CSV `PA_selected_counts` means selected table rows, not absence-only counts.

Logs in `${WORK_ROOT}/logs/2025-06-12modeling_Batch_1.log` contain multiple attempts: lines 14–17 report 1,701 -> 532 followed by failure; lines 742–746 report 1,571 -> 504 and successful formatting. Lines 751–755 report fitted models and TSS >0.7 plus ROC >0.7 fallback selection. Across this log there are 10 messages for TSS >0.8 and ROC >0.8, and 7 fallback messages for >0.7/>0.7. These are runtime thresholds for those attempts, not proof that every final selected model passed the same gate. Direct evaluation of the currently selected Abutilon RUN1 GLM gives calibration TSS 0.864 / ROC 0.971, but validation TSS 0.544 / ROC 0.864. Therefore the selected object does not support a blanket claim of validation TSS >0.7 or >0.8. Which metric partition the historical selector used remains UNCLEAR. Earlier fallback logs can name RUN4 whereas the current projection object selects RUN1: use the current object for current identity, and retain the historical mismatch.

## Metadata census (current saved objects)

**VERIFIED current model metadata:** 814 distinct species-name rows: 737 historical-ledger candidates and 77 later-only entries. All predictor sets were read from saved model metadata, after replacing 13 stale historical model locators with actual gap13 repair RDS objects and replacing the stale Spartina locator with its directly read clean model.

| Actual saved predictor used | Historical-ledger candidates (737) | Later-only entries (77) |
|---|---:|---:|
| reclamation | 298 | 6 |
| GDP | 724 | 47 |
| population | 690 | 65 |
| night lights / VIIRS | 168 | 70 |
| built-up | 735 | 63 |

These are current object-level counts, **not counts proved to have entered the 2025 final writer**. Human variables are not necessarily present together. Exact names are retained in the CSV.

| Realised calibration/PA metadata: historical candidates | Count |
|---|---:|
| 4 RUN columns, random PA, 1 replicate | 668 |
| 4 RUN columns, disk PA, 1 replicate | 47 |
| 3 RUN columns, disk PA, 1 replicate | 10 |
| 5 RUN columns, disk PA, 1 replicate | 11 |
| Calibration/PA not reread: historical-only Diodia teres | 1 |

The algorithm union of historical saved computed models is GLM, RFd, MAXNET. 711 objects have all three, 17 have GLM/RFd, 6 GLM/MAXNET, 3 MAXNET only. This is the successfully built-model inventory, not proof of the requested algorithms or selected ensemble composition at every historical invocation.

For the 736 formatted objects inspected, `PA_selected_counts - presence_n` is 1,000 in 693 rows and 22–532 in 43 rows. This is a derived table-count difference, not a new universal PA-generation setting: PA.table includes presences, as directly verified in the pilot. Thus a universal 1,000 realised PA assertion is unsupported, particularly for disk objects. The full CSV retains the observed counts and strategy rather than replacing them with a script default.

**CONFLICT — later repair metadata:** actual gap13 repair objects contain `ID` in 10 of 13 predictor sets: Acer platanoides, Adenostoma fasciculatum, Amaranthus viridis, Cecropia peltata, Cryptostegia madagascariensis, Lepidium draba, Metrosideros excelsa, Panicum dichotomiflorum, Sonchus oleraceus, Vachellia farnesiana. The earlier locator audit pointed to their older historical parents and did not establish these actual repair predictor sets. These 10 belong to later-only entries, not the historical 737 set; no claim is made that ID entered the submitted 2025 response. The later released projection's exact component/byte-version linkage still needs to be established before treating a clean-release label as verified. No repair or refitting was performed here.

The GAP75 34/35 predictor-stack labels are not fitted-variable counts. Actual per-species output manifests report selected historical PA_RUN_GLM/RFd/MAXNET components and smaller input subsets, e.g. Physalis lanceifolia 15 and Plantago aristata 18. Their saved fitted metadata was used rather than declaring all 34/35 stack layers as predictors.

The August11 locator audit contains a stale pre-repair Spartina alterniflora object (36 predictors including ID). The actual clean replacement `${WORK_ROOT}/results/Spartina_noID_final_v1/Spartina_alterniflora_clean_noID_models.rds` was independently loaded: BIOMOD.models.out, 35 predictors, no ID, with reclamation/GDP/population/built-up/VIIRS. The final CSV replaces the stale row with that actual object; unverified calibration/PA fields for this replacement are left UNCLEAR. August13 v2 and v3 projection manifests use different clean projection filenames/scale variants for this taxon. Historical Sporobolus alterniflorus and later Spartina alterniflora are both separate rows in these ledgers; they must not be silently merged during lineage counting.

Extraction was first validated on ten objects, then applied to the 813 initial model locators, then extended to historical-only Diodia teres and corrected for actual 13-species repair objects plus the clean Spartina replacement from `${WORK_ROOT}/publication_release_v1/audits/final_813_predictor_contamination_audit.csv`. That census file supplied locators only; actual predictor names were read anew from saved model/model-projection metadata. Serialized objects were loaded without fitting; unusually large objects were capped at 100 MiB and unsupported/missing metadata remains UNCLEAR.

The 737-row historical manifest and 813-row later ledger intersect in only 736 names: historical Diodia teres is absent from the latter. The final CSV therefore contains **814 rows**: all 737 historical candidates plus 77 later-only names. Two rows labelled ORIGINAL_FROZEN by the later manifest (Adiantum latifolium and Musa ornata) are absent from the historical manifest, while historical Opuntia ficus-indica is labelled GAP75_DIRECT34_V1 later. Source-generation labels and historical membership are distinct; the CSV retains both. This is a **candidate lineage partition**, not acceptance of all 813 as contributors to the 2025 index. Parent model predictor metadata establishes fitted-model input variables; only the stated Abutilon example was also checked at fitted GLM term/coefficients level. Algorithm-specific term selection and causal independence cannot be inferred for every component from parent metadata alone.

CSV `accepted_model_object` is an object locator requested by the user, not a claim that its current bytes are proven to have produced the submitted raster. `metadata_status=VERIFIED` applies to extracted predictor metadata, not submitted membership or QC acceptance. `projected_model` identifies current projection selections when available. Unknown membership/input paths are retained as UNCLEAR rather than dropping species.

## Index calculation and denominator

| Question | Evidence and status |
|---|---|
| Species membership | Current ledger: 737 = 653 ensemble + 84 fallback. Retrospective manifest: 488 paths + 249 NA. Historical final 735 apparent successes comes from in-root records of the earlier interleaved log. **CONFLICT / UNCLEAR exact set**. |
| Ensemble vs fallback | Current projection RDS names distinguish EMmean/EMmedian ensemble and individual PA_RUN_ALGO fallback. This was directly checked in ten pilots and in the metadata census where available. The current summary's 653/84 split is not a frozen 2025 writer proof. |
| Layer selection | Protocol takes `models.projected[1]`, recursively finds matching tif/grd, and uses first match. Original retrospective manifest has 405 TSSbin paths; later v3 has 384 TSSbin paths. **CONFLICT** with an unqualified all-continuous interpretation; exact historical layer selection is UNCLEAR. No all-raster value scan performed. |
| Normalisation | M020/C002/protocol describe per-species `(x-min)/(max-min)`, constant ->0. **MOST LIKELY historical rule**; exact writer unavailable. |
| NA | Records describe NA ->0 before accumulation. Hash-matched raster's prior audit records all cells finite. **MOST LIKELY historical rule**. Ocean zero is not evidence of range masking. |
| Denominator | Described constant successful-input count, not per-cell non-NA count. **UNCLEAR 735 vs737** and exact writer counter state. |
| Failed species | Missing/read-failed species are described as skipped. Whether other caught accumulation failures still incremented the historical denominator is **UNCLEAR**. A later in-root aggregate script increments after caught addition errors; this demonstrates a possible counter hazard, not its occurrence in the 2025 output. |
| Native/invasive range | No executed mask trace established for normalized2.0. Later mask products/QC are a separate generation. **UNCLEAR historical masking**. |

## Later products: keep separate from submitted response

`hpc_output_lineage.csv` inventories plausible historical rasters/backups, later 813 aggregates, release/summary records, and the 488 retrospective historical projection paths. Per-species September variant input rasters were omitted from the final compact inventory; aggregate/QC/manifest records are retained. Dates are UTC filesystem mtimes, not assumed run or acceptance dates. Writer fields are supplied only when traced to an output statement or explicitly labelled historical attribution. Existence and declared QC do not establish submitted use.

Important distinct generations:

- July 29 813 final, negative-TSS sensitivity, strict-rescue and quality-improved aggregates.
- August 7 publication aggregate; August 10 full-coverage aggregate.
- August 13 clean 813 global aggregate (current writer explicitly reads `final_aggregation_input_813_v2.csv`, not merely a v3 manifest label): `${WORK_ROOT}/results/aggregation_publication_813_clean_v1_20260813/hotspot_mean_suitability_normalized.tif`.
- Later non-native 813: `${NON_NATIVE_ROOT}/02_derived_non_native_potential/04_aggregate/rasters/multispecies_non_native_invasion_potential_813.tif` (41,919,567 bytes; current inventory mtime UTC 2026-08-16T22:44:18).
- Supplementary 814 aggregates and September 17 binary/continuous variants, with different normalisation/denominator semantics. They are not interchangeable response surfaces.

The August non-native workflow has actual per-species product paths/checksums in `03_species_non_native_projections/manifests/non_native_projection_manifest_813_v1.csv`, `09_mask_one_taxon.R` inverse masking, and `04_aggregate/qc/non_native_aggregate_QC.csv`. QC records 813 inputs, 37,324,800 finite cells, maximum 0.679676480764142 and SHA-256 `78d20efa3e508744d8cf7ee518f9cc347dd43ff3ea6597ee8bdf598756d0cdef`; these are producer QC records, not newly computed independent spatial QC. The manifest/provenance distinguishes 810 masked taxa plus three documented global-equal cases. Script `11_aggregate_non_native.R` normalises the masked species rasters, fills NA with zero, and divides by its processed count. Native-domain species still count in that constant denominator. September scripts `34_aggregate_variants.R`/`36_aggregate_variants_814.R` instead use no min-max and a per-cell available-taxon mean with native domains NA. Both differ from the historical global response.

## What would close the remaining lineage gaps

A contemporaneous final writer log/process identity plus a hash-bearing species/layer input manifest tied to the matched normalized2.0 raster would resolve the exact contributors, layer type and denominator. The archived local extraction script/invocation and final SEM/GAM input-table fingerprints would resolve the remaining downstream links. These were not present in the inspected in-scope records; this audit did not reconstruct or substitute a new response.