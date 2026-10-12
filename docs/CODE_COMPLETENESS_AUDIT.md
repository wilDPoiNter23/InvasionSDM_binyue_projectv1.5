# Code completeness audit

**PUBLICATION CODE COVERAGE: SUBSTANTIALLY COMPLETE**

Audit date: 2026-10-12. Branch: codex/publication-code-integration. The branch now exposes actual full source, not only a sparse framework. This judgement concerns inspectable source; it does not certify unattended reproduction, complete lineage or scientific acceptance. Specific writers/input versions remain unresolved, so coverage is not COMPLETE.

## Coverage accounting

The matrix has 28 manuscript outputs and 15 analysis-stage rows. These denominators overlap and are not 43 independent analyses.

| Scope | COMPLETE_REAL_SOURCE | PARTIAL_SOURCE | REFERENCE_ONLY | UNRESOLVED | MISSING | Total |
|---|---:|---:|---:|---:|---:|---:|
| Manuscript outputs | 20 | 4 | 0 | 3 | 1 | 28 |
| Analysis stages | 12 | 3 | 0 | 0 | 0 | 15 |
| Combined matrix | 32 | 7 | 0 | 3 | 1 | 43 |

COMPLETE_REAL_SOURCE means complete identified historical implementation/component code is inspectable. Original input, run identity and final version are separate lineage fields. Manual layout may remain unresolved when full statistical/component code exists; an unidentified statistical writer is not treated as complete. No reference-only reconstruction is counted as complete.

## Recovery and comparison

- 79 local source records: 72 full configured R copies, four damaged R originals as full text, three shell snapshots.
- 285 live-HPC source locations, 268 unique hashes. All 263 prior R/shell cached originals match live server bytes; 22 relevant Python helpers also recovered. Later 813/non-native/environment-only/QC sources remain historical/supporting archives.
- All 23 specifically requested filenames are represented. The seven named historical 0401/0402/0405 generators were recovered from the local code root; they are absent within the two permitted HPC project roots.
- Twelve earlier integration files were demoted to archive/prior_integration_references; the main catalog points to recovered source. Their previous version/authority was not established. See metadata/reference_demotion.csv.
- Original/raw SHA, public SHA, full snapshots and structural comparisons establish identity. Filename similarity is not equivalence evidence. Historical variants are retained separately; no shortened reconstruction replaces real source.

The previous source inventory recorded candidates and recommended destinations, not necessarily files committed to the branch. Baseline Git-tree checks now distinguish actual presence from recommendations. This was the main gap in the earlier integration. See metadata/source_branch_comparison.csv.

A/B/C/D/E comparison: files already structurally supported remain provenance references (A); partial/unconfirmed previous main-path implementations were demoted in favour of full real source (B); missing real source added (C); materially different batch/PCA/GAM/SEM versions retained separately (D); unidentified writers remain unresolved (E). The per-source manifest and output matrix are the detailed evidence.

## Major analysis status

| Analysis | Real source found? | In repository? | Final version identified? | Status |
|---|---|---|---|---|
| occurrence processing | Full historical implementations found | Yes | Unresolved/conflicting | COMPLETE_REAL_SOURCE / UNCLEAR |
| environmental stack | Full historical implementations found | Yes | Most likely; run metadata gap | COMPLETE_REAL_SOURCE / MOST_LIKELY |
| variable selection | Full historical implementations found | Yes | Most likely; run metadata gap | COMPLETE_REAL_SOURCE / MOST_LIKELY |
| SDM | Full historical implementations found | Yes | Most likely; run metadata gap | COMPLETE_REAL_SOURCE / MOST_LIKELY |
| projection | Full historical implementations found | Yes | Most likely; run metadata gap | COMPLETE_REAL_SOURCE / MOST_LIKELY |
| multispecies index | Full historical implementations found | Yes | Unresolved/conflicting | PARTIAL_SOURCE / UNCLEAR |
| city extraction | Full historical implementations found | Yes | Most likely; run metadata gap | COMPLETE_REAL_SOURCE / MOST_LIKELY |
| reclaimed/non-reclaimed analysis | Full historical implementations found | Yes | Most likely; run metadata gap | COMPLETE_REAL_SOURCE / MOST_LIKELY |
| PCA | Full historical implementations found | Yes | Unresolved/conflicting | COMPLETE_REAL_SOURCE / CONFLICT |
| SEM | Full historical implementations found | Yes | Unresolved/conflicting | PARTIAL_SOURCE / UNCLEAR |
| GAM | Full historical implementations found | Yes | Unresolved/conflicting | COMPLETE_REAL_SOURCE / CONFLICT |
| scale-dependent analysis | Full historical implementations found | Yes | Most likely; run metadata gap | COMPLETE_REAL_SOURCE / MOST_LIKELY |
| spatial diagnostics | Full historical implementations found | Yes | Unresolved/conflicting | COMPLETE_REAL_SOURCE / CONFLICT |
| city-effect calculations | Full historical implementations found | Yes | Most likely; run metadata gap | COMPLETE_REAL_SOURCE / MOST_LIKELY |
| figure generation | Full historical implementations found | Yes | Unresolved/conflicting | PARTIAL_SOURCE / UNCLEAR |

## Recheck GAM / scale dependence

Full 060503scale_dependentEffect.r and 010303_rerun_gam_after_boundary_update.R are in code/08_gam/real_sources, with dependencies 060504–060507. RECOVERED_CODE_SOURCE_ROOT configures this source directory independently of historical data/output root RECOVERED_CODE_ROOT. The obsolete missing-source note was archived after replacement verification.

Actual B and conditional-curve formula:

```r
risk ~ te(log_recl, area_km2, k=c(5,5)) +
  s(anthro_pc1,k=5) + s(climate_pc1,k=5) +
  s(soil_pc1,k=5) + s(marine_pc1,k=5)
# gam(..., method="REML")
```

This raw-area/marine specification contradicts the historical header's no-marine/Regions/log-area claim. Expressions were preserved; comments were not treated as authority. Recheck input is <LOCAL_ANALYSIS_ROOT>/FinalResults/data/gamtest_withpca_recheck.csv (SHA 619a138fa64b0a10082570707ccd12e580c4b9fd005cde2fb9762820d479dee2), with n=84, adjusted R²=.787874, deviance explained=83.3688%, AIC=−220.189470.

The 108-city area distribution supplies P85=5621.49087825 and P90=8531.9724488 km². Conditional-curve endpoint differences change sign between these areas. 5,621.5 is a rounded lower bracket endpoint, not an exact fitted breakpoint. Complete diagnostics, grids, conditional peaks and spatial calculations are preserved.

## PCA / SEM

060501 loading PCA includes reclamation in the anthropogenic group and aspect in geomorphic group, using mean imputation. 010303 recheck-score PCA excludes those variables, uses group complete cases and aligns signs to old scores. Both full implementations are present; conflict remains explicit.

Matching piecewise SEM historical source selects input interactively and contains consecutive alternative subsets. Original fit input/object/execution state remains unknown. Original preprocessing is damaged and preserved as sem_preprocessing_damaged.R.txt; no replacement SEM is invented. All 14 reported coefficient rows match author-held SEM2.0-zjy table1. 060502 consumes the expanded SEM table and hard-codes these coefficients, but does not prove the original fit input.

## Remaining gaps

- Main Fig. 1/2: some component/writer/selected composite versions unresolved; exact normalized2.0 index writer not proven.
- Supplementary Fig. 5: full candidate curve implementations present, selected final curves/data set unresolved.
- ED Table 3: no matching model/source identified.
- ED Table 4 and Supplementary Fig. 1: editorial inventory/diagram generator unresolved.
- Supplementary Table 6: 16 group counts/city lists match stored CSV, exact cross-tab writer unresolved.
- Supplementary Table 3 mixes versions; Main Fig. 4 uses old image/recheck numerical text; spatial Fig. 8/Table 4 use old n=83 chain.
- SEM exact original fitting input/object and interactive execution state unknown.

Manual figure assembly can remain unresolved without omitting statistical component code. Every gap is represented in metadata/manuscript_code_coverage.csv.

## Validation and review suitability

239 R source-record checks pass parse and scientific expression comparison against full public snapshots; private raw-original-to-public-snapshot comparison also passes after normalizing documented path/account substitutions and directory trailing slashes. Some HPC records are duplicate identical bytes. Seven damaged original R files remain full text snapshots, not passing entrypoints. No model source was evaluated and no scientific repair invented.

Static credential/private-path scans, original/public hash checks, source/link/catalog checks and staged-file allowlist are required before commit. No raw data, rasters, models or manuscripts are included. Parse success is not runtime or scientific acceptance; historical interactive/undefined object dependencies remain disclosed.

**Peer-review assessment:** substantially suitable to inspect recovered real implementations with explicit limitations. Not suitable to claim every result is computationally reproducible.

## HPC protection

All unique recovered relevant code hashes have full public-safe snapshots and private raw copies. Models, inputs, projected rasters, output tables, original execution records and accepted-run metadata remain protected. Private working/hpc_cleanup_after_code_recovery.csv marks preserved source as ARCHIVE_SMALL_SOURCE and project containers as KEEP_UNTIL_ACCEPTANCE. No DELETE_CANDIDATE and no deletion authorization are issued.

The response-curve source 0406 retains a historical export path missing its leading slash, represented as <HISTORICAL_RELATIVE_WORK_ROOT> for inspection. It was not silently repaired or promoted to a proven final writer. Slurm account locators are public placeholders in archived snapshots.
