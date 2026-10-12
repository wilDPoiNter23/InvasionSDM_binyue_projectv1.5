# Original source: build_summary_files.py
# Original source location: ${NON_NATIVE_ROOT}/00_audit/phase1b/build_summary_files.py
# Original source SHA-256: fd48df581e4900b9f0d57e4fe1e788cab079d0fa04a8e1d2a01bc562b0a7d92c
# Recovery: live read-only SSH, 2026-10-12
# Classification: SUPPORTING (HISTORICAL_SUPPORTING_AUDIT_LINEAGE_HELPER)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/usr/bin/env python3
"""Build count_transition_summary.csv and species_set_differences.csv"""
import os, csv

OUT = os.path.dirname(os.path.abspath(__file__))

# ── count_transition_summary.csv ──
transitions = [
    {
        'transition_id': 'T01',
        'from_stage': 'accepted_species_list',
        'from_count': '813',
        'to_stage': 'models_dir',
        'to_count': '813',
        'delta': '0',
        'delta_explanation': 'All 813 accepted species have model directories on HPC',
        'evidence_source': 'models/ directory listing (813 dirs + Rplots.pdf + spatial_thin_log.txt)',
        'verified': 'yes'
    },
    {
        'transition_id': 'T02',
        'from_stage': 'models_dir',
        'from_count': '813',
        'to_stage': 'ensemble_models_out',
        'to_count': '675',
        'delta': '-138',
        'delta_explanation': '138 species have only .models.out (no ensemble.models.out); these used individual algorithm outputs only',
        'evidence_source': 'models/*/ directory file listing',
        'verified': 'yes'
    },
    {
        'transition_id': 'T03',
        'from_stage': 'models_dir',
        'from_count': '813',
        'to_stage': 'ensemble_models_rds',
        'to_count': '813',
        'delta': '0',
        'delta_explanation': 'All 813 species have RDS files in results/ensemble_models/ (1594 total: 813 best_ensemble + 781 ensemble)',
        'evidence_source': 'results/ensemble_models/ directory listing',
        'verified': 'yes'
    },
    {
        'transition_id': 'T04',
        'from_stage': 'models_dir',
        'from_count': '813',
        'to_stage': 'individual_models_rds',
        'to_count': '813',
        'delta': '0',
        'delta_explanation': 'All 813 species have individual model RDS files',
        'evidence_source': 'results/individual_models/ directory listing',
        'verified': 'yes'
    },
    {
        'transition_id': 'T05',
        'from_stage': 'ensemble_models_rds',
        'from_count': '813',
        'to_stage': 'projections',
        'to_count': '756',
        'delta': '-57',
        'delta_explanation': '57 species have model RDS but no projection raster (58 models lack projections, 1 extra projection for species not in models dir)',
        'evidence_source': 'results/projections/ directory listing (756 files)',
        'verified': 'yes'
    },
    {
        'transition_id': 'T06',
        'from_stage': 'projections',
        'from_count': '756',
        'to_stage': 'projection_summary',
        'to_count': '737',
        'delta': '-19',
        'delta_explanation': '19 species have projection files but were excluded from the final projection_summary.csv (and thus from the pressure raster)',
        'evidence_source': 'results/evaluations/projection_summary.csv (737 rows)',
        'verified': 'yes'
    },
    {
        'transition_id': 'T07',
        'from_stage': 'projections',
        'from_count': '756',
        'to_stage': 'projections_by_type',
        'to_count': '654 ensemble + 102 fallback',
        'delta': '0',
        'delta_explanation': '654 ensemble_projection.rds + 102 fallback_projection.rds = 756 total',
        'evidence_source': 'results/projections/ filename analysis',
        'verified': 'yes'
    },
    {
        'transition_id': 'T08',
        'from_stage': 'projection_summary',
        'from_count': '737',
        'to_stage': 'summary_by_type',
        'to_count': '653 ensemble + 84 fallback',
        'delta': '0',
        'delta_explanation': 'projection_summary.csv: 653 ensemble + 84 fallback = 737. Note: 1 species with ensemble projection was classified as fallback in summary, 18 fallback projections excluded from summary.',
        'evidence_source': 'results/evaluations/projection_summary.csv content analysis',
        'verified': 'yes'
    },
    {
        'transition_id': 'T09',
        'from_stage': 'projection_summary',
        'from_count': '737',
        'to_stage': 'pressure_raster',
        'to_count': '<= 735',
        'delta': '>= -2',
        'delta_explanation': 'REcombine_job.out reports 2 missing rasters out of 737; effective contribution <= 735 species. Exact count requires R3 (HPC stdout).',
        'evidence_source': 'REcombine_job.out log (referenced in Phase 1 audit); stack_failed_species_log.xlsx exists on HPC',
        'verified': 'partial'
    },
    {
        'transition_id': 'T10',
        'from_stage': '29_not_evaluated',
        'from_count': '29',
        'to_stage': 'models_dir',
        'to_count': '29',
        'delta': '0',
        'delta_explanation': 'CRITICAL FINDING: All 29 "accepted-but-not-evaluated" species HAVE model directories on HPC. 20 of 29 also have projection rasters. The evaluation gap was a Windows-side artifact.',
        'evidence_source': 'Cross-reference of 29-list against models/ and projections/ on HPC',
        'verified': 'yes'
    },
]

csv_path = os.path.join(OUT, "count_transition_summary.csv")
with open(csv_path, 'w', newline='') as f:
    writer = csv.DictWriter(f, fieldnames=[
        'transition_id', 'from_stage', 'from_count', 'to_stage',
        'to_count', 'delta', 'delta_explanation', 'evidence_source', 'verified'
    ])
    writer.writeheader()
    writer.writerows(transitions)
print(f"count_transition_summary.csv: {len(transitions)} transitions written")

# ── species_set_differences.csv ──
matrix_path = os.path.join(OUT, "species_lineage_matrix.csv")
rows = []
with open(matrix_path) as f:
    reader = csv.DictReader(f)
    for row in reader:
        rows.append(row)

in_models_not_proj = [r for r in rows if r['in_models_dir'] == 'True' and r['in_projections'] == 'False']
in_proj_not_summary = [r for r in rows if r['in_projections'] == 'True' and r['in_projection_summary'] == 'False']
in_summary_not_proj = [r for r in rows if r['in_projection_summary'] == 'True' and r['in_projections'] == 'False']

differences = []
diff_id = 0

for r in in_models_not_proj:
    diff_id += 1
    notes_parts = ['Model exists but no projection raster generated']
    if r['has_ensemble_models_out'] == 'False':
        notes_parts.append('no ensemble model (individual-only species)')
    if r['in_29_not_evaluated'] == 'True':
        notes_parts.append('in 29 not-evaluated list')
    differences.append({
        'difference_id': f"D-{diff_id:03d}",
        'set_a': 'models/',
        'set_b': 'projections/',
        'direction': 'in_A_not_B',
        'species': r['species'],
        'has_ensemble_model': r['has_ensemble_models_out'],
        'in_29_list': r['in_29_not_evaluated'],
        'selected_output_type': r['selected_output_type'],
        'notes': '; '.join(notes_parts)
    })

for r in in_proj_not_summary:
    diff_id += 1
    notes_parts = ['Projection raster exists but excluded from final summary (not in pressure raster)']
    if r['in_29_not_evaluated'] == 'True':
        notes_parts.append('in 29 not-evaluated list')
    differences.append({
        'difference_id': f"D-{diff_id:03d}",
        'set_a': 'projections/',
        'set_b': 'projection_summary',
        'direction': 'in_A_not_B',
        'species': r['species'],
        'has_ensemble_model': r['has_ensemble_models_out'],
        'in_29_list': r['in_29_not_evaluated'],
        'selected_output_type': r['projection_type'],
        'notes': '; '.join(notes_parts)
    })

for r in in_summary_not_proj:
    diff_id += 1
    differences.append({
        'difference_id': f"D-{diff_id:03d}",
        'set_a': 'projection_summary',
        'set_b': 'projections/',
        'direction': 'in_A_not_B',
        'species': r['species'],
        'has_ensemble_model': r['has_ensemble_models_out'],
        'in_29_list': r['in_29_not_evaluated'],
        'selected_output_type': r['summary_type'],
        'notes': 'In summary but projection file missing on HPC'
    })

diff_path = os.path.join(OUT, "species_set_differences.csv")
with open(diff_path, 'w', newline='') as f:
    writer = csv.DictWriter(f, fieldnames=[
        'difference_id', 'set_a', 'set_b', 'direction', 'species',
        'has_ensemble_model', 'in_29_list', 'selected_output_type', 'notes'
    ])
    writer.writeheader()
    writer.writerows(differences)
print(f"species_set_differences.csv: {len(differences)} differences written")

print(f"\nSet differences summary:")
print(f"  models/ - projections/ = {len(in_models_not_proj)} species")
print(f"  projections/ - summary  = {len(in_proj_not_summary)} species")
print(f"  summary - projections/   = {len(in_summary_not_proj)} species")
