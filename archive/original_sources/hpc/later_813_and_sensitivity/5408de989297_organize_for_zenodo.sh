# Original source: organize_for_zenodo.sh
# Original source location: ${WORK_ROOT}/src/utils/organize_for_zenodo.sh
# Original source SHA-256: 5408de989297ad93302785e860a968de13444387e710ed730066ef67b01122e6
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/bin/bash
# organize_for_zenodo.sh — Reorganize invasionSDM_binyue for Zenodo publication
# Run from: <WORK_ROOT>
set -euo pipefail
cd <WORK_ROOT>

echo "=== Reorganizing invasionSDM_binyue for Zenodo ==="
echo ""

# ── Create new directory structure ──────────────────────────────────────────
mkdir -p src/modeling
mkdir -p src/analysis
mkdir -p src/repair
mkdir -p src/slurm
mkdir -p src/utils
mkdir -p docs/audit
mkdir -p archive/v1_deprecated
mkdir -p archive/slurm_old
mkdir -p archive/coastal_stage2
mkdir -p results/repair_pilot_v2/logs
mkdir -p results/repair_pilot_v2/tmp

# ── CORE MODELING SCRIPTS ───────────────────────────────────────────────────
echo "Moving core modeling scripts..."
cp -n data/script/0401batch_modeling_config.r src/modeling/ 2>/dev/null || true

# ── ANALYSIS SCRIPTS ────────────────────────────────────────────────────────
echo "Moving analysis scripts..."
mv -n extract_evaluations_v2.R src/analysis/ 2>/dev/null || true
mv -n full_region_audit.R src/analysis/ 2>/dev/null || true
mv -n full_target_validation.R src/analysis/ 2>/dev/null || true
mv -n benchmark_20deg.R src/analysis/ 2>/dev/null || true

# ── REPAIR PILOT SCRIPTS (current active work) ──────────────────────────────
echo "Moving repair pilot scripts..."
mv -n repair_pilot_preflight_v2.r src/repair/repair_pilot_preflight.r 2>/dev/null || true
mv -n repair_pilot_modeling_v2.r src/repair/repair_pilot_modeling.r 2>/dev/null || true
mv -n targeted_5sp_repair.r src/repair/ 2>/dev/null || true
mv -n factorial_13sp_attrition.r src/repair/ 2>/dev/null || true
mv -n fast_13sp_audit.r src/repair/ 2>/dev/null || true
mv -n audit_13sp_retention.r src/repair/ 2>/dev/null || true

# ── SLURM SCRIPTS ───────────────────────────────────────────────────────────
echo "Organizing SLURM scripts..."
# Active/current SLURM scripts
mv -n slurm_5sp_repair_v2.sh src/slurm/repair_pilot.sh 2>/dev/null || true
mv -n slurm_5sp_repair.sh src/slurm/ 2>/dev/null || true

# Archive old SLURM scripts
for f in slurm_*.sh; do
  [ -f "$f" ] && mv -n "$f" archive/slurm_old/ 2>/dev/null
done

# ── ARCHIVE DEPRECATED R SCRIPTS ────────────────────────────────────────────
echo "Archiving deprecated R scripts..."
deprecated=(
  repair_single_species.r
  repair_single_species_v2.r
  extract_evaluations.R
  run_aggregation_813.R
  run_aggregation_813_fixed.R
  run_aggregation_813_optimized.R
  run_aggregation_final.R
  run_improve_one_species.R
  run_negative_tss_reproject.R
  run_quality_improved_aggregate_v1.R
  run_sensitivity_aggregation.R
  run_strict_rescue_aggregate.R
  manual_ensemble_project.r
  proper_ensemble_project.r
  rebuild_ensemble_and_project.r
  rebuild_ensemble_v2.r
  regional_dry_run.R
  reproject_single_species.r
  build_coastal_derived_rasters.R
  classify_coastal_association.R
  production_global_marine_coverage.R
)
for f in "${deprecated[@]}"; do
  [ -f "$f" ] && mv -n "$f" archive/v1_deprecated/ 2>/dev/null
done

# ── ARCHIVE COASTAL ANALYSIS ────────────────────────────────────────────────
echo "Archiving coastal analysis..."
if [ -d "COASTAL_DRIVER_ANALYSIS_ONLY_NOT_FOR_GLOBAL_TERRESTRIAL_SDM" ]; then
  mv -n COASTAL_DRIVER_ANALYSIS_ONLY_NOT_FOR_GLOBAL_TERRESTRIAL_SDM/* archive/coastal_stage2/ 2>/dev/null || true
  rmdir COASTAL_DRIVER_ANALYSIS_ONLY_NOT_FOR_GLOBAL_TERRESTRIAL_SDM 2>/dev/null || true
fi

# ── ORGANIZE AUDIT FILES ───────────────────────────────────────────────────
echo "Organizing audit files..."
if [ -d "audit" ]; then
  # Move all audit CSVs to docs/audit/
  for f in audit/*.csv audit/*.md; do
    [ -f "$f" ] && mv -n "$f" docs/audit/ 2>/dev/null
  done
  rmdir audit 2>/dev/null || true
fi

# ── CREATE LOGS SYMLINK ─────────────────────────────────────────────────────
echo "Linking logs..."
# Keep logs/ as-is (contains SLURM outputs), just ensure it exists
mkdir -p logs

echo ""
echo "=== DONE ===" 
echo "New structure:"
echo "  src/modeling/     — Core SDM modeling scripts"
echo "  src/analysis/     — Analysis and validation scripts"
echo "  src/repair/       — Negative-TSS repair pilot"
echo "  src/slurm/        — Active SLURM submission scripts"
echo "  docs/audit/       — Audit reports and diagnostics"
echo "  archive/          — Deprecated scripts (v1, old SLURM, coastal stage2)"
echo "  data/             — Input data (unchanged)"
echo "  results/          — Model outputs (unchanged)"
echo "  models/           — Model objects (unchanged)"
echo "  logs/             — Log files"
echo ""
echo "Remaining in root:"
ls -1 *.r *.sh *.R *.zip 2>/dev/null || echo "  (none — clean!)"
