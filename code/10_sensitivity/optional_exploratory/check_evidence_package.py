#!/usr/bin/env python3
"""Check saved numerical evidence, source preservation and required reporting."""
import argparse
import csv
import hashlib
import json
import math
from pathlib import Path


def rows(path):
    with path.open(encoding="utf-8-sig", newline="") as handle:
        return list(csv.DictReader(handle))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-root", type=Path, required=True)
    args = parser.parse_args()
    root = args.output_root.resolve(strict=True)
    control = root / "00_project_control"
    checks = []

    def record(name, passed, detail):
        checks.append({"check": name, "status": "PASS" if passed else "FAIL", "detail": detail})

    protection = rows(control / "protected_input_checksums.csv")
    for source in protection:
        path_key = next(key for key in source if key in {"path", "source_path", "input_path", "file_path"})
        checksum_key = next(key for key in source if "sha256" in key.lower())
        actual = hashlib.sha256(Path(source[path_key]).read_bytes()).hexdigest()
        record("protected_source_unchanged", actual == source[checksum_key], source[path_key])
    models = rows(root / "02_gam_robustness/tables/model_comparison.csv")
    record("common_gam_sample", len(models) == 12 and all(int(row["n"]) == 84 for row in models), "12 fits; common 84-city sample")
    record("unique_gam_definitions", len({(row["response"], row["model"]) for row in models}) == 12, "Response/formula pairs")
    record("finite_gam_metrics", all(math.isfinite(float(row[key])) for row in models for key in ["adj_r2", "deviance_explained", "AIC", "focal_edf", "focal_F", "focal_p"]), "Saved fit statistics")
    reproduction = rows(root / "02_gam_robustness/qc/historical_reproduction.csv")
    record("archived_g0_reproduction", all(row["pass"].lower() == "true" for row in reproduction), "Archived table reproduction; does not resolve fresh raster concordance")
    pixels = rows(root / "01_environment_only/tables/raster_comparisons.csv")
    record("paired_raster_statistics", len(pixels) == 1 and all(math.isfinite(float(pixels[0][key])) for key in ["pearson", "spearman", "MAE", "RMSE", "reference_mean", "comparison_mean"]), "Finite paired-cell comparison")
    cities = rows(root / "01_environment_only/tables/three_response_city_extraction.csv")
    record("city_extraction", len(cities) == 108 and len({row["city"].lower() for row in cities}) == 108, "Unique final city geometry IDs")
    review = rows(control / "reviewer_analysis_map.csv")
    matrix = rows(root / "07_integrated_evidence/reviewer_evidence_matrix.csv")
    allowed = {"ADDRESSED", "PARTIALLY_ADDRESSED", "NOT_ADDRESSED", "NOT_APPLICABLE", "AUTHOR_DECISION_REQUIRED"}
    record("exact_review_ids", len(matrix) == 52 and {row["comment_id"] for row in matrix} == {row["comment_id"] for row in review}, "All source comment IDs")
    record("review_status_vocabulary", all(row["status"] in allowed for row in matrix), "Scientific concern status remains distinct from computation")
    required = {
        "text_results/01_verified_facts/00_all_verified_results.md",
        "text_results/02_scientific_interpretation/environment_only_interpretation.md",
        "text_results/02_scientific_interpretation/gam_robustness_interpretation.md",
        "text_results/02_scientific_interpretation/sampling_interpretation.md",
        "text_results/02_scientific_interpretation/nested_contrasts_interpretation.md",
        "text_results/02_scientific_interpretation/integrated_interpretation.md",
        "text_results/03_manuscript_ready/methods_additions.md",
        "text_results/03_manuscript_ready/results_additions.md",
        "text_results/03_manuscript_ready/discussion_additions.md",
        "text_results/03_manuscript_ready/limitations_additions.md",
        "text_results/03_manuscript_ready/supplementary_methods.md",
        "text_results/03_manuscript_ready/figure_and_table_recommendations.md",
        "text_results/04_rebuttal_ready/reviewer_evidence_matrix.csv",
        "text_results/04_rebuttal_ready/reviewer_response_drafts.md",
        "text_results/04_rebuttal_ready/editor_summary.md",
        "text_results/05_internal_decisions/author_decisions_required.md",
        "text_results/05_internal_decisions/results_that_change_the_story.md",
        "text_results/05_internal_decisions/analyses_not_completed.md",
        "text_results/05_internal_decisions/recommended_revision_priority.md",
        "07_integrated_evidence/main_findings_summary.csv",
        "07_integrated_evidence/manuscript_result_comparison.csv",
        "07_integrated_evidence/recommended_main_vs_supplement.csv",
        "07_integrated_evidence/analysis_output_index.csv",
        "RELEASE_ANALYSIS_SUMMARY.md",
    }
    for relative in sorted(required):
        record("required_report", (root / relative).is_file(), relative)
    table = control / "saved_evidence_validation.csv"
    with table.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=["check", "status", "detail"])
        writer.writeheader()
        writer.writerows(checks)
    summary = {"status": "PASS" if all(row["status"] == "PASS" for row in checks) else "FAIL",
               "checks": len(checks), "failures": [row for row in checks if row["status"] == "FAIL"],
               "scope": "Saved numerical evidence and reporting checks; not scientific acceptance or a graphics-process pass"}
    (control / "saved_evidence_validation.json").write_text(json.dumps(summary, indent=2), encoding="utf-8")
    print(json.dumps(summary, indent=2))
    return 0 if summary["status"] == "PASS" else 1


if __name__ == "__main__":
    raise SystemExit(main())
