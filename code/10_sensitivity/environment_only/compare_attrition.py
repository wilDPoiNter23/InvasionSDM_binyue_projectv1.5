"""Create descriptive attrition tables from a frozen accepted pair list."""
import argparse
import csv
import hashlib
import json
import math
import statistics
from pathlib import Path


def read(path, delimiter=","):
    with Path(path).open(encoding="utf-8-sig", newline="") as handle:
        return list(csv.DictReader(handle, delimiter=delimiter))


def write(path, rows):
    with Path(path).open("w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=list(rows[0]))
        writer.writeheader()
        writer.writerows(rows)


def number(value):
    try:
        result = float(value)
        return result if math.isfinite(result) else None
    except (ValueError, TypeError):
        return None


def count(vector):
    return len(set(x.strip() for x in vector.split(";") if x.strip())) if vector else ""


parser = argparse.ArgumentParser()
parser.add_argument("--paired-manifest", required=True)
parser.add_argument("--taxon-column", default="species")
parser.add_argument("--species-roster", required=True)
parser.add_argument("--model-performance", required=True)
parser.add_argument("--inventory", required=True)
parser.add_argument("--output-dir", required=True)
args = parser.parse_args()
roster_rows = read(args.species_roster)
performance_rows = read(args.model_performance)
inventory_rows = read(args.inventory, "\t")
pair_rows = read(args.paired_manifest)
paired = {r[args.taxon_column].strip() for r in pair_rows}
if len(paired) != len(pair_rows):
    raise ValueError("Frozen paired list contains duplicate taxa")
roster = {r["species"].strip(): r for r in roster_rows}
performance = {r["accepted_taxon"].strip(): r for r in performance_rows}
inventory = {r["accepted_taxon"].strip(): r for r in inventory_rows}
if len(inventory) != len(inventory_rows):
    raise ValueError("Sensitivity inventory contains duplicate taxa")
if paired - inventory.keys():
    raise ValueError("Frozen pairs include taxa absent from sensitivity inventory")
detail = []
for taxon, inv in sorted(inventory.items()):
    perf = performance.get(taxon, {})
    full_field = "clean_full_predictor_vector" if inv["variant"] == "CLEAN_ENV" else "baseline_predictor_vector"
    full_vector = inv.get(full_field, "")
    detail.append({
        "accepted_taxon": taxon,
        "paired_status": "PAIRED" if taxon in paired else "EXCLUDED",
        "occurrence_record_count": roster.get(taxon, {}).get("record_count", ""),
        "full_best_TSS": perf.get("best_TSS", ""),
        "full_best_ROC": perf.get("best_ROC", ""),
        "baseline_predictor_count": count(inv.get("baseline_predictor_vector", "")),
        "matched_full_predictor_count": count(full_vector),
        "matched_full_predictor_vector_field": full_field,
        "performance_source_generation": perf.get("source_generation", ""),
        "performance_product_type": perf.get("product_type", ""),
        "performance_selection_tier": perf.get("selection_tier", ""),
        "inventory_variant": inv["variant"],
        "missing_from_author_813_roster": taxon not in roster,
        "missing_from_final_813_performance": taxon not in performance,
        "predictor_count_provenance": "inventory_recorded_vector",
        "scope": "ATTRITION_DIAGNOSTIC_ONLY",
    })
summary = []
for status in ("PAIRED", "EXCLUDED"):
    group = [r for r in detail if r["paired_status"] == status]
    for metric in ("occurrence_record_count", "full_best_TSS", "full_best_ROC", "baseline_predictor_count", "matched_full_predictor_count"):
        values = [number(r[metric]) for r in group]
        finite = [v for v in values if v is not None]
        summary.append({"group": status, "metric": metric, "n_species": len(group), "n_available": len(finite), "n_missing": len(group)-len(finite), "mean": statistics.mean(finite) if finite else "", "median": statistics.median(finite) if finite else "", "minimum": min(finite) if finite else "", "maximum": max(finite) if finite else "", "scope": "ATTRITION_DIAGNOSTIC_ONLY"})
out = Path(args.output_dir)
out.mkdir(parents=True, exist_ok=True)
write(out / "paired_vs_excluded_species_summary.csv", summary)
write(out / "paired_vs_excluded_species_details.csv", detail)
source_info = []
for label, path, rows in (("author_roster", args.species_roster, roster_rows), ("final813_performance", args.model_performance, performance_rows), ("sensitivity_inventory", args.inventory, inventory_rows), ("frozen_paired_manifest", args.paired_manifest, pair_rows)):
    data = Path(path).read_bytes()
    source_info.append({"source": label, "row_count": len(rows), "size_bytes": len(data), "sha256": hashlib.sha256(data).hexdigest()})
report = {"paired_n": len(paired), "excluded_n": len(inventory)-len(paired), "scope_n": len(inventory), "sources": source_info, "missing_from_author_roster": sorted(inventory.keys()-roster.keys()), "missing_from_performance": sorted(inventory.keys()-performance.keys()), "notes": ["Occurrence counts are author roster record counts, not deduplicated coordinates.", "FULL performance is the historical final813 table and retains original versus revised source-generation labels; CLEAN_FULL refit performance is not substituted.", "Predictor counts use the predictor vectors recorded in the supplied inventory.", "Paired eligibility is taken only from the supplied frozen manifest; no selection thresholds changed."]}
(out / "attrition_diagnostic_sources.json").write_text(json.dumps(report, indent=2)+"\n", encoding="utf-8")
print(json.dumps({k: report[k] for k in ("paired_n", "excluded_n", "scope_n", "missing_from_author_roster", "missing_from_performance")}))
