"""Check saved cohort tables and independently reconstruct point estimates."""
import argparse
import csv
import json
import math
from collections import defaultdict
from pathlib import Path


def rows(path):
    with path.open(encoding="utf-8-sig", newline="") as handle:
        return list(csv.DictReader(handle))


def number(value):
    try:
        return float(value)
    except (ValueError, TypeError):
        return math.nan


def close(actual, expected, tolerance=1e-10):
    return (math.isnan(actual) and math.isnan(expected)) or abs(actual - expected) <= tolerance


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("configuration", type=Path)
    parser.add_argument("output", type=Path)
    args = parser.parse_args()
    cfg = json.loads(args.configuration.read_text(encoding="utf-8-sig"))
    screen = Path(cfg["screen_output"])
    output = Path(cfg["association_output"])
    checks = []

    def check(name, passed, details=""):
        checks.append(dict(check=name, passed=bool(passed), details=details))

    coverage = rows(screen / "city_coverage.csv")
    for layer in {r["layer"] for r in coverage}:
        q = [r for r in coverage if r["layer"] == layer]
        check(layer + "_108_unique_cities", len(q) == len({r["city_id"] for r in q}) == 108)
        check(layer + "_disjoint_categories", all(number(r["overlap_excess_km2"]) <=
              max(0.001, number(r["mapped_area_km2"]) * 0.001) for r in q))
    data = rows(screen / "native_cell_category_suitability.csv")
    identifiers = [(r["layer"], r["response"], r["city_id"], r["reported_category"]) for r in data]
    check("unique_city_category_response", len(identifiers) == len(set(identifiers)))
    check("native_cell_support_bounds", all(
        number(r["covered_cell_equivalents"]) <= number(r["native_cells"]) + 1e-7 and
        number(r["effective_cells"]) <= number(r["native_cells"]) + 1e-7 for r in data))
    check("suitability_bounds", all(not math.isfinite(number(r["mean_suitability"])) or
          0 <= number(r["mean_suitability"]) <= 1 for r in data))
    coefficients = rows(output / "period_association.csv")
    contrasts = rows(output / "old_recent_contrast.csv")
    for summary in coefficients:
        response, scope = summary["response"], summary["scope"]
        selected = [r for r in data if r["layer"] == "coastal_change" and
                    r["response"] == response and math.isfinite(number(r["mean_suitability"]))]
        if scope == "native_support":
            selected = [r for r in selected if number(r["effective_cells"]) >= 2 and
                        number(r["native_cells"]) >= 3]
        groups = defaultdict(list)
        for row in selected:
            groups[row["city_id"]].append(row)
        groups = {city: q for city, q in groups.items() if len(q) >= 2}
        xx_total = xy_total = 0.0
        differences = []
        for q in groups.values():
            areas = [number(r["mapped_area_km2"]) for r in q]
            weights = [area / sum(areas) for area in areas]
            x = [(number(r["reported_category"]) - 1990) / 5 for r in q]
            y = [number(r["mean_suitability"]) for r in q]
            mx = sum(w * value for w, value in zip(weights, x))
            my = sum(w * value for w, value in zip(weights, y))
            xx_total += sum(w * (value - mx) ** 2 for w, value in zip(weights, x))
            xy_total += sum(w * (vx - mx) * (vy - my) for w, vx, vy in zip(weights, x, y))
            old = [r for r in q if number(r["reported_category"]) <= 2000]
            recent = [r for r in q if number(r["reported_category"]) >= 2015]
            if old and recent:
                def mean(z):
                    return sum(number(r["mapped_area_km2"]) * number(r["mean_suitability"])
                               for r in z) / sum(number(r["mapped_area_km2"]) for r in z)
                differences.append(mean(recent) - mean(old))
        beta = xy_total / xx_total if xx_total > 0 else math.nan
        label = response + "_" + scope
        check(label + "_city_count", len(groups) == int(summary["n_cities"]))
        check(label + "_slope", close(beta, number(summary["slope_per_five_year_cohort"])))
        pair = next(r for r in contrasts if r["response"] == response and r["scope"] == scope)
        delta = sum(differences) / len(differences) if differences else math.nan
        check(label + "_paired_count", len(differences) == int(pair["n_cities"]))
        check(label + "_paired_difference", close(delta, number(pair["recent_minus_old"])))
    result = dict(status="PASS" if all(r["passed"] for r in checks) else "FAIL", checks=checks,
                  scope="Saved-table consistency and point-estimate reconstruction; not causal validation")
    args.output.write_text(json.dumps(result, indent=2) + "\n", encoding="utf-8")
    print(json.dumps(result, indent=2))
    return 0 if result["status"] == "PASS" else 1


if __name__ == "__main__":
    raise SystemExit(main())
