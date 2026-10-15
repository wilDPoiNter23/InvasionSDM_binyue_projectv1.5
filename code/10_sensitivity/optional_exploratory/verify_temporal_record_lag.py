"""Independently reconstruct descriptive first-record bounds and city weights."""
import argparse
import csv
import hashlib
import json
import math
from collections import Counter, defaultdict
from fractions import Fraction
from pathlib import Path


STATES = ("AFTER_ENDPOINT", "WITHIN_INTERVAL", "BEFORE_INTERVAL", "NO_VALID_YEAR", "AFTER_OBSERVATION_HORIZON")
BINS = ("0_to_5", "over_5_to_10", "over_10")


def sha(path):
    result = hashlib.sha256()
    with Path(path).open("rb") as handle:
        for block in iter(lambda: handle.read(1048576), b""):
            result.update(block)
    return result.hexdigest()


def read(path):
    with Path(path).open(encoding="utf-8-sig", newline="") as handle:
        return list(csv.DictReader(handle))


def number(value):
    return None if value in (None, "", "NA", "NaN") else int(value)


def check(condition, message):
    if not condition:
        raise ValueError(message)


def record(city, endpoint, year):
    state = ("NO_VALID_YEAR" if year is None else "AFTER_OBSERVATION_HORIZON" if year > 2024 else
             "BEFORE_INTERVAL" if year < endpoint - 5 else "WITHIN_INTERVAL" if year <= endpoint else "AFTER_ENDPOINT")
    r = dict(city_id=city, endpoint=endpoint, start_year=endpoint - 5, first_record_year=year,
             observation_horizon=2024, potential_followup_years=2024 - endpoint, temporal_state=state,
             lag_lower_years=None, lag_upper_years=None)
    for name in BINS:
        for bound in ("certain", "possible"):
            r[name + "_" + bound] = None
    if state == "AFTER_ENDPOINT":
        lo, hi = year - endpoint, year - endpoint + 5
        r.update(lag_lower_years=lo, lag_upper_years=hi)
        # Each delay range is tested against disjoint real-valued bins.
        intervals = ((0, True, 5), (5, False, 10), (10, False, math.inf))
        for name, (left, closed, right) in zip(BINS, intervals):
            intersects = hi >= left if closed else hi > left
            inside = lo >= left if closed else lo > left
            r[name + "_possible"] = int(intersects and lo <= right)
            r[name + "_certain"] = int(inside and hi <= right)
        check(sum(r[name + "_certain"] for name in BINS) <= 1, "Certain bins overlap")
        check(sum(r[name + "_possible"] for name in BINS) >= 1, "No compatible delay bin")
    return r


def weights(rows):
    sizes = Counter((r["city_id"], r["endpoint"]) for r in rows)
    city_cohorts = Counter(city for city, endpoint in sizes)
    return [Fraction(1, len(city_cohorts) * city_cohorts[r["city_id"]] * sizes[(r["city_id"], r["endpoint"])]) for r in rows]


def weighted(rows, field, predicate=None):
    if not rows:
        return None
    return float(sum((w * (int(predicate(r)) if predicate else r[field]) for r, w in zip(rows, weights(rows))), Fraction(0)))


def median(rows, field):
    mass = defaultdict(Fraction)
    for row, weight in zip(rows, weights(rows)):
        mass[row[field]] += weight
    running = Fraction(0)
    for value in sorted(mass):
        running += mass[value]
        if running >= Fraction(1, 2):
            return value
    return None


def metrics(rows, city=False):
    after = [r for r in rows if r["temporal_state"] == "AFTER_ENDPOINT"]
    fields = dict(n_units=len(rows), n_after_units=len(after))
    if city:
        fields.update(n_cohorts=len({r["endpoint"] for r in rows}), n_after_cohorts=len({r["endpoint"] for r in after}))
    else:
        fields.update(n_cities=len({r["city_id"] for r in rows}), n_after_cities=len({r["city_id"] for r in after}))
    prefix = "" if city else "city_balanced_"
    for state in STATES:
        fields["count_" + state] = sum(r["temporal_state"] == state for r in rows)
        fields[prefix + "fraction_" + state] = weighted(rows, None, lambda r: r["temporal_state"] == state)
    for bound in ("lower", "upper"):
        field = "lag_" + bound + "_years"
        fields[prefix + "mean_" + field] = weighted(after, field)
        fields[prefix + "median_" + field] = median(after, field)
    for name in BINS:
        for bound in ("certain", "possible"):
            fields[prefix + "fraction_" + name + "_" + bound] = weighted(after, name + "_" + bound)
    return fields


def verify(input_root, output_root):
    source, out = Path(input_root), Path(output_root)
    receipt = json.loads((out / "record_lag_summary.json").read_text(encoding="utf-8-sig"))
    check(receipt["observation_horizon"] == 2024, "Observation horizon differs")
    checks = 0
    for name, expected in receipt["input_sha256"].items():
        path = Path(receipt["configuration"]["target_manifest"]) if name == "target_manifest" else source / name
        check(sha(path) == expected, "Input checksum mismatch: " + name)
        checks += 1
    for name, expected in receipt["output_sha256"].items():
        check(sha(out / name) == expected, "Output checksum mismatch: " + name)
        checks += 1
    original = [r for r in read(source / "species_first_records.csv") if r["kind"] == "coastal_dissolved_city_cohort"]
    polygons = [r for r in read(source / "polygon_record_summary.csv") if r["kind"] == "coastal_dissolved_city_cohort"]
    check(len(original) == receipt["source_coastal_pairs"] and len(polygons) == receipt["source_cohorts"], "Source support mismatch")
    check(dict(Counter(r["temporal_class"] for r in original)) == receipt["frozen_class_counts"], "Frozen class counts differ")
    extraction = json.loads((source / "extraction.json").read_text(encoding="utf-8-sig"))
    manifest_path = receipt["configuration"]["target_manifest"]
    check(sha(manifest_path) == extraction["roster_sha256"], "Manifest does not bind to frozen extraction")
    selected = [r for r in read(manifest_path) if r["asset_role"] == "PAIRED_FULL"]
    taxa = {r["species"].strip() for r in selected}
    check(len(selected) == len(taxa) == extraction["target_count"] == 684 and
          all(r["used_in_paired_sensitivity"].lower() in ("true", "yes", "1") for r in selected), "Target scope differs")
    for filename in ("target_roster_used.csv", "target_year_qc.csv"):
        rows = read(source / filename)
        check(len(rows) == 684 and {r["species"].strip() for r in rows} == taxa, "Roster set differs")
    pairs = []
    for s in original:
        check(s["species"] in taxa, "First-record taxon is outside target scope")
        r = record(s["city_id"], number(s["endpoint"]), number(s["first_record_year"]))
        r.update(polygon_id=s["polygon_id"], species=s["species"],
                 ambiguous_membership_records=number(s["ambiguous_coastal_membership_records"]),
                 frozen_temporal_class=s["temporal_class"], native_status=s["native_status"])
        pairs.append(r)
    expected = {"species_pair_record_lag_bounds.csv": pairs,
                "cohort_first_any_record_lag_bounds.csv": [], "city_balanced_record_lag_source.csv": [],
                "record_lag_summary.csv": [], "fixed_followup_opportunity_summary.csv": []}
    for scope in ("all_frozen_pairs", "exclude_ambiguous_pairs"):
        subset = [r for r in pairs if scope == "all_frozen_pairs" or r["ambiguous_membership_records"] == 0]
        groups = defaultdict(list)
        for r in subset:
            groups[r["polygon_id"]].append(r)
        units = {"species_cohort_pair": subset, "cohort_first_any_target": [],
                 "cohort_earliest_after_endpoint_species_first_conditional": []}
        for p in polygons:
            group = groups[p["polygon_id"]]
            years = [r["first_record_year"] for r in group if r["first_record_year"] is not None]
            if scope == "all_frozen_pairs":
                check((min(years) if years else None) == number(p["first_any_target_year"]), "Source first-any mismatch")
                check(len(years) == number(p["target_species_observed"]), "Source dated species support mismatch")
            endpoint = number(p["endpoint"])
            for unit, conditional in (("cohort_first_any_target", False),
                                      ("cohort_earliest_after_endpoint_species_first_conditional", True)):
                eligible = [y for y in years if endpoint < y <= 2024] if conditional else years
                r = record(p["city_id"], endpoint, min(eligible) if eligible else None)
                r.update(scope=scope, polygon_id=p["polygon_id"],
                    first_record_definition="EARLIEST_AFTER_ENDPOINT_SPECIES_FIRST_CONDITIONAL" if conditional else "FIRST_ANY_TARGET_RECORD",
                    n_retained_species_pairs=len(group), n_dated_species_pairs=len(years),
                    frozen_record_state=p["record_state"], native_status=p["native_status"])
                units[unit].append(r)
                expected["cohort_first_any_record_lag_bounds.csv"].append(r)
        for unit, rows in units.items():
            for window in (None, 5, 10):
                eligible = rows if window is None else [r for r in rows if 2024 - r["endpoint"] >= window]
                for endpoint in ["ALL"] + sorted({r["endpoint"] for r in eligible}):
                    section = eligible if endpoint == "ALL" else [r for r in eligible if r["endpoint"] == endpoint]
                    base = dict(unit=unit, scope=scope, cohort_endpoint=endpoint)
                    summary = dict(base, **metrics(section))
                    summary["potential_followup_years"] = None if endpoint == "ALL" else 2024 - endpoint
                    if window is None:
                        expected["record_lag_summary.csv"].append(summary)
                        for city in sorted({r["city_id"] for r in section}):
                            city_source = dict(base, city_id=city, **metrics([r for r in section if r["city_id"] == city], city=True))
                            expected["city_balanced_record_lag_source.csv"].append(city_source)
                    else:
                        summary["minimum_potential_followup_years"] = window
                        after = [r for r in section if r["temporal_state"] == "AFTER_ENDPOINT"]
                        for bound, field in (("certain", "lag_upper_years"), ("possible", "lag_lower_years")):
                            summary["within_window_" + bound + "_fraction_conditional_after_record"] = weighted(
                                after, None, lambda r, f=field: r[f] <= window)
                        expected["fixed_followup_opportunity_summary.csv"].append(summary)
    keys = {
        "species_pair_record_lag_bounds.csv": ("polygon_id", "species"),
        "cohort_first_any_record_lag_bounds.csv": ("scope", "polygon_id", "first_record_definition"),
        "city_balanced_record_lag_source.csv": ("unit", "scope", "cohort_endpoint", "city_id"),
        "record_lag_summary.csv": ("unit", "scope", "cohort_endpoint"),
        "fixed_followup_opportunity_summary.csv": ("unit", "scope", "cohort_endpoint", "minimum_potential_followup_years")}
    counts = {}
    for filename, wanted in expected.items():
        actual = read(out / filename)
        fields = keys[filename]
        make_key = lambda row: tuple(str(row[f]) for f in fields)
        got = {make_key(row): row for row in actual}
        target = {make_key(row): row for row in wanted}
        check(len(got) == len(actual) and len(target) == len(wanted) and set(got) == set(target), "Output row identity/support mismatch: " + filename)
        for key, expected_row in target.items():
            for field, value in expected_row.items():
                observed = got[key].get(field)
                if value is None:
                    valid = observed == ""
                elif isinstance(value, (int, float)):
                    try:
                        valid = math.isfinite(float(observed)) and math.isclose(float(observed), value, rel_tol=1e-10, abs_tol=1e-10)
                    except (TypeError, ValueError):
                        valid = False
                else:
                    valid = observed == str(value)
                check(valid, "Independent mismatch: {} {} {}".format(filename, key, field))
                checks += 1
        counts[filename] = len(wanted)
    return dict(status="PASS", checks=checks, verified_rows=counts, source_coastal_pairs=len(original),
                source_cohorts=len(polygons), output_receipt_sha256=sha(out / "record_lag_summary.json"),
                interpretation="Independent arithmetic and identity checks; no colonisation inference")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("input_root", type=Path)
    parser.add_argument("output_root", type=Path)
    parser.add_argument("new_receipt", type=Path)
    args = parser.parse_args()
    check(not args.new_receipt.exists(), "Verification receipt already exists")
    try:
        result = verify(args.input_root, args.output_root)
    except Exception as error:
        args.new_receipt.write_text(json.dumps(dict(status="FAIL", error=str(error)), indent=2) + "\n", encoding="utf-8")
        raise
    args.new_receipt.write_text(json.dumps(result, indent=2) + "\n", encoding="utf-8")
    print(json.dumps(result))


if __name__ == "__main__":
    main()
