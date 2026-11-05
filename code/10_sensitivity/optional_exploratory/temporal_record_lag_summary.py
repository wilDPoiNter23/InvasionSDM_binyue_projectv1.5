"""Describe bounded first-record delays without inferring colonisation dates."""
import argparse
import csv
import hashlib
import json
import statistics
from collections import Counter, defaultdict
from pathlib import Path


COASTAL = "coastal_dissolved_city_cohort"
STATES = ("AFTER_ENDPOINT", "WITHIN_INTERVAL", "BEFORE_INTERVAL", "NO_VALID_YEAR",
          "AFTER_OBSERVATION_HORIZON")
BINS = ("0_to_5", "over_5_to_10", "over_10")


def digest(path):
    h = hashlib.sha256()
    with Path(path).open("rb") as handle:
        for block in iter(lambda: handle.read(1048576), b""):
            h.update(block)
    return h.hexdigest()


def read_csv(path):
    with Path(path).open(encoding="utf-8-sig", newline="") as handle:
        return list(csv.DictReader(handle))


def write_csv(path, rows):
    if not rows:
        return
    with Path(path).open("w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=list(rows[0]))
        writer.writeheader()
        writer.writerows(rows)


def integer(value):
    if value is None or str(value).strip() in ("", "NA", "NaN"):
        return None
    result = int(float(value))
    if float(value) != result:
        raise ValueError("Expected an integer year or count")
    return result


def require(condition, message):
    if not condition:
        raise ValueError(message)


def classify(year, endpoint, horizon):
    if year is None:
        return "NO_VALID_YEAR"
    if year > horizon:
        return "AFTER_OBSERVATION_HORIZON"
    if year < endpoint - 5:
        return "BEFORE_INTERVAL"
    if year <= endpoint:
        return "WITHIN_INTERVAL"
    return "AFTER_ENDPOINT"


def bin_bounds(lower, upper):
    """Bins are [0,5], (5,10], (10,infinity); bounds remain intervals."""
    require(0 < lower <= upper, "Only positive post-endpoint bounds are binned")
    return {
        "0_to_5_certain": int(upper <= 5),
        "0_to_5_possible": int(lower <= 5),
        "over_5_to_10_certain": int(lower > 5 and upper <= 10),
        "over_5_to_10_possible": int(upper > 5 and lower <= 10),
        "over_10_certain": int(lower > 10),
        "over_10_possible": int(upper > 10),
    }


def lag_row(base, year, endpoint, horizon):
    state = classify(year, endpoint, horizon)
    after = state == "AFTER_ENDPOINT"
    row = dict(base, endpoint=endpoint, start_year=endpoint - 5,
               first_record_year=year, observation_horizon=horizon,
               potential_followup_years=horizon - endpoint, temporal_state=state,
               lag_lower_years=year - endpoint if after else None,
               lag_upper_years=year - endpoint + 5 if after else None)
    row.update({name + suffix: None for name in BINS for suffix in ("_certain", "_possible")})
    if after:
        row.update(bin_bounds(row["lag_lower_years"], row["lag_upper_years"]))
    return row


def balanced_median(rows, field):
    cities = defaultdict(lambda: defaultdict(list))
    for row in rows:
        if row["temporal_state"] == "AFTER_ENDPOINT":
            cities[row["city_id"]][row["endpoint"]].append(row)
    weighted = []
    for cohorts in cities.values():
        for records in cohorts.values():
            weight = 1 / (len(cities) * len(cohorts) * len(records))
            weighted.extend((r[field], weight) for r in records)
    cumulative = 0
    for value, weight in sorted(weighted):
        cumulative += weight
        if cumulative >= .5 - 1e-12:
            return value
    return None


def validate_inputs(cfg):
    root = Path(cfg["input_root"])
    paths = {name: root / name for name in (
        "species_first_records.csv", "polygon_record_summary.csv", "extraction.json",
        "target_roster_used.csv", "target_year_qc.csv", "temporal_screen.json")}
    paths["target_manifest"] = Path(cfg["target_manifest"])
    receipt = json.loads(paths["extraction.json"].read_text(encoding="utf-8-sig"))
    require(receipt["target_count"] == 684, "Frozen extraction must contain 684 target taxa")
    require(digest(paths["target_manifest"]) == receipt["roster_sha256"], "Frozen manifest checksum changed")
    manifest = read_csv(paths["target_manifest"])
    selected = [r for r in manifest if r["asset_role"] == "PAIRED_FULL"]
    require(selected and all(str(r["used_in_paired_sensitivity"]).lower() in ("true", "1", "yes")
                             for r in selected), "Manifest includes non-paired taxa")
    taxa = [r["species"].strip() for r in selected]
    require(len(taxa) == len(set(taxa)) == 684, "Target manifest must select 684 unique taxa")
    for name in ("target_roster_used.csv", "target_year_qc.csv"):
        used = [r["species"].strip() for r in read_csv(paths[name])]
        require(len(used) == 684 and set(used) == set(taxa), "Frozen target identity mismatch: " + name)
    pairs = [r for r in read_csv(paths["species_first_records.csv"]) if r["kind"] == COASTAL]
    polygons = [r for r in read_csv(paths["polygon_record_summary.csv"]) if r["kind"] == COASTAL]
    require(len({p["polygon_id"] for p in polygons}) == len(polygons), "Duplicate cohort polygon")
    require(len({(p["city_id"], p["endpoint"]) for p in polygons}) == len(polygons), "Duplicate city endpoint")
    by_polygon = {p["polygon_id"]: p for p in polygons}
    seen = set()
    for r in pairs:
        require(r["species"] in taxa, "Non-target taxon in first-record table")
        require((r["polygon_id"], r["species"]) not in seen, "Duplicate species-cohort pair")
        seen.add((r["polygon_id"], r["species"]))
        require(r["polygon_id"] in by_polygon, "Unknown cohort polygon")
        p = by_polygon[r["polygon_id"]]
        endpoint, start, year = integer(r["endpoint"]), integer(r["start_year"]), integer(r["first_record_year"])
        require(endpoint in range(1990, 2021, 5) and start == endpoint - 5, "Invalid five-year cohort")
        require(r["city_id"] == p["city_id"] and endpoint == integer(p["endpoint"])
                and start == integer(p["start_year"]), "Cohort identity mismatch")
        expected = ("NO_VALID_YEAR" if year is None else "PRE_COHORT" if year < start else
                    "WITHIN_COHORT_INTERVAL" if year <= endpoint else "AFTER_COHORT_ENDPOINT")
        require(r["temporal_class"] == expected, "Frozen temporal class mismatch")
        require(integer(r["lag_lower_years"]) == (None if year is None else year - endpoint)
                and integer(r["lag_upper_years"]) == (None if year is None else year - start), "Frozen lag bounds mismatch")
    groups = defaultdict(list)
    for r in pairs:
        groups[r["polygon_id"]].append(r)
    for p in polygons:
        endpoint, start = integer(p["endpoint"]), integer(p["start_year"])
        require(endpoint in range(1990, 2021, 5) and start == endpoint - 5, "Invalid polygon cohort")
        rows = groups[p["polygon_id"]]
        years = [integer(r["first_record_year"]) for r in rows if integer(r["first_record_year"]) is not None]
        require(integer(p["first_any_target_year"]) == (min(years) if years else None), "First-any year mismatch")
        require(integer(p["target_species_observed"]) == len(years), "Dated species count mismatch")
        require(integer(p["valid_target_records"]) == sum(integer(r["valid_dated_records"]) for r in rows), "Dated target record count mismatch")
    return paths, pairs, polygons, groups


def balanced_summary(rows, unit, scope):
    """Equal cohorts within each city, then equal cities; after-only lag bounds."""
    city_rows, pooled = [], []
    for endpoint in ["ALL"] + sorted({r["endpoint"] for r in rows}):
        selected = rows if endpoint == "ALL" else [r for r in rows if r["endpoint"] == endpoint]
        groups = defaultdict(list)
        for r in selected:
            groups[r["city_id"]].append(r)
        current = []
        for city, records in sorted(groups.items()):
            cohorts = defaultdict(list)
            for r in records:
                cohorts[r["endpoint"]].append(r)
            after_cohorts = [[r for r in values if r["temporal_state"] == "AFTER_ENDPOINT"]
                             for values in cohorts.values()]
            after_cohorts = [values for values in after_cohorts if values]
            item = dict(unit=unit, scope=scope, cohort_endpoint=endpoint, city_id=city,
                        n_units=len(records), n_cohorts=len(cohorts),
                        n_after_units=sum(len(v) for v in after_cohorts),
                        n_after_cohorts=len(after_cohorts))
            for state in STATES:
                item["count_" + state] = sum(r["temporal_state"] == state for r in records)
                item["fraction_" + state] = statistics.mean(
                    sum(r["temporal_state"] == state for r in v) / len(v) for v in cohorts.values())
            for field in ("lag_lower_years", "lag_upper_years"):
                item["mean_" + field] = (statistics.mean(statistics.mean(r[field] for r in v)
                                                       for v in after_cohorts) if after_cohorts else None)
                item["median_" + field] = balanced_median(records, field)
            for name in BINS:
                for suffix in ("_certain", "_possible"):
                    item["fraction_" + name + suffix] = (statistics.mean(
                        statistics.mean(r[name + suffix] for r in v) for v in after_cohorts)
                        if after_cohorts else None)
            current.append(item)
            city_rows.append(item)
        after = [item for item in current if item["n_after_units"]]
        result = dict(unit=unit, scope=scope, cohort_endpoint=endpoint, n_cities=len(current),
                      n_units=len(selected), n_after_cities=len(after),
                      n_after_units=sum(item["n_after_units"] for item in current),
                      potential_followup_years=(selected[0]["observation_horizon"] - endpoint
                                                if endpoint != "ALL" else None))
        for state in STATES:
            result["count_" + state] = sum(item["count_" + state] for item in current)
            result["city_balanced_fraction_" + state] = (statistics.mean(item["fraction_" + state]
                                                                       for item in current) if current else None)
        for field in ("mean_lag_lower_years", "mean_lag_upper_years"):
            result["city_balanced_" + field] = statistics.mean(item[field] for item in after) if after else None
        for field in ("lag_lower_years", "lag_upper_years"):
            result["city_balanced_median_" + field] = balanced_median(selected, field)
        for name in BINS:
            for suffix in ("_certain", "_possible"):
                field = "fraction_" + name + suffix
                result["city_balanced_" + field] = statistics.mean(item[field] for item in after) if after else None
        pooled.append(result)
    return city_rows, pooled


def run(cfg):
    horizon = cfg["observation_horizon"]
    require(horizon == 2024, "This descriptive contract uses the predeclared 2024 horizon")
    paths, pairs, polygons, groups = validate_inputs(cfg)
    out = Path(cfg["output_root"])
    require(not out.exists() or not any(out.iterdir()), "Output directory must be empty")
    out.mkdir(parents=True, exist_ok=True)
    pair_rows = []
    for r in pairs:
        pair_rows.append(lag_row(dict(polygon_id=r["polygon_id"], city_id=r["city_id"], species=r["species"],
            ambiguous_membership_records=integer(r["ambiguous_coastal_membership_records"]),
            frozen_temporal_class=r["temporal_class"], native_status=r["native_status"]),
            integer(r["first_record_year"]), integer(r["endpoint"]), horizon))
    cohort_rows, city_rows, summary, opportunity = [], [], [], []
    for scope in ("all_frozen_pairs", "exclude_ambiguous_pairs"):
        selected_pairs = [r for r in pair_rows if scope == "all_frozen_pairs" or not r["ambiguous_membership_records"]]
        by_polygon = defaultdict(list)
        for r in selected_pairs:
            by_polygon[r["polygon_id"]].append(r)
        current_cohorts, after_cohorts = [], []
        for p in polygons:
            items = by_polygon[p["polygon_id"]]
            years = [r["first_record_year"] for r in items if r["first_record_year"] is not None]
            row = lag_row(dict(scope=scope, polygon_id=p["polygon_id"], city_id=p["city_id"],
                first_record_definition="FIRST_ANY_TARGET_RECORD",
                n_retained_species_pairs=len(items), n_dated_species_pairs=len(years),
                frozen_record_state=p["record_state"], native_status=p["native_status"]),
                min(years) if years else None, integer(p["endpoint"]), horizon)
            current_cohorts.append(row)
            after_years = [y for y in years if integer(p["endpoint"]) < y <= horizon]
            after_row = lag_row(dict(scope=scope, polygon_id=p["polygon_id"], city_id=p["city_id"],
                first_record_definition="EARLIEST_AFTER_ENDPOINT_SPECIES_FIRST_CONDITIONAL",
                n_retained_species_pairs=len(items), n_dated_species_pairs=len(years),
                frozen_record_state=p["record_state"], native_status=p["native_status"]),
                min(after_years) if after_years else None, integer(p["endpoint"]), horizon)
            after_cohorts.append(after_row)
        cohort_rows.extend(current_cohorts)
        cohort_rows.extend(after_cohorts)
        for unit, rows in (("species_cohort_pair", selected_pairs), ("cohort_first_any_target", current_cohorts),
                           ("cohort_earliest_after_endpoint_species_first_conditional", after_cohorts)):
            per_city, totals = balanced_summary(rows, unit, scope)
            city_rows.extend(per_city)
            summary.extend(totals)
            for window in (5, 10):
                eligible = [r for r in rows if r["potential_followup_years"] >= window]
                _, totals = balanced_summary(eligible, unit, scope)
                for result in totals:
                    result["minimum_potential_followup_years"] = window
                    result["within_window_certain_fraction_conditional_after_record"] = (
                        result["city_balanced_fraction_0_to_5_certain"] if window == 5 else
                        None)
                    result["within_window_possible_fraction_conditional_after_record"] = (
                        result["city_balanced_fraction_0_to_5_possible"] if window == 5 else
                        None)
                    if window == 10:
                        sub = eligible if result["cohort_endpoint"] == "ALL" else [
                            r for r in eligible if r["endpoint"] == result["cohort_endpoint"]]
                        # Separate reconstruction preserves interval uncertainty at ten years.
                        per_city_window = defaultdict(lambda: defaultdict(list))
                        for r in sub:
                            if r["temporal_state"] == "AFTER_ENDPOINT":
                                per_city_window[r["city_id"]][r["endpoint"]].append(r)
                        for bound, comparator in (("certain", "lag_upper_years"), ("possible", "lag_lower_years")):
                            values = [statistics.mean(statistics.mean(int(r[comparator] <= window) for r in v)
                                                       for v in cohorts.values()) for cohorts in per_city_window.values()]
                            result["within_window_" + bound + "_fraction_conditional_after_record"] = (
                                statistics.mean(values) if values else None)
                    opportunity.append(result)
    write_csv(out / "species_pair_record_lag_bounds.csv", pair_rows)
    write_csv(out / "cohort_first_any_record_lag_bounds.csv", cohort_rows)
    write_csv(out / "city_balanced_record_lag_source.csv", city_rows)
    write_csv(out / "record_lag_summary.csv", summary)
    write_csv(out / "fixed_followup_opportunity_summary.csv", opportunity)
    receipt = dict(status="DESCRIPTIVE_TARGET_FIRST_RECORD_DELAYS_ONLY", observation_horizon=horizon,
        configuration=cfg, input_sha256={name: digest(path) for name, path in paths.items()},
        source_coastal_pairs=len(pairs), source_cohorts=len(polygons),
        frozen_class_counts=dict(Counter(r["temporal_class"] for r in pairs)),
        bin_definition={"0_to_5": "[0,5]", "over_5_to_10": "(5,10]", "over_10": "(10,infinity)"},
        weighting="Equal cohorts within city, then equal cities; lag statistics conditional on after-endpoint records",
        possible_bin_fractions_need_not_sum_to_one=True,
        absent_records_zero_or_right_censored=False, survival_or_age_regression_fitted=False,
        limitations=["First database record is not arrival, establishment or colonisation",
          "Native status and establishment status remain unresolved",
          "Unknown survey effort prevents survival or hazard interpretation",
          "Later cohorts have shorter potential database observation windows",
          "No confidence interval is assigned to unknown survey coverage",
          "Strict sensitivity drops whole species-cohort pairs with any ambiguous coastal membership",
          "Earliest-after summaries condition on a post-endpoint species first record and retain a separate first-any analysis",
          "The first-record table cannot identify the earliest later repeat record of species first recorded before the endpoint",
          "Five- and ten-year opportunity subsets are descriptive, not invasion rates or evidence of continuous surveys",
          "Records within or before the cohort interval are retained as timing/location incompatibility indicators",
          "Cohorts lacking dates remain unknown and are never assigned zero delay"])
    receipt["output_sha256"] = {path.name: digest(path) for path in sorted(out.glob("*.csv"))}
    (out / "record_lag_summary.json").write_text(json.dumps(receipt, indent=2) + "\n", encoding="utf-8")
    return receipt


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("configuration", type=Path)
    args = parser.parse_args()
    cfg = json.loads(args.configuration.read_text(encoding="utf-8-sig"))
    result = run(cfg)
    print(json.dumps({key: result[key] for key in ("status", "source_coastal_pairs", "source_cohorts")}))


if __name__ == "__main__":
    main()
