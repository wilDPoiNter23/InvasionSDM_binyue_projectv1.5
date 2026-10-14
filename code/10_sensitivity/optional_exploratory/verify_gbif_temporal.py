"""Independently reconstruct polygon first years and interval arithmetic."""
import argparse
import csv
import json
from collections import defaultdict
from pathlib import Path


def rows(path):
    if not path.exists():
        return []
    with path.open(encoding="utf-8-sig", newline="") as handle:
        return list(csv.DictReader(handle))


def integer(s):
    return int(s) if s not in ("", "NA", None) else None


def flag(s):
    return str(s).lower() == "true"


def verify(out):
    candidates = {int(r["record_id"]): r for r in []}
    for i, r in enumerate(rows(out / "candidate_records.csv"), 1):
        candidates[i] = r
    memberships = rows(out / "polygon_record_membership.csv")
    grouped = defaultdict(list)
    for r in memberships:
        source = candidates[int(r["record_id"])]
        for col in ("species", "year", "source_rows", "country_count"):
            assert str(source[col]) == str(r[col]), (col, source, r)
        for col in ("is_target", "valid_year", "zero_zero"):
            assert flag(source[col]) == flag(r[col]), (col, source, r)
        grouped[r["polygon_id"]].append(r)
    assert len({(r["polygon_id"], r["record_id"]) for r in memberships}) == len(memberships)
    firsts = rows(out / "species_first_records.csv")
    expected_keys = {(r["polygon_id"], r["species"]) for r in memberships if flag(r["is_target"])}
    actual_keys = [(f["polygon_id"], f["species"]) for f in firsts]
    assert set(actual_keys) == expected_keys and len(actual_keys) == len(expected_keys)
    for f in firsts:
        target = [r for r in grouped[f["polygon_id"]] if r["species"] == f["species"] and flag(r["is_target"])]
        valid = [r for r in target if flag(r["valid_year"]) and not flag(r["zero_zero"])]
        earliest = min((integer(r["year"]) for r in valid), default=None)
        assert integer(f["first_record_year"]) == earliest
        assert int(f["unique_records"]) == len(target)
        assert int(f["valid_dated_records"]) == len(valid)
        assert int(f["source_rows"]) == sum(int(r["source_rows"]) for r in target)
        endpoint, start = integer(f["endpoint"]), integer(f["start_year"])
        if earliest is not None and endpoint is not None:
            assert start == endpoint - 5
            assert integer(f["lag_lower_years"]) == earliest - endpoint
            assert integer(f["lag_upper_years"]) == earliest - start
            expected = "PRE_COHORT" if earliest < start else ("WITHIN_COHORT_INTERVAL" if earliest <= endpoint else "AFTER_COHORT_ENDPOINT")
            assert f["temporal_class"] == expected
        else:
            assert integer(f["lag_lower_years"]) is None and integer(f["lag_upper_years"]) is None
    summaries = rows(out / "polygon_record_summary.csv")
    assert {p["polygon_id"] for p in summaries} == {p["polygon_id"] for p in rows(out / "polygon_index.csv")}
    for p in summaries:
        m = grouped[p["polygon_id"]]
        target = [r for r in m if flag(r["valid_year"]) and flag(r["is_target"]) and not flag(r["zero_zero"])]
        assert integer(p["first_any_target_year"]) == min((integer(r["year"]) for r in target), default=None)
        assert int(p["unique_records"]) == len(m)
        assert int(p["source_rows"]) == sum(int(r["source_rows"]) for r in m)
        assert int(p["missing_year_records"]) == sum(integer(r["year"]) is None for r in m)
        assert int(p["valid_target_records"]) == len(target)
    sampling = rows(out / "polygon_year_sampling.csv")
    expected_years = {(r["polygon_id"], integer(r["year"])) for r in memberships if integer(r["year"]) is not None}
    assert {(r["polygon_id"], integer(r["year"])) for r in sampling} == expected_years
    for y in sampling:
        m = [r for r in grouped[y["polygon_id"]] if integer(r["year"]) == integer(y["year"])]
        assert int(y["unique_source_records"]) == len(m)
        assert int(y["source_rows"]) == sum(int(r["source_rows"]) for r in m)
        assert int(y["unique_target_records"]) == sum(flag(r["is_target"]) for r in m)
    qc = rows(out / "global_year_qc.csv")[0]
    assert int(qc["source_rows"]) == sum(int(qc[k]) for k in ("missing_year_rows", "invalid_year_rows", "valid_year_rows"))
    assert int(qc["source_rows"]) == sum(int(r["source_rows"]) for r in rows(out / "global_year_counts.csv"))
    screen = json.loads((out / "temporal_screen.json").read_text())
    assert screen["biological_invasion_dates_inferred"] is False
    assert screen["unrecorded_cohorts_absent_or_right_censored"] is False
    return dict(status="NUMERICAL_TABLE_CHECKS_PASS", candidate_rows=len(candidates),
                polygon_memberships=len(memberships), species_polygon_records=len(firsts),
                limitation="Arithmetic and saved membership checked; geometry requires independent spatial spot checks")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("output_root", type=Path)
    parser.add_argument("receipt", type=Path)
    args = parser.parse_args()
    result = verify(args.output_root)
    args.receipt.write_text(json.dumps(result, indent=2) + "\n", encoding="utf-8")


if __name__ == "__main__":
    main()
