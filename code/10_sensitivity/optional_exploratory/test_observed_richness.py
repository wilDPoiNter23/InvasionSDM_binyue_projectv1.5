"""Synthetic date, duplicate, boundary and observation-support checks."""
import csv
import json
import subprocess
import sys
from pathlib import Path

import duckdb


def main():
    out = Path(sys.argv[1])
    out.mkdir(parents=True, exist_ok=False)
    database = out / "fixture.duckdb"
    con = duckdb.connect(str(database))
    con.execute("CREATE TABLE speciesocc_data(species VARCHAR, decimalLongitude DOUBLE, decimalLatitude DOUBLE, year INTEGER, countryCode VARCHAR)")
    con.executemany("INSERT INTO speciesocc_data VALUES (?,?,?,?,?)", [
        ("A", 1., 1., 2010, "AA"), ("A", 1., 1., 2010, "AA"),
        ("B", 2., 1., 2024, "AA"), ("B", 1., 1., None, "AA"),
        ("C", 1., 1., 1599, "AA"), ("C", 1., 1., 2025, "AA"),
        ("C", 1., 1., 1600, "AA"), ("A", 1., 1., 2000, "AA"),
        ("D", 50., 50., 2015, "BB"), ("A", 181., 1., 2015, "AA")])
    con.close()
    (out / "windows.csv").write_text("city_id,xmin,ymin,xmax,ymax\nC1,0,0,2,2\nC2,2,0,4,2\nC3,10,10,11,11\n")
    cfg = {"database": str(database), "output_root": str(out), "expected_source_rows": 10,
           "expected_source_species": 4, "maximum_candidate_rows": 100,
           "threads": 1, "memory_gb": 1}
    (out / "config.json").write_text(json.dumps(cfg))
    script = Path(__file__).with_name("observed_richness_extract.py")
    subprocess.run([sys.executable, str(script), str(out / "config.json")], check=True)
    with (out / "candidate_records.csv").open(newline="") as f:
        rows = list(csv.DictReader(f))
    assert len(rows) == 7, rows
    duplicate = [r for r in rows if r["species"] == "A" and r["year"] == "2010"]
    assert len(duplicate) == 1 and int(duplicate[0]["source_rows"]) == 2
    assert sum(r["year"] == "" for r in rows) == 1
    assert sum(r["valid_year"].lower() == "true" for r in rows) == 4
    assert len([r for r in rows if float(r["longitude"]) == 2]) == 1
    print("PASS_EXTRACT_DATES_DUPLICATES_BOUNDARY", len(rows))


if __name__ == "__main__":
    main()
