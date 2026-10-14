"""Read-only global year QC and bounded occurrence-window extraction."""
import argparse
import csv
import hashlib
import json
from pathlib import Path

import duckdb


def sha256(path):
    h = hashlib.sha256()
    with open(path, "rb") as handle:
        for chunk in iter(lambda: handle.read(1048576), b""):
            h.update(chunk)
    return h.hexdigest()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("configuration", type=Path)
    args = parser.parse_args()
    cfg = json.loads(args.configuration.read_text(encoding="utf-8-sig"))
    out = Path(cfg["output_root"])
    if (out / "extraction.json").exists():
        raise RuntimeError("Existing extraction retained")
    database_digest = sha256(cfg["database"])
    if cfg.get("database_expected_sha256") and database_digest != cfg["database_expected_sha256"]:
        raise RuntimeError("Occurrence database checksum differs from immutable source")
    con = duckdb.connect(cfg["database"], read_only=True)
    con.execute("SET threads = " + str(int(cfg.get("threads", 4))))
    con.execute("SET memory_limit = '" + str(int(cfg.get("memory_gb", 8))) + "GB'")
    # Only in-memory temporary tables are created in the read-only connection.
    with open(cfg["target_roster"], encoding="utf-8-sig", newline="") as handle:
        roster = list(csv.DictReader(handle))
    roles = cfg.get("target_asset_roles")
    if roles:
        roster = [r for r in roster if r["asset_role"] in roles]
        if not roster or any(str(r["used_in_paired_sensitivity"]).lower() not in ("true", "1", "yes") for r in roster):
            raise RuntimeError("Manifest selection includes records outside paired sensitivity")
    taxa = [r[cfg.get("target_species_column", "species")].strip() for r in roster]
    if not taxa or len(set(taxa)) != len(taxa) or any(not s for s in taxa):
        raise RuntimeError("Target roster must be nonempty and unique")
    if cfg.get("expected_target_count") and len(taxa) != cfg["expected_target_count"]:
        raise RuntimeError("Target roster count differs from frozen scope")
    con.execute("CREATE TEMP TABLE targets(species VARCHAR)")
    con.executemany("INSERT INTO targets VALUES (?)", [(s,) for s in taxa])
    con.execute("CREATE TEMP TABLE windows AS SELECT * FROM read_csv_auto(?)", [str(out / "windows.csv")])
    lo, hi = int(cfg.get("minimum_valid_year", 1600)), int(cfg["maximum_valid_year"])
    valid = "year BETWEEN {} AND {}".format(lo, hi)
    coordinates = ("isfinite(decimalLongitude) AND isfinite(decimalLatitude) AND "
                   "decimalLongitude BETWEEN -180 AND 180 AND decimalLatitude BETWEEN -90 AND 90")
    con.execute("CREATE TEMP VIEW source AS SELECT *, trim(species) AS species_name FROM main.speciesocc_data")

    def save(name, query):
        result = con.execute(query)
        with (out / name).open("w", encoding="utf-8", newline="") as handle:
            writer = csv.writer(handle)
            writer.writerow([v[0] for v in result.description])
            while True:
                batch = result.fetchmany(10000)
                if not batch:
                    break
                writer.writerows(batch)

    save("global_year_qc.csv", """SELECT count(*) source_rows,
        count(DISTINCT species_name) source_species,
        count(*) FILTER(WHERE year IS NULL) missing_year_rows,
        count(*) FILTER(WHERE year IS NOT NULL AND NOT ({valid})) invalid_year_rows,
        count(*) FILTER(WHERE {valid}) valid_year_rows,
        min(year) earliest_raw_year,max(year) latest_raw_year,
        count(*) FILTER(WHERE {coordinates}) valid_coordinate_rows,
        count(*) FILTER(WHERE decimalLongitude=0 AND decimalLatitude=0) zero_zero_rows,
        count(*) FILTER(WHERE species_name IN(SELECT species FROM targets)) target_rows
        FROM source""".format(valid=valid, coordinates=coordinates))
    save("global_year_counts.csv", """SELECT year,count(*) source_rows,
        count(DISTINCT species_name) species_count,
        count(*) FILTER(WHERE species_name IN(SELECT species FROM targets)) target_rows
        FROM source GROUP BY year ORDER BY year""")
    save("target_year_qc.csv", """SELECT t.species,count(s.species_name) source_rows,
        count(*) FILTER(WHERE s.species_name IS NOT NULL AND s.year IS NULL) missing_year_rows,
        count(*) FILTER(WHERE s.species_name IS NOT NULL AND s.{valid}) valid_year_rows,
        min(s.year) earliest_raw_year,max(s.year) latest_raw_year
        FROM targets t LEFT JOIN source s ON t.species=s.species_name GROUP BY t.species ORDER BY t.species""".format(valid=valid))
    # A single spatial range join avoids scanning the source once per polygon.
    # Exact coordinate/species/year duplicates are retained as a count, not treated as independent detections.
    con.execute("""CREATE TEMP TABLE candidates AS
        SELECT species_name AS species,decimalLongitude longitude,decimalLatitude latitude,year,
        count(*) source_rows, string_agg(DISTINCT countryCode,'|') country_codes,
        count(DISTINCT countryCode) country_count,
        species_name IN(SELECT species FROM targets) is_target,
        ({valid}) valid_year,
        decimalLongitude=0 AND decimalLatitude=0 zero_zero
        FROM source s WHERE {coordinates} AND EXISTS(
          SELECT 1 FROM windows w WHERE s.decimalLongitude BETWEEN w.xmin AND w.xmax
          AND s.decimalLatitude BETWEEN w.ymin AND w.ymax)
        GROUP BY species_name,decimalLongitude,decimalLatitude,year""".format(valid=valid, coordinates=coordinates))
    n = con.execute("SELECT count(*) FROM candidates").fetchone()[0]
    if n > int(cfg["maximum_candidate_rows"]):
        raise RuntimeError("Candidate row bound exceeded: {}".format(n))
    save("candidate_records.csv", "SELECT * FROM candidates ORDER BY species,longitude,latitude,year NULLS LAST")
    Path(out / "target_roster_used.csv").write_text("species\n" + "\n".join('"' + s.replace('"', '""') + '"' for s in taxa) + "\n", encoding="utf-8")
    receipt = dict(status="BOUNDED_EXTRACTION", database_sha256=database_digest,
                   database_bytes=Path(cfg["database"]).stat().st_size, duckdb_version=duckdb.__version__,
                   configuration_sha256=sha256(args.configuration), roster_sha256=sha256(cfg["target_roster"]),
                   roster_scope=cfg["target_scope"], target_count=len(taxa), candidate_rows=n,
                   year_valid_range=[lo, hi], native_status="UNRESOLVED",
                   effort_definition="All source-table records and target-group records; not verified all vascular plants",
                   limitations=["No occurrence identifiers, event dates, coordinate uncertainty or establishment status",
                                "Exact species-coordinate-year collapse cannot resolve specimen identity",
                                "Year validity bounds flag records without deleting candidate evidence"])
    (out / "extraction.json").write_text(json.dumps(receipt, indent=2) + "\n", encoding="utf-8")
    con.close()


if __name__ == "__main__":
    main()
