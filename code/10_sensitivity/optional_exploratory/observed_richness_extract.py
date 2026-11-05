"""Extract full-city occurrences from a selected invasive-species database."""
import csv
import hashlib
import json
import sys
from pathlib import Path

import duckdb


def sha256(path):
    h = hashlib.sha256()
    with open(path, "rb") as f:
        for block in iter(lambda: f.read(1048576), b""):
            h.update(block)
    return h.hexdigest()


def main():
    config = Path(sys.argv[1])
    cfg = json.loads(config.read_text(encoding="utf-8-sig"))
    out = Path(cfg["output_root"])
    if (out / "richness_extraction.json").exists():
        raise RuntimeError("Existing extraction retained")
    digest = sha256(cfg["database"])
    if cfg.get("database_expected_sha256") and digest != cfg["database_expected_sha256"]:
        raise RuntimeError("Source database checksum mismatch")
    con = duckdb.connect(cfg["database"], read_only=True)
    con.execute("SET threads = " + str(int(cfg.get("threads", 4))))
    con.execute("SET memory_limit = '" + str(int(cfg.get("memory_gb", 8))) + "GB'")
    con.execute("CREATE TEMP TABLE windows AS SELECT * FROM read_csv_auto(?)", [str(out / "windows.csv")])
    con.execute("CREATE TEMP VIEW source AS SELECT trim(species) species, decimalLongitude longitude, decimalLatitude latitude, year, countryCode FROM main.speciesocc_data")

    def save(name, query):
        result = con.execute(query)
        with (out / name).open("w", encoding="utf-8", newline="") as f:
            writer = csv.writer(f)
            writer.writerow([v[0] for v in result.description])
            while True:
                batch = result.fetchmany(10000)
                if not batch:
                    break
                writer.writerows(batch)

    qc = con.execute("""SELECT count(*),count(DISTINCT species),
      count(*) FILTER(WHERE year IS NULL),
      count(*) FILTER(WHERE year IS NOT NULL AND NOT(year BETWEEN 1600 AND 2024)),
      count(*) FILTER(WHERE NOT(coalesce(isfinite(longitude) AND isfinite(latitude)
      AND longitude BETWEEN -180 AND 180 AND latitude BETWEEN -90 AND 90,FALSE))),
      count(*) FILTER(WHERE species IS NULL OR species='') FROM source""").fetchone()
    for key, value in [("expected_source_rows", qc[0]), ("expected_source_species", qc[1])]:
        if cfg.get(key) is not None and int(cfg[key]) != value:
            raise RuntimeError("Source contract mismatch: " + key)
    if qc[5]:
        raise RuntimeError("Blank species names require an explicit scope decision")
    save("raw_species_roster.csv", "SELECT species,count(*) source_rows FROM source GROUP BY species ORDER BY species")
    save("source_year_counts.csv", "SELECT year,count(*) source_rows,count(DISTINCT species) species_count FROM source GROUP BY year ORDER BY year NULLS LAST")
    # Each source row hits the same number of windows for its coordinate.
    # Dividing by that multiplicity retains exact source duplicate counts.
    con.execute("""CREATE TEMP TABLE candidates AS
      SELECT species,longitude,latitude,year,
       CAST(count(*) / count(DISTINCT w.city_id) AS BIGINT) source_rows,
       string_agg(DISTINCT countryCode,'|') country_codes,count(DISTINCT countryCode) country_count,
       coalesce(year BETWEEN 1600 AND 2024,FALSE) valid_year,
       longitude=0 AND latitude=0 zero_zero FROM source s
      JOIN windows w ON s.longitude BETWEEN w.xmin AND w.xmax
       AND s.latitude BETWEEN w.ymin AND w.ymax
       WHERE isfinite(longitude) AND isfinite(latitude) AND longitude BETWEEN -180 AND 180
       AND latitude BETWEEN -90 AND 90
       GROUP BY species,longitude,latitude,year""")
    n = con.execute("SELECT count(*) FROM candidates").fetchone()[0]
    if n > int(cfg["maximum_candidate_rows"]):
        raise RuntimeError("Candidate row bound exceeded")
    save("candidate_records.csv", "SELECT * FROM candidates ORDER BY species,longitude,latitude,year NULLS LAST")
    receipt = dict(status="FULL_CITY_CANDIDATES", database_sha256=digest,
        configuration_sha256=sha256(config), windows_sha256=sha256(out / "windows.csv"),
        duckdb_version=duckdb.__version__, source_rows=qc[0], source_species=qc[1],
        missing_year_rows=qc[2], invalid_year_rows=qc[3], invalid_coordinate_rows=qc[4],
        candidate_rows=n, valid_year_range=[1600,2024],
        scope="Selected invasive-species source roster; local native status unresolved",
        intensity_definition="Target-group unique species-coordinate-year records; no independent all-flora effort",
        limitations=["No record identifiers or coordinate uncertainty", "Missing and invalid years retained for QA"])
    (out / "richness_extraction.json").write_text(json.dumps(receipt, indent=2) + "\n")
    con.close()


if __name__ == "__main__":
    main()
