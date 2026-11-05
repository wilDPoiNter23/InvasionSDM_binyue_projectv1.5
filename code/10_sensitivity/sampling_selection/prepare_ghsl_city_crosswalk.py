"""Match source cities to GHSL names, retaining explicit reviewed decisions.

Usage: python prepare_ghsl_city_crosswalk.py configuration.json
Centroid coordinates must have been transformed from the published geometry
CRS to WGS84. The provider's centroid LON/LAT-named fields are projected metres.
Reviewed links are external data with key, ghsl_ids, reason and match_status.
No nearest-city fallback is used for unmatched names.
"""
import csv
import hashlib
import json
import math
from pathlib import Path
import re
import sqlite3
import sys
import unicodedata


def normalise(value):
    value = value or ""
    if "\u00c3" in value:
        try:
            value = value.encode("latin1").decode("utf8")
        except (UnicodeEncodeError, UnicodeDecodeError):
            pass
    value = value.replace("\u0110", "D").replace("\u0111", "d")
    value = unicodedata.normalize("NFKD", value).encode("ascii", "ignore").decode()
    return re.sub(r"[^a-z0-9]+", " ", value.lower()).strip()


def read_csv(path):
    with open(path, encoding="utf-8-sig", newline="") as handle:
        return list(csv.DictReader(handle))


def distance(source, target):
    try:
        a = float(source["latitude_raw"].replace(",", ""))
        b = float(source["longitude_raw"])
    except ValueError:
        return None
    p, q = map(math.radians, (a, float(target["latitude"])))
    dl = math.radians(float(target["longitude"]) - b)
    h = math.sin((q-p)/2)**2 + math.cos(p)*math.cos(q)*math.sin(dl/2)**2
    return 12742 * math.asin(min(1, math.sqrt(h)))


def main(path):
    cfg = json.loads(Path(path).read_text(encoding="utf-8-sig"))
    out = Path(cfg["output_root"])
    if out.exists():
        raise ValueError('Existing crosswalk retained; use a fresh output_root.')
    out.mkdir(parents=True)
    pool = read_csv(cfg["pool"])
    assert len(pool) == 135 and len({r["key"] for r in pool}) == 135
    conn = sqlite3.connect(cfg["general_gpkg"])
    conn.row_factory = sqlite3.Row
    rows = [dict(r) for r in conn.execute(
        "SELECT ID_UC_G0, GC_UCN_MAI_2025 AS name, GC_UCN_LIS_2025 AS names, "
        "GC_CNT_GAD_2025 AS country, GC_CNT_UNN_2025 AS country_un "
        "FROM GHSL_UCDB_THEME_GENERAL_CHARACTERISTICS_GLOBE_R2024A")]
    conn.close()
    centroids = {int(r["ID_UC_G0"]): r for r in read_csv(cfg["centroids"])}
    for r in rows:
        r.update({k: v for k, v in centroids[r["ID_UC_G0"]].items() if k != "ID_UC_G0"})
    by_id = {r["ID_UC_G0"]: r for r in rows}
    reviewed = {r["key"]: r for r in read_csv(cfg["reviewed_links"])}
    country_alias = {"usa": "united states", "uae": "united arab emirates",
                     "republic of korea": "south korea", "russain federation": "russia",
                     "sa": "south africa", "dominican rep": "dominican republic",
                     "china sar": "china"}
    result = []
    for source in pool:
        country = country_alias.get(normalise(source["country_raw"]), normalise(source["country_raw"]))
        name = normalise(source["city"])
        candidates = [r for r in rows if country in (normalise(r["country"]), normalise(r["country_un"]))]
        matches = [r for r in candidates if name == normalise(r["name"]) or
                   name in [normalise(v) for v in re.split("[;,]", r["names"] or "")]]
        if source["key"] in reviewed:
            decision = reviewed[source["key"]]
            ids = [int(x) for x in decision["ghsl_ids"].split(";") if x]
            status, reason = decision["match_status"], decision["reason"]
        elif len(matches) == 1:
            ids = [matches[0]["ID_UC_G0"]]
            status, reason = "EXACT_NAME_COUNTRY", "Unique main-name or constituent-name match within country"
        else:
            ids = []
            status, reason = "UNRESOLVED", "No unique verified name-country match"
        assert all(i in by_id for i in ids)
        d = [distance(source, by_id[i]) for i in ids]
        d = [v for v in d if v is not None]
        result.append(dict(source_key=source["key"], source_city=source["city"],
                           source_country=source["country_raw"].strip(),
                           source_group=source["group"], continent=source["continent"],
                           retained_exact=source["retained_exact"],
                           ghsl_ids=";".join(map(str, sorted(set(ids)))),
                           ghsl_names="; ".join(by_id[i]["name"] or "" for i in ids),
                           ghsl_countries="; ".join(sorted({by_id[i]["country_un"] for i in ids})),
                           match_status=status, match_reason=reason,
                           source_to_nearest_centroid_km=min(d) if d else None,
                           alternative_name_match_ids=";".join(str(r["ID_UC_G0"]) for r in matches)))
    with (out / "city_crosswalk.csv").open("w", encoding="utf-8", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=result[0]); writer.writeheader(); writer.writerows(result)
    receipt = {"source_records": 135, "matched_records": sum(bool(r["ghsl_ids"]) for r in result),
               "unresolved": [r["source_city"] for r in result if not r["ghsl_ids"]],
               "inputs": {k: hashlib.sha256(Path(cfg[k]).read_bytes()).hexdigest()
                          for k in ("pool", "general_gpkg", "centroids", "reviewed_links")}}
    (out / "crosswalk_receipt.json").write_text(json.dumps(receipt, indent=2), encoding="utf-8")
    print(json.dumps({k: receipt[k] for k in ("source_records", "matched_records", "unresolved")}))


if __name__ == "__main__":
    main(sys.argv[1])
