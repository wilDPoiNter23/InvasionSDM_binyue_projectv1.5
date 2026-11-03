"""Count public GBIF Plantae records in verified WGS84 GHSL geometries.

Usage: python query_gbif_city_record_counts.py CONFIG.json
Config requires geometry_csv and output_root; relative paths resolve beside the
configuration. CSV columns: ID_UC_G0, geometry_wkt, area_km2. Identical repeated
IDs are collapsed; conflicting geometry or area is rejected before querying.
Geometry must already be validated, use longitude/latitude order and correctly
oriented POLYGON/MULTIPOLYGON rings. This standard-library client does not repair
or simplify geometries. Area must be a separately verified positive km2 value.

windows defaults to ["all_available", "years2000_2024"]. Add
"precise2000_2024" to that list, or set include_precise=true, for the separate
2000-2024 sensitivity restricted to reported uncertainty of 0-1000 metres.
all_available includes undated records and has no year cutoff. The two dated
windows use occurrence years, not publication dates or historical availability.
All windows require Plantae (GBIF backbone key 6), coordinates, PRESENT status
and no GBIF geospatial issue. No country, selected-species, native-range, or
basis-of-record filter is applied. Counts describe reporting volume, including
duplicates and any record types passing these filters, not sampling visits or
inventory completeness. Precision filtering excludes unreported uncertainty.

Only anonymous GET taxonomy and POST search requests are made; no downloads
are requested. Endpoint: https://api.gbif.org/v1/occurrence/search/predicate
Reference: https://techdocs.gbif.org/en/openapi/v1/occurrence
Keep outputs outside the code repository. Stored requests, responses, geometry
hashes and UTC times document a mutable index snapshot; they are not a frozen
GBIF download. No API response is converted to zero on failure. Successful
response files are reused only after hash and count validation. Incompatible
evidence stops the run; use a distinct output_root for a changed protocol.
"""

import argparse
import csv
import hashlib
import json
import math
from pathlib import Path
import re
import sys
import time
from datetime import datetime, timezone
from email.utils import parsedate_to_datetime
from urllib.error import HTTPError, URLError
from urllib.request import Request, urlopen


SEARCH_URL = "https://api.gbif.org/v1/occurrence/search/predicate"
TAXON_URL = "https://api.gbif.org/v1/species/6"
WINDOWS = {"all_available", "years2000_2024", "precise2000_2024"}


def utc_now():
    return datetime.now(timezone.utc).isoformat()


def encoded(value):
    return json.dumps(value, sort_keys=True, separators=(",", ":"),
                      ensure_ascii=False, allow_nan=False).encode("utf-8")


def digest(value):
    return hashlib.sha256(encoded(value)).hexdigest()


def write_json(path, value):
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_suffix(path.suffix + ".tmp")
    temporary.write_text(json.dumps(value, indent=2, ensure_ascii=False,
                                    allow_nan=False) + "\n", encoding="utf-8")
    temporary.replace(path)


def read_json(path):
    return json.loads(path.read_text(encoding="utf-8-sig"))


def ensure_request(path, expected):
    if path.exists():
        existing = read_json(path)
        timestamp = existing.pop("created_at_utc", None)
        if timestamp is None or datetime.fromisoformat(timestamp).utcoffset() is None:
            raise ValueError("Existing request lacks a timestamp: " + str(path))
        if existing != expected:
            raise ValueError("Incompatible existing request: " + str(path))
    else:
        write_json(path, dict(expected, created_at_utc=utc_now()))


def valid_count(response):
    count = response.get("count")
    if type(count) is not int or count < 0:
        raise ValueError("Response lacks a nonnegative integer count")
    if response.get("limit") != 0 or response.get("results") != []:
        raise ValueError("Response does not match a count-only request")
    return count


def read_geometries(path):
    rows = {}
    with path.open(encoding="utf-8-sig", newline="") as handle:
        reader = csv.DictReader(handle)
        required = {"ID_UC_G0", "geometry_wkt", "area_km2"}
        if not required.issubset(reader.fieldnames or []):
            raise ValueError("Geometry CSV lacks required columns")
        for line, source in enumerate(reader, 2):
            identifier = source["ID_UC_G0"].strip()
            if not re.fullmatch(r"[0-9]+", identifier):
                raise ValueError("Invalid GHSL identifier on CSV line " + str(line))
            identifier = str(int(identifier))
            geometry = source["geometry_wkt"].strip()
            if not re.match(r"^(?:MULTIPOLYGON|POLYGON)\s*\(", geometry):
                raise ValueError("Expected WGS84 polygon for " + identifier)
            area = float(source["area_km2"])
            if not math.isfinite(area) or area <= 0:
                raise ValueError("Area must be finite and positive for " + identifier)
            row = {"ID_UC_G0": identifier, "geometry_wkt": geometry,
                   "area_km2": area,
                   "geometry_sha256": hashlib.sha256(geometry.encode()).hexdigest()}
            if identifier in rows and rows[identifier] != row:
                raise ValueError("Conflicting geometry or area for " + identifier)
            rows[identifier] = row
    if not rows:
        raise ValueError("Geometry CSV is empty")
    return list(rows.values())


def make_request(row, window):
    if window not in WINDOWS:
        raise ValueError("Unknown window: " + str(window))
    predicates = [{"type": "equals", "key": key, "value": value} for key, value in
                  (("TAXON_KEY", "6"), ("HAS_COORDINATE", "true"),
                   ("HAS_GEOSPATIAL_ISSUE", "false"),
                   ("OCCURRENCE_STATUS", "PRESENT"))]
    if window != "all_available":
        predicates.extend([
            {"type": "greaterThanOrEquals", "key": "YEAR", "value": "2000"},
            {"type": "lessThanOrEquals", "key": "YEAR", "value": "2024"}])
    if window == "precise2000_2024":
        predicates.extend([
            {"type": "greaterThanOrEquals", "key": "COORDINATE_UNCERTAINTY_IN_METERS",
             "value": "0"},
            {"type": "lessThanOrEquals", "key": "COORDINATE_UNCERTAINTY_IN_METERS",
             "value": "1000"}])
    predicates.append({"type": "within", "geometry": row["geometry_wkt"]})
    request = {"method": "POST", "url": SEARCH_URL,
               "body": {"predicate": {"type": "and", "predicates": predicates},
                        "limit": 0}}
    return {"schema_version": 1, "ID_UC_G0": row["ID_UC_G0"], "window": window,
            "area_km2": row["area_km2"], "geometry_sha256": row["geometry_sha256"],
            "request": request, "request_sha256": digest(request)}


class PublicClient:
    def __init__(self):
        self.last_finished = 0.0
        self.calls = 0

    def fetch(self, request):
        attempts = []
        started = utc_now()
        for attempt in range(5):
            time.sleep(max(0, 0.25 - (time.monotonic() - self.last_finished)))
            body = encoded(request["body"]) if "body" in request else None
            headers = {"Accept": "application/json"}
            if body is not None:
                headers["Content-Type"] = "application/json"
            retry_after = None
            try:
                self.calls += 1
                with urlopen(Request(request["url"], data=body, headers=headers,
                                     method=request["method"]), timeout=60) as response:
                    payload = json.load(response)
                    status = response.status
                return {"started_at_utc": started, "finished_at_utc": utc_now(),
                        "http_status": status, "attempt_count": attempt + 1,
                        "prior_attempts": attempts, "request_sha256": digest(request),
                        "response_sha256": digest(payload), "response": payload}
            except HTTPError as error:
                attempts.append({"utc": utc_now(), "http_status": error.code,
                                 "detail": error.read(4096).decode("utf-8", "replace")})
                if error.code != 429 and not 500 <= error.code <= 599:
                    raise RuntimeError(json.dumps(attempts)) from error
                retry_after = error.headers.get("Retry-After")
            except (URLError, TimeoutError) as error:
                attempts.append({"utc": utc_now(), "error": str(error)})
            finally:
                self.last_finished = time.monotonic()
            if attempt == 4:
                raise RuntimeError("Five attempts exhausted: " + json.dumps(attempts))
            delay = min(30, 2 ** attempt)
            if retry_after:
                try:
                    wait = float(retry_after)
                except ValueError:
                    wait = (parsedate_to_datetime(retry_after) -
                            datetime.now(timezone.utc)).total_seconds()
                if wait > 60:
                    raise RuntimeError("Server asks for a longer pause; retry later: "
                                       + str(retry_after))
                delay = max(delay, wait)
            time.sleep(delay)


def validate_response(evidence, request):
    if evidence.get("request_sha256") != digest(request):
        raise ValueError("Response request hash does not match")
    if evidence.get("response_sha256") != digest(evidence.get("response")):
        raise ValueError("Response body hash does not match")
    if evidence.get("http_status") not in (200, 201):
        raise ValueError("Stored response is not successful")
    for field in ("started_at_utc", "finished_at_utc"):
        if datetime.fromisoformat(evidence[field]).utcoffset() is None:
            raise ValueError("Response timestamp lacks timezone")


def run(config_path):
    config = read_json(config_path)
    def location(key):
        path = Path(config[key])
        return (config_path.parent / path).resolve() if not path.is_absolute() else path.resolve()
    geometry_path, output = location("geometry_csv"), location("output_root")
    rows = read_geometries(geometry_path)
    windows = config.get("windows", ["all_available", "years2000_2024"])
    if not isinstance(windows, list) or not windows or not all(isinstance(w, str) for w in windows):
        raise ValueError("windows must be a nonempty list of window names")
    if config.get("include_precise", False):
        windows = windows + ["precise2000_2024"]
    windows = list(dict.fromkeys(windows))
    plan = [(row, window, make_request(row, window)) for row in rows for window in windows]
    manifest = {"schema_version": 1, "windows": windows,
                "geometries": sorted(rows, key=lambda r: int(r["ID_UC_G0"]))}
    ensure_request(output / "protocol.json", manifest)
    # Check every existing evidence file before initiating network requests.
    for row, window, expected in plan:
        folder = output / "queries" / row["ID_UC_G0"] / window
        ensure_request(folder / "request.json", expected)
        response_path = folder / "response.json"
        if response_path.exists():
            cached = read_json(response_path)
            validate_response(cached, expected["request"])
            valid_count(cached["response"])
            if cached.get("geometry_sha256") != row["geometry_sha256"]:
                raise ValueError("Cached geometry hash differs: " + str(response_path))
    client = PublicClient()
    taxon_request = {"method": "GET", "url": TAXON_URL}
    taxon_path = output / "taxon_lookup.json"
    cached_paths = [output / "queries" / row["ID_UC_G0"] / window / "response.json"
                    for row, window, expected in plan]
    if not taxon_path.exists() and any(path.exists() for path in cached_paths):
        raise ValueError("Cached counts lack their original taxonomy lookup")
    taxon = read_json(taxon_path) if taxon_path.exists() else client.fetch(taxon_request)
    validate_response(taxon, taxon_request)
    identity = taxon["response"]
    if (identity.get("key"), identity.get("scientificName"), identity.get("rank"),
        identity.get("taxonomicStatus")) != (6, "Plantae", "KINGDOM", "ACCEPTED"):
        raise ValueError("GBIF taxon lookup does not identify accepted Plantae")
    if not taxon_path.exists():
        write_json(taxon_path, taxon)
    for path in cached_paths:
        if path.exists() and read_json(path).get("taxon_lookup_sha256") != digest(taxon):
            raise ValueError("Cached taxonomy lookup hash differs: " + str(path))
    summary = []
    failed = 0
    for row, window, expected in plan:
        folder = output / "queries" / row["ID_UC_G0"] / window
        response_path = folder / "response.json"
        result = {"ID_UC_G0": row["ID_UC_G0"], "window": window,
                  "area_km2": row["area_km2"], "count": "", "records_per_km2": "",
                  "status": "FAILED", "finished_at_utc": "",
                  "geometry_sha256": row["geometry_sha256"],
                  "request_sha256": expected["request_sha256"], "detail": ""}
        try:
            if response_path.exists():
                evidence = read_json(response_path)
                status = "CACHED"
            else:
                evidence = client.fetch(expected["request"])
                validate_response(evidence, expected["request"])
                valid_count(evidence["response"])
                evidence.update(geometry_sha256=row["geometry_sha256"],
                                taxon_lookup_sha256=digest(taxon))
                write_json(response_path, evidence)
                status = "QUERIED"
            if evidence.get("taxon_lookup_sha256") != digest(taxon):
                raise ValueError("Cached taxonomy lookup hash differs")
            count = valid_count(evidence["response"])
            result.update(count=count, records_per_km2=count / row["area_km2"],
                          status=status, finished_at_utc=evidence["finished_at_utc"])
        except (RuntimeError, ValueError, OSError) as error:
            failed += 1
            result["detail"] = str(error)
            write_json(folder / ("failure_" + datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%S%fZ") + ".json"),
                       {"utc": utc_now(), "request_sha256": expected["request_sha256"],
                        "geometry_sha256": row["geometry_sha256"], "error": str(error)})
        summary.append(result)
        print(row["ID_UC_G0"], window, result["status"], result["count"], flush=True)
    temporary = output / "counts.csv.tmp"
    with temporary.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=list(summary[0]))
        writer.writeheader()
        writer.writerows(summary)
    temporary.replace(output / "counts.csv")
    report = {"utc": utc_now(), "unique_geometries": len(rows), "windows": windows,
              "http_calls": client.calls, "failed_queries": failed,
              "summary": str(output / "counts.csv")}
    print(json.dumps(report, indent=2))
    return 1 if failed else 0


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("config", type=Path)
    arguments = parser.parse_args()
    try:
        sys.exit(run(arguments.config.resolve()))
    except (ValueError, RuntimeError, OSError, KeyError) as error:
        print("Stopped: " + str(error), file=sys.stderr)
        sys.exit(2)
