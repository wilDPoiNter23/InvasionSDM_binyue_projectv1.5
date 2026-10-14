#!/usr/bin/env python3
"""Reconstruct native-cell mining summaries and adjusted point estimates."""
import csv
import json
import math
import sys
from collections import defaultdict
from pathlib import Path
import numpy as np


def records(path):
    with Path(path).open(encoding="utf-8-sig", newline="") as handle:
        return list(csv.DictReader(handle))


def design(rows, model):
    rec = np.array([float(r["reclamation_km2"]) for r in rows])
    damage = np.array([float(r["damage_km2"]) for r in rows])
    area = np.array([float(r["footprint_km2"]) for r in rows])
    lat = np.radians([float(r["cell_latitude"]) for r in rows])
    lon = np.radians([float(r["cell_longitude"]) for r in rows])
    cols = [np.log1p(rec), np.log1p(damage), np.log1p(area),
            lat / (math.pi / 2), (lat / (math.pi / 2)) ** 2,
            np.cos(lat) * np.cos(lon), np.cos(lat) * np.sin(lon)]
    if model == "reclamation_timing":
        cols.append(np.array([(float(r["endpoint_area_sum"]) / float(r["reclamation_km2"]) - 2000) / 10 for r in rows]))
    return np.column_stack(cols)


def main():
    cfg = json.loads(Path(sys.argv[1]).read_text(encoding="utf-8-sig"))
    out = Path(cfg["output_root"])
    # Absence is a failed check, never evidence of successful execution.
    points = records(out / "mine_points.csv")
    cells = records(out / "native_cells.csv")
    estimates = records(out / "model_estimates.csv")
    receipt = json.loads((out / "screen_complete.json").read_text())
    raw = {r["polygon_id"]: r for r in records(cfg["mining_csv"])}
    rec_cols = [k for k in next(iter(raw.values())) if k.startswith("reclamation_area_km2_")]
    dam_cols = [k for k in next(iter(raw.values())) if k.startswith("damage_area_km2_")]
    grid = receipt["grid"]
    checks = []

    def check(name, ok):
        checks.append({"name": name, "passed": bool(ok)})

    if cfg.get("grid_metadata"):
        precise = json.loads(Path(cfg["grid_metadata"]).read_text())
        previous = {v["path"]: v["sha256"] for v in receipt["inputs"]}
        check("precise metadata bound to the original raster inputs",
              all(previous.get(v["path"]) == v["sha256"] for v in precise["inputs"]))
        check("precise metadata preserves extent and dimensions",
              all(grid[k] == precise["grid"][k] for k in ["xmin", "xmax", "ymin", "ymax", "nrow", "ncol"]))
        grid = precise["grid"]
    check("grid resolution agrees with extent and dimensions",
          abs(grid["xres"] * grid["ncol"] - (grid["xmax"] - grid["xmin"])) < 1e-9
          and abs(grid["yres"] * grid["nrow"] - (grid["ymax"] - grid["ymin"])) < 1e-9)

    check("unique complete polygon identities", len(points) == len(raw) == len({r["polygon_id"] for r in points}))
    correct = True
    for row in points:
        s = raw[row["polygon_id"]]
        r = sum(float(s[k]) for k in rec_cols)
        d = sum(float(s[k]) for k in dam_cols)
        e = sum(float(s[k]) * int(k.rsplit("_", 1)[-1]) for k in rec_cols)
        correct &= math.isclose(r, float(row["reclamation_km2"]), rel_tol=1e-10, abs_tol=1e-10)
        correct &= math.isclose(d, float(row["damage_km2"]), rel_tol=1e-10, abs_tol=1e-10)
        correct &= math.isclose(e, float(row["endpoint_area_sum"]), rel_tol=1e-10, abs_tol=1e-7)
        lon, lat = float(s["centroid_longitude_deg"]), float(s["centroid_latitude_deg"])
        col = math.floor((lon - grid["xmin"]) / grid["xres"])
        rix = math.floor((grid["ymax"] - lat) / grid["yres"])
        if 0 <= col < grid["ncol"] and 0 <= rix < grid["nrow"]:
            correct &= int(row["cell"]) == rix * grid["ncol"] + col + 1
    check("source period sums and geographic cell indexing", correct)
    groups = defaultdict(list)
    for row in points:
        if row["cell"] == "NA":
            continue
        groups[("all_centroids", row["cell"])].append(row)
        if row["compact"] == "TRUE":
            groups[("compact_centroids", row["cell"])].append(row)
    aggregate_ok = True
    for row in cells:
        z = groups[(row["scope"], row["cell"])]
        aggregate_ok &= len(z) == int(row["n_mines"])
        for key in ["footprint_km2", "damage_km2", "reclamation_km2", "endpoint_area_sum"]:
            aggregate_ok &= math.isclose(sum(float(v[key]) for v in z), float(row[key]), rel_tol=1e-9, abs_tol=1e-7)
        aggregate_ok &= len({v["country"] for v in z}) == int(row["n_countries"])
        for key in ["FULL", "ENV"]:
            if row[key] != "NA":
                aggregate_ok &= all(v[key] != "NA" and abs(float(v[key]) - float(row[key])) < 1e-10 for v in z)
    check("native-cell aggregation and shared-cell response identity", aggregate_ok)
    coefficient_ok = True
    fits = {}
    for result in estimates:
        key = (result["scope"], result["model"], result["response"])
        if key not in fits:
            scope, model, response = key
            z = [r for r in cells if r["scope"] == scope and int(r["n_countries"]) == 1 and r["FULL"] != "NA" and r["ENV"] != "NA" and (model != "reclamation_timing" or float(r["reclamation_km2"]) > 0)]
            counts = defaultdict(int)
            for r in z:
                counts[r["country"]] += 1
            z = [r for r in z if counts[r["country"]] >= cfg["minimum_cells_per_country"]]
            x = design(z, model)
            y = np.array([float(r[response]) for r in z])
            for country in {r["country"] for r in z}:
                ix = [i for i, r in enumerate(z) if r["country"] == country]
                x[ix] -= x[ix].mean(axis=0)
                y[ix] -= y[ix].mean()
            beta = np.linalg.lstsq(x, y, rcond=None)[0]
            fits[key] = (beta, len(z))
        beta, n = fits[key]
        coefficient_ok &= n == int(result["n_cells"])
        coefficient_ok &= abs(beta[int(result["term_index"]) - 1] - float(result["estimate"])) < 1e-8
        coefficient_ok &= float(result["ci_low"]) <= float(result["estimate"]) <= float(result["ci_high"])
    check("independent country-demeaned least-squares reconstruction", coefficient_ok)
    check("prediction range", all(0 <= float(r[k]) <= 1 for r in cells for k in ["FULL", "ENV"] if r[k] != "NA"))
    passed = all(v["passed"] for v in checks)
    report = {"status": "PASS_NATIVE_CELL_SCREEN" if passed else "FAIL", "checks": checks,
              "scope": "Centroid assignment and exploratory static-suitability associations; not polygon extraction or observed invasion",
              "cluster_interval_reconstruction": False}
    target = out / cfg.get("verification_filename", "independent_verification.json")
    if target.exists():
        raise FileExistsError("Existing verification retained: " + str(target))
    target.write_text(json.dumps(report, indent=2) + "\n")
    print(json.dumps(report, indent=2))
    return 0 if passed else 1


if __name__ == "__main__":
    raise SystemExit(main())
