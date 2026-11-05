#!/usr/bin/env python3
"""Reconstruct country fixed effects and spatial cluster intervals."""
import csv
import json
import math
import sys
from collections import Counter
from pathlib import Path

import numpy as np


def beta_fraction(a, b, x):
    """Continued fraction for the regularized incomplete beta."""
    tiny = 1e-300
    c = 1.0
    d = 1 - (a + b) * x / (a + 1)
    d = 1 / (d if abs(d) > tiny else tiny)
    h = d
    for m in range(1, 501):
        for numerator in (m * (b - m) * x / ((a + 2*m - 1) * (a + 2*m)),
                          -(a + m) * (a + b + m) * x / ((a + 2*m) * (a + 2*m + 1))):
            d = 1 + numerator * d
            c = 1 + numerator / c
            d = 1 / (d if abs(d) > tiny else tiny)
            c = c if abs(c) > tiny else tiny
            delta = d * c
            h *= delta
        if abs(delta - 1) < 2e-14:
            return h
    raise ArithmeticError("Incomplete beta did not converge")


def regularized_beta(x, a, b):
    if x <= 0:
        return 0.0
    if x >= 1:
        return 1.0
    factor = math.exp(math.lgamma(a+b)-math.lgamma(a)-math.lgamma(b)
                      + a*math.log(x)+b*math.log1p(-x))
    if x < (a+1)/(a+b+2):
        return factor*beta_fraction(a,b,x)/a
    return 1-factor*beta_fraction(b,a,1-x)/b


def student_t_critical(df):
    """Two-sided 95 percent Student-t critical value without SciPy."""
    if df < 1:
        raise ValueError("Positive Student-t degrees of freedom required")
    low, high = 0.0, 32.0
    for _ in range(90):
        mid = (low+high)/2
        tail = .5*regularized_beta(df/(df+mid*mid), df/2, .5)
        if tail > .025:
            low = mid
        else:
            high = mid
    return (low+high)/2


def read_rows(path):
    with Path(path).open(encoding="utf-8-sig", newline="") as handle:
        return list(csv.DictReader(handle))


def main():
    cfg = json.loads(Path(sys.argv[1]).read_text(encoding="utf-8-sig"))
    out = Path(cfg["output_root"])
    target = out / cfg.get("cluster_verification_filename", "cluster_interval_verification.json")
    if target.exists():
        raise FileExistsError("Existing interval verification retained")
    cells = read_rows(out / "native_cells.csv")
    estimates = read_rows(out / "model_estimates.csv")
    fits = {}
    errors = []
    checks = 0
    for result in estimates:
        key = (result["scope"], result["model"], result["response"])
        if key not in fits:
            scope, model, response = key
            rows = [r for r in cells if r["scope"] == scope
                    and int(r["n_countries"]) == 1
                    and r["FULL"] not in ("NA", "NaN", "")
                    and r["ENV"] not in ("NA", "NaN", "")
                    and (model != "reclamation_timing" or float(r["reclamation_km2"]) > 0)]
            counts = Counter(r["country"] for r in rows)
            rows = [r for r in rows if counts[r["country"]] >= cfg["minimum_cells_per_country"]]
            lat = np.radians([float(r["cell_latitude"]) for r in rows])
            lon = np.radians([float(r["cell_longitude"]) for r in rows])
            columns = [np.log1p([float(r[k]) for r in rows])
                       for k in ("reclamation_km2", "damage_km2", "footprint_km2")]
            columns += [lat / (math.pi / 2), (lat / (math.pi / 2)) ** 2,
                        np.cos(lat) * np.cos(lon), np.cos(lat) * np.sin(lon)]
            if model == "reclamation_timing":
                columns += [np.array([(float(r["endpoint_area_sum"]) /
                                       float(r["reclamation_km2"]) - 2000) / 10 for r in rows])]
            x = np.column_stack(columns)
            y = np.array([float(r[response]) for r in rows])
            countries = np.array([r["country"] for r in rows])
            for country in np.unique(countries):
                index = countries == country
                x[index] -= x[index].mean(axis=0)
                y[index] -= y[index].mean()
            if np.linalg.matrix_rank(x) != x.shape[1]:
                raise ValueError("Rank-deficient country-demeaned design")
            beta = np.linalg.lstsq(x, y, rcond=None)[0]
            residual = y - x @ beta
            bread = np.linalg.inv(x.T @ x)
            fits[key] = (rows, x, beta, residual, bread, len(np.unique(countries)))
        rows, x, beta, residual, bread, n_countries = fits[key]
        blocksize = float(result["spatial_block_degrees"])
        labels = [(math.floor((float(r["cell_longitude"]) + 180) / blocksize),
                   math.floor((float(r["cell_latitude"]) + 90) / blocksize)) for r in rows]
        block_keys = {b: i for i, b in enumerate(sorted(set(labels)))}
        score = np.zeros((len(block_keys), x.shape[1]))
        np.add.at(score, [block_keys[b] for b in labels], x * residual[:, None])
        g, n, k = len(block_keys), len(rows), x.shape[1] + n_countries
        covariance = bread @ (score.T @ score) @ bread * g / (g - 1) * (n - 1) / (n - k)
        se = np.sqrt(np.maximum(0, np.diag(covariance)))
        critical = student_t_critical(g - 1)
        index = int(result["term_index"]) - 1
        expected = {"estimate": beta[index], "cluster_se": se[index],
                    "ci_low": beta[index] - critical * se[index],
                    "ci_high": beta[index] + critical * se[index]}
        for field, value in expected.items():
            checks += 1
            if not np.isclose(value, float(result[field]), rtol=1e-7, atol=1e-9):
                errors.append({"fit": key, "block_degrees": blocksize, "term": result["term"],
                               "field": field, "expected": float(value), "stored": float(result[field])})
        for field, value in {"n_cells": n, "n_countries": n_countries, "n_spatial_blocks": g}.items():
            checks += 1
            if int(result[field]) != value:
                errors.append({"fit": key, "field": field, "expected": value, "stored": result[field]})
    if not estimates:
        errors.append({"error": "No model estimates"})
    report = {"status": "PASS_CLUSTER_INTERVAL_RECONSTRUCTION" if not errors else "FAIL",
              "n_fits": len(fits), "n_numeric_checks": checks, "errors": errors,
              "scope": "Country-demeaned coefficients, spatial sandwich covariance and t intervals"}
    target.write_text(json.dumps(report, indent=2) + "\n")
    print(json.dumps(report, indent=2))
    return 0 if not errors else 1


if __name__ == "__main__":
    raise SystemExit(main())
