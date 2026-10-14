"""Independently recompute thresholds from retained point-score tables."""
import argparse
import csv
import hashlib
import json
import math
from pathlib import Path
from binary_thresholds import maximum_tss, quantile_type7


def verify(directory):
    root = Path(directory)
    receipts = []
    for table in sorted(root.rglob('thresholds.csv')):
        with table.open(newline='', encoding='utf-8-sig') as stream:
            rows = list(csv.DictReader(stream))
        for row in rows:
            source = table.parent / row['point_score_table']
            with source.open(newline='', encoding='utf-8-sig') as stream:
                points = list(csv.DictReader(stream))
            scores = [float(p['score_raw']) if p['score_raw'] not in ('', 'NA') else None for p in points]
            labels = [int(p['label']) for p in points]
            presence = [s for s, y in zip(scores, labels) if y == 1]
            t10 = quantile_type7(presence)
            tss = maximum_tss(scores, labels)
            checks = {
                't10_raw': math.isclose(t10, float(row['t10_raw']), abs_tol=1e-8, rel_tol=1e-10),
                'max_tss_raw': math.isclose(tss['cutoff'], float(row['max_tss_raw']), abs_tol=1e-8, rel_tol=1e-10),
                'max_tss': math.isclose(tss['tss'], float(row['max_tss']), abs_tol=1e-10),
                'finite_presence': sum(y == 1 and s is not None and math.isfinite(s) for s, y in zip(scores, labels)) == int(row['n_finite_presence']),
                'finite_background': sum(y == 0 and s is not None and math.isfinite(s) for s, y in zip(scores, labels)) == int(row['n_finite_background']),
                'source_scope': row['validation_scope'] == 'CURRENT_MAP_CALIBRATION_POOLED_PA',
                'inclusive_boundary': row['boundary'] == 'score >= cutoff',
            }
            receipts.append(dict(species=row['species'], group=row['group'], checks=checks,
                status='PASS' if all(checks.values()) else 'FAIL',
                point_scores_sha256=hashlib.sha256(source.read_bytes()).hexdigest(),
                threshold_table_sha256=hashlib.sha256(table.read_bytes()).hexdigest()))
    if not receipts:
        raise ValueError('No completed threshold tables found')
    return dict(status='PASS' if all(x['status'] == 'PASS' for x in receipts) else 'FAIL',
                products=len(receipts), records=receipts)


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('directory'); parser.add_argument('receipt')
    args = parser.parse_args()
    if Path(args.receipt).exists():
        raise ValueError('Receipt exists')
    result = verify(args.directory)
    Path(args.receipt).write_text(json.dumps(result, indent=2), encoding='utf-8')
    print(json.dumps({k: result[k] for k in ('status', 'products')}))
    raise SystemExit(0 if result['status'] == 'PASS' else 1)
