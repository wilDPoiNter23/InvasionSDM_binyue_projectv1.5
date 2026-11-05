"""Build two-threshold conversion rows from computed main calibration records."""
import argparse
import csv
import re
from pathlib import Path


def build(thresholds, output_root, manifest):
    if Path(manifest).exists():
        raise ValueError('Manifest exists')
    with open(thresholds, newline='', encoding='utf-8-sig') as stream:
        rows = list(csv.DictReader(stream))
    if len({r['species'] for r in rows}) != len(rows) or any(r['group'] != 'FULL' for r in rows):
        raise ValueError('Unique main FULL rows required')
    result = []
    names = set()
    for row in rows:
        stem = re.sub(r'[^A-Za-z0-9_-]', '_', row['species'])
        if stem in names:
            raise ValueError('Species filename collision')
        names.add(stem)
        for method in ('t10', 'max_tss'):
            result.append(dict(species=row['species'], group='FULL', threshold=method,
                projection_path=row['projection_path'], cutoff_raw=row[method + '_raw'],
                scale_divisor=row['scale_divisor'],
                output_path=str(Path(output_root) / method / (stem + '__' + method + '.tif')),
                validation_scope=row['validation_scope'], scientific_status=row['scientific_status']))
    with open(manifest, 'w', newline='', encoding='utf-8') as stream:
        writer = csv.DictWriter(stream, fieldnames=list(result[0]));writer.writeheader();writer.writerows(result)
    return len(rows), len(result)


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    for key in ('thresholds', 'output_root', 'manifest'):
        parser.add_argument(key)
    args = parser.parse_args(); print(build(args.thresholds, args.output_root, args.manifest))
