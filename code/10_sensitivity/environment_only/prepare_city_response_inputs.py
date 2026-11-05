"""Adapt verified city-zone means to the existing city-model input schemas."""
import argparse
import csv
import json
import math
from pathlib import Path


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--zone-means', type=Path, required=True)
    parser.add_argument('--city-input', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    args.output.mkdir(parents=True, exist_ok=True)
    key = lambda x: x.strip().lower()
    accum = {}
    with args.zone_means.open(encoding='utf-8-sig', newline='') as handle:
        for row in csv.DictReader(handle):
            if row['rule'] != 'alien_mean' or row['footprint'] != 'HISTORICAL':
                continue
            try:
                value, area = float(row['mean']), float(row['finite_area_km2'])
            except ValueError:
                continue
            if not math.isfinite(value) or area <= 0:
                continue
            record = accum.setdefault((key(row['city_id']), row['group']), [0., 0.])
            record[0] += value * area
            record[1] += area
    means = {k: v[0] / v[1] for k, v in accum.items()}
    with args.city_input.open(encoding='utf-8-sig', newline='') as handle:
        reader = csv.DictReader(handle)
        fields, cities = reader.fieldnames, list(reader)
    excluded = {'zhanzhou', 'danzhou'}
    missing = [r['city'] for r in cities if key(r['city']) not in excluded
               and any((key(r['city']), g) not in means
                       for g in ('FULL_814', 'PAIRED_FULL', 'PAIRED_ENV'))]
    if missing:
        raise ValueError('Missing city responses: ' + ', '.join(missing))
    with (args.output / 'full_city_input.csv').open('w', newline='', encoding='utf-8') as handle:
        writer = csv.DictWriter(handle, fieldnames=fields)
        writer.writeheader()
        for row in cities:
            row = dict(row)
            row['risk'] = means.get((key(row['city']), 'FULL_814'), 'NA')
            writer.writerow(row)
    with (args.output / 'paired_city_input.csv').open('w', newline='', encoding='utf-8') as handle:
        writer = csv.writer(handle)
        writer.writerow(['city', 'PAIRED_FULL_684', 'PAIRED_ENV_684'])
        for row in cities:
            city = key(row['city'])
            if city not in excluded:
                writer.writerow([row['city'], means[(city, 'PAIRED_FULL')],
                                 means[(city, 'PAIRED_ENV')]])
    (args.output / 'response_identity.json').write_text(json.dumps(dict(
        definition='Area-weighted city mean of finite non-native species means',
        rule='Native domains excluded; genuine missing predictions retained as unknown',
        existing_script_labels=dict(ARCHIVED_GAM_RISK='FULL_814_NATIVE',
                                    PAIRED_FULL='FULL_684_NATIVE', PAIRED_ENV='ENV_684_NATIVE'),
        predictors='Unchanged; only risk and paired response columns replaced',
        retained_legacy_columns='Outcome-derived classifications such as risk_class are not updated and must not be used as new hotspot classifications',
        city_input_rows=len(cities), excluded_city_keys=sorted(excluded)), indent=2))
    print('Prepared unchanged city-model schemas from verified native-masked means')


if __name__ == '__main__':
    main()
