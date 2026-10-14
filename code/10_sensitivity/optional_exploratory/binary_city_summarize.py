"""Combine paired species fractions with an explicit fixed denominator."""
import argparse
import csv
import json
import math
from collections import defaultdict
from pathlib import Path


def summarize(root, output):
    output = Path(output)
    if output.exists():
        raise ValueError('Output exists')
    grouped = defaultdict(list)
    identities = set()
    inputs = sorted(Path(root).rglob('city_species_threshold_area.csv'))
    if not inputs:
        raise ValueError('No city extraction tables')
    for table in inputs:
        with table.open(newline='', encoding='utf-8-sig') as stream:
            for row in csv.DictReader(stream):
                key = tuple(row[k] for k in ('city_id', 'region', 'group', 'method'))
                identity = key + (row['species'],)
                if identity in identities:
                    raise ValueError('Duplicate species-region response')
                identities.add(identity); grouped[key].append(row)
    results = []
    for key, rows in sorted(grouped.items()):
        fractions = [float(r['suitable_fraction']) for r in rows if r['suitable_fraction'] not in ('NA', '')]
        if any(not math.isfinite(x) or not 0 <= x <= 1 for x in fractions):
            raise ValueError('Invalid suitable fraction')
        complete = len(rows) == 684 and len(fractions) == 684
        results.append(dict(zip(('city_id', 'region', 'group', 'method'), key),
            threshold={'t10': 'T10', 'max_tss': 'MAX_TSS'}[key[3]],
            frozen_species_n=684, contributed_species_n=len(rows), finite_species_n=len(fractions),
            n_species=len(fractions), coverage=len(fractions) / 684,
            mean_species_suitable_fraction=sum(fractions) / 684 if complete else '',
            sum_species_suitable_fraction=sum(fractions) if complete else '',
            sum_species_suitable_area_km2=sum(float(r['suitable_area_km2']) for r in rows) if complete else '',
            suitable_species_area_km2=sum(float(r['suitable_area_km2']) for r in rows) if complete else '',
            sum_common_area_species_km2=sum(float(r['common_finite_area_km2']) for r in rows) if complete else '',
            status='COMPLETE_FIXED_684' if complete else 'INCOMPLETE_UNKNOWN_RESPONSE'))
    with output.open('w', newline='', encoding='utf-8') as stream:
        writer = csv.DictWriter(stream, fieldnames=list(results[0]))
        writer.writeheader(); writer.writerows(results)
    return dict(regions=len(results), complete=sum(r['status'] == 'COMPLETE_FIXED_684' for r in results),
                limitation='Sum of species suitable areas is not unique habitat area; no scientific acceptance implied.')


if __name__ == '__main__':
    parser = argparse.ArgumentParser(); parser.add_argument('root'); parser.add_argument('output')
    args = parser.parse_args(); print(json.dumps(summarize(args.root, args.output)))
