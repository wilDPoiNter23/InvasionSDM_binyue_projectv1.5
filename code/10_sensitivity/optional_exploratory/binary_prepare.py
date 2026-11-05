"""Prepare separate calibration manifests without changing accepted rosters."""
import argparse
import csv
import hashlib
import json
from pathlib import Path


def read(path, delimiter=','):
    with open(path, newline='', encoding='utf-8-sig') as stream:
        return list(csv.DictReader(stream, delimiter=delimiter))


def prepare(asset_manifest, sensitivity_inventory, paired_manifest, output):
    output = Path(output)
    if output.exists():
        raise ValueError('Output directory exists')
    assets = read(asset_manifest)
    inventory = read(sensitivity_inventory, '\t')
    pairs = read(paired_manifest)
    main = {r['species']: r for r in assets if r['asset_role'] == 'MAIN_FULL_PRESERVATION_CANDIDATE'}
    rows = {'main_full': [], 'paired_full': [], 'paired_env': []}
    for r in inventory:
        s = r['accepted_taxon']
        source = main.get(s)
        projection = source['selected_continuous_projection'] if source else r['baseline_global_projection_path']
        rows['main_full'].append(dict(species=s, group='FULL', projection_path=projection,
            formatted_input_path=source['formatted_input_path'] if source else r['formatted_input_path'],
            model_path=source['model_object_path'] if source else r['baseline_model_path'],
            scale_divisor=1000, scientific_status='MAIN_LINEAGE_UNRESOLVED'))
    for pair in pairs:
        for group in ('full', 'env'):
            source = next(r for r in assets if r['species'] == pair['species'] and r['asset_role'] == 'PAIRED_' + group.upper())
            rows['paired_' + group].append(dict(species=pair['species'], group=group.upper(),
                projection_path=pair[group + '_projection'], formatted_input_path=source['formatted_input_path'],
                model_path=pair[group + '_model'], scale_divisor=pair[group + '_scale_divisor'],
                scientific_status='ACCEPTED_PAIRED_SOURCE_CALIBRATION_SENSITIVITY'))
    main_by_species = {r['species']: r for r in rows['main_full']}
    identity = ('projection_path', 'formatted_input_path', 'scale_divisor')
    rows['paired_full_distinct'] = [r for r in rows['paired_full']
        if any(str(r[k]) != str(main_by_species[r['species']][k]) for k in identity)]
    rows['paired_full_reuse'] = [r for r in rows['paired_full']
        if all(str(r[k]) == str(main_by_species[r['species']][k]) for k in identity)]
    for key, data in rows.items():
        if len({r['species'] for r in data}) != len(data):
            raise ValueError('Duplicate species in ' + key)
        if any('TSSbin' in r['projection_path'] or not r['projection_path'] for r in data):
            raise ValueError('Missing continuous authority in ' + key)
    output.mkdir(parents=True)
    for key, data in rows.items():
        with open(output / (key + '.csv'), 'w', newline='', encoding='utf-8') as stream:
            writer = csv.DictWriter(stream, fieldnames=list(rows['main_full'][0]))
            writer.writeheader(); writer.writerows(data)
    receipt = dict(counts={key: len(data) for key, data in rows.items()},
        source_hashes={str(p): hashlib.sha256(Path(p).read_bytes()).hexdigest()
                       for p in (asset_manifest, sensitivity_inventory, paired_manifest)},
        score_scope='CURRENT_MAP_CALIBRATION_POOLED_PA',
        main_scale_policy='DECLARED_1000_READER_RANGE_CHECK_REQUIRED',
        limitation='Calibration only; retained presences and pooled stored pseudoabsences overlap model training. Not independent OOF validation or an exact Zhang validation protocol.')
    (output / 'receipt.json').write_text(json.dumps(receipt, indent=2), encoding='utf-8')
    return receipt


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    for name in ('asset_manifest', 'sensitivity_inventory', 'paired_manifest', 'output'):
        parser.add_argument(name)
    args = parser.parse_args()
    print(json.dumps(prepare(args.asset_manifest, args.sensitivity_inventory, args.paired_manifest, args.output), indent=2))
