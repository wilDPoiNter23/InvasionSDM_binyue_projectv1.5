"""Collect calibration batches and bind paired reuse by exact product identity."""
import argparse
import csv
import hashlib
import json
from pathlib import Path
from binary_batch_completion import discover_batches


def read(path):
    with open(path, newline='', encoding='utf-8-sig') as stream:
        return list(csv.DictReader(stream))


def write(path, rows, fields=None):
    if not rows and fields is None:
        return
    fields = fields or list(dict.fromkeys(k for row in rows for k in row))
    with open(path, 'w', newline='', encoding='utf-8') as stream:
        writer = csv.DictWriter(stream, fieldnames=fields)
        writer.writeheader(); writer.writerows(rows)


def harvest(root):
    computed, failures, inputs = {}, {}, []
    for batch in discover_batches(root):
        if batch['status'] != 'COMPLETED_BATCH':
            continue
        directory = Path(batch['directory'])
        table = directory / 'thresholds.csv'
        for row in batch['exclusions']:
            failures[row['species']] = row['reason']
        if not batch['rows']:
            continue
        inputs.append(dict(path=str(table), sha256=hashlib.sha256(table.read_bytes()).hexdigest()))
        for row in batch['rows']:
            species = row['species']
            if species in computed:
                raise ValueError('Duplicate threshold product: ' + species)
            row['point_score_table'] = str((table.parent / row['point_score_table']).resolve())
            if not Path(row['point_score_table']).is_file():
                raise ValueError('Point score backing table missing')
            computed[species] = row
    return computed, failures, inputs


def collect(config):
    out = Path(config['output_directory'])
    if out.exists():
        raise ValueError('Output directory exists')
    manifests = Path(config['prepared_manifest_directory'])
    expected = {key: read(manifests / (key + '.csv')) for key in
                ('main_full', 'paired_full', 'paired_env', 'paired_full_distinct', 'paired_full_reuse')}
    if len(expected['main_full']) != 814 or len(expected['paired_full']) != 684 or len(expected['paired_env']) != 684:
        raise ValueError('Required requested814 and paired684 counts differ')
    main, main_fail, source_main = harvest(config['main_threshold_root'])
    env, env_fail, source_env = harvest(config['env_threshold_root'])
    distinct, distinct_fail, source_distinct = harvest(config['distinct_threshold_root'])
    for name, found, wanted in (('main', main, expected['main_full']),
                               ('env', env, expected['paired_env']),
                               ('distinct', distinct, expected['paired_full_distinct'])):
        if set(found) - {r['species'] for r in wanted}:
            raise ValueError('Unexpected species in ' + name)
    missing, full_rows, env_rows = [], [], []
    def bind(wanted, found, failures, cohort):
        answer = []
        for target in wanted:
            species = target['species']
            if species not in found:
                missing.append(dict(species=species, group=target['group'], cohort=cohort,
                    reason=failures.get(species, 'NO_COMPLETED_THRESHOLD_RECORD')))
                continue
            row = dict(found[species])
            for field in ('projection_path', 'formatted_input_path', 'scale_divisor'):
                if str(row[field]) != str(target[field]):
                    raise ValueError('Product identity differs: ' + species + '/' + field)
            if row['validation_scope'] != 'CURRENT_MAP_CALIBRATION_POOLED_PA':
                raise ValueError('Unexpected validation scope')
            row['threshold_source_scientific_status'] = row['scientific_status']
            row['scientific_status'] = target['scientific_status']
            answer.append(row)
        return answer
    main_rows = bind(expected['main_full'], main, main_fail, 'MAIN_FULL_814')
    full_rows.extend(bind(expected['paired_full_reuse'], main, main_fail, 'PAIRED_FULL_REUSED'))
    full_rows.extend(bind(expected['paired_full_distinct'], distinct, distinct_fail, 'PAIRED_FULL_DISTINCT'))
    env_rows.extend(bind(expected['paired_env'], env, env_fail, 'PAIRED_ENV'))
    full_rows.sort(key=lambda r: r['species']); env_rows.sort(key=lambda r: r['species'])
    complete_pairs = set(r['species'] for r in full_rows) & set(r['species'] for r in env_rows)
    paired_rows = [r for r in full_rows + env_rows if r['species'] in complete_pairs]
    out.mkdir(parents=True)
    write(out / 'main_thresholds.csv', main_rows)
    write(out / 'paired_thresholds.csv', paired_rows)
    write(out / 'missing_threshold_assets.csv', missing,
          ['species', 'group', 'cohort', 'reason'])
    receipt = dict(requested_main_species=814, computed_main_species=len(main_rows),
        requested_main_binary_maps=1628, planned_computed_main_binary_maps=2 * len(main_rows),
        main_threshold_complete=len(main_rows) == 814, frozen_paired_species=684,
        computed_paired_full=len(full_rows), computed_paired_env=len(env_rows),
        common_complete_pairs=len(complete_pairs), paired_threshold_complete=len(complete_pairs) == 684,
        requested_unique_products=814 + len(expected['paired_full_distinct']) + 684,
        missing_records=len(missing), input_threshold_tables=source_main + source_env + source_distinct,
        skipped_incomplete_batches=[{k:b[k] for k in ('directory','status','reason')}
            for root in (config['main_threshold_root'],config['env_threshold_root'],config['distinct_threshold_root'])
            for b in discover_batches(root) if b['status']!='COMPLETED_BATCH'],
        scope='CURRENT_MAP_CALIBRATION_POOLED_PA',
        interpretation='Calibration sensitivity; accepted source does not establish independently held-out validation or scientific acceptance.')
    (out / 'collection_receipt.json').write_text(json.dumps(receipt, indent=2), encoding='utf-8')
    return receipt


if __name__ == '__main__':
    parser = argparse.ArgumentParser();parser.add_argument('config')
    args = parser.parse_args()
    with open(args.config, encoding='utf-8-sig') as stream:
        result = collect(json.load(stream))
    print(json.dumps({k: v for k, v in result.items() if k != 'input_threshold_tables'}, indent=2))
