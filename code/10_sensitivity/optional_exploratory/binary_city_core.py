"""Freeze a coverage-only species core across every city before aggregation."""
import argparse
import csv
import hashlib
import json
import math
from collections import defaultdict
from pathlib import Path


def rows(path):
    with Path(path).open(newline='', encoding='utf-8-sig') as stream:
        yield from csv.DictReader(stream)


def number(value):
    return float(value) if value not in ('', 'NA', None) else math.nan


def write(path, records, fields=None):
    fields = fields or list(records[0])
    with Path(path).open('w', newline='', encoding='utf-8') as stream:
        writer = csv.DictWriter(stream, fieldnames=fields);writer.writeheader();writer.writerows(records)


def build(root, paired_manifest, output_directory, expected_cities=108, expected_species=684):
    output = Path(output_directory)
    if output.exists():
        raise ValueError('Fresh core output directory required')
    paired = list(rows(paired_manifest));roster = [r['species'] for r in paired]
    if len(roster) != expected_species or len(set(roster)) != len(roster):
        raise ValueError('Frozen source species roster differs')
    wanted = set(roster)
    tables = sorted(Path(root).rglob('city_species_threshold_area.csv'))
    if not tables:
        raise ValueError('No city extraction tables')
    identities = set();coverage = defaultdict(dict);city_ids = set();regions = set();sources = []
    combinations = {('FULL', 't10'), ('FULL', 'max_tss'), ('ENV', 't10'), ('ENV', 'max_tss')}
    # This pass uses coverage metadata only, never suitable areas or fractions.
    for table in tables:
        sources.append(dict(path=str(table), sha256=hashlib.sha256(table.read_bytes()).hexdigest()))
        for row in rows(table):
            species, city, region = row['species'], row['city_id'], row['region']
            group_method = (row['group'], row['method'])
            if species not in wanted or group_method not in combinations or region not in ('CITY', 'MAPPED_RECLAMATION'):
                raise ValueError('Unexpected species or response scope')
            identity = (species, city, region) + group_method
            if identity in identities:
                raise ValueError('Duplicate species-region-group-threshold row')
            identities.add(identity);regions.add((city, region))
            area = number(row['common_finite_area_km2']);cells = number(row['common_finite_cells'])
            if math.isinf(area) or math.isinf(cells) or (math.isfinite(area) and area < 0) or (math.isfinite(cells) and cells < 0):
                raise ValueError('Negative or infinite common support')
            if math.isfinite(area) and area > 0 and (not math.isfinite(cells) or cells <= 0):
                raise ValueError('Positive area requires positive finite cells')
            if region != 'CITY':
                continue
            city_ids.add(city)
            record = coverage[species].setdefault(city, dict(area=area, cells=cells, combinations=set()))
            same_area = (math.isnan(area) and math.isnan(record['area'])) or math.isclose(area, record['area'], rel_tol=1e-10, abs_tol=1e-9)
            same_cells = (math.isnan(cells) and math.isnan(record['cells'])) or cells == record['cells']
            if not same_area or not same_cells:
                raise ValueError('Paired common support differs across group or threshold')
            record['combinations'].add(group_method)
    if len(city_ids) != expected_cities:
        raise ValueError('City scope differs from required fixed city count')
    if any(city not in city_ids for city, _ in regions):
        raise ValueError('Reclamation region has an unknown city identifier')
    core, excluded = [], []
    for species in roster:
        metadata = coverage.get(species, {})
        positive = [c for c in city_ids if c in metadata and math.isfinite(metadata[c]['area']) and metadata[c]['area'] > 0 and metadata[c]['combinations'] == combinations]
        missing = sorted(city_ids - set(positive))
        if missing:
            excluded.append(dict(species=species, positive_common_city_n=len(positive), required_city_n=expected_cities,
                excluded_city_n=len(missing), excluded_cities='|'.join(missing), reason='NOT_POSITIVE_COMMON_SUPPORT_IN_EVERY_CITY'))
        else:
            core.append(species)
    if not core:
        raise ValueError('Coverage core is empty')
    core = sorted(core);core_set = set(core);k = len(core)
    output.mkdir(parents=True)
    write(output / 'core_species_roster.csv', [dict(species=s) for s in core])
    write(output / 'source_species_coverage_exclusions.csv', excluded,
          ['species', 'positive_common_city_n', 'required_city_n', 'excluded_city_n', 'excluded_cities', 'reason'])
    write(output / 'city_roster.csv', [dict(city_id=c) for c in sorted(city_ids)])
    roster_hash = hashlib.sha256((output / 'core_species_roster.csv').read_bytes()).hexdigest()
    # The core is already frozen. Only this second pass reads suitable outcomes.
    aggregate = defaultdict(lambda: dict(species=set(), finite=set(), fractions=0.0, suitable=0.0, common=0.0))
    for table in tables:
        for row in rows(table):
            if row['species'] not in core_set:
                continue
            key = (row['city_id'], row['region'], row['group'], row['method'])
            record = aggregate[key];record['species'].add(row['species'])
            common = number(row['common_finite_area_km2']);suitable = number(row['suitable_area_km2']);fraction = number(row['suitable_fraction'])
            if math.isfinite(suitable) and suitable < 0:
                raise ValueError('Negative suitable area')
            if math.isfinite(fraction) and not 0 <= fraction <= 1:
                raise ValueError('Suitable fraction outside [0,1]')
            if math.isfinite(common) and common > 0 and math.isfinite(suitable) and math.isfinite(fraction):
                if suitable > common + max(1e-8, 1e-10 * common) or not math.isclose(suitable / common, fraction, rel_tol=1e-9, abs_tol=1e-10):
                    raise ValueError('Suitable area and fraction do not reconcile')
                record['finite'].add(row['species']);record['fractions'] += fraction
                record['suitable'] += suitable;record['common'] += common
    result = []
    for city, region in sorted(regions):
        for group, method in sorted(combinations):
            record = aggregate[(city, region, group, method)]
            complete = record['species'] == core_set and record['finite'] == core_set
            if region == 'CITY' and not complete:
                raise ValueError('A frozen-core city response is incomplete')
            result.append(dict(city_id=city, region=region, group=group, method=method,
                threshold={'t10': 'T10', 'max_tss': 'MAX_TSS'}[method],frozen_species_n=k,
                contributed_species_n=len(record['species']),finite_species_n=len(record['finite']),
                n_species=len(record['finite']),coverage=len(record['finite']) / k,
                mean_species_suitable_fraction=record['fractions'] / k if complete else '',
                sum_species_suitable_fraction=record['fractions'] if complete else '',
                sum_species_suitable_area_km2=record['suitable'] if complete else '',
                suitable_species_area_km2=record['suitable'] if complete else '',
                sum_common_area_species_km2=record['common'] if complete else '',
                status='COMPLETE_FIXED_K' if complete else 'INCOMPLETE_UNKNOWN_RESPONSE',
                core_roster_sha256=roster_hash))
    write(output / 'core_city_responses.csv', result)
    receipt = dict(status='COVERAGE_CORE_FROZEN',source_species_n=expected_species,core_species_n=k,
        excluded_species_n=expected_species-k,required_city_n=expected_cities,
        eligibility_rule='Positive finite common FULL/ENV area in every CITY polygon, identical across both threshold methods; no suitable outcome used for selection',
        core_roster_sha256=roster_hash,paired_manifest_sha256=hashlib.sha256(Path(paired_manifest).read_bytes()).hexdigest(),
        source_tables=sources,response_rows=len(result),complete_city_rows=sum(r['region']=='CITY' and r['status']=='COMPLETE_FIXED_K' for r in result),
        complete_reclamation_rows=sum(r['region']=='MAPPED_RECLAMATION' and r['status']=='COMPLETE_FIXED_K' for r in result),
        limitation='Changes the species estimand to a coverage subset. Missing reclamation support remains unknown. Summed species area is not unique habitat area; no causal inference or acceptance is established.')
    (output / 'core_receipt.json').write_text(json.dumps(receipt,indent=2),encoding='utf-8')
    return receipt


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    for name in ('root', 'paired_manifest', 'output_directory'):
        parser.add_argument(name)
    parser.add_argument('--expected-cities', type=int, default=108)
    parser.add_argument('--expected-species', type=int, default=684)
    args = parser.parse_args()
    answer = build(args.root,args.paired_manifest,args.output_directory,args.expected_cities,args.expected_species)
    print(json.dumps({k:v for k,v in answer.items() if k!='source_tables'},indent=2))
