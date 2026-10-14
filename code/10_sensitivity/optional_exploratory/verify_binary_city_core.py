"""Independent coverage-intersection and deterministic response verification."""
import argparse
import csv
import hashlib
import json
import math
from collections import defaultdict
from pathlib import Path


def records(path):
    with Path(path).open(newline='', encoding='utf-8-sig') as stream:
        yield from csv.DictReader(stream)


def sha(path):
    h = hashlib.sha256()
    with Path(path).open('rb') as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b''):
            h.update(chunk)
    return h.hexdigest()


def numeric(value):
    return math.nan if value in ('', 'NA', None) else float(value)


def close(actual, expected):
    return math.isclose(float(actual), float(expected), rel_tol=1e-10, abs_tol=1e-8)


def verify(root, paired_manifest, core_directory, sample_cities=12, expected_source_tables=None):
    if sample_cities<1:raise ValueError('At least one deterministic city required')
    directory = Path(core_directory)
    receipt = json.loads((directory / 'core_receipt.json').read_text(encoding='utf-8-sig'))
    paired = [r['species'] for r in records(paired_manifest)]
    if len(paired) != len(set(paired)) or len(paired) != receipt['source_species_n']:
        raise ValueError('Source species roster does not reconcile')
    if sha(paired_manifest) != receipt['paired_manifest_sha256']:
        raise ValueError('Source roster checksum differs')
    source_tables = sorted(Path(root).rglob('city_species_threshold_area.csv'))
    recorded = {str(Path(r['path']).resolve()): r['sha256'] for r in receipt['source_tables']}
    if len(recorded) != len(receipt['source_tables']) or {str(p.resolve()) for p in source_tables} != set(recorded):
        raise ValueError('Source table inventory differs')
    if expected_source_tables is not None and len(source_tables) != expected_source_tables:
        raise ValueError('Source table count differs')
    if any(sha(p) != recorded[str(p.resolve())] for p in source_tables):
        raise ValueError('Source table checksum differs')
    combinations = [('FULL','t10'),('FULL','max_tss'),('ENV','t10'),('ENV','max_tss')]
    bits = {v: 1 << i for i,v in enumerate(combinations)}
    coverage = {};cities = set();regions = set();wanted = set(paired)
    # Reconstruct coverage only. No suitable outcome contributes to eligibility.
    for path in source_tables:
        for row in records(path):
            key = (row['city_id'],row['region'],row['species']);combination = (row['group'],row['method'])
            if row['species'] not in wanted or combination not in bits or row['region'] not in ('CITY','MAPPED_RECLAMATION'):
                raise ValueError('Unexpected coverage scope')
            area = numeric(row['common_finite_area_km2']);cells = numeric(row['common_finite_cells'])
            if math.isinf(area) or math.isinf(cells) or (math.isfinite(area) and area < 0) or (math.isfinite(cells) and cells < 0):
                raise ValueError('Invalid common coverage')
            value = coverage.setdefault(key,dict(area=area,cells=cells,mask=0))
            if value['mask'] & bits[combination]:
                raise ValueError('Duplicate source response')
            same_area = (math.isnan(area) and math.isnan(value['area'])) or close(area,value['area'])
            same_cells = (math.isnan(cells) and math.isnan(value['cells'])) or cells == value['cells']
            if not same_area or not same_cells:
                raise ValueError('Common denominators differ across groups or thresholds')
            value['mask'] |= bits[combination]
            regions.add((row['city_id'],row['region']))
            if row['region']=='CITY':cities.add(row['city_id'])
    if len(cities) != receipt['required_city_n'] or {r['city_id'] for r in records(directory/'city_roster.csv')} != cities:
        raise ValueError('Fixed city roster differs')
    city_eligible = {}
    for city in cities:
        city_eligible[city] = {s for s in paired if (city,'CITY',s) in coverage and
            coverage[(city,'CITY',s)]['mask']==15 and math.isfinite(coverage[(city,'CITY',s)]['area']) and
            coverage[(city,'CITY',s)]['area']>0 and math.isfinite(coverage[(city,'CITY',s)]['cells']) and coverage[(city,'CITY',s)]['cells']>0}
    independently_eligible = set.intersection(*(city_eligible[c] for c in cities))
    frozen = [r['species'] for r in records(directory/'core_species_roster.csv')]
    k = len(frozen)
    if len(set(frozen)) != k or set(frozen) != independently_eligible or k != receipt['core_species_n']:
        raise ValueError('Frozen core differs from independently reconstructed city intersection')
    roster_sha = sha(directory/'core_species_roster.csv')
    if roster_sha != receipt['core_roster_sha256'] or receipt['excluded_species_n'] != len(paired)-k:
        raise ValueError('Core roster hash or exclusion count differs')
    exclusions = list(records(directory/'source_species_coverage_exclusions.csv'))
    if {r['species'] for r in exclusions} != wanted-set(frozen) or len(exclusions)!=len(paired)-k:
        raise ValueError('Exclusion roster differs')
    for row in exclusions:
        valid = {c for c in cities if row['species'] in city_eligible[c]}
        if int(row['positive_common_city_n'])!=len(valid) or set(row['excluded_cities'].split('|'))!=cities-valid:
            raise ValueError('Excluded coverage city counts differ')
    core = set(frozen)
    absent_reclamation = sorted(c for c in cities if (c,'MAPPED_RECLAMATION') not in regions)
    incomplete_reclamation = sorted(c for c in cities if (c,'MAPPED_RECLAMATION') in regions and
        any((c,'MAPPED_RECLAMATION',s) not in coverage or coverage[(c,'MAPPED_RECLAMATION',s)]['mask']!=15 or
            not math.isfinite(coverage[(c,'MAPPED_RECLAMATION',s)]['area']) or coverage[(c,'MAPPED_RECLAMATION',s)]['area']<=0 for s in core))
    all_cities = sorted(cities);n = min(sample_cities,len(all_cities));selected = []
    for candidate in incomplete_reclamation[:1]+absent_reclamation[:1]+[all_cities[round(i*(len(all_cities)-1)/max(1,n-1))] for i in range(n)]+all_cities:
        if candidate not in selected:selected.append(candidate)
        if len(selected)==n:break
    selected_set = set(selected);aggregate = defaultdict(lambda:dict(species=set(),finite=set(),fractions=[],suitable=[],common=[]))
    for path in source_tables:
        for row in records(path):
            if row['city_id'] not in selected_set or row['species'] not in core:continue
            key = (row['city_id'],row['region'],row['group'],row['method']);a = aggregate[key]
            a['species'].add(row['species']);common = numeric(row['common_finite_area_km2'])
            suitable = numeric(row['suitable_area_km2']);fraction = numeric(row['suitable_fraction'])
            if math.isfinite(common) and common>0 and math.isfinite(suitable) and math.isfinite(fraction):
                if not 0<=fraction<=1 or suitable<0 or not close(suitable/common,fraction):
                    raise ValueError('Source outcomes do not reconcile with common area')
                a['finite'].add(row['species']);a['fractions'].append(fraction);a['suitable'].append(suitable);a['common'].append(common)
    outputs = list(records(directory/'core_city_responses.csv'));output_index = {}
    for row in outputs:
        key = (row['city_id'],row['region'],row['group'],row['method'])
        if key in output_index:raise ValueError('Duplicate core output')
        if int(row['frozen_species_n'])!=k or row['core_roster_sha256']!=roster_sha:
            raise ValueError('Output roster binding differs')
        if row['region']=='CITY' and (row['status']!='COMPLETE_FIXED_'+str(k) or
            int(row['contributed_species_n'])!=k or int(row['n_species'])!=k or
            int(row['finite_species_n'])!=k or not close(row['coverage'],1)):
            raise ValueError('Fixed-core CITY completeness differs')
        output_index[key]=row
    if len(outputs)!=receipt['response_rows'] or set(output_index)!={(c,r,g,m) for c,r in regions for g,m in combinations}:
        raise ValueError('Output scope does not reconcile')
    checked = 0;max_error = 0.0;unknown_checked = 0
    for city in selected:
        for c,region in sorted(regions):
            if c!=city:continue
            for group,method in combinations:
                key = (city,region,group,method);a = aggregate[key];row=output_index[key]
                complete = a['species']==core and a['finite']==core
                if int(row['contributed_species_n'])!=len(a['species']) or int(row['n_species'])!=len(a['finite']) or int(row['finite_species_n'])!=len(a['finite']) or not close(row['coverage'],len(a['finite'])/k):
                    raise ValueError('Response contributing-species counts differ')
                expected_status = 'COMPLETE_FIXED_'+str(k) if complete else 'INCOMPLETE_UNKNOWN_RESPONSE'
                if row['status']!=expected_status:raise ValueError('Response completeness status differs')
                expected = dict(mean_species_suitable_fraction=math.fsum(a['fractions'])/k,
                    sum_species_suitable_fraction=math.fsum(a['fractions']),
                    sum_species_suitable_area_km2=math.fsum(a['suitable']),suitable_species_area_km2=math.fsum(a['suitable']),
                    sum_common_area_species_km2=math.fsum(a['common']))
                for field,value in expected.items():
                    if complete:
                        if not close(row[field],value):raise ValueError('Independent response sum differs: '+field)
                        max_error=max(max_error,abs(float(row[field])-value))
                    elif row[field] not in ('','NA'):
                        raise ValueError('Incomplete response was assigned a numerical outcome')
                if not complete:unknown_checked+=1
                checked+=1
    return dict(status='PASS',source_tables_checked=len(source_tables),source_species_n=len(paired),
        required_city_n=len(cities),core_species_n=k,independent_eligible_species_n=len(independently_eligible),
        core_roster_sha256=roster_sha,selected_cities=selected,selected_city_n=len(selected),
        incomplete_reclamation_city_included=any(c in selected_set for c in incomplete_reclamation),
        absent_reclamation_city_included=any(c in selected_set for c in absent_reclamation),
        response_rows_recomputed=checked,unknown_response_rows_checked=unknown_checked,maximum_absolute_sum_difference=max_error,
        original_core_receipt_sha256=sha(directory/'core_receipt.json'),
        interpretation='Independent coverage eligibility and aggregate binding verification; not scientific acceptance or causal validation.')


def self_test():
    import tempfile
    with tempfile.TemporaryDirectory() as temporary:
        root=Path(temporary);tables=root/'source';tables.mkdir();core=root/'core';core.mkdir()
        def put(path,data):
            with path.open('w',newline='') as stream:
                writer=csv.DictWriter(stream,fieldnames=list(data[0]));writer.writeheader();writer.writerows(data)
        paired=root/'paired.csv';put(paired,[dict(species=s) for s in ('A','B')])
        put(core/'core_species_roster.csv',[dict(species='A')]);put(core/'city_roster.csv',[dict(city_id=c) for c in ('one','two','three')])
        put(core/'source_species_coverage_exclusions.csv',[dict(species='B',positive_common_city_n=2,required_city_n=3,excluded_city_n=1,excluded_cities='one',reason='COVERAGE')])
        source=[];output=[];rh=sha(core/'core_species_roster.csv')
        combinations=[('FULL','t10'),('FULL','max_tss'),('ENV','t10'),('ENV','max_tss')]
        for city,region in [('one','CITY'),('two','CITY'),('three','CITY'),('one','MAPPED_RECLAMATION')]:
            for group,method in combinations:
                for species in ('A','B'):
                    area=0 if region=='MAPPED_RECLAMATION' or (city=='one' and species=='B') else 4
                    source.append(dict(species=species,city_id=city,region=region,group=group,method=method,
                        common_finite_area_km2=area,common_finite_cells=4 if area else 0,suitable_area_km2=area/2,suitable_fraction=.5 if area else 'NA'))
                complete=region=='CITY'
                output.append(dict(city_id=city,region=region,group=group,method=method,threshold='T10' if method=='t10' else 'MAX_TSS',
                    frozen_species_n=1,contributed_species_n=1,finite_species_n=1 if complete else 0,n_species=1 if complete else 0,coverage=1 if complete else 0,
                    mean_species_suitable_fraction=.5 if complete else '',sum_species_suitable_fraction=.5 if complete else '',
                    sum_species_suitable_area_km2=2 if complete else '',suitable_species_area_km2=2 if complete else '',sum_common_area_species_km2=4 if complete else '',
                    status='COMPLETE_FIXED_1' if complete else 'INCOMPLETE_UNKNOWN_RESPONSE',core_roster_sha256=rh))
        table=tables/'city_species_threshold_area.csv';put(table,source);put(core/'core_city_responses.csv',output)
        rc=dict(source_species_n=2,core_species_n=1,excluded_species_n=1,required_city_n=3,core_roster_sha256=rh,
            paired_manifest_sha256=sha(paired),source_tables=[dict(path=str(table),sha256=sha(table))],response_rows=len(output))
        (core/'core_receipt.json').write_text(json.dumps(rc))
        result=verify(tables,paired,core);assert result['status']=='PASS' and result['unknown_response_rows_checked']==4
        output[0]['suitable_species_area_km2']=3;put(core/'core_city_responses.csv',output)
        try:verify(tables,paired,core)
        except ValueError:pass
        else:raise AssertionError('Altered response was accepted')
        output[0]['suitable_species_area_km2']=2;output[-1]['mean_species_suitable_fraction']=0;put(core/'core_city_responses.csv',output)
        try:verify(tables,paired,core)
        except ValueError:pass
        else:raise AssertionError('Missing reclamation was accepted as zero')
        return dict(status='PASS',fixtures_checked=3)


if __name__=='__main__':
    parser=argparse.ArgumentParser()
    for name in ('root','paired_manifest','core_directory','receipt'):parser.add_argument(name,nargs='?')
    parser.add_argument('--sample-cities',type=int,default=12);parser.add_argument('--expected-source-tables',type=int)
    parser.add_argument('--self-test',action='store_true');args=parser.parse_args()
    if args.self_test:print(json.dumps(self_test()))
    else:
        if not all((args.root,args.paired_manifest,args.core_directory,args.receipt)):parser.error('Four paths required')
        if Path(args.receipt).exists():raise ValueError('Fresh verification receipt required')
        answer=verify(args.root,args.paired_manifest,args.core_directory,args.sample_cities,args.expected_source_tables)
        Path(args.receipt).write_text(json.dumps(answer,indent=2),encoding='utf-8');print(json.dumps(answer,indent=2))
