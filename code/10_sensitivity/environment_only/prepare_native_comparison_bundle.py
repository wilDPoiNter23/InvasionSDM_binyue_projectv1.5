"""Package saved city-cell inputs and exact deduplicated native geometries."""
import argparse
import csv
import hashlib
import json
from pathlib import Path
import sqlite3
import struct
import tarfile


def sha256(path):
    h = hashlib.sha256()
    with Path(path).open('rb') as handle:
        for block in iter(lambda: handle.read(8 * 1024 * 1024), b''):
            h.update(block)
    return h.hexdigest()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--comparison', required=True, type=Path)
    parser.add_argument('--paired-manifest', required=True, type=Path)
    parser.add_argument('--native-vector', action='append', required=True, type=Path)
    parser.add_argument('--output', required=True, type=Path)
    args = parser.parse_args()
    args.output.mkdir(parents=True, exist_ok=True)
    geometry_path = args.output / 'native_geometries.wkbpack'
    if geometry_path.exists():
        raise FileExistsError(geometry_path)
    unique = {}; mapping = []; sources = []
    with geometry_path.open('wb') as output:
        for path in args.native_vector:
            connection = sqlite3.connect(path.resolve().as_uri() + '?mode=ro', uri=True)
            table, column = connection.execute(
                'SELECT table_name, column_name FROM gpkg_geometry_columns').fetchone()
            query = f'SELECT accepted_taxon, mapping_source, "{column}" FROM "{table}"'
            count = 0
            for taxon, source, blob in connection.execute(query):
                if blob is None:
                    continue
                if blob[:2] != b'GP':
                    raise ValueError('Unexpected geometry format')
                envelope = (blob[3] >> 1) & 7
                envelope_sizes = {0: 0, 1: 4, 2: 6, 3: 6, 4: 8}
                wkb = blob[8 + 8 * envelope_sizes[envelope]:]
                digest = hashlib.sha256(wkb).hexdigest()
                if digest not in unique:
                    unique[digest] = len(unique) + 1
                    output.write(struct.pack('<I', len(wkb)))
                    output.write(wkb)
                mapping.append((taxon, source, unique[digest], str(path)))
                count += 1
            connection.close()
            sources.append(dict(path=str(path), sha256=sha256(path), features=count))
    mapping_path = args.output / 'native_geometry_membership.csv'
    with mapping_path.open('w', newline='', encoding='utf-8') as handle:
        writer = csv.writer(handle)
        writer.writerow(['accepted_taxon', 'mapping_source', 'geometry_id', 'source_vector'])
        writer.writerows(mapping)
    files = []
    for name in ('support.rds', 'species_inputs.csv', 'raster_input_metadata.csv'):
        files.append((args.comparison / name, 'cached_comparison/' + name))
    files += [(p, 'cached_comparison/input_cells/' + p.name)
              for p in sorted((args.comparison / 'input_cells').glob('*.rds'))]
    files += [(args.paired_manifest, 'paired_species_manifest.csv'),
              (geometry_path, geometry_path.name), (mapping_path, mapping_path.name)]
    record = dict(native_sources=sources, unique_geometries=len(unique),
                  features=len(mapping), geometry_rule='Exact WKB bytes; no simplification',
                  files=[dict(member=name, source=str(path), bytes=path.stat().st_size,
                              sha256=sha256(path)) for path, name in files])
    manifest = args.output / 'transfer_manifest.json'
    manifest.write_text(json.dumps(record, indent=2), encoding='utf-8')
    archive = args.output / 'native_comparison_inputs.tar.gz'
    with tarfile.open(archive, 'w:gz') as bundle:
        for path, name in files:
            bundle.add(path, arcname=name, recursive=False)
        bundle.add(manifest, arcname=manifest.name)
    (args.output / 'archive.sha256').write_text(sha256(archive) + '\n', encoding='ascii')
    print(json.dumps(dict(archive=str(archive), bytes=archive.stat().st_size,
                          unique_geometries=len(unique), features=len(mapping))))


if __name__ == '__main__':
    main()
