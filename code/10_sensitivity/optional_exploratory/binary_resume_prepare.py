"""Prepare non-overlapping missing-product manifests after writer quiescence."""
import argparse
import datetime
import hashlib
import json
import math
from pathlib import Path
from binary_batch_completion import discover_batches, read_rows
from binary_prepare import read


def check_quiescence(path):
    with open(path, encoding='utf-8-sig') as stream:
        receipt = json.load(stream)
    if receipt.get('observed_active_writer_jobs') != 0:
        raise ValueError('Writer quiescence has not been established')
    jobs = receipt.get('writer_jobs', [])
    terminal = {'COMPLETED', 'CANCELLED', 'FAILED', 'TIMEOUT', 'NODE_FAIL', 'OUT_OF_MEMORY', 'PREEMPTED', 'BOOT_FAIL'}
    if not jobs or any(not str(j.get('state', '')).split() or str(j.get('state', '')).split()[0] not in terminal for j in jobs):
        raise ValueError('Every recorded writer job must be terminal')
    observed = datetime.datetime.fromisoformat(receipt['observed_utc'].replace('Z', '+00:00'))
    now = datetime.datetime.now(datetime.timezone.utc)
    age = (now - observed).total_seconds()
    if age < -30 or age > 600:
        raise ValueError('Quiescence receipt must be current within ten minutes')
    return receipt


def census(original_manifest, threshold_root):
    expected = read(original_manifest)
    if not expected:
        raise ValueError('Original manifest is empty')
    keys = [(r['species'], r['group']) for r in expected]
    if len(set(keys)) != len(keys):
        raise ValueError('Original manifest has duplicate products')
    target = dict(zip(keys, expected))
    found, skipped, exclusions = {}, [], []
    for batch in discover_batches(threshold_root):
        if batch['status'] != 'COMPLETED_BATCH':
            skipped.append({k: batch[k] for k in ('directory', 'status', 'reason')})
            continue
        for row in batch['rows']:
            key = (row['species'], row['group'])
            if key not in target:
                raise ValueError('Unexpected completed product')
            if key in found:
                raise ValueError('Duplicate completed product')
            source = target[key]
            for field in ('projection_path', 'formatted_input_path'):
                if row[field] != source[field]:
                    raise ValueError('Completed product metadata differs: ' + field)
            actual_scale, expected_scale = float(row['scale_divisor']), float(source['scale_divisor'])
            if not math.isfinite(actual_scale) or actual_scale != expected_scale:
                raise ValueError('Completed product scale differs')
            found[key] = dict(batch_directory=batch['directory'], row=row)
        exclusions.extend(batch['exclusions'])
    missing = [r for r, key in zip(expected, keys) if key not in found]
    diagnostic = dict(requested=len(expected), completed=len(found), missing=len(missing),
        skipped_incomplete_batches=skipped, excluded_records=len(exclusions),
        original_manifest_sha256=hashlib.sha256(Path(original_manifest).read_bytes()).hexdigest(),
        completed_product_keys=[list(k) for k in sorted(found)],
        scope='COMPLETED_MARKER_AND_METADATA_ONLY; independent numerical threshold QC remains required')
    return missing, diagnostic


def prepare(original_manifest, threshold_root, output_manifest, receipt_path, quiescence_receipt=None, diagnostic=False):
    if not diagnostic:
        if not quiescence_receipt:
            raise ValueError('A current terminal-writer quiescence receipt is required')
        quiet = check_quiescence(quiescence_receipt)
    else:
        quiet = None
    missing, result = census(original_manifest, threshold_root)
    result['mode'] = 'DIAGNOSTIC_NONFROZEN' if diagnostic else 'FROZEN_MISSING_PRODUCTS'
    if diagnostic:
        return result
    output_manifest, receipt_path = Path(output_manifest), Path(receipt_path)
    if output_manifest.exists() or receipt_path.exists():
        raise ValueError('Fresh manifest and receipt paths required')
    import csv
    fields = list(read_rows(original_manifest)[0])
    output_manifest.parent.mkdir(parents=True, exist_ok=True)
    with output_manifest.open('w', newline='', encoding='utf-8') as stream:
        writer = csv.DictWriter(stream, fieldnames=fields);writer.writeheader();writer.writerows(missing)
    result['missing_manifest_sha256'] = hashlib.sha256(output_manifest.read_bytes()).hexdigest()
    result['quiescence_receipt_sha256'] = hashlib.sha256(Path(quiescence_receipt).read_bytes()).hexdigest()
    result['writer_jobs'] = quiet['writer_jobs']
    receipt_path.parent.mkdir(parents=True, exist_ok=True)
    receipt_path.write_text(json.dumps(result, indent=2), encoding='utf-8')
    return result


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    for name in ('original_manifest', 'threshold_root', 'output_manifest', 'receipt'):
        parser.add_argument(name)
    parser.add_argument('--quiescence-receipt');parser.add_argument('--diagnostic', action='store_true')
    args = parser.parse_args()
    result = prepare(args.original_manifest, args.threshold_root, args.output_manifest, args.receipt,
                     args.quiescence_receipt, args.diagnostic)
    print(json.dumps({k: v for k, v in result.items() if k != 'completed_product_keys'}, indent=2))
