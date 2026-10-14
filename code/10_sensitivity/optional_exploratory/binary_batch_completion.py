"""Recognize completed calibration batches from their last-written marker."""
import csv
from pathlib import Path


def read_rows(path):
    with Path(path).open(newline='', encoding='utf-8-sig') as stream:
        return list(csv.DictReader(stream))


def inspect_batch(directory):
    directory = Path(directory)
    marker = directory / 'availability.txt'
    answer = dict(directory=str(directory), status='SKIPPED_INCOMPLETE_BATCH', reason='MARKER_MISSING')
    if not marker.is_file():
        return answer
    try:
        counts = {}
        for line in marker.read_text(encoding='utf-8-sig').splitlines():
            key, value = line.split()
            if key not in ('requested', 'computed', 'excluded') or key in counts:
                raise ValueError('Unexpected or repeated marker field')
            counts[key] = int(value)
        if set(counts) != {'requested', 'computed', 'excluded'}:
            raise ValueError('Incomplete marker fields')
        if any(value < 0 for value in counts.values()) or counts['requested'] != counts['computed'] + counts['excluded']:
            raise ValueError('Marker counts do not reconcile')
        rows = read_rows(directory / 'thresholds.csv') if (directory / 'thresholds.csv').is_file() else []
        exclusions = read_rows(directory / 'excluded.csv') if (directory / 'excluded.csv').is_file() else []
        if len(rows) != counts['computed'] or len(exclusions) != counts['excluded']:
            raise ValueError('Actual row counts differ from marker')
        for row in rows:
            if not {'species', 'group', 'projection_path', 'formatted_input_path', 'scale_divisor', 'point_score_table'} <= set(row):
                raise ValueError('Threshold row schema incomplete')
            if not (directory / row['point_score_table']).is_file():
                raise ValueError('Point score backing table missing')
        names = [(r['species'], r['group']) for r in rows + exclusions]
        if len(set(names)) != len(names):
            raise ValueError('Duplicate batch species/group')
        answer.update(status='COMPLETED_BATCH', reason='', counts=counts, rows=rows, exclusions=exclusions)
    except (ValueError, KeyError, TypeError, OSError, UnicodeError, csv.Error) as error:
        answer['reason'] = str(error)
    return answer


def discover_batches(root):
    root = Path(root)
    candidates = {p.parent for pattern in ('availability.txt', 'thresholds.csv', 'excluded.csv') for p in root.rglob(pattern)}
    candidates.update(p.parent for p in root.rglob('point_scores') if p.is_dir())
    return [inspect_batch(p) for p in sorted(candidates)]
