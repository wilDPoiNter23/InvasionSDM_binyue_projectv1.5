"""Check completion boundaries, metadata identity and non-overlapping resume."""
import csv
import datetime
import json
import tempfile
import unittest
from pathlib import Path
from binary_resume_prepare import census, prepare
from binary_collect import harvest
from binary_verify_thresholds import verify


class ResumeTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory();self.root = Path(self.temp.name)
        self.manifest = self.root / 'original.csv';self.batches = self.root / 'batches'
        self.rows = [dict(species='Species_' + str(i), group='FULL', projection_path='map_' + str(i),
            formatted_input_path='formatted_' + str(i), scale_divisor=1000,
            scientific_status='SYNTHETIC') for i in range(3)]
        self.write(self.manifest, self.rows)
        self.complete = self.batches / 'batch_0';self.complete.mkdir(parents=True)
        (self.complete / 'scores.csv').write_text('label,score_raw\n1,800\n0,100\n')
        self.row = dict(self.rows[0], point_score_table='scores.csv')
        self.write(self.complete / 'thresholds.csv', [self.row])
        (self.complete / 'availability.txt').write_text('requested 1\ncomputed 1\nexcluded 0\n')
        partial = self.batches / 'batch_1';partial.mkdir()
        self.write(partial / 'thresholds.csv', [dict(self.rows[1], point_score_table='scores.csv')])

    def tearDown(self):
        self.temp.cleanup()

    def write(self, path, rows):
        with path.open('w', newline='') as stream:
            writer = csv.DictWriter(stream, fieldnames=list(rows[0]));writer.writeheader();writer.writerows(rows)

    def test_partial_batch_cannot_erase_missing_product(self):
        missing, receipt = census(self.manifest, self.batches)
        self.assertEqual([r['species'] for r in missing], ['Species_1', 'Species_2'])
        self.assertEqual(receipt['completed'], 1)
        self.assertEqual(len(receipt['skipped_incomplete_batches']), 1)

    def test_duplicate_completed_product_is_rejected(self):
        other = self.batches / 'duplicate';other.mkdir()
        (other / 'scores.csv').write_text('label,score_raw\n1,800\n0,100\n')
        self.write(other / 'thresholds.csv', [self.row])
        (other / 'availability.txt').write_text('requested 1\ncomputed 1\nexcluded 0\n')
        with self.assertRaises(ValueError):
            census(self.manifest, self.batches)

    def test_changed_projection_metadata_is_rejected(self):
        wrong = dict(self.row, projection_path='different_product')
        self.write(self.complete / 'thresholds.csv', [wrong])
        with self.assertRaises(ValueError):
            census(self.manifest, self.batches)

    def test_malformed_last_marker_is_incomplete(self):
        (self.complete / 'availability.txt').write_text('requested 1\ncomputed 1\n')
        missing, receipt = census(self.manifest, self.batches)
        self.assertEqual(len(missing), 3)
        self.assertEqual(receipt['completed'], 0)

    def test_final_plan_needs_quiescence_and_hashes_disjoint_roster(self):
        output = self.root / 'missing.csv';receipt_path = self.root / 'receipt.json'
        with self.assertRaises(ValueError):
            prepare(self.manifest, self.batches, output, receipt_path)
        quiet = self.root / 'quiet.json'
        quiet.write_text(json.dumps(dict(observed_active_writer_jobs=0,
            observed_utc=datetime.datetime.now(datetime.timezone.utc).isoformat(),
            writer_jobs=[dict(job_id='example', state='CANCELLED')])))
        receipt = prepare(self.manifest, self.batches, output, receipt_path, quiet)
        self.assertEqual(receipt['missing'], 2)
        self.assertEqual(receipt['mode'], 'FROZEN_MISSING_PRODUCTS')
        self.assertEqual(len(receipt['missing_manifest_sha256']), 64)
        with output.open() as stream:
            self.assertEqual({r['species'] for r in csv.DictReader(stream)}, {'Species_1', 'Species_2'})

    def test_collector_ignores_unmarked_threshold_table(self):
        computed, failures, inputs = harvest(self.batches)
        self.assertEqual(set(computed), {'Species_0'})
        self.assertEqual(len(inputs), 1)
        self.assertEqual(failures, {})

    def test_numeric_verifier_ignores_unmarked_threshold_table(self):
        complete = dict(self.row,t10_raw=800,max_tss_raw=800,max_tss=1,
            n_finite_presence=1,n_finite_background=1,
            validation_scope='CURRENT_MAP_CALIBRATION_POOLED_PA',boundary='score >= cutoff')
        self.write(self.complete / 'thresholds.csv', [complete])
        result = verify(self.batches)
        self.assertEqual(result['status'], 'PASS')
        self.assertEqual(result['products'], 1)
        self.assertEqual(len(result['skipped_incomplete_batches']), 1)


if __name__ == '__main__':
    unittest.main()
