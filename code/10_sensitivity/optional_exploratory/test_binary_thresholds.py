"""Synthetic checks for calibration thresholds and inclusive boundaries."""
import unittest
import csv
import tempfile
from pathlib import Path
from binary_thresholds import quantile_type7, maximum_tss, binarize
from binary_verify_thresholds import verify


class ThresholdTests(unittest.TestCase):
    def test_quantile(self):
        self.assertEqual(quantile_type7([0, 1000]), 100)

    def test_maximum_tss(self):
        result = maximum_tss([0, 100, 800, 900], [0, 0, 1, 1])
        self.assertEqual(result['cutoff'], 800)
        self.assertEqual(result['tss'], 1)

    def test_ties_and_scale(self):
        result = maximum_tss([0, 500, 500, 1000], [0, 1, 0, 1])
        self.assertEqual(result['cutoff'], 500)
        self.assertEqual(result['tss'], 0.5)

    def test_zero_na_boundary(self):
        self.assertEqual(binarize([None, float('nan'), 0, 1, 1000], 0),
                         [None, None, 1, 1, 1])
        self.assertEqual(binarize([499, 500, 501], 500), [0, 1, 1])

    def test_invalid_labels(self):
        with self.assertRaises(ValueError):
            maximum_tss([1, 2], [1, None])

    def test_independent_receipt_detects_wrong_cutpoint(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            with (root / 'scores.csv').open('w', newline='') as stream:
                writer = csv.writer(stream)
                writer.writerow(['score_raw', 'label'])
                writer.writerows([(0, 0), (100, 0), (800, 1), (900, 1)])
            row = dict(species='Example species', group='FULL', point_score_table='scores.csv',
                       t10_raw=810, max_tss_raw=800, max_tss=1,
                       n_finite_presence=2, n_finite_background=2,
                       validation_scope='CURRENT_MAP_CALIBRATION_POOLED_PA', boundary='score >= cutoff')
            def write_table():
                with (root / 'thresholds.csv').open('w', newline='') as stream:
                    writer = csv.DictWriter(stream, fieldnames=list(row))
                    writer.writeheader(); writer.writerow(row)
            write_table()
            self.assertEqual(verify(root)['status'], 'PASS')
            row['max_tss_raw'] = 801
            write_table()
            self.assertEqual(verify(root)['status'], 'FAIL')


if __name__ == '__main__':
    unittest.main()
