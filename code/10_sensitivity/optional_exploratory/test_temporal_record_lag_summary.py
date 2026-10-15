"""Synthetic checks for interval bins, city weights and frozen roster guards."""
import csv
import importlib.util
import json
import tempfile
import unittest
from pathlib import Path

spec = importlib.util.spec_from_file_location("record_lag", Path(__file__).with_name("temporal_record_lag_summary.py"))
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)


class RecordLagTests(unittest.TestCase):
    def test_interval_boundaries(self):
        result = module.bin_bounds(5, 10)
        self.assertEqual(result["0_to_5_possible"], 1)
        self.assertEqual(result["over_5_to_10_possible"], 1)
        self.assertEqual(result["over_5_to_10_certain"], 0)
        self.assertEqual(result["over_10_possible"], 0)
        self.assertEqual(module.bin_bounds(11, 16)["over_10_certain"], 1)
        self.assertEqual(module.classify(None, 2000, 2024), "NO_VALID_YEAR")
        self.assertEqual(module.classify(2025, 2000, 2024), "AFTER_OBSERVATION_HORIZON")

    def test_complete_synthetic_contract(self):
        with tempfile.TemporaryDirectory() as name:
            root = Path(name)
            def save(filename, rows):
                module.write_csv(root / filename, rows)
            manifest = [dict(species="Taxon " + str(i), asset_role="PAIRED_FULL", used_in_paired_sensitivity="TRUE")
                        for i in range(684)]
            save("manifest.csv", manifest)
            save("target_roster_used.csv", [dict(species=r["species"]) for r in manifest])
            save("target_year_qc.csv", [dict(species=r["species"]) for r in manifest])
            (root / "extraction.json").write_text(json.dumps(dict(target_count=684, roster_sha256=module.digest(root / "manifest.csv"))))
            (root / "temporal_screen.json").write_text("{}")
            specifications = [("c1", 2000, 2002), ("c1", 2000, 2020), ("c1", 2010, None),
                              ("c2", 2000, 1990), ("c2", 2000, 2012), ("c3", 2020, 2023)]
            pairs = []
            for i, (city, endpoint, year) in enumerate(specifications):
                temporal = ("NO_VALID_YEAR" if year is None else "PRE_COHORT" if year < endpoint - 5 else
                            "WITHIN_COHORT_INTERVAL" if year <= endpoint else "AFTER_COHORT_ENDPOINT")
                pairs.append(dict(polygon_id=city + str(endpoint), kind=module.COASTAL, city_id=city,
                    species=manifest[i]["species"], endpoint=endpoint, start_year=endpoint - 5,
                    first_record_year=year, lag_lower_years=None if year is None else year - endpoint,
                    lag_upper_years=None if year is None else year - endpoint + 5, temporal_class=temporal,
                    valid_dated_records=int(year is not None), ambiguous_coastal_membership_records=int(i == 1),
                    native_status="UNRESOLVED"))
            save("species_first_records.csv", pairs)
            polygons = []
            for polygon in sorted({r["polygon_id"] for r in pairs}):
                group = [r for r in pairs if r["polygon_id"] == polygon]
                years = [r["first_record_year"] for r in group if r["first_record_year"] is not None]
                polygons.append(dict(polygon_id=polygon, city_id=group[0]["city_id"], kind=module.COASTAL,
                    endpoint=group[0]["endpoint"], start_year=group[0]["start_year"],
                    first_any_target_year=min(years) if years else None, target_species_observed=len(years),
                    valid_target_records=len(years), record_state="SYNTHETIC", native_status="UNRESOLVED"))
            save("polygon_record_summary.csv", polygons)
            cfg = dict(input_root=str(root), target_manifest=str(root / "manifest.csv"),
                       observation_horizon=2024, output_root=str(root / "output"))
            receipt = module.run(cfg)
            self.assertEqual(receipt["source_coastal_pairs"], 6)
            rows = module.read_csv(root / "output" / "record_lag_summary.csv")
            result = next(r for r in rows if r["unit"] == "species_cohort_pair" and
                          r["scope"] == "all_frozen_pairs" and r["cohort_endpoint"] == "ALL")
            self.assertAlmostEqual(float(result["city_balanced_mean_lag_lower_years"]), 26 / 3)
            self.assertEqual(float(result["city_balanced_median_lag_lower_years"]), 3)
            self.assertEqual(int(result["count_NO_VALID_YEAR"]), 1)
            first_any = next(r for r in rows if r["unit"] == "cohort_first_any_target" and
                             r["scope"] == "all_frozen_pairs" and r["cohort_endpoint"] == "ALL")
            conditional = next(r for r in rows if r["unit"] == "cohort_earliest_after_endpoint_species_first_conditional"
                               and r["scope"] == "all_frozen_pairs" and r["cohort_endpoint"] == "ALL")
            self.assertEqual(int(first_any["count_BEFORE_INTERVAL"]), 1)
            self.assertEqual(int(first_any["n_after_units"]), 2)
            self.assertEqual(int(conditional["n_after_units"]), 3)
            opportunities = module.read_csv(root / "output" / "fixed_followup_opportunity_summary.csv")
            self.assertFalse(any(r["cohort_endpoint"] == "2020" for r in opportunities))
            with self.assertRaises(ValueError):
                module.run(cfg)
            manifest[0]["used_in_paired_sensitivity"] = "FALSE"
            save("manifest.csv", manifest)
            with self.assertRaises(ValueError):
                module.validate_inputs(cfg)
            (root / "extraction.json").write_text(json.dumps(dict(target_count=684, roster_sha256=module.digest(root / "manifest.csv"))))
            with self.assertRaisesRegex(ValueError, "non-paired"):
                module.validate_inputs(cfg)


if __name__ == "__main__":
    unittest.main()
