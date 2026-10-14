"""Coverage-only eligibility and incomplete reclamation response checks."""
import csv
import json
import tempfile
import unittest
from pathlib import Path
from binary_city_core import build


class CoreTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory();self.root = Path(self.temp.name)
        self.tables = self.root / 'batches';self.tables.mkdir()
        self.paired = self.root / 'paired.csv'
        self.write(self.paired,[dict(species=s) for s in ('A','B','C')])
        self.data = []
        for species in ('A','B','C'):
            for city in ('one','two','three'):
                for group in ('FULL','ENV'):
                    for method in ('t10','max_tss'):
                        area = 0 if (species,city) in {('B','one'),('C','three')} else 4
                        self.data.append(dict(species=species,city_id=city,region='CITY',group=group,
                            method=method,common_finite_area_km2=area,common_finite_cells=4 if area else 0,
                            suitable_area_km2=area/4,suitable_fraction=.25 if area else 'NA'))
            for group in ('FULL','ENV'):
                for method in ('t10','max_tss'):
                    self.data.append(dict(species=species,city_id='one',region='MAPPED_RECLAMATION',group=group,
                        method=method,common_finite_area_km2=0,common_finite_cells=0,suitable_area_km2=0,suitable_fraction='NA'))
        self.table = self.tables / 'city_species_threshold_area.csv';self.write(self.table,self.data)

    def tearDown(self):
        self.temp.cleanup()

    def write(self,path,data):
        with path.open('w',newline='') as stream:
            writer=csv.DictWriter(stream,fieldnames=list(data[0]));writer.writeheader();writer.writerows(data)

    def run_core(self,name='output'):
        return build(self.tables,self.paired,self.root/name,expected_cities=3,expected_species=3)

    def test_fixed_core_and_unknown_reclamation(self):
        result=self.run_core()
        self.assertEqual(result['core_species_n'],1)
        self.assertEqual(result['complete_city_rows'],12)
        self.assertEqual(result['complete_reclamation_rows'],0)
        with (self.root/'output/core_city_responses.csv').open() as stream:
            data=list(csv.DictReader(stream))
        reclamation=[r for r in data if r['region']=='MAPPED_RECLAMATION']
        self.assertTrue(all(r['mean_species_suitable_fraction']=='' for r in reclamation))
        self.assertTrue(all(r['status']=='INCOMPLETE_UNKNOWN_RESPONSE' for r in reclamation))

    def test_negative_coverage_rejected(self):
        self.data[0]['common_finite_area_km2']=-1;self.write(self.table,self.data)
        with self.assertRaises(ValueError):self.run_core()

    def test_duplicate_row_rejected(self):
        self.write(self.table,self.data+[self.data[0]])
        with self.assertRaises(ValueError):self.run_core()

    def test_scope_loss_rejected(self):
        self.write(self.table,[r for r in self.data if r['city_id']!='three'])
        with self.assertRaises(ValueError):self.run_core()

    def test_core_does_not_depend_on_suitable_outcome(self):
        one=self.run_core('before')
        for row in self.data:
            if row['common_finite_area_km2']>0:
                row['suitable_fraction']=.9;row['suitable_area_km2']=.9*row['common_finite_area_km2']
        self.write(self.table,self.data);two=self.run_core('after')
        self.assertEqual(one['core_roster_sha256'],two['core_roster_sha256'])

    def test_group_denominator_difference_rejected(self):
        self.data[1]['common_finite_area_km2']=3;self.write(self.table,self.data)
        with self.assertRaises(ValueError):self.run_core()


if __name__=='__main__':unittest.main()
