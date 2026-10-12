# Original source: production_guards.py
# Original source location: ${NON_NATIVE_ROOT}/audit/environment_only_sensitivity/production_guards.py
# Original source SHA-256: e6d36524a37155505eda6d114b90d646f42665eb41e8e56b97ab6010c4f63319
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_ENVIRONMENT_ONLY_SENSITIVITY)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
import csv,math
from pathlib import Path

def strict_inventory(path):
 b=Path(path).read_bytes()
 if b.startswith(b'\xef\xbb\xbf') or b'\r' in b or b'"' in b or not b.endswith(b'\n'):
  raise ValueError('Unsafe TSV encoding/newlines/quotes')
 lines=b.decode('utf-8').splitlines();n=len(lines[0].split('\t'))
 if any(len(z.split('\t'))!=n for z in lines):raise ValueError('Wrong TSV column count')
 rows=list(csv.DictReader(lines,delimiter='\t',quoting=csv.QUOTE_NONE))
 return rows

def capacity(submitted,nodes,cpus):
 return max(0,min(30-submitted,20-nodes,(1280-cpus)//64))

def write_inventory(path,rows):
 keys=list(rows[0]);lines=['\t'.join(keys)]
 for r in rows:
  vals=[str(r.get(k,'')) for k in keys]
  if any(any(z in v for z in '\t\r\n"') for v in vals):raise ValueError('Unsafe TSV field')
  lines.append('\t'.join(vals))
 p=Path(path);temp=p.with_suffix('.tsv.new');temp.write_bytes(('\n'.join(lines)+'\n').encode('utf-8'));strict_inventory(temp);temp.replace(p)

def parse_sbatch_output(output):
 import re
 ids=[line.strip().split(';')[0] for line in output.splitlines() if re.fullmatch(r'\d+(?:;[A-Za-z0-9_.-]+)?',line.strip())]
 if len(ids)!=1:raise ValueError('Ambiguous sbatch output; reconcile queue/accounting, never resubmit blindly')
 return ids[0]

def frozen_submission_shell():
 return 'source /etc/profile.d/modules.sh_20220105; module add loginnode; export R_LIBS_USER=<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu; export R_LIBS_SITE="$R_LIBS_USER" R_LIBS="$R_LIBS_USER"; export SENSITIVITY_FROZEN_MODULE_INITIALIZED=1; export SENSITIVITY_FROZEN_LD_SHA256=$(printf \'%s\' "$LD_LIBRARY_PATH" | sha256sum | cut -d\' \' -f1); '
