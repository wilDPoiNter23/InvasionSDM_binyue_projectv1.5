"""Reconstruct coastal conditional contrasts and between-city coefficients."""
import csv,json,sys,math
from pathlib import Path
from collections import defaultdict
import numpy as np

def rows(p):
    with p.open(newline="",encoding="utf-8-sig") as h:return list(csv.DictReader(h))
def number(x):
    try:return float(x)
    except (ValueError,TypeError):return float("nan")
def supported(r):return number(r["native_cells"])>=3 and number(r["effective_cells"])>=2

def main():
    cfg=json.loads(Path(sys.argv[1]).read_text(encoding="utf-8-sig"));out=Path(cfg["association_output"])
    target=out/"independent_joint_statistics.json"
    if target.exists():raise RuntimeError("Existing verification retained")
    d=rows(Path(cfg["joint_output"])/"joint_native_summary.csv")
    d=[r for r in d if all(math.isfinite(number(r[n])) for n in cfg["responses"])]
    errors=[];checks=0
    def compare(label,actual,expected):
        nonlocal checks
        checks+=1
        if not math.isclose(actual,expected,rel_tol=1e-8,abs_tol=1e-10):errors.append([label,actual,expected])
    gates=rows(out/"use_contrast_support.csv")
    estimates={(r["scope"],r["response"],r["use_a"],r["use_b"]):r for r in rows(out/"supported_use_contrasts.csv")}
    for g in gates:
        q=d if g["scope"]=="all_mapped" else [r for r in d if supported(r)]
        left={(r["city_id"],r["period_endpoint"]):r for r in q if r["landuse"]==g["use_a"]}
        right={(r["city_id"],r["period_endpoint"]):r for r in q if r["landuse"]==g["use_b"]}
        common=set(left)&set(right);bycity=defaultdict(list)
        for k in common:
            a,b=left[k],right[k]
            bycity[k[0]].append((number(b[g["response"]])-number(a[g["response"]]),min(number(a["area_km2"]),number(b["area_km2"]))))
        differences=[sum(v*w for v,w in z)/sum(w for v,w in z) for z in bycity.values() if len(z)>=int(cfg["minimum_contrast_periods"])]
        compare("Matched city support",len(differences),int(g["n_matched_cities"]))
        compare("Matched cohort support",len(common),int(g["n_matched_city_periods"]))
        key=(g["scope"],g["response"],g["use_a"],g["use_b"])
        if key in estimates:compare(str(key),sum(differences)/len(differences),number(estimates[key]["use_b_minus_use_a"]))
    source=rows(out/"between_city_model_source.csv")
    estimates={(r["scope"],r["response"],r["adjustment"]):r for r in rows(out/"between_city_static_association.csv")}
    for g in rows(out/"between_city_support.csv"):
        if g["status"]!="SUPPORTED":continue
        q=source if g["scope"]=="all_mapped" else [r for r in source if supported(r)]
        fields=[v.strip() for v in g["formula"].split("~")[1].split("+")]
        q=[r for r in q if all(math.isfinite(number(r[v])) for v in fields+[g["response"]])]
        x=np.array([[1.]+[number(r[v]) for v in fields] for r in q]);y=np.array([number(r[g["response"]]) for r in q])
        beta=np.linalg.lstsq(x,y,rcond=None)[0]
        compare("Between-city support",len(q),int(g["n_cities"]))
        key=(g["scope"],g["response"],g["adjustment"])
        compare(str(key),float(beta[fields.index("cohort")+1]),number(estimates[key]["slope_per_later_mean_five_year_cohort"]))
    report=dict(status="PASS_CONTRAST_AND_BETWEEN_POINTS" if not errors else "FAIL",numeric_checks=checks,errors=errors,scope="Independent matched support, use contrasts and OLS point estimates; bootstrap intervals not independently regenerated")
    target.write_text(json.dumps(report,indent=2)+"\n");print(json.dumps(report,indent=2))
    return 0 if not errors else 1
if __name__=="__main__":raise SystemExit(main())
