"""Recompute footprint fractions by planar clipping on the unchanged response grid."""
import csv,json,math,sys,hashlib
from pathlib import Path
from collections import defaultdict
from verify_mining_polygon_support import rows,shape_rings,clip_ring,ring_area

def number(x):
    try:return float(x)
    except (ValueError,TypeError):return float("nan")
def write(path,data):
    if not data:return
    with path.open("w",newline="") as h:
        w=csv.DictWriter(h,fieldnames=list(data[0]));w.writeheader();w.writerows(data)

def main():
    cfg=json.loads(Path(sys.argv[1]).read_text());src=Path(cfg["fraction_source_output"]);out=Path(cfg["output_root"])
    if out.exists():raise RuntimeError("Existing fraction sensitivity retained")
    out.mkdir(parents=True)
    old=rows(src/"native_polygon_overlaps.csv");source={r["polygon_id"]:r for r in rows(cfg["mining_csv"])}
    grid=json.loads((src/"polygon_complete.json").read_text())["grid"]
    shapes=shape_rings(cfg["mining_shapefile"],set(r["polygon_id"] for r in old),cfg["polygon_id_field"])
    grouped=defaultdict(list);zero=[];maximum=0.;differences=[]
    for i,r in enumerate(old):
        cell=int(r["cell"])-1;col=cell%grid["ncol"];row=cell//grid["ncol"]
        x=grid["xmin"]+col*grid["xres"];north=grid["ymax"]-row*grid["yres"];south=north-grid["yres"]
        bounds=(x,x+grid["xres"],south,north)
        fraction=abs(math.fsum(ring_area(clip_ring(ring,bounds)) for ring in shapes[r["polygon_id"]]))/(grid["xres"]*grid["yres"])
        if not math.isfinite(fraction) or fraction>1+1e-8:raise RuntimeError("Invalid independently clipped fraction")
        difference=abs(fraction-float(r["fraction"]));maximum=max(maximum,difference)
        if difference>1e-6:differences.append(dict(polygon_id=r["polygon_id"],cell=r["cell"],old_fraction=r["fraction"],clipped_fraction=fraction,absolute_difference=difference))
        if fraction<=0:zero.append(dict(polygon_id=r["polygon_id"],cell=r["cell"]));continue
        area=6371.0088**2*math.radians(grid["xres"])*(math.sin(math.radians(north))-math.sin(math.radians(south)))
        r["fraction"]=fraction;r["overlap_km2"]=fraction*area
        grouped[r["polygon_id"]].append(r)
        if i%20000==0:print("Clipped",i,"of",len(old),flush=True)
    polygons=[];overlaps=[];cells=defaultdict(list)
    for identity,z in grouped.items():
        s=source[identity];total=sum(r["overlap_km2"] for r in z);weights=[r["overlap_km2"]/total for r in z]
        effective=1/sum(w*w for w in weights);large=len(z)>=cfg["large_minimum_cells"] and effective>=cfg["large_minimum_effective_cells"]
        rc=[k for k in s if k.startswith("reclamation_area_km2_")];dc=[k for k in s if k.startswith("damage_area_km2_")]
        totals=dict(footprint_km2=float(s["area_km2_source"]),damage_km2=sum(float(s[k]) for k in dc),
          reclamation_km2=sum(float(s[k]) for k in rc),endpoint_area_sum=sum(float(s[k])*int(k.split("_")[-1]) for k in rc))
        joint=[j for j,r in enumerate(z) if math.isfinite(number(r["FULL"])) and math.isfinite(number(r["ENV"]))]
        finite=sum(weights[j] for j in joint)
        poly=dict(polygon_id=identity,country=s["country_name"],n_cells=len(z),effective_cells=effective,overlap_area_km2=total,
          source_footprint_km2=totals["footprint_km2"],joint_finite_area_fraction=finite)
        for response in ("FULL","ENV"):poly[response+"_mean"]=sum(weights[j]*number(z[j][response]) for j in joint)/finite if finite else "NA"
        polygons.append(poly)
        for r,w in zip(z,weights):
            r["allocation_weight"]=w;r["large_supported"]="TRUE" if large else "FALSE"
            for k,value in totals.items():r[k]=value*w
            overlaps.append(r);cells[("all_polygon_support",r["cell"])].append(r)
            if large:cells[("large_polygon_support",r["cell"])].append(r)
    aggregates=[]
    for (scope,cell),z in cells.items():
        countries=set(r["country"] for r in z);index=int(cell)-1;row=index//grid["ncol"];col=index%grid["ncol"]
        r=dict(scope=scope,cell=cell,n_mines=len(z),country=next(iter(countries)) if len(countries)==1 else "MULTIPLE",n_countries=len(countries))
        for key in ("footprint_km2","damage_km2","reclamation_km2","endpoint_area_sum"):r[key]=sum(a[key] for a in z)
        r.update(footprint_support_sum_km2=sum(a["overlap_km2"] for a in z),fraction_sum=sum(a["fraction"] for a in z),FULL=z[0]["FULL"],ENV=z[0]["ENV"],
          cell_longitude=grid["xmin"]+(col+.5)*grid["xres"],cell_latitude=grid["ymax"]-(row+.5)*grid["yres"])
        aggregates.append(r)
    write(out/"polygon_support.csv",polygons);write(out/"native_polygon_overlaps.csv",overlaps);write(out/"native_cells.csv",aggregates)
    (out/"geometry_exclusions.csv").write_bytes((src/"geometry_exclusions.csv").read_bytes())
    with (out/"unsupported_polygons.csv").open("w",newline="") as h:
        writer=csv.writer(h);writer.writerow(["polygon_id"])
        writer.writerows([[key] for key in sorted(set(source)-set(grouped))])
    write(out/"fraction_method_differences.csv",differences);write(out/"zero_area_intersections.csv",zero)
    receipt=dict(status="INDEPENDENT_PLANAR_FRACTIONS_PENDING_SF_CHECK",source=str(src),grid=grid,
      source_overlaps_sha256=hashlib.sha256((src/"native_polygon_overlaps.csv").read_bytes()).hexdigest(),n_intersections=len(overlaps),zero_area_intersections=len(zero),
      differing_intersections_above_1e_6=len(differences),maximum_absolute_fraction_difference=maximum,
      writer_sha256=hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),response_values_reused=True)
    (out/"fraction_reconstruction.json").write_text(json.dumps(receipt,indent=2)+"\n")
    print(json.dumps(receipt,indent=2))
if __name__=="__main__":main()
