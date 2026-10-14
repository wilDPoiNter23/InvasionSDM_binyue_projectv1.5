#!/usr/bin/env python3
"""Check polygon allocation, shared cells and sampled native intersections."""
import csv
import json
import math
import struct
import sys
from collections import defaultdict
from pathlib import Path


def rows(path):
    with Path(path).open(encoding="utf-8-sig", newline="") as handle:
        return list(csv.DictReader(handle))


def ring_area(ring):
    return .5*sum(a[0]*b[1]-b[0]*a[1] for a,b in zip(ring,ring[1:]+ring[:1]))


def clip_ring(ring, bounds):
    points = ring
    for axis,bound,sign in ((0,bounds[0],1),(0,bounds[1],-1),(1,bounds[2],1),(1,bounds[3],-1)):
        clipped = []
        if not points:
            break
        for a,b in zip(points[-1:]+points[:-1],points):
            inside_a,inside_b = sign*(a[axis]-bound)>=0,sign*(b[axis]-bound)>=0
            if inside_a != inside_b:
                ratio=(bound-a[axis])/(b[axis]-a[axis])
                clipped.append((a[0]+ratio*(b[0]-a[0]),a[1]+ratio*(b[1]-a[1])))
            if inside_b:
                clipped.append(b)
        points=clipped
    return points


def shape_rings(path, selected, id_field):
    """Read polygon rings and their DBF identity without spatial dependencies."""
    result = {}
    with Path(path).with_suffix(".dbf").open("rb") as dbf, Path(path).open("rb") as shp:
        header=dbf.read(32)
        n,hlen,rlen=struct.unpack_from("<IHH",header,4)
        fields=[]
        while True:
            first=dbf.read(1)
            if first==b"\r":
                break
            descriptor=first+dbf.read(31)
            fields.append((descriptor[:11].split(b"\0")[0].decode("ascii"),descriptor[16]))
        offset=1
        location=None
        for name,length in fields:
            if name==id_field:
                location=(offset,length)
            offset+=length
        if location is None:
            raise ValueError("Polygon identity field missing in DBF")
        dbf.seek(hlen); shp.seek(100)
        for _ in range(n):
            record=dbf.read(rlen)
            rh=shp.read(8)
            if len(rh)!=8:
                raise ValueError("Truncated shapefile")
            content=shp.read(struct.unpack(">ii",rh)[1]*2)
            identity=record[location[0]:location[0]+location[1]].decode("ascii").strip()
            if identity not in selected:
                continue
            typ=struct.unpack_from("<i",content)[0]
            if typ not in (5,15,25):
                raise ValueError("Unsupported polygon shape type")
            parts,npoints=struct.unpack_from("<ii",content,36)
            starts=list(struct.unpack_from("<"+"i"*parts,content,44))+[npoints]
            vertices=[struct.unpack_from("<dd",content,44+4*parts+16*j) for j in range(npoints)]
            result[identity]=[vertices[a:b] for a,b in zip(starts[:-1],starts[1:])]
    return result


def main():
    cfg=json.loads(Path(sys.argv[1]).read_text(encoding="utf-8-sig")); out=Path(cfg["output_root"])
    target=out/"polygon_independent_verification.json"
    if target.exists():
        raise FileExistsError("Existing polygon verification retained")
    source={r["polygon_id"]:r for r in rows(cfg["mining_csv"])}
    polygons={r["polygon_id"]:r for r in rows(out/"polygon_support.csv")}
    overlaps=rows(out/"native_polygon_overlaps.csv"); cells=rows(out/"native_cells.csv")
    metadata=json.loads((out/"polygon_complete.json").read_text()); grid=metadata["grid"]
    checks=[]
    def check(name,value):
        checks.append({"name":name,"passed":bool(value)})
    bypoly=defaultdict(list); bycell=defaultdict(list)
    area_ok=True
    for r in overlaps:
        bypoly[r["polygon_id"]].append(r); bycell[r["cell"]].append(r)
        cell=int(r["cell"])-1; row=cell//grid["ncol"]
        north=grid["ymax"]-row*grid["yres"]; south=north-grid["yres"]
        area=6371.0088**2*math.radians(grid["xres"])*(math.sin(math.radians(north))-math.sin(math.radians(south)))
        fraction=float(r["fraction"])
        area_ok &= 0<fraction<=1+1e-8 and math.isclose(area*fraction,float(r["overlap_km2"]),rel_tol=1e-8,abs_tol=1e-10)
        p=polygons[r["polygon_id"]]
        large=int(p["n_cells"])>=cfg["large_minimum_cells"] and float(p["effective_cells"])>=cfg["large_minimum_effective_cells"]
        area_ok &= (r["large_supported"]=="TRUE")==large
    check("native spherical overlap areas and large-mine eligibility",area_ok)
    check("unique polygon-cell support",len({(r["polygon_id"],r["cell"]) for r in overlaps})==len(overlaps))
    allocation=True
    for identity,z in bypoly.items():
        s=source[identity]; p=polygons[identity]
        total=sum(float(r["overlap_km2"]) for r in z)
        weights=[float(r["overlap_km2"])/total for r in z]
        allocation &= math.isclose(total,float(p["overlap_area_km2"]),rel_tol=1e-9)
        allocation &= int(p["n_cells"])==len(z)
        allocation &= math.isclose(1/sum(w*w for w in weights),float(p["effective_cells"]),rel_tol=1e-9)
        expected={"footprint_km2":float(s["area_km2_source"]),
                  "damage_km2":sum(float(v) for k,v in s.items() if k.startswith("damage_area_km2_")),
                  "reclamation_km2":sum(float(v) for k,v in s.items() if k.startswith("reclamation_area_km2_")),
                  "endpoint_area_sum":sum(float(v)*int(k.rsplit("_",1)[1]) for k,v in s.items() if k.startswith("reclamation_area_km2_"))}
        for i,r in enumerate(z):
            allocation &= math.isclose(weights[i],float(r["allocation_weight"]),rel_tol=1e-9)
            for key,value in expected.items():
                allocation &= math.isclose(value*weights[i],float(r[key]),rel_tol=1e-9,abs_tol=1e-7)
        joint=[i for i,r in enumerate(z) if r["FULL"] not in ("NA","NaN","") and r["ENV"] not in ("NA","NaN","")]
        coverage=sum(weights[i] for i in joint)
        allocation &= math.isclose(coverage,float(p["joint_finite_area_fraction"]),abs_tol=1e-9)
        if coverage>0:
            for response in ("FULL","ENV"):
                mean=sum(weights[i]*float(z[i][response]) for i in joint)/coverage
                allocation &= math.isclose(mean,float(p[response+"_mean"]),abs_tol=1e-9)
    check("source totals conserved and polygon weighted means reconstructed",allocation)
    aggregate=True
    for r in cells:
        z=bycell[r["cell"]]
        if r["scope"]=="large_polygon_support":
            z=[v for v in z if v["large_supported"]=="TRUE"]
        aggregate &= len(z)==int(r["n_mines"]) and len({v["country"] for v in z})==int(r["n_countries"])
        for key in ("footprint_km2","damage_km2","reclamation_km2","endpoint_area_sum"):
            aggregate &= math.isclose(sum(float(v[key]) for v in z),float(r[key]),rel_tol=1e-9,abs_tol=1e-7)
        for response in ("FULL","ENV"):
            aggregate &= all(v[response]==r[response] for v in z)
    check("shared native cells collapsed with preserved response identity",aggregate)
    # Spread deterministic samples across the full intersection table.
    sample=[overlaps[i] for i in sorted({round(i*(len(overlaps)-1)/127) for i in range(128)})]
    shapes=shape_rings(cfg["mining_shapefile"],{r["polygon_id"] for r in sample},cfg["polygon_id_field"])
    max_error=0.0
    for r in sample:
        cell=int(r["cell"])-1; col=cell%grid["ncol"]; row=cell//grid["ncol"]
        x=grid["xmin"]+col*grid["xres"]; y=grid["ymax"]-row*grid["yres"]
        bounds=(x,x+grid["xres"],y-grid["yres"],y)
        ringlist=shapes[r["polygon_id"]]
        fraction=abs(sum(ring_area(clip_ring(ring,bounds)) for ring in ringlist))/(grid["xres"]*grid["yres"])
        max_error=max(max_error,abs(fraction-float(r["fraction"])))
    check("independent rectangle clipping agrees with exact native fractions",max_error<1e-6)
    check("native cell keys unique per scope",len({(r["scope"],r["cell"]) for r in cells})==len(cells))
    passed=all(c["passed"] for c in checks)
    report={"status":"PASS_POLYGON_SUPPORT" if passed else "FAIL","checks":checks,
            "n_sampled_intersections":len(sample),"max_sample_fraction_absolute_error":max_error,
            "scope":"Source totals, support allocation and independent planar native intersections; cluster intervals checked separately",
            "independent_raster_read":False,"scientific_acceptance":False}
    target.write_text(json.dumps(report,indent=2)+"\n"); print(json.dumps(report,indent=2))
    return 0 if passed else 1


if __name__=="__main__":
    raise SystemExit(main())
