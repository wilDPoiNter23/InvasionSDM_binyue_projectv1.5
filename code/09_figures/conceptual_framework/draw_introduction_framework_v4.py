"""Draw a coastal landscape illustration with editable SVG labels and geometry."""

from __future__ import annotations

import argparse
from pathlib import Path
from xml.etree import ElementTree as ET

import cairosvg

from draw_introduction_framework import SVG, circle, el, group, line, path, rect, text


K = {
    "ink": "#2D3B3B", "muted": "#637477", "rule": "#D6DCDA",
    "water": "#DBECEE", "water_deep": "#AECFD3", "water_mark": "#A9CDD1",
    "land": "#E5EEE3", "land_edge": "#B4C9B3", "tree": "#8FAF8E",
    "wetland": "#C7D9BD", "urban": "#C9CFCC", "urban_edge": "#A4B2AE",
    "roof": "#DDE0DB", "reclaim": "#DDC18A", "reclaim_edge": "#AE8D56",
    "teal": "#3F7875", "rust": "#A56956", "white": "#FFFFFF",
}


def label(parent: ET.Element, x: float, y: float, value: str, size: float = 27,
          weight: str = "normal", colour: str | None = None,
          anchor: str = "start") -> ET.Element:
    return text(parent, x, y, value, size=size, weight=weight,
                fill=colour or K["ink"], anchor=anchor)


def polygon(parent: ET.Element, points: list[tuple[float, float]], *, fill: str = "none",
            stroke: str = "none", width: float = 1.5, **kwargs: object) -> ET.Element:
    d = "M" + " L".join(f"{x:.1f},{y:.1f}" for x, y in points) + " Z"
    return path(parent, d, fill=fill, stroke=stroke, width=width,
                stroke_linejoin="round", **kwargs)


def trace(parent: ET.Element, points: list[tuple[float, float]], *,
          stroke: str, width: float = 2, dash: str | None = None,
          **kwargs: object) -> ET.Element:
    d = "M" + " L".join(f"{x:.1f},{y:.1f}" for x, y in points)
    attrs = {"stroke_linecap": "round", "stroke_linejoin": "round", **kwargs}
    if dash:
        attrs["stroke_dasharray"] = dash
    return path(parent, d, stroke=stroke, width=width, **attrs)


def p(u: float, v: float) -> tuple[float, float]:
    return 383 + 1.15*u - .467*v, 270 + .28*u + .456*v


def plan(parent: ET.Element, pts: list[tuple[float, float]], **kwargs: object) -> ET.Element:
    return polygon(parent, [p(u, v) for u, v in pts], **kwargs)


def tree(parent: ET.Element, u: float, v: float, r: float) -> None:
    x, y = p(u, v)
    path(parent, f"M{x-2:.1f},{y:.1f} Q{x:.1f},{y-8:.1f} {x+3:.1f},{y-1:.1f}",
         stroke=K["land_edge"], width=1.3)
    circle(parent, x-4, y-5, r*.75, fill=K["tree"], opacity="0.7")
    circle(parent, x+4, y-6, r, fill=K["tree"], opacity="0.87")
    circle(parent, x+1, y-10, r*.76, fill=K["land_edge"], opacity="0.8")


def building(parent: ET.Element, u: float, v: float, du: float, dv: float,
             h: float) -> None:
    a, b, c, d = [p(*q) for q in ((u, v), (u+du, v),
                                  (u+du, v+dv), (u, v+dv))]
    polygon(parent, [b, c, (c[0], c[1]-h), (b[0], b[1]-h)],
            fill=K["urban_edge"], opacity="0.83")
    polygon(parent, [c, d, (d[0], d[1]-h), (c[0], c[1]-h)],
            fill=K["urban_edge"], opacity="0.68")
    polygon(parent, [(x, y-h) for x, y in (a,b,c,d)],
            fill=K["roof"], stroke=K["urban_edge"], width=1.1)


def small_city(parent: ET.Element, x: float, y: float, w: float,
               patch_x: float) -> None:
    h = 92
    polygon(parent, [(x,y), (x+w,y), (x+w+17,y+h), (x-9,y+h)],
            fill=K["urban"], stroke=K["urban_edge"], width=2)
    polygon(parent, [(x+patch_x,y+h-26), (x+patch_x+45,y+h-26),
                     (x+patch_x+46,y+h-1), (x+patch_x,y+h-1)],
            fill=K["reclaim"], stroke=K["reclaim_edge"], width=1.8)


def build() -> ET.Element:
    root = ET.Element(f"{{{SVG}}}svg", {
        "width": "180mm", "height": "108.75mm", "viewBox": "0 0 2400 1450",
        "version": "1.1", "role": "img",
        "aria-label": "Coastal reclamation and invasive-plant suitability conceptual illustration",
    })
    defs = el(root, "defs")
    detail_clip = el(defs, "clipPath", id="detail-clip")
    circle(detail_clip, 1942, 390, 170, fill=K["white"])
    rect(root, 0, 0, 2400, 1450, fill=K["white"])
    title = group(root, "figure-title", "Removable title", True)
    label(title, 85, 85, "From coastal land creation to invasive-plant suitability", 43, "bold")
    line(root, 85, 118, 2310, 118, stroke=K["rule"], width=2)

    a = group(root, "panel-a", "a Coastal city and reclamation", True)
    label(a, 85, 186, "a", 34, "bold")
    label(a, 130, 186, "A coast becomes urban land", 35, "bold")
    scene = group(a, "coastal-scene", "Continuous coastal landscape")
    bounds = [p(*q) for q in ((0,0),(850,0),(850,690),(0,690))]
    # Only the near edge is drawn as a shallow cutaway; height is symbolic.
    polygon(scene, [bounds[1],bounds[2],(bounds[2][0],bounds[2][1]+31),
                    (bounds[1][0],bounds[1][1]+31)], fill=K["water_deep"])
    polygon(scene, [bounds[2],bounds[3],(bounds[3][0],bounds[3][1]+31),
                    (bounds[2][0],bounds[2][1]+31)], fill=K["water_deep"],
            opacity="0.76")
    polygon(scene, bounds, fill=K["water"])
    shore = [(534,0),(542,56),(529,110),(546,170),(536,225),
             (554,285),(541,337),(557,405),(548,467),(564,530),
             (548,590),(562,645),(555,690)]
    land_shape = [(0,0),*shore,(0,690)]
    plan(scene, land_shape, fill=K["land"])
    # Coastal fringe has a narrow, irregular transition where there is no city.
    plan(scene, [(512,0),(534,0),(542,56),(529,110),(546,170),
                 (519,170),(513,110),(524,56)], fill=K["wetland"])
    plan(scene, [(535,594),(548,590),(562,645),(555,690),(526,690),
                 (530,645)], fill=K["wetland"])
    for pts in (
        [(25,75),(107,83),(182,76),(233,65)],
        [(15,161),(92,170),(157,159),(216,172)],
        [(16,582),(89,573),(155,583),(220,567)],
        [(28,634),(101,625),(167,632),(236,618)],
    ):
        trace(scene,[p(*q) for q in pts],stroke=K["land_edge"],width=1.8)
    for u,v,r in ((54,112,7),(88,123,6),(134,103,8),(176,138,7),
                  (61,223,7),(113,245,6),(164,216,8),(98,347,8),
                  (152,369,7),(203,349,6),(66,466,7),(111,491,8),
                  (164,473,6),(211,497,7),(48,587,6),(105,608,8),
                  (162,608,7),(229,595,6)):
        tree(scene,u,v,r)
    # Fine water marks support the spatial setting without representing flow.
    for pts in (
        [(680,72),(717,68),(752,76)],[(716,132),(748,135),(777,129)],
        [(758,558),(792,553),(823,558)],[(685,623),(724,628),(765,622)],
    ):
        trace(scene,[p(*q) for q in pts],stroke=K["water_mark"],width=2)
    urban = group(scene, "existing-urban-land", "Existing urban area")
    urban_pts = [(237,79),(499,83),(525,127),(518,189),(532,242),
                 (520,318),(533,391),(521,458),(533,520),(511,572),
                 (244,560),(220,503),(231,428),(223,365),(238,300),
                 (225,236),(239,163)]
    plan(urban, urban_pts, fill=K["urban"], stroke=K["urban_edge"], width=1.8)
    for pts in (
        [(234,230),(323,225),(411,232),(519,233)],
        [(232,365),(327,361),(416,368),(529,369)],
        [(322,91),(325,176),(326,271),(328,358),(330,458),(328,545)],
    ):
        trace(urban,[p(*q) for q in pts],stroke=K["white"],width=8,opacity="0.78")
    for args in (
        (260,115,49,50,20),(350,107,58,53,29),(432,120,45,48,18),
        (258,267,53,49,16),(351,266,52,55,31),(439,277,43,50,23),
        (263,402,49,53,22),(356,404,55,53,26),(442,411,46,51,17),
        (263,492,48,40,13),(360,493,51,41,18),(441,492,45,43,14),
    ):
        building(urban,*args)
    reclaimed = group(scene,"reclaimed-land","Newly created land")
    new_pts = [(532,218),(690,211),(710,245),(756,245),(757,506),
               (716,508),(711,545),(560,547),(548,514),(563,463),
               (548,402),(557,344),(541,285)]
    plan(reclaimed,new_pts,fill=K["reclaim"],stroke=K["reclaim_edge"],width=2.4)
    for pts in (
        [(586,229),(592,321),(595,413),(590,524)],
        [(653,220),(656,321),(658,409),(653,528)],
        [(555,350),(623,347),(698,348),(745,344)],
    ):
        trace(reclaimed,[p(*q) for q in pts],stroke=K["white"],width=3.3,
              opacity="0.63")
    # Pier is a small piece of context, rather than an arrival-rate variable.
    trace(reclaimed,[p(*q) for q in ((754,350),(818,350),(819,375))],
          stroke=K["reclaim_edge"],width=5)
    former = group(scene,"former-shoreline","Former shoreline")
    trace(former,[p(*q) for q in shore],stroke=K["ink"],width=3.0,dash="11 11")
    boundary = group(scene,"city-boundary","City boundary")
    city_edge = [(210,70),(534,68),(551,207),(770,204),(775,553),
                 (545,556),(534,584),(219,585),(204,493)]
    plan(boundary,city_edge,stroke=K["ink"],width=3.3)
    labels = group(a,"direct-labels","Landscape labels")
    label(labels,95,310,"Surrounding",28)
    label(labels,95,347,"landscape",28)
    trace(labels,[(255,350),(323,410)],stroke=K["muted"],width=1.6)
    label(labels,587,818,"Existing city",28)
    trace(labels,[(664,782),(682,648)],stroke=K["muted"],width=1.6)
    label(labels,1070,853,"Reclaimed land",28,"bold")
    trace(labels,[(1162,811),(1101,700)],stroke=K["reclaim_edge"],width=1.7)
    label(labels,1341,424,"Sea",28,colour=K["muted"])
    # Two line styles are decoded beside the landscape, not in a large legend.
    line(labels,91,918,149,918,stroke=K["ink"],width=3)
    label(labels,165,927,"City boundary",25)
    line(labels,517,918,575,918,stroke=K["ink"],width=3,
         stroke_dasharray="11 10")
    label(labels,591,927,"Former shoreline",25)
    comparisons = group(a,"spatial-comparisons","Two nested spatial comparisons")
    line(comparisons,86,974,1478,974,stroke=K["rule"],width=2)
    label(comparisons,90,1030,"Two spatial contrasts",29,"bold")
    label(comparisons,91,1098,"1",30,"bold",K["teal"])
    label(comparisons,133,1098,"Whole city  /  surrounding coast",27)
    label(comparisons,91,1168,"2",30,"bold",K["reclaim_edge"])
    label(comparisons,133,1168,"Reclaimed  /  other urban land",27)
    label(comparisons,133,1207,"within each city",24,colour=K["muted"])

    line(root,1538,148,1538,1260,stroke=K["rule"],width=2)
    b = group(root,"panel-b","b Alternative suitability expectations",True)
    label(b,1591,186,"b",34,"bold")
    label(b,1638,186,"New land, uncertain suitability",34,"bold")
    # This close view is a single reclaimed surface, not two habitat types.
    detail = group(b,"reclaimed-detail","Enlarged reclaimed coastal edge")
    circle(detail,1942,390,170,fill=K["water"])
    clipped = el(detail,"g",clip_path="url(#detail-clip)")
    path(clipped,"M1710 209 H1889 C1860 252 1873 279 1857 314 C1873 346 1849 378 1864 408 C1844 449 1863 491 1850 570 H1710 Z",
         fill=K["urban"])
    path(clipped,"M1860 317 C1902 322 1928 323 1958 309 L1958 340 L2019 340 L2019 312 L2146 312 L2146 523 L2020 523 L2020 492 L1949 492 C1919 471 1895 463 1859 468 C1872 419 1845 386 1864 351 Z",
         fill=K["reclaim"],stroke=K["reclaim_edge"],width=2)
    path(clipped,"M1857 231 C1874 280 1847 294 1860 317 C1874 346 1849 378 1864 408 C1844 449 1863 491 1850 555",
         stroke=K["ink"],width=2.1,stroke_dasharray="9 9")
    trace(clipped,[(1890,381),(1989,381),(2100,383)],stroke=K["white"],width=4,
          opacity="0.73")
    trace(clipped,[(1965,344),(1967,466)],stroke=K["white"],width=3.1,
          opacity="0.73")
    circle(detail,1942,390,170,stroke=K["rule"],width=2)
    label(b,1620,594,"Same newly created surface",26,colour=K["muted"])
    op = group(b,"hypothesis-opportunity","H1 higher predicted suitability")
    con = group(b,"hypothesis-constraints","H2 lower predicted suitability")
    line(b,1619,635,2312,635,stroke=K["rule"],width=2)
    line(op,1620,688,1672,688,stroke=K["teal"],width=5)
    label(op,1691,696,"H1  Higher suitability",29,"bold",K["teal"])
    line(con,1620,749,1672,749,stroke=K["rust"],width=5)
    label(con,1691,757,"H2  Lower suitability",29,"bold",K["rust"])
    label(b,1620,807,"Alternative expectations; neither is presumed",24,
          colour=K["muted"])

    c = group(root,"panel-c","c Spatial predictions",True)
    line(c,1591,857,2312,857,stroke=K["rule"],width=2)
    label(c,1591,915,"c",34,"bold")
    label(c,1638,915,"Spatial predictions",34,"bold")
    label(c,1619,974,"Within-city contrast",27,"bold")
    city = group(c,"within-city-contrast","Reclaimed versus other urban land")
    small_city(city,1640,1003,278,210)
    label(city,1640,1155,"Other urban",24)
    label(city,1847,1155,"Reclaimed",24)
    eq = label(city,1620,1222,"",27,"bold")
    for value,sub in (("Δ",False),("local",True),(" = S",False),
                      ("reclaimed",True),(" − S",False),("other urban",True)):
        attrs={"baseline_shift":"sub","font_size":18} if sub else {}
        el(eq,"tspan",**attrs).text=value
    label(city,1620,1261,"Either sign is possible",23,colour=K["muted"])
    line(c,1991,974,1991,1275,stroke=K["rule"],width=2)
    context = group(c,"hypothesis-context","H3 urban-context dependence")
    label(context,2035,974,"H3  Urban context",27,"bold")
    small_city(context,2045,1034,83,48)
    small_city(context,2204,1016,143,108)
    label(context,2050,1184,"Small",23)
    label(context,2218,1184,"Large",23)
    label(context,2035,1233,"Equal land addition",23,colour=K["muted"])
    label(context,2035,1261,"different city area",23,colour=K["muted"])

    tests = group(root,"study-tests","Study test strip",True)
    line(tests,85,1328,2312,1328,stroke=K["rule"],width=2)
    label(tests,85,1386,"Response  Predicted invasive-plant suitability",28,"bold")
    label(tests,1119,1386,"Nested contrasts",24)
    circle(tests,1085,1378,3.5,fill=K["muted"])
    label(tests,1509,1386,"Reclamation × city size",24)
    circle(tests,1476,1378,3.5,fill=K["muted"])
    label(tests,2015,1386,"FULL / environment-only",24)
    circle(tests,1982,1378,3.5,fill=K["muted"])
    return root


def validate(root: ET.Element) -> list[str]:
    nodes=list(root.iter())
    ids=[n.get("id") for n in nodes if n.get("id")]
    assert len(ids)==len(set(ids))
    assert not any(n.tag.endswith("}image") or n.tag.endswith("}foreignObject")
                   for n in nodes)
    required={"figure-title","panel-a","panel-b","panel-c","city-boundary",
              "former-shoreline","reclaimed-land","hypothesis-opportunity",
              "hypothesis-constraints","hypothesis-context","study-tests"}
    assert required.issubset(ids),required-set(ids)
    return ["SVG XML: valid",f"Unique IDs: {len(ids)}",
            "Raster/foreign image elements: 0",
            f"Editable text elements: {sum(n.tag.endswith('}text') for n in nodes)}",
            "Required semantic groups: present"]


def main() -> None:
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir",type=Path,required=True)
    args=parser.parse_args()
    figs,qc=args.output_dir/"figures",args.output_dir/"qc"
    figs.mkdir(parents=True,exist_ok=True)
    qc.mkdir(parents=True,exist_ok=True)
    root=build()
    checks=validate(root)
    data=ET.tostring(root,encoding="utf-8",xml_declaration=True)
    svg=figs/"introduction_conceptual_framework_v4.svg"
    png=figs/"introduction_conceptual_framework_v4_preview.png"
    svg.write_bytes(data)
    ET.parse(svg)
    cairosvg.svg2png(bytestring=data,write_to=str(png),
                     output_width=3600,output_height=2175)
    checks.append("Preview: rendered from SVG with Python/CairoSVG at 3600 x 2175 pixels")
    (qc/"svg_validation_introduction_conceptual_framework_v4.txt").write_text("\n".join(checks)+"\n",encoding="utf-8")
    print(svg)
    print(png)


if __name__=="__main__":
    main()
