"""Draw an editable conceptual figure of coastal reclamation and plant suitability."""

from __future__ import annotations

import argparse
from pathlib import Path
from xml.etree import ElementTree as ET

import cairosvg

from draw_introduction_framework import SVG, circle, el, group, line, path, rect, text


C = {
    "ink": "#29383B", "soft": "#66767A", "rule": "#D8DFDE",
    "land": "#E9F0E7", "land_line": "#BFD2C0", "sea": "#E2EFF1",
    "sea_line": "#BAD3D7", "city": "#CCD2CF", "city_dark": "#9EAAA7",
    "sand": "#E3C990", "sand_alt": "#EBD7AE", "sand_edge": "#BA985F",
    "purple": "#766D91", "purple_pale": "#EAE7EF", "teal": "#458079",
    "rust": "#AA725D", "white": "#FFFFFF",
}


def t(parent: ET.Element, x: float, y: float, value: str, *, size: float = 27,
      weight: str = "normal", fill: str | None = None,
      anchor: str = "start") -> ET.Element:
    return text(parent, x, y, value, size=size, weight=weight,
                fill=fill or C["ink"], anchor=anchor)


def trace(parent: ET.Element, d: str, *, stroke: str | None = None,
          width: float = 2, dash: str | None = None, **attrs: object) -> ET.Element:
    kw = {"stroke_linecap": "round", "stroke_linejoin": "round", **attrs}
    if dash:
        kw["stroke_dasharray"] = dash
    return path(parent, d, stroke=stroke or C["ink"], width=width, **kw)


def arrow(parent: ET.Element, x1: float, y1: float, x2: float, y2: float,
          colour: str, dashed: bool = False) -> None:
    attrs = {"stroke_dasharray": "8 8"} if dashed else {}
    line(parent, x1, y1, x2, y2, stroke=colour, width=2.1, **attrs)
    path(parent, f"M{x2-9} {y2-5} L{x2} {y2} L{x2-9} {y2+5}",
         stroke=colour, width=2.1, stroke_linecap="round")


def ship(parent: ET.Element, x: float, y: float, colour: str) -> None:
    trace(parent, f"M{x-42} {y} H{x+43} L{x+28} {y+18} H{x-26} Z",
          stroke=colour, width=2.2)
    rect(parent, x-15, y-25, 30, 25, fill="none", stroke=colour, width=2)
    line(parent, x+1, y-46, x+1, y-25, stroke=colour, width=2)
    trace(parent, f"M{x-49} {y+26} Q{x-25} {y+20} {x} {y+26} T{x+48} {y+26}",
          stroke=colour, width=1.6)


def transit(parent: ET.Element, x: float, y: float, colour: str) -> None:
    trace(parent, f"M{x-43} {y+22} L{x-20} {y-24} H{x+17} L{x+41} {y+22}",
          stroke=colour, width=2)
    trace(parent, f"M{x-34} {y+24} L{x-13} {y-15} H{x+12} L{x+33} {y+24}",
          stroke=colour, width=2)
    line(parent, x, y-10, x, y+19, stroke=colour, width=2,
         stroke_dasharray="8 7")


def people(parent: ET.Element, x: float, y: float, colour: str) -> None:
    for dx, h in ((-24, 0), (3, -9), (30, 1)):
        circle(parent, x+dx, y-25+h, 7, stroke=colour, width=2)
        trace(parent, f"M{x+dx-13} {y+17} L{x+dx-8} {y-7+h} Q{x+dx} {y-14+h} {x+dx+8} {y-7+h} L{x+dx+13} {y+17}",
              stroke=colour, width=2)


def landuse(parent: ET.Element, ident: str, x: float, y: float, w: float,
            h: float, name: str, kind: str) -> None:
    g = group(parent, ident, name)
    rect(g, x, y, w, h, fill=C["sand_alt"], stroke=C["white"], width=2)
    cx = x + w/2
    if kind == "port":
        line(g, x+21, y+33, x+63, y+33, stroke=C["sand_edge"], width=3)
        line(g, x+63, y+33, x+63, y+54, stroke=C["sand_edge"], width=3)
        trace(g, f"M{x+83} {y+43} H{x+119} L{x+110} {y+56} H{x+89} Z",
              stroke=C["sand_edge"], width=2)
    elif kind == "transport":
        line(g, x+31, y+31, x+w-31, y+31, stroke=C["sand_edge"], width=2)
        line(g, x+31, y+51, x+w-31, y+51, stroke=C["sand_edge"], width=2)
        line(g, x+60, y+41, x+w-51, y+41, stroke=C["sand_edge"], width=1.5,
             stroke_dasharray="10 7")
    elif kind == "residential":
        for dx in (34, 81):
            rect(g, x+dx, y+36, 31, 26, fill=C["white"], stroke=C["sand_edge"], width=1.4)
            trace(g, f"M{x+dx-4} {y+36} L{x+dx+15.5} {y+20} L{x+dx+35} {y+36}",
                  stroke=C["sand_edge"], width=1.4)
    elif kind == "commercial":
        rect(g, x+25, y+17, 50, 33, fill=C["white"], stroke=C["sand_edge"], width=1.4)
        rect(g, x+87, y+13, 46, 37, fill=C["white"], stroke=C["sand_edge"], width=1.4)
        for dx in (93, 108, 123):
            line(g, x+dx, y+33, x+dx+5, y+33, stroke=C["sand_edge"], width=1.2)
    else:
        for dx, dy in ((40,31),(96,26),(151,34),(203,30)):
            line(g, x+dx, y+dy+21, x+dx, y+dy, stroke=C["sand_edge"], width=1.6)
            trace(g, f"M{x+dx} {y+dy+11} q-10 -9 -12 -2 M{x+dx} {y+dy+13} q8 -9 11 -2",
                  stroke=C["sand_edge"], width=1.3)
    if kind == "commercial":
        t(g, cx, y+h-36, "Commercial /", size=19, anchor="middle")
        t(g, cx, y+h-12, "industrial", size=19, anchor="middle")
    else:
        t(g, cx, y+h-14, name, size=20, anchor="middle")


def build() -> ET.Element:
    root = ET.Element(f"{{{SVG}}}svg", {
        "width": "180mm", "height": "113.25mm", "viewBox": "0 0 2400 1510",
        "version": "1.1", "role": "img",
        "aria-label": "Conceptual framework for coastal reclamation, arrival opportunities and predicted invasive-plant suitability",
    })
    rect(root, 0, 0, 2400, 1510, fill=C["white"])
    title = group(root, "figure-title", "Removable title", True)
    t(title, 80, 76, "Coastal reclamation: arrival, habitat and spatial context", size=40,
      weight="bold")
    t(title, 80, 119, "Does new urban land also create suitable habitat for invasive alien plants?",
      size=27, fill=C["soft"])
    line(root, 80, 146, 2320, 146, stroke=C["rule"], width=2)

    a = group(root, "panel-a", "a Reclamation as urban expansion", True)
    t(a, 80, 208, "a", size=34, weight="bold")
    t(a, 122, 208, "Reclamation as urban expansion", size=33, weight="bold")
    coast = group(a, "coastline", "Coastal plan view")
    rect(coast, 82, 258, 710, 586, fill=C["sea"])
    path(coast,"M82 258 H464 C448 315 470 343 455 396 C469 448 445 483 462 531 C450 587 472 617 456 671 C465 738 448 786 461 844 H82 Z",
         fill=C["land"])
    for d in ("M93 334 C149 308 198 319 232 335", "M94 721 C162 691 209 698 245 720"):
        trace(coast,d,stroke=C["land_line"],width=2)
    for d in ("M759 294 C718 287 682 291 650 303", "M777 779 C727 767 680 771 636 785"):
        trace(coast,d,stroke=C["sea_line"],width=2)
    city = group(coast, "existing-city", "Existing urban land")
    path(city,"M231 329 H449 C465 366 445 405 460 443 C451 498 466 553 455 597 C468 660 450 719 463 764 H230 C243 708 225 662 238 605 C226 545 240 497 229 449 C237 396 226 360 231 329 Z",
         fill=C["city"])
    for x,y,w,h in ((259,362,57,43),(341,365,78,39),(260,513,65,46),
                    (357,502,60,48),(265,648,76,45),(366,648,57,47)):
        rect(city,x,y,w,h,fill=C["city_dark"],opacity="0.54")
    line(city, 246, 472, 451, 472, stroke=C["white"], width=9, opacity="0.8")
    line(city, 333, 343, 333, 744, stroke=C["white"], width=8, opacity="0.8")
    reclaimed = group(coast, "reclaimed-land", "Reclaimed urban extension")
    path(reclaimed,"M457 327 H729 V354 H773 V746 H721 V774 H460 C451 727 467 678 455 597 C465 542 450 495 460 443 C449 407 464 365 457 327 Z",
         fill=C["sand"],stroke=C["sand_edge"],width=2.4)
    landuse(reclaimed,"landuse-port",489,367,135,93,"Port","port")
    landuse(reclaimed,"landuse-transport",626,367,135,93,"Transport","transport")
    landuse(reclaimed,"landuse-residential",489,463,135,107,"Residential","residential")
    landuse(reclaimed,"landuse-commercial",626,463,135,107,"Commercial / industrial","commercial")
    landuse(reclaimed,"landuse-open",489,590,272,121,"Open ground","open")
    former = group(coast,"shoreline-former","Former shoreline")
    trace(former,"M453 277 C443 319 470 343 455 396 C469 448 445 483 462 531 C450 587 472 617 456 671 C465 738 448 786 461 827",
          stroke=C["ink"],width=3,dash="11 10")
    current = group(coast,"shoreline-current","Current shoreline")
    trace(current,"M729 328 H774 V746 H721 V775",stroke=C["sand_edge"],width=3)
    boundary = group(coast,"city-boundary","Study city boundary")
    trace(boundary,"M214 315 H463 L786 329 V791 H214 Z",stroke=C["ink"],width=2.8)
    labels_a = group(a,"panel-a-labels","Landscape labels")
    t(labels_a,103,299,"Surrounding",size=20)
    t(labels_a,103,326,"landscape",size=20)
    t(labels_a,268,607,"Existing",size=24,weight="bold")
    t(labels_a,268,637,"urban land",size=24,weight="bold")
    t(labels_a,527,347,"Reclaimed land",size=25,weight="bold")
    t(labels_a,735,299,"Sea",size=25,fill=C["soft"])
    line(a,96,884,151,884,stroke=C["ink"],width=3)
    t(a,163,893,"City boundary",size=21)
    line(a,390,884,446,884,stroke=C["ink"],width=3,stroke_dasharray="10 9")
    t(a,459,893,"Former shoreline",size=21)
    line(a,96,921,151,921,stroke=C["sand_edge"],width=3)
    t(a,163,930,"Current shoreline",size=21)
    line(a,94,973,787,973,stroke=C["rule"],width=2)
    t(a,96,1015,"Urban expansion creates new land",size=26,weight="bold")
    t(a,96,1050,"and multiple possible urban functions.",size=25,fill=C["soft"])
    t(a,96,1112,"Connected to trade, transport",size=23,fill=C["soft"])
    t(a,96,1144,"and human movement",size=23,fill=C["soft"])

    b = group(root,"panel-b","b Arrival and suitability",True)
    t(b,870,208,"b",size=34,weight="bold")
    t(b,912,208,"Arrival is not establishment",size=33,weight="bold")
    arrival = group(b,"arrival-opportunities","Introduction opportunities")
    t(arrival,898,283,"Urban connections",size=28,weight="bold")
    line(arrival,899,299,1560,299,stroke=C["rule"],width=2)
    for x in (1012,1211,1410):
        circle(arrival,x,389,69,fill=C["purple_pale"])
    ship(arrival,1012,392,C["purple"])
    transit(arrival,1211,392,C["purple"])
    people(arrival,1410,392,C["purple"])
    t(arrival,1012,488,"Trade",size=24,anchor="middle")
    t(arrival,1211,488,"Transport",size=24,anchor="middle")
    t(arrival,1410,488,"Movement",size=24,anchor="middle")
    arrow(arrival,984,522,1480,522,C["purple"],True)
    t(arrival,902,583,"Introduction opportunities",size=29,weight="bold",fill=C["purple"])
    t(arrival,902,625,"Potential arrival; not measured directly",size=24,fill=C["soft"])
    line(b,898,670,1561,670,stroke=C["rule"],width=2)
    suitability = group(b,"habitat-suitability","Habitat suitability after arrival")
    t(suitability,898,729,"Habitat suitability",size=29,weight="bold")
    t(suitability,898,766,"The same new land can differ in suitability",size=24,fill=C["soft"])
    rect(suitability,1092,805,280,68,fill=C["sand"],stroke=C["sand_edge"],width=2)
    for x in (1148,1220,1292):
        line(suitability,x,816,x,861,stroke=C["white"],width=2,opacity="0.8")
    line(suitability,1232,873,1232,910,stroke=C["soft"],width=1.8,
         stroke_dasharray="8 8")
    line(suitability,1012,910,1452,910,stroke=C["soft"],width=1.8,
         stroke_dasharray="8 8")
    line(suitability,1012,910,1012,932,stroke=C["soft"],width=1.8,
         stroke_dasharray="8 8")
    line(suitability,1452,910,1452,932,stroke=C["soft"],width=1.8,
         stroke_dasharray="8 8")
    high = group(b,"suitability-high","H1 higher suitability")
    low = group(b,"suitability-low","H2 lower suitability")
    circle(high,1012,937,4.5,fill=C["teal"])
    circle(low,1452,937,4.5,fill=C["rust"])
    t(high,898,1000,"H1  Higher suitability",size=27,weight="bold",fill=C["teal"])
    t(high,898,1042,"Resource opportunities",size=23,fill=C["soft"])
    t(low,1261,1000,"H2  Lower suitability",size=27,weight="bold",fill=C["rust"])
    t(low,1261,1042,"Environmental constraints",size=23,fill=C["soft"])
    line(b,898,1100,1561,1100,stroke=C["rule"],width=2)
    t(b,898,1161,"Arrival and suitability can diverge.",size=28,weight="bold")
    t(b,898,1202,"These are alternative expectations.",size=24,fill=C["soft"])

    c = group(root,"panel-c","c Spatial comparisons and context",True)
    t(c,1666,208,"c",size=34,weight="bold")
    t(c,1708,208,"Spatial comparisons",size=33,weight="bold")
    comp = group(c,"spatial-comparisons","Nested spatial comparison design")
    t(comp,1676,278,"1  City / surrounding landscape",size=26,weight="bold")
    rect(comp,1692,306,186,99,fill=C["land"],stroke=C["land_line"],width=1.6)
    rect(comp,1768,333,84,54,fill=C["city"],stroke=C["ink"],width=1.5)
    t(comp,1909,350,"Whole-city setting",size=23)
    t(comp,1909,385,"relative to nearby coast",size=22,fill=C["soft"])
    line(comp,1676,433,2320,433,stroke=C["rule"],width=1.6)
    t(comp,1676,486,"2  Reclaimed / other urban land",size=26,weight="bold")
    rect(comp,1692,516,186,99,fill=C["city"],stroke=C["ink"],width=1.6)
    rect(comp,1810,554,47,38,fill=C["sand"],stroke=C["sand_edge"],width=1.5)
    t(comp,1909,558,"Two land classes",size=23)
    t(comp,1909,593,"inside the same city",size=22,fill=C["soft"])
    line(comp,1676,648,2320,648,stroke=C["rule"],width=1.6)
    context = group(c,"urban-context","City-size dependence")
    t(context,1676,706,"3  Urban context and city size",size=26,weight="bold")
    rect(context,1717,761,113,104,fill=C["city"],stroke=C["ink"],width=1.6)
    rect(context,1791,827,33,25,fill=C["sand"],stroke=C["sand_edge"],width=1.4)
    rect(context,1954,761,237,104,fill=C["city"],stroke=C["ink"],width=1.6)
    rect(context,2152,827,33,25,fill=C["sand"],stroke=C["sand_edge"],width=1.4)
    t(context,1773,905,"Small city",size=22,anchor="middle")
    t(context,2072,905,"Large city",size=22,anchor="middle")
    t(context,1676,956,"Same land addition; different city share",size=23,fill=C["soft"])
    graph = group(c,"conceptual-curves","Common and city-dependent relationships")
    line(graph,1745,1187,2233,1187,stroke=C["ink"],width=2)
    line(graph,1745,1187,1745,1004,stroke=C["ink"],width=2)
    trace(graph,"M1766 1161 C1890 1131 2070 1109 2208 1073",
          stroke=C["soft"],width=3)
    trace(graph,"M1766 1161 C1890 1138 2055 1118 2208 1103",
          stroke=C["teal"],width=3,dash="9 7")
    trace(graph,"M1766 1161 C1883 1098 2050 1062 2208 1042",
          stroke=C["rust"],width=3,dash="3 7")
    t(graph,1753,996,"Predicted suitability",size=21)
    t(graph,1987,1231,"Reclamation extent",size=21,anchor="middle")
    line(graph,1753,1273,1800,1273,stroke=C["soft"],width=2.5)
    t(graph,1811,1280,"Common",size=19)
    line(graph,1944,1273,1991,1273,stroke=C["teal"],width=2.5,
         stroke_dasharray="9 7")
    line(graph,1944,1280,1991,1280,stroke=C["rust"],width=2.5,
         stroke_dasharray="3 7")
    t(graph,2002,1280,"City-size dependent",size=19)
    t(c,1676,1330,"Conceptual expectations",size=21,fill=C["soft"])

    strip = group(root,"study-strip","Study link strip",True)
    line(strip,80,1374,2320,1374,stroke=C["rule"],width=2)
    t(strip,80,1433,"Predicted invasive-plant suitability",size=27,weight="bold")
    for x,label_text in ((792,"Species distribution models"),
                         (1197,"Nested spatial contrasts"),
                         (1601,"City-scale and scale-dependent models"),
                         (2083,"FULL / environment-only")):
        circle(strip,x,1423,3.5,fill=C["soft"])
        t(strip,x+16,1431,label_text,size=20)
    return root


def validate(root: ET.Element) -> list[str]:
    nodes = list(root.iter())
    ids = [node.get("id") for node in nodes if node.get("id")]
    assert len(ids) == len(set(ids)), "Duplicate SVG IDs"
    assert not any(node.tag.endswith("}image") or node.tag.endswith("}foreignObject")
                   for node in nodes), "Non-vector element found"
    required = {"figure-title", "panel-a", "panel-b", "panel-c", "coastline",
                "shoreline-former", "shoreline-current", "existing-city",
                "reclaimed-land", "landuse-port", "landuse-residential",
                "landuse-transport", "landuse-commercial", "landuse-open",
                "arrival-opportunities", "suitability-high", "suitability-low",
                "spatial-comparisons", "study-strip"}
    assert required.issubset(ids), required - set(ids)
    values = " ".join(node.text or "" for node in nodes if node.tag.endswith("}text"))
    for phrase in ("Introduction opportunities", "Habitat suitability", "Open ground",
                   "Reclaimed land", "Former shoreline", "Predicted invasive-plant suitability"):
        assert phrase in values, phrase
    return ["SVG XML: valid", f"Unique IDs: {len(ids)}",
            "Raster/foreign elements: 0",
            f"Editable text elements: {sum(node.tag.endswith('}text') for node in nodes)}",
            "Required semantic groups and message labels: present"]


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", type=Path, required=True)
    parser.add_argument("--replace-existing", action="store_true",
                        help="Replace files produced by an earlier run of this script")
    args = parser.parse_args()
    figs = args.output_dir / "figures"
    qc = args.output_dir / "qc"
    figs.mkdir(parents=True, exist_ok=True)
    qc.mkdir(parents=True, exist_ok=True)
    root = build()
    checks = validate(root)
    data = ET.tostring(root, encoding="utf-8", xml_declaration=True)
    svg = figs / "introduction_reclamation_framework.svg"
    png = figs / "introduction_reclamation_framework_preview.png"
    assert args.replace_existing or (not svg.exists() and not png.exists()), \
        "Requested figure filenames already exist"
    svg.write_bytes(data)
    ET.parse(svg)
    cairosvg.svg2png(bytestring=data, write_to=str(png),
                     output_width=3600, output_height=2265)
    checks.append("Preview: rendered in Python at 3600 x 2265 pixels")
    (qc / "svg_validation_introduction_reclamation_framework.txt").write_text("\n".join(checks) + "\n", encoding="utf-8")
    print(svg)
    print(png)


if __name__ == "__main__":
    main()
