"""Draw an editable Introduction figure about reclamation and plant suitability."""

from __future__ import annotations

import argparse
from pathlib import Path
from xml.etree import ElementTree as ET

import cairosvg


SVG = "http://www.w3.org/2000/svg"
ET.register_namespace("", SVG)

P = {
    "ink": "#23383B", "muted": "#64777A", "rule": "#DCE4E2",
    "sea": "#DDECEF", "sea_line": "#A8C9CD", "land": "#E9EEE5",
    "vegetation": "#8EAD96", "vegetation_dark": "#557D6F",
    "city": "#C9D0CD", "city_side": "#ADB9B5", "city_top": "#E3E8E5",
    "reclaim": "#E5CA91", "reclaim_light": "#F1DEB8", "reclaim_edge": "#B58E53",
    "arrival": "#75698D", "arrival_light": "#EAE5EF",
    "favour": "#48877B", "favour_light": "#DBECE5",
    "restrict": "#AB765C", "restrict_light": "#F2E5D9",
    "white": "#FFFFFF",
}


def add(parent: ET.Element, tag: str, **attrs: object) -> ET.Element:
    node = ET.SubElement(parent, f"{{{SVG}}}{tag}")
    for key, value in attrs.items():
        node.set(key.replace("_", "-"), str(value))
    return node


def g(parent: ET.Element, ident: str, label: str | None = None) -> ET.Element:
    node = add(parent, "g", id=ident)
    if label:
        node.set("aria-label", label)
    return node


def txt(parent: ET.Element, x: float, y: float, value: str, size: int = 30,
        colour: str = "ink", weight: int = 400, anchor: str = "start") -> ET.Element:
    node = add(parent, "text", x=x, y=y, fill=P.get(colour, colour),
               font_family="Arial, Helvetica, sans-serif", font_size=size,
               font_weight=weight, text_anchor=anchor)
    node.text = value
    return node


def ln(parent: ET.Element, x1: float, y1: float, x2: float, y2: float,
       colour: str = "ink", width: float = 2.5, dash: str | None = None,
       opacity: float = 1) -> ET.Element:
    attrs = dict(x1=x1, y1=y1, x2=x2, y2=y2, stroke=P.get(colour, colour),
                 stroke_width=width, stroke_linecap="round", opacity=opacity)
    if dash:
        attrs["stroke_dasharray"] = dash
    return add(parent, "line", **attrs)


def poly(parent: ET.Element, points: str, fill: str, stroke: str | None = None,
         width: float = 2, opacity: float = 1) -> ET.Element:
    attrs = dict(points=points, fill=P.get(fill, fill), opacity=opacity)
    if stroke:
        attrs.update(stroke=P.get(stroke, stroke), stroke_width=width,
                     stroke_linejoin="round")
    return add(parent, "polygon", **attrs)


def path(parent: ET.Element, d: str, fill: str = "none", stroke: str | None = None,
         width: float = 2.5, dash: str | None = None, opacity: float = 1) -> ET.Element:
    attrs = dict(d=d, fill=P.get(fill, fill), opacity=opacity)
    if stroke:
        attrs.update(stroke=P.get(stroke, stroke), stroke_width=width,
                     stroke_linecap="round", stroke_linejoin="round")
    if dash:
        attrs["stroke_dasharray"] = dash
    return add(parent, "path", **attrs)


def rect(parent: ET.Element, x: float, y: float, w: float, h: float,
         fill: str, stroke: str | None = None, width: float = 2,
         rx: float = 0) -> ET.Element:
    attrs = dict(x=x, y=y, width=w, height=h, fill=P.get(fill, fill))
    if stroke:
        attrs.update(stroke=P.get(stroke, stroke), stroke_width=width)
    if rx:
        attrs["rx"] = rx
    return add(parent, "rect", **attrs)


def ellipse(parent: ET.Element, cx: float, cy: float, rx: float, ry: float,
            fill: str, stroke: str | None = None, width: float = 2) -> ET.Element:
    attrs = dict(cx=cx, cy=cy, rx=rx, ry=ry, fill=P.get(fill, fill))
    if stroke:
        attrs.update(stroke=P.get(stroke, stroke), stroke_width=width)
    return add(parent, "ellipse", **attrs)


def arrow(parent: ET.Element, x1: float, y1: float, x2: float, y2: float,
          colour: str = "arrival", width: float = 3, dash: str | None = None) -> None:
    ln(parent, x1, y1, x2, y2, colour, width, dash)
    path(parent, f"M{x2-12} {y2-7} L{x2} {y2} L{x2-12} {y2+7}",
         stroke=colour, width=width)


def building(parent: ET.Element, x: float, y: float, w: float, h: float,
             depth: float = 18, roof: str = "city_top",
             face: str = "city") -> None:
    poly(parent, f"{x},{y} {x+w},{y} {x+w+depth},{y-depth} {x+depth},{y-depth}",
         roof, "city_side", 1.7)
    poly(parent, f"{x+w},{y} {x+w+depth},{y-depth} {x+w+depth},{y+h-depth} {x+w},{y+h}",
         "city_side", "city_side", 1.4)
    rect(parent, x, y, w, h, face, "city_side", 1.5)
    for xx in range(int(x+14), int(x+w-8), 26):
        for yy in range(int(y+18), int(y+h-8), 28):
            rect(parent, xx, yy, 9, 9, "white")


def seed(parent: ET.Element, x: float, y: float, scale: float = 1,
         colour: str = "arrival", rotation: float = 0) -> None:
    n = g(parent, f"seed-{int(x)}-{int(y)}")
    n.set("transform", f"translate({x} {y}) rotate({rotation}) scale({scale})")
    ellipse(n, 0, 0, 8, 4.2, colour)
    ln(n, -6, -1, -18, -12, colour, 1.6)
    for dx, dy in ((-22, -16), (-18, -20), (-13, -17)):
        ln(n, -18, -12, dx, dy, colour, 1.2)


def grass(parent: ET.Element, x: float, y: float, scale: float = 1,
          colour: str = "vegetation_dark") -> None:
    n = g(parent, f"grass-{int(x)}-{int(y)}")
    n.set("transform", f"translate({x} {y}) scale({scale})")
    for d in ("M0 0 Q-8 -23 -23 -28", "M0 0 Q-4 -30 0 -42",
              "M0 0 Q7 -26 24 -34", "M0 0 Q14 -14 28 -13"):
        path(n, d, stroke=colour, width=2.3)


def plant(parent: ET.Element, x: float, y: float, scale: float = 1,
          colour: str = "favour", kind: int = 0) -> None:
    n = g(parent, f"plant-{int(x)}-{int(y)}")
    n.set("transform", f"translate({x} {y}) scale({scale})")
    path(n, "M0 0 Q3 -36 0 -84", stroke=colour, width=3)
    if kind % 3 == 0:
        for d in ("M0 -23 Q-27 -48 -36 -35 Q-15 -19 0 -23",
                  "M1 -44 Q29 -69 39 -55 Q19 -36 1 -44",
                  "M0 -66 Q-22 -86 -28 -77 Q-18 -62 0 -66"):
            path(n, d, fill=colour)
    elif kind % 3 == 1:
        for d in ("M0 -25 Q-27 -33 -40 -15 Q-20 -11 0 -25",
                  "M1 -42 Q30 -60 43 -40 Q21 -27 1 -42",
                  "M0 -67 Q-16 -91 -26 -81 Q-16 -66 0 -67"):
            path(n, d, fill=colour)
        for xx, yy in ((-2,-88),(5,-83),(-8,-82)):
            ellipse(n, xx, yy, 5, 5, colour)
    else:
        for dx, dy in ((-30,-22),(27,-31),(-24,-53),(27,-62)):
            path(n, f"M0 {dy+9} Q{dx/2} {dy-10} {dx} {dy}", stroke=colour, width=2.2)
            ellipse(n, dx, dy, 12, 6, colour)


def panel_header(parent: ET.Element, letter: str, title: str, x: float) -> None:
    txt(parent, x, 188, letter, 42, "ink", 700)
    txt(parent, x+55, 188, title, 36, "ink", 700)


def draw_coast(root: ET.Element) -> None:
    a = g(root, "panel-a", "Coastal reclamation within an expanding city")
    panel_header(a, "a", "An expanding coast", 80)
    scene = g(a, "coastal-oblique-scene", "Oblique coastal city illustration")
    poly(scene, "80,252 965,230 965,1120 80,1105", "sea")
    for d in ("M710 274 Q820 252 944 273", "M736 1010 Q838 995 947 1024"):
        path(scene, d, stroke="sea_line", width=3)
    poly(scene, "80,255 489,241 474,1108 80,1105", "land")
    vegetation = g(scene, "terrestrial-habitat", "Surrounding terrestrial habitat")
    for x,y,s in ((129,388,.85),(128,742,.75),(130,939,.95)):
        grass(vegetation,x,y,s)
    existing = g(scene, "existing-urban-land", "Existing urban land")
    poly(existing, "177,355 486,334 475,1022 171,1030", "city_top", "city_side", 3)
    poly(existing, "171,1030 475,1022 471,1044 170,1051", "city_side")
    for x,y,w,h in ((215,418,72,83),(326,390,83,98),(203,666,83,106),
                    (326,619,96,94),(222,867,102,91)):
        building(existing,x,y,w,h,15)
    # One continuous road crosses the former shoreline into the new land.
    path(existing,"M175 562 L470 552 L893 535",stroke="white",width=21)
    path(existing,"M175 562 L470 552 L893 535",stroke="muted",width=1.6,dash="18 26")
    reclaimed = g(scene, "reclaimed-land", "New urban land added seaward")
    poly(reclaimed, "485,335 869,322 925,388 910,958 851,1012 477,1023",
         "reclaim", "reclaim_edge", 3.5)
    poly(reclaimed, "477,1023 851,1012 846,1031 473,1044", "reclaim_edge")
    # A subtle mosaic suggests possible functions without a stage sequence.
    poly(reclaimed,"501,370 660,363 651,514 498,522","reclaim_light")
    poly(reclaimed,"689,360 856,354 876,482 681,492","reclaim_light")
    poly(reclaimed,"505,609 665,603 656,783 500,788","reclaim_light")
    poly(reclaimed,"691,603 879,595 875,784 683,791","reclaim_light")
    poly(reclaimed,"506,823 855,812 848,982 497,991","reclaim_light")
    path(reclaimed,"M470 552 L893 535",stroke="white",width=21)
    path(reclaimed,"M470 552 L893 535",stroke="muted",width=1.6,dash="18 26")
    # Port: quay, containers, crane and a small vessel.
    port = g(reclaimed,"landuse-port","Container port")
    for x,y in ((538,405),(581,400),(539,438)):
        poly(port,f"{x},{y} {x+31},{y-2} {x+39},{y-12} {x+8},{y-10}",
             "white","reclaim_edge",1.5)
        rect(port,x,y,31,23,"#C9B595","reclaim_edge",1.4)
    ln(port,618,380,618,466,"reclaim_edge",3)
    ln(port,618,384,656,382,"reclaim_edge",3)
    ln(port,650,382,650,447,"reclaim_edge",2)
    poly(port,"822,376 897,374 882,395 838,398","white","reclaim_edge",2)
    txt(port,526,500,"Port",28)
    transport=g(reclaimed,"landuse-transport","Transport network")
    path(transport,"M687 398 L851 391",stroke="city_side",width=10)
    path(transport,"M687 424 L855 416",stroke="city_side",width=8)
    ln(transport,748,393,756,422,"white",2)
    txt(transport,694,472,"Transport",28)
    residential=g(reclaimed,"landuse-residential","Residential buildings")
    for x,y in ((522,681),(583,660)):
        building(residential,x,y,45,53,10,"white","city")
        poly(residential,f"{x-7},{y} {x+22},{y-25} {x+58},{y-5} {x+47},{y-5} {x+22},{y-14} {x+3},{y}",
             "#B6C2BC","city_side",1.2)
    txt(residential,518,757,"Residential",27)
    industry=g(reclaimed,"landuse-commercial","Commercial or industrial structures")
    building(industry,724,660,113,82,15,"white","city")
    ln(industry,745,643,745,674,"city_side",2)
    txt(industry,695,752,"Commercial /",25)
    txt(industry,709,780,"industrial",25)
    open_ground=g(reclaimed,"landuse-open","Open ground")
    for x,y in ((546,912),(637,944),(748,907)):
        grass(open_ground,x,y,.48,"vegetation")
    txt(open_ground,569,974,"Open ground",27)
    former=g(scene,"former-shoreline","Former shoreline")
    path(former,"M485 269 Q472 365 484 452 Q470 545 481 632 Q470 753 480 840 Q468 952 477 1067",
         stroke="ink",width=3.8,dash="12 11")
    present=g(scene,"present-shoreline","Present shoreline")
    path(present,"M870 322 L925 388 L910 958 L851 1012",stroke="reclaim_edge",width=4)
    labels=g(a,"coastal-labels","Coast and shoreline labels")
    txt(labels,94,310,"Surrounding habitat",27)
    txt(labels,219,369,"Existing urban land",28,"ink",600)
    txt(labels,638,304,"New reclaimed land",28,"ink",600)
    txt(labels,861,1078,"Sea",27,"muted")
    ln(labels,91,1165,146,1165,"ink",3,"11 10")
    txt(labels,160,1175,"Former shoreline",27)
    ln(labels,513,1165,568,1165,"reclaim_edge",3)
    txt(labels,583,1175,"Present shoreline",27)


def draw_arrival(root: ET.Element) -> None:
    b=g(root,"panel-b","Propagule arrival and habitat conditions")
    panel_header(b,"b","Arrival and establishment",1040)
    arrival=g(b,"propagule-arrival","Potential arrival through urban connections")
    txt(arrival,1075,273,"Propagule arrival",36,"arrival",700)
    txt(arrival,1075,315,"Trade, transport and human activity",28,"muted")
    # Three simple vectors refer to the three plausible arrival connections.
    ship=g(arrival,"shipping","Maritime trade and shipping")
    poly(ship,"1110,417 1248,417 1227,447 1139,447","white","arrival",3)
    rect(ship,1158,383,45,32,"white","arrival",2)
    ln(ship,1180,357,1180,382,"arrival",3)
    path(ship,"M1109 461 Q1145 452 1175 461 T1245 461",stroke="sea_line",width=2.4)
    road=g(arrival,"road-connectivity","Road and transport connectivity")
    path(road,"M1324 461 L1383 355 L1437 355 L1497 461",stroke="city_side",width=5)
    path(road,"M1350 461 L1397 379 L1422 379 L1470 461",stroke="arrival",width=3)
    ln(road,1410,383,1410,450,"arrival",2,"10 12")
    people=g(arrival,"human-movement","Human movement")
    for xx,head in ((1612,369),(1670,353),(1728,371)):
        ellipse(people,xx,head,12,12,"white","arrival",2.4)
        path(people,f"M{xx-19} 447 L{xx-12} {head+37} Q{xx} {head+24} {xx+12} {head+37} L{xx+19} 447",
             stroke="arrival",width=2.6)
    arrow(arrival,1780,412,1861,412,"arrival",2.6,"8 9")
    for x,y,r in ((1905,395,-14),(1970,412,13),(2025,383,-20),(2093,412,8)):
        seed(arrival,x,y,1,"arrival",r)
    path(arrival,"M1805 452 C1894 497 2020 501 2130 452",stroke="arrival",width=3)
    txt(arrival,1882,523,"Potential pathways",27,"muted")
    ln(b,1075,571,2189,571,"rule",2)
    txt(b,1075,640,"Habitat conditions after arrival",33,"ink",700)
    txt(b,1075,681,"The same propagules can encounter contrasting new habitats",27,"muted")
    # Two habitats receive the same stylised propagules.
    habitats=g(b,"habitat-contrast","Alternative establishment conditions")
    favourable=g(habitats,"favourable-habitat","Favourable conditions on reclaimed land")
    txt(favourable,1082,750,"Favourable habitat conditions",30,"favour",700)
    txt(favourable,1082,789,"Available space · resource opportunities",25,"muted")
    path(favourable,"M1090 925 Q1241 906 1408 923 L1592 914",stroke="favour",width=3.5)
    poly(favourable,"1090,927 1240,910 1408,925 1592,916 1592,1061 1090,1061",
         "favour_light")
    for x,y,r in ((1146,875,-16),(1380,867,18),(1525,875,-20)):
        seed(favourable,x,y,.9,"arrival",r)
    ln(favourable,1147,816,1147,854,"arrival",2,"6 7")
    path(favourable,"M1139 847 L1147 857 L1155 847",stroke="arrival",width=2)
    for x,y,s,k in ((1211,919,.72,0),(1318,916,.91,1),(1478,918,.62,2)):
        plant(favourable,x,y,s,"favour",k)
    grass(favourable,1539,922,.55,"vegetation_dark")
    restrictive=g(habitats,"restrictive-habitat","Restrictive conditions on reclaimed land")
    txt(restrictive,1683,750,"Restrictive habitat conditions",30,"restrict",700)
    txt(restrictive,1683,789,"Engineered substrate · soil and water constraints",25,"muted")
    poly(restrictive,"1690,910 2185,910 2185,958 1690,958","city_top","city_side",2.3)
    poly(restrictive,"1690,960 2185,960 2185,1061 1690,1061","restrict_light")
    for x in (1802,1962,2109):
        ln(restrictive,x,917,x+18,954,"white",3)
    for x,y,r in ((1755,878,-12),(1907,875,16),(2077,880,-19)):
        seed(restrictive,x,y,.9,"arrival",r)
    ln(restrictive,1757,816,1757,855,"arrival",2,"6 7")
    path(restrictive,"M1749 848 L1757 858 L1765 848",stroke="arrival",width=2)
    path(restrictive,"M1932 961 Q1941 934 1949 942",stroke="restrict",width=2.5)
    txt(b,1075,1131,"Arrival does not guarantee suitable habitat or establishment",30,"ink",600)


def draw_spatial(root: ET.Element) -> None:
    c=g(root,"panel-c","City-scale spatial assessment")
    panel_header(c,"c","Spatial context",2270)
    comparisons=g(c,"spatial-comparisons","Nested spatial contrasts")
    txt(comparisons,2295,280,"City versus surrounding coast",29,"ink",600)
    rect(comparisons,2300,321,301,151,"land","vegetation",2)
    path(comparisons,"M2492 323 C2451 366 2488 406 2462 470",stroke="sea_line",width=3)
    poly(comparisons,"2479,321 2600,321 2600,472 2463,472","sea")
    poly(comparisons,"2364,352 2472,346 2492,433 2370,440","city","ink",2)
    poly(comparisons,"2481,369 2495,371 2492,423 2482,422","reclaim")
    txt(comparisons,2642,371,"Nested coastal",26,"muted")
    txt(comparisons,2642,406,"assessment",26,"muted")
    ln(c,2300,504,3105,504,"rule",2)
    txt(comparisons,2295,559,"Reclaimed versus existing urban land",29,"ink",600)
    rect(comparisons,2300,600,301,150,"city","ink",2)
    poly(comparisons,"2478,602 2599,602 2599,748 2481,748","reclaim","reclaim_edge",2)
    path(comparisons,"M2480 602 L2480 747",stroke="ink",width=2.5,dash="9 9")
    txt(comparisons,2641,649,"Shared city",26,"muted")
    txt(comparisons,2641,684,"context",26,"muted")
    ln(c,2300,781,3105,781,"rule",2)
    cityscale=g(c,"city-size-and-conditions","City size and environmental context")
    txt(cityscale,2295,838,"City size and urban conditions",29,"ink",600)
    rect(cityscale,2303,889,233,156,"land","vegetation",1.5)
    rect(cityscale,2610,889,493,156,"#E4ECE7","vegetation",1.5)
    rect(cityscale,2340,929,151,86,"city","ink",2)
    rect(cityscale,2651,911,410,122,"city","ink",2)
    rect(cityscale,2443,978,40,30,"reclaim","reclaim_edge",1.5)
    rect(cityscale,3015,996,40,30,"reclaim","reclaim_edge",1.5)
    txt(cityscale,2366,1086,"Small city",26)
    txt(cityscale,2766,1086,"Large city",26)
    txt(cityscale,2299,1138,"Equal added area; different share of the city",27,"muted")


def botanical_icon(parent: ET.Element, x: int, y: int, kind: int) -> None:
    n=g(parent,f"botanical-form-{kind}","Generic alien plant silhouette")
    if kind == 0:
        plant(n,x,y,.7,"vegetation_dark",0)
    elif kind == 1:
        grass(n,x,y,1.3,"vegetation_dark")
        for dx in (-17,0,19):
            ellipse(n,x+dx,y-59,4.5,8,"vegetation_dark")
    elif kind == 2:
        plant(n,x,y,.63,"vegetation_dark",1)
        path(n,f"M{x+3} {y-46} Q{x+40} {y-68} {x+45} {y-87}",stroke="vegetation_dark",width=2.3)
    else:
        plant(n,x,y,.65,"vegetation_dark",2)
        grass(n,x+12,y,.6,"vegetation_dark")


def suitability_layer(parent: ET.Element, x: int, y: int, ident: str,
                      patches: tuple[tuple[int,int,int,int], ...]) -> None:
    n=g(parent,ident,"Species-specific predicted suitability layer")
    poly(n,f"{x},{y} {x+225},{y-17} {x+267},{y+85} {x+42},{y+101}",
         "#F5F7F4","city_side",2)
    for px,py,rx,ry in patches:
        ellipse(n,x+px,y+py,rx,ry,"favour_light")
    poly(n,f"{x+16},{y+20} {x+146},{y+11} {x+219},{y+69} {x+49},{y+81}",
         "none","muted",1.5)


def draw_multispecies(root: ET.Element) -> None:
    lower=g(root,"multispecies-and-context","Multispecies predictions and urban covariates")
    ln(lower,80,1227,3120,1227,"rule",3)
    txt(lower,80,1295,"From species-specific predictions to city patterns",33,"ink",700)
    species=g(lower,"botanical-silhouettes","Multiple unnamed alien plant forms")
    for x,kind in ((164,0),(262,1),(360,2),(461,3)):
        botanical_icon(species,x,1470,kind)
    txt(lower,104,1534,"Multiple alien plants",27,"muted")
    arrow(lower,509,1437,572,1437,"muted",2.6)
    stack=g(lower,"species-specific-layers","Species-specific suitability predictions")
    suitability_layer(stack,595,1341,"species-layer-1",((60,47,25,14),(160,42,31,18)))
    suitability_layer(stack,630,1381,"species-layer-2",((86,42,36,18),(187,50,29,12)))
    suitability_layer(stack,663,1422,"species-layer-3",((45,54,38,17),(159,34,38,19)))
    txt(lower,591,1564,"Species-specific predicted suitability",27)
    arrow(lower,980,1437,1048,1437,"muted",2.6)
    aggregate=g(lower,"aggregated-suitability","Continuous city-level suitability map")
    rect(aggregate,1075,1346,443,166,"#F4F7F5","city_side",2)
    # The gradient encodes continuous predicted suitability, not a species count.
    clip=add(aggregate,"clipPath",id="city-map-clip")
    poly(clip,"1108,1379 1409,1367 1468,1449 1168,1462","white")
    grad=rect(aggregate,1077,1347,439,164,"url(#suitability-gradient)")
    grad.set("clip-path","url(#city-map-clip)")
    poly(aggregate,"1108,1379 1409,1367 1468,1449 1168,1462","none","ink",2)
    poly(aggregate,"1364,1370 1411,1368 1468,1449 1412,1452","reclaim","reclaim_edge",2)
    txt(lower,1080,1564,"Aggregated suitability across species",27)
    txt(lower,1148,1608,"City-level suitability patterns",27,"muted")
    context=g(lower,"urban-context-variables","Wider environmental and socioeconomic context")
    ln(context,1620,1332,1620,1608,"rule",2.5)
    txt(context,1683,1347,"Wider urban context",31,"ink",700)
    txt(context,1683,1394,"Climate",27)
    txt(context,1979,1394,"Soil",27)
    txt(context,2229,1394,"Marine environment",27)
    txt(context,1683,1466,"Human activity",27)
    txt(context,1979,1466,"Reclamation extent",27)
    path(context,"M3060 1363 H3082 V1481 H3060",stroke="muted",width=2)
    ln(context,3082,1481,3082,1513,"muted",2)
    for x1,x2,y in ((1683,3101,1516),(1683,3101,1558)):
        ln(context,x1,y,x2,y,"rule",2)
    txt(context,1683,1545,"Associations with predicted suitability",29,"muted",600)
    txt(context,1683,1604,"Across cities, without assuming causal effects",26,"muted")


def draw_footer(root: ET.Element) -> None:
    foot=g(root,"sustainable-development-strip","Subordinate planning and surveillance motivation")
    ln(foot,80,1687,3120,1687,"rule",3)
    txt(foot,80,1747,"Evidence for sustainable coastal planning and invasion surveillance",30,"ink",600)
    txt(foot,80,1793,"SDG 11.3 · Sustainable urban planning",25,"muted")
    txt(foot,1051,1793,"SDG 15.8 · Invasive alien species management",25,"muted")
    txt(foot,2398,1793,"Practical relevance, not SDG measurement",23,"muted")


def build() -> ET.Element:
    root=ET.Element(f"{{{SVG}}}svg",{
        "viewBox":"0 0 3200 1850", "width":"200mm", "height":"115.625mm",
        "version":"1.1", "role":"img",
        "aria-label":"Conceptual framework for coastal reclamation, propagule arrival and predicted invasive-plant suitability",
    })
    defs=add(root,"defs")
    gradient=add(defs,"linearGradient",id="suitability-gradient",x1="0%",x2="100%",y1="70%",y2="20%")
    for pos,col in (("0%","#F4E8D4"),("44%","#A8CFBE"),("100%","#3C8179")):
        add(gradient,"stop",offset=pos,stop_color=col)
    rect(root,0,0,3200,1850,"white")
    title=g(root,"removable-title","Removable figure title")
    txt(title,80,88,"New coastal land does not necessarily create suitable habitat",43,"ink",700)
    txt(title,80,132,"Urban connections, establishment conditions and city-level context",29,"muted")
    ln(root,80,217,3120,217,"rule",2)
    draw_coast(root)
    draw_arrival(root)
    draw_spatial(root)
    draw_multispecies(root)
    draw_footer(root)
    return root


def validate(root: ET.Element) -> list[str]:
    nodes=list(root.iter())
    ids=[n.get("id") for n in nodes if n.get("id")]
    assert len(ids)==len(set(ids)),"Duplicate SVG IDs"
    assert not any(n.tag.endswith("}image") or n.tag.endswith("}foreignObject")
                   for n in nodes),"Raster or foreign content found"
    assert not any(n.tag.endswith("}use") for n in nodes),"External references found"
    required={"panel-a","panel-b","panel-c","former-shoreline","present-shoreline",
              "propagule-arrival","favourable-habitat","restrictive-habitat",
              "botanical-silhouettes","species-specific-layers",
              "aggregated-suitability","urban-context-variables",
              "sustainable-development-strip"}
    assert required.issubset(ids),required-set(ids)
    labels=" ".join(n.text or "" for n in nodes if n.tag.endswith("}text"))
    for phrase in ("Propagule arrival","Favourable habitat conditions",
                   "Restrictive habitat conditions","City versus surrounding coast",
                   "Species-specific predicted suitability","Aggregated suitability across species",
                   "SDG 11.3","SDG 15.8"):
        assert phrase in labels,phrase
    return ["SVG XML: valid",f"Unique IDs: {len(ids)}",
            "Raster, foreignObject and external use elements: 0",
            f"Editable text elements: {sum(n.tag.endswith('}text') for n in nodes)}",
            "Required semantic layers and message labels: present"]


def main() -> None:
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir",type=Path,required=True)
    args=parser.parse_args()
    figures=args.output_dir/"figures"
    qc=args.output_dir/"qc"
    figures.mkdir(parents=True,exist_ok=True)
    qc.mkdir(parents=True,exist_ok=True)
    root=build()
    checks=validate(root)
    data=ET.tostring(root,encoding="utf-8",xml_declaration=True)
    svg=figures/"introduction_reclamation_ecological_framework.svg"
    png=figures/"introduction_reclamation_ecological_framework_preview.png"
    svg.write_bytes(data)
    ET.parse(svg)
    cairosvg.svg2png(bytestring=data,write_to=str(png),
                     output_width=4800,output_height=2775)
    checks.append("Preview: rendered in Python at 4800 x 2775 pixels")
    (qc/"svg_validation_ecological_framework.txt").write_text(
        "\n".join(checks)+"\n",encoding="utf-8")
    print(svg)
    print(png)


if __name__=="__main__":
    main()
