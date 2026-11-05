"""Draw an editable conceptual introduction figure as native SVG elements."""

from __future__ import annotations

import argparse
from pathlib import Path
from xml.etree import ElementTree as ET

import cairosvg


SVG = "http://www.w3.org/2000/svg"
INK = "http://www.inkscape.org/namespaces/inkscape"
ET.register_namespace("", SVG)
ET.register_namespace("inkscape", INK)

INK = "http://www.inkscape.org/namespaces/inkscape"
C = {
    "ink": "#27373C",
    "muted": "#617077",
    "hair": "#C6D0D0",
    "sea": "#DFEFF3",
    "sea_line": "#88B4C1",
    "coast": "#E5EFE4",
    "coast_deep": "#B8CEB6",
    "urban": "#CCD2D0",
    "urban_deep": "#9EA9A6",
    "reclaimed": "#E8C789",
    "reclaimed_deep": "#C39546",
    "teal": "#2A7777",
    "teal_pale": "#E7F2F0",
    "rust": "#B36A4D",
    "rust_pale": "#F6ECE6",
    "white": "#FFFFFF",
}


def el(parent: ET.Element, tag: str, **attrs: object) -> ET.Element:
    return ET.SubElement(parent, f"{{{SVG}}}{tag}", {k.replace("_", "-"): str(v) for k, v in attrs.items()})


def group(parent: ET.Element, ident: str, label: str | None = None, layer: bool = False) -> ET.Element:
    attrs = {"id": ident}
    if layer:
        attrs[f"{{{INK}}}groupmode"] = "layer"
        attrs[f"{{{INK}}}label"] = label or ident
    return ET.SubElement(parent, f"{{{SVG}}}g", attrs)


def path(parent: ET.Element, d: str, *, fill: str = "none", stroke: str = "none", width: float = 1, **kw: object) -> ET.Element:
    return el(parent, "path", d=d, fill=fill, stroke=stroke, stroke_width=width, **kw)


def line(parent: ET.Element, x1: float, y1: float, x2: float, y2: float, *, stroke: str = C["ink"], width: float = 2, **kw: object) -> ET.Element:
    return el(parent, "line", x1=x1, y1=y1, x2=x2, y2=y2, stroke=stroke, stroke_width=width, **kw)


def rect(parent: ET.Element, x: float, y: float, w: float, h: float, *, fill: str = "none", stroke: str = "none", width: float = 1, rx: float = 0, **kw: object) -> ET.Element:
    return el(parent, "rect", x=x, y=y, width=w, height=h, rx=rx, fill=fill, stroke=stroke, stroke_width=width, **kw)


def circle(parent: ET.Element, x: float, y: float, r: float, *, fill: str = "none", stroke: str = "none", width: float = 1, **kw: object) -> ET.Element:
    return el(parent, "circle", cx=x, cy=y, r=r, fill=fill, stroke=stroke, stroke_width=width, **kw)


def text(parent: ET.Element, x: float, y: float, value: str, *, size: float = 26, weight: str = "normal", fill: str = C["ink"], anchor: str = "start", **kw: object) -> ET.Element:
    node = el(parent, "text", x=x, y=y, fill=fill, font_family="Arial, Helvetica, sans-serif", font_size=size, font_weight=weight, text_anchor=anchor, **kw)
    node.text = value
    return node


def multiline(parent: ET.Element, x: float, y: float, values: tuple[str, ...], *, size: float = 25, leading: float = 32, fill: str = C["ink"], weight: str = "normal") -> None:
    node = text(parent, x, y, "", size=size, fill=fill, weight=weight)
    for i, value in enumerate(values):
        span = el(node, "tspan", x=x, dy=0 if i == 0 else leading)
        span.text = value


def plant(parent: ET.Element, x: float, y: float, scale: float, colour: str, sparse: bool = False) -> None:
    line(parent, x, y, x, y - 33 * scale, stroke=colour, width=2.3 * scale, stroke_linecap="round")
    path(parent, f"M{x},{y-19*scale} C{x-22*scale},{y-34*scale} {x-27*scale},{y-8*scale} {x},{y-19*scale}", fill=colour)
    if not sparse:
        path(parent, f"M{x},{y-26*scale} C{x+20*scale},{y-43*scale} {x+28*scale},{y-18*scale} {x},{y-26*scale}", fill=colour)


def arrow(parent: ET.Element, x1: float, y1: float, x2: float, y2: float, *, colour: str = C["ink"], dashed: bool = False, width: float = 2) -> None:
    attrs = {"marker_end": "url(#arrow-small)", "stroke_dasharray": "8 7"} if dashed else {"marker_end": "url(#arrow-small)"}
    line(parent, x1, y1, x2, y2, stroke=colour, width=width, **attrs)


def build() -> ET.Element:
    root = ET.Element(f"{{{SVG}}}svg", {"width": "180mm", "height": "124mm", "viewBox": "0 0 1800 1240", "version": "1.1", "role": "img", "aria-label": "Conceptual framework for coastal reclamation and predicted invasive-plant suitability"})
    defs = el(root, "defs")
    marker = el(defs, "marker", id="arrow-small", markerWidth=10, markerHeight=10, refX=8, refY=5, orient="auto", markerUnits="strokeWidth")
    path(marker, "M1 1 L9 5 L1 9 Z", fill=C["ink"])
    hatch = el(defs, "pattern", id="reclaimed-hatch", patternUnits="userSpaceOnUse", width=22, height=22, patternTransform="rotate(35)")
    line(hatch, 0, 0, 0, 22, stroke=C["reclaimed_deep"], width=1.8, opacity=0.45)
    rect(root, 0, 0, 1800, 1240, fill=C["white"])

    title = group(root, "figure-title", "Removable figure title", True)
    text(title, 72, 75, "From coastal land creation to invasive-plant suitability", size=43, weight="bold")
    line(root, 72, 103, 1728, 103, stroke=C["hair"], width=2)
    text(root, 1728, 132, "Does creating urban land also create suitable habitat for invasive plants?", size=24, anchor="end", fill=C["muted"])

    a = group(root, "panel-a", "a Coastal urban expansion", True)
    text(a, 72, 160, "a", size=34, weight="bold")
    text(a, 109, 160, "Coastal urban expansion", size=31, weight="bold")
    # The hero illustration shows contiguous city, reclaimed land and sea.
    ill = group(a, "coastline", "Coastal landscape and land transformation")
    path(ill, "M80 192 H970 C944 239 957 273 943 310 C962 346 950 401 974 443 H80 Z", fill=C["coast"])
    path(ill, "M970 192 H1720 V443 H974 C950 401 962 346 943 310 C957 273 944 239 970 192 Z", fill=C["sea"])
    # Restrained topographic lines make the surroundings read as land.
    for d in ("M123 278 C212 231 308 246 395 275", "M96 377 C207 338 299 351 421 382", "M118 409 C212 394 291 408 388 423"):
        path(ill, d, stroke=C["coast_deep"], width=2, opacity=0.65)
    urban = group(ill, "existing-urban-land", "Existing urban land")
    path(urban, "M444 231 L949 231 C942 258 958 278 943 310 C960 343 947 399 968 434 L445 434 Z", fill=C["urban"])
    for x, y, w, h in ((490, 252, 74, 43), (588, 264, 110, 48), (740, 244, 77, 55), (830, 301, 77, 40), (510, 333, 106, 66), (653, 344, 70, 48), (760, 369, 107, 42)):
        rect(urban, x, y, w, h, fill=C["urban_deep"], opacity=0.6, rx=2)
    line(urban, 478, 319, 945, 319, stroke=C["white"], width=10, opacity=0.8)
    line(urban, 647, 242, 647, 423, stroke=C["white"], width=8, opacity=0.75)
    reclaimed = group(ill, "reclaimed-land", "Newly reclaimed land")
    reclamation_d = "M950 275 L1200 275 L1200 307 L1292 307 L1292 425 L1171 425 L1171 407 L968 407 C949 377 960 344 943 310 C953 294 947 284 950 275 Z"
    path(reclaimed, reclamation_d, fill=C["reclaimed"], stroke=C["reclaimed_deep"], width=2)
    path(reclaimed, reclamation_d, fill="url(#reclaimed-hatch)")
    line(reclaimed, 978, 326, 1238, 326, stroke=C["white"], width=8, opacity=0.78)
    for x, y in ((1022, 366), (1079, 377), (1140, 362)):
        plant(reclaimed, x, y, 0.5, C["teal"], sparse=True)
    former = group(ill, "former-shoreline", "Former shoreline")
    path(former, "M950 240 C935 274 966 287 943 310 C960 343 947 399 968 433", stroke=C["ink"], width=3, stroke_dasharray="12 9")
    bound = group(ill, "city-boundary", "Study-city boundary")
    path(bound, "M439 222 H978 L1298 269 L1310 434 H438 Z", stroke=C["ink"], width=3.1, stroke_linejoin="round")
    labels = group(a, "panel-a-labels", "Coast labels")
    text(labels, 124, 258, "Surrounding landscape", size=26)
    text(labels, 568, 295, "Existing urban land", size=27)
    text(labels, 1037, 303, "Reclaimed land", size=27, weight="bold")
    text(labels, 1384, 318, "Sea", size=28, fill=C["muted"])
    text(labels, 979, 267, "Former shoreline", size=23)
    # Transport and propagule icons remain contextual, not a measured pathway.
    intro = group(ill, "introduction-context", "Theoretical introduction opportunities")
    path(intro, "M1370 377 L1469 377 L1452 395 L1393 395 Z", fill=C["white"], stroke=C["ink"], width=2)
    rect(intro, 1400, 348, 42, 27, fill=C["white"], stroke=C["ink"], width=2)
    line(intro, 1365, 403, 1475, 403, stroke=C["sea_line"], width=2)
    circle(intro, 1330, 326, 4, fill=C["teal"])
    circle(intro, 1350, 310, 3, fill=C["teal"])
    circle(intro, 1361, 329, 3, fill=C["teal"])
    path(intro, "M1321 339 C1338 322 1347 314 1362 311", stroke=C["teal"], width=1.8, stroke_dasharray="5 7")
    text(intro, 1490, 381, "Introduction opportunities", size=24, fill=C["muted"])
    legend = group(a, "legend", "Line-style key")
    line(legend, 1395, 221, 1460, 221, stroke=C["ink"], width=3)
    text(legend, 1472, 229, "City boundary", size=22)
    line(legend, 1395, 254, 1460, 254, stroke=C["ink"], width=3, stroke_dasharray="10 7")
    text(legend, 1472, 262, "Former shoreline", size=22)
    compare = group(a, "spatial-comparisons", "Nested spatial comparisons")
    # Bracket one contrasts the whole city with its neighbouring land.
    line(compare, 130, 478, 418, 478, stroke=C["coast_deep"], width=5)
    line(compare, 440, 478, 1307, 478, stroke=C["ink"], width=5)
    for x in (130, 418, 440, 1307):
        line(compare, x, 468, x, 490, stroke=C["ink"], width=2)
    text(compare, 1345, 486, "City vs surrounding landscape", size=25)
    # Bracket two divides the same study city into its non-reclaimed and reclaimed parts.
    line(compare, 446, 523, 944, 523, stroke=C["urban_deep"], width=5)
    line(compare, 963, 523, 1291, 523, stroke=C["reclaimed_deep"], width=5)
    for x in (446, 944, 963, 1291):
        line(compare, x, 513, x, 535, stroke=C["ink"], width=2)
    multiline(compare, 1345, 519, ("Reclaimed vs non-reclaimed", "land within cities"), size=23, leading=27)

    b = group(root, "panel-b", "b Alternative habitat expectations", True)
    line(root, 72, 579, 1728, 579, stroke=C["hair"], width=2)
    text(b, 72, 630, "b", size=34, weight="bold")
    text(b, 109, 630, "What conditions does new land create?", size=30, weight="bold")
    # A subtle link identifies the magnified patch as reclaimed land from panel a.
    path(b, "M1084 439 C1088 496 877 511 839 557 L839 669 H455 V677", stroke=C["reclaimed_deep"], width=2.2, stroke_dasharray="8 9", opacity=0.8)
    circle(b, 455, 696, 20, fill=C["reclaimed"], stroke=C["reclaimed_deep"], width=2)
    plant(b, 455, 702, 0.38, C["teal"])
    text(b, 486, 704, "Reclaimed habitat", size=23, fill=C["muted"])
    h1 = group(b, "hypothesis-opportunity", "H1 habitat opportunities")
    h2 = group(b, "hypothesis-constraints", "H2 environmental constraints")
    path(b, "M455 717 C390 743 309 752 255 782", stroke=C["teal"], width=2.5, stroke_dasharray="8 8")
    path(b, "M455 717 C514 743 591 752 654 782", stroke=C["rust"], width=2.5, stroke_dasharray="8 8")
    line(b, 458, 777, 458, 1067, stroke=C["hair"], width=2)
    line(h1, 93, 796, 424, 796, stroke=C["teal"], width=4)
    text(h1, 93, 834, "H1  Habitat opportunities", size=27, weight="bold")
    rect(h1, 99, 873, 315, 72, fill=C["teal_pale"], rx=5)
    plant(h1, 151, 933, 0.9, C["teal"])
    plant(h1, 227, 933, 0.63, C["teal"], sparse=True)
    plant(h1, 305, 933, 0.75, C["teal"])
    circle(h1, 364, 892, 8, fill=C["sea_line"])
    text(h1, 96, 980, "Resource availability", size=24)
    text(h1, 96, 1014, "Space for establishment", size=24)
    text(h1, 96, 1051, "Higher suitability if dominant", size=23, weight="bold", fill=C["teal"])
    line(h2, 480, 796, 821, 796, stroke=C["rust"], width=4)
    text(h2, 480, 834, "H2  Environmental constraints", size=27, weight="bold")
    rect(h2, 487, 873, 320, 72, fill=C["rust_pale"], rx=5)
    for x, y, w in ((509, 912, 58), (576, 890, 66), (656, 911, 69), (740, 892, 51)):
        path(h2, f"M{x} {y+22} L{x+10} {y} L{x+w-12} {y+2} L{x+w} {y+25} Z", fill=C["urban_deep"], stroke=C["rust"], width=1.5)
    text(h2, 483, 980, "Substrate constraints", size=24)
    text(h2, 483, 1014, "Soil and water conditions", size=24)
    text(h2, 483, 1051, "Lower suitability if dominant", size=23, weight="bold", fill=C["rust"])

    c = group(root, "panel-c", "c Spatial predictions and tests", True)
    line(root, 873, 592, 873, 1081, stroke=C["hair"], width=2)
    text(c, 904, 630, "c", size=34, weight="bold")
    text(c, 941, 630, "Does the relationship depend on urban context?", size=29, weight="bold")
    local = group(c, "within-city-contrast", "Within-city suitability contrast")
    text(local, 906, 682, "Within-city contrast", size=25, weight="bold")
    path(local, "M912 700 H1278 V790 H912 Z", fill="none", stroke=C["ink"], width=2.4)
    rect(local, 929, 722, 134, 43, fill=C["reclaimed"], stroke=C["reclaimed_deep"], width=2)
    rect(local, 1102, 722, 152, 43, fill=C["urban"], stroke=C["urban_deep"], width=2)
    text(local, 995, 779, "Reclaimed", size=21, anchor="middle")
    text(local, 1178, 779, "Non-reclaimed", size=21, anchor="middle")
    equation = text(local, 1301, 728, "", size=27, weight="bold")
    el(equation, "tspan").text = "Δ"
    sub = el(equation, "tspan", baseline_shift="sub", font_size=19)
    sub.text = "local"
    el(equation, "tspan").text = " = S"
    sub = el(equation, "tspan", baseline_shift="sub", font_size=19)
    sub.text = "reclaimed"
    el(equation, "tspan").text = " − S"
    sub = el(equation, "tspan", baseline_shift="sub", font_size=19)
    sub.text = "non-reclaimed"
    text(local, 1301, 766, "Δ > 0  Opportunity expectation", size=22, fill=C["teal"])
    text(local, 1301, 792, "Δ < 0  Constraint expectation", size=22, fill=C["rust"])
    line(c, 906, 816, 1714, 816, stroke=C["hair"], width=1.7)
    context = group(c, "hypothesis-context", "H3 urban-context dependence")
    text(context, 906, 857, "H3  Urban-context dependence", size=26, weight="bold")
    # Both ochre patches have exactly the same 42 × 29 SVG-unit footprint.
    rect(context, 925, 891, 132, 109, fill=C["urban"], stroke=C["ink"], width=2)
    rect(context, 994, 966, 42, 29, fill=C["reclaimed"], stroke=C["reclaimed_deep"], width=1.5)
    rect(context, 1103, 891, 252, 109, fill=C["urban"], stroke=C["ink"], width=2)
    rect(context, 1292, 966, 42, 29, fill=C["reclaimed"], stroke=C["reclaimed_deep"], width=1.5)
    text(context, 991, 1027, "Small city", size=22, anchor="middle")
    text(context, 1229, 1027, "Large city", size=22, anchor="middle")
    text(context, 906, 1067, "Same land addition, different urban context", size=22, fill=C["muted"])
    graph = group(c, "conceptual-predictions", "Conceptual relationship alternatives")
    text(graph, 1438, 858, "Conceptual predictions", size=23, weight="bold")
    line(graph, 1466, 997, 1704, 997, stroke=C["ink"], width=2)
    line(graph, 1466, 997, 1466, 890, stroke=C["ink"], width=2)
    path(graph, "M1481 975 C1540 953 1607 934 1693 914", stroke=C["muted"], width=2.4)
    path(graph, "M1481 976 C1548 971 1614 951 1693 932", stroke=C["teal"], width=2.4, stroke_dasharray="8 5")
    path(graph, "M1481 976 C1548 942 1614 911 1693 899", stroke=C["rust"], width=2.4, stroke_dasharray="3 6")
    text(graph, 1584, 1031, "Reclamation extent", size=21, anchor="middle")
    text(graph, 1466, 886, "Predicted suitability", size=19)
    line(graph, 1450, 1060, 1490, 1060, stroke=C["muted"], width=2.3)
    text(graph, 1498, 1067, "Shared", size=19)
    line(graph, 1580, 1055, 1615, 1055, stroke=C["teal"], width=2.3, stroke_dasharray="8 5")
    line(graph, 1580, 1065, 1615, 1065, stroke=C["rust"], width=2.3, stroke_dasharray="3 6")
    text(graph, 1621, 1067, "City-dependent", size=19)

    tests = group(root, "study-tests", "Study tests", True)
    line(tests, 72, 1101, 1728, 1101, stroke=C["hair"], width=2)
    text(tests, 72, 1140, "Study tests", size=25, weight="bold")
    entries = ((272, 405, "Nested spatial contrasts"), (693, 463, "Reclamation × city-size association"), (1172, 553, "Matched FULL and environment-only predictions"))
    for x, w, _ in entries:
        line(tests, x, 1165, x + w, 1165, stroke=C["hair"], width=2)
    labels = group(tests, "study-test-labels", "Study-test labels")
    for x, w, label in entries:
        text(labels, x + w / 2, 1153, label, size=22, anchor="middle")
    text(tests, 72, 1210, "Predicted invasive-plant suitability", size=24, fill=C["ink"], weight="bold")
    text(tests, 1726, 1210, "Conceptual framework", size=21, fill=C["muted"], anchor="end")
    return root


def validate(root: ET.Element) -> list[str]:
    nodes = list(root.iter())
    ids = [n.attrib["id"] for n in nodes if "id" in n.attrib]
    assert len(ids) == len(set(ids)), "Duplicate SVG IDs"
    assert not any(n.tag.endswith("}image") for n in nodes), "Raster image element found"
    assert not any(n.tag.endswith("}foreignObject") for n in nodes), "Foreign object found"
    assert sum(n.tag.endswith("}text") for n in nodes) >= 35, "Expected editable labels"
    required = {"figure-title", "panel-a", "panel-b", "panel-c", "city-boundary", "former-shoreline", "reclaimed-land", "hypothesis-opportunity", "hypothesis-constraints", "hypothesis-context", "spatial-comparisons", "study-tests"}
    assert required.issubset(ids), required - set(ids)
    return [f"SVG XML: valid", f"Unique IDs: {len(ids)}", "Raster image elements: 0", "Editable text elements: " + str(sum(n.tag.endswith("}text") for n in nodes)), "Required semantic groups: present"]


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", type=Path, required=True, help="Task output directory")
    args = parser.parse_args()
    figure_dir = args.output_dir / "figures"
    qc_dir = args.output_dir / "qc"
    figure_dir.mkdir(parents=True, exist_ok=True)
    qc_dir.mkdir(parents=True, exist_ok=True)
    root = build()
    checks = validate(root)
    data = ET.tostring(root, encoding="utf-8", xml_declaration=True)
    svg_path = figure_dir / "introduction_conceptual_framework.svg"
    png_path = figure_dir / "introduction_conceptual_framework_preview.png"
    svg_path.write_bytes(data)
    ET.parse(svg_path)
    cairosvg.svg2png(bytestring=data, write_to=str(png_path), output_width=3600, output_height=2480)
    checks.extend(["Preview: rendered from SVG in Python at 3600 × 2480 pixels", "Visual inspection: pending"])
    (qc_dir / "svg_validation_introduction_conceptual_framework.txt").write_text("\n".join(checks) + "\n", encoding="utf-8")
    print(svg_path)
    print(png_path)


if __name__ == "__main__":
    main()
