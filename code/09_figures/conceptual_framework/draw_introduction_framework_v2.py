"""Draw a restrained, editable coastal-reclamation introduction schematic."""

from __future__ import annotations

import argparse
from pathlib import Path
from xml.etree import ElementTree as ET

import cairosvg

from draw_introduction_framework import (
    SVG,
    INK,
    circle,
    el,
    group,
    line,
    path,
    rect,
    text,
)


P = {
    "ink": "#26343A",
    "soft": "#637278",
    "rule": "#CFD7D4",
    "land": "#ECF1EA",
    "land_edge": "#BCD0BE",
    "sea": "#E6F0F1",
    "sea_line": "#B8D0D3",
    "city": "#C9D0CE",
    "city_block": "#A6B2AF",
    "sand": "#D9B878",
    "sand_line": "#B69150",
    "teal": "#4D8179",
    "rust": "#A96D56",
    "white": "#FFFFFF",
}


def t(parent: ET.Element, x: float, y: float, s: str, *, size: float = 24, weight: str = "normal", fill: str | None = None, anchor: str = "start", **kw: object) -> ET.Element:
    return text(parent, x, y, s, size=size, weight=weight, fill=fill or P["ink"], anchor=anchor, **kw)


def gline(parent: ET.Element, d: str, *, stroke: str | None = None, width: float = 2, dash: str | None = None, opacity: float = 1) -> None:
    attrs = {"opacity": opacity}
    if dash:
        attrs["stroke_dasharray"] = dash
    path(parent, d, stroke=stroke or P["ink"], width=width, **attrs)


def land_polygon(parent: ET.Element, d: str, fill: str, stroke: str | None = None, width: float = 1.8) -> None:
    path(parent, d, fill=fill, stroke=stroke or fill, width=width, stroke_linejoin="round")


def build() -> ET.Element:
    root = ET.Element(
        f"{{{SVG}}}svg",
        {
            "width": "180mm",
            "height": "118mm",
            "viewBox": "0 0 1800 1180",
            "version": "1.1",
            "role": "img",
            "aria-label": "Conceptual coastal land-creation and invasive-plant suitability figure",
        },
    )
    defs = el(root, "defs")
    marker = el(defs, "marker", id="fine-arrow", markerWidth=8, markerHeight=8, refX=7, refY=4, orient="auto", markerUnits="strokeWidth")
    path(marker, "M1 1 L7 4 L1 7", stroke=P["ink"], width=1.5)
    rect(root, 0, 0, 1800, 1180, fill=P["white"])

    title = group(root, "figure-title", "Removable title", True)
    t(title, 72, 69, "From coastal land creation to invasive-plant suitability", size=37, weight="bold")
    t(title, 72, 107, "Does creating urban land also create suitable habitat for invasive plants?", size=23, fill=P["soft"])
    line(root, 72, 134, 1728, 134, stroke=P["rule"], width=1.8)

    # The coastal drawing is the visual anchor; the other panels remain subordinate.
    a = group(root, "panel-a", "a Coastal urban expansion", True)
    t(a, 72, 178, "a", size=30, weight="bold")
    t(a, 111, 178, "Coastal urban expansion", size=27, weight="bold")
    coast = group(a, "coastline", "Coastal plan view")
    land_d = "M74 218 H684 C657 274 690 300 668 355 C687 397 650 442 673 496 C650 552 683 588 662 651 C691 702 660 762 677 790 H74 Z"
    sea_d = "M684 218 H1041 V790 H677 C660 762 691 702 662 651 C683 588 650 552 673 496 C650 442 687 397 668 355 C690 300 657 274 684 218 Z"
    land_polygon(coast, land_d, P["land"])
    land_polygon(coast, sea_d, P["sea"])
    for d in (
        "M97 298 C191 262 258 268 331 292",
        "M89 353 C180 321 271 332 341 355",
        "M95 708 C180 671 250 681 323 706",
        "M785 282 C864 269 951 273 1021 288",
        "M839 319 C906 315 970 320 1024 330",
        "M865 738 C924 746 978 743 1020 731",
    ):
        gline(coast, d, stroke=P["land_edge"] if d.startswith("M9") else P["sea_line"], width=2, opacity=0.75)

    existing = group(coast, "existing-urban-land", "Existing city")
    existing_d = "M309 283 L645 283 C659 312 647 333 661 355 C647 397 670 438 653 496 C669 537 652 587 651 650 C661 696 650 735 660 761 L313 761 C328 718 305 691 319 647 C300 603 315 559 304 524 C313 483 299 434 315 397 C304 357 315 324 309 283 Z"
    land_polygon(existing, existing_d, P["city"])
    for x, y, w, h in ((357, 327, 86, 50), (477, 334, 115, 59), (359, 440, 122, 72), (516, 451, 90, 48), (355, 580, 93, 61), (479, 565, 115, 73), (382, 683, 83, 42), (500, 679, 90, 37)):
        rect(existing, x, y, w, h, fill=P["city_block"], opacity=0.54, rx=2)
    gline(existing, "M342 411 H644", stroke=P["white"], width=8, opacity=0.8)
    gline(existing, "M460 296 V740", stroke=P["white"], width=8, opacity=0.8)
    gline(existing, "M342 550 H633", stroke=P["white"], width=6, opacity=0.7)

    reclaim = group(coast, "reclaimed-land", "Newly reclaimed patch")
    reclaimed_d = "M661 423 L834 423 L834 455 L900 455 L900 708 L837 708 L837 741 L668 741 C649 716 666 686 651 650 C667 594 650 548 673 496 C663 470 654 449 661 423 Z"
    land_polygon(reclaim, reclaimed_d, P["sand"], P["sand_line"], 2.6)
    # Sparse, editable linework suggests engineered parcels without a mechanistic cartoon.
    for d in ("M706 454 V712", "M763 454 V714", "M825 478 V698", "M674 548 H865", "M678 631 H876"):
        gline(reclaim, d, stroke=P["white"], width=3, opacity=0.66)
    line(reclaim, 844, 474, 971, 474, stroke=P["sand_line"], width=4)
    line(reclaim, 971, 474, 971, 509, stroke=P["sand_line"], width=4)
    circle(reclaim, 936, 450, 2.7, fill=P["soft"])
    circle(reclaim, 956, 435, 2.3, fill=P["soft"])
    group(coast, "introduction-context", "Port and transport context")

    former = group(coast, "former-shoreline", "Dashed former shoreline")
    gline(former, "M655 302 C679 331 652 345 668 355 C687 397 650 442 673 496 C650 552 683 588 662 651 C690 706 660 740 670 760", width=3, dash="11 10")
    bound = group(coast, "city-boundary", "Study-city boundary")
    gline(bound, "M291 264 H655 C707 289 740 367 855 406 L919 432 V768 H296 Z", width=3.2)

    labels = group(a, "panel-a-labels", "Direct coastal labels")
    t(labels, 108, 275, "Surrounding", size=25)
    t(labels, 108, 305, "landscape", size=25)
    t(labels, 361, 477, "Existing urban land", size=25)
    t(labels, 712, 589, "Reclaimed land", size=26, weight="bold")
    t(labels, 916, 310, "Sea", size=25, fill=P["soft"])
    t(labels, 701, 373, "Former shoreline", size=21)
    gline(labels, "M698 380 L672 405", stroke=P["soft"], width=1.5)

    legend = group(a, "legend", "Boundary key")
    line(legend, 104, 824, 165, 824, stroke=P["ink"], width=3)
    t(legend, 178, 833, "City boundary", size=20)
    line(legend, 375, 824, 436, 824, stroke=P["ink"], width=3, stroke_dasharray="10 8")
    t(legend, 449, 833, "Former shoreline", size=20)
    compare = group(a, "spatial-comparisons", "Nested spatial comparisons")
    t(compare, 94, 892, "City vs surrounding landscape", size=22)
    line(compare, 112, 910, 296, 910, stroke=P["land_edge"], width=4)
    line(compare, 314, 910, 905, 910, stroke=P["ink"], width=4)
    for x in (112, 296, 314, 905):
        line(compare, x, 902, x, 919, stroke=P["ink"], width=1.7)
    t(compare, 94, 960, "Reclaimed vs non-reclaimed urban land", size=22)
    line(compare, 315, 978, 651, 978, stroke=P["city_block"], width=4)
    line(compare, 669, 978, 904, 978, stroke=P["sand_line"], width=4)
    for x in (315, 651, 669, 904):
        line(compare, x, 970, x, 987, stroke=P["ink"], width=1.7)

    line(root, 1069, 167, 1069, 1010, stroke=P["rule"], width=1.7)
    b = group(root, "panel-b", "b Alternative expectations", True)
    t(b, 1109, 178, "b", size=30, weight="bold")
    t(b, 1148, 178, "Alternative expectations", size=27, weight="bold")
    t(b, 1110, 223, "Same new land", size=22, fill=P["soft"])
    base = group(b, "reclaimed-habitat", "Shared reclaimed setting")
    land_polygon(base, "M1120 264 H1692 V322 H1120 Z", P["sand"], P["sand_line"], 2)
    for x in (1185, 1255, 1325, 1395, 1465, 1535, 1605):
        line(base, x, 273, x, 314, stroke=P["white"], width=2, opacity=0.65)
    gline(b, "M1406 322 V359 M1406 359 H1229 V386 M1406 359 H1582 V386", stroke=P["soft"], width=1.7, dash="7 7")
    h1 = group(b, "hypothesis-opportunity", "H1 higher suitability")
    h2 = group(b, "hypothesis-constraints", "H2 lower suitability")
    circle(h1, 1229, 386, 5, fill=P["teal"])
    circle(h2, 1582, 386, 5, fill=P["rust"])
    t(h1, 1120, 438, "H1", size=23, weight="bold", fill=P["teal"])
    t(h1, 1120, 473, "Higher suitability", size=27)
    t(h1, 1120, 506, "if opportunities dominate", size=21, fill=P["soft"])
    t(h2, 1450, 438, "H2", size=23, weight="bold", fill=P["rust"])
    t(h2, 1450, 473, "Lower suitability", size=27)
    t(h2, 1450, 506, "if constraints dominate", size=21, fill=P["soft"])
    line(b, 1108, 551, 1728, 551, stroke=P["rule"], width=1.7)

    c = group(root, "panel-c", "c Spatial predictions", True)
    t(c, 1109, 607, "c", size=30, weight="bold")
    t(c, 1148, 607, "Spatial predictions", size=27, weight="bold")
    local = group(c, "within-city-contrast", "Local suitability comparison")
    t(local, 1110, 653, "Within the same city", size=22, fill=P["soft"])
    rect(local, 1122, 676, 235, 92, stroke=P["ink"], width=2, rx=2)
    rect(local, 1142, 699, 86, 39, fill=P["sand"], stroke=P["sand_line"], width=1.7)
    rect(local, 1250, 699, 87, 39, fill=P["city"], stroke=P["city_block"], width=1.7)
    t(local, 1185, 758, "New land", size=18, anchor="middle")
    t(local, 1294, 758, "Other urban", size=18, anchor="middle")
    equation = t(local, 1390, 714, "", size=25, weight="bold")
    el(equation, "tspan").text = "Δ"
    el(equation, "tspan", baseline_shift="sub", font_size=17).text = "local"
    el(equation, "tspan").text = " = S"
    el(equation, "tspan", baseline_shift="sub", font_size=17).text = "new"
    el(equation, "tspan").text = " − S"
    el(equation, "tspan", baseline_shift="sub", font_size=17).text = "other urban"
    t(local, 1390, 750, "Either sign is possible", size=21, fill=P["soft"])

    context = group(c, "hypothesis-context", "H3 urban-context dependence")
    t(context, 1110, 808, "H3  Urban-context dependence", size=25, weight="bold")
    t(context, 1110, 842, "Same land addition, different city area", size=20, fill=P["soft"])
    rect(context, 1128, 872, 104, 93, fill=P["city"], stroke=P["ink"], width=1.7)
    rect(context, 1192, 938, 27, 20, fill=P["sand"], stroke=P["sand_line"], width=1.3)
    rect(context, 1263, 872, 184, 93, fill=P["city"], stroke=P["ink"], width=1.7)
    rect(context, 1407, 938, 27, 20, fill=P["sand"], stroke=P["sand_line"], width=1.3)
    t(context, 1180, 994, "Smaller city", size=18, anchor="middle")
    t(context, 1355, 994, "Larger city", size=18, anchor="middle")

    graph = group(c, "conceptual-predictions", "Alternative city-scale relationships")
    line(graph, 1503, 962, 1712, 962, stroke=P["ink"], width=1.8)
    line(graph, 1503, 962, 1503, 858, stroke=P["ink"], width=1.8)
    gline(graph, "M1514 945 C1578 928 1639 905 1699 891", stroke=P["soft"], width=2.2)
    gline(graph, "M1514 945 C1586 943 1643 932 1699 914", stroke=P["teal"], width=2.2, dash="8 6")
    gline(graph, "M1514 945 C1578 908 1633 879 1699 874", stroke=P["rust"], width=2.2, dash="3 6")
    t(graph, 1607, 989, "Reclamation extent", size=18, anchor="middle")
    t(graph, 1503, 848, "Predicted suitability", size=18)
    line(graph, 1512, 1012, 1546, 1012, stroke=P["soft"], width=2)
    t(graph, 1554, 1018, "Shared", size=17)
    line(graph, 1621, 1008, 1652, 1008, stroke=P["teal"], width=2, stroke_dasharray="8 6")
    line(graph, 1621, 1016, 1652, 1016, stroke=P["rust"], width=2, stroke_dasharray="3 6")
    t(graph, 1661, 1018, "City-dependent", size=17)

    tests = group(root, "study-tests", "Study comparison strip", True)
    line(tests, 72, 1042, 1728, 1042, stroke=P["rule"], width=1.8)
    t(tests, 72, 1082, "Study tests", size=23, weight="bold")
    for x in (402, 934, 1395):
        circle(tests, x, 1074, 3.2, fill=P["soft"])
    t(tests, 423, 1082, "Nested spatial contrasts", size=21)
    t(tests, 955, 1082, "Reclamation × city size", size=21)
    t(tests, 1416, 1082, "FULL / environment-only", size=21)
    t(tests, 72, 1139, "Response: predicted invasive-plant suitability", size=23, weight="bold")
    return root


def validate(root: ET.Element) -> list[str]:
    nodes = list(root.iter())
    ids = [n.get("id") for n in nodes if n.get("id")]
    assert len(ids) == len(set(ids)), "Duplicate SVG IDs"
    assert not any(n.tag.endswith("}image") for n in nodes), "Raster image found"
    assert not any(n.tag.endswith("}foreignObject") for n in nodes), "Foreign object found"
    required = {"figure-title", "panel-a", "panel-b", "panel-c", "city-boundary", "former-shoreline", "reclaimed-land", "hypothesis-opportunity", "hypothesis-constraints", "hypothesis-context", "spatial-comparisons", "study-tests"}
    assert required.issubset(ids), required - set(ids)
    count = sum(n.tag.endswith("}text") for n in nodes)
    assert count >= 25
    return ["SVG XML: valid", f"Unique IDs: {len(ids)}", "Raster image elements: 0", f"Editable text elements: {count}", "Required semantic groups: present"]


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", type=Path, required=True)
    args = parser.parse_args()
    figs = args.output_dir / "figures"
    qc = args.output_dir / "qc"
    figs.mkdir(parents=True, exist_ok=True)
    qc.mkdir(parents=True, exist_ok=True)
    root = build()
    checks = validate(root)
    data = ET.tostring(root, encoding="utf-8", xml_declaration=True)
    svg = figs / "introduction_conceptual_framework_v2.svg"
    png = figs / "introduction_conceptual_framework_v2_preview.png"
    svg.write_bytes(data)
    ET.parse(svg)
    cairosvg.svg2png(bytestring=data, write_to=str(png), output_width=3600, output_height=2360)
    checks.extend(["Preview: rendered from SVG in Python at 3600 x 2360 pixels", "Visual inspection: pending"])
    (qc / "svg_validation_introduction_conceptual_framework_v2.txt").write_text("\n".join(checks) + "\n", encoding="utf-8")
    print(svg)
    print(png)


if __name__ == "__main__":
    main()
