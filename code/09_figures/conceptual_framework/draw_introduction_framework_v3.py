"""Draw an editable oblique-view coastal reclamation concept figure."""

from __future__ import annotations

import argparse
from pathlib import Path
from xml.etree import ElementTree as ET

import cairosvg

from draw_introduction_framework import SVG, circle, el, group, line, path, rect, text


C = {
    "ink": "#26343A", "muted": "#657378", "rule": "#D6DDDB",
    "sea": "#E3EFF0", "sea_edge": "#C1D6D8", "sea_side": "#BDD3D5",
    "land": "#E7EFE5", "land_side": "#B8CBB7", "contour": "#CAD9C9",
    "urban": "#CBD1CE", "urban_dark": "#9FABAA", "urban_side": "#A9B3AF",
    "sand": "#DFC38B", "sand_edge": "#BA985D", "sand_side": "#C2A36E",
    "teal": "#477C76", "rust": "#A86F5B", "white": "#FFFFFF",
}


def txt(parent: ET.Element, x: float, y: float, value: str, size: float = 29,
        weight: str = "normal", fill: str | None = None, anchor: str = "start") -> ET.Element:
    return text(parent, x, y, value, size=size, weight=weight,
                fill=fill or C["ink"], anchor=anchor)


def polygon(parent: ET.Element, pts: list[tuple[float, float]], fill: str = "none",
            stroke: str = "none", width: float = 1.5, **kwargs: object) -> ET.Element:
    d = "M" + " L".join(f"{x:.1f},{y:.1f}" for x, y in pts) + " Z"
    return path(parent, d, fill=fill, stroke=stroke, width=width, stroke_linejoin="round", **kwargs)


def open_path(parent: ET.Element, pts: list[tuple[float, float]], stroke: str,
              width: float = 2, dash: str | None = None, **kwargs: object) -> ET.Element:
    d = "M" + " L".join(f"{x:.1f},{y:.1f}" for x, y in pts)
    attrs = {"stroke_linecap": "round", "stroke_linejoin": "round", **kwargs}
    if dash:
        attrs["stroke_dasharray"] = dash
    return path(parent, d, stroke=stroke, width=width, **attrs)


def project(u: float, v: float, ox: float = 420, oy: float = 285,
            sx: float = 1.0, sy: float = 1.0) -> tuple[float, float]:
    return ox + sx * (u - .55 * v), oy + sy * (.28 * u + .36 * v)


def plan(parent: ET.Element, points: list[tuple[float, float]], **kwargs: object) -> ET.Element:
    return polygon(parent, [project(u, v) for u, v in points], **kwargs)


def block(parent: ET.Element, u: float, v: float, du: float, dv: float,
          height: float, colour: str = "urban") -> None:
    a, b, c, d = [project(*p) for p in
                  ((u, v), (u + du, v), (u + du, v + dv), (u, v + dv))]
    side = C["urban_side"] if colour == "urban" else C["sand_side"]
    polygon(parent, [b, c, (c[0], c[1] - height), (b[0], b[1] - height)],
            fill=side)
    polygon(parent, [d, c, (c[0], c[1] - height), (d[0], d[1] - height)],
            fill=side, opacity="0.84")
    polygon(parent, [(x, y - height) for x, y in (a, b, c, d)],
            fill=C["urban"] if colour == "urban" else C["sand"],
            stroke=C["urban_dark"] if colour == "urban" else C["sand_edge"], width=1.1)


def mini_tile(parent: ET.Element, x: float, y: float, w: float, h: float,
              fill: str, side: str, edge: str = "none", rise: float = 15) -> None:
    top = [(x, y), (x + w, y + .28*w), (x + w - .52*h, y + .28*w + .36*h),
           (x - .52*h, y + .36*h)]
    polygon(parent, [top[1], top[2], (top[2][0], top[2][1] + rise),
                     (top[1][0], top[1][1] + rise)], fill=side)
    polygon(parent, [top[2], top[3], (top[3][0], top[3][1] + rise),
                     (top[2][0], top[2][1] + rise)], fill=side, opacity="0.8")
    polygon(parent, top, fill=fill, stroke=edge, width=1.8)


def build() -> ET.Element:
    root = ET.Element(f"{{{SVG}}}svg", {
        "width": "180mm", "height": "112.5mm", "viewBox": "0 0 2400 1500",
        "version": "1.1", "role": "img",
        "aria-label": "Oblique conceptual view of coastal reclamation and predicted invasive-plant suitability",
    })
    rect(root, 0, 0, 2400, 1500, fill=C["white"])

    title = group(root, "figure-title", "Removable title", True)
    txt(title, 83, 95, "From coastal land creation to invasive-plant suitability", 45, "bold")
    txt(title, 85, 145, "Does creating urban land also create suitable habitat for invasive plants?", 30,
        fill=C["muted"])
    line(root, 83, 180, 2315, 180, stroke=C["rule"], width=2)

    a = group(root, "panel-a", "a Coastal land transformation", True)
    txt(a, 84, 241, "a", 37, "bold")
    txt(a, 132, 241, "Coastal land transformation", 36, "bold")
    scene = group(a, "coastal-scene", "Oblique coastal landscape")
    corner = [project(*p) for p in ((0, 0), (700, 0), (700, 650), (0, 650))]
    polygon(scene, [corner[1], corner[2], (corner[2][0], corner[2][1] + 38),
                    (corner[1][0], corner[1][1] + 38)], fill=C["sea_side"])
    polygon(scene, [corner[2], corner[3], (corner[3][0], corner[3][1] + 38),
                    (corner[2][0], corner[2][1] + 38)], fill=C["sea_side"], opacity="0.78")
    polygon(scene, corner, fill=C["sea"], stroke=C["sea_edge"], width=2.1)
    # The drawing is a symbolic ground plane; its vertical face has no data meaning.
    coast = [(475, 0), (488, 110), (478, 210), (492, 300),
             (480, 405), (495, 515), (485, 650)]
    land_plan = [(0, 0), *coast, (0, 650)]
    plan(scene, land_plan, fill=C["land"])
    for v in (65, 126, 585):
        open_path(scene, [project(u, v + .04*u) for u in (20, 95, 170)],
                  C["contour"], 2.1)
    existing = group(scene, "existing-urban-land", "Existing urban land")
    city_plan = [(190, 82), (470, 82), (485, 155), (478, 245),
                 (491, 365), (483, 470), (493, 555), (192, 555)]
    plan(existing, city_plan, fill=C["urban"], stroke=C["urban_dark"], width=1.8)
    # A few low-rise blocks give an oblique view without making buildings the subject.
    for u, v, du, dv, h in ((235, 125, 47, 45, 26), (324, 130, 48, 61, 31),
                           (398, 133, 40, 60, 25), (243, 251, 56, 56, 20),
                           (332, 252, 57, 44, 30), (405, 272, 45, 57, 21),
                           (238, 391, 47, 57, 23), (319, 392, 48, 53, 27),
                           (402, 393, 49, 55, 19)):
        block(existing, u, v, du, dv, h)
    for v in (222, 366):
        open_path(existing, [project(u, v) for u in (198, 268, 348, 468)],
                  C["white"], 9, opacity="0.85")

    reclaimed = group(scene, "reclaimed-land", "Seaward reclaimed land")
    patch = [(486, 228), (632, 228), (632, 278), (672, 278),
             (672, 481), (633, 481), (633, 522), (494, 522),
             (482, 458), (491, 365), (480, 292)]
    plan(reclaimed, patch, fill=C["sand"], stroke=C["sand_edge"], width=2.3)
    for v in (302, 385, 467):
        open_path(reclaimed, [project(u, v) for u in (508, 570, 630)],
                  C["white"], 3.5, opacity="0.75")
    for u in (555, 612):
        open_path(reclaimed, [project(u, v) for v in (245, 319, 404, 494)],
                  C["white"], 3.5, opacity="0.75")
    # A modest wharf locates the seaward edge without depicting a measured pathway.
    open_path(reclaimed, [project(*p) for p in ((670, 345), (697, 345), (697, 363))],
              C["sand_edge"], 4)
    former = group(scene, "former-shoreline", "Dashed former shoreline")
    open_path(former, [project(*p) for p in coast], C["ink"], 3.0, "12 11")
    bound = group(scene, "city-boundary", "Solid city boundary")
    boundary = [(175, 73), (499, 73), (509, 213), (680, 213),
                (688, 538), (502, 538), (503, 572), (175, 572)]
    plan(bound, boundary, stroke=C["ink"], width=3.3)
    # Direct labels sit off the terrain; their leader marks identify surfaces.
    labels = group(a, "terrain-labels", "Direct terrain labels")
    txt(labels, 110, 315, "Surrounding", 29)
    txt(labels, 110, 351, "landscape", 29)
    open_path(labels, [(263, 358), (346, 427)], C["muted"], 1.8)
    txt(labels, 443, 826, "Existing urban land", 30)
    open_path(labels, [(584, 785), (602, 613)], C["muted"], 1.8)
    txt(labels, 846, 826, "Reclaimed land", 30, "bold")
    open_path(labels, [(899, 785), (862, 657)], C["sand_edge"], 1.9)
    txt(labels, 1012, 386, "Sea", 29, fill=C["muted"])
    txt(labels, 176, 880, "City boundary", 26)
    line(labels, 99, 871, 155, 871, stroke=C["ink"], width=3.4)
    txt(labels, 518, 880, "Former shoreline", 26)
    line(labels, 432, 871, 495, 871, stroke=C["ink"], width=3.2, stroke_dasharray="12 10")
    line(a, 85, 920, 1170, 920, stroke=C["rule"], width=2)
    comparisons = group(a, "spatial-comparisons", "Nested spatial comparisons")
    txt(comparisons, 91, 977, "1", 29, "bold", C["teal"])
    txt(comparisons, 134, 977, "City  /  surrounding landscape", 29)
    txt(comparisons, 91, 1043, "2", 29, "bold", C["sand_edge"])
    txt(comparisons, 134, 1043, "Reclaimed  /  other urban land", 29)
    txt(comparisons, 134, 1085, "within the same city", 26, fill=C["muted"])

    line(root, 1230, 210, 1230, 1280, stroke=C["rule"], width=2)
    b = group(root, "panel-b", "b Alternative suitability expectations", True)
    txt(b, 1290, 241, "b", 37, "bold")
    txt(b, 1338, 241, "One new land surface, two possibilities", 35, "bold")
    shared = group(b, "shared-new-land", "Same reclaimed surface")
    mini_tile(shared, 1590, 286, 515, 330, C["sea"], C["sea_side"], C["sea_edge"], 17)
    mini_tile(shared, 1700, 337, 325, 255, C["sand"], C["sand_side"], C["sand_edge"], 16)
    for i in range(3):
        x = 1708 + i*75
        open_path(shared, [(x, 357 + i*23), (x+126, 392+i*23)],
                  C["white"], 2.3, opacity="0.8")
    txt(b, 1531, 584, "Same reclaimed land", 28, fill=C["muted"])
    open_path(b, [(1665, 602), (1665, 628), (1437, 628), (1437, 651)],
              C["muted"], 2.0, "8 8")
    open_path(b, [(1665, 628), (2013, 628), (2013, 651)], C["muted"], 2.0, "8 8")
    op = group(b, "hypothesis-opportunity", "H1 higher suitability")
    con = group(b, "hypothesis-constraints", "H2 lower suitability")
    circle(op, 1437, 653, 5, fill=C["teal"])
    circle(con, 2013, 653, 5, fill=C["rust"])
    txt(op, 1320, 709, "H1", 30, "bold", C["teal"])
    txt(op, 1320, 751, "Higher suitability", 31)
    txt(op, 1320, 790, "where opportunities dominate", 25, fill=C["muted"])
    line(op, 1320, 809, 1748, 809, stroke=C["teal"], width=3)
    txt(con, 1850, 709, "H2", 30, "bold", C["rust"])
    txt(con, 1850, 751, "Lower suitability", 31)
    txt(con, 1850, 790, "where constraints dominate", 25, fill=C["muted"])
    line(con, 1850, 809, 2295, 809, stroke=C["rust"], width=3)

    c = group(root, "panel-c", "c Context and spatial prediction", True)
    line(c, 1290, 855, 2315, 855, stroke=C["rule"], width=2)
    txt(c, 1290, 919, "c", 37, "bold")
    txt(c, 1338, 919, "Where is suitability compared?", 35, "bold")
    txt(c, 1316, 974, "Within one city", 28, fill=C["muted"])
    city_tile = group(c, "within-city-contrast", "Within-city land comparison")
    mini_tile(city_tile, 1430, 995, 214, 128, C["urban"], C["urban_side"], C["urban_dark"], 12)
    mini_tile(city_tile, 1630, 1051, 125, 83, C["sand"], C["sand_side"], C["sand_edge"], 9)
    txt(city_tile, 1330, 1194, "Other urban", 25)
    txt(city_tile, 1640, 1194, "Reclaimed", 25)
    equation = txt(city_tile, 1316, 1244, "", 27, "bold")
    for value, sub in (("Δ", False), ("local", True), (" = S", False),
                       ("reclaimed", True), (" − S", False), ("other urban", True)):
        attrs = {"baseline_shift": "sub", "font_size": 18} if sub else {}
        el(equation, "tspan", **attrs).text = value
    txt(city_tile, 1316, 1282, "Either sign is possible", 24, fill=C["muted"])
    line(c, 1840, 955, 1840, 1287, stroke=C["rule"], width=2)
    context = group(c, "hypothesis-context", "H3 city context")
    txt(context, 1888, 974, "Urban context", 28, fill=C["muted"])
    mini_tile(context, 1930, 1032, 116, 88, C["urban"], C["urban_side"], C["urban_dark"], 10)
    mini_tile(context, 2200, 1003, 185, 130, C["urban"], C["urban_side"], C["urban_dark"], 10)
    # Identical top-plane parcels denote an equal absolute land addition.
    mini_tile(context, 2014, 1090, 36, 30, C["sand"], C["sand_side"], C["sand_edge"], 4)
    mini_tile(context, 2284, 1090, 36, 30, C["sand"], C["sand_side"], C["sand_edge"], 4)
    txt(context, 1931, 1201, "Smaller city", 25)
    txt(context, 2159, 1201, "Larger city", 25)
    txt(context, 1900, 1250, "Same land addition", 25, fill=C["muted"])
    txt(context, 1900, 1282, "different city area", 25, fill=C["muted"])

    tests = group(root, "study-tests", "Study question strip", True)
    line(tests, 83, 1331, 2315, 1331, stroke=C["rule"], width=2)
    txt(tests, 85, 1393, "Predicted invasive-plant suitability", 31, "bold")
    txt(tests, 1053, 1393, "Nested contrasts", 26)
    circle(tests, 1022, 1384, 4, fill=C["muted"])
    txt(tests, 1455, 1393, "Reclamation × city size", 26)
    circle(tests, 1422, 1384, 4, fill=C["muted"])
    txt(tests, 1922, 1393, "FULL / environment-only", 26)
    circle(tests, 1889, 1384, 4, fill=C["muted"])
    return root


def validate(root: ET.Element) -> list[str]:
    nodes = list(root.iter())
    ids = [n.get("id") for n in nodes if n.get("id")]
    assert len(ids) == len(set(ids))
    assert not any(n.tag.endswith("}image") or n.tag.endswith("}foreignObject") for n in nodes)
    required = {"figure-title", "panel-a", "panel-b", "panel-c", "city-boundary",
                "former-shoreline", "reclaimed-land", "hypothesis-opportunity",
                "hypothesis-constraints", "hypothesis-context", "study-tests"}
    assert required.issubset(ids), required - set(ids)
    return ["SVG XML: valid", f"Unique IDs: {len(ids)}",
            "Raster/foreign image elements: 0",
            f"Editable text elements: {sum(n.tag.endswith('}text') for n in nodes)}",
            "Required semantic groups: present"]


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", type=Path, required=True)
    args = parser.parse_args()
    figs, qc = args.output_dir / "figures", args.output_dir / "qc"
    figs.mkdir(parents=True, exist_ok=True)
    qc.mkdir(parents=True, exist_ok=True)
    root = build()
    checks = validate(root)
    data = ET.tostring(root, encoding="utf-8", xml_declaration=True)
    svg = figs / "introduction_conceptual_framework_v3.svg"
    png = figs / "introduction_conceptual_framework_v3_preview.png"
    svg.write_bytes(data)
    ET.parse(svg)
    cairosvg.svg2png(bytestring=data, write_to=str(png), output_width=3600, output_height=2250)
    checks.append("Preview: rendered from SVG with Python/CairoSVG at 3600 x 2250 pixels")
    (qc / "svg_validation_introduction_conceptual_framework_v3.txt").write_text("\n".join(checks) + "\n", encoding="utf-8")
    print(svg)
    print(png)


if __name__ == "__main__":
    main()
