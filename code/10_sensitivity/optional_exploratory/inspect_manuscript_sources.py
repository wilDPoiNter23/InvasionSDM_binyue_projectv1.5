#!/usr/bin/env python3
"""Read protected manuscript sources and verify existing text extractions."""
import argparse
import hashlib
import json
from pathlib import Path
import xml.etree.ElementTree as ET
import zipfile


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-root", type=Path, required=True)
    parser.add_argument("--input", type=Path, action="append", required=True)
    args = parser.parse_args()
    control = args.output_root / "00_project_control"
    control.mkdir(parents=True, exist_ok=True)
    records = []
    ns = {"w": "http://schemas.openxmlformats.org/wordprocessingml/2006/main",
          "m": "http://schemas.openxmlformats.org/officeDocument/2006/math"}
    for path in args.input:
        payload = path.read_bytes()
        with zipfile.ZipFile(path) as archive:
            tree = ET.fromstring(archive.read("word/document.xml"))
        paragraphs = ["".join(node.text or "" for node in para.findall(".//w:t", ns))
                      for para in tree.findall(".//w:p", ns)]
        text = "\n".join(paragraphs)
        destination = control / ("source_" + path.stem.replace(" ", "_") + ".txt")
        if destination.exists():
            if destination.read_text(encoding="utf-8") != text:
                raise ValueError(f"Existing extraction differs; preserved without overwrite: {destination}")
        else:
            destination.write_text(text, encoding="utf-8")
        records.append({"source_path": str(path.resolve()), "sha256": hashlib.sha256(payload).hexdigest(),
                        "paragraphs": len(paragraphs), "tables": len(tree.findall(".//w:tbl", ns)),
                        "mathematical_objects": len(tree.findall(".//m:oMath", ns)),
                        "extraction_path": str(destination.resolve()),
                        "source_unchanged": path.read_bytes() == payload,
                        "scope": "Text extraction; equations also require source XML or document inspection"})
    receipt = control / "source_reading_receipt.json"
    receipt.write_text(json.dumps(records, indent=2), encoding="utf-8")
    print(f"Verified {len(records)} protected document extractions")


if __name__ == "__main__":
    main()
