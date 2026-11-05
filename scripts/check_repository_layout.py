#!/usr/bin/env python3
"""Check publication file placement and text hygiene without running analyses.

Uses the Git index and non-ignored working files, including pending additions.
Ignored private working notes are outside the public text scope. Root entries
are checked even when ignored. No files are modified by this command.
"""

import argparse
import json
import re
import subprocess
import sys
from pathlib import Path

DIRECTORIES = frozenset({"code", "config", "docs", "examples", "metadata",
                         "scripts", "tools", "working", "zenodo"})
RULES_FILE = "AGEN" + "TS.md"
ROOT_FILES = frozenset({"README.md", "CITATION.cff", RULES_FILE,
                        ".gitignore", ".gitattributes", "LICENSE", "LICENSE.md"})
BAD_NAME = re.compile(r"final2|final_final|new_final|copy|latest_version", re.I)
CHINESE = re.compile(r"[\u3400-\u4dbf\u4e00-\u9fff\uf900-\ufaff\U00020000-\U000323af]")
PRIVATE_PATH = re.compile(
    r"\b[A-Z]:[\\/]|\\\\[A-Za-z0-9_.-]+\\[A-Za-z0-9_$.-]+|"
    r"(?<![A-Za-z0-9_:/])/(?:thfs\d+|home|scratch|gpfs|lustre|data|work|mnt|beegfs|nfs|pfs|public\d+|fs\d+|projects?)(?:/|\b)", re.I)
# Character classes keep the rule expression subject to its own text policy.
ATTRIBUTION = re.compile(
    r"(?<![A-Za-z0-9])(?:A[I]|Code[x]|ag[e]nts?)(?![A-Za-z0-9])|forensic\s+audit|generated\s+by", re.I)
FORBIDDEN_SUFFIXES = frozenset({
    ".rds", ".rda", ".rdata", ".pkl", ".pickle", ".joblib", ".rdat",
    ".model", ".models", ".biomod", ".qs",
    ".tif", ".tiff", ".grd", ".gri", ".asc", ".img", ".nc", ".vrt",
    ".bil", ".bsq", ".bip", ".shp", ".shx", ".dbf", ".gpkg",
    ".log", ".out", ".err", ".rout", ".parquet", ".feather",
    ".h5", ".hdf5", ".duckdb", ".sqlite", ".zip", ".7z", ".gz", ".tar",
})
TABULAR = frozenset({".csv", ".tsv", ".xlsx", ".xls"})
PUBLIC_TABLES = frozenset({
    "metadata/archived_source_checksums.csv", "metadata/city_variables.csv",
    "metadata/hpc_environment_observed_20261013.csv", "metadata/predictor_list.csv",
    "metadata/species_list.csv", "zenodo/zenodo_manifest.csv",
})


def git_paths(root, *arguments):
    result = subprocess.run(["git", "-C", str(root), "ls-files", "-z", *arguments],
                            check=True, capture_output=True)
    return {entry.decode("utf-8") for entry in result.stdout.split(b"\0") if entry}


def check(root):
    tracked = git_paths(root, "--cached")
    candidates = tracked | git_paths(root, "--others", "--exclude-standard")
    errors = []
    for entry in sorted(root.iterdir()):
        if entry.name == ".git":
            continue
        if entry.is_dir() and entry.name not in DIRECTORIES:
            errors.append(f"Unapproved top-level directory: {entry.name}")
        elif not entry.is_dir() and entry.name not in ROOT_FILES:
            errors.append(f"Unapproved root file (scripts and data belong below directories): {entry.name}")
        if BAD_NAME.search(entry.name):
            errors.append(f"Prohibited filename: {entry.name}")

    checked = 0
    for relative in sorted(candidates):
        path = Path(relative)
        parts = {part.lower() for part in path.parts}
        suffix = path.suffix.lower()
        if any(BAD_NAME.search(part) for part in path.parts):
            errors.append(f"Prohibited filename: {relative}")
        if relative in tracked:
            if "working" in parts or relative == "config/config.local.R":
                errors.append(f"Tracked private working file: {relative}")
            raw = (bool(parts & {"raw", "raw_data", "data", "models", "model_objects", "logs"})
                   or (suffix in TABULAR and bool(re.search(r"(?:^|[_-])raw(?:[_.-]|$)", path.name, re.I))))
            tabular_input = suffix in TABULAR and relative not in PUBLIC_TABLES
            if path.name.lower().startswith(".renviron") or suffix in FORBIDDEN_SUFFIXES or raw or tabular_input:
                errors.append(f"Tracked data, model, raster, log or private environment file: {relative}")
        # This directory is private only while untracked; tracked content fails above.
        if path.parts[0] == "working":
            continue
        full_path = root / path
        if not full_path.is_file():
            errors.append(f"Cannot inspect indexed or pending file: {relative}")
            continue
        if full_path.is_symlink():
            errors.append(f"Public symbolic link cannot be inspected safely: {relative}")
            continue
        if relative in tracked and suffix in FORBIDDEN_SUFFIXES:
            continue
        try:
            if full_path.stat().st_size > 16 * 1024 * 1024:
                errors.append(f"Public file exceeds the inspection size limit: {relative}")
                continue
            payload = full_path.read_bytes()
            encoding = "utf-16" if payload.startswith((b"\xff\xfe", b"\xfe\xff")) else "utf-8-sig"
            content = payload.decode(encoding)
        except (OSError, UnicodeError) as exc:
            errors.append(f"Cannot inspect public text: {relative}: {exc}")
            continue
        checked += 1
        for label, pattern in (("Chinese text", CHINESE), ("Private absolute path", PRIVATE_PATH)):
            for match in pattern.finditer(content):
                # A capitalised configuration placeholder is a portable root,
                # whereas an actual absolute directory must still fail.
                if label == "Private absolute path" and match.group().startswith("/"):
                    prefix = content[max(0, match.start() - 200):match.start()]
                    if re.search(r"(?:<[A-Z][A-Z0-9_]*>|\$\{[A-Z][A-Z0-9_]*\})$", prefix):
                        continue
                line = content.count("\n", 0, match.start()) + 1
                errors.append(f"{label}: {relative}:{line}")
        if relative != RULES_FILE:
            for match in ATTRIBUTION.finditer(content):
                line = content.count("\n", 0, match.start()) + 1
                errors.append(f"Prohibited attribution wording: {relative}:{line}")
    return {"status": "FAIL" if errors else "PASS", "tracked_files": len(tracked),
            "public_text_files_checked": checked, "errors": sorted(set(errors)),
            "scope": "Layout and publication hygiene only; scientific reproducibility is not validated."}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=Path(__file__).resolve().parents[1],
                        help="Repository to inspect (defaults to this script's repository).")
    args = parser.parse_args()
    try:
        root = args.root.resolve(strict=True)
        result = subprocess.run(["git", "-C", str(root), "rev-parse", "--show-toplevel"],
                                check=True, capture_output=True, text=True, encoding="utf-8")
        if Path(result.stdout.strip()).resolve() != root:
            raise ValueError("The supplied root must be a Git repository root")
        report = check(root)
    except (OSError, ValueError, subprocess.CalledProcessError) as exc:
        print(json.dumps({"status": "FAIL", "errors": [str(exc)]}, indent=2))
        return 1
    print(json.dumps(report, indent=2))
    return 1 if report["errors"] else 0


if __name__ == "__main__":
    sys.exit(main())
