"""Check publication-source structure and syntax without running any analysis.

Requires Python 3, Rscript and Bash. No R packages or scientific inputs are loaded.
"""
import argparse
import ast
import csv
import hashlib
import json
import os
from pathlib import Path
import re
import subprocess
import sys
from urllib.parse import unquote


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--rscript", default="Rscript")
    parser.add_argument("--bash", default="bash")
    args = parser.parse_args()
    root = Path(__file__).resolve().parents[1]
    files = sorted(p for folder in ("code", "config", "tools")
                   for p in (root / folder).rglob("*") if p.is_file())
    errors = []
    counts = {"r_files": 0, "python_files": 0, "bash_files": 0,
              "markdown_links": 0, "configuration_aliases": 0,
              "archived_source_checksums": 0}
    for p in files:
        if p.suffix == ".py":
            counts["python_files"] += 1
            try:
                ast.parse(p.read_text(encoding="utf-8"), filename=str(p.relative_to(root)))
            except (SyntaxError, UnicodeError) as exc:
                errors.append(str(exc))
        if p.suffix == ".sh":
            counts["bash_files"] += 1
            try:
                result = subprocess.run([args.bash, "-n", str(p)], cwd=root,
                                        capture_output=True, text=True, timeout=30)
                if result.returncode:
                    errors.append(f"Bash syntax: {p.relative_to(root)}: {result.stderr.strip()}")
            except (OSError, subprocess.TimeoutExpired) as exc:
                errors.append(f"Bash check unavailable: {exc}")

    rfiles = [p for p in files if p.suffix == ".R"]
    counts["r_files"] = len(rfiles)
    # Rscript's Windows command-line launcher can truncate multiline -e input.
    r_expression = (
        'if (.Platform$OS.type == "windows") Sys.setlocale("LC_CTYPE", "English_United States.utf8"); '
        'fs <- c(list.files("code", pattern="[.]R$", recursive=TRUE, full.names=TRUE), '
        'list.files("config", pattern="[.]R$", recursive=TRUE, full.names=TRUE)); '
        'for (f in fs) parse(f, encoding="UTF-8"); '
        'cat("R_PARSE_PASS", length(fs), "files\\n")'
    )
    try:
        environment = dict(os.environ, LC_ALL="C", LANG="C")
        result = subprocess.run([args.rscript, "--vanilla", "-e", r_expression],
                                cwd=root, capture_output=True, text=True,
                                encoding="utf-8", errors="replace", timeout=60,
                                env=environment)
        if result.returncode or f"R_PARSE_PASS {len(rfiles)} files" not in result.stdout:
            errors.append(f"R parse failed: {result.stdout.strip()} {result.stderr.strip()}")
    except (OSError, subprocess.TimeoutExpired) as exc:
        errors.append(f"R check unavailable: {exc}")

    configured = set(re.findall(r"^\s*(\w+)\s*=", (root / "config/config.example.R").read_text(encoding="utf-8"), re.M))
    used = set()
    for p in rfiles:
        used.update(re.findall(r'''publication_path\(["']([^"']+)''', p.read_text(encoding="utf-8")))
    counts["configuration_aliases"] = len(used)
    for alias in sorted(used - configured):
        errors.append(f"Missing configuration alias: {alias}")

    with (root / "metadata/archived_source_checksums.csv").open(encoding="utf-8", newline="") as handle:
        for row in csv.DictReader(handle):
            counts["archived_source_checksums"] += 1
            source = root / row["public_script"]
            if not source.is_file() or hashlib.sha256(source.read_bytes()).hexdigest() != row["original_sha256"]:
                errors.append(f"Archived original checksum differs: {row['public_script']}")

    markdown = [root / "README.md"]
    markdown.extend(p for folder in ("docs", "zenodo", "code")
                    for p in (root / folder).rglob("*.md"))
    for p in markdown:
        for target in re.findall(r"\]\(([^)]+)\)", p.read_text(encoding="utf-8")):
            if re.match(r"^[a-zA-Z][a-zA-Z0-9+.-]*:", target) or target.startswith("#"):
                continue
            counts["markdown_links"] += 1
            target = unquote(target.split("#", 1)[0].strip("<>"))
            if target and not (p.parent / target).exists():
                errors.append(f"Broken local link: {p.relative_to(root)} -> {target}")

    print(json.dumps({"status": "PASS" if not errors else "FAIL",
                      "checks": counts, "errors": errors,
                      "scope": "Static checks only; no models, inputs, packages, or results validated."}, indent=2))
    return 1 if errors else 0


if __name__ == "__main__":
    sys.exit(main())
