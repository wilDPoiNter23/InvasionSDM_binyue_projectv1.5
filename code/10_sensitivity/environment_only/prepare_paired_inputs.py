#!/usr/bin/env python3
"""Freeze inputs from the accepted baseline inventory and completed paired checks."""
import argparse
import csv
import hashlib
from pathlib import Path


def continuous_path(reference):
    if any(token in reference.name for token in ("TSSbin", "TSSfilt")):
        stem = reference.stem.replace("_TSSbin", "").replace("_TSSfilt", "")
        candidates = [reference.with_name(stem + ext) for ext in (reference.suffix, ".tif", ".grd")]
        reference = next((p for p in candidates if p.is_file()), None)
    if reference is None or not reference.is_file():
        raise FileNotFoundError("A matching continuous FULL projection is required")
    if any(token.lower() in reference.name.lower() for token in ("TSSbin", "TSSfilt", "binary")):
        raise ValueError("Binary or threshold-filtered inputs are not continuous predictions")
    if reference.suffix.lower() not in (".tif", ".tiff", ".grd"):
        raise ValueError("Continuous raster input must be TIFF or GRD")
    if reference.suffix == ".grd" and not reference.with_suffix(".gri").is_file():
        raise FileNotFoundError("The continuous .grd projection requires its .gri companion")
    return reference


def fingerprint(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest() if path else ""


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--inventory", required=True)
    parser.add_argument("--output", required=True)
    args = parser.parse_args()
    destination = Path(args.output)
    if destination.exists():
        raise FileExistsError("Choose a new manifest; existing manifests are preserved")
    with open(args.inventory, newline="", encoding="utf-8-sig") as handle:
        inventory = list(csv.DictReader(handle, delimiter="\t"))
    if len({r["accepted_taxon"] for r in inventory}) != len(inventory):
        raise ValueError("The inventory contains duplicate species")
    pairs = []
    for row in inventory:
        env_root = Path(row["output_root"])
        full_root = Path(row["clean_full_output_root"]) if row["clean_full_output_root"] else None
        env_receipt = env_root / "VALIDATED.rds"
        full_receipt = full_root / "VALIDATED.rds" if full_root else None
        pair_receipt = full_root.parent / "PAIRED_VALIDATED.rds" if full_root else None
        if not env_receipt.is_file() or (full_root and (not full_receipt.is_file() or not pair_receipt.is_file())):
            continue
        full = continuous_path(full_root / "ensemble_suitability.tif" if full_root else Path(row["baseline_global_projection_path"]))
        env = env_root / "ensemble_suitability.tif"
        if not env.is_file():
            raise FileNotFoundError(f"Accepted ENV projection missing: {row['accepted_taxon']}")
        pairs.append(dict(
            species=row["accepted_taxon"], full_status="ACCEPTED", env_status="ACCEPTED",
            full_projection=str(full), env_projection=str(env),
            full_model_type="CLEAN_FULL" if full_root else "FULL", env_model_type=row["variant"],
            full_model=str(full_root / "models.rds") if full_root else row["baseline_model_path"],
            env_model=str(env_root / "models.rds"), full_scale_divisor=1000, env_scale_divisor=1000,
            full_receipt=str(full_receipt) if full_receipt else "",
            env_receipt=str(env_receipt), pair_receipt=str(pair_receipt) if pair_receipt else "",
            full_receipt_sha256=fingerprint(full_receipt), env_receipt_sha256=fingerprint(env_receipt),
            pair_receipt_sha256=fingerprint(pair_receipt),
            notes="No native-range mask; each cell uses the species with finite values in both groups."))
    if not pairs:
        raise ValueError("No accepted paired predictions found")
    destination.parent.mkdir(parents=True, exist_ok=True)
    with destination.open("x", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=list(pairs[0]))
        writer.writeheader()
        writer.writerows(sorted(pairs, key=lambda x: x["species"]))
    print(f"Frozen {len(pairs)} paired species: {destination}")


if __name__ == "__main__":
    main()
