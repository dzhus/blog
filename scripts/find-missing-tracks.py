#!/usr/bin/env python3
"""List external .gpx basenames that are not present under trips/.

Also prints commented bash that would create a trip folder and copy each
missing track (no file operations are performed).
"""

from __future__ import annotations

import argparse
import shlex
import sys
from pathlib import Path


def collect_gpx_basenames(root: Path) -> set[str]:
    names: set[str] = set()
    for path in root.rglob("*"):
        if path.is_file() and path.suffix.lower() == ".gpx":
            names.add(path.name)
    return names


def collect_gpx_by_basename(root: Path) -> dict[str, Path]:
    """Map basename -> first path found (stable via sorted walk)."""
    found: dict[str, Path] = {}
    for path in sorted(root.rglob("*")):
        if path.is_file() and path.suffix.lower() == ".gpx":
            found.setdefault(path.name, path)
    return found


def trip_slug_for_track(gpx_path: Path) -> str:
    """Derive a trip folder name from the GPX stem (spaces -> hyphens)."""
    stem = gpx_path.stem.strip()
    slug = "-".join(stem.split())
    return slug or gpx_path.stem


def commented_create_trip(trips_root: Path, gpx_path: Path) -> str:
    slug = trip_slug_for_track(gpx_path)
    trip_dir = trips_root / slug
    dest = trip_dir / gpx_path.name
    cmd = (
        f"mkdir -p {shlex.quote(str(trip_dir))} && "
        f"cp {shlex.quote(str(gpx_path))} {shlex.quote(str(dest))}"
    )
    return f"# {cmd}"


def main() -> int:
    parser = argparse.ArgumentParser(
        description=(
            "Compare .gpx files under trips/ with an external tracks directory "
            "and list external basenames missing from every trip. "
            "Also prints commented bash to create a trip and copy each missing track."
        )
    )
    parser.add_argument(
        "external_tracks_dir",
        type=Path,
        help="Directory of .gpx tracks to check (e.g. ~/Sync/tracks)",
    )
    parser.add_argument(
        "--trips",
        type=Path,
        default=Path("trips"),
        help="Trips root directory (default: trips)",
    )
    args = parser.parse_args()

    trips_root: Path = args.trips
    external_root: Path = args.external_tracks_dir

    if not trips_root.is_dir():
        print(f"error: trips directory not found: {trips_root}", file=sys.stderr)
        return 1
    if not external_root.is_dir():
        print(
            f"error: external tracks directory not found: {external_root}",
            file=sys.stderr,
        )
        return 1

    trip_names = collect_gpx_basenames(trips_root)
    external_by_name = collect_gpx_by_basename(external_root)
    missing = sorted(set(external_by_name) - trip_names)

    for name in missing:
        print(name)
        print(commented_create_trip(trips_root, external_by_name[name]))

    print(
        f"{len(missing)} missing of {len(external_by_name)} external "
        f"({len(trip_names)} in trips)",
        file=sys.stderr,
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
