#!/usr/bin/env python3
"""List external .gpx basenames that are not present under trips/."""

from __future__ import annotations

import argparse
import sys
from pathlib import Path


def collect_gpx_basenames(root: Path) -> set[str]:
    names: set[str] = set()
    for path in root.rglob("*"):
        if path.is_file() and path.suffix.lower() == ".gpx":
            names.add(path.name)
    return names


def main() -> int:
    parser = argparse.ArgumentParser(
        description=(
            "Compare .gpx files under trips/ with an external tracks directory "
            "and list external basenames missing from every trip."
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
    external_names = collect_gpx_basenames(external_root)
    missing = sorted(external_names - trip_names)

    for name in missing:
        print(name)

    print(
        f"{len(missing)} missing of {len(external_names)} external "
        f"({len(trip_names)} in trips)",
        file=sys.stderr,
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
