#!/usr/bin/env python3
"""Suggest (or apply) moving photos into trips/ by EXIF date vs GPX date ranges."""

from __future__ import annotations

import argparse
import json
import shutil
import subprocess
import sys
import xml.etree.ElementTree as ET
from dataclasses import dataclass
from datetime import date, datetime
from pathlib import Path
from typing import Any

IMAGE_SUFFIXES = {".jpg", ".jpeg", ".png", ".webp"}


@dataclass(frozen=True)
class TripWindow:
    slug: str
    directory: Path
    start: date
    end: date


@dataclass
class PhotoExif:
    size: int
    date: date | None
    lat: float | None
    lon: float | None


def die(msg: str, code: int = 1) -> int:
    print(f"error: {msg}", file=sys.stderr)
    return code


def local_name(tag: str) -> str:
    if "}" in tag:
        return tag.rsplit("}", 1)[1]
    return tag


def parse_gpx_point_times(path: Path) -> tuple[datetime | None, datetime | None]:
    """Return min/max aware or naive datetimes from trkpt/rtept <time> elements."""
    try:
        root = ET.parse(path).getroot()
    except ET.ParseError as err:
        raise ValueError(f"invalid GPX XML in {path}: {err}") from err

    times: list[datetime] = []

    def consider(el: ET.Element) -> None:
        for child in el:
            if local_name(child.tag) != "time" or not (child.text or "").strip():
                continue
            raw = child.text.strip()
            # Support Z and numeric offsets; treat as UTC-ish for calendar date.
            normalized = raw.replace("Z", "+00:00")
            try:
                times.append(datetime.fromisoformat(normalized))
            except ValueError:
                continue

    for el in root.iter():
        name = local_name(el.tag)
        if name in {"trkpt", "rtept"}:
            consider(el)

    if not times:
        return None, None
    return min(times), max(times)


def load_trip_windows(trips_root: Path) -> list[TripWindow]:
    windows: list[TripWindow] = []
    for trip_dir in sorted(p for p in trips_root.iterdir() if p.is_dir() and not p.name.startswith(".")):
        gpx_files = sorted(trip_dir.glob("*.gpx")) + sorted(trip_dir.glob("*.GPX"))
        if not gpx_files:
            continue
        trip_min: datetime | None = None
        trip_max: datetime | None = None
        for gpx in gpx_files:
            tmin, tmax = parse_gpx_point_times(gpx)
            if tmin is None or tmax is None:
                print(
                    f"warning: no usable <time> in {gpx.relative_to(trips_root)}",
                    file=sys.stderr,
                )
                continue
            if trip_min is None or tmin < trip_min:
                trip_min = tmin
            if trip_max is None or tmax > trip_max:
                trip_max = tmax
        if trip_min is None or trip_max is None:
            print(
                f"warning: skipping trip {trip_dir.name}: no GPX point times",
                file=sys.stderr,
            )
            continue
        windows.append(
            TripWindow(
                slug=trip_dir.name,
                directory=trip_dir,
                start=trip_min.date(),
                end=trip_max.date(),
            )
        )
    windows.sort(key=lambda w: (w.start, w.slug))
    return windows


def collect_trip_basenames(trips_root: Path) -> set[str]:
    names: set[str] = set()
    for path in trips_root.rglob("*"):
        if path.is_file() and path.suffix.lower() in IMAGE_SUFFIXES:
            names.add(path.name)
    return names


def collect_media_images(media_root: Path) -> list[Path]:
    return sorted(
        p
        for p in media_root.rglob("*")
        if p.is_file() and p.suffix.lower() in IMAGE_SUFFIXES
    )


def load_cache(cache_path: Path) -> dict[str, Any]:
    if not cache_path.is_file():
        return {}
    try:
        raw = json.loads(cache_path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError):
        return {}
    return raw if isinstance(raw, dict) else {}


def save_cache(cache_path: Path, cache: dict[str, Any]) -> None:
    cache_path.parent.mkdir(parents=True, exist_ok=True)
    cache_path.write_text(
        json.dumps(cache, indent=2, sort_keys=True) + "\n",
        encoding="utf-8",
    )


def parse_exif_date(value: Any) -> date | None:
    if value is None:
        return None
    text = str(value).strip()
    if not text:
        return None
    # Common exiftool forms: "YYYY:MM:DD HH:MM:SS" or ISO-8601.
    candidates = [
        text,
        text.replace("Z", "+00:00"),
        text[:19].replace(":", "-", 2) if len(text) >= 10 and text[4] == ":" else text,
    ]
    for raw in candidates:
        try:
            if "T" in raw or "+" in raw[10:] or raw.endswith("Z"):
                return datetime.fromisoformat(raw.replace("Z", "+00:00")).date()
        except ValueError:
            pass
        for fmt in ("%Y:%m:%d %H:%M:%S", "%Y-%m-%d %H:%M:%S", "%Y-%m-%d"):
            try:
                return datetime.strptime(raw[:19], fmt).date()
            except ValueError:
                continue
    head = text[:10].replace(":", "-")
    try:
        return date.fromisoformat(head)
    except ValueError:
        return None


def parse_coord(value: Any) -> float | None:
    if value is None:
        return None
    try:
        num = float(value)
    except (TypeError, ValueError):
        return None
    return num if abs(num) <= 180 else None


def ensure_exiftool() -> str:
    from shutil import which

    path = which("exiftool")
    if not path:
        raise FileNotFoundError(
            "exiftool not found on PATH (install with Homebrew: brew install exiftool)"
        )
    return path


def exiftool_batch(exiftool: str, paths: list[Path]) -> dict[str, dict[str, Any]]:
    """Return mapping of absolute path -> exiftool JSON object."""
    if not paths:
        return {}
    # Pass paths via -@ file list to avoid argv limits.
    import tempfile

    with tempfile.NamedTemporaryFile("w", encoding="utf-8", delete=False) as tmp:
        for path in paths:
            tmp.write(f"{path}\n")
        list_path = tmp.name
    try:
        proc = subprocess.run(
            [
                exiftool,
                "-json",
                "-n",
                "-DateTimeOriginal",
                "-CreateDate",
                "-GPSLatitude",
                "-GPSLongitude",
                "-@",
                list_path,
            ],
            check=False,
            capture_output=True,
            text=True,
        )
    finally:
        Path(list_path).unlink(missing_ok=True)

    if proc.returncode not in (0, 1):
        # exiftool uses 1 when some files have minor warnings
        raise RuntimeError(
            f"exiftool failed ({proc.returncode}): {proc.stderr.strip() or proc.stdout.strip()}"
        )

    try:
        rows = json.loads(proc.stdout or "[]")
    except json.JSONDecodeError as err:
        raise RuntimeError(f"exiftool returned invalid JSON: {err}") from err

    out: dict[str, dict[str, Any]] = {}
    for row in rows:
        src = row.get("SourceFile")
        if not src:
            continue
        out[str(Path(src).resolve())] = row
    return out


def cache_entry_valid(entry: Any, size: int) -> bool:
    return (
        isinstance(entry, dict)
        and entry.get("size") == size
        and "date" in entry
    )


def entry_to_photo_exif(entry: dict[str, Any], size: int) -> PhotoExif:
    date_raw = entry.get("date")
    photo_date: date | None = None
    if isinstance(date_raw, str) and date_raw:
        try:
            photo_date = date.fromisoformat(date_raw)
        except ValueError:
            photo_date = None
    lat = entry.get("lat")
    lon = entry.get("lon")
    return PhotoExif(
        size=size,
        date=photo_date,
        lat=float(lat) if isinstance(lat, (int, float)) else None,
        lon=float(lon) if isinstance(lon, (int, float)) else None,
    )


def resolve_exif_for_paths(
    paths: list[Path],
    cache: dict[str, Any],
    exiftool: str,
) -> dict[str, PhotoExif]:
    result: dict[str, PhotoExif] = {}
    need_read: list[Path] = []

    for path in paths:
        key = str(path.resolve())
        size = path.stat().st_size
        entry = cache.get(key)
        if cache_entry_valid(entry, size):
            result[key] = entry_to_photo_exif(entry, size)
        else:
            need_read.append(path)

    if need_read:
        batch = exiftool_batch(exiftool, need_read)
        for path in need_read:
            key = str(path.resolve())
            size = path.stat().st_size
            row = batch.get(key, {})
            photo_date = parse_exif_date(row.get("DateTimeOriginal")) or parse_exif_date(
                row.get("CreateDate")
            )
            lat = parse_coord(row.get("GPSLatitude"))
            lon = parse_coord(row.get("GPSLongitude"))
            cache[key] = {
                "size": size,
                "date": photo_date.isoformat() if photo_date else None,
                "lat": lat,
                "lon": lon,
            }
            result[key] = PhotoExif(size=size, date=photo_date, lat=lat, lon=lon)

    return result


def find_trip_for_date(
    photo_date: date, windows: list[TripWindow]
) -> TripWindow | None:
    matches = [w for w in windows if w.start <= photo_date <= w.end]
    if not matches:
        return None
    if len(matches) > 1:
        # Prefer the tightest window; should not happen with current trips.
        matches.sort(key=lambda w: ((w.end - w.start).days, w.slug))
    return matches[0]


def main() -> int:
    parser = argparse.ArgumentParser(
        description=(
            "Match photos in a media directory to trips/ by EXIF date vs GPX "
            "date ranges. Default is dry-run; pass --apply to move files."
        )
    )
    parser.add_argument(
        "media_dir",
        type=Path,
        help="External photo tree (e.g. flickr-takeaway-processor/media)",
    )
    parser.add_argument(
        "--trips",
        type=Path,
        default=Path("trips"),
        help="Trips root directory (default: trips)",
    )
    parser.add_argument(
        "--cache",
        type=Path,
        default=Path("_cache/assign-photos-exif.json"),
        help="EXIF cache JSON path (default: _cache/assign-photos-exif.json)",
    )
    parser.add_argument(
        "--apply",
        action="store_true",
        help="Move matched photos into trip folders (default: dry-run)",
    )
    parser.add_argument(
        "--clean-up-assigned",
        action="store_true",
        help=(
            "Remove media files whose basename already exists under trips/ "
            "(dry-run unless --apply is also set)"
        ),
    )
    parser.add_argument(
        "--verbose",
        action="store_true",
        help="List unmatched photos and those missing EXIF dates",
    )
    args = parser.parse_args()

    media_root: Path = args.media_dir
    trips_root: Path = args.trips
    cache_path: Path = args.cache

    if not media_root.is_dir():
        return die(f"media directory not found: {media_root}")
    if not trips_root.is_dir():
        return die(f"trips directory not found: {trips_root}")

    try:
        exiftool = ensure_exiftool()
    except FileNotFoundError as err:
        return die(str(err))

    windows = load_trip_windows(trips_root)
    if not windows:
        return die(f"no trips with usable GPX times under {trips_root}")

    trip_basenames = collect_trip_basenames(trips_root)
    photos = collect_media_images(media_root)
    cache = load_cache(cache_path)

    try:
        exif_by_path = resolve_exif_for_paths(photos, cache, exiftool)
    except RuntimeError as err:
        return die(str(err))
    finally:
        save_cache(cache_path, cache)

    suggested = 0
    skipped = 0
    to_remove = 0
    unmatched = 0
    no_exif = 0
    moved = 0
    removed = 0

    for path in photos:
        if path.name in trip_basenames:
            skipped += 1
            if args.clean_up_assigned:
                print(f"REMOVE  {path}")
                to_remove += 1
                if args.apply:
                    path.unlink()
                    removed += 1
            elif args.verbose:
                print(f"SKIP  {path}  (basename already in trips/)", file=sys.stderr)
            continue

        key = str(path.resolve())
        exif = exif_by_path.get(key)
        if exif is None or exif.date is None:
            no_exif += 1
            if args.verbose:
                print(f"NODATE  {path}", file=sys.stderr)
            continue

        trip = find_trip_for_date(exif.date, windows)
        if trip is None:
            unmatched += 1
            if args.verbose:
                print(f"UNMATCHED  {path}  ({exif.date.isoformat()})", file=sys.stderr)
            continue

        dest = trip.directory / path.name
        try:
            shown_dest = dest.relative_to(Path.cwd())
        except ValueError:
            shown_dest = dest
        print(f"MOVE  {path}  ->  {shown_dest}")
        suggested += 1

        if args.apply:
            if dest.exists():
                print(
                    f"error: destination exists, not moving: {dest}",
                    file=sys.stderr,
                )
                continue
            shutil.move(str(path), str(dest))
            trip_basenames.add(path.name)
            moved += 1

    summary = (
        f"{suggested} suggested, {skipped} skipped (already in trips), "
        f"{unmatched} unmatched, {no_exif} no EXIF date"
    )
    if args.clean_up_assigned:
        summary += f", {to_remove} to remove"
    if args.apply:
        summary += f", {moved} moved"
        if args.clean_up_assigned:
            summary += f", {removed} removed"
    print(summary, file=sys.stderr)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
