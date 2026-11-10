#!/usr/bin/env python3
"""Flag GPX days that look like Quality Mountain Days (heuristic).

Heuristic rules (trackable from GPX + DEM; not a full Mountain Training
assessment — planning/leadership/navigation judgement are out of scope):

  https://www.mountain-training.org/help/frequently-asked-questions/what-is-a-quality-day/

  - journey time that day > 5 hours (last − first timed point that day)
  - max elevation that day > 610 m (Mapterhorn DEM at each track point)

Elevations come from Mapterhorn Terrarium WebP tiles (zoom 14, fallback 12),
cached under ``_cache/elevation/mapterhorn/`` (same layout as the site build).

Requires Pillow (``pip install Pillow``) or a ``dwebp`` binary on PATH.
"""

from __future__ import annotations

import argparse
import csv
import io
import math
import sys
import tempfile
import time
import urllib.error
import urllib.request
import xml.etree.ElementTree as ET
from collections import defaultdict
from dataclasses import dataclass, field
from datetime import date, datetime, timedelta
from pathlib import Path

ELEVATION_SOURCE_ID = "mapterhorn"
ELEVATION_ZOOM = 14
ELEVATION_ZOOM_MIN = 12
ELEVATION_TILE_SIZE = 512
FETCH_GAP_S = 0.08
USER_AGENT = (
    "dzhus.org-blog-qmd/1.0 (https://dzhus.org; personal Quality Mountain Day scan)"
)

MIN_JOURNEY = timedelta(hours=5)
MIN_MAX_ELEV_M = 610.0

# Optional Pillow; otherwise fall back to `dwebp`.
try:
    from PIL import Image

    _HAS_PIL = True
except ImportError:  # pragma: no cover
    Image = None  # type: ignore[assignment, misc]
    _HAS_PIL = False


@dataclass(frozen=True)
class TrackPoint:
    lat: float
    lon: float
    time: datetime


@dataclass
class DayStats:
    day: date
    points: list[TrackPoint] = field(default_factory=list)

    @property
    def duration(self) -> timedelta:
        if len(self.points) < 2:
            return timedelta(0)
        return self.points[-1].time - self.points[0].time

    @property
    def start(self) -> datetime:
        return self.points[0].time

    @property
    def end(self) -> datetime:
        return self.points[-1].time


@dataclass
class TripData:
    slug: str
    directory: Path
    days: dict[date, DayStats]

    @property
    def calendar_days(self) -> list[date]:
        return sorted(self.days)


def die(msg: str, code: int = 1) -> int:
    print(f"error: {msg}", file=sys.stderr)
    return code


def local_name(tag: str) -> str:
    if "}" in tag:
        return tag.rsplit("}", 1)[1]
    return tag


def parse_time(raw: str) -> datetime | None:
    text = raw.strip().replace("Z", "+00:00")
    try:
        return datetime.fromisoformat(text)
    except ValueError:
        return None


def parse_gpx_points(path: Path) -> list[TrackPoint]:
    try:
        root = ET.parse(path).getroot()
    except ET.ParseError as err:
        raise ValueError(f"invalid GPX XML in {path}: {err}") from err

    points: list[TrackPoint] = []
    for el in root.iter():
        if local_name(el.tag) not in {"trkpt", "rtept"}:
            continue
        try:
            lat = float(el.attrib["lat"])
            lon = float(el.attrib["lon"])
        except (KeyError, ValueError):
            continue
        t: datetime | None = None
        for child in el:
            if local_name(child.tag) != "time" or not (child.text or "").strip():
                continue
            t = parse_time(child.text)
            if t is not None:
                break
        if t is None:
            continue
        points.append(TrackPoint(lat=lat, lon=lon, time=t))
    return points


def discover_trip_dirs(root: Path) -> list[Path]:
    """Return dirs that hold .gpx files to scan.

    Prefer each immediate subdirectory that contains .gpx. If none do but
    ``root`` itself holds .gpx files, return ``[root]``.
    """
    subdirs = sorted(
        p for p in root.iterdir() if p.is_dir() and not p.name.startswith(".")
    )
    trips = [
        d
        for d in subdirs
        if any(p.suffix.lower() == ".gpx" for p in d.iterdir() if p.is_file())
    ]
    if trips:
        return trips
    if any(p.suffix.lower() == ".gpx" for p in root.iterdir() if p.is_file()):
        return [root]
    return []


def gpx_files_in_dir(directory: Path) -> list[Path]:
    return sorted(
        p for p in directory.iterdir() if p.is_file() and p.suffix.lower() == ".gpx"
    )


def iter_gpx_files(path: Path) -> list[Path]:
    """List .gpx files for a single file or a trips/flat directory scan."""
    if path.is_file():
        return [path]
    files: list[Path] = []
    for trip_dir in discover_trip_dirs(path):
        files.extend(gpx_files_in_dir(trip_dir))
    return files


def gpx_label(gpx: Path, root: Path | None) -> str:
    if root is not None:
        try:
            return str(gpx.relative_to(root))
        except ValueError:
            pass
    return str(gpx)


def load_trip_from_gpx_files(
    gpx_files: list[Path],
    *,
    slug: str,
    directory: Path,
    root: Path | None = None,
) -> TripData | None:
    if not gpx_files:
        return None

    by_day: dict[date, list[TrackPoint]] = defaultdict(list)
    for gpx in gpx_files:
        try:
            pts = parse_gpx_points(gpx)
        except ValueError as err:
            print(f"warning: {err}", file=sys.stderr)
            continue
        if not pts:
            print(
                f"warning: no usable timed points in {gpx_label(gpx, root)}",
                file=sys.stderr,
            )
            continue
        for p in pts:
            by_day[p.time.date()].append(p)

    if not by_day:
        return None

    days: dict[date, DayStats] = {}
    for d, pts in by_day.items():
        pts.sort(key=lambda p: p.time)
        days[d] = DayStats(day=d, points=pts)

    return TripData(slug=slug, directory=directory, days=days)


def load_trip_from_gpx(gpx_path: Path) -> TripData | None:
    return load_trip_from_gpx_files(
        [gpx_path],
        slug=gpx_path.stem,
        directory=gpx_path.parent,
        root=gpx_path.parent,
    )


# --- Mapterhorn DEM (Terrarium WebP tiles) ---------------------------------


def lon_to_tile_x(lon: float, z: int) -> float:
    return ((lon + 180.0) / 360.0) * (2**z)


def lat_to_tile_y(lat: float, z: int) -> float:
    rad = math.radians(lat)
    return (
        (1.0 - math.log(math.tan(rad) + 1.0 / math.cos(rad)) / math.pi) / 2.0
    ) * (2**z)


def decode_terrarium(r: int, g: int, b: int) -> float:
    return r * 256.0 + g + b / 256.0 - 32768.0


def decode_webp_rgba(data: bytes) -> tuple[int, int, bytes]:
    """Return (width, height, RGBA bytes). Prefers Pillow; else ``dwebp``."""
    if _HAS_PIL:
        assert Image is not None
        with Image.open(io.BytesIO(data)) as im:
            rgba = im.convert("RGBA")
            w, h = rgba.size
            return w, h, rgba.tobytes()

    import shutil
    import subprocess

    dwebp = shutil.which("dwebp")
    if not dwebp:
        raise RuntimeError(
            "Need Pillow (`pip install Pillow`) or a `dwebp` binary on PATH "
            "to decode Mapterhorn WebP tiles"
        )
    with tempfile.TemporaryDirectory() as tmp:
        src = Path(tmp) / "tile.webp"
        dst = Path(tmp) / "tile.pam"
        src.write_bytes(data)
        subprocess.run(
            [dwebp, str(src), "-pam", "-o", str(dst), "-quiet"],
            check=True,
            capture_output=True,
        )
        pam = dst.read_bytes()
    # Minimal PAM parser: header ends at first blank line, then raw RGBA.
    sep = pam.find(b"\n\n")
    if sep < 0:
        raise RuntimeError("invalid PAM from dwebp")
    header = pam[:sep].decode("ascii", errors="replace").splitlines()
    body = pam[sep + 2 :]
    width = height = None
    for line in header:
        if line.startswith("WIDTH "):
            width = int(line.split()[1])
        elif line.startswith("HEIGHT "):
            height = int(line.split()[1])
    if width is None or height is None:
        raise RuntimeError("PAM missing WIDTH/HEIGHT")
    expected = width * height * 4
    if len(body) < expected:
        raise RuntimeError("PAM body truncated")
    return width, height, body[:expected]


class Mapterhorn:
    def __init__(self, cache_dir: Path) -> None:
        self.cache_dir = cache_dir
        self._pixels: dict[str, bytes] = {}
        self._missing: set[str] = set()
        self._last_fetch = 0.0

    def _cache_path(self, z: int, x: int, y: int) -> Path:
        return self.cache_dir / str(z) / str(x) / f"{y}.webp"

    def _missing_path(self, z: int, x: int, y: int) -> Path:
        return self.cache_dir / str(z) / str(x) / f"{y}.missing"

    def _throttle(self) -> None:
        wait = self._last_fetch + FETCH_GAP_S - time.monotonic()
        if wait > 0:
            time.sleep(wait)
        self._last_fetch = time.monotonic()

    def _fetch_tile(self, z: int, x: int, y: int) -> bytes | None:
        key = f"{z}/{x}/{y}"
        if key in self._missing:
            return None
        miss = self._missing_path(z, x, y)
        dest = self._cache_path(z, x, y)
        if miss.exists():
            self._missing.add(key)
            return None
        if dest.exists():
            return dest.read_bytes()

        self._throttle()
        url = f"https://tiles.mapterhorn.com/{z}/{x}/{y}.webp"
        req = urllib.request.Request(url, headers={"User-Agent": USER_AGENT})
        try:
            with urllib.request.urlopen(req, timeout=60) as resp:
                data = resp.read()
        except urllib.error.HTTPError as err:
            if err.code == 404:
                miss.parent.mkdir(parents=True, exist_ok=True)
                miss.write_bytes(b"")
                self._missing.add(key)
                return None
            raise RuntimeError(
                f"Mapterhorn tile fetch failed {z}/{x}/{y}: HTTP {err.code}"
            ) from err
        except urllib.error.URLError as err:
            raise RuntimeError(
                f"Mapterhorn tile fetch failed {z}/{x}/{y}: {err}"
            ) from err

        dest.parent.mkdir(parents=True, exist_ok=True)
        tmp = dest.with_suffix(dest.suffix + f".{time.time_ns()}.tmp")
        try:
            tmp.write_bytes(data)
            tmp.replace(dest)
        finally:
            if tmp.exists():
                tmp.unlink(missing_ok=True)
        return data

    def _load_pixels(self, z: int, x: int, y: int) -> bytes | None:
        key = f"{z}/{x}/{y}"
        if key in self._missing:
            return None
        cached = self._pixels.get(key)
        if cached is not None:
            return cached
        webp = self._fetch_tile(z, x, y)
        if webp is None:
            return None
        width, height, pixels = decode_webp_rgba(webp)
        if width != ELEVATION_TILE_SIZE or height != ELEVATION_TILE_SIZE:
            raise RuntimeError(
                f"Unexpected Mapterhorn tile size {width}x{height} for {key}"
            )
        self._pixels[key] = pixels
        return pixels

    def _pixel_elev(self, pixels: bytes, px: int, py: int) -> float:
        i = (py * ELEVATION_TILE_SIZE + px) * 4
        return decode_terrarium(pixels[i], pixels[i + 1], pixels[i + 2])

    def _elevation_at_zoom(
        self, lat: float, lon: float, zoom: int
    ) -> float | None:
        max_tile = 2**zoom
        fx = lon_to_tile_x(lon, zoom)
        fy = lat_to_tile_y(lat, zoom)
        px = fx * ELEVATION_TILE_SIZE
        py = fy * ELEVATION_TILE_SIZE
        x0 = math.floor(px)
        y0 = math.floor(py)
        x1 = x0 + 1
        y1 = y0 + 1
        tx = px - x0
        ty = py - y0

        def sample_world(wx: int, wy: int) -> float | None:
            tile_x = math.floor(wx / ELEVATION_TILE_SIZE)
            tile_y = math.floor(wy / ELEVATION_TILE_SIZE)
            local_x = (
                (wx - tile_x * ELEVATION_TILE_SIZE) % ELEVATION_TILE_SIZE
                + ELEVATION_TILE_SIZE
            ) % ELEVATION_TILE_SIZE
            local_y = max(
                0,
                min(
                    ELEVATION_TILE_SIZE - 1,
                    wy - tile_y * ELEVATION_TILE_SIZE,
                ),
            )
            tile_x = ((tile_x % max_tile) + max_tile) % max_tile
            tile_y = max(0, min(max_tile - 1, tile_y))
            pixels = self._load_pixels(zoom, tile_x, tile_y)
            if pixels is None:
                return None
            return self._pixel_elev(pixels, local_x, local_y)

        e00 = sample_world(x0, y0)
        if e00 is None:
            return None
        e10 = sample_world(x1, y0)
        if e10 is None:
            return None
        e01 = sample_world(x0, y1)
        if e01 is None:
            return None
        e11 = sample_world(x1, y1)
        if e11 is None:
            return None
        e0 = e00 * (1 - tx) + e10 * tx
        e1 = e01 * (1 - tx) + e11 * tx
        return e0 * (1 - ty) + e1 * ty

    def elevation_at(self, lat: float, lon: float) -> float:
        for z in range(ELEVATION_ZOOM, ELEVATION_ZOOM_MIN - 1, -1):
            elev = self._elevation_at_zoom(lat, lon, z)
            if elev is not None:
                return elev
        raise RuntimeError(
            f"Mapterhorn has no elevation tiles for {lat:.5f},{lon:.5f} "
            f"(tried z{ELEVATION_ZOOM_MIN}–{ELEVATION_ZOOM})"
        )

    def max_elevation(self, points: list[TrackPoint]) -> float | None:
        if not points:
            return None
        max_elev = self.elevation_at(points[0].lat, points[0].lon)
        for p in points[1:]:
            elev = self.elevation_at(p.lat, p.lon)
            if elev > max_elev:
                max_elev = elev
        return max_elev


def format_duration(td: timedelta) -> str:
    total = int(td.total_seconds())
    if total < 0:
        total = 0
    h, rem = divmod(total, 3600)
    m, _ = divmod(rem, 60)
    return f"{h}h{m:02d}m"


def main() -> int:
    repo_root = Path(__file__).resolve().parents[1]
    parser = argparse.ArgumentParser(
        description=(
            "Scan trip GPX tracks and flag days that heuristically match "
            "Quality Mountain Day criteria (>5h journey, max elev >610 m)."
        )
    )
    parser.add_argument(
        "path",
        type=Path,
        nargs="?",
        default=Path("trips"),
        help=(
            "Trips root (subdirs = trips), a single trip dir with .gpx, "
            "or one .gpx file (default: trips)"
        ),
    )
    parser.add_argument(
        "--cache-dir",
        type=Path,
        default=None,
        help=(
            "Mapterhorn tile cache directory "
            f"(default: <repo>/_cache/elevation/{ELEVATION_SOURCE_ID})"
        ),
    )
    parser.add_argument(
        "--show-rejected",
        action="store_true",
        help="Also list days that fail one or more heuristic checks",
    )
    parser.add_argument(
        "--format",
        choices=("table", "csv"),
        default="table",
        help="Output format for results on stdout (default: table)",
    )
    args = parser.parse_args()

    path: Path = args.path
    if not path.exists():
        return die(f"path not found: {path}")
    if path.is_file():
        if path.suffix.lower() != ".gpx":
            return die(f"expected a .gpx file or directory: {path}")
    elif not path.is_dir():
        return die(f"expected a .gpx file or directory: {path}")

    cache_dir: Path = args.cache_dir or (
        repo_root / "_cache" / "elevation" / ELEVATION_SOURCE_ID
    )
    cache_dir.mkdir(parents=True, exist_ok=True)

    if not _HAS_PIL:
        import shutil

        if not shutil.which("dwebp"):
            return die(
                "Pillow is not installed and `dwebp` was not found. "
                "Install with: python3 -m venv scripts/.venv && "
                "scripts/.venv/bin/pip install Pillow"
            )
        print(
            "warning: Pillow not installed; decoding tiles via dwebp (slower)",
            file=sys.stderr,
        )

    trips: list[TripData] = []
    scan_label = str(path)
    gpx_paths = iter_gpx_files(path)
    if not gpx_paths:
        return die(f"no .gpx files found under {path}")
    for gpx_path in gpx_paths:
        trip = load_trip_from_gpx(gpx_path)
        if trip is None:
            print(
                f"warning: skipping {gpx_path.name}: no usable GPX times",
                file=sys.stderr,
            )
            continue
        trips.append(trip)
    if not trips:
        return die(f"no usable timed points under {path}")

    dem = Mapterhorn(cache_dir)
    candidates: list[tuple[TripData, DayStats, float]] = []
    rejected: list[tuple[str, date, str]] = []
    trips_loaded = len(trips)

    for trip in trips:
        for day in trip.calendar_days:
            stats = trip.days[day]
            if stats.duration <= MIN_JOURNEY:
                rejected.append(
                    (
                        trip.slug,
                        day,
                        f"duration {format_duration(stats.duration)} ≤ 5h",
                    )
                )
                continue

            print(
                f"sampling elevation: {trip.slug} {day.isoformat()} "
                f"({len(stats.points)} pts, {format_duration(stats.duration)})…",
                file=sys.stderr,
            )
            try:
                max_elev = dem.max_elevation(stats.points)
            except RuntimeError as err:
                print(f"warning: {trip.slug} {day}: {err}", file=sys.stderr)
                rejected.append((trip.slug, day, f"elevation error: {err}"))
                continue

            assert max_elev is not None
            if max_elev <= MIN_MAX_ELEV_M:
                rejected.append(
                    (
                        trip.slug,
                        day,
                        f"max elev {max_elev:.0f} m ≤ {MIN_MAX_ELEV_M:.0f} m",
                    )
                )
                continue

            candidates.append((trip, stats, max_elev))

    print(
        f"Scanned {trips_loaded} trip(s) from {scan_label}; "
        f"{len(candidates)} Quality Mountain Day candidate(s).",
        file=sys.stderr,
    )

    rows = sorted(candidates, key=lambda row: (row[1].day, row[0].slug))

    if args.format == "csv":
        writer = csv.writer(sys.stdout)
        if args.show_rejected:
            writer.writerow(
                [
                    "trip",
                    "date",
                    "duration",
                    "duration_s",
                    "max_elev_m",
                    "points",
                    "status",
                    "reason",
                ]
            )
            for trip, stats, max_elev in rows:
                writer.writerow(
                    [
                        trip.slug,
                        stats.day.isoformat(),
                        format_duration(stats.duration),
                        int(stats.duration.total_seconds()),
                        f"{max_elev:.1f}",
                        len(stats.points),
                        "candidate",
                        "",
                    ]
                )
            for slug, day, reason in sorted(rejected, key=lambda r: (r[1], r[0])):
                writer.writerow(
                    [slug, day.isoformat(), "", "", "", "", "rejected", reason]
                )
        else:
            writer.writerow(
                ["trip", "date", "duration", "duration_s", "max_elev_m", "points"]
            )
            for trip, stats, max_elev in rows:
                writer.writerow(
                    [
                        trip.slug,
                        stats.day.isoformat(),
                        format_duration(stats.duration),
                        int(stats.duration.total_seconds()),
                        f"{max_elev:.1f}",
                        len(stats.points),
                    ]
                )
        return 0

    print()
    print(
        f"{'trip':<28} {'date':<12} {'duration':>8} {'max_m':>7}  points"
    )
    print("-" * 68)
    for trip, stats, max_elev in rows:
        print(
            f"{trip.slug:<28} {stats.day.isoformat():<12} "
            f"{format_duration(stats.duration):>8} {max_elev:7.0f}  "
            f"{len(stats.points)}"
        )

    if args.show_rejected and rejected:
        print()
        print("Rejected days:")
        for slug, day, reason in sorted(rejected, key=lambda r: (r[1], r[0])):
            print(f"  {slug} {day.isoformat()}: {reason}")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
