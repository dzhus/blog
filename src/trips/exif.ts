import fs from "node:fs";
import path from "node:path";
import exifr from "exifr";

export type PhotoExif = {
  capturedAt: Date;
  displayCapturedAt: string;
  lat: number;
  lon: number;
  /** Multi-line hover tooltip: date/time, exposure, GPS. */
  tooltip: string;
};

function formatExifLocal(dt: Date): string {
  const pad = (n: number) => String(n).padStart(2, "0");
  return `${dt.getFullYear()}-${pad(dt.getMonth() + 1)}-${pad(dt.getDate())} ${pad(dt.getHours())}:${pad(dt.getMinutes())}:${pad(dt.getSeconds())}`;
}

function formatAperture(fNumber: unknown): string | null {
  if (typeof fNumber !== "number" || !Number.isFinite(fNumber) || fNumber <= 0) {
    return null;
  }
  const rounded = Math.round(fNumber * 10) / 10;
  const text = Number.isInteger(rounded) ? String(rounded) : rounded.toFixed(1);
  return `f/${text}`;
}

function formatShutter(exposureTime: unknown): string | null {
  if (
    typeof exposureTime !== "number" ||
    !Number.isFinite(exposureTime) ||
    !(exposureTime > 0)
  ) {
    return null;
  }
  if (exposureTime >= 1) {
    const text = Number.isInteger(exposureTime)
      ? String(exposureTime)
      : exposureTime.toFixed(1);
    return `${text}s`;
  }
  return `1/${Math.round(1 / exposureTime)}`;
}

function formatIso(iso: unknown): string | null {
  const raw = Array.isArray(iso) ? iso[0] : iso;
  if (typeof raw !== "number" || !Number.isFinite(raw) || raw <= 0) return null;
  return `ISO ${Math.round(raw)}`;
}

function formatGps(lat: number, lon: number): string {
  return `${lat.toFixed(5)}, ${lon.toFixed(5)}`;
}

export function formatExifTooltip(parts: {
  displayCapturedAt: string;
  aperture: string | null;
  shutter: string | null;
  iso: string | null;
  lat: number;
  lon: number;
}): string {
  const lines = [parts.displayCapturedAt];
  const exposure = [parts.aperture, parts.shutter, parts.iso]
    .filter(Boolean)
    .join(" · ");
  if (exposure) lines.push(exposure);
  lines.push(formatGps(parts.lat, parts.lon));
  return lines.join("\n");
}

export async function readPhotoExif(filePath: string): Promise<PhotoExif> {
  const label = path.relative(process.cwd(), filePath) || filePath;
  const buf = await fs.promises.readFile(filePath);

  let data: Record<string, unknown> | undefined;
  try {
    data = await exifr.parse(buf, {
      pick: [
        "DateTimeOriginal",
        "FNumber",
        "ExposureTime",
        "ISO",
        "ISOSpeedRatings",
        "PhotographicSensitivity",
      ],
    });
  } catch (err) {
    const detail = err instanceof Error ? err.message : String(err);
    throw new Error(
      `Failed to read EXIF from ${label}: ${detail}. Each trip photo must include DateTimeOriginal and GPS coordinates.`,
    );
  }

  const dt = data?.DateTimeOriginal;
  if (!(dt instanceof Date) || Number.isNaN(+dt)) {
    throw new Error(
      `Missing EXIF DateTimeOriginal in ${label}. Add a valid DateTimeOriginal tag (CreateDate/filename are not used).`,
    );
  }

  let lat: number | undefined;
  let lon: number | undefined;
  try {
    const gps = await exifr.gps(buf);
    if (
      gps &&
      typeof gps.latitude === "number" &&
      typeof gps.longitude === "number" &&
      Number.isFinite(gps.latitude) &&
      Number.isFinite(gps.longitude)
    ) {
      lat = gps.latitude;
      lon = gps.longitude;
    }
  } catch (err) {
    const detail = err instanceof Error ? err.message : String(err);
    throw new Error(
      `Failed to read GPS EXIF from ${label}: ${detail}. Each trip photo must include GPS latitude/longitude for the map.`,
    );
  }

  if (lat == null || lon == null) {
    throw new Error(
      `Missing GPS coordinates in ${label}. Each trip photo must include EXIF GPS latitude/longitude to appear on the trip map.`,
    );
  }

  const displayCapturedAt = formatExifLocal(dt);
  const aperture = formatAperture(data?.FNumber);
  const shutter = formatShutter(data?.ExposureTime);
  const iso = formatIso(
    data?.ISO ?? data?.ISOSpeedRatings ?? data?.PhotographicSensitivity,
  );

  return {
    capturedAt: dt,
    displayCapturedAt,
    lat,
    lon,
    tooltip: formatExifTooltip({
      displayCapturedAt,
      aperture,
      shutter,
      iso,
      lat,
      lon,
    }),
  };
}

export function formatIsoCapturedAt(date: Date): string {
  if (Number.isNaN(+date)) return "";
  return date.toISOString();
}
